# Targets for using a previously built random forest model to 
# predict whether a stream is episodic or not episodic based
# on its associated attributes 

source('6_PredictClass/src/navigate_nhdplus_network.R') 
source('6_PredictClass/src/applyToHucs.R')

p6_targets <- list(

  ###### 1) Identify HUC-2 watersheds based on states of interest  ######
  
  # Setup states as an sf polygon
  # tar_target(p6_states, c('WI')),
  tar_target(p6_states, p1_conus_state_cds),
  
  tar_target(p6_state_sf, usmap::us_map(include = p6_states) %>%
               st_as_sf(coords = c('x', 'y'),
                        crs = usmap::usmap_crs()) %>%
               st_transform(crs = 4326)),
  
  # For each state, identify the HUC-02s that overlap then combine so we only have one of each HUC).
  # We will work at the HUC-02 level because everything else takes too long
  tar_target(p6_state_huc_sf_all, get_huc(AOI = p6_state_sf, type = 'huc02'),
             pattern = map(p6_state_sf), iteration = 'list'),
  
  tar_target(p6_state_huc_sf, bind_rows(p6_state_huc_sf_all) %>% distinct() %>% 
               st_transform(crs(p1_nhdplus_flowlines_sf))),

  # # Find comids in the HUCs through flowline intersect
  tar_target(p6_huc_flowlines_sf, {
    huc1 = st_intersects(p1_nhdplus_flowlines_sf %>%
                           dplyr::select(COMID, FromNode, ToNode, Divergence, FTYPE,
                                         AreaSqKM, LENGTHKM, GNIS_ID),
                         p6_state_huc_sf)
    huc1.indx = which(as.numeric(huc1) == 1)

    huc_flowlines_sf = p1_nhdplus_flowlines_sf[huc1.indx,] %>%
      dplyr::select(COMID, FromNode, ToNode, Divergence, FTYPE, AreaSqKM,
                    LENGTHKM, GNIS_ID,StreamOrde) %>%
      mutate(region = lonlat_to_state(st_endpoint(.))) %>%
      left_join(tibble(region = state.name) %>%
                  bind_cols(tibble(region_fname = state.abb))) %>% 
      mutate(huc2 = p6_state_huc_sf$huc2[1])
    return(huc_flowlines_sf)
  }, pattern = map(p6_state_huc_sf), iteration = 'list'),

  # # Find comids in the STATES through flowline intersect
  tar_target(p6_state_flowlines_sf_list, {
    state1 = st_intersects(p1_nhdplus_flowlines_sf %>%
                           dplyr::select(COMID, FromNode, ToNode, Divergence, FTYPE,
                                         AreaSqKM, LENGTHKM, GNIS_ID),
                         p6_state_sf %>% st_transform(crs(p1_nhdplus_flowlines_sf)))
    state1.indx = which(as.numeric(state1) == 1)
    
    state_flowlines_sf = p1_nhdplus_flowlines_sf[state1.indx,] %>%
      dplyr::select(COMID, FromNode, ToNode, Divergence, FTYPE, AreaSqKM,
                    LENGTHKM, GNIS_ID,StreamOrde) %>%
      mutate(region = lonlat_to_state(st_endpoint(.))) %>%
      left_join(tibble(region = state.name) %>%
                  bind_cols(tibble(region_fname = state.abb)))
    return(state_flowlines_sf)
  }, pattern = map(p6_state_sf), iteration = 'list'),
  
  tar_target(p6_state_flowlines_sf,
    bind_rows(p6_state_flowlines_sf_list)), 
  
  # List comids without geometry
  tarchetypes::tar_group_count(p6_state_comids, 
                  count = 200, 
                  p6_state_flowlines_sf %>% 
                    st_drop_geometry() %>% 
                    sample_frac(size = 1)), # randomize rows 
                  
  # List comids link table without geometry for each HUC 
  tar_target(p6_flowlines_huc_tbl,
             p6_huc_flowlines_sf %>%
               hydroloom::add_toids() %>% 
               st_drop_geometry() %>%
               select(comid = COMID, toid, huc2),
             pattern = map(p6_huc_flowlines_sf), iteration = 'list'),
  
  ########### 2) For each HUC, identify upstream comids for state comids ###########
  # This must be done in groups of COMIDS for each HUC - took 4 hours
  tar_target(p6_huc2_upstream_comids, {
    p6_flowlines_huc_use = p6_flowlines_huc_tbl[[2]]
    p6_index_ids = make_index_ids(p6_flowlines_huc_use)
    p6_froms = make_fromids(p6_index_ids)
    
    # Number of workers is based on the number of cores your computer has
    # so that we don't use up ALL possible cores on your computer
    comp_cores <- detectCores()
    n_cores <- ifelse(comp_cores > 3, comp_cores, 2)
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)

    use_state_comids = p6_state_comids %>% dplyr::filter(COMID %in% p6_flowlines_huc_use$comid)
    
    upstream_tbl <- foreach(i=1:nrow(use_state_comids),
                            .combine=bind_rows,
                            # Need to explicitly pass in packages and functions so that
                            # the parallel workers have them available.
                            .export = c('identify_upstream_comids_hy','get_sorted_HD','sort_network_hy_HD'),
                            .packages = c('nhdplusTools', 'tidyverse','hydroloom','sf'),
                            .verbose = FALSE) %dopar% {
                            identify_upstream_comids_hy(comid_in = use_state_comids$COMID[i],
                                                          flines_network = p6_flowlines_huc_use,
                                                          index_ids = p6_index_ids,
                                                          froms = p6_froms)
                            }

    # Restore the regular state
    registerDoSEQ()
    stopCluster(cl)
    
    outfile = sprintf('6_PredictClass/out/allupstreamcomids_huc_%s.feather',
                      paste(p6_flowlines_huc_use$huc2[1], unique(use_state_comids$tar_group), sep = '_'))
    write_feather(upstream_tbl, outfile)

    return(upstream_tbl)

  }, pattern = map(p6_state_comids), iteration = 'list'),
  tar_target(p6_huc7_upstream_comids, {
    p6_flowlines_huc_use = p6_flowlines_huc_tbl[[3]]
    p6_index_ids = make_index_ids(p6_flowlines_huc_use)
    p6_froms = make_fromids(p6_index_ids)
    
    # Number of workers is based on the number of cores your computer has
    # so that we don't use up ALL possible cores on your computer
    comp_cores <- detectCores()
    n_cores <- ifelse(comp_cores > 3, comp_cores, 2)
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    use_state_comids = p6_state_comids %>% filter(COMID %in% p6_flowlines_huc_use$comid)
    
    upstream_tbl <- foreach(i=1:nrow(use_state_comids),
                            .combine=bind_rows,
                            # Need to explicitly pass in packages and functions so that
                            # the parallel workers have them available.
                            .export = c('identify_upstream_comids_hy','get_sorted_HD','sort_network_hy_HD'),
                            .packages = c('nhdplusTools', 'tidyverse','hydroloom','sf'),
                            .verbose = FALSE) %dopar% {
                              identify_upstream_comids_hy(comid_in = use_state_comids$COMID[i],
                                                          flines_network = p6_flowlines_huc_use,
                                                          index_ids = p6_index_ids,
                                                          froms = p6_froms)
                            }
    
    # Restore the regular state
    registerDoSEQ()
    stopCluster(cl)
    
    outfile = sprintf('6_PredictClass/out/allupstreamcomids_huc_%s.feather',
                      paste(p6_flowlines_huc_use$huc2[1], p6_state_comids$tar_group[1], sep = '_'))
    write_feather(upstream_tbl, outfile)
    
    return(upstream_tbl)
    
  }, pattern = map(p6_state_comids), iteration = 'list'),
  
  tar_target(p6_huc10_upstream_comids, {
    p6_flowlines_huc_use = p6_flowlines_huc_tbl[[4]]
    p6_index_ids = make_index_ids(p6_flowlines_huc_use)
    p6_froms = make_fromids(p6_index_ids)
    
    # Number of workers is based on the number of cores your computer has
    # so that we don't use up ALL possible cores on your computer
    comp_cores <- detectCores()
    n_cores <- ifelse(comp_cores > 3, comp_cores, 2)
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    use_state_comids = p6_state_comids %>% filter(COMID %in% p6_flowlines_huc_use$comid)
    
    upstream_tbl <- foreach(i=1:nrow(use_state_comids),
                            .combine=bind_rows,
                            # Need to explicitly pass in packages and functions so that
                            # the parallel workers have them available.
                            .export = c('identify_upstream_comids_hy','get_sorted_HD','sort_network_hy_HD'),
                            .packages = c('nhdplusTools', 'tidyverse','hydroloom','sf'),
                            .verbose = FALSE) %dopar% {
                              identify_upstream_comids_hy(comid_in = use_state_comids$COMID[i],
                                                          flines_network = p6_flowlines_huc_use,
                                                          index_ids = p6_index_ids,
                                                          froms = p6_froms)
                            }
    
    # Restore the regular state
    registerDoSEQ()
    stopCluster(cl)
    
    outfile = sprintf('6_PredictClass/out/allupstreamcomids_huc_%s.feather',
                      paste(p6_flowlines_huc_use$huc2[1], p6_state_comids$tar_group[1], sep = '_'))
    write_feather(upstream_tbl, outfile)
    
    return(upstream_tbl)
    
  }, pattern = map(p6_state_comids), iteration = 'list'),
  tar_target(p6_huc5_upstream_comids, {
    p6_flowlines_huc_use = p6_flowlines_huc_tbl[[5]]
    p6_index_ids = make_index_ids(p6_flowlines_huc_use)
    p6_froms = make_fromids(p6_index_ids)
    
    # Number of workers is based on the number of cores your computer has
    # so that we don't use up ALL possible cores on your computer
    comp_cores <- detectCores()
    n_cores <- ifelse(comp_cores > 3, comp_cores, 2)
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    use_state_comids = p6_state_comids %>% filter(COMID %in% p6_flowlines_huc_use$comid)
    
    upstream_tbl <- foreach(i=1:nrow(use_state_comids),
                            .combine=bind_rows,
                            # Need to explicitly pass in packages and functions so that
                            # the parallel workers have them available.
                            .export = c('identify_upstream_comids_hy','get_sorted_HD','sort_network_hy_HD'),
                            .packages = c('nhdplusTools', 'tidyverse','hydroloom','sf'),
                            .verbose = FALSE) %dopar% {
                              identify_upstream_comids_hy(comid_in = use_state_comids$COMID[i],
                                                          flines_network = p6_flowlines_huc_use,
                                                          index_ids = p6_index_ids,
                                                          froms = p6_froms)
                            }
    
    # Restore the regular state
    registerDoSEQ()
    stopCluster(cl)
    
    outfile = sprintf('6_PredictClass/out/allupstreamcomids_huc_%s.feather',
                      paste(p6_flowlines_huc_use$huc2[1], p6_state_comids$tar_group[1], sep = '_'))
    write_feather(upstream_tbl, outfile)
    
    return(upstream_tbl)
    
  }, pattern = map(p6_state_comids), iteration = 'list'),
  tar_target(p6_huc8_upstream_comids, {
    p6_flowlines_huc_use = p6_flowlines_huc_tbl[[6]]
    p6_index_ids = make_index_ids(p6_flowlines_huc_use)
    p6_froms = make_fromids(p6_index_ids)
    
    # Number of workers is based on the number of cores your computer has
    # so that we don't use up ALL possible cores on your computer
    comp_cores <- detectCores()
    n_cores <- ifelse(comp_cores > 3, comp_cores, 2)
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    use_state_comids = p6_state_comids %>% filter(COMID %in% p6_flowlines_huc_use$comid)
    
    upstream_tbl <- foreach(i=1:nrow(use_state_comids),
                            .combine=bind_rows,
                            # Need to explicitly pass in packages and functions so that
                            # the parallel workers have them available.
                            .export = c('identify_upstream_comids_hy','get_sorted_HD','sort_network_hy_HD'),
                            .packages = c('nhdplusTools', 'tidyverse','hydroloom','sf'),
                            .verbose = FALSE) %dopar% {
                              identify_upstream_comids_hy(comid_in = use_state_comids$COMID[i],
                                                          flines_network = p6_flowlines_huc_use,
                                                          index_ids = p6_index_ids,
                                                          froms = p6_froms)
                            }
    
    # Restore the regular state
    registerDoSEQ()
    stopCluster(cl)
    
    outfile = sprintf('6_PredictClass/out/allupstreamcomids_huc_%s.feather',
                      paste(p6_flowlines_huc_use$huc2[1], p6_state_comids$tar_group[1], sep = '_'))
    write_feather(upstream_tbl, outfile)
    
    return(upstream_tbl)
    
  }, pattern = map(p6_state_comids), iteration = 'list'),
  tar_target(p6_huc4_upstream_comids, {
    p6_flowlines_huc_use = p6_flowlines_huc_tbl[[7]]
    p6_index_ids = make_index_ids(p6_flowlines_huc_use)
    p6_froms = make_fromids(p6_index_ids)
    
    # Number of workers is based on the number of cores your computer has
    # so that we don't use up ALL possible cores on your computer
    comp_cores <- detectCores()
    n_cores <- ifelse(comp_cores > 3, comp_cores, 2)
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    use_state_comids = p6_state_comids %>% filter(COMID %in% p6_flowlines_huc_use$comid)
    
    upstream_tbl <- foreach(i=1:nrow(use_state_comids),
                            .combine=bind_rows,
                            # Need to explicitly pass in packages and functions so that
                            # the parallel workers have them available.
                            .export = c('identify_upstream_comids_hy','get_sorted_HD','sort_network_hy_HD'),
                            .packages = c('nhdplusTools', 'tidyverse','hydroloom','sf'),
                            .verbose = FALSE) %dopar% {
                              identify_upstream_comids_hy(comid_in = use_state_comids$COMID[i],
                                                          flines_network = p6_flowlines_huc_use,
                                                          index_ids = p6_index_ids,
                                                          froms = p6_froms)
                            }
    
    # Restore the regular state
    registerDoSEQ()
    stopCluster(cl)
    
    outfile = sprintf('6_PredictClass/out/allupstreamcomids_huc_%s.feather',
                      paste(p6_flowlines_huc_use$huc2[1], p6_state_comids$tar_group[1], sep = '_'))
    write_feather(upstream_tbl, outfile)
    
    return(upstream_tbl)
    
  }, pattern = map(p6_state_comids), iteration = 'list'),
  tar_target(p6_huc6_upstream_comids, {
    p6_flowlines_huc_use = p6_flowlines_huc_tbl[[8]]
    p6_index_ids = make_index_ids(p6_flowlines_huc_use)
    p6_froms = make_fromids(p6_index_ids)
    
    # Number of workers is based on the number of cores your computer has
    # so that we don't use up ALL possible cores on your computer
    comp_cores <- detectCores()
    n_cores <- ifelse(comp_cores > 3, comp_cores, 2)
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    use_state_comids = p6_state_comids %>% filter(COMID %in% p6_flowlines_huc_use$comid)
    
    upstream_tbl <- foreach(i=1:nrow(use_state_comids),
                            .combine=bind_rows,
                            # Need to explicitly pass in packages and functions so that
                            # the parallel workers have them available.
                            .export = c('identify_upstream_comids_hy','get_sorted_HD','sort_network_hy_HD'),
                            .packages = c('nhdplusTools', 'tidyverse','hydroloom','sf'),
                            .verbose = FALSE) %dopar% {
                              identify_upstream_comids_hy(comid_in = use_state_comids$COMID[i],
                                                          flines_network = p6_flowlines_huc_use,
                                                          index_ids = p6_index_ids,
                                                          froms = p6_froms)
                            }
    
    # Restore the regular state
    registerDoSEQ()
    stopCluster(cl)
    
    outfile = sprintf('6_PredictClass/out/allupstreamcomids_huc_%s.feather',
                      paste(p6_flowlines_huc_use$huc2[1], p6_state_comids$tar_group[1], sep = '_'))
    write_feather(upstream_tbl, outfile)
    
    return(upstream_tbl)
    
  }, pattern = map(p6_state_comids), iteration = 'list'),
  tar_target(p6_huc9_upstream_comids, {
    p6_flowlines_huc_use = p6_flowlines_huc_tbl[[9]]
    p6_index_ids = make_index_ids(p6_flowlines_huc_use)
    p6_froms = make_fromids(p6_index_ids)
    
    # Number of workers is based on the number of cores your computer has
    # so that we don't use up ALL possible cores on your computer
    comp_cores <- detectCores()
    n_cores <- ifelse(comp_cores > 3, comp_cores, 2)
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    use_state_comids = p6_state_comids %>% filter(COMID %in% p6_flowlines_huc_use$comid)
    
    upstream_tbl <- foreach(i=1:nrow(use_state_comids),
                            .combine=bind_rows,
                            # Need to explicitly pass in packages and functions so that
                            # the parallel workers have them available.
                            .export = c('identify_upstream_comids_hy','get_sorted_HD','sort_network_hy_HD'),
                            .packages = c('nhdplusTools', 'tidyverse','hydroloom','sf'),
                            .verbose = FALSE) %dopar% {
                              identify_upstream_comids_hy(comid_in = use_state_comids$COMID[i],
                                                          flines_network = p6_flowlines_huc_use,
                                                          index_ids = p6_index_ids,
                                                          froms = p6_froms)
                            }
    
    # Restore the regular state
    registerDoSEQ()
    stopCluster(cl)
    
    outfile = sprintf('6_PredictClass/out/allupstreamcomids_huc_%s.feather',
                      paste(p6_flowlines_huc_use$huc2[1], p6_state_comids$tar_group[1], sep = '_'))
    write_feather(upstream_tbl, outfile)
    
    return(upstream_tbl)
    
  }, pattern = map(p6_state_comids), iteration = 'list'),
  tar_target(p6_huc11_upstream_comids, {
    p6_flowlines_huc_use = p6_flowlines_huc_tbl[[10]]
    p6_index_ids = make_index_ids(p6_flowlines_huc_use)
    p6_froms = make_fromids(p6_index_ids)
    
    # Number of workers is based on the number of cores your computer has
    # so that we don't use up ALL possible cores on your computer
    comp_cores <- detectCores()
    n_cores <- ifelse(comp_cores > 3, comp_cores, 2)
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    use_state_comids = p6_state_comids %>% filter(COMID %in% p6_flowlines_huc_use$comid)
    
    upstream_tbl <- foreach(i=1:nrow(use_state_comids),
                            .combine=bind_rows,
                            # Need to explicitly pass in packages and functions so that
                            # the parallel workers have them available.
                            .export = c('identify_upstream_comids_hy','get_sorted_HD','sort_network_hy_HD'),
                            .packages = c('nhdplusTools', 'tidyverse','hydroloom','sf'),
                            .verbose = FALSE) %dopar% {
                              identify_upstream_comids_hy(comid_in = use_state_comids$COMID[i],
                                                          flines_network = p6_flowlines_huc_use,
                                                          index_ids = p6_index_ids,
                                                          froms = p6_froms)
                            }
    
    # Restore the regular state
    registerDoSEQ()
    stopCluster(cl)
    
    outfile = sprintf('6_PredictClass/out/allupstreamcomids_huc_%s.feather',
                      paste(p6_flowlines_huc_use$huc2[1], p6_state_comids$tar_group[1], sep = '_'))
    write_feather(upstream_tbl, outfile)
    
    return(upstream_tbl)
    
  }, pattern = map(p6_state_comids), iteration = 'list'),
  tar_target(p6_huc3_upstream_comids, {
    p6_flowlines_huc_use = p6_flowlines_huc_tbl[[11]]
    p6_index_ids = make_index_ids(p6_flowlines_huc_use)
    p6_froms = make_fromids(p6_index_ids)
    
    # Number of workers is based on the number of cores your computer has
    # so that we don't use up ALL possible cores on your computer
    comp_cores <- detectCores()
    n_cores <- ifelse(comp_cores > 3, comp_cores, 2)
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    use_state_comids = p6_state_comids %>% filter(COMID %in% p6_flowlines_huc_use$comid)
    
    upstream_tbl <- foreach(i=1:nrow(use_state_comids),
                            .combine=bind_rows,
                            # Need to explicitly pass in packages and functions so that
                            # the parallel workers have them available.
                            .export = c('identify_upstream_comids_hy','get_sorted_HD','sort_network_hy_HD'),
                            .packages = c('nhdplusTools', 'tidyverse','hydroloom','sf'),
                            .verbose = FALSE) %dopar% {
                              identify_upstream_comids_hy(comid_in = use_state_comids$COMID[i],
                                                          flines_network = p6_flowlines_huc_use,
                                                          index_ids = p6_index_ids,
                                                          froms = p6_froms)
                            }
    
    # Restore the regular state
    registerDoSEQ()
    stopCluster(cl)
    
    outfile = sprintf('6_PredictClass/out/allupstreamcomids_huc_%s.feather',
                      paste(p6_flowlines_huc_use$huc2[1], p6_state_comids$tar_group[1], sep = '_'))
    write_feather(upstream_tbl, outfile)
    
    return(upstream_tbl)
    
  }, pattern = map(p6_state_comids), iteration = 'list'),
  tar_target(p6_huc1_upstream_comids, {
    p6_flowlines_huc_use = p6_flowlines_huc_tbl[[1]]
    p6_index_ids = make_index_ids(p6_flowlines_huc_use)
    p6_froms = make_fromids(p6_index_ids)
    
    # Number of workers is based on the number of cores your computer has
    # so that we don't use up ALL possible cores on your computer
    comp_cores <- detectCores()
    n_cores <- ifelse(comp_cores > 3, comp_cores, 2)
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    use_state_comids = p6_state_comids %>% filter(COMID %in% p6_flowlines_huc_use$comid)
    
    upstream_tbl <- foreach(i=1:nrow(use_state_comids),
                            .combine=bind_rows,
                            # Need to explicitly pass in packages and functions so that
                            # the parallel workers have them available.
                            .export = c('identify_upstream_comids_hy','get_sorted_HD','sort_network_hy_HD'),
                            .packages = c('nhdplusTools', 'tidyverse','hydroloom','sf'),
                            .verbose = FALSE) %dopar% {
                              identify_upstream_comids_hy(comid_in = use_state_comids$COMID[i],
                                                          flines_network = p6_flowlines_huc_use,
                                                          index_ids = p6_index_ids,
                                                          froms = p6_froms)
                            }
    
    # Restore the regular state
    registerDoSEQ()
    stopCluster(cl)
    
    outfile = sprintf('6_PredictClass/out/allupstreamcomids_huc_%s.feather',
                      paste(p6_flowlines_huc_use$huc2[1], p6_state_comids$tar_group[1], sep = '_'))
    write_feather(upstream_tbl, outfile)
    
    return(upstream_tbl)
    
  }, pattern = map(p6_state_comids), iteration = 'list'),
  
  # Bind output lists to dataframes 
  tar_target(p6_huc1_upstream_comids_df, bindHucs(p6_huc1_upstream_comids)),
  tar_target(p6_huc2_upstream_comids_df, bindHucs(p6_huc2_upstream_comids)),
  tar_target(p6_huc3_upstream_comids_df, bindHucs(p6_huc3_upstream_comids)),
  tar_target(p6_huc4_upstream_comids_df, bindHucs(p6_huc4_upstream_comids)),
  tar_target(p6_huc5_upstream_comids_df, bindHucs(p6_huc5_upstream_comids)),
  tar_target(p6_huc6_upstream_comids_df, bindHucs(p6_huc6_upstream_comids)),
  tar_target(p6_huc7_upstream_comids_df, bindHucs(p6_huc7_upstream_comids)),
  tar_target(p6_huc8_upstream_comids_df, bindHucs(p6_huc8_upstream_comids)),
  tar_target(p6_huc9_upstream_comids_df, bindHucs(p6_huc9_upstream_comids)),
  tar_target(p6_huc10_upstream_comids_df, bindHucs(p6_huc10_upstream_comids)),
  tar_target(p6_huc11_upstream_comids_df, bindHucs(p6_huc11_upstream_comids)),
  
  # Get unique upstream comids for each HUC 
  tar_target(p6_huc1_upstream_unqiuecomids, getUpstream(p6_huc1_upstream_comids_df)),
  tar_target(p6_huc2_upstream_unqiuecomids, getUpstream(p6_huc2_upstream_comids_df)),
  tar_target(p6_huc3_upstream_unqiuecomids, getUpstream(p6_huc3_upstream_comids_df)),
  tar_target(p6_huc4_upstream_unqiuecomids, getUpstream(p6_huc4_upstream_comids_df)),
  tar_target(p6_huc5_upstream_unqiuecomids, getUpstream(p6_huc5_upstream_comids_df)),
  tar_target(p6_huc6_upstream_unqiuecomids, getUpstream(p6_huc6_upstream_comids_df)),
  tar_target(p6_huc7_upstream_unqiuecomids, getUpstream(p6_huc7_upstream_comids_df)),
  tar_target(p6_huc8_upstream_unqiuecomids, getUpstream(p6_huc8_upstream_comids_df)),
  tar_target(p6_huc9_upstream_unqiuecomids, getUpstream(p6_huc9_upstream_comids_df)),
  tar_target(p6_huc10_upstream_unqiuecomids, getUpstream(p6_huc10_upstream_comids_df)),
  tar_target(p6_huc11_upstream_unqiuecomids, getUpstream(p6_huc11_upstream_comids_df)),
  
  ################## 3) Download all attributes for COMIDs ##################
  # Download NHD+ attributes data for each COMID in HUC
  tar_target(p6_huc1_nhdplus_attr_vals_tbl,
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = p6_huc1_upstream_unqiuecomids),
             pattern = map(p1_nhdplus_attr_list),
             iteration = 'list'),
  # Pivot & combine NHD+ attributes
  tar_target(p6_huc1_attr_nhd, prepare_nhd_attributes(p6_huc1_nhdplus_attr_vals_tbl, NULL)),
  
  # Download NHD+ attributes data for each COMID in HUC
  tar_target(p6_huc2_nhdplus_attr_vals_tbl,
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = p6_huc2_upstream_unqiuecomids),
             pattern = map(p1_nhdplus_attr_list),
             iteration = 'list'),
  # Pivot & combine NHD+ attributes
  tar_target(p6_huc2_attr_nhd, prepare_nhd_attributes(p6_huc2_nhdplus_attr_vals_tbl, NULL)),
  
  # Download NHD+ attributes data for each COMID in HUC
  tar_target(p6_huc3_nhdplus_attr_vals_tbl,
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = p6_huc3_upstream_unqiuecomids),
             pattern = map(p1_nhdplus_attr_list),
             iteration = 'list'),
  # Pivot & combine NHD+ attributes
  tar_target(p6_huc3_attr_nhd, prepare_nhd_attributes(p6_huc3_nhdplus_attr_vals_tbl, NULL)),
  
  # Download NHD+ attributes data for each COMID in HUC
  tar_target(p6_huc4_nhdplus_attr_vals_tbl,
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = p6_huc4_upstream_unqiuecomids),
             pattern = map(p1_nhdplus_attr_list),
             iteration = 'list'),
  # Pivot & combine NHD+ attributes
  tar_target(p6_huc4_attr_nhd, prepare_nhd_attributes(p6_huc4_nhdplus_attr_vals_tbl, NULL)),
  
  # Download NHD+ attributes data for each COMID in HUC
  tar_target(p6_huc5_nhdplus_attr_vals_tbl,
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = p6_huc5_upstream_unqiuecomids),
             pattern = map(p1_nhdplus_attr_list),
             iteration = 'list'),
  # Pivot & combine NHD+ attributes
  tar_target(p6_huc5_attr_nhd, prepare_nhd_attributes(p6_huc5_nhdplus_attr_vals_tbl, NULL)),
  
  # Download NHD+ attributes data for each COMID in HUC
  tar_target(p6_huc6_nhdplus_attr_vals_tbl,
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = p6_huc6_upstream_unqiuecomids),
             pattern = map(p1_nhdplus_attr_list),
             iteration = 'list'),
  # Pivot & combine NHD+ attributes
  tar_target(p6_huc6_attr_nhd, prepare_nhd_attributes(p6_huc6_nhdplus_attr_vals_tbl, NULL)),
  
  # Download NHD+ attributes data for each COMID in HUC
  tar_target(p6_huc7_nhdplus_attr_vals_tbl,
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = p6_huc7_upstream_unqiuecomids),
             pattern = map(p1_nhdplus_attr_list),
             iteration = 'list'),
  # Pivot & combine NHD+ attributes
  tar_target(p6_huc7_attr_nhd, prepare_nhd_attributes(p6_huc7_nhdplus_attr_vals_tbl, NULL)),
  
  # Download NHD+ attributes data for each COMID in HUC
  tar_target(p6_huc8_nhdplus_attr_vals_tbl,
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = p6_huc8_upstream_unqiuecomids),
             pattern = map(p1_nhdplus_attr_list),
             iteration = 'list'),
  # Pivot & combine NHD+ attributes
  tar_target(p6_huc8_attr_nhd, prepare_nhd_attributes(p6_huc8_nhdplus_attr_vals_tbl, NULL)),
  
  # Download NHD+ attributes data for each COMID in HUC
  tar_target(p6_huc9_nhdplus_attr_vals_tbl,
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = p6_huc9_upstream_unqiuecomids),
             pattern = map(p1_nhdplus_attr_list),
             iteration = 'list'),
  # Pivot & combine NHD+ attributes
  tar_target(p6_huc9_attr_nhd, prepare_nhd_attributes(p6_huc9_nhdplus_attr_vals_tbl, NULL)),
  
  # Download NHD+ attributes data for each COMID in HUC
  tar_target(p6_huc10_nhdplus_attr_vals_tbl,
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = p6_huc10_upstream_unqiuecomids),
             pattern = map(p1_nhdplus_attr_list),
             iteration = 'list'),
  # Pivot & combine NHD+ attributes
  tar_target(p6_huc10_attr_nhd, prepare_nhd_attributes(p6_huc10_nhdplus_attr_vals_tbl, NULL)),
  
  # Download NHD+ attributes data for each COMID in HUC
  tar_target(p6_huc11_nhdplus_attr_vals_tbl,
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = p6_huc11_upstream_unqiuecomids),
             pattern = map(p1_nhdplus_attr_list),
             iteration = 'list'),
  # Pivot & combine NHD+ attributes
  tar_target(p6_huc11_attr_nhd, prepare_nhd_attributes(p6_huc11_nhdplus_attr_vals_tbl, NULL)),
  
  ################## 4) Link to catchments and calculate basin areas  ##################
  # Filter NHD+ catchment polygons to just the COMIDs used here
  tar_target(p6_huc1_catchment_sf, p1_nhdplus_catchments_sf %>% filter(nhd_comid %in% p6_huc1_upstream_unqiuecomids)),
  # Calculate the total area of each catchment & its upstream catchments
  tar_target(p6_huc1_attr_basinArea, calculate_catchment_areas(polys_sf = p6_huc1_catchment_sf,
                                                          comid_upstream_tbl = p6_huc1_upstream_comids_df,
                                                          comid_site_xwalk = NULL)),
  # Filter NHD+ catchment polygons to just the COMIDs used here
  tar_target(p6_huc2_catchment_sf, p1_nhdplus_catchments_sf %>% filter(nhd_comid %in% p6_huc2_upstream_unqiuecomids)),
  # Calculate the total area of each catchment & its upstream catchments
  tar_target(p6_huc2_attr_basinArea, calculate_catchment_areas(polys_sf = p6_huc2_catchment_sf,
                                                               comid_upstream_tbl = p6_huc2_upstream_comids_df,
                                                               comid_site_xwalk = NULL)),
  # Filter NHD+ catchment polygons to just the COMIDs used here
  tar_target(p6_huc3_catchment_sf, p1_nhdplus_catchments_sf %>% filter(nhd_comid %in% p6_huc3_upstream_unqiuecomids)),
  # Calculate the total area of each catchment & its upstream catchments
  tar_target(p6_huc3_attr_basinArea, calculate_catchment_areas(polys_sf = p6_huc3_catchment_sf,
                                                               comid_upstream_tbl = p6_huc3_upstream_comids_df,
                                                               comid_site_xwalk = NULL)),
  # Filter NHD+ catchment polygons to just the COMIDs used here
  tar_target(p6_huc4_catchment_sf, p1_nhdplus_catchments_sf %>% filter(nhd_comid %in% p6_huc4_upstream_unqiuecomids)),
  # Calculate the total area of each catchment & its upstream catchments
  tar_target(p6_huc4_attr_basinArea, calculate_catchment_areas(polys_sf = p6_huc4_catchment_sf,
                                                               comid_upstream_tbl = p6_huc4_upstream_comids_df,
                                                               comid_site_xwalk = NULL)),
  # Filter NHD+ catchment polygons to just the COMIDs used here
  tar_target(p6_huc5_catchment_sf, p1_nhdplus_catchments_sf %>% filter(nhd_comid %in% p6_huc5_upstream_unqiuecomids)),
  # Calculate the total area of each catchment & its upstream catchments
  tar_target(p6_huc5_attr_basinArea, calculate_catchment_areas(polys_sf = p6_huc5_catchment_sf,
                                                               comid_upstream_tbl = p6_huc5_upstream_comids_df,
                                                               comid_site_xwalk = NULL)),
  # Filter NHD+ catchment polygons to just the COMIDs used here
  tar_target(p6_huc6_catchment_sf, p1_nhdplus_catchments_sf %>% filter(nhd_comid %in% p6_huc6_upstream_unqiuecomids)),
  # Calculate the total area of each catchment & its upstream catchments
  tar_target(p6_huc6_attr_basinArea, calculate_catchment_areas(polys_sf = p6_huc6_catchment_sf,
                                                               comid_upstream_tbl = p6_huc6_upstream_comids_df,
                                                               comid_site_xwalk = NULL)),
  # Filter NHD+ catchment polygons to just the COMIDs used here
  tar_target(p6_huc7_catchment_sf, p1_nhdplus_catchments_sf %>% filter(nhd_comid %in% p6_huc7_upstream_unqiuecomids)),
  # Calculate the total area of each catchment & its upstream catchments
  tar_target(p6_huc7_attr_basinArea, calculate_catchment_areas(polys_sf = p6_huc7_catchment_sf,
                                                               comid_upstream_tbl = p6_huc7_upstream_comids_df,
                                                               comid_site_xwalk = NULL)),
  # Filter NHD+ catchment polygons to just the COMIDs used here
  tar_target(p6_huc8_catchment_sf, p1_nhdplus_catchments_sf %>% filter(nhd_comid %in% p6_huc8_upstream_unqiuecomids)),
  # Calculate the total area of each catchment & its upstream catchments
  tar_target(p6_huc8_attr_basinArea, calculate_catchment_areas(polys_sf = p6_huc8_catchment_sf,
                                                               comid_upstream_tbl = p6_huc8_upstream_comids_df,
                                                               comid_site_xwalk = NULL)),
  # Filter NHD+ catchment polygons to just the COMIDs used here
  tar_target(p6_huc9_catchment_sf, p1_nhdplus_catchments_sf %>% filter(nhd_comid %in% p6_huc9_upstream_unqiuecomids)),
  # Calculate the total area of each catchment & its upstream catchments
  tar_target(p6_huc9_attr_basinArea, calculate_catchment_areas(polys_sf = p6_huc9_catchment_sf,
                                                               comid_upstream_tbl = p6_huc9_upstream_comids_df,
                                                               comid_site_xwalk = NULL)),
  # Filter NHD+ catchment polygons to just the COMIDs used here
  tar_target(p6_huc10_catchment_sf, p1_nhdplus_catchments_sf %>% filter(nhd_comid %in% p6_huc10_upstream_unqiuecomids)),
  # Calculate the total area of each catchment & its upstream catchments
  tar_target(p6_huc10_attr_basinArea, calculate_catchment_areas(polys_sf = p6_huc10_catchment_sf,
                                                               comid_upstream_tbl = p6_huc10_upstream_comids_df,
                                                               comid_site_xwalk = NULL)),
  # Filter NHD+ catchment polygons to just the COMIDs used here
  tar_target(p6_huc11_catchment_sf, p1_nhdplus_catchments_sf %>% filter(nhd_comid %in% p6_huc11_upstream_unqiuecomids)),
  # Calculate the total area of each catchment & its upstream catchments
  tar_target(p6_huc11_attr_basinArea, calculate_catchment_areas(polys_sf = p6_huc11_catchment_sf,
                                                               comid_upstream_tbl = p6_huc11_upstream_comids_df,
                                                               comid_site_xwalk = NULL)),
  
  ################## 5) Get road salt per catchment ##################
  # This includes any catchments that will only be used for upstream calculations
  # Road salt raster not passed to function due to problems with temporary directories 
  tar_target(p6_huc1_catchment_salt, aggregate_road_salt_per_poly(road_salt_rast = NULL, 
                                                                  polys_sf = p6_huc1_catchment_sf,
                                                                  rasterTifs = p1_sb_road_salt_tif)),
  tar_target(p6_huc2_catchment_salt, aggregate_road_salt_per_poly(road_salt_rast = NULL, 
                                                                  polys_sf = p6_huc2_catchment_sf,
                                                                  rasterTifs = p1_sb_road_salt_tif)),
  tar_target(p6_huc3_catchment_salt, aggregate_road_salt_per_poly(road_salt_rast = NULL, 
                                                                  polys_sf = p6_huc3_catchment_sf,
                                                                  rasterTifs = p1_sb_road_salt_tif)),
  tar_target(p6_huc4_catchment_salt, aggregate_road_salt_per_poly(road_salt_rast = NULL, 
                                                                  polys_sf = p6_huc4_catchment_sf,
                                                                  rasterTifs = p1_sb_road_salt_tif)),
  tar_target(p6_huc5_catchment_salt, aggregate_road_salt_per_poly(road_salt_rast = NULL, 
                                                                  polys_sf = p6_huc5_catchment_sf,
                                                                  rasterTifs = p1_sb_road_salt_tif)),
  tar_target(p6_huc6_catchment_salt, aggregate_road_salt_per_poly(road_salt_rast = NULL, 
                                                                  polys_sf = p6_huc6_catchment_sf,
                                                                  rasterTifs = p1_sb_road_salt_tif)),
  tar_target(p6_huc7_catchment_salt, aggregate_road_salt_per_poly(road_salt_rast = NULL, 
                                                                  polys_sf = p6_huc7_catchment_sf,
                                                                  rasterTifs = p1_sb_road_salt_tif)),
  tar_target(p6_huc8_catchment_salt, aggregate_road_salt_per_poly(road_salt_rast = NULL, 
                                                                  polys_sf = p6_huc8_catchment_sf,
                                                                  rasterTifs = p1_sb_road_salt_tif)),
  tar_target(p6_huc9_catchment_salt, aggregate_road_salt_per_poly(road_salt_rast = NULL, 
                                                                  polys_sf = p6_huc9_catchment_sf,
                                                                  rasterTifs = p1_sb_road_salt_tif)),
  tar_target(p6_huc10_catchment_salt, aggregate_road_salt_per_poly(road_salt_rast = NULL, 
                                                                  polys_sf = p6_huc10_catchment_sf,
                                                                  rasterTifs = p1_sb_road_salt_tif)),
  tar_target(p6_huc11_catchment_salt, aggregate_road_salt_per_poly(road_salt_rast = NULL, 
                                                                  polys_sf = p6_huc11_catchment_sf,
                                                                  rasterTifs = p1_sb_road_salt_tif)),
  
  # Then, map salt for each NHD COMID catchment polygon to sites and calculate cumulative road salt
  tar_target(p6_huc1_attr_roadSalt, map_catchment_roadSalt_to_site(road_salt_comid = p6_huc1_catchment_salt, 
                                                              basin_areas = p6_huc1_attr_basinArea,
                                                              comid_site_xwalk = NULL,
                                                              comid_upstream_tbl = p6_huc1_upstream_comids_df) %>% 
                                    select(nhd_comid, attr_roadSaltCumulativePerSqKm)),
  tar_target(p6_huc2_attr_roadSalt, map_catchment_roadSalt_to_site(road_salt_comid = p6_huc2_catchment_salt, 
                                                                   basin_areas = p6_huc2_attr_basinArea,
                                                                   comid_site_xwalk = NULL,
                                                                   comid_upstream_tbl = p6_huc2_upstream_comids_df) %>% 
               select(nhd_comid, attr_roadSaltCumulativePerSqKm)),
  tar_target(p6_huc3_attr_roadSalt, map_catchment_roadSalt_to_site(road_salt_comid = p6_huc3_catchment_salt, 
                                                                   basin_areas = p6_huc3_attr_basinArea,
                                                                   comid_site_xwalk = NULL,
                                                                   comid_upstream_tbl = p6_huc3_upstream_comids_df) %>% 
               select(nhd_comid, attr_roadSaltCumulativePerSqKm)),
  tar_target(p6_huc4_attr_roadSalt, map_catchment_roadSalt_to_site(road_salt_comid = p6_huc4_catchment_salt, 
                                                                   basin_areas = p6_huc4_attr_basinArea,
                                                                   comid_site_xwalk = NULL,
                                                                   comid_upstream_tbl = p6_huc4_upstream_comids_df) %>% 
               select(nhd_comid, attr_roadSaltCumulativePerSqKm)),
  tar_target(p6_huc5_attr_roadSalt, map_catchment_roadSalt_to_site(road_salt_comid = p6_huc5_catchment_salt, 
                                                                   basin_areas = p6_huc5_attr_basinArea,
                                                                   comid_site_xwalk = NULL,
                                                                   comid_upstream_tbl = p6_huc5_upstream_comids_df) %>% 
               select(nhd_comid, attr_roadSaltCumulativePerSqKm)),
  tar_target(p6_huc6_attr_roadSalt, map_catchment_roadSalt_to_site(road_salt_comid = p6_huc6_catchment_salt, 
                                                                   basin_areas = p6_huc6_attr_basinArea,
                                                                   comid_site_xwalk = NULL,
                                                                   comid_upstream_tbl = p6_huc6_upstream_comids_df) %>% 
               select(nhd_comid, attr_roadSaltCumulativePerSqKm)),
  tar_target(p6_huc7_attr_roadSalt, map_catchment_roadSalt_to_site(road_salt_comid = p6_huc7_catchment_salt, 
                                                                   basin_areas = p6_huc7_attr_basinArea,
                                                                   comid_site_xwalk = NULL,
                                                                   comid_upstream_tbl = p6_huc7_upstream_comids_df) %>% 
               select(nhd_comid, attr_roadSaltCumulativePerSqKm)),
  tar_target(p6_huc8_attr_roadSalt, map_catchment_roadSalt_to_site(road_salt_comid = p6_huc8_catchment_salt, 
                                                                   basin_areas = p6_huc8_attr_basinArea,
                                                                   comid_site_xwalk = NULL,
                                                                   comid_upstream_tbl = p6_huc8_upstream_comids_df) %>% 
               select(nhd_comid, attr_roadSaltCumulativePerSqKm)),
  tar_target(p6_huc9_attr_roadSalt, map_catchment_roadSalt_to_site(road_salt_comid = p6_huc9_catchment_salt, 
                                                                   basin_areas = p6_huc9_attr_basinArea,
                                                                   comid_site_xwalk = NULL,
                                                                   comid_upstream_tbl = p6_huc9_upstream_comids_df) %>% 
               select(nhd_comid, attr_roadSaltCumulativePerSqKm)),
  tar_target(p6_huc10_attr_roadSalt, map_catchment_roadSalt_to_site(road_salt_comid = p6_huc10_catchment_salt, 
                                                                   basin_areas = p6_huc10_attr_basinArea,
                                                                   comid_site_xwalk = NULL,
                                                                   comid_upstream_tbl = p6_huc10_upstream_comids_df) %>% 
               select(nhd_comid, attr_roadSaltCumulativePerSqKm)),
  tar_target(p6_huc11_attr_roadSalt, map_catchment_roadSalt_to_site(road_salt_comid = p6_huc11_catchment_salt, 
                                                                   basin_areas = p6_huc11_attr_basinArea,
                                                                   comid_site_xwalk = NULL,
                                                                   comid_upstream_tbl = p6_huc11_upstream_comids_df) %>% 
               select(nhd_comid, attr_roadSaltCumulativePerSqKm)),
  
  
  ############### 6) Prepare the attributes from Zell and Sanford 2020 are based on NHD+ COMIDs #############
  tar_target(p6_huc1_attr_depth2wt_trnmsv, prepare_sb_gw_attrs(p1_sb_depth2wt_csv,
                                                          p1_sb_transmissivity_csv,
                                                          tibble(nhd_comid = p6_huc1_upstream_unqiuecomids),
                                                          returnSite = FALSE)),
  tar_target(p6_huc2_attr_depth2wt_trnmsv, prepare_sb_gw_attrs(p1_sb_depth2wt_csv,
                                                               p1_sb_transmissivity_csv,
                                                               tibble(nhd_comid = p6_huc2_upstream_unqiuecomids),
                                                               returnSite = FALSE)),

  tar_target(p6_huc3_attr_depth2wt_trnmsv, prepare_sb_gw_attrs(p1_sb_depth2wt_csv,
                                                               p1_sb_transmissivity_csv,
                                                               tibble(nhd_comid = p6_huc3_upstream_unqiuecomids),
                                                               returnSite = FALSE)),
  
  tar_target(p6_huc4_attr_depth2wt_trnmsv, prepare_sb_gw_attrs(p1_sb_depth2wt_csv,
                                                               p1_sb_transmissivity_csv,
                                                               tibble(nhd_comid = p6_huc4_upstream_unqiuecomids),
                                                               returnSite = FALSE)),
  
  tar_target(p6_huc5_attr_depth2wt_trnmsv, prepare_sb_gw_attrs(p1_sb_depth2wt_csv,
                                                               p1_sb_transmissivity_csv,
                                                               tibble(nhd_comid = p6_huc5_upstream_unqiuecomids),
                                                               returnSite = FALSE)),
  
  tar_target(p6_huc6_attr_depth2wt_trnmsv, prepare_sb_gw_attrs(p1_sb_depth2wt_csv,
                                                               p1_sb_transmissivity_csv,
                                                               tibble(nhd_comid = p6_huc6_upstream_unqiuecomids),
                                                               returnSite = FALSE)),
  
  tar_target(p6_huc7_attr_depth2wt_trnmsv, prepare_sb_gw_attrs(p1_sb_depth2wt_csv,
                                                               p1_sb_transmissivity_csv,
                                                               tibble(nhd_comid = p6_huc7_upstream_unqiuecomids),
                                                               returnSite = FALSE)),
  
  tar_target(p6_huc8_attr_depth2wt_trnmsv, prepare_sb_gw_attrs(p1_sb_depth2wt_csv,
                                                               p1_sb_transmissivity_csv,
                                                               tibble(nhd_comid = p6_huc8_upstream_unqiuecomids),
                                                               returnSite = FALSE)),
  
  tar_target(p6_huc9_attr_depth2wt_trnmsv, prepare_sb_gw_attrs(p1_sb_depth2wt_csv,
                                                               p1_sb_transmissivity_csv,
                                                               tibble(nhd_comid = p6_huc9_upstream_unqiuecomids),
                                                               returnSite = FALSE)),
  
  tar_target(p6_huc10_attr_depth2wt_trnmsv, prepare_sb_gw_attrs(p1_sb_depth2wt_csv,
                                                               p1_sb_transmissivity_csv,
                                                               tibble(nhd_comid = p6_huc10_upstream_unqiuecomids),
                                                               returnSite = FALSE)),
  
  tar_target(p6_huc11_attr_depth2wt_trnmsv, prepare_sb_gw_attrs(p1_sb_depth2wt_csv,
                                                               p1_sb_transmissivity_csv,
                                                               tibble(nhd_comid = p6_huc11_upstream_unqiuecomids),
                                                               returnSite = FALSE)),

  
  # Didn't worry about downloading Flow from NOAA's National Water Model because
  # the downloads were really slow and the final optimized model from May 2024
  # did not include or need Flow. 
  
  #################### 7) Combine all attributes into a single table for each HUC-2 ###################
  tar_target(p6_huc1_attr_all, combine_static_attributes(joinby = 'nhd_comid',
                                                    p6_huc1_attr_basinArea %>% select(nhd_comid, attr_areaCumulativeSqKm),
                                                    p6_huc1_attr_roadSalt,
                                                    p6_huc1_attr_nhd,
                                                    p6_huc1_attr_depth2wt_trnmsv) %>% 
               # Drop the `attr_` prefix so that the results are cleaner
               rename_with(~gsub("attr_", "", .x))),
  tar_target(p6_huc2_attr_all, combine_static_attributes(joinby = 'nhd_comid',
                                                         p6_huc2_attr_basinArea %>% select(nhd_comid, attr_areaCumulativeSqKm),
                                                         p6_huc2_attr_roadSalt,
                                                         p6_huc2_attr_nhd,
                                                         p6_huc2_attr_depth2wt_trnmsv) %>% 
               # Drop the `attr_` prefix so that the results are cleaner
               rename_with(~gsub("attr_", "", .x))),
  tar_target(p6_huc3_attr_all, combine_static_attributes(joinby = 'nhd_comid',
                                                         p6_huc3_attr_basinArea %>% select(nhd_comid, attr_areaCumulativeSqKm),
                                                         p6_huc3_attr_roadSalt,
                                                         p6_huc3_attr_nhd,
                                                         p6_huc3_attr_depth2wt_trnmsv) %>% 
               # Drop the `attr_` prefix so that the results are cleaner
               rename_with(~gsub("attr_", "", .x))),
  tar_target(p6_huc4_attr_all, combine_static_attributes(joinby = 'nhd_comid',
                                                         p6_huc4_attr_basinArea %>% select(nhd_comid, attr_areaCumulativeSqKm),
                                                         p6_huc4_attr_roadSalt,
                                                         p6_huc4_attr_nhd,
                                                         p6_huc4_attr_depth2wt_trnmsv) %>% 
               # Drop the `attr_` prefix so that the results are cleaner
               rename_with(~gsub("attr_", "", .x))),
  tar_target(p6_huc5_attr_all, combine_static_attributes(joinby = 'nhd_comid',
                                                         p6_huc5_attr_basinArea %>% select(nhd_comid, attr_areaCumulativeSqKm),
                                                         p6_huc5_attr_roadSalt,
                                                         p6_huc5_attr_nhd,
                                                         p6_huc5_attr_depth2wt_trnmsv) %>% 
               # Drop the `attr_` prefix so that the results are cleaner
               rename_with(~gsub("attr_", "", .x))),
  tar_target(p6_huc6_attr_all, combine_static_attributes(joinby = 'nhd_comid',
                                                         p6_huc6_attr_basinArea %>% select(nhd_comid, attr_areaCumulativeSqKm),
                                                         p6_huc6_attr_roadSalt,
                                                         p6_huc6_attr_nhd,
                                                         p6_huc6_attr_depth2wt_trnmsv) %>% 
               # Drop the `attr_` prefix so that the results are cleaner
               rename_with(~gsub("attr_", "", .x))),
  tar_target(p6_huc7_attr_all, combine_static_attributes(joinby = 'nhd_comid',
                                                         p6_huc7_attr_basinArea %>% select(nhd_comid, attr_areaCumulativeSqKm),
                                                         p6_huc7_attr_roadSalt,
                                                         p6_huc7_attr_nhd,
                                                         p6_huc7_attr_depth2wt_trnmsv) %>% 
               # Drop the `attr_` prefix so that the results are cleaner
               rename_with(~gsub("attr_", "", .x))),
  tar_target(p6_huc8_attr_all, combine_static_attributes(joinby = 'nhd_comid',
                                                         p6_huc8_attr_basinArea %>% select(nhd_comid, attr_areaCumulativeSqKm),
                                                         p6_huc8_attr_roadSalt,
                                                         p6_huc8_attr_nhd,
                                                         p6_huc8_attr_depth2wt_trnmsv) %>% 
               # Drop the `attr_` prefix so that the results are cleaner
               rename_with(~gsub("attr_", "", .x))),
  tar_target(p6_huc9_attr_all, combine_static_attributes(joinby = 'nhd_comid',
                                                         p6_huc9_attr_basinArea %>% select(nhd_comid, attr_areaCumulativeSqKm),
                                                         p6_huc9_attr_roadSalt,
                                                         p6_huc9_attr_nhd,
                                                         p6_huc9_attr_depth2wt_trnmsv) %>% 
               # Drop the `attr_` prefix so that the results are cleaner
               rename_with(~gsub("attr_", "", .x))),
  tar_target(p6_huc10_attr_all, combine_static_attributes(joinby = 'nhd_comid',
                                                         p6_huc10_attr_basinArea %>% select(nhd_comid, attr_areaCumulativeSqKm),
                                                         p6_huc10_attr_roadSalt,
                                                         p6_huc10_attr_nhd,
                                                         p6_huc10_attr_depth2wt_trnmsv) %>% 
               # Drop the `attr_` prefix so that the results are cleaner
               rename_with(~gsub("attr_", "", .x))),
  tar_target(p6_huc11_attr_all, combine_static_attributes(joinby = 'nhd_comid',
                                                         p6_huc11_attr_basinArea %>% select(nhd_comid, attr_areaCumulativeSqKm),
                                                         p6_huc11_attr_roadSalt,
                                                         p6_huc11_attr_nhd,
                                                         p6_huc11_attr_depth2wt_trnmsv) %>% 
               # Drop the `attr_` prefix so that the results are cleaner
               rename_with(~gsub("attr_", "", .x))), 
  
  ################### 8) Combine individual HUC tables into one full attribute table ######################
  tar_target(p6_attr_all, bind_rows(p6_huc1_attr_all,p6_huc2_attr_all,p6_huc3_attr_all,p6_huc4_attr_all,
                                    p6_huc5_attr_all,p6_huc6_attr_all,p6_huc7_attr_all,p6_huc8_attr_all,
                                    p6_huc9_attr_all,p6_huc10_attr_all,p6_huc11_attr_all)),
  
  
  ############### 9) Use the model and attributes to predict episodic/not episodic ##############

  # Verify the attributes developed in p6_ match what is needed by the optimized model
  tar_target(p6_verify_model_attrs, {
    attrs_model <- row.names(p5_rf_model_optimized$importance)
    stopifnot(all(attrs_model %in% names(p6_attr_all)))
  }),

  tar_target(p6_predict_episodic, p6_attr_all %>%
               mutate(pred = as.character(predict(p5_rf_model_optimized, .))) %>%
               replace_na(list(pred = 'Not classified')) %>% 
               mutate(pred_fct = factor(pred, 
                                        levels = c('high', 'low', 'none'))) %>% 
               select(nhd_comid, pred, pred_fct) %>% 
               distinct()),
  
  # Pull out COMIDs that were modeled as a vector
  tar_target(p6_predicted_comid, p6_attr_all %>% pull(nhd_comid) %>% unique()),
  
  # Find their streamorder so that we can filter to bigger streams for mapping
  tar_target(p6_predicted_comid_streamorder, bind_rows(p6_huc_flowlines_sf) %>%
               st_drop_geometry() %>%
               filter(COMID %in% p6_predicted_comid) %>%
               select(nhd_comid = COMID, streamorder = StreamOrde) %>% 
               distinct())

)
# 
# 
#   
#   
#   
#   
#   

# 
# # Pull out COMIDs that were modeled as a vector
# tar_target(p6_predicted_comid, p6_attr_all %>% pull(nhd_comid) %>% unique()),
# 
# # Find their streamorder so that we can filter to bigger streams for mapping
# tar_target(p6_predicted_comid_streamorder, huc_flowlines %>%
#              st_drop_geometry() %>%
#              filter(nhd_comid %in% p6_predicted_comid) %>%
#              select(nhd_comid, streamorder = StreamOrde)),
# 
# # Can easily use `p6_predicted_comid_lengths` to group_by
# tar_target(p6_comid_lengths, huc_flowlines %>%
#              st_drop_geometry() %>%
#              select(nhd_comid, reach_length_km = LENGTHKM)),
# tar_target(p6_predicted_comid_lengths,
#            p6_predict_episodic %>%
#              left_join(p6_comid_lengths, by = 'nhd_comid')),
# tar_target(p6_predicted_comid_lengths_summary,
#            p6_state_comids %>%
#              left_join(p6_predicted_comid_lengths, by = 'nhd_comid') %>%
#              group_by(pred, region) %>%
#              summarize(total_length_km = sum(reach_length_km, na.rm=T),
#                        .groups='keep'))



