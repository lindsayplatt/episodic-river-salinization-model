
#' @title Use a downloaded network to identify upstream COMIDs
#' @description Rather than query the webservice using the `navigate_nldi()`
#' function, employ hydroloom functionality to query a local network of flowlines
#' with `toid` in order to identify the upstream COMIDs for a given COMID as 
#' the outlet. Similar to `identify_upstream_comids()` but doesn't hit a web 
#' service each time it is called. If a COMID is the furthest upstream, only 
#' one row will be returned and the same COMID will be in both columns.
#' 
#' @param comid_in a single character string representing the NHD COMID
#' @param flines_network a data.frame with at least one column giving the 
#' identifier and one called `toid` declaring the downstream flowline. See 
#' `?nhdplusTools::get_sorted()` for more information.
#' 
#' @return a tibble of COMIDs with three columns - `nhd_comid` which includes the
#' COMID passed into the function and `nhd_comid_upstream` which are the COMIDs
#' found to be upstream of the value in `nhd_comid`. Note that values in 
#' `nhd_comid` will repeat as necessary to align with however many 
#' `nhd_comid_upstream` values they have.
#' 
#' 
identify_upstream_comids_hy <- function(comid_in, flines_network, index_ids, froms) {
  # Using `split = TRUE` to get the terminalID
  get_sorted_HD(flines_network, outlets = comid_in, split=TRUE, index_ids, froms) %>% 
  dplyr::select(nhd_comid = terminalID, 
           nhd_comid_upstream = comid)
}

#' @title Get Sorted Network
#' @description given a tree with an id and and toid in the
#' first and second columns, returns a sorted and potentially
#' split set of output.
#'
#' Can also be used as a very fast implementation of upstream
#' with tributaries navigation. The full network from each
#' outlet is returned in sorted order.
#' @param x data.frame with an identifier and to identifier in the
#' first and second columns.
#' @param split logical if TRUE, the result will be split into
#' independent networks identified by the id of their outlet. The
#' outlet id of each independent network is added as a "terminalID"
#' attribute.
#' @param outlets same as id in x; if specified only the network
#' emanating from these outlets will be considered and returned.
#' @return data.frame containing a topologically sorted version
#' of the requested network and optionally a terminal id.
get_sorted_HD <- function(flines_network, split, outlets = NULL, index_ids, froms) {
  
  orig_names <- names(flines_network)[1:2]
  
  names(flines_network)[1:2] <- c("id", "toid")
  
  flines_network = sort_network_hy_HD(flines_network, split, outlets, index_ids, froms)
  
  names(flines_network)[1:2] <- orig_names
  
  if(split) {
    names(flines_network)[names(flines_network) == "terminal_id"] <- "terminalID"
  }
  return(flines_network)
}

#' @title sort network
#' @description given a network with an id and and toid, returns a sorted
#' and potentially split set of output.
#'
#' Can also be used as a very fast implementation of upstream
#' with tributaries navigation. The full network from each
#' outlet is returned in sorted order.
#'
#' If a network includes diversions, all flowlines downstream of
#' the diversion are visited prior to continuing upstream. See
#' note on the `outlets` parameter for implications of this
#' implementation detail.
#' @param split logical if TRUE, the result will be split into
#' independent networks identified by the id of their outlet. The
#' outlet id of each independent network is added as a "terminalid"
#' attribute.
#' @param outlets same as id in x. if specified, only the network
#' emanating from these outlets will be considered and returned.
#' NOTE: If outlets does not include all outlets from a given
#' network containing diversions, a partial network may be returned.
#' @returns data.frame containing a topologically sorted version
#' of the requested network and optionally a terminal id.
sort_network_hy_HD <- function(x, split, outlets = NULL, index_ids, froms) {
  x <- hy(x)
  
  # hy_g <- get_hyg(x, add = TRUE, id = id)
  
  x <- select(st_drop_geometry(x), id, toid, everything())
  
  
  if(!is.null(outlets)) {
    starts <- which(index_ids$to_list$id %in% outlets)
  } else {
    # All the start nodes
    if(any(x$toid != get_outlet_value(x) & !x$toid %in% x$id)) {
      warning("no outlet found -- will start from outlets that go no where.")
      starts <- which(index_ids$to_list$id %in% x$id[!x$toid %in% x$id])
    } else {
      starts <- which(index_ids$to_list$id %in% x$id[x$toid == get_outlet_value(x)])
    }
  }
  # Some vectors to track results
  to_visit <- out <- rep(0, length(index_ids$to_list$id))
  
  # Use to track if a node is ready to be visited.
  # will subtract from this and not visit the upstream until ready element = 1
  ready <- index_ids$lengths
  
  if(split) {
    set <- out
    out_list <- rep(list(list()), length(starts))
  }
  
  
  # output order tracker
  o <- 1
  set_id <- 1
  
  for(s in starts) {
    
    # Set up the starting node
    node <- s
    
    # within set node tracker for split = TRUE
    n <- 1
    # v is a pointer into the to_visit vector
    v <- 1
    
    trk <- 1
    
    while(v > 0) {
      
      # track the order that nodes were visited
      out[node] <- o
      # increment to the next node
      o <- o + 1
      
      if(split) {
        set[n] <- node
        n <- n + 1
      }
      
      # loop over upstream catchments
      # does nothing if froms_l[node] == 0
      
      for(from in seq_len(froms$lengths[node])) {
        
        # grab the next upstream node
        next_node <- froms$froms[from, node]
        
        # check if we have a node to visit
        # not needed? was in the if below node <= ncol(froms$froms) &&
        if(!is.na(next_node)) {
          
          if(ready[next_node] == 1) {
            # Add the next node to visit to the tracking vector
            to_visit[v] <- next_node
            
            v <- v + 1
          } else {
            # we don't want to visit an upstream neighbor until all its
            # downstream neighbors have been visited. Ready is initialized
            # to the length of downstream neighbors and provides a check.
            ready[next_node] <- ready[next_node] - 1
          }
          
        }}
      
      # go to the last element added in to_visit
      v <- v - 1
      node <- to_visit[v]
      
      trk <- trk + 1
      
      if(trk > length(index_ids$to_list$id) * 2) {
        stop("runaway while loop, something wrong with the network?")
      }
      
    }
    
    if(split) {
      out_list[[set_id]] <- index_ids$to_list$id[set[1:(n - 1)]]
      set_id <- set_id + 1
    }
  }
  
  if(split) names(out_list) <- index_ids$to_list$id[starts]
  
  ### rewrites x into the correct order. ###
  id_order <- unique(x$id)[which(out != 0)]
  out <- out[out != 0]
  
  if(split & o - 1 != length(id_order)) stop("Are two or more outlets within the same network?")
  
  if(is.null(outlets) && length(unique(x$id)) != length(out)) warning("some features missed in sort. Are there loops in the network?")
  
  x <- filter(x, .data$id %in% id_order) |>
    left_join(tibble(id = id_order, sorter = out), by = "id") |>
    arrange(desc(.data$sorter)) |>
    select(-"sorter")
  
  if(split) {
    
    # # this is only two columns
    ids <- as(names(out_list), class(pull(x[1, 1])))

    out_list <- data.frame(ids = ids) |>
      dplyr::mutate(set = out_list) |>
      unnest("set")
    
    names(out_list) <- c('terminal_id', 'id')
    
    ### adds grouping terminalID to x ###
    x <- left_join(x, out_list, by = names(x)[1])
    
  }
  
  # x <- put_hyg(x, hy_g)
  
  x
}

lonlat_to_state <- function(pointsDF,
                            states = spData::us_states,
                            name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
  pointsDF = st_transform(pointsDF, crs = 3857)
  pts <- st_as_sf(pointsDF, crs = 3857)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  # pts <- st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}

