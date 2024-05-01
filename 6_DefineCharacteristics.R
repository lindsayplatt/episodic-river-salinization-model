# Targets for applying a random forest to link attributes
# to each site that is experiencing "episodic salinization"
# or "baseflow salinization" or both.

source('6_DefineCharacteristics/src/prep_attr_randomforest.R')
source('6_DefineCharacteristics/src/apply_randomforest.R')
source('6_DefineCharacteristics/src/evaluate_randomforest.R')
source('6_DefineCharacteristics/src/visualize_attribute_distributions.R')
source('6_DefineCharacteristics/src/visualize_results.R')

p6_targets <- list(
  
  # Set number of attributes to keep during feature selection step 
  tar_target(p6_attr_count, ncol(p3_static_attributes)-1),
  tar_target(p6_attr_cutoff, p6_attr_count/2), 
  
  ##### A. Run random forest for both episodic & baseflow salinization #####
  
  ###### Prep data for RF ######
  
  tar_target(p6a_site_attr, prep_attr_randomforest(p3_static_attributes, p4_episodic_sites, p5_sc_baseflow_trend)),
  tar_target(p6a_site_attr_rf, dplyr::select(p6a_site_attr, -site_no)),
  
  ###### Determine optimal RF configs ######
  
  # Run the different combinations of hyperparameter options for mtry and ntree 5 different times (seed length = 5)
  # and identify the best `mtry` and `ntree` to minimize error
  tar_target(p6a_rf_model_tuning, tune_randomForest_hyperparameters(p6a_site_attr_rf, seed = c(12, 1789, 3019, 618, 104))),
  tar_target(p6a_hypertuned_params, optimize_hyperparameters(p6a_rf_model_tuning)),
  
  # Now run the hypertuned model 
  tar_target(p6a_rf_model_hypertuned, apply_randomforest(p6a_site_attr_rf, p6a_hypertuned_params$mtry, p6a_hypertuned_params$ntree)),
  
  # Perform feature selection by only choosing the top 50% important predictors
  tar_target(p6a_site_attr_rf_optimal, optimize_attrs(p6a_site_attr_rf, p6a_rf_model_hypertuned, 
                                                      n_important = p6_attr_cutoff)),
  
  # Now with the new site attributes, re-tune the model and identify the optimal `mtry` and `ntree` with the new feature set
  tar_target(p6a_rf_model_finetuning, tune_randomForest_hyperparameters(p6a_site_attr_rf_optimal, seed = c(16, 3810, 73, 917, 218))),
  tar_target(p6a_finetuned_params, optimize_hyperparameters(p6a_rf_model_finetuning)),
  
  ###### Run optimal RF ######
  
  tar_target(p6a_rf_model_optimized, apply_randomforest(p6a_site_attr_rf_optimal, p6a_finetuned_params$mtry, p6a_finetuned_params$ntree)),
  
  ###### Evaluate RF output ######
  
  tar_target(p6a_rf_oob, mean(p6a_rf_model_optimized$err.rate[,'OOB'])),
  tar_target(p6a_rf_attr_importance, calculate_attr_importance(p6a_rf_model_optimized)),
  tar_target(p6a_rf_attr_importance_viz, visualize_attr_importance(p6a_rf_attr_importance)),
  tar_target(p6a_rf_attr_partdep, calculate_partial_dependence(p6a_rf_model_optimized, 
                                                              p6a_site_attr_rf_optimal,
                                                              focus_class = "Both")),
  tar_target(p6a_rf_attr_partdep_viz, visualize_partial_dependence(p6a_rf_attr_partdep, p6a_site_attr_rf_optimal)),
  
  ###### Visualize site category attribute distributions ######
  
  tar_target(p6a_attrs_num_viz_all, visualize_numeric_attrs(p6a_site_attr)), # Viz with all attributes!
  tar_target(p6a_attrs_num_viz, visualize_numeric_attrs(p6a_site_attr_rf_optimal)),
  tar_target(p6a_category_map, visualize_catgory_sites_map(p6a_site_attr, p1_nwis_sc_sites_sf, p1_conus_state_cds)),
  
  ##### B. Run random forest for just episodic #####
  
  ###### Prep data for RF ######
  
  tar_target(p6b_site_attr, prep_attr_randomforest(p3_static_attributes, sites_episodic = p4_episodic_sites)),
  tar_target(p6b_site_attr_rf, dplyr::select(p6b_site_attr, -site_no)),
  
  ###### Determine optimal RF configs ######
  
  # Run the different combinations of hyperparameter options for mtry and ntree 5 different times (seed length = 5)
  # and identify the best `mtry` and `ntree` to minimize error
  tar_target(p6b_rf_model_tuning, tune_randomForest_hyperparameters(p6b_site_attr_rf, seed = c(739, 62, 6391, 391, 2))),
  tar_target(p6b_hypertuned_params, optimize_hyperparameters(p6b_rf_model_tuning)),
  
  # Now run the hypertuned model 
  tar_target(p6b_rf_model_hypertuned, apply_randomforest(p6b_site_attr_rf, p6b_hypertuned_params$mtry, p6b_hypertuned_params$ntree)),
  
  # Perform feature selection by only choosing the top 50% important predictors
  tar_target(p6b_site_attr_rf_optimal, optimize_attrs(p6b_site_attr_rf, p6b_rf_model_hypertuned, 
                                                      n_important = p6_attr_cutoff)),
  
  # Now with the new site attributes, re-tune the model and identify the optimal `mtry` and `ntree` with the new feature set
  tar_target(p6b_rf_model_finetuning, tune_randomForest_hyperparameters(p6b_site_attr_rf_optimal, seed = c(281, 9502, 472, 93, 720))),
  tar_target(p6b_finetuned_params, optimize_hyperparameters(p6b_rf_model_finetuning)),
  
  ###### Run optimal RF ######
  
  tar_target(p6b_rf_model_optimized, apply_randomforest(p6b_site_attr_rf_optimal, p6b_finetuned_params$mtry, p6b_finetuned_params$ntree)),
  
  ###### Evaluate RF output ######
  
  tar_target(p6b_rf_oob, mean(p6b_rf_model_optimized$err.rate[,'OOB'])),
  tar_target(p6b_rf_attr_importance, calculate_attr_importance(p6b_rf_model_optimized)),
  tar_target(p6b_rf_attr_importance_viz, visualize_attr_importance(p6b_rf_attr_importance)),
  tar_target(p6b_rf_attr_partdep, calculate_partial_dependence(p6b_rf_model_optimized, 
                                                               p6b_site_attr_rf_optimal,
                                                               focus_class = "Episodic")),
  tar_target(p6b_rf_attr_partdep_viz, visualize_partial_dependence(p6b_rf_attr_partdep, p6b_site_attr_rf_optimal)),
  
  ###### Visualize site category attribute distributions ######
  
  tar_target(p6b_attrs_num_viz, visualize_numeric_attrs(p6b_site_attr_rf_optimal)),
  tar_target(p6b_category_map, visualize_catgory_sites_map(p6b_site_attr, p1_nwis_sc_sites_sf, p1_conus_state_cds)),
  
  ##### C. Run random forest for just baseflow salinization #####
  
  ###### Prep data for RF ######
  
  tar_target(p6c_site_attr, prep_attr_randomforest(p3_static_attributes, site_baseflow_trend_info = p5_sc_baseflow_trend)),
  tar_target(p6c_site_attr_rf, dplyr::select(p6c_site_attr, -site_no)),
  
  ###### Determine optimal RF configs ######
  
  # Run the different combinations of hyperparameter options for mtry and ntree 5 different times (seed length = 5)
  # and identify the best `mtry` and `ntree` to minimize error
  tar_target(p6c_rf_model_tuning, tune_randomForest_hyperparameters(p6c_site_attr_rf, seed = c(819, 83, 618, 1820, 8291))),
  tar_target(p6c_hypertuned_params, optimize_hyperparameters(p6c_rf_model_tuning)),
  
  # Now run the hypertuned model 
  tar_target(p6c_rf_model_hypertuned, apply_randomforest(p6c_site_attr_rf, p6c_hypertuned_params$mtry, p6c_hypertuned_params$ntree)),
  
  # Perform feature selection by only choosing the top 50% important predictors
  tar_target(p6c_site_attr_rf_optimal, optimize_attrs(p6c_site_attr_rf, p6c_rf_model_hypertuned, 
                                                      n_important = p6_attr_cutoff)),
  
  # Now with the new site attributes, re-tune the model and identify the optimal `mtry` and `ntree` with the new feature set
  tar_target(p6c_rf_model_finetuning, tune_randomForest_hyperparameters(p6c_site_attr_rf_optimal, seed = c(6381, 7492, 35, 99, 245))),
  tar_target(p6c_finetuned_params, optimize_hyperparameters(p6c_rf_model_finetuning)),
  
  ###### Run optimal RF ######
  
  tar_target(p6c_rf_model_optimized, apply_randomforest(p6c_site_attr_rf_optimal, p6c_finetuned_params$mtry, p6c_finetuned_params$ntree)),
  
  ###### Evaluate RF output ######
  
  tar_target(p6c_rf_oob, mean(p6c_rf_model_optimized$err.rate[,'OOB'])),
  tar_target(p6c_rf_attr_importance, calculate_attr_importance(p6c_rf_model_optimized)),
  tar_target(p6c_rf_attr_importance_viz, visualize_attr_importance(p6c_rf_attr_importance)),
  tar_target(p6c_rf_attr_partdep, calculate_partial_dependence(p6c_rf_model_optimized, 
                                                               p6c_site_attr_rf_optimal,
                                                               focus_class = "positive")),
  tar_target(p6c_rf_attr_partdep_viz, visualize_partial_dependence(p6c_rf_attr_partdep, p6c_site_attr_rf_optimal)),
  
  ###### Visualize site category attribute distributions ######
  
  tar_target(p6c_attrs_num_viz, visualize_numeric_attrs(p6c_site_attr_rf_optimal)),
  tar_target(p6c_category_map, visualize_catgory_sites_map(p6c_site_attr, p1_nwis_sc_sites_sf, p1_conus_state_cds)),
  
  ##### Save resulting RF outputs as PNGs #####
  
  # Saving in the `log/` folder and committing so that we can git compare future versions
  
  tar_target(p6a_rf_results_png, create_summary_view(
    '6_DefineCharacteristics/log/random_forest_results_combined.png',
    p6a_rf_attr_partdep_viz,
    p6a_attrs_num_viz,
    p6a_rf_attr_importance_viz,
    p6a_category_map
  ), format='file'), 
  
  tar_target(p6b_rf_results_png, create_summary_view(
    '6_DefineCharacteristics/log/random_forest_results_episodic.png',
    p6b_rf_attr_partdep_viz,
    p6b_attrs_num_viz, 
    p6b_rf_attr_importance_viz,
    p6b_category_map
  ), format='file'), 
  
  tar_target(p6c_rf_results_png, create_summary_view(
    '6_DefineCharacteristics/log/random_forest_results_baseflow.png',
    p6c_rf_attr_partdep_viz,
    p6c_attrs_num_viz, 
    p6c_rf_attr_importance_viz,
    p6c_category_map
  ), format='file')
  
)
