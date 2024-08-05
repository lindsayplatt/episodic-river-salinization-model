# Targets for applying a random forest to link attributes
# to each site that is experiencing "episodic salinization"

source('5_DefineCharacteristics/src/prep_attr_randomforest.R')
source('5_DefineCharacteristics/src/apply_randomforest.R')
source('5_DefineCharacteristics/src/evaluate_randomforest.R')
source('5_DefineCharacteristics/src/visualize_attribute_distributions.R')
source('5_DefineCharacteristics/src/visualize_results.R')

p5_targets <- list(
  
  # Set number of attributes to keep during feature selection step 
  tar_target(p5_attr_count, ncol(p3_static_attributes)-1),
  tar_target(p5_attr_cutoff, 8), 
  
  ##### Run random forest for just episodic #####
  
  ###### Prep data for RF ######
  
  tar_target(p5_site_attr, prep_attr_randomforest(p3_static_attributes, sites_episodic = p4_episodic_sites)),
  tar_target(p5_site_attr_rf, dplyr::select(p5_site_attr, -site_no) %>% 
               na.omit()),
  
  ###### Determine optimal RF configs ######
  
  # Run the different combinations of hyperparameter options for mtry and ntree 5 different times (seed length = 5)
  # and identify the best `mtry` and `ntree` to minimize error
  tar_target(p5_rf_model_tuning, tune_randomForest_hyperparameters(p5_site_attr_rf, seed = c(739, 62, 6391, 391, 2))),
  tar_target(p5_hypertuned_params, optimize_hyperparameters(p5_rf_model_tuning)),
  
  # Now run the hypertuned model 
  tar_target(p5_rf_model_hypertuned, apply_randomforest(p5_site_attr_rf, p5_hypertuned_params$mtry, p5_hypertuned_params$ntree)$model),
  
  # Perform feature selection by only choosing the top 50% important predictors
  tar_target(p5_site_attr_rf_optimal, optimize_attrs(site_attr_data = p5_site_attr_rf,
                                                      rf_model = p5_rf_model_hypertuned, 
                                                      n_important = p5_attr_cutoff)),
  
  # Now with the new site attributes, re-tune the model and identify the optimal `mtry` and `ntree` with the new feature set
  tar_target(p5_rf_model_finetuning, tune_randomForest_hyperparameters(p5_site_attr_rf_optimal, seed = c(281, 9502, 472, 93, 720))),
  tar_target(p5_finetuned_params, optimize_hyperparameters(p5_rf_model_finetuning)),
  
  ###### Run optimal RF ######
  
  # Train and assess prediction accuracy of an RF model using the optimized parameters and top attributes
  tar_target(p5_rf_model_optimized_all, apply_randomforest(p5_site_attr_rf_optimal, p5_finetuned_params$mtry,
                                                           p5_finetuned_params$ntree, do_split = TRUE)),
  tar_target(p5_rf_model_optimized, p5_rf_model_optimized_all$model),
  
  ###### Evaluate RF output ######
  
  tar_target(p5_rf_testpreds, p5_rf_model_optimized_all$test_results),
  tar_target(p5_rf_accuracy, p5_rf_model_optimized_all$accuracy),
  tar_target(p5_rf_oob, mean(p5_rf_model_optimized$err.rate[,'OOB'])),
  tar_target(p5_rf_attr_importance, calculate_attr_importance(p5_rf_model_optimized)),
  tar_target(p5_rf_attr_importance_viz, visualize_attr_importance1(rf_model_importance = p5_rf_attr_importance)),
  tar_target(p5_rf_attr_partdep, calculate_partial_dependence(p5_rf_model_optimized, 
                                                               p5_site_attr_rf_optimal,
                                                               focus_class = "Episodic")),
  tar_target(p5_rf_attr_partdep_viz, visualize_partial_dependence(p5_rf_attr_partdep, p5_site_attr_rf_optimal)),
  
  ###### Visualize site category attribute distributions ######
  
  tar_target(p5_attrs_num_viz, visualize_numeric_attrs(p5_site_attr_rf_optimal)),
  tar_target(p5_category_map, visualize_catgory_sites_map(p5_site_attr, p1_nwis_sc_sites_sf, p1_conus_state_cds)),
  
  ##### Save resulting RF outputs as PNGs #####
  
  tar_target(p5_rf_results_png, create_summary_view(
    '5_DefineCharacteristics/log/random_forest_results_episodic.png',
    p5_rf_attr_partdep_viz,
    p5_attrs_num_viz, 
    p5_rf_attr_importance_viz,
    p5_category_map
  ), format='file')
  
)
