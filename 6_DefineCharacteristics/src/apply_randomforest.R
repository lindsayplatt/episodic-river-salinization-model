
#' @title Run a random forest model
#' @description Using the `randomForest::randomForest()` function, run a 
#' random forest model for the given set of site attributes and site categories.
#' 
#' @param site_attr_data a tibble with the columns `site_category_fact` and any
#' number of columns that give static attributes (not prefixed with `attr_`)
#' @param mtry the number of variables randomly sampled as candidates at each 
#' split; by default it will use the square root of the number of attributes.
#' @param ntree the number of trees to try in the random forest; defaults to 500.
#' @param seed a numeric value to use in `set.seed()` to ensure reproducibility; 
#' defaults to 24
#' 
#' @returns an list object of class randomForest, see `?randomForest::randomForest`
#' 
apply_randomforest <- function(site_attr_data, mtry = NULL, ntree = NULL, seed = 24) {
  
  # Use the default for mtry in `randomForest()` if passed in as NULL here
  if(is.null(mtry)) mtry <- floor(sqrt(ncol(site_attr_data) - 1))
  
  set.seed(seed)
  randomForest(
    site_category_fact ~ .,
    data = site_attr_data,
    mtry = mtry, 
    importance = TRUE)
  
}

#' @title Tune random forest hyperparameters
#' @description Find the optimal `mtry` and `ntree` hyperparameters for a
#' random forest model. This function splits the data into train/test using
#' an 80/20 split, then runs multiple random forests to identify the best
#' values for each hyperparameter. Each hyperparameter combination is run 
#' as many times as the number of seeds passed in. For each seed, it will 
#' select the value of `mtry` and `ntree` that minimizes the out-of-bag
#' error. After this function, the best overall `mtry` and `ntree` will need
#' to be identified. 
#' 
#' @param site_attr_data a tibble with the columns `site_category_fact` and any
#' number of columns that give static attributes (not prefixed with `attr_`)
#' @param seed a numeric vector giving values to use in `set.seed()` to test the
#' hyperparameter tuning combinations multiple times. Defaults to 19 and 1019
#' @param mtry_options a vector giving the number of variables randomly sampled 
#' as candidates at each split. Defaults to 2 thru 8.
#' @param ntree_options a vector giving the number of trees to try in the random
#' forest. Defaults to 500, 1000, 5000, and 10000.
#' 
#' @returns a tibble with a row per `seed` specifying the optimal `mtry`, the
#' optimal `ntree`, and the accuracy of that combination for the given test.
#' 
tune_randomForest_hyperparameters <- function(site_attr_data, 
                                              seed = c(19, 1019), 
                                              mtry_options = 2:8, 
                                              ntree_options = c(500,1000,5000,10000)) {
  map(seed, ~{
    
    set.seed(.x)
    
    # Split into test/train
    trainIndex <- sample(1:nrow(site_attr_data), 0.8*nrow(site_attr_data))
    train_data <- site_attr_data[trainIndex, ]
    test_data <- site_attr_data[-trainIndex, ]
    
    # Set up a grid of hyperparameters to search over
    tuning_grid <- expand.grid(mtry = mtry_options,  # Number of variables to sample
                               ntree = ntree_options)  # Number of trees
    
    # Initialize an empty list to store models
    models <- list()
    
    # Iterate over each combination of hyperparameters
    for (i in 1:nrow(tuning_grid)) {
      # Train a random forest model using the current hyperparameters
      model <- randomForest(site_category_fact ~ ., data = train_data,
                            mtry = tuning_grid$mtry[i],
                            ntree = tuning_grid$ntree[i])
      
      # Store the model in the list
      models[[i]] <- model
    }
    
    # Evaluate models on the test set and store the results
    accuracy <- numeric(nrow(tuning_grid))
    for (i in 1:length(models)) {
      predictions <- predict(models[[i]], newdata = test_data)
      accuracy[i] <- mean(predictions == test_data$site_category_fact)
    }
    
    # Find the best model
    best_model_index <- which.max(accuracy)
    best_model <- models[[best_model_index]]
    best_accuracy <- accuracy[best_model_index]
    
    tibble(mtry = best_model$mtry, 
           ntree = best_model$ntree, 
           accuracy = best_accuracy)
  }) %>% 
    bind_rows(.id = 'seed')
}

#' @title Selects the optimal `mtry` and `ntree` from the tuning tests
#' @description This function selects the best `mtry` and `ntree` value from the
#' different hyperparameter tuning tests that were run by choosing the combination
#' that occurred most frequently (the "statistical mode").
#' 
#' @param tuning_results a tibble with the columns `seed`, `mtry`, `ntree`, and
#' `accuracy` as returned from `tune_randomForest_hyperparameters()`
#' 
#' @returns a single row table with the columns `mtry` and `ntree` giving the best 
#' value of each to use in a future random forest model run
#' 
optimize_hyperparameters <- function(tuning_results) {
  tuning_results %>% 
    # Identify the frequency of each mtry/ntree combination
    group_by(mtry, ntree) %>% 
    tally() %>% 
    ungroup() %>% 
    # Identify the mode (most frequent) value of the data
    filter(n == max(n)) %>% 
    # Keep the lower values if there is a tie
    arrange(mtry, ntree) %>% 
    slice(1) %>% 
    select(-n)
}

#' @title Identify the most important attributes to use in a future random forest model
#' @description This function identifies attributes that are highest among the 
#' importance metrics ranking from a previous random forest model run in order to
#' better optimize the next random forest model run with fewer, more targeted attributes.
#' 
#' @param site_attr_data a tibble with the columns `site_category_fact` and any
#' number of columns that give static attributes (not prefixed with `attr_`)
#' @param rf_model a model object of a random forest that was already run; likely
#' returned from `apply_randomforest()`
#' @param n_important the maximum number of attributes to keep and be used in the 
#' next, more optimized random forest run. Uses the "importance" metric to rank
#' the attributes and only keeps the highest. Defaults to top 10 most important.
#' 
#' @returns a tibble with the columns `site_category_fact` and columns that 
#' correspond to only the top `n_important` attributes identified
#' 
optimize_attrs <- function(site_attr_data, rf_model, n_important = 10) {
  
  # Extract mean importance for each attribute from the model
  attr_importance <- rf_model$importance %>% 
    as_tibble(rownames = 'attribute') %>% 
    dplyr::select(attribute, importance = MeanDecreaseGini) %>% 
    arrange(desc(importance)) 
  
  # Identify most important attributes
  most_important_attrs <- attr_importance %>% 
    head(n_important) %>% 
    pull(attribute)
    
  # Trim the attribute data down to only that set of unique attributes
  site_attr_data_trim <- site_attr_data %>% 
    dplyr::select(site_category_fact, all_of(most_important_attrs))
  
  return(site_attr_data_trim)
}
