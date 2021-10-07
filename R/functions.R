packages_used <- function(){
    
    library(here)
    library(readxl)
    library(tidyverse)
    library(tidymodels)
    library(janitor)
    library(EnvStats)
    library(bestNormalize)
    library(finetune)
    library(doParallel)
    library(extrafont)
    
}

readind_data <- function(){
    
    dir.create("data")
    
    link_train <- "https://doi.org/10.1371/journal.pone.0238703.s007"
    link_test <- "https://doi.org/10.1371/journal.pone.0238703.s008"
    
    dest_folder_train <- here::here("data", "train.xlsx")
    dest_folder_test <- here::here("data", "test.xlsx")
    
    utils::download.file(link_train, destfile = dest_folder_train, mode = "wb") # wb se OS for Windows
    utils::download.file(link_test, destfile = dest_folder_test, mode = "wb")
    
    sheet_train <- readxl::read_excel(dest_folder_train, skip = 1)
    sheet_test <- readxl::read_excel(dest_folder_test, skip = 1)
    
    dplyr::bind_rows(sheet_train, sheet_test) |> 
        janitor::clean_names() |>  
        dplyr::mutate(across(c(stem:rotation), forcats::as_factor),
                      across(c(tx, d), as.integer))
    
}


split_data <- function(df, seed_value = 1504, prop = 0.75, strata = cod_volume){
    
    set.seed(seed_value)
    rsample::initial_split(data = df, prop, strata = cod_volume) -> data_split
    
}


test_set_split <- function(first_split, seed_value = 1504){
    
    set.seed(seed_value)
    rsample::testing(first_split)
    
}


train_set_split <- function(first_split, seed_value = 1504){
    
    set.seed(seed_value)
    rsample::training(first_split)
    
}


kfold_cv <- function(train_set,...){
    
    rsample::vfold_cv(data = train_set, ...)
    
}


preproc_rec <- function(df){
    
    recipes::recipe(v ~ dbh + h + tx + d, data = df) |> 
        recipes::step_interact(terms = ~ dbh:h) |> 
        recipes::step_mutate(dbh_sqrd = dbh^2, h_sqrd = h^2) |> 
        recipes::step_normalize(recipes::all_predictors(), -tx, -d)
    
}


simple_rec <- function(df){
    
    recipes::recipe(v ~ dbh + h + tx + d, data = df)
    
}


def_specs <- function(mode = "regression"){
    
    lm_mod <- 
        parsnip::linear_reg() |> 
        parsnip::set_engine("lm") |> 
        parsnip::set_mode(mode)
    
    penalized_lm_mod <- 
        parsnip::linear_reg(penalty = tune::tune(),
                            mixture = tune::tune()) |> 
        parsnip::set_engine("glmnet") |>
        parsnip::set_mode(mode)
    
    bag_mars_mod <- 
        baguette::bag_mars(prod_degree = tune::tune(), 
                           prune_method = "exhaustive",
                           num_terms = tune::tune()) |> 
        parsnip::set_engine("earth", times = 4) |> 
        parsnip::set_mode(mode)
    
    dec_tree <- 
        parsnip::decision_tree(cost_complexity = tune::tune(),
                               tree_depth = tune::tune(),
                               min_n = tune::tune()) |> 
        parsnip::set_engine("rpart") |> 
        parsnip::set_mode(mode)
    
    bag_cart_mod <- 
        baguette::bag_tree(cost_complexity = tune::tune(),
                           tree_depth = tune::tune(),
                           min_n = tune::tune()) |> 
        parsnip::set_engine("rpart", times = 50L) |>
        parsnip::set_mode(mode)
    
    rf_spec <- 
        parsnip::rand_forest(mtry = tune::tune(), 
                             min_n = tune::tune(),
                             trees = 1000) |> 
        parsnip::set_engine("ranger") |> 
        parsnip::set_mode(mode)
    
    xgb_spec <- 
        parsnip::boost_tree(tree_depth = tune::tune(),
                            learn_rate = tune::tune(),
                            loss_reduction = tune::tune(),
                            min_n = tune::tune(),
                            sample_size = tune::tune(),
                            mtry = tune::tune(),
                            trees = 1000,
                            stop_iter = 20) |> 
        parsnip::set_engine("xgboost") |> 
        parsnip::set_mode(mode)
    
    svm_r_spec <- 
        parsnip::svm_rbf(cost = tune::tune(),
                         rbf_sigma = tune::tune(),
                         margin = tune::tune()) |> 
        parsnip::set_engine("kernlab") |> 
        parsnip::set_mode(mode)
    
    nnet_spec <- 
        parsnip::mlp(hidden_units = tune::tune(),
                     penalty = tune::tune(), 
                     epochs = tune::tune()) |> 
        parsnip::set_engine("nnet") |> 
        parsnip::set_mode(mode)
    
    list("linear_reg" = lm_mod,
         "bag_mars"= bag_mars_mod,
         "decision_tree" = dec_tree, 
         "bag_cart" = bag_cart_mod, 
         "rf" = rf_spec, 
         "xgb" = xgb_spec,
         "svm_rbf" = svm_r_spec, 
         "nnet_mlp" = nnet_spec,
         "penalized_reg" = penalized_lm_mod)
    
}


workflow_config <- function(specs, rec1, rec2){
    
    workflowsets::workflow_set(
        models = specs,
        preproc = list(normalized = rec1,
                       simple = rec2
        )
    )
    
}


racing_defs <- function(...){
    finetune::control_race(
        save_pred = TRUE,
        parallel_over = "everything",
        save_workflow = TRUE,
        ...)
}


race_tuning <- function(all_wflow, resamples, ctrl, def_seed = 1504){
    
    clusters <- parallel::detectCores() - 1
    cl <- parallel::makePSOCKcluster(clusters)
    doParallel::registerDoParallel(cl)
    
    race_results <-
        workflowsets::workflow_map(
            all_wflow,
            seed = def_seed,
            resamples = resamples,
            control = ctrl,
            fn = "tune_race_anova",
            grid = 25,
            metrics = yardstick::metric_set(yardstick::rmse,
                                            yardstick::rsq, 
                                            yardstick::huber_loss, 
                                            yardstick::mae)
            
        )
    
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
    
    race_results
    
}



plot_results <- function(race_rslts, mtrc = "rmse",...){
    workflowsets::autoplot(
        race_rslts,
        rank_metric = mtrc,  
        metric = mtrc,       
        select_best = TRUE,
        ...
    ) -> plot_racing
    
    graphics::plot(plot_racing)
    ggplot2::ggsave(glue::glue(mtrc, ".png"), path = here::here("plots"))
    
}


select_models <- function(grid_results, metric = "rmse", rank_posit = 1){
    
    workflowsets::rank_results(grid_results, select_best = TRUE) |> 
        dplyr::relocate(rank) |> 
        dplyr::select(-c(.config, model, std_err)) |> 
        dplyr::filter(.metric == "rmse" & rank == rank_posit) 
    
}


fit_model <- function(grid_results, model_ranked, df_split, metric = "rmse"){
    
    name_model <- model_ranked |> purrr::pluck("wflow_id", 1)
    
    model <- grid_results |> 
        workflowsets::extract_workflow_set_result(id = name_model) |>
        tune::select_best(metric = metric)
    
    grid_results |> 
        workflowsets::extract_workflow(name_model) |>
        tune::finalize_workflow(model) |> 
        tune::last_fit(split = df_split,
                       metrics = yardstick::metric_set(yardstick::rmse, 
                                                       yardstick::rsq, 
                                                       yardstick::huber_loss, 
                                                       yardstick::mae))
    
}



metrics_mod <- function(best_mod, model_ranked){
    
    name_model <- model_ranked |> purrr::pluck("wflow_id", 1)
    
    workflowsets::collect_metrics(best_mod) |> 
        dplyr::select(-.config) |> 
        dplyr::rename(Metric = .metric, Estimate = .estimate, Estimator = .estimator) |> 
        kableExtra::kbl(caption = glue::glue("Performance ", sub("normalized_", "", name_model))) |> 
        kableExtra::kable_classic(full_width = F, 
                                  html_font = "Cambria", 
                                  font_size = 16) |> 
        kableExtra::save_kable(file = here::here("plots", 
                                                 glue::glue("perf_", name_model, ".png")),
                               self_contained = F)
}


scatterplot <- function(.x, .y){
    
    ggplot2::ggplot(.x) + 
        ggplot2::geom_point(mapping = ggplot2::aes(x = .pred, y = v)) + 
        ggplot2::geom_point(mapping = ggplot2::aes(x = .pred, y = v), 
                            alpha = 0.5, 
                            size = 2, 
                            color = "black", 
                            fill = "mediumspringgreen", 
                            pch = 21) + 
        ggplot2::geom_abline(lty = 2, 
                             col = "black", 
                             size = 1) +
        #tune::coord_obs_pred() +
        ggplot2::theme_classic()  +
        ggplot2::ggtitle(.y) +
        ggplot2::labs(x = "Volume Predito (m^3)",
                      y = "Volume Observado (m^3)") +
        ggplot2::theme(text = ggplot2::element_text(family = "Source Code Pro")) -> plot_scatterplot
    
    plot_scatterplot
    ggplot2::ggsave(glue::glue(.y, ".png"), path = here::here("plots"))
    
}


accessing_models <- function(mod1, mod2, model_ranked1, model_ranked2){
    
    name_model1 <- model_ranked1 |> purrr::pluck("wflow_id", 1)
    name_model2 <- model_ranked2 |> purrr::pluck("wflow_id", 1)
    
    workflowsets::collect_predictions(mod1) |> 
        dplyr::mutate(model = name_model1) |> 
        dplyr::bind_rows(workflowsets::collect_predictions(mod2) |> 
                             dplyr::mutate(model = name_model2)) |>
        tidyr::nest(data = -c(model)) |> 
        dplyr::mutate(plots = purrr::map2(data, model, scatterplot))
    
}


