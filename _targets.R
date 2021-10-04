library(targets)
library(tarchetypes)
source(here::here("R", "functions.R"))

list(
    targets::tar_target(packages, packages_used()),
    targets::tar_target(dataset, readind_data()),
    targets::tar_target(vol_split, split_data(dataset, 1504, 0.75, cod_volume)),
    targets::tar_target(test_split, test_set_split(vol_split)),
    targets::tar_target(train_split, train_set_split(vol_split)),
    targets::tar_target(vol_resamples, kfold_cv(train_split, strata = cod_volume, repeats = 5)),
    targets::tar_target(normalized_vol_rec, preproc_rec(train_split)),
    targets::tar_target(simple_vol_rec, simple_rec(train_split)),
    targets::tar_target(all_specs, def_specs()),
    targets::tar_target(wflow_vol, workflow_config(all_specs, normalized_vol_rec, simple_vol_rec)),
    targets::tar_target(racing_ctrl, racing_defs()),
    targets::tar_target(results_vol, race_tuning(wflow_vol, vol_resamples, racing_ctrl, 1504)),
    targets::tar_target(plot_rsq, plot_results(results_vol, "rsq")),
    targets::tar_target(best_model, select_models(results_vol, "rmse", 1)),
    targets::tar_target(intermediary_model, select_models(results_vol, "rmse", 10)),
    targets::tar_target(fit_best_mod, fit_model(results_vol, best_model, vol_split)),
    targets::tar_target(fit_intermediary_model, fit_model(results_vol, intermediary_model, vol_split)),
    targets::tar_target(metrics_best, metrics_mod(fit_best_mod, best_model)),
    targets::tar_target(metrics_intermediary, metrics_mod(fit_intermediary_model, intermediary_model)),
    targets::tar_target(model_comparison, accessing_models(fit_best_mod,
                                                           fit_intermediary_model, 
                                                           best_model, 
                                                           intermediary_model)),
    tarchetypes::tar_render(report, here::here("report", "report.Rmd"))
)


