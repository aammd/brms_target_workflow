# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "brms"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source(files = "functions.R")
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(
    name = data,
    command = tibble(x = seq(from = -1, to = 1, length.out = 33),
                     y = 0)
  ),
  tar_target(
    name = bf_linear,
    command = bf(y ~ 0 + Intercept + x, family = poisson())
  ),
  tar_target(
    name = prior_linear,
    command = c(prior(normal(0, .5), class = "b", coef = "x"),
                prior(normal(3, .5), class = "b", coef = "Intercept"))
  ),
  tar_target(
    prior_predictive_draws,
    simulate_from_prior(data_values = data,
                        prior_spec = prior_linear,
                        bf_object = bf_linear,
                        draws_wanted = 50:100)
  ),
  tar_target(
    brm_linear,
    brm(bf_linear,
        data = data,
        prior = prior_linear,
        sample_prior = "only",
        chains =2,
        iter = 1000,
        backend = "cmdstanr",
        file_refit = "on_change")
  ),
  tar_target(
    one_model,
    set_up_one_model(
      one_sampled_model = brm_linear,
      dataframe = df_list[[1]]
    )
  ),
  tar_target(
    df_list,
    prior_predictive_draws$simulated_data
  ),
  tar_target(
    all_models,
    command = update(one_model,
                     newdata = df_list,
                     recompile = FALSE),
    pattern = map(df_list),
    iteration = "list"
  ),
  tar_target(
    coverage,
    command = calculate_coverage(prior_predictive_draws, all_models)
  ),
  tar_quarto(report, "index.qmd")
)
