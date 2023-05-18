## functions


make_prior_draws_df <- function(brms_prior_model,
                                draw_vec = 25:44){

  brms_prior_model |>
    as.data.frame(draw = draw_vec) |>
    dplyr::mutate(draw_id = draw_vec) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      simulated_data = list(dplyr::bind_cols(
        # extract model data
        brms_prior_model$data,
        # observations simulated from the prior
        y_new = brms::posterior_predict(
          brms_prior_model, draw_ids = draw_id) |> c(),
        # expected predicted values from the prior
        epred = brms::posterior_epred(
          brms_prior_model, draw_ids = draw_id) |> c()
      ) |>
        # most awkward part -- rename placeholder y variable
        dplyr::select(-y) |>
        dplyr::rename(y = y_new)
      )
    )
}

make_unnest_prior_dataframe <- function(prediction_df,
                                        original_data,
                                        x_name){
  prediction_df |>
    dplyr::mutate(num_obs = list(original_data[x_name])) |>
    tidyr::unnest(cols = c(epred, preds, num_obs))
}

merge_prior_and_posterior_summary <- function(prior_df, posterior_df){

  prior_params_df <- prior_df |>
    dplyr::select(-lprior, -lp__, -simulated_data) |>
    tidyr::pivot_longer(cols = -draw_id, names_to = "variable")

  combined <- prior_params_df |>
    dplyr::left_join(posterior_df |>
                       tidyr::unnest(coefs),
                     by = c("draw_id", "variable"))

  return(combined)

}

calculate_post_coverage <- function(.prior_post_combined){
  .prior_post_combined |>
    dplyr::mutate(covered = value > q5 & value < q95) |>
    dplyr::group_by(variable) |>
    dplyr::summarise(prop_covered = mean(covered))
}

###

#' sets up a model for calculating
#'
#'For some reason the brms model wants to recompile by default. This
#'
#' @param one_sampled_model a model, probably the one that was sampled to generate the prior predictions
#' @param dataframe one dataframe (could be a posterior)
#'
#' @return
set_up_one_model <- function(one_sampled_model, dataframe){
  update(one_sampled_model,
         sample_prior = "no",
         newdata = dataframe,
         iter = 2000, chains = 4)
}


simulate_from_prior <- function(data_values,
                                prior_spec,
                                bf_object,
                                draws_wanted = 25:49){

  brm_prior_sample_only <- brm(bf_object,
                               data = data_values,
                               prior = prior_spec,
                               sample_prior = "only",
                               chains = 2,
                               iter = 1000,
                               backend = "cmdstanr",
                               file_refit = "on_change")

  twenty_five_simulations <- make_prior_draws_df(
    brms_prior_model = brm_prior_sample_only,
    draw_vec = 25:49)

  return(twenty_five_simulations)
}

#' Calculate coverage of the posterior
#'
#' Composed function that combines three steps:
#' * summarize posterior
#' * merge with the prior predictive draws (these contain the true parameter values)
#' * calculate how many times the true value is in the interval
#'
#' @param prior_predictive_draws the dataframe of prior draws. result of `simulate_from_prior()`
#' @param all_models the list of all models fit to these draws.
#'
#' @return
#' @export
#'
#' @examples
calculate_coverage <- function(prior_predictive_draws, all_models){

  coverage_posteriors <- tibble::tibble(
    draw_id = prior_predictive_draws$draw_id,
    coefs = purrr::map(all_models, posterior::summarise_draws))

  prior_post_combined <- merge_prior_and_posterior_summary(
    prior_predictive_draws,
    coverage_posteriors)

  coverage <- calculate_post_coverage(prior_post_combined)

  return(coverage)

}
