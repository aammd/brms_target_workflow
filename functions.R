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

