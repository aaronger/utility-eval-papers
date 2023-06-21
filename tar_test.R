library(targets)
list(tar_target(name = forecast_data, command = get_forecast_data(forecast_dates)), 
    tar_target(name = truth_data, command = get_truth_data()), 
    tar_target(name = score_data, command = get_forecast_scores(forecast_data, 
        truth_data)), tar_map(values = values, tar_target(alloscore, 
        run_alloscore(forecast_data, truth_data, forecast_dates, 
            models))))
