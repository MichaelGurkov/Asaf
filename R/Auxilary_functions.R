#'
#' @title Identify peaks
#'
#' @description This function identifies local peaks in time series
#'
#' @param raw_timeseries
#'
identify_peaks = function(raw_timeseries){

  ind_vec = rep(0, length(raw_timeseries))

  diff_vec = c(NA, sign(diff(raw_timeseries)))

  diff2_vec = c(NA, sign(diff(diff_vec)))

  ind_vec[diff_vec == -1] = 1

  return(ind_vec)

}


#' @title calculate drawdown
#'
#' @param horizon
#'
#' @description This functions calculates the drawdown which is the
#'  (percentage) drop since the peak defined by \code{horizon}
#'
#' @import slider
#'
calculate_drawdown = function(timeseries,  horizon = Inf){

  drawdown = slide_dbl(.x = timeseries,
                   .f = ~ .x[length(.x)] / max(.x, na.rm = TRUE) - 1,
                   .before = horizon)

  return(drawdown)





}
