#' correlation_consistency_trio
#' 
#' Computes the minimum and maximum values for each of three correlations given the other two correlations
#' @param rAB Correlation between A and B
#' @param rBC Correlation between B and C
#' @param rAC Correlation between A and C
#' @param digits The number of digits to round the output to
#' @examples
#' \dontrun{
#' correlation_consistency_trio(rAB = 0.5, rBC = -0.1, rAC = 0.1)
#' }
#' @export
correlation_consistency_trio <- function(rAB, rBC, rAC, digits = 2) {
  # calculate bounds for each pair
  bounds_AB <- correlation_consistency_single(rBC, rAC, rAB, digits)
  bounds_BC <- correlation_consistency_single(rAB, rAC, rBC, digits)
  bounds_AC <- correlation_consistency_single(rAB, rBC, rAC, digits)
  
  # rename columns for clarity
  bounds_AB <- bounds_AB |>
    rename(rAB_lower = rYZ_lower,
           rAB_upper = rYZ_upper,
           rAB_consistency = consistent)
  
  bounds_BC <- bounds_BC |>
    rename(rBC_lower = rYZ_lower,
           rBC_upper = rYZ_upper,
           rBC_consistency = consistent)
  
  bounds_AC <- bounds_AC |>
    rename(rAC_lower = rYZ_lower,
           rAC_upper = rYZ_upper,
           rAC_consistency = consistent)
  
  # summarize results
  results <- data.frame(
    reported__r_AB = rAB,
    lower__r_AB = bounds_AB$rAB_lower,
    upper__r_AB = bounds_AB$rAB_upper,
    consistent__r_AB = bounds_AB$rAB_consistency,
    
    reported__r_BC = rBC,
    lower__r_BC = bounds_BC$rBC_lower,
    upper__r_BC = bounds_BC$rBC_upper,
    consistent__r_BC = bounds_BC$rBC_consistency,
    
    reported__r_AC = rAC,
    lower__r_AC = bounds_AC$rAC_lower,
    upper__r_AC = bounds_AC$rAC_upper,
    consistent__r_AC = bounds_AC$rAC_consistency
  ) |>
    # wrangle
    pivot_longer(cols = everything(),
                 names_to = c("metric", "variables"),
                 names_sep = "__",
                 values_to = "value") |>
    pivot_wider(names_from = "metric",
                values_from = "value") |>
    mutate(consistent = case_when(consistent == 0 ~ "inconsistent",
                                  consistent == 1 ~ "consistent"),
           variables = str_remove(variables, "r_")) |>
    rename(result = consistent,
           lower_possible = lower,
           upper_possible = upper)
  
  return(results)
}
