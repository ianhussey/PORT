#'Correlation Boundary
#'
#'Computes the minimum and maximum values for a correlation between two variables in a correlation matrix
#'
#'@param corMat A correlation matrix
#'@param var1 A number corresponding to the position in the matrix of the first variable of interest
#'@param var2 A number corresponding to the position in the matrix of the second variable of interest
#'@param method "default" computes the bound based of using values of 0 and pi in the correlative angle matrix. "greek" uses a closed form equation to compute.
#'@return A vector in which the first element is the minimum correlation and the second element is the maximum correlation
#'@examples
#'\dontrun{
#'x <- matrix(c(1,.5,.5,0,.5,1,.5,.3,.5,.5,1,-.1,0,.3,-.1,1), nrow = 4, ncol = 4)
#'x[4,3] <- NA
#'x[3,4] <- NA
#'boundary(x, 4, 3)
#'}
#'@export
boundary <- function(corMat, var1, var2, method = "default"){ #find correlation boundary by NUMPACHAROEN's logic on page 407.

  if(nrow(corMat) != ncol(corMat))
  {
    stop("Matrix must be square!")
  }
  if(nrow(corMat) <= 1)
  {
    stop("Dimension must be greater than 1!")
  }
  n <- nrow(corMat)
  if(identical(method, "default"))
  {
    if(var1 == var2) #if elements are equal, their bounded correlation is 1
      return(c(1, 1))
    if(var1 == ncol(corMat)) #if var1 is in the last column, just move var2 to second to last
      corMat <- corSwap(corMat, var2, ncol(corMat) - 1)
    if(var2 == ncol(corMat)) #if var2 is in the last column, just move var1 to second to last
      corMat <- corSwap(corMat, var1, ncol(corMat) - 1)
    if(var2 != ncol(corMat) && var1 != ncol(corMat)) #if var1 and var2 are not in the last and second to last column, move them there
    {
      corMat <- corSwap(corMat, var1, ncol(corMat))
      corMat <- corSwap(corMat, var2, ncol(corMat) - 1)
    }
    upper <- corToAng(corMat)
    lower <- corToAng(corMat)
    upper[ncol(corMat), ncol(corMat)-1] <- 0 #set C[n, n-1] to 0 for maximum correlation
    lower[ncol(corMat), ncol(corMat)-1] <- pi #set C[n, n-1] to pi for minimum correlation
    return(c(angleToCor(lower)[ncol(corMat), ncol(corMat)-1],angleToCor(upper)[ncol(corMat), ncol(corMat)-1])) #convert new correlative angle matrices (upper and lower above) to correlation matrices, and return the value of C[n, n-1] which is now minimized and maximized.
  }
  if(identical(method, "greek"))
  {
    if(var1 == var2) #if elements are equal, their bounded correlation is 1
      return(c(1, 1))
    if(var1 == ncol(corMat)) #if var1 is in the last column, just move var2 to second to last
      corMat <- corSwap(corMat, var2, ncol(corMat) - 1)
    if(var2 == ncol(corMat)) #if var2 is in the last column, just move var1 to second to last
      corMat <- corSwap(corMat, var1, ncol(corMat) - 1)
    if(var2 != ncol(corMat) && var1 != ncol(corMat)) #if var1 and var2 are not in the last and second to last column, move them there
    {
      corMat <- corSwap(corMat, var1, ncol(corMat))
      corMat <- corSwap(corMat, var2, ncol(corMat) - 1)
    }
    theta <- corToAng(corMat)
    corr_alpha <- 1
    corr_beta <- cos(theta[n, (n - 1)])
    corr_gamma <- 1
    for(k in 1:(n - 2))
    {
      corr_alpha <- corr_alpha * sin(theta[(n - 1), k])
      corr_beta <- corr_beta * sin(theta[n, k])
    }
    for(k in 1:(n - 1))
    {
      corr_gamma <- corr_gamma * sin(theta[n, k])
    }
    lower <- corMat[n, (n - 1)] + (corr_alpha * (-1 * sqrt(corr_beta^2 + corr_gamma^2) - corr_beta))
    upper <- corMat[n, (n - 1)] + (corr_alpha * (sqrt(corr_beta^2 + corr_gamma^2) - corr_beta))
    return(c(lower, upper))
  }
}

#' Swaps columns and rows i and j in a correlation matrix
#' @param corMat A square correlation matrix
#' @param i, j Column and row indices to swap
#' @return The correlation matrix with swapped columns and rows
#' @export
corSwap <- function(corMat, i, j) {
  if (i != j) {
    # Swap columns
    corMat[, c(i, j)] <- corMat[, c(j, i)]
    # Swap rows using the same indices to ensure symmetry
    corMat[c(i, j), ] <- corMat[c(j, i), ]
  }
  return(corMat)
}

#'angleToCor
#'
#'@export
angleToCor <- function(angMat) {
  if (!isSquareMatrix(angMat)) stop("Matrix must be square!")
  
  b <- matrix(nrow = nrow(angMat), ncol = ncol(angMat))
  for (i in seq_len(nrow(angMat))) {
    b[i, ] <- sapply(seq_len(ncol(angMat)), function(j) bsolve(angMat, i, j))
  }
  return(b %*% t(b))
}

#'corSwap
#'
#'@export
corSwap <- function(corMat, i, j) {
  if (i != j) {
    corMat[, c(i, j)] <- corMat[, c(j, i)]
    corMat[c(i, j), ] <- corMat[c(j, i), ]
  }
  return(corMat)
}

#'Impute missing correlation from a matrix
#'
#'@export
corImpute <- function(corMat, method = "average", interval_prob = NA, interval = NA) {
  if (!isSquareMatrix(corMat)) stop("Matrix must be square!")
  if (countMiss(corMat) > 1) stop("Only 1 missing correlation currently supported!")
  
  bounds <- matSolve(corMat)
  if (is.null(bounds)) return(corMat)  # No missing values found
  
  position <- which(is.na(corMat), arr.ind = TRUE)
  replacement <- switch(method,
                        "average" = mean(bounds),
                        "lbound" = bounds[1],
                        "rbound" = bounds[2],
                        "min" = ifelse(bounds[1]^2 > bounds[2]^2, bounds[2], bounds[1]),
                        "max" = ifelse(bounds[1]^2 > bounds[2]^2, bounds[1], bounds[2]),
                        "uniform" = runif(1, min = bounds[1], max = bounds[2]),
                        "custom" = customBound(bounds, interval_prob, interval),
                        stop("Invalid method specified!"))
  corMat[position] <- replacement
  return(corMat)
}

#'customBound
#'
#'@export
customBound <- function(bounds, interval_prob, interval) {
  if (any(is.na(interval_prob), is.na(interval), length(interval_prob)*2 != length(interval), sum(interval_prob) != 1)) {
    stop("Invalid custom interval specifications!")
  }
  interval_pick <- sample(seq_along(interval_prob), size = 1, prob = interval_prob)
  bound_range <- bounds[1] + diff(bounds) * interval[c(interval_pick * 2 - 1, interval_pick * 2)]
  runif(1, min = bound_range[1], max = bound_range[2])
}

#'Check if matrix is square
#'
#'@export
isSquareMatrix <- function(mat) {
  nrow(mat) == ncol(mat) && nrow(mat) > 1
}

#'Count missing elements in a matrix
#'
#'@export
countMiss <- function(corMat) {
  sum(lower.tri(corMat, diag = TRUE) & is.na(corMat))
}

#'bsolve
#'
#'implemention of NUMPACHAROEN (2013) algorithm
#'
#'@export
bsolve <- function(angMat, i, j){ 
  if(nrow(angMat) != ncol(angMat))
  {
    stop("Matrix must be square!")
  }
  dimension = nrow(angMat)
  if(i == 1 && j == 1)
    return(1)
  if(i >= 2 && j == 1)
    return(cos(angMat[i,j]))
  if(i == j && i >= 2 && j <= dimension)
  {
    answer <- 1
    for(index in 1:(j - 1))
    {
      answer <- answer * sin(angMat[i,index])
    }
    return(answer)
  }
  if(j >= 2 && j <= (i - 1))
  {
    answer <- cos(angMat[i,j])
    for(index in 1:(j - 1))
    {
      answer <- answer * sin(angMat[i,index])
    }
    return(answer)
  }
  if(j >= (i + 1) && j <= dimension)
    return(0)
}

#'cholesky
#'
#'Custom Cholesky-Crout algorithm to handle missing data
#'
#'@export
cholesky <- function(mat){ 
  x <- matrix(0, ncol = NCOL(mat), nrow = NROW(mat))
  for(j in 1:ncol(mat))
  {
    for(i in j:nrow(mat))
    {
      if(j == i)
      {
        temp = 0
        if((j - 1) != 0)
        {
          for(k in 1:(j - 1))
          {
            temp = temp + (x[j, k]^2)
          }
        }
        x[i, j] = sqrt(mat[j, j] - temp)
      }
      else
      {
        temp = 0
        if((j - 1) != 0)
        {
          for(k in 1:(j - 1))
          {
            temp = temp + (x[i, k] * x[j, k])
          }
        }
        x[i, j] = (mat[i, j] - temp)/x[j, j]
      }
    }
  }
  x[is.nan(x)] <- 0
  return(x)
}

#'thetaSolve
#'
#'Solves for theta using logic on NUMPACHAROEN (2013) page 405. Partial angle matrix can be provided to speed up computation (so code can access what it already has calculated previously instead of having to compute everything from the beginning column again).
#'
#'@export
thetaSolve <- function(bMat, i, j, partAng=NA){ 
  if(!is.matrix(partAng) || !is.data.frame(partAng))
    partAng <- matrix(NA, ncol = NCOL(bMat), nrow = NROW(bMat))
  if(nrow(bMat) != ncol(bMat))
  {
    stop("Matrix must be square!")
  }
  dimension = nrow(bMat)
  if(j >= i)
    return(0)
  if(i >= 2 && j == 1)
    return(acos(bMat[i, j]))
  if(j >= 2 && j <= (i - 1))
  {
    sinprod <- 1
    for(index in 1:(j - 1))
    {
      if(is.na(partAng[i, index]))
        sinprod <- sinprod * sin(thetaSolve(bMat, i, index))
      else
        sinprod <- sinprod * sin(partAng[i, index])
    }
    if(is.na(sinprod))
      return(NA)
    if(sinprod == 0)
      return(0)
    else
    {
      if(is.na((bMat[i, j]/sinprod)))
        return(NA)
      if((bMat[i, j]/sinprod) > 1) #If product is out of the domain of acos, treat it as acos(1)
        return(0)
      if((bMat[i, j]/sinprod) < -1) #If product is out of the domain of acos, treat it as acos(-1)
        return(pi)
      else
        return(acos(bMat[i, j]/sinprod))
    }
  }
}

#'Solves for the boundary of one missing correlation
#'
#'Automatically detects and returns the boundary of one missing correlation in a correlation matrix
#'@param corMat A correlation matrix with one missing correlation
#'@return A vector in which the first element is the minimum correlation and the second element is the maximum correlation
#'@examples
#'\dontrun{
#'x <- matrix(c(1,.5,.5,0,.5,1,.5,.3,.5,.5,1,-.1,0,.3,-.1,1), nrow = 4, ncol = 4)
#'x[4,3] <- NA
#'x[3,4] <- NA
#'matSolve(x)
#'}
#'@export
matSolve <- function(corMat) #automatically finds the boundaries of one missing entry in a correlative matrix
{
  if(countMiss(corMat) > 1)
    stop("Correlation matrix has more than 1 missing entry!")
  for(i in 1:nrow(corMat))
  {
    for(j in 1:i)
    {
      if(is.na(corMat[i,j]))
      {
        return(boundary(corMat, i, j))
      }
    }
  }
}

#'Convert an upper or lower triangle to a full matrix
#'
#'@export
triangle_to_cor_matrix <- function(triangle){
  
  # if input is a df or tibble, convert to matrix
  if(is.data.frame(triangle) | is_tibble(triangle)){
    triangle <- as.matrix(triangle)
  } 
  if(!is.matrix(triangle)){
    stop("Input must be a matrix, data frame, or tibble")
  }
  
  # assess if the triangle is an upper triangle. I.e., are all the lower tri values NA and all the upper tri values non-NA? If so, transpose the matrix to make it a lower triangle. 
  if(all(is.na(triangle[lower.tri(triangle)])) &
     all(!is.na(triangle[upper.tri(triangle)]))){
    triangle <- t(triangle)
  }
  
  # create an empty matrix of the same dimension as 'triangle' to hold the mirrored values
  mirror_matrix <- matrix(0, nrow = nrow(triangle), ncol = ncol(triangle))
  
  # assign the lower triangle of 'mirror_matrix' with the lower triangle of 'triangle'
  mirror_matrix[lower.tri(mirror_matrix)] <- triangle[lower.tri(triangle)]
  
  # add the transposed 'mirror_matrix' to itself to complete the mirroring process
  full_matrix <- mirror_matrix + t(mirror_matrix) - diag(nrow(triangle))
  
  # fill diagonal with 1s
  diag(full_matrix) <- 1
  
  return(full_matrix)
}

#' Correlation Matrix to Correlative Angle Matrix
#'
#' Transform a correlation matrix to its correlative angle matrix form
#' @param corMat A matrix which contains the correlations between variables
#' @return A correlative angle matrix corresponding to the input correlation matrix
#' @examples
#' \dontrun{
#'x <- matrix(c(1,.5,.5,0,.5,1,.5,.3,.5,.5,1,-.1,0,.3,-.1,1), nrow = 4, ncol = 4)
#'x[4,3] <- NA
#'x[3,4] <- NA
#'y <- corToAng(x)
#'y[4,3] <- 0
#'round(angleToCor(y), 2)
#'}
#' @export
corToAng <- function(corMat){ #function to loop over a correlation matrix calling thetaSolve on each element to get the theta values for the correlative angle matrix
  if(nrow(corMat) != ncol(corMat))
  {
    stop("Matrix must be square!")
  }
  if(nrow(corMat) <= 1)
  {
    stop("Dimension must be greater than 1!")
  }
  angMat <- matrix(NA, nrow = nrow(corMat), ncol = ncol(corMat))
  b <- cholesky(corMat)
  for(j in 1:ncol(angMat))
  {
    for(i in 1:nrow(angMat))
    {
      angMat[i,j] <- thetaSolve(b, i, j, angMat)
    }
  }
  return(angMat)
}

#' Check if correlation matrix is positive definite
#'
#'@export
positive_definite <- function(correlation_matrix){
  all(eigen(correlation_matrix)$values > 0)
}

#' Check if all elements of a correlation matrix are within the bounds [-1, 1]
#'
#'@export
within_bounds <- function(correlation_matrix){
  all(correlation_matrix <= +1) & all(correlation_matrix >= -1)
}

#' Apply multiple checks to a correlation matrix
#'
#'@export
check_correlation_matrix <- function(data_matrix) {
  require(stats)  # Ensure the stats package is loaded for rank function
  
  original_matrix <- data_matrix
  imputed_matrix <- matrix(NA, nrow = nrow(data_matrix), ncol = ncol(data_matrix))
  diff_matrix <- matrix(NA, nrow = nrow(data_matrix), ncol = ncol(data_matrix))
  
  # Function to impute a single element
  singleImpute <- function(mat, row, col) {
    mat[row, col] <- NA  # Introduce NA at the specified position
    imputed_value <- corImpute(mat, method = "average")
    return(imputed_value[row, col])
  }
  
  # Perform imputation and calculate differences
  for (i in 1:nrow(data_matrix)) {
    for (j in 1:ncol(data_matrix)) {
      suppressWarnings({
        imputed_matrix[i, j] <- singleImpute(data_matrix, i, j)
        diff_matrix[i, j] <- original_matrix[i, j] - imputed_matrix[i, j]
      })
    }
  }
  
  # Format matrix with original and difference
  formatted_matrix <- matrix("", nrow = nrow(data_matrix), ncol = ncol(data_matrix))
  abs_diff <- abs(diff_matrix)
  ranks <- matrix(rank(-abs_diff, ties.method = "min"), nrow = nrow(data_matrix))  # Ensure ranks is a matrix
  
  for (i in 1:nrow(data_matrix)) {
    for (j in 1:ncol(data_matrix)) {
      stars <- ifelse(ranks[i, j] == 1, "***",
                      ifelse(ranks[i, j] == 2, "**",
                             ifelse(ranks[i, j] == 3, "*", "")))
      #formatted_matrix[i, j] <- sprintf("%.2f [%.2f]%s", janitor::round_half_up(original_matrix[i, j], 2), janitor::round_half_up(diff_matrix[i, j], 2), stars)
      formatted_matrix[i, j] <- sprintf("%.2f [%+.2f]%s", janitor::round_half_up(original_matrix[i, j], 2), -janitor::round_half_up(diff_matrix[i, j], 2), stars)
      #formatted_matrix[i, j] <- sprintf("%+.2f [%+.2f]%s", janitor::round_half_up(original_matrix[i, j], 2), janitor::round_half_up(diff_matrix[i, j], 2), stars)
    }
  }
  
  upper_tri_only <- function(mat){
    diag(mat) <- NA
    mat[upper.tri(mat)] <- NA
    t(mat)
  }
  
  original_matrix <- upper_tri_only(original_matrix)
  imputed_matrix <- upper_tri_only(imputed_matrix)
  diff_matrix <- upper_tri_only(diff_matrix)
  formatted_matrix <- upper_tri_only(formatted_matrix)
  
  return(list(
    original_matrix = original_matrix,
    original_is_pd = positive_definite(triangle_to_cor_matrix(original_matrix)),
    original_is_within_bounds = within_bounds(triangle_to_cor_matrix(original_matrix)),
    imputed_matrix = imputed_matrix,
    difference_matrix = diff_matrix,
    formatted_results = formatted_matrix
  ))
}