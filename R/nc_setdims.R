
#' Inform on the dimensions for each variable
#' @param df input dataframe containing the gridded variables and dimensions
#' @param dims character array indicating the dimensions
#' @export
nc_setdims <- function(df, dims) {

  ## Identify the variable names
  tmp <- names(df)
  vars <- tmp[sapply(tmp, function(var) {! var %in% dims}, USE.NAMES = FALSE)]

  ## Create a dataframe with all dimensions as rows and all variables as columns
  df.setdims <- data.frame(matrix(ncol = length(vars), nrow = length(dims)), row.names = dims)
  colnames(df.setdims) <- vars

  return(df.setdims)
}
