
#' Format the data frame into gridded matrix
#' @param df dataframe containing all the data
#' @param df.setdims dataframe with the dimension informations
#' @export
nc_format <- function(df, df.setdims) {

  l <- vector("list")

  vars <- names(df.setdims)
  l$vars <- plyr::llply(vars, function(varname) {
    order.dims <- df.setdims[[varname]] %>% .[!is.na(.)]
    formula.dims <- paste(rownames(df.setdims)[order.dims], collapse = " ~ ")
    var.out <- reshape2::acast(df, formula.dims, drop=FALSE, value.var = varname, fun.aggregate = mean)
    names(attr(var.out, "dimnames")) <- rownames(df.setdims)[order.dims]
    var.out[is.na(var.out)] <- -999
    return(var.out)
  }); names(l$vars) <- vars


  dims <- rownames(df.setdims)
  l$dims <- plyr::llply(dims, function(dimname) {
    as.numeric(levels(df[[dimname]]))
  }); names(l$dims) <- dims

  return(l)
}
