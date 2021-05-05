
#' Write the gridded netcdf file
#' @param fn name of the output file
#' @param l list of the variable and dimension informations
#' @export
nc_write <- function(fn, l) {

  dir.create(dirname(fn), showWarnings = FALSE)
  if(file.exists(fn)) file.remove(fn)

  l.def <- vector("list")

  dims <- names(l$dims)
  l.def$dims <- plyr::llply(dims, function(dimname) {ncdf4::ncdim_def(dimname, units = "", vals=l$dims[[dimname]])}); names(l.def$dims) <- dims

  vars <- names(l$vars)
  l.def$vars <- plyr::llply(vars, function(varname) {
    dims <- names(attr(l$vars[[varname]], "dimnames"))
    dim <- plyr::llply(dims, function(dimname) l.def$dims[[dimname]])
    ncdf4::ncvar_def(name = varname, units = "", dim = dim, missval = -999)
  }); names(l.def$vars) <- vars

  nc.out <- ncdf4::nc_create(fn, l.def$var, force_v4 = TRUE)

  null <- plyr::llply(vars, function(varname) {
    ncdf4::ncvar_put(nc.out,l.def$vars[[varname]], l$vars[[varname]])
  })

  ncdf4::nc_close(nc.out)

  return(NULL)

}
