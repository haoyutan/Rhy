.onAttach <-
function(libname, pkgname)
{
  hy.log.debug(".onAttach")
  where <- as.environment(paste("package:", pkgname, sep = ""))
  .init.hy.lp(where)
}
