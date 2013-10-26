reload <-
function()
{
  install.packages("Rhy", repos = NULL)
  if ("package:Rhy" %in% search())
    detach("package:Rhy", unload = TRUE)
  library("Rhy")
}
