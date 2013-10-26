test.hy.lp.Dict <-
function()
{
  d1 <- new("hy.lp.Dict", dict = testdata.hy.lp.dict1)
  d2 <- new("hy.lp.Dict", dict = testdata.hy.lp.dict2)
  print(d1)
  cat("\n")
  print(d2)
}
