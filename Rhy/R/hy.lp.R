.init.hy.lp <-
function(where)
{
  hy.log.debug(".init.hy.lp")

  setClass("hy.lp.Dict", representation(dict = "matrix"), where = where)
  setMethod("initialize", "hy.lp.Dict", hy.lp.Dict.initialize, where = where)
  setMethod("show", "hy.lp.Dict", hy.lp.Dict.show, where = where)
  setMethod("[", "hy.lp.Dict", hy.lp.Dict.subset, where = where)
  setMethod("dim", "hy.lp.Dict", hy.lp.Dict.dim, where = where)
}

hy.lp.Dict.initialize <-
function(.Object, dict) {
  nr <- nrow(dict)
  nc <- ncol(dict)
  if (nr < 2 || nc < 2)
    stop("A dict should contain at least 2 rows and 2 columns.")
  if (is.null(colnames(dict)))
    colnames(dict) <- c("b", paste("x", 1:(nc - 1), sep = ""))
  if (is.null(rownames(dict)))
    rownames(dict) <- c(paste("x", (1:(nr - 1)) + (nc - 1), sep = ""), "z")
  .Object@dict <- dict
  return(.Object)
}

hy.lp.Dict.show <-
function(object) {
  hy.log.debug(".hy.lp.Dict.show")
  print(object@dict)
}

hy.lp.Dict.subset <-
function(x, i, j)
{
  x@dict[i, j]
}

hy.lp.Dict.dim <-
function(x)
{
  dim(x@dict)
}

hy.lp.Dict.choose <-
function(object)
{
  ret <- list(entering = NULL, leaving = NULL)
  D <- object@dict

  # choose entering variable
  coef.nonbasic <- D[nrow(D), -1]
  candidates.entering <- names(coef.nonbasic[coef.nonbasic > 0])
  if (length(candidates.entering) == 0)
    return(ret)
  entering.order <- order(as.integer(gsub("x", "", candidates.entering)))
  xj <- candidates.entering[entering.order[1]]
  ret$entering <- xj

  # choose leaving variable
  coef.xj <- D[-nrow(D), xj]
  negative.coef.xj.pos <- (coef.xj < 0)
  negative.coef.xj <- coef.xj[negative.coef.xj.pos]
  candidates.leaving <- names(negative.coef.xj)
  if (length(candidates.leaving) == 0)
    return(ret)
  b <- D[-nrow(D), 1]
  upperbound.xj <- b[negative.coef.xj.pos] / (-negative.coef.xj)
  idx <- which(upperbound.xj == min(upperbound.xj))
  candidates.leaving <- candidates.leaving[idx]
  leaving.order <- order(as.integer(gsub("x", "", candidates.leaving)))
  xi <- candidates.leaving[leaving.order[1]]
  ret$leaving <- xi

  return(ret)
}

hy.lp.Dict.pivot <-
function(object, entering, leaving)
{
  ret <- list(oldDict = object, entering = entering, leaving = leaving,
      newDict = NULL)

  # use shorter names
  D <- object@dict
  xe <- which(colnames(D) == entering)
  xl <- which(rownames(D) == leaving)

  if (length(xe) == 0 || length(xl) == 0)
    stop("invalid entering or leaving variables")

  row.leaving <- D[xl,]
  denominator <- -row.leaving[xe]
  row.leaving[xe] <- -1 
  row.leaving <- row.leaving / denominator

  for (i in 1:nrow(D)) {
    if (i == xl) {
      D[i,] <- row.leaving
    } else {
      c <- D[i, xe]
      D[i, xe] <- 0
      D[i,] <- D[i,] + row.leaving * c
    }
  }
  rownames(D)[xl] <- entering
  colnames(D)[xe] <- leaving

  ret$newDict <- new("hy.lp.Dict", dict = D)
  return(ret)
}
