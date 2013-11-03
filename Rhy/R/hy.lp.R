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
  ret <- list(inputDict = object, entering = entering, leaving = leaving,
      outputDict = NULL)

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

  ret$outputDict <- new("hy.lp.Dict", dict = D)
  return(ret)
}

hy.lp.Dict.magicPivot <-
function(object)
{
  D <- object@dict
  nr <- nrow(D)

  if (rownames(D)[nr] != "w" || colnames(D)[2] != "x0")
    warning("This Dict is not a legal auxiliary Dict.")
 
  b <- D[-nr, 1]
  leaving <- names(b)[which.min(b)]
  hy.lp.Dict.pivot(object, "x0", leaving)
}

hy.lp.Dict.solve <-
function(object, trace.steps = FALSE)
{
  D <- object
  nr <- nrow(D)
  nc <- ncol(D)

  ret <- list(inputDict = D, outputDict = D, z = D[nr, 1], iter = 0,
    status = "initial")
  if (trace.steps)
    ret$steps <- list()

  iter <- 0
  while (TRUE) {
    el <- hy.lp.Dict.choose(D)
    if (is.null(el$entering)) {
      ret$status <- "optimal"
      return(ret)
    } else if (is.null(el$leaving)) {
      ret$status <- "unbounded"
      return(ret)
    }

    iter <- iter + 1
    step <- hy.lp.Dict.pivot(D, el$entering, el$leaving)

    ret$z <- step$outputDict[nr, 1]
    ret$iter <- iter
    ret$status = "ongoing"
    ret$outputDict <- D <- step$outputDict

    if (trace.steps) {
      step$inputDict <- NULL
      ret$steps[[length(ret$steps) + 1]] <- step
    }
  }
  # should not reach here
}

hy.lp.Dict.infeasible2Aux <-
function(object)
{
  D <- object@dict
  nr <- nrow(D)
  nc <- ncol(D)

  D.aux <- matrix(0, nrow = nr, ncol = nc + 1)
  D.aux[1:(nr - 1), 1] <- D[1:(nr - 1), 1]
  D.aux[nr, 2] <- -1
  D.aux[1:(nr - 1), 2] <- 1
  D.aux[1:(nr - 1), -c(1, 2)] <- D[1:(nr - 1), -1]

  rownames(D.aux) <- c(rownames(D)[-nr], "w")
  colnames(D.aux) <- c("b", "x0", colnames(D)[-1])

  new("hy.lp.Dict", D.aux)
}

hy.lp.Dict.aux2InitDict <-
function(object, c)
{
  D <- object@dict
  nr <- nrow(D)
  nc <- ncol(D)

  D.init <- matrix(0, nrow = nr, ncol = nc - 1)
  D.init[1:(nr - 1), 1] <- D[1:(nr - 1), 1]

  x0.col <- which(colnames(D) == "x0")
  D.init[nr, -1] <- c
  D.init[1:(nr - 1), -1] <- D[1:(nr - 1), -c(1, x0.col)]

  rownames(D.init) <- c(rownames(D)[-nr], "z")
  colnames(D.init) <- colnames(D)[-x0.col]

  new("hy.lp.Dict", D.init)
}

hy.lp.stdForm2Dict <-
function(c, A, b)
{
  ncon <- nrow(A)  # number of constraints
  nvar <- ncol(A)  # number of input variables
  nslack <- ncon   # number of slack variables

  D <- matrix(0, nrow = ncon + 1, ncol = nvar + 1)
  rownames(D) <- c(paste("x", nvar + 1:nslack, sep = ""), "z")
  colnames(D) <- c("b", paste("x", 1:nvar, sep = ""))

  D[, 1] <- c(b, 0)
  D[-nrow(D), -1] <- -A
  D[nrow(D),] <- c(0, c)

  new("hy.lp.Dict", D)
}

hy.lp.stdForm2AuxDict <-
function(A, b)
{
  ncon <- nrow(A)  # number of constraints
  nvar <- ncol(A)  # number of input variables
  nslack <- ncon   # number of slack variables

  D <- matrix(0, nrow = ncon + 1, ncol = nvar + 2)
  rownames(D) <- c(paste("x", nvar + 1:nslack, sep = ""), "w")
  colnames(D) <- c("b", paste("x", 0:nvar, sep = ""))

  D[, 1] <- c(b, 0)
  D[, 2] <- 1
  D[-nrow(D), -c(1, 2)] <- -A
  D[nrow(D), 1:2] <- c(0, -1)

  new("hy.lp.Dict", D)
}

hy.lp.solve <-
function(obj, mat, dir, rhs, bounds = NULL, types = NULL, max = TRUE,
    enable.trace = FALSE, extra = list(), ...)
{
  ret <- list(problem = NULL, solution = NULL, trace = NULL)
  ret$problem <- list(obj = obj, mat = mat, dir = dir, rhs = rhs,
      bounds = bounds, types = types, max = max, extra = extra)
  if (enable.trace)
    ret$trace <- list()

  trace <- function(key, message) {
    if (is.null(key))
      key <- length(ret$trace) + 1
    if (enable.trace)
      ret$trace[[key]] <<- message
  }

  c <- obj
  A <- mat
  b <- rhs
  op <- dir
  
  if (all(b >= 0)) {
    trace(NULL, "All elements in b are >= 0. Skip Initialization phase.")

    D <- hy.lp.stdForm2Dict(c, A, b)
    trace("FeasibleDictionary", D)

    step.result <- hy.lp.Dict.solve(D, trace.steps = enable.trace)
    trace("OptimizationPhase", step.result)

    ret$solution <- list(status = step.result$status, objective = step.result$z)
    trace("Solution", ret$solution)
  } else {
    trace(NULL, "Some elements in b are < 0. Require Initialization phase.")

    D.aux <- hy.lp.stdForm2AuxDict(A, b)
    trace("InitialAuxiliaryDictionary", D.aux)

    step.result <- hy.lp.Dict.magicPivot(D.aux)
    trace("MagicPivot", step.result)

    D.aux <- step.result$outputDict
    trace("FeasibleAuxiliaryDictionary", D.aux)

    step.result <- hy.lp.Dict.solve(D.aux, trace.steps = enable.trace)
    trace("InitializationPhase", step.result)

    if (step.result$status == "optimal" && step.result$z == 0) {
      trace(NULL, "The optimal solution of the auxiliary dictionary is 0.")

      D.aux <- step.result$outputDict
      trace("FinalAuxiliaryDictionary", D.aux)

      D <- hy.lp.Dict.aux2InitDict(D.aux, c)
      trace("InitialDictionary", D)

      step.result <- hy.lp.Dict.solve(D, trace.steps = enable.trace)
      trace("OptimizationPhase", step.result)

      ret$solution <- list(status = step.result$status, objective = step.result$z)
      trace("Solution", ret$solution)
    } else if (step.result$status == "optimal" && step.result$z < 0) {
      trace(NULL, "The optimal solution of the auxiliary dictionary is < 0.")
      trace(NULL, "The original problem is infeasible.")

      ret$solution <- list(status = "infeasible", objective = NA)
      trace("Solution", ret$solution)
    } else if (step.result$status == "unbounded") {
      trace(NULL, "The auxiliary problem is unbounded.")
      trace(NULL, "Should not get here. How's that possbile?")
    } else {
      trace(NULL, "I don't know what happens.")
      trace(NULL, "Should not get here. How's that possbile?")
    }
  }

  return(ret)
}
