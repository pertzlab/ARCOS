#' Calculate a vector of nearest neighbours
#'
#'
#' @param x a numeric vector.
#' @param ... parameters passed to the RANN:nn2() function.
#'
#' @keywords internal
#' @return a vector with nearest neighbours
#'
#' @import RANN
#'
#' @examples
#' library(ARCOS)
#'
#'
#'
calcVecNNdists <- function(x, k = 2, ...) {

  if (is.vector(x)) {
    if (length(x) > 1) {
      locNN = RANN::nn2(x, k = k, ...)
      locRes = locNN$nn.dist[, k]
    } else {
      locRes = NULL
    }
  } else {
    if (nrow(x) > 1) {
      locNN = RANN::nn2(x, k = k, ...)
      locRes = locNN$nn.dist[, k]
    } else {
      locRes = NULL
    }
  }


  return(locRes)
}


#' Calculate nearest-neighbour distances
#'
#' Wrapper for the \code{calcVecNNdists} function.
#'
#' @title "Calculate NN distances"
#' @param obj an arcosTS object.
#' @param nnn an integer, sets the n-th nearest neighbour; default 1.
#' @param act logical, if TRUE, the NN distance is calculated only on active objects, i.e. thos with binarised measurement greater than 1; default FALSE.
#'
#' @return a data.table
#'
#' @import data.table
#'
#' @rdname calcNNdists
#' @export calcNNdists
#'
#' @examples
#' library(ARCOS)
#' library(data.table)
#' ts = data.table(frame = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5),
#'                 objid = c(1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 4, 1, 4),
#'                 x = c(1, 3, 1.2, 2.5, 3.5, 0.9, 2.6, 2.9, 3.2, 1.1, 2.8, 3.1, 1, 3))
#'
#' arcosTS(ts,
#'         colPos = "x",
#'         col = list(Frame = "frame",
#'                    IDobj = "objid",
#'                    RT = NULL,
#'                    IDcoll = NULL),
#'         interVal = 1.,
#'         interType = "fixed")
#'
#' nnDists = calcNNdists(ts)
calcNNdists <- function(obj, nnn = 1, act = FALSE) {
  UseMethod("calcNNdists")
}

calcNNdists.default <- function(obj, nnn = 1, act = FALSE) {
  cat("This is a generic function\n")
}

#' @rdname calcNNdists
#' @export calcNNdists.arcosTS
#' @export
calcNNdists.arcosTS <- function(obj, nnn = 1, act = FALSE) {

  stopifnot(is.arcosTS(obj))

  stopifnot(nnn > 0)

  colFrame <- attr(obj, "colFrame")
  colPos <- attr(obj, "colPos")

  if (act) {
    colMeas <- attr(obj, "colMeasBin")

    locDT = obj[get(colMeas) > 0,
                .(nnDist = calcVecNNdists(.SD[,
                                              colPos,
                                              with = F],
                                          k = nnn + 1)),
                by = c(colFrame)]
  } else {
    locDT = obj[,
                .(nnDist = calcVecNNdists(.SD[,
                                              colPos,
                                              with = F],
                                          k = nnn + 1)),
                by = c(colFrame)]

  }


  return(locDT)
}
