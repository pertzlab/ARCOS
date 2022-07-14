#' Export data to Napari
#'
#' @title "Export data to Napari"
#' @param objColl an arcosTS object with collective events.
#' @param dirpath a string with the file name.
#' @param ... parameters passed to (unused)
#'
#' @return writes files to a disk
#' @import data.table
#'
#' @rdname export2napari
#' @export export2napari
#'
#' @examples
#' cat("no examples")
export2napari <- function(objColl, dirpath, ...) {
  UseMethod("export2napari")
}

export2napari.default <- function(objColl, dirpath, ...) {
  cat("This is a generic function\n")
}

#' @rdname export2napari
#' @export export2napari.arcosTS
#' @export
export2napari.arcosTS <- function(objColl, dirpath, ...) {

  stopifnot(is.arcosTS(objColl))

  colFrame <- attr(objColl, "colFrame")
  colIDcoll <- attr(objColl, "colIDcoll")
  colPos <- attr(objColl, "colPos")


  ## Prepare the layer with collective events
  locCols4napari <- c(
    colIDcoll,
    colFrame,
    rev(colPos))

  dtNap <- objColl[,
                   ..locCols4napari]

  setnames(dtNap,
           c("index", "axis-0", "axis-1", "axis-2"))

  fwrite(dtNap,
         file.path(dirpath, "napari-coll.csv"),
         row.names = F)

  ## Prepare the layer with convex hulls around collective events
  locDTch <- objColl[,
                     .SD[grDevices::chull(get(colPos[1]),
                                          get(colPos[2]))],
                     by = c(colFrame,
                            colIDcoll)]

  # Remove single vertices
  setkeyv(locDTch,
          c(colFrame,
            colIDcoll))

  locDTch <- locDTch[ setkeyv(locDTch[,
                                      .N,
                                      by = c(colIDcoll,
                                             colFrame)][N > 1],
                              c(colFrame,
                                colIDcoll)) ]

  # Additional columns to conform to Napari standard
  locCols4napari <- c(
    colIDcoll,
    colFrame,
    rev(colPos))

  dtNap <- locDTch[,
                   ..locCols4napari]

  dtNap[,
        c("shape-type") := "polygon"]

  dtNap[,
        c("vertex-index") := 0:(.N-1),
        by = c(colFrame,
               colIDcoll)]

  dtNap[,
        index := .GRP - 1,
        by = c(colFrame,
               colIDcoll)]

  locCols4napari <- c(
    "index",
    "shape-type",
    "vertex-index",
    colFrame,
    rev(colPos),
    colIDcoll)

  dtNap <- dtNap[,
                 ..locCols4napari]

  setnames(dtNap,
           c("index", "shape-type", "vertex-index", "axis-0", "axis-1", "axis-2", "collid"))

  fwrite(dtNap,
         file.path(dirpath, "napari-chull.csv"),
         row.names = F)
}
