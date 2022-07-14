#' Save plots of collective events in 2D
#'
#' Save subsequent frames as PNG or PDF files with optional collective events overlaid as convex hull polygons
#' over the original data. Optionally, a binarised measurement can be plotted as black dots and
#' arbitrary annotations as asterisks.
#'
#' @param inDTobj a data.table with time series in long format; required.
#' @param inDTcoll a data.table with collective events in long format; e.g. created by trackCollEvents; default NULL.
#' @param inDTbin a data.table with a binarised measurement to plot as black dots; default NULL.
#' @param inDTanno a data.table with annotations to plot as asterisks.
#' @param inCols a list with column names, default list(frame = "frame", x = "x", y = "y", id = "trackID", clid = "clTrackID", m = "m", mb = "mb").
#' @param inDirPath a string with the path to save output images, default ".".
#' @param inFileCore a string with a prefix for output image files, default ".".
#' @param inXlim a vector with limits for the x-axis, default c(0, 1024).
#' @param inYlim a vector with limits for the y-axis, default c(0, 1024).
#' @param inPlotWH a vector with width and height of the output image in inches, default c(3,3).
#' @param inRevY logical, whether to reverse the y-axis, default TRUE.
#' @param inStretchF logical, whether to stretch the output sequence to include frames without objects, default TRUE.
#' @param inPtSzMain numeric, size of main points, default 1.
#' @param inPtSzBin numeric, size of points that indicate binarised measurement, default 0.1.
#' @param inPtSzAnno numeric, size of points that indicate annotated data, default 2.
#' @param inGGtheme additional ggplot2 theme definitions, default NULL.
#' @param inPlotType definition of the output image type, either "png" or "pdf".
#' @param inColPal colour palette for the main data, default 'RdYlBu'.
#'
#' @keywords internal
#' @return does not return any value; saves a sequence of images to the specified folder.
#' @import data.table
#'
#' @examples
#' cat("no example")
savePlotCollEvents2D = function(inDTobj,
                                inDTcoll = NULL,
                                inDTbin = NULL,
                                inDTanno = NULL,
                                inCols = list(frame = "frame",
                                              x = "x",
                                              y = "y",
                                              id = "trackID",
                                              clid = "clTrackID",
                                              m = "m",
                                              mb = "mb"),
                                inDirPath = ".",
                                inFileCore = "",
                                inXlim = c(0, 1024),
                                inYlim = c(0, 1024),
                                inPlotWH = c(3,3),
                                inRevY = TRUE,
                                inStretchF = TRUE,
                                inPtSzMain = 2,
                                inPtSzBin = 1,
                                inPtSzAnno = 2,
                                inGGtheme = NULL,
                                inPlotType = c("png", "pdf"),
                                inColPal = 'RdYlBu') {

  if (!dir.exists(inDirPath))
    dir.create(inDirPath, recursive = T)

  inPlotType = match.arg(inPlotType)

  # Check whether input is DT
  # Check whether specified column names exist in DTs

  # Stretch frames to include frames without objects.
  # Frames are stretched to a sequence with period 1
  if (inStretchF) {
    setkeyv(inDTobj,
            c(inCols$frame))

    inDTobj = inDTobj[setkeyv(inDTobj[,
                                      .(seq(min(inDTobj[[inCols$frame]], na.rm = T),
                                            max(inDTobj[[inCols$frame]], na.rm = T),
                                            1))],
                              c('V1'))]
  }

  ## Calculate convex hulls around collective events
  # Add selected cluster trackid's to the original table
  if (!is.null(inDTcoll)) {
    locDTcollObj = merge(inDTobj,
                         inDTcoll[,
                                  c(inCols$frame,
                                    inCols$id,
                                    inCols$clid),
                                  with = F],
                         by = c(inCols$frame,
                                inCols$id))



    locDTch = locDTcollObj[,
                           .SD[grDevices::chull(get(inCols$x),
                                                get(inCols$y))],
                           by = c(inCols$frame,
                                  inCols$clid)]
  }

  # Calculate min/max for the colour scale
  # Thanks to this, the colour scale will stay the same throughout the frame sequence
  locColourLim = c(min(inDTobj[[inCols$m]]),
                   max(inDTobj[[inCols$m]]))

  for (kk in unique(inDTobj[[inCols$frame]])) {

    # DT with all objects in a frame
    locDTobjF = inDTobj[get(inCols$frame) == kk]

    # DT with active objects
    if (!is.null(inDTbin)) {
      locDTbinF = inDTbin[get(inCols$frame) == kk]
    }

    # DT with convex hulls of collective events in a frame
    if (!is.null(inDTcoll)) {
      locDTchF = locDTch[get(inCols$frame) == kk]
    }

    # DT with manual annotations
    if (!is.null(inDTanno)) {
      locDTannoF = inDTanno[get(inCols$frame) == kk]
    } else {
      locDTannoF = NULL
    }

    locP = ggplot2::ggplot()

    if (nrow(locDTobjF) > 0) {
      locP = locP +
        ggplot2::geom_point(data = locDTobjF,
                            ggplot2::aes(x = .data[[inCols$x]],
                                         y = .data[[inCols$y]],
                                         color = .data[[inCols$m]]),
                            size = inPtSzMain,
                            stroke = 0,
                            shape=16,
                            alpha = 0.8) +
        ggplot2::scale_color_distiller(palette = inColPal,
                                       limits = locColourLim)
    }

    if (!is.null(inDTbin))
      if (nrow(locDTbinF) > 0) {
        locP = locP +
          ggnewscale::new_scale_color() +
          ggplot2::geom_point(data = locDTbinF,
                              ggplot2::aes(x = .data[[inCols$x]],
                                           y = .data[[inCols$y]],
                                           color = as.factor(get(inCols$mb))),
                              size = inPtSzBin,
                              alpha = 1,
                              stroke = 0,
                              shape=16,
                              color = "#000000")
      }


    if (!is.null(inDTcoll))
      if (nrow(locDTchF) > 0) {
        locP = locP +
          ggnewscale::new_scale_color() +
          ggplot2::geom_polygon(data = locDTchF,
                                ggplot2::aes(x = get(inCols$x),
                                             y = get(inCols$y),
                                             group = get(inCols$clid)),
                                fill = NA,
                                size = 0.25,
                                color = "#000000")
      }

    if (!is.null(inDTanno))
      if (nrow(locDTannoF) > 0) {
        locP = locP +
          ggnewscale::new_scale_color() +
          ggplot2::geom_point(data = locDTannoF,
                              ggplot2::aes(x = get(inCols$x),
                                           y = get(inCols$y)),
                              size = inPtSzAnno,
                              stroke = 1,
                              alpha = 0.8,
                              color = "#00FFFF",
                              shape = 4)
      }

    if (inRevY) {
      locP = locP +
        ggplot2::scale_y_reverse() +
        ggplot2::coord_fixed(ratio = 1,
                             xlim = inXlim,
                             ylim = rev(inYlim),
                             expand = F)
    } else {
      locP = locP +
        ggplot2::coord_fixed(ratio = 1,
                             xlim = inXlim,
                             ylim = inYlim,
                             expand = F)

    }

    locP = locP +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL) +
      inGGtheme

    ggplot2::ggsave(filename = sprintf("%s/%sF%04d.%s", inDirPath, inFileCore, kk, inPlotType),
                    plot = locP,
                    width = inPlotWH[1],
                    height = inPlotWH[2])
  }
}


#' Save plots of collective events
#'
#' Wrapper for the \code{savePlotCollEvents2D} function.
#'
#' @title "Save plots of collective events"
#' @param objTS an arcosTS object with time series.
#' @param objColl an arcosTS object with collective events, defult NULL.
#' @param plotbin a boolean, whether dots with binarised measurement should be plotted; default FALSE.
#' @param datanno a data.table with T/X/Y annotations overlaid on the plot, default NULL.
#' @param outdir a string with the dierctory to save the folder with frames; default ".".
#' @param filecore a string with a prefix for output image files, default ".".
#' @param xlim a vector with limits for the x-axis, default c(0, 1024).
#' @param ylim a vector with limits for the y-axis, default c(0, 1024).
#' @param plotwh a vector with width and height of the output image in inches, default c(3,3).
#' @param revy logical whether to reverse the y-axis, default TRUE.
#' @param stretchf logical whether to stretch the output sequence to include frames without objects, default TRUE.
#' @param ptszmain numeric, size of main points in the plot, default 1.
#' @param ptszbin numeric, size of points that indicate binarised measurement, default 0.1.
#' @param ptszanno numeric, size of points that indicate annotated data, default 2.
#' @param ggtheme a ggtheme object with additional style definitions; default NULL.
#' @param imtype definition of the output image type, either "png" or "pdf".
#' @param colpal colour palette for the main data, default 'RdYlBu'.
#'
#' @return NULL
#'
#' @rdname savePlotColl2D
#' @export savePlotColl2D
#'
#' @examples
#' cat("no examples")
savePlotColl2D <- function(objTS,
                           objColl = NULL,
                           plotbin = FALSE,
                           datanno = NULL,
                           outdir = ".",
                           filecore = "",
                           xlim = c(0, 1024),
                           ylim = c(0, 1024),
                           plotwh = c(3,3),
                           revy = FALSE,
                           stretchf = TRUE,
                           ptszmain = 2,
                           ptszbin = 1,
                           ptszanno = 2,
                           ggtheme = NULL,
                           imtype = c("png", "pdf"),
                           colpal = 'RdYlBu') {
  UseMethod("savePlotColl2D")
}

savePlotColl2D.default <- function(objTS,
                                   objColl = NULL,
                                   plotbin = FALSE,
                                   datanno = NULL,
                                   outdir = ".",
                                   filecore = "",
                                   xlim = c(0, 1024),
                                   ylim = c(0, 1024),
                                   plotwh = c(3,3),
                                   revy = FALSE,
                                   stretchf = TRUE,
                                   ptszmain = 2,
                                   ptszbin = 1,
                                   ptszanno = 2,
                                   ggtheme = NULL,
                                   imtype = c("png", "pdf"),
                                   colpal = 'RdYlBu') {
  cat("This is a generic function\n")
}

#' @rdname savePlotColl2D
#' @export savePlotColl2D.arcosTS
#' @export
savePlotColl2D.arcosTS <- function(objTS,
                                   objColl = NULL,
                                   plotbin = FALSE,
                                   datanno = NULL,
                                   outdir = ".",
                                   filecore = "",
                                   xlim = c(0, 1024),
                                   ylim = c(0, 1024),
                                   plotwh = c(3,3),
                                   revy = TRUE,
                                   stretchf = TRUE,
                                   ptszmain = 2,
                                   ptszbin = 1,
                                   ptszanno = 2,
                                   ggtheme = NULL,
                                   imtype = c("png", "pdf"),
                                   colpal = 'RdYlBu') {

  stopifnot(is.arcosTS(objTS))
  #stopifnot(is.arcosTS(objColl))

  colFrame <- attr(objTS, "colFrame")
  colIDobj <- attr(objTS, "colIDobj")
  colIDcoll <- attr(objColl, "colIDcoll")
  colPos <- attr(objTS, "colPos")
  colMeas <- attr(objTS, "colMeas")
  colMeasBin <- attr(objTS, "colMeasBin")

  if (is.null(colFrame))
    stop("Frame number column not defined in the data.")

  if (is.null(colIDobj))
    stop("Object/track identifier column not defined in the data.")

  #if (is.null(colIDcoll))
  #  stop("Collective event identifier column not defined in the data.")

  if (is.null(colPos))
    stop("Position columns not defined in the data.")

  if (is.null(colMeas))
    stop("Measurement column not defined in the data.")

  if (is.null(colMeasBin) & plotbin)
    stop("You asked to plot a layer with binarised measurement but the column is not defined in the data.")


  if (plotbin) objTSbin = objTS[get(colMeasBin) > 0] else objTSbin = NULL

  savePlotCollEvents2D(inDTobj = objTS,
                       inDTcoll = objColl,
                       inDTbin = objTSbin,
                       inDTanno = datanno,
                       inCols = list(frame = colFrame,
                                     x = colPos[1],
                                     y = colPos[2],
                                     id = colIDobj,
                                     clid = colIDcoll,
                                     m = colMeas,
                                     mb = colMeasBin),
                       inDirPath = outdir,
                       inFileCore = filecore,
                       inXlim = xlim,
                       inYlim = ylim,
                       inPlotWH = plotwh,
                       inRevY = revy,
                       inStretchF = stretchf,
                       inPtSzMain = ptszmain,
                       inPtSzBin = ptszbin,
                       inPtSzAnno = ptszanno,
                       inGGtheme = ggtheme,
                       inPlotType = imtype,
                       inColPal = colpal)
}
