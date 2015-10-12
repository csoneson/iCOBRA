## This function is almost verbatim copied from shiny::nearPoints. The only
## change is the inclusion of round() on line 27.
nearPoints2 <- function (df, coordinfo, xvar = NULL, yvar = NULL, panelvar1 = NULL,
                         panelvar2 = NULL, threshold = 5, maxpoints = NULL, addDist = FALSE,
                         allRows = FALSE) {
  if (is.null(coordinfo)) {
    if (addDist)
      df$dist_ <- NA_real_
    if (allRows)
      df$selected_ <- FALSE
    else df <- df[0, , drop = FALSE]
    return(df)
  }
  if (is.null(coordinfo$x)) {
    stop("nearPoints requires a click/hover/double-click object with x and y values.")
  }
  xvar <- shiny:::'%OR%'(xvar, coordinfo$mapping$x)
  yvar <- shiny:::'%OR%'(yvar, coordinfo$mapping$y)
  panelvar1 <- shiny:::'%OR%'(panelvar1, coordinfo$mapping$panelvar1)
  panelvar2 <- shiny:::'%OR%'(panelvar2, coordinfo$mapping$panelvar2)
  if (is.null(xvar))
    stop("nearPoints: not able to automatically infer `xvar` from coordinfo")
  if (is.null(yvar))
    stop("nearPoints: not able to automatically infer `yvar` from coordinfo")
  x <- shiny:::asNumber(df[[xvar]])
  y <- shiny:::asNumber(df[[yvar]])
  coordPx <- shiny:::scaleCoords(coordinfo$x, round(coordinfo$y), coordinfo)
  dataPx <- shiny:::scaleCoords(x, y, coordinfo)
  dists <- sqrt((dataPx$x - round(coordPx$x))^2 + (dataPx$y - coordPx$y)^2)
  if (addDist)
    df$dist_ <- dists
  keep_rows <- (dists <= threshold)
  if (!is.null(panelvar1))
    keep_rows <- keep_rows & shiny:::panelMatch(coordinfo$panelvar1,
                                        df[[panelvar1]])
  if (!is.null(panelvar2))
    keep_rows <- keep_rows & shiny:::panelMatch(coordinfo$panelvar2,
                                        df[[panelvar2]])
  keep_idx <- which(keep_rows)
  dists <- dists[keep_idx]
  keep_idx <- keep_idx[order(dists)]
  if (!is.null(maxpoints) && length(keep_idx) > maxpoints) {
    keep_idx <- keep_idx[seq_len(maxpoints)]
  }
  if (allRows) {
    df$selected_ <- FALSE
    df$selected_[keep_idx] <- TRUE
  }
  else {
    df <- df[keep_idx, , drop = FALSE]
  }
  df
}
