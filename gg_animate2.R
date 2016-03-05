gg_animate2 <- function(p = last_plot(), filename = NULL,
                       saver = NULL, title_frame = TRUE, ...) {
  if (is.null(p)) {
    stop("no plot to save")
  }
  
  built <- ggplot_build(p)
  
  # get frames
  frames <- plyr::compact(lapply(built$data, function(d) d$frame))
  
  if (length(frames) == 0) {
    stop("No frame aesthetic found; cannot create animation")
  }
  
  if (is.factor(frames[[1]])) {
    # for factors, have to use unlist to combine
    frames <- sort(unique(unlist(frames)))
  } else {
    frames <- sort(unique(do.call(c, frames)))
  }
  frames <- sort(unique(frames))
  
  plots <- lapply(frames, function(f) {
    # replace each data object with a subset
    b <- built
    for (i in seq_along(b$data)) {
      frame_vec <- b$data[[i]]$frame
      if (!is.null(frame_vec)) {
        sub <- (frame_vec == f | is.na(frame_vec))
        if (!is.null(b$data[[i]]$cumulative)) {
          sub <- sub | (b$data[[i]]$cumulative & (frame_vec <= f))
        }
        
        b$data[[i]] <- b$data[[i]][sub, ]
      }
    }
    
    # title plot according to frame
    if (title_frame) {
      if (!is.null(b$plot$labels$title)) {
        b$plot$labels$title <- paste(b$plot$labels$title, f)
      } else {
        b$plot$labels$title <- f
        l <- which(rbind(sapply(b$data, function(x) x$label == "frame")) == TRUE)
        if (length (l) > 0){
          b$data[[l]]$label <- paste(substr(f, 6, 7), substr(f, 1, 4))
        }
        m <- which(rbind(sapply(b$data, function(x) x$label == "@lustyandlewd")) == TRUE)
        if (length (m) > 0){
          b$data[[m]]$label <- NULL
        }
      }
    }
    
    b
  })
  
  ret <- list(plots = plots, frames = frames)
  class(ret) <- "gg_animate"
  
  if (!is.null(filename)) {
    gg_animate_save(ret, filename, saver, ...)
    ret$saved <- TRUE
  } else {
    ret$ani_opts <- list(...)
    ret$saved <- FALSE
  }
  
  ret
}


#' Print a gganimate object, allowing browsing in RStudio
#'
#' Print a gganimate object as browsable HTML, which allows visualization
#' directly in RStudio. If we are in knitr, directly print each of the
#' images instead (you should use the \code{fig.show = "animate"} option
#' in the chunk).
#'
#' @param x gg_animate object
#' @param format What format to display in, such as "gif" (default),
#' "mp4", or "avi".
#' @param ... Extra arguments for the <img> or <video> tag, such
#' as width or height
#'
#' This saves the plot to a file using \code{\link{gg_animate_save}}
#' (and then loads the contents of that file into memory) if it has
#' not already been saved.
#'
#' @export
print.gg_animate <- function(x, format = "gif", ...) {
  # if knitr is running, use a special case. Print all figures
  if (!(is.null(getOption("knitr.in.progress")))) {
    # don't print if it has already been saved
    if (!x$saved) {
      for (pl in x$plots) {
        plot_ggplot_build(pl)
      }
    }
    return()
  }
  
  # if it has not yet been saved to a file, save now (to a temporary file)
  if (!x$saved) {
    x <- do.call(gg_animate_save, c(list(x, saver = format), x$ani_opts))
  }
  
  # construct HTML
  if (!is.null(x$mime_type) && grepl("^video", x$mime_type)) {
    d <- htmltools::tags$video(htmltools::tags$source(src = x$src),
                               autoplay = TRUE,
                               loop = TRUE, ...)
  } else if (!is.null(x$mime_type) && grepl("^image", x$mime_type)) {
    d <- htmltools::tags$img(src = x$src, ...)
  } else {
    message("opening gganimate file stored at", x$filename)
    auto_browse(x$filename)
    return()
  }
  
  print(htmltools::browsable(d))
}