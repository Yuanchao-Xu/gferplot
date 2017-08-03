

# this is for setting plot radius
getRadius <- function(y, small = 1.5, medium = 3, large = 5) {
  r <- sapply(y, function(x) {
    if (x <= 40000 & x >= 15000) {
      x <- medium
    } else if (x < 15000) {
      x <- small
    } else {
      x <- large
    }
  })
  return (r)
}


#' plotScatterPie
#'
#' plot scatter pie chart for multidimension analysis, such as waternomics. This plot can
#' provide information about water use/wastewater of each provinces and GDP mix of each provinces,
#' see examples.
#' @param data a dataframe with information like x, y, r, etc. Refer to \code{GDPmix}
#' @param labelLine how far is label to pie chart, can be left with default value.
#' @param xmeanLine if plot x mean line
#' @param ymeanLine if plot y mean line
#' @importFrom graphics plot abline text
#' @importFrom ggrepel geom_text_repel
#' @importFrom scatterpie geom_scatterpie
#' @import ggplot2
#'

plotScatterPie <- function(data, xmeanLine = TRUE, ymeanLine = TRUE, labelLine = NULL) {

  with (data, {
    layer_basic <- ggplot()
    #  layer_points <- geom_point(data = data, aes(x, y, size = radius))

    data$radius <- getRadius(data$GDP)

    col <- c(rgb(107/255, 128/255, 51/255), rgb(2/255, 3/255, 3/255), rgb(13/255, 119/255, 185/255))

    layer_pie <- geom_scatterpie(data = data, aes(x, y, r = radius),
                                 cols = colnames(data)[3:5], color = NA)

    if (is.null(labelLine)) labelLine <- max(data$radius)/3

    layer_label <- geom_text_repel(data = data, aes(x, y, label = Province),
                                   point.padding = unit(labelLine, "lines"))
    #layer_legend <- geom_scatterpie_legend(data$radius, x= 0, y=0)

    layer_plot <- layer_basic + layer_pie + layer_label + scale_fill_manual(values=col) + coord_equal()

    if (xmeanLine == TRUE) layer_plot <- layer_plot + geom_vline(xintercept = mean(data$x), color = 'red', size = 1.5, linetype = 2)
    if (ymeanLine == TRUE) layer_plot <- layer_plot + geom_hline(yintercept = mean(data$y), color = 'red', size = 1.5, linetype = 2)



    style <- ggstyle()

    print(layer_plot + style)

  })
}


#' @import ggplot2
ggstyle <- function() {
  # a <- theme_classic() +
  #   theme(axis.line = element_line(size = 1, colour = "black"))
             #panel.background = element_rect(fill = "white"),
             #panel.grid.major = element_line(colour = "grey50"))
}





########################################################################################
#######################################################################################
#'
#'
#'
#' #' plotWaternomics
#' #'
#' #' plot special waternomics chart for CWR
#' #'
#' #'
#' #' @param data a dataframe containing columns of GDP, Value-added of fisrt/second/third industry, x and y
#' #' x and y are the coordinates of the plot, and it varies depending on needs, can be per capita
#' #' water use vs. per capita wastewater, etc. See example of year 2015 by \code{GDPmix}
#' #'
#' #' Columns should be exactly the same as GDPmix, which means, first column is province, seconnd is x,
#' #' third is y, fourth is Frist, etc. Load GDPmix to have a look at the columns.
#' #' @param title chart title
#' #' @param xname x axis name
#' #' @param yname y axis name
#' #' @param small radius of small circle, default is 1.5
#' #' @param medium radius of medium circle, default is 3
#' #' @param large radius of large circle, default is 5
#' #' @param legend whether to show legend, default is TRUE
#' #' @param label whether to show label, default is TRUE
#' #' @param xmean a line showing mean value of x
#' #' @param ymean a line showing mean value of y
#' #' @param line whether to show ablines, default is TRUE
#' #' @importFrom mapplots add.pie
#' #' @importFrom graphics plot plot.new
#' #' @importFrom grDevices rgb
#' #' @importFrom plotrix thigmophobe.labels
#' #' @export
#' #' @examples
#' #' \dontrun{
#' #' data(GDPmix)
#' #' plotwaternomics(GDPmix)
#' #' }
#'
#'
#'
#' plotWaternomics <- function(data, title = '', xname = '', yname = '', small = 1.5, medium = 3, large = 5,
#'                             legend = TRUE, label = TRUE, xmean = NULL, ymean = NULL, line = TRUE) {
#'   # plot basic plot based on x and y
#'   #plot.new()
#'
#'   # for some buffer space, extend the x and y axis, redefine them first
#'   xlim <- getLim(data$x)
#'
#'   ylim <- getLim(data$y)
#'
#'
#'   plot(data$x, data$y, main = title, xlab = xname, ylab = yname,
#'        xlim = xlim, ylim = ylim,
#'        bty = 'n')
#'
#'
#'
#'   provinceNum <- nrow(data)
#'
#'   # decide radius of the pie charts
#'   r <- getRadius(data$GDP, small, medium, large)
#'
#'   # define CWR color
#'   col <- c(rgb(107/255, 128/255, 51/255), rgb(2/255, 3/255, 3/255), rgb(13/255, 119/255, 185/255))
#'
#'
#'
#'
#'   # add pie charts
#'   for (i in 1:provinceNum) {
#'
#'     add.pie(x = data$x[i], y = data$y[i], z = c(data$First[i], data$Second[i], data$Third[i]),
#'             labels = '', radius = r[i], col = col, border = "white")
#'     #thigmophobe.labels(data$x[i], data$y[i], data$Province[i], offset = r[i]/3)
#'
#'   }
#'   if (legend == TRUE) {
#'     legend('topright', c("Agriculture","Industry","Services"), cex = 0.8,
#'            fill = col)
#'   }
#'   if (label == TRUE) {
#'     thigmophobe.labels(data$x, data$y, data$Province, offset = 1)
#'   }
#'
#'
#'   if (line == TRUE) {
#'     # add ablines of national mean or mean
#'     if (is.null(xmean)) xmean <- round(mean(data$x), 2)
#'     if (is.null(ymean)) ymean <- round(mean(data$y), 2)
#'     abline(h = ymean, v = xmean, col = '#c00000', lwd = 3, lty = 2)
#'   }
#'
#'   #spread.labels(data$x, data$y, data$Province, offsets = 0.01)
#'
#' }
#'
#'
#'
#' #' plotWaternomics_legend
#' #'
#' #' legend for plot special waternomics chart for CWR
#' #'
#' #' @param label whether to show label, default is TRUE
#' #' @importFrom mapplots add.pie
#' #' @importFrom graphics plot abline text
#' #' @importFrom grDevices rgb
#' #' @export
#' #' @examples
#' #' \dontrun{
#' #' plotwaternomics_legend()
#' #' }
#' plotWaternomics_legend <- function(label = TRUE) {
#'   r <- c(5, 3, 1.5)
#'   x <- c(0, 0, 0)
#'   y <- c(7, 16, 21)
#'   plot (x, y, xlim = c(-3, 3), ylim = c(0, 23), xaxt = 'n', yaxt = 'n', bty = 'n')
#'
#'   col <- c(rgb(107/255, 128/255, 51/255), rgb(2/255, 3/255, 3/255), rgb(13/255, 119/255, 185/255))
#'
#'   for (i in 1:3) {
#'     add.pie(x = x[i], y = y[i], z = c(1, 1, 1), radius = r[i], labels = '', col = col, border = 'white')
#'   }
#'   if (label == TRUE) {
#'     size <- 1.2
#'     text(-0.7, 8.5, "Service", col = 'white', cex = size)
#'     text(0.7, 8.5, "Agriculture", col = 'white', cex = size)
#'     text(0, 4, "Industry", col = 'white', cex = size)
#'   }
#' }
#'
#'
#'
#'
#'
#' getLim <- function(x) {
#'   # decide axis interval
#'   dig <- nchar(min(round(x)))
#'
#'   x1 <- round(min(x) - (max(x) - min(x))/length(x), -(dig - 1))
#'   x2 <- round(max(x) + (max(x) - min(x))/length(x), -(dig - 1))
#'
#'   return(c(x1, x2))
#' }
#'
#'
#'
