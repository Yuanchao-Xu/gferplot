
#' plotScatterPie
#'
#' plot scatter pie chart for multidimension analysis, such as waternomics. This plot can
#' provide information about water use/wastewater of each provinces and GDP mix of each provinces,
#' see examples.
#' @param data a dataframe showing different management intersections. See examples
#' @param
#' @param newColname2
#' @importFrom tidyr gather
#' @import circlize
#' @examples
#'
#'
#'
#' \dontrun{
#' plotChord(cm)
#' }
#'
plotChord <- function(data) {

  # tidy up data
  a <- gather(data, 'responsibility', 'value', 2:ncol(data))

  # clean the canvass
  circos.clear()
  # plot basic plot
  chordDiagram(a, annotationTrack = "grid")

  # change label settings
  circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
                facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  }, bg.border = NA)


  # set up small gap and big gap
  small_gap = 1
  big_gap = 20

  # calculate angle for each category
  nr = nrow(data)
  nc = ncol(data)
  n_sector = nr + nc
  row_sector_degree = (360 - small_gap*(n_sector - 2) - big_gap*2) * 0.5 +
    small_gap*(nr-1)

  start_degree = 90 - (180 - row_sector_degree)/2

  # calculate starting degree
  gaps = c(rep(small_gap, nrow(data) - 1), big_gap, rep(small_gap, ncol(data) - 1), big_gap)
  circos.par(gap.after = gaps, start.degree = start_degree)

  circos.clear()
  abline(h = 0, lty = 2, col = "#00000080")


}
set.seed(999)
mat = matrix(sample(18, 18), 3, 6)
rownames(mat) = paste0("S", 1:3)
colnames(mat) = paste0("E", 1:6)

row_sum = sum(rowSums(abs(mat)))
col_sum = sum(colSums(abs(mat)))
small_gap = 1
big_gap = 20
nr = nrow(mat)
nc = ncol(mat)
n_sector = nr + nc
row_sector_degree = (360 - small_gap*(n_sector - 2) - big_gap*2) * (row_sum/(row_sum + col_sum)) +
  small_gap*(nr-1)
start_degree = 90 - (180 - row_sector_degree)/2
gaps = c(rep(small_gap, nrow(mat) - 1), big_gap, rep(small_gap, ncol(mat) - 1), big_gap)
circos.par(gap.after = gaps, start.degree = start_degree)
chordDiagram(mat, grid.col = grid.col)
circos.clear()
abline(v = 0, lty = 2, col = "#00000080")


