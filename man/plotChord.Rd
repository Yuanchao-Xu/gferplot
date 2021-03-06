% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotChord.R
\name{plotChord}
\alias{plotChord}
\title{plotScatterPie}
\usage{
plotChord(data, t = FALSE, ifsep = TRUE, trans = 0.3, highlight = NULL,
  xlim = c(-1, 1), ylim = c(-1, 1))
}
\arguments{
\item{data}{a dataframe showing different management intersections. See the data frame in the example}

\item{t}{is transpose the dataframe, by default, lines flow from row to column, if t == TRUE, lines will
flow from columns to rows. Once transposed,}

\item{ifsep}{if separate row and col categories in the chart, default is TRUE}

\item{trans}{transparency of the chart's lines, default is 0.3}

\item{highlight}{a string or string array of highlighted items, MUST be selected from first column (which represents names) or colnames.
if highlight has more than 2 items, they should belong to same category, either colnames, or names. One name and one column name is not allowed.}

\item{xlim}{x limit of the chart, default is c(-1, 1)}

\item{ylim}{y limte of the chart, default is c(-1, 1)}
}
\description{
if 'Summation of cell padding on y-direction are larger than the height of the cells' appears, just enlarge the xlim or ylim accordingly
}
\details{
plot scatter pie chart for multidimension analysis, such as waternomics. This plot can
provide information about water use/wastewater of each provinces and GDP mix of each provinces,
see examples.
}
\examples{



\dontrun{
plotChord(cm)
plotChord(cm, t = T)
plotChord(cm, highlight = 'MEP')
plotChord(cm, highlight = 'Investment')
}

}
