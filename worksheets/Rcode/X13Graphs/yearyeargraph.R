# FUNCTION: yearyeargraph(m, abbrv, graph.title)
# PURPOSE: Creates a graph with each year its own line.
# ARGUMENTS: 
# - m:           a seasonal object
# - abbrv:       the series type to graph -- can be passed to series(m, abbrv)
# - graph.title: the graph title.
# OPTIONS:
# - abbrv =  "a1" - graph.title = "Original Series"
# - abbrv =  "b1" - graph.title = "Prior-Adjusted Series"
# - abbrv = "d11" - graph.title = "Seasonally Adjusted Series"
# - abbrv = "d12" - graph.title = "Trend"
yearyeargraph <- function(m, abbrv, graph.title = "") {
  fq  <- udg(m, "freq") # Period, number of observations per year
  ser <- series(m, abbrv)
  
  first.year <- start(ser)[1]
  last.year  <- end(ser)[1]
  
  if (last.year - first.year + 1 > 18) {
    first.year <- last.year - 17
    print("Only the last 18 years will be plotted.")
  }
  
  our.colors <- c(
    "navy", "purple", "magenta3", "thistle4", "steelblue4", "springgreen4", "lightseagreen",
    "darkgoldenrod2", "firebrick2", "darkred", "indianred4", "coral3", "rosybrown4", "gray57", "honeydew3",
    "yellowgreen", "darkorange", "pink2", "dodgerblue2"
  )
  
  
  plot(1:(fq + 1), type = "n", ylim = range(ser), bty = "o", las = "1", ann = FALSE, )
  title(main = paste(graph.title, " -- ", deparse(substitute(m))))
  if (fq == 12) {
    title(xlab = "Month")
  } else if (fq == 4) {
    title(xlab = "Quarter")
  } else {
    title(xlab = "Period")
  }
  
  y.vals <- window(ser, start = c(first.year, 1), end = c(first.year + 1, 1))
  x.ax.val <- start(ser)[2]:13
  lines(x.ax.val, y.vals, col = our.colors[1])
  
  for (v in first.year + 1:last.year) {
    y.vals <- window(ser, start = c(v, 1), end = c(v + 1, 1))
    x.ax.val <- 1:length(y.vals)
    lines(x.ax.val, y.vals, col = our.colors[v - first.year + 1])
  }
}
# END