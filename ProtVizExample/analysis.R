### analysis.R : my additional file ### 

library("RforProteomics")
data(qnt)

findClosest <- function(input) {
  if (is.null(input$plotma_click)) {
    1
  } else {
    # again log of x, so that points are 'close' in the log x axis
    pt <- c(input$plotma_click$x, y=input$plotma_click$y)
    sqdists <- colMeans((t(fData(qnt)[, c("baseMean", "log2FC")]) - pt)^2)
    which.min(sqdists)
  }
}
