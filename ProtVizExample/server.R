### server.R : builds the plots and tables ###

library(shiny)
shinyServer(function(input, output) {
  
  # MA-plot
  output$plotma <- renderPlot({
    # process possible input
    idx <- findClosest( input )
    par(mar=c(5,5,3,2),cex.lab=2,cex.main=2,cex.axis=1.5)
    # MA-plot of all genes
    plot(fData(qnt)$baseMean, fData(qnt)$log2FC,
         xlab = "A", ylab = "M")
    # add circle for the selected point
    points(fData(qnt)$baseMean[idx],
           fData(qnt)$log2FC[idx],
           col="dodgerblue", cex=3, lwd=3)
  })

  # counts plot
  output$plotcounts <- renderPlot({
    # process possible input
    idx <- findClosest( input )
    par(mar=c(5,5,3,2),cex.lab=2,cex.main=2,cex.axis=1.5)
    # plot the counts for the selected gene
    plot(1:6,
         exprs(qnt)[idx, ],
         col = c("red", "steelblue")[as.numeric(qnt$conditions)],
         pch = 19,
         xlab = "samples",
         ylab = "intensity")
  })
  
})
