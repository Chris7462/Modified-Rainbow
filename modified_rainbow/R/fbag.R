fbag = function (data, scores, factor, xlim = NULL, ylim = range(data$y, na.rm = TRUE), xlab, ylab, plotlegend, legendpos, ncol, projmethod, ...) 
{
  y <- t(data$y)
  x <- data$x
  #   if(projmethod == "PCAproj")
  #   {
  #     rob <- PCAproj(y, k = 2, center = median)$score
  #   }
  #   if(projmethod == "rapca")
  #   {
  #     rob <- fdpca(x, data$y)$coeff[,2:3]
  #     rownames(rob) = 1:(dim(data$y)[2])
  #     colnames(rob) = c("Comp.1","Comp.2")
  #   }
  rob <- scores[,1:2]
  colnames(rob) <- c('Comp.1', 'Comp.2')
  rownames(rob) <- 1:dim(rob)[1]
  pcbag <- compute.bagplot(rob[, 1], rob[, 2], factor = factor)
  if (pcbag$is.one.dim == TRUE) {
    stop("Bivariate principal component scores lie in one direction.")
  }
  else {
    outlier <- as.numeric(rownames(pcbag$pxy.outlier))
    # add by Chris
    #outlier <- rev(outlier)
    inside <- as.numeric(rownames(pcbag$pxy.bag))
    insidecurve <- y[inside, ]
    maximum1 <- apply(insidecurve, 2, max, na.rm=FALSE)
    minimum1 <- apply(insidecurve, 2, min, na.rm=FALSE)
    out <- as.numeric(rownames(pcbag$pxy.outer))
    outcurve <- y[out, ]
    maximum2 <- apply(outcurve, 2, max, na.rm=FALSE)
    minimum2 <- apply(outcurve, 2, min, na.rm=FALSE)
    p = dim(y)[2]
    low = up = matrix(, p, 1)
    for (i in 1:p) {
      up[i, ] = quantile(outcurve[, i], probs = 0.75, na.rm=TRUE)
      low[i, ] = quantile(outcurve[, i], probs = 0.25, na.rm=TRUE)
    }
    IQR = up - low
    dist <- (rob[, 1] - pcbag$center[1])^2 + (rob[, 2] - pcbag$center[2])^2
    center <- order(dist)[1]
    centercurve <- y[center, ]
    #notchlow <- centercurve - 1.57 * (IQR)/sqrt(nrow(y))
    #notchupper <- centercurve + 1.57 * (IQR)/sqrt(nrow(y))
    n <- length(outlier)
    plot(c(x, rev(x)), c(maximum2, rev(minimum2)), type = "n", 
         main = "", ylim = ylim, xlab = xlab, ylab = ylab, ...)
    # remove nan points
    idx <- which(!is.na(maximum2))

    polygon(c(x[idx], rev(x[idx])), c(maximum2[idx], rev(minimum2[idx])), border = FALSE, 
            col = "light gray", ylim = ylim, ...)
    idx <- which(!is.na(maximum1))
    polygon(c(x[idx], rev(x[idx])), c(maximum1[idx], rev(minimum1[idx])), border = FALSE, 
            col = "dark gray", ...)
    #lines(fts(x, notchlow), col = "blue", lty = 2, ...)
    #lines(fts(x, notchupper), col = "blue", lty = 2, ...)
    lines(fts(x, centercurve), col = "black", ...)
    if (n > 0) {
      outliercurve <- y[outlier, ]
      if ( n == 1 ){
        #lines(fts(x, outliercurve), col = rainbow(n), ...)
        lines(fts(x, outliercurve), col = rainbow(4)[1:length(outlier)], ...)
        ncol = 1
      } else {
        #lines(fts(x, t(outliercurve)), col = rainbow(n), ...)
        lines(fts(x, t(outliercurve)), col = rainbow(4)[1:length(outlier)], ...)
      }
      if (plotlegend == TRUE) {
        #legend(legendpos, c(colnames(data$y)[outlier]), col = rainbow(n), lty = 1, ncol = ncol, cex=1.5)
        legend(legendpos, c(colnames(data$y)[outlier]), col = rainbow(4)[1:length(outlier)], lty = 1, ncol = ncol, cex=1.5)
      }
      return(outlier)
    }
  }
}
