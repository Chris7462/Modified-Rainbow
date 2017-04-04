bbag = function (data, scores, factor, label=TRUE, projmethod, ...) 
{
  y <- t(data$y)
  x = data$x
  #   if(projmethod == "PCAproj")
  #   {
  #     sco <- PCAproj(y, k = 2, center = median)$scores
  #   }
  #   if(projmethod == "rapca")
  #   {
  #     sco = fdpca(x, data$y)$coeff[,2:3]
  #     rownames(sco) = 1:ncol(data$y)
  #   }
  sco = scores[,1:2]
  colnames(sco) <- c('Comp.1', 'Comp.2')
  rownames(sco) <- 1:dim(sco)[1]
  tmp <- compute.bagplot(sco[, 1], sco[, 2], factor = factor, verbose = FALSE)
  if (tmp$is.one.dim == TRUE) {
    warning("Bivariate principal component scores lie in one direction.")
  }
  plot.bagplot(tmp, col.loophull = gray(0.95), col.baghull = gray(0.8), 
               show.whiskers = FALSE, xlab = "FPC score 1", ylab = "FPC score 2", ...)			

  #points(sco[, 1], sco[, 2], pch = 16, cex = 0.5, col = 1)
  points(sco[, 1], sco[, 2], pch = 16, col = 1)
  points(tmp$center[1], tmp$center[2], pch = 8, cex = 1.5, col = "red")
  outliers <- as.numeric(rownames(tmp$pxy.outlier))

  points(sco[outliers, 1], sco[outliers, 2], col = rainbow(length(outliers)), pch = 16, cex = 1.5)
  box()
  if (length(outliers) != 0) {
    if (label) {
      year = rownames(y)
      text(sco[outliers, 1], sco[outliers, 2], year[outliers], 
           adj = c(-0.2,0.5), cex = 1, col = rainbow(length(outliers)))
    }
    return(outliers)
  }
}

