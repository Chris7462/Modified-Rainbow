bhdr = function (data, scores, alpha = c(0.01, 0.5), label = TRUE, shadecols, 
                 pointcol, projmethod, ...) 
{
  y = t(data$y)
  x = data$x
  #   if(projmethod == "PCAproj")
  #   {
  #     sco = PCAproj(y, k = 2, center = median)$scores
  #   }
  #   if(projmethod == "rapca")
  #   {
  #     sco = fdpca(x, data$y)$coeff[,2:3]
  #     rownames(sco) = 1:ncol(data$y)
  #   }
  sco = scores[,1:2]
  band = Hscv.diag(sco, binned = TRUE)
  if (any(diag(band) < 10^(-30))) {
    stop("Computationally singular due to at least one of the diagonal elements of bandwidth matrix is very close to 0.")
  }
  else {
    den <- kde(x = sco, H = 0.8 * band)
    den <- list(x = den$eval.points[[1]], y = den$eval.points[[2]], z = den$estimate)
    hdr1 <- hdrcde::hdr.2d(sco[, 1], sco[, 2], prob = alpha, den)
    plot.hdr2d(hdrcde::hdr.2d(sco[, 1], sco[, 2], prob = alpha, den),
               shadecols = shadecols, pointcol = pointcol, xlab = "FPC score 1", 
               ylab = "FPC score 2", show.points = FALSE, , xaxs = "i", yaxs = "i", ...)
    #points(sco[, 1], sco[, 2], pch = 16, cex = 0.5, col = 1)
    points(sco[, 1], sco[, 2], pch = 16, col = 1)
    points(hdr1$mode[1], hdr1$mode[2], pch = 8, col = "red", cex = 1.5)
    index <- hdr1$fxy <= min(hdr1$falpha)
    outliers <- which(as.vector(index))
    points(sco[outliers, 1], sco[outliers, 2], col = rainbow(length(outliers)), pch = 16, cex = 1.5)

    if (label) {
      year = rownames(y)
      text(sco[outliers, 1] , sco[outliers, 2], year[outliers],
           adj = c(-0.2, 0.5) , cex = 1, col = rainbow(length(outliers)))
    }
    return(outliers)
  }
}

