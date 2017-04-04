fhdr = function (data, scores, alpha = c(0.01, 0.5), label = TRUE, xlab, ylab, 
                 plotlegend, legendpos, ncol, projmethod, ...) 
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
  ylim = range(y, na.rm = TRUE)
  band = Hscv.diag(sco, binned = TRUE)
  if (any(diag(band) < 10^(-30))) {
    stop("Computationally singular due to at least one of the diagonal elements of bandwidth matrix is very close to 0.")
  }
  else {
    den <- kde(x = sco, H = 0.8 * band)
    hdr1 <- hdrcde::hdr.2d(sco[, 1], sco[, 2], prob = alpha, list(x = den$eval.points[[1]], y = den$eval.points[[2]], z = den$estimate))
    index <- (hdr1$fxy < min(hdr1$falpha))
    outlier <- which(as.vector(index))
    # add by chris
    outlier <- outlier[order(rank(hdr1$fxy)[outlier])]
    #
    index <- (hdr1$fxy > hdr1$falpha[1])
    out <- which(as.vector(index))
    outcurve <- y[out, ]
    maximum2 <- apply(outcurve, 2, max, na.rm=TRUE)
    minimum2 <- apply(outcurve, 2, min, na.rm=TRUE)
    index <- (hdr1$fxy > hdr1$falpha[2])
    inside <- y[which(as.vector(index)),]
    maximum3 <- apply(inside, 2, max, na.rm=TRUE)
    minimum3 <- apply(inside, 2, min, na.rm=TRUE)
    #inside <- as.matrix(which(as.vector(index)))
    #insideone <- which(pam(inside, 2)$clustering == 2)
    #insidetwo <- which(pam(inside, 2)$clustering == 1)
    #insideone <- y[inside[insideone, ], ]
    #insidetwo <- y[inside[insidetwo, ], ]
    #maximum3 <- apply(insideone, 2, max, na.rm=TRUE)
    #minimum3 <- apply(insideone, 2, min, na.rm=TRUE)
    #maximum4 <- apply(insidetwo, 2, max, na.rm=TRUE)
    #minimum4 <- apply(insidetwo, 2, min, na.rm=TRUE)
    dist <- (sco[, 1] - hdr1$mode[1])^2 + (sco[, 2] - hdr1$mode[2])^2
    center <- order(dist)[1]
    centercurve <- y[center, ]
    n <- length(outlier)
    x <- data$x
    plot(c(x, rev(x)), c(maximum2, rev(minimum2)), type = "n", 
         main = "", ylim = ylim, xlab = xlab, ylab = ylab, 
         ...)
    polygon(c(x, rev(x)), c(maximum2, rev(minimum2)), border = FALSE, 
            col = "light gray", ylim = ylim, ...)
    polygon(c(x, rev(x)), c(maximum3, rev(minimum3)), border = FALSE, 
            col = "dark gray", ...)
    #polygon(c(x, rev(x)), c(maximum4, rev(minimum4)), border = FALSE, 
    #    col = "dark gray", ...)
    lines(fts(x, centercurve), col = "black", ...)
    if (n > 0) {
      outliercurve <- y[outlier, ]
      if (n == 1) {
        #lines(fts(x, as.matrix(outliercurve)), col = rainbow(n), ...)
        lines(fts(x, as.matrix(outliercurve)), col = rainbow(4)[1:length(outlier)], ...)
      }
      if (n > 1) {
        #lines(fts(x, t(outliercurve)), col = rainbow(n), ...)
        lines(fts(x, t(outliercurve)), col = rainbow(4)[1:length(outlier)], ...)
      }
      if (plotlegend == TRUE) {
        #legend(legendpos, c(colnames(data$y)[outlier]), col = rainbow(n), lty = 1, ncol = ncol, cex=1.5)
        legend(legendpos, c(colnames(data$y)[outlier]), col = rainbow(4)[1:length(outlier)], lty = 1, ncol = ncol, cex=1.5)
      }
    }
    return(outlier)
  }
}

