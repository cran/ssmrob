x2weight.covMcd <-
function(xMat)
{
  if(dim(xMat)[2]==3)
  {  
    tmc = qchisq(0.95, ncol(xMat[,-1]))
    cov1 = cov.rob(xMat[,-1], method="mcd")
    dist = mahalanobis(xMat[,-1], cov1$center, cov1$cov) 
    return(ifelse(dist<tmc, 1, tmc/dist))
  } else {
    tmc1 = qchisq(0.95, 2)
    tmc2 = qchisq(0.95, ncol(xMat[,-1]) - 2)
    imrnr = dim(xMat)[2]
    part1 = c(1 + which(abs(cor(xMat[,imrnr], xMat[,-c(1,imrnr)])) == min(abs(cor(xMat[,imrnr], xMat[,-c(1,imrnr)])))), imrnr)
    part2 = c(-1, -part1)
    cov1 = cov.rob(xMat[, part1], method="mcd")
    dist = mahalanobis(xMat[, part1], cov1$center, cov1$cov)
    weight1 = ifelse(dist < tmc1, 1, tmc1 / dist)
    cov2 = cov.rob(xMat[, part2], method="mcd")
    dist = mahalanobis(as.matrix(xMat[, part2]), cov2$center, cov2$cov) 
    weight2 = ifelse(dist < tmc2, 1, tmc2 / dist)
    xwght = weight1 * weight2
    return(xwght)
  }
}
