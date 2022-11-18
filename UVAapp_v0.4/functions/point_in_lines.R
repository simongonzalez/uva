findInt <- function (x1,y1,x2,y2) {
  #
  # x1 and y1 are the coordinates of the points on the INCREASING curve.
  # x2 and y2 are the coordinates of the points on the DECREASING curve.
  #
  y1star <- approx(x2,y2,xout=x1,yleft=Inf,yright=-Inf)$y
  k <- sum(y1 <= y1star)
  y2star <- approx(x1,y1,xout=x2,yleft=-Inf,yright=Inf)$y
  ell <- sum(y2 = y2star)
  b1 <- y1[k]
  b2 <- y2[ell]
  m1 <- (y1[k+1] - y1[k])/(x1[k+1] - x1[k])
  m2 <- (y2[ell+1] - y2[ell])/(x2[ell+1] - x2[ell])
  x <- (b1-b2-m1*x1[k]+m2*x2[ell])/(m2-m1)
  y <- b1 + m1*(x-x1[k])
  c(x=x,y=y)
}

findInt(df$x, df$y, line_x, line_y)






m1 <- lm(df$y~df$x -1)
m2 <- lm(line_y~line_x -1)

a <- coef(m1)-coef(m2)

c(x=-a[[1]]/a[[2]], y=coef(m1)[[2]]*(-a[[1]]/a[[2]]) + coef(m1)[[1]])

above<-df$x>line_x

x1 = df$x
x2 = line_x

line_xy = data.frame(x = line_x, y = line_y)

mdl = data.frame(x = df$x, y = df$y)

l1 <- SpatialLines(list(Lines(list(Line(cbind(df$x, df$y))), 1)))
l2 <- SpatialLines(list(Lines(list(Line(cbind(line_x, line_y))), 1)))


print(middl_cnt)

m1 <- lm(middl_cnt$y~middl_cnt$x)
m2 <- lm(c(y, y2)~c(x, x2))

# Now calculate it!    
a <- coef(m1)-coef(m2)
val = c(x=-a[[1]]/a[[2]], y=coef(m1)[[2]]*(-a[[1]]/a[[2]]) + coef(m1)[[1]])

# cm <- rbind(coef(m1),coef(m2)) # Coefficient matrix
# 
# M <- matrix( c(coef(m1)[2], coef(m2)[2], -1,-1), nrow=2, ncol=2 )
# intercepts <- as.matrix( c(coef(m1)[1], coef(m2)[1]) )  # a column matrix
# -solve(M) %*% intercepts -> val

print(val)

#val = c(-solve(cbind(cm[,2],-1)) %*% cm[,1])


#....................................................................................
#End of function


