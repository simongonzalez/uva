#function to give the xy point given the origin point, a line length and an angle
#returns the xy of the other end of the line
#....................................................................................
lines_fn <- function(x0, y0, length_ln, angle_ln){
  
  angle_ln = angle_ln * pi / 180
  
  ab <- cos(angle_ln) * length_ln
  bc <- sign(sin(angle_ln)) * sqrt(length_ln^2 - ab^2)
  
  x1 <- x0 + ab
  y1 <- y0 + bc
  
  vals = c(x1,y1)
  
  return(vals)
}
#....................................................................................
#End of function
