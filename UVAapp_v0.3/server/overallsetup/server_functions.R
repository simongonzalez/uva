#======================================================================================
#======================================================================================
# FUNCTIONS----------------------------------------------------------------------------
#======================================================================================
#======================================================================================

#Negative selections
`%nin%` <- Negate(`%in%`)

#Function to find the angle between the origin point and the intersection point
#This function finds the angle from the origin point to a given point
#....................................................................................
find_angle <- function(origin, point){
  #xy points for reference
  zero_point_x <- 10
  zero_point_y <- 0
  
  #differences between xy points
  measure_point_x <- point[1] - origin[1]
  measure_point_y <- point[2] - origin[2]
  
  #angle in radians between the two points
  angle <- atan2(measure_point_y, measure_point_x) - atan2(zero_point_y, zero_point_x)
  
  #covert from radians to degrees
  angle <- angle * 360 / (2*pi)
  
  #convert angles to inverted y axis (for pixels)
  if (angle < 0){
    angle <- angle + 360
  }
  
  left_angle <- angle
  right_angle <- 180 - angle
  
  return(list(left_angle, right_angle))
}

#Function to give the xy point given the origin point, a line length and an angle
#Returns the xy of the other end of the line
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