#function to find the angle between the origin point and the intersection point
#....................................................................................
find_angle = function(origin, point){
  zero_point.x = 10
  zero_point.y = 0
  
  measure_point.x = point[1] - origin[1]
  measure_point.y = point[2] - origin[2]
  
  angle = atan2(measure_point.y, measure_point.x) - atan2(zero_point.y, zero_point.x)
  
  #covert from radians to degrees
  angle = angle * 360 / (2*pi)
  
  if (angle < 0){
    angle = angle + 360
  }
  left_angle = angle
  right_angle = 180 - angle
  
  return(list(left_angle, right_angle))
}
#....................................................................................
#End of function
