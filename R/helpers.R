#' convert degrees to radians
deg_to_rad <- function(degrees) {
  return(pi * degrees / 180)
}


#' get a particular set of system values
get_system_values <- function(system, values = c("constants", "variables")) {
  match.arg(values)

  value_names <- switch(values,
                        constants = c("gravity", "length1", "length2", "mass1", "mass2"),
                        variables = c("angle1", "angle2", "velocity1", "velocity2"))
  return(system[value_names])
}
