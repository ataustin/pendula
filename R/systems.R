#' Make a system of initial conditions
#'
#' A "system" is a specification of the initial conditions for your double pendulum
#' simulation.  You can set arbitrary values for the physical properties of your setup.
#' Note that arguments numbered "1" refer to the pendulum closest to the system's origin
#' (in other words the primary pivot).
#'
#' @param gravity gravitational acceleration (in meters per second squared)
#' @param length1,length2 length of pendulum arms (in meters)
#' @param mass1,mass2 mass of the pendulum bobs (in kilograms)
#' @param angle1,angle2 counterclockwise deflection of the arms away from a resting state
#'                      (in degrees)
#' @param velocity1,velocity2 initial angular velocity of the arms (in degrees per second)
#'
#' @return a single-row data.frame with specified values
#' @export
make_system <- function(gravity = 9.8,
                        length1 = 1.0,
                        length2 = 1.0,
                        mass1 = 1.0,
                        mass2 = 1.0,
                        angle1 = 0.0,
                        angle2 = 0.0,
                        velocity1 = 0.0,
                        velocity2 = 0.0) {

  system <- data.frame(gravity = gravity,
                       length1 = length1,
                       length2 = length2,
                       mass1 = mass1,
                       mass2 = mass2,
                       angle1 = angle1,
                       angle2 = angle2,
                       velocity1 = velocity1,
                       velocity2 = velocity2)

  if(any(system < 0)) {
    stop("Cannot have negative values as parameters of the system.")
  }

  return(system)
}



#' Include another arbitrary system of initial conditions
#'
#'
add_system <- function(system, new_system) {
  if(!inherits(new_system, "data.frame")) {
    stop("New system must inherit from data.frame in order to be added.")
  }

  out <- rbind(system, new_system)
  out
}


single_tweaks <- function(start, tweak, n_tweaks) {
  seq(from = start, by = tweak, length.out = n_tweaks + 1)
}


tweak_system <- function(system, n_tweaks, ...) {
  if(nrow(system) > 1) {
    stop("You can only tweak a single system. You have too many rows in your system object.")
  }

  tweak_list <- list(...)

  if(!length(tweak_list)) {
    warning("No tweaks provided in the dots.  Returning original system.")
    return(system)
  }

  tweaks_in_system <- names(tweak_list) %in% names(system)
  if(!all(tweaks_in_system)) {
    bad_tweaks <- names(tweak_list)[!tweaks_in_system]
    stop(paste("The following parameters are not in your system; did you spell something wrong?\n ",
               paste(bad_tweaks, collapse = ", ")))
  }

  tweaked <- system[rep(1, n_tweaks + 1), ]
  for(el in names(tweak_list)) {
    tweaked[[el]] <- single_tweaks(system[[el]], tweak_list[[el]], n_tweaks)
  }

  rownames(tweaked) <- NULL

  row_has_negative <- apply(tweaked, 1, function(x) any(x < 0))
  if(any(row_has_negative)) {
    stop("Some tweaks resulted in negative values; please adjust number or kind of tweaks.")
  }

  tweaked
}
