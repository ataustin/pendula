library(odeintr)
library(gganimate)

get_coords <- function(system, duration, step_size = 0.1) {
  variable_int <- integrate(system, duration, step_size)
  variable_int <- setNames(variable_int,
                           nm = c("time", names(get_system_values(system, "variables"))))
  coords <- variable_to_coord(variable_int, system)
  coords
}


integrate <- function(system, duration, step_size) {
  variables <- deg_to_rad(unlist(get_system_values(system, "variables")))
  variable_int <- odeintr::integrate_sys(partial_derivatives,
                                         init = variables,
                                         duration = duration,
                                         step_size = step_size)

  variable_int
}


variable_to_coord <- function(integration, system) {
  x1 <- system$length1 * sin(integration$angle1)
  y1 <- -system$length1 * cos(integration$angle1)
  x2 <- x1 + (system$length2 * sin(integration$angle2))
  y2 <- y1 - (system$length2 * cos(integration$angle2))

  coords <- data.frame(time = integration$time,
                       x1 = x1,
                       y1 = y1,
                       x2 = x2,
                       y2 = y2)

  coords
}


partial_derivatives <- function(vars, t) {
  # use a global variable "system" to to define constants gravity, length1, length2, mass1, mass2.
  # these don't vary so are not an argument to this function which is to be integrated

  angle1 <- vars[1]
  angle2 <- vars[2]
  velocity1 <- vars[3]
  velocity2 <- vars[4]

  d    <- numeric(length(vars))
  d[1] <- velocity1
  d[2] <- velocity2

  angle_diff <- angle2 - angle1
  sin_diff   <- sin(angle_diff)
  cos_diff   <- cos(angle_diff)
  mass_sum   <- system$mass1 + system$mass2

  denominator <- mass_sum * system$length1 - system$mass2 * system$length1 * (cos_diff^2)

  d[3] <- (system$mass2 * system$length1 * velocity1 * angle2 * sin_diff * cos_diff +
           system$mass2 * system$gravity * sin(angle2) * cos_diff +
           system$mass2 * system$length2 * (velocity2^2) * sin_diff -
           mass_sum     * system$gravity * sin(angle1)) /
           denominator

  d[4] <- (mass_sum * system$gravity * sin(angle1) * cos_diff -
           system$mass2 * system$length2 * (velocity2^2) * sin_diff * cos_diff -
           mass_sum * system$length1 * (velocity1^2) * sin_diff -
           mass_sum * system$gravity * sin(angle2)) /
          (denominator * system$length2 / system$length1)

  d
}




system <- make_system(angle1 = 175, angle2 = 180)

anim_time <- 10
anim_step <- 0.02

df <- get_coords(system,
                 duration = anim_time,
                 step_size = anim_step)

pl <-
  ggplot(df)+
    geom_segment(aes(xend=x1,yend=y1),x=0,y=0)+
    geom_segment(aes(xend=x2,yend=y2,x=x1,y=y1))+
    geom_point(size=5,x=0,y=0)+
    geom_point(aes(x1,y1),col="red",size=system$mass1)+
    geom_point(aes(x2,y2),col="blue",size=system$mass2)+
    scale_y_continuous(limits=c(-2,2))+
    scale_x_continuous(limits=c(-2,2))+
    theme_void() +
    transition_time(time) +
    labs(title = "{frame_time} seconds")

animate(pl,
        nframes = nrow(df),
        fps = 1/anim_step)
