
#' lb_to_kg
#'
#' @param dat_lb A vector of values in pounds
#'
#' @return A vector of values in kilograms; multiplied by 2.204624
#' @export
#'
lb_to_kg <- function(dat_lb) {
  dat_kg <- dat_lb * (2.204624)
  return(dat_kg)

}


#' kg_to_lb
#'
#' @param dat_kg A vector of values in kg
#'
#' @return A vector of values in pounds; multiplied by ~0.45
#' @export
#'
kg_to_lb <- function(dat_kg) {
  dat_lb <- dat_kg * (0.453592)
  return(dat_lb)

}


#' ac_to_ha
#'
#' @param dat_ac A vector of values in acres
#'
#' @return A vector of values in hectares; multiplied by ~0.41
#' @export
#'
ac_to_ha <- function(dat_ac) {
  dat_ha <- dat_ac * (0.4046863)
  return(dat_ha)

}


#' ha_to_ac
#'
#' @param dat_ha A vector of values in hectares
#'
#' @return A vector of values in acres; multiplied by ~2.47
#' @export
#'
ha_to_ac <- function(dat_ha) {
  dat_ac <- dat_ha * (2.47105)
  return(dat_ac)

}


#' ha_to_m2
#'
#' @param dat_ha A vector of values in hectares
#'
#' @return A vector of values in m2; multiplied by 10 000
#' @export
#'
ha_to_m2 <- function(dat_ha) {
  dat_m2 <- dat_ha * (10000)
  return(dat_m2)

}

#' mm_to_in
#'
#' @param dat_mm A vector of values in mm
#'
#' @return A vector of values in inches; multiplied by ~25.4
#' @export
#'
mm_to_in <- function(dat_mm) {
  dat_in <- dat_mm * (25.4)
  return(dat_in)

}
