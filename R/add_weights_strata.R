#' Add a column of stratified survey weights to a data frame.
#' For use in surveys where you took a sample population out of a larger
#' source population, with a simple-random or stratified survey design.
#'
#' Creates weight based on dividing stratified population counts from the source
#' population by surveyed counts in the sample population.
#'
#' @param x a data frame of survey data
#'
#' @param p a data frame containing population data for groups in `...`
#'
#' @param ... shared grouping columns across both `x` and `p`. These are used
#'   to match the weights to the correct subset of the population.
#'
#' @param population the column in `p` that defines the population numbers
#'
#' @param surv_weight the name of the new column to store the weights. Defaults to
#'   "surv_weight".
#'
#' @param surv_weight_ID the name of the new ID column to be created. Defaults to
#'   "surv_weight_ID"
#'
#' @author Zhian N. Kamvar Alex Spina Lukas Richter
#' @export
#'
#' @examples
#'
#' # define a fake dataset of survey data
#' # including household and individual information
#' x <- data.frame(stringsAsFactors=FALSE,
#'          cluster = c("Village A", "Village A", "Village A", "Village A",
#'                      "Village A", "Village B", "Village B", "Village B"),
#'     household_id = c(1, 1, 1, 1, 2, 2, 2, 2),
#'      eligibile_n = c(6, 6, 6, 6, 6, 3, 3, 3),
#'       surveyed_n = c(4, 4, 4, 4, 4, 3, 3, 3),
#'    individual_id = c(1, 2, 3, 4, 4, 1, 2, 3),
#'          age_grp = c("0-10", "20-30", "30-40", "50-60", "50-60", "20-30",
#'                      "50-60", "30-40"),
#'              sex = c("Male", "Female", "Male", "Female", "Female", "Male",
#'                      "Female", "Female"),
#'          outcome = c("Y", "Y", "N", "N", "N", "N", "N", "Y")
#' )
#'
#' # define a fake population data set
#' # including age group, sex, counts and proportions
#' p <- sitrep::gen_population(total = 10000,
#'   groups = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60"),
#'   proportions = c(0.1, 0.2, 0.3, 0.4, 0.2, 0.1))
#'
#'   # make sure col names match survey dataset
#' p <- dplyr::rename(p, age_grp = groups, sex = strata, population = n)
#'
#' # add weights to a stratified simple random sample
#' # weight based on age group and sex
#' add_weights_strata(x, p = p, age_grp, sex, population = population)
#'
add_weights_strata <- function(x, p, ...,
                               population = population,
                               surv_weight = "surv_weight",
                               surv_weight_ID = "surv_weight_ID") {

  ## define vars and throw error if not existant
  pop <- tidyselect::vars_select(names(p), {{population}}, .strict = FALSE)
  if (length(pop) == 0) {
    cll   <- match.call()
    ppltn <- rlang::as_name(rlang::enquo(population))
    stop(glue::glue("{ppltn} is not one of the columns of {cll[['p']]}, check spelling"))
  } else {
    population      <- rlang::ensym(pop)
  }
  surv_weight_ID  <- rlang::sym(surv_weight_ID)
  surv_weight     <- rlang::sym(surv_weight)
  onames          <- names(x)

  # create a merger ID by age group and sex
  p <- tidyr::unite(p, !!surv_weight_ID, ...)
  x <- tidyr::unite(x, !!surv_weight_ID, ..., remove = FALSE)

  # get study sample counts
  counts <- dplyr::count(x, !!surv_weight_ID)

  # merge counts to population data
  p <- dplyr::left_join(p, counts, by = rlang::as_name(surv_weight_ID))

  # create weight variable
  p <- dplyr::mutate(p, !!surv_weight := !!population / .data$n)
  p <- dplyr::select(p, !!surv_weight, !!surv_weight_ID)


  # merge to study sample
  res <- dplyr::left_join(x, p, by = rlang::as_name(surv_weight_ID))

  # return in original order of x
  dplyr::select(res, onames, !!surv_weight_ID, !!surv_weight)

}
