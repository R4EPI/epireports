#' Plot a population pyramid (age-sex) from a dataframe.
#'
#' @param data Your dataframe (e.g. linelist)
#' @param age_group the name of a column in the data frame that defines the age
#'   group categories. Defaults to "age_group"
#' @param split_by the name of a column in the data frame that defines the
#'   the bivariate column. Defaults to "sex". See NOTE
#' @param stack_by the name of the column in the data frame to use for shading
#'   the bars
#' @param proportional If `TRUE`, bars will represent proportions of cases out
#'   of the entire population. Otherwise (`FALSE`, default), bars represent
#'   case counts
#' @param na.rm  If `TRUE`, this removes NA counts from the age groups. Defaults
#'   to `TRUE`.
#' @param show_halfway When `TRUE` (default), a dashed vertical line will be
#'   added to each of the age bars showing the halfway point for the
#'   un-stratified age group. When `FALSE`, no halfway point is marked.
#' @param vertical_lines If you would like to add dashed vertical lines to help
#' visual interpretation of numbers. Default is to not show (`FALSE`),
#' to turn on write `TRUE`.
#' @param horizontal_lines If `TRUE` (default), horizontal dashed lines will
#'   appear behind the bars of the pyramid
#' @param pyramid if `TRUE`, then binary `split_by` variables will result in
#'   a population pyramid (non-binary variables cannot form a pyramid). If
#'   `FALSE`, a pyramid will not form.
#' @param pal a color palette function or vector of colors to be passed to
#'   [ggplot2::scale_fill_manual()] defaults to the first "qual" palette from
#'   [ggplot2::scale_fill_brewer()].
#'
#' @note If the `split_by` variable is bivariate (e.g. an indicator for
#' pregnancy), then the result will show up as a pyramid, otherwise, it will be
#' presented as a facetted barplot with with empty bars in the background
#' indicating the range of the un-facetted data set. Values of `spit_by` will
#' show up as labels at top of each facet.
#'
#' @importFrom apyramid age_pyramid
#' @export
#' @examples
#' library(ggplot2)
#' old <- theme_set(theme_classic(base_size = 18))
#'
#' set.seed(2018-01-15)
#' ages <- cut(sample(80, 150, replace = TRUE),
#'             breaks = c(0, 5, 10, 30, 90), right = FALSE)
#' sex  <- sample(c("Female", "Male"), 150, replace = TRUE)
#' gender <- sex
#' gender[sample(5)] <- "NB"
#' ill  <- sample(c("case", "non-case"), 150, replace = TRUE)
#' dat  <- data.frame(AGE              = ages,
#'                    sex              = factor(sex, c("Male", "Female")),
#'                    gender           = factor(gender, c("Male", "NB", "Female")),
#'                    ill              = ill,
#'                    stringsAsFactors = FALSE)
#'
#' # Create the age pyramid, stratifying by sex
#' print(ap   <- plot_age_pyramid(dat, age_group = AGE))
#'
#' # Create the age pyramid, stratifying by gender, which can include non-binary
#' print(apg  <- plot_age_pyramid(dat, age_group = AGE, split_by = gender))
#'
#' # Remove NA categories with na.rm = TRUE
#' dat2 <- dat
#' dat2[1, 1] <- NA
#' dat2[2, 2] <- NA
#' dat2[3, 3] <- NA
#' print(ap   <- plot_age_pyramid(dat2, age_group = AGE))
#' print(ap   <- plot_age_pyramid(dat2, age_group = AGE, na.rm = TRUE))
#'
#' # Stratify by case definition and customize with ggplot2
#' ap   <- plot_age_pyramid(dat, age_group = AGE, split_by = ill) +
#'   theme_bw(base_size = 16) +
#'   labs(title = "Age groups by case definition")
#' print(ap)
#'
#' # Stratify by multiple factors
#' ap <- plot_age_pyramid(dat,
#'                        age_group = AGE,
#'                        split_by = sex,
#'                        stack_by = ill,
#'                        vertical_lines = TRUE) +
#'   labs(title = "Age groups by case definition and sex")
#' print(ap)
#'
#' # Display proportions
#' ap <- plot_age_pyramid(dat,
#'                        age_group = AGE,
#'                        split_by = sex,
#'                        stack_by = ill,
#'                        proportional = TRUE,
#'                        vertical_lines = TRUE) +
#'   labs(title = "Age groups by case definition and sex")
#' print(ap)
#'
#' # empty group levels will still be displayed
#' dat3 <- dat2
#' dat3[dat$AGE == "[0,5)", "sex"] <- NA
#' plot_age_pyramid(dat3, age_group = AGE)
#' theme_set(old)
plot_age_pyramid <- function(data, age_group = "age_group", split_by = "sex",
                             stack_by = NULL, proportional = FALSE, na.rm = TRUE,
                             show_halfway = TRUE, vertical_lines = FALSE,
                             horizontal_lines = TRUE, pyramid = TRUE,
                             pal = NULL) {

  age_pyramid(data,
              age_group        = {{age_group}},
              split_by         = {{split_by}},
              stack_by         = {{stack_by}},
              proportional     = proportional,
              na.rm            = na.rm,
              show_midpoint    = show_halfway,
              vertical_lines   = vertical_lines,
              horizontal_lines = horizontal_lines,
              pyramid          = pyramid,
              pal              = pal
  )

}
