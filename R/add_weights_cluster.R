#' Add a column of cluster survey weights to a data frame.
#'
#' For use in surveys where you took a sample population out of a larger
#' source population, with a cluster survey design.
#'
#' Will multiply the inverse chances of a cluster being selected, a household
#' being selected within a cluster, and an individual being selected within a
#' household.
#' 
#' As follows:
#'
#' ```
#' ((clusters available) / (clusters surveyed)) *
#' ((households in each cluster) / (households surveyed in each cluster)) *
#' ((individuals eligible in each household) / (individuals interviewed))
#' ```
#'
#' In the case where both ignore_cluster and ignore_household are TRUE, this
#' will simply be: 
#' 
#' ```
#' 1 * 1 * (individuals eligible in each household) / (individuals interviewed)
#' ```
#'
#' @param x a data frame of survey data
#'
#' @param cl a data frame containing a list of clusters and the number of
#'   households in each. 
#'
#' @param eligible the column in `x` which specifies the number of people
#'   eligible for being interviewed in that household. (e.g. the total number of
#'   children)
#'
#' @param interviewed the column in `x` which specifies the number of people
#'   actually interviewed in that household.
#'
#' @param cluster_cl the column in `cl` that lists all possible clusters.
#'   Ignored if `ignore_cluster` is TRUE.
#'
#' @param cluster_x the column in `x` that indicates which cluster rows belong
#'   to. Ignored if `ignore_cluster` is TRUE.
#'
#' @param household_cl the column in `cl` that lists the number of households
#'   per cluster.  Ignored if `ignore_household` is TRUE.
#'
#' @param household_x the column in `x` that indicates a unique household
#'   identifier. Ignored if `ignore_household` is TRUE.
#'
#' @param ignore_cluster If TRUE (default), set the weight for clusters to be 1.
#'   This assumes that your sample was taken in a way which is a close
#'   approximation of a simple random sample. Ignores inputs from `cluster_cl`
#'   as well as `cluster_x`. 
#'
#' @param ignore_household If TRUE (default), set the weight for households to 
#'   be 1. This assumes that your sample of households was takenin a way which
#'   is a close approximation of a simple random sample. Ignores inputs from
#'   `household_cl` and `household_x`.
#'
#' @param surv_weight the name of the new column to store the weights. Defaults 
#'   to "surv_weight".
#'
#' @param surv_weight_ID the name of the new ID column to be created. Defaults 
#'   to "surv_weight_ID"
#'
#' @author Alex Spina, Zhian N. Kamvar, Lukas Richter
#' @export
#' @examples
#'
#'
#' # define a fake dataset of survey data
#' # including household and individual information
#' x <- data.frame(stringsAsFactors=FALSE,
#'          cluster = c("Village A", "Village A", "Village A", "Village A",
#'                      "Village A", "Village B", "Village B", "Village B"),
#'     household_id = c(1, 1, 1, 1, 2, 2, 2, 2),
#'       eligible_n = c(6, 6, 6, 6, 6, 3, 3, 3),
#'       surveyed_n = c(4, 4, 4, 4, 4, 3, 3, 3),
#'    individual_id = c(1, 2, 3, 4, 4, 1, 2, 3),
#'          age_grp = c("0-10", "20-30", "30-40", "50-60", "50-60", "20-30",
#'                      "50-60", "30-40"),
#'              sex = c("Male", "Female", "Male", "Female", "Female", "Male",
#'                      "Female", "Female"),
#'          outcome = c("Y", "Y", "N", "N", "N", "N", "N", "Y")
#' )
#'
#' # define a fake dataset of cluster listings
#' # including cluster names and number of households
#' cl <- tibble::tribble(
#'      ~cluster, ~n_houses,
#'   "Village A",        23,
#'   "Village B",        42,
#'   "Village C",        56,
#'   "Village D",        38
#' )
#'
#'
#' # add weights to a cluster sample
#' # include weights for cluster, household and individual levels
#' add_weights_cluster(x, cl = cl, 
#'                     eligible = eligible_n, 
#'                     interviewed = surveyed_n,
#'                     cluster_cl = cluster, household_cl = n_houses,
#'                     cluster_x = cluster,  household_x = household_id,
#'                     ignore_cluster = FALSE, ignore_household = FALSE)
#'
#'
#' # add weights to a cluster sample
#' # ignore weights for cluster and household level (set equal to 1)
#' # only include weights at individual level
#' add_weights_cluster(x, cl = cl, 
#'                     eligible = eligible_n, 
#'                     interviewed = surveyed_n,
#'                     cluster_cl = cluster, household_cl = n_houses,
#'                     cluster_x = cluster,  household_x = household_id,
#'                     ignore_cluster = TRUE, ignore_household = TRUE)
#'
add_weights_cluster <- function(x, cl,
                                eligible, interviewed,
                                cluster_x = NULL, cluster_cl = NULL,
                                household_x = NULL, household_cl = NULL,
                                ignore_cluster = TRUE, ignore_household = TRUE,
                                surv_weight = "surv_weight", 
                                surv_weight_ID =  "surv_weight_ID") {

 

  # define vars
  clus_id_cl <- tidyselect::vars_select(colnames(cl), {{cluster_cl}})
  hh_id_cl   <- tidyselect::vars_select(colnames(cl), {{household_cl}})
  clus_id_x  <- tidyselect::vars_select(colnames(x),  {{cluster_x}})
  hh_id_x    <- tidyselect::vars_select(colnames(x),  {{household_x}})
  indiv_eli  <- tidyselect::vars_select(colnames(x),  {{eligible}})
  indiv_surv <- tidyselect::vars_select(colnames(x),  {{interviewed}})


  if (ignore_cluster) {
    cluster_chance <- 1
  } else {
    ## chance of a cluster being selected
    # the total number of clusters in the area
    n_clusters_total  <- length(unique(cl[[clus_id_cl]]))
    # the number of clusters that were surveyed
    n_clusters_chosen <- length(unique(x[[clus_id_x]]))
    # inverse chance of a cluster being chosen
    cluster_chance <- n_clusters_total / n_clusters_chosen
  }

  if (ignore_household) {
    cl$hh_chance <- 1
  } else {
    ## chance of a household being selected in each cluster
    # number of households chosen in each cluster
    n_households_chosen <- dplyr::summarise(
      dplyr::group_by(x, {{cluster_x}}),
      n_hh = sum(!duplicated({{household_x}}))
    )

    # join counts of households surveyed to list of clusters
    cl <- dplyr::left_join(cl, n_households_chosen, by = setNames(clus_id_x, clus_id_cl))

    # inverse chance of a household being chosen in each cluster
    cl[["hh_chance"]] <- cl[[hh_id_cl]] / cl[["n_hh"]]
  }


  # multiply cluster chance and household chance
  cl$clus_hh_chance <- cl$hh_chance * cluster_chance



  ## chance of an individual being selected in their household
  # create a subset - so we dont merge too many vars in later
  temp <- dplyr::select(x, clus_id_x, hh_id_x, indiv_eli, indiv_surv)

  # inverse chance of individual being chosen in their household
  temp[["indiv_chance"]] <- temp[[indiv_eli]] / temp[[indiv_surv]]

  # add in the cluster_household chance
  temp <- dplyr::left_join(temp, cl, by = setNames(clus_id_cl, clus_id_x))

  # create final weight by multiplying indivdual chance by cluster_household chance
  temp[[surv_weight]] <- temp$indiv_chance * temp$clus_hh_chance

  # create a survey weight id by merging cluster and household id
  temp <- tidyr::unite(temp, {{surv_weight_ID}}, clus_id_x, hh_id_x)

  # only keep the weight and the weight id
  temp <- dplyr::select(temp, {{surv_weight}}, {{surv_weight_ID}})

  # bind weights on to original dataset
  d <- dplyr::bind_cols(x, temp)

  # return new dataset with weights
  d

}

