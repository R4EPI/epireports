


nsampled <- c(5, 3)
Ct <- 4L # total clusters
Cn <- 2L # number sampled

Ht <- rep(c(23, 42), times = nsampled) # total households
Hn <- rep(c(2,  1),  times = nsampled) # number sampled

En <- rep(c(6,  3),  times = nsampled) # Expected individuals
Sn <- rep(c(4,  3),  times = nsampled) # surveyed individuals

full  <- (Ct / Cn) * (Ht / Hn) * (En / Sn)
no_hh <- (Ct / Cn) *    1L     * (En / Sn)
no_cl <-    1L     * (Ht / Hn) * (En / Sn)
ignor <-    1L     *    1L     * (En / Sn)

input_linelist <- data.frame(stringsAsFactors=FALSE,
         cluster = c("Village A", "Village A", "Village A", "Village A",
                     "Village A", "Village B", "Village B", "Village B"),
    household_id = c(1, 1, 1, 1, 2, 2, 2, 2),
      eligible_n = c(6, 6, 6, 6, 6, 3, 3, 3),
      surveyed_n = c(4, 4, 4, 4, 4, 3, 3, 3),
   individual_id = c(1, 2, 3, 4, 4, 1, 2, 3),
         age_grp = c("0-10", "20-30", "30-40", "50-60", "50-60", "20-30",
                     "50-60", "30-40"),
             sex = c("Male", "Female", "Male", "Female", "Female", "Male",
                     "Female", "Female"),
         outcome = c("Y", "Y", "N", "N", "N", "N", "N", "Y")
)

clusters <- data.frame(stringsAsFactors=FALSE,
     cluster = c("Village A", "Village B", "Village C", "Village D"),
    n_houses = c(23, 42, 56, 38)
)

test_that("add_weights will ignore clusters and households by default", {

  res <- add_weights_cluster(x = input_linelist, cl = clusters,
                             cluster_x    = cluster,
                             cluster_cl   = cluster,
                             household_x  = household_id,
                             household_cl = n_houses,
                             eligible     = eligible_n,
                             interviewed  = surveyed_n,
                             surv_weight  = "wuzzlewozzle")

  expect_equal(res$wuzzlewozzle, ignor)

})

test_that("clusters and households can be identified", {

  res <- add_weights_cluster(x = input_linelist, cl = clusters,
                             cluster_x    = cluster,
                             cluster_cl   = cluster,
                             household_x  = household_id,
                             household_cl = n_houses,
                             eligible     = eligible_n,
                             interviewed  = surveyed_n,
                             ignore_cluster = FALSE,
                             ignore_household = FALSE,
                             surv_weight  = "wuzzlewozzle")

  expect_equal(res$wuzzlewozzle, full)

})

test_that("clusters can be ignored", {

  res <- add_weights_cluster(x = input_linelist, cl = clusters,
                             cluster_x    = cluster,
                             cluster_cl   = cluster,
                             household_x  = household_id,
                             household_cl = n_houses,
                             eligible     = eligible_n,
                             interviewed  = surveyed_n,
                             ignore_cluster = TRUE,
                             ignore_household = FALSE,
                             surv_weight  = "wuzzlewozzle")

  expect_equal(res$wuzzlewozzle, no_cl)

})

test_that("households can be ignored", {

  res <- add_weights_cluster(x = input_linelist, cl = clusters,
                             cluster_x    = cluster,
                             cluster_cl   = cluster,
                             household_x  = household_id,
                             household_cl = n_houses,
                             eligible     = eligible_n,
                             interviewed  = surveyed_n,
                             ignore_cluster = FALSE,
                             ignore_household = TRUE,
                             surv_weight  = "wuzzlewozzle")

  expect_equal(res$wuzzlewozzle, no_hh)

})



# checking that the weight produced is correct
# based on lecture from Manchester university
# https://www.methods.manchester.ac.uk/themes/survey-and-statistical-methods/survey-weights/

clusters <- data.frame(stringsAsFactors=FALSE,
                             cluster = c(1:20),
                             n_houses   = c(200, 250, rep(300, times = 18)))

input_linelist <- data.frame(stringsAsFactors=FALSE,
                             household_id = c(1:10),
                             cluster = c(rep(1, times = 5),
                                         rep(2, times = 5)),
                             eligible_n = c(2, 3, 1, 4, 2, 2, 3, 1, 4, 3),
                             surveyed_n = c(rep(1, times = 10)),
                             manc_weight = c(800, 1200, 400, 1600, 800, 1000,
                                             1500, 500, 2000, 1500)
                            )


test_that("weights calculated correctly", {

  res <- add_weights_cluster(x = input_linelist, cl = clusters,
                             cluster_x    = cluster,
                             cluster_cl   = cluster,
                             household_x  = household_id,
                             household_cl = n_houses,
                             eligible     = eligible_n,
                             interviewed  = surveyed_n,
                             ignore_cluster = FALSE,
                             ignore_household = FALSE,
                             surv_weight  = "wuzzlewozzle")

  expect_equal(res$wuzzlewozzle, res$manc_weight)

})
