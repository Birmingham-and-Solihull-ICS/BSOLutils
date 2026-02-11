
example_dt <-
    data.frame(
        age_related_seed = rep(seq(0.8, 1.2, 0.07), 32),
        age_group_code = rep(seq(1,6), 32),
        sex_group_code = rep(rep(c(1,2), each = 6), 16),
        imd_code = rep(c(1,2,3,4,5,999), 32)
    )


set.seed(123)

example_dt$numerator <- rpois(192,example_dt$age_related_seed * 30)
example_dt$denominator <- rpois(192,example_dt$age_related_seed * 85)

ISR_example <- example_dt[, names(example_dt) != "age_related_seed", drop = FALSE]
usethis::use_data(ISR_example, overwrite = TRUE)

rate <-
    sum(example_dt$numerator) / sum(example_dt$denominator)


library(BSOLutils)


a <- ISR_deprivation(example_dt)

ISR_deprivation_plot(a)

