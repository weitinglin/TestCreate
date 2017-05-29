context("create phenodata")

test_that("create a data.frame with phenodata suited for cel import",{
    test <- data.frame(Name=c("case_1","case_2","control_1"),
                       FileName=c("case_1","case_2","control_1"),
                       experiment.set=c("case_1","case_2","control_1"),
                       stringsAsFactors = FALSE)

    expect_that(create_phenodata(experiment.set = c("case_1","case_2","control_1")), equals(test))



})
