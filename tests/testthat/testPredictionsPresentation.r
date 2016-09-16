#devtools::test("asremlPlus")
context("prediction_presentation")

cat("#### Test for predictparallel.asreml\n")
test_that("predictparallel.asreml", {
  skip_if_not_installed("asreml")
  skip_on_cran()
  library(asreml)
  library(asremlPlus)
  data(WaterRunoff.dat)
  current.asr <- asreml(fixed = pH ~ Benches + (Sources * (Type + Species)), 
                        random = ~ Benches:MainPlots,
                        keep.order=TRUE, data= WaterRunoff.dat)
  current.asrt <- asrtests(current.asr, NULL, NULL)
  diffs <- predictparallel.asreml(classify = "Sources:Type", 
                                  asreml.obj = current.asr, tables = "none", 
                                  x.num = "xDay", x.fac = "Date", 
                                  x.pred.values=sort(unique(WaterRunoff.dat$xDay)),
                                  x.plot.values=c(0,28,56,84),
                                  wald.tab = current.asrt$wald.tab, 
                                  present = c("Type","Species","Sources"))
  testthat::expect_is(diffs, "alldiffs")
})

cat("#### Test for predictionplot.asreml\n")
test_that("predictionplot.asreml", {
  skip_if_not_installed("asreml")
  skip_on_cran()
  library(asreml)
  library(asremlPlus)
  data(WaterRunoff.dat)
  current.asr <- asreml(fixed = log.Turbidity ~ Benches + Sources + Type + Species +
                          Sources:Type + Sources:Species + 
                          Sources:xDay + Species:xDay + Species:Date,
                        data = WaterRunoff.dat, keep.order = TRUE)
  current.asrt <- asrtests(current.asr, NULL, NULL)
  predictions <- predict(current.asr, class="Species:Date:xDay", 
                         present = c("Type","Species","Sources"),
                         levels=list(xDay=unique(WaterRunoff.dat$xDay)))$predictions$pvals
  predictions <- predictions[predictions$est.status == "Estimable",]
  
  x.title <- "Days since first observation"
  names(x.title) <- "xDay"
  predictionplot.asreml(classify="Species:Date:xDay", y = "predicted.value", 
                        data = predictions, wald.tab = current.asrt$wald.tab, 
                        x.num = "xDay", x.fac = "Date", 
                        titles = x.title,
                        y.title = "Predicted log(Turbidity)",
                        present = c("Type","Species","Sources"),
                        error.intervals = "none", 
                        ggplotFuncs = list(ggtitle("Transformed turbidity over time")))

  
  testthat::expect_warning(diffs <- predictparallel.asreml(classify="Species:Date:xDay", 
                                                           present=c("Type","Species","Sources"), 
                                                           asreml.obj = current.asr, 
                                                           x.num = "xDay", x.fac = "Date", 
                                                           x.pred.values=sort(unique(WaterRunoff.dat$xDay)),
                                                           x.plot.values=c(0,28,56,84),
                                                           wald.tab = current.asrt$wald.tab))
  predictionplot.asreml(classify="Species:Date:xDay", y = "predicted.value", 
                        data = diffs$predictions, wald.tab = current.asrt$wald.tab, 
                        x.num = "xDay", x.fac = "Date", 
                        titles = x.title,
                        y.title = "Predicted log(Turbidity)")
  testthat::expect_silent("dummy")
})

cat("#### Test for pred.present.asreml\n")
test_that("pred.present.asreml", {
  skip_if_not_installed("asreml")
  skip_on_cran()
  library(asreml)
  library(asremlPlus)
  data(WaterRunoff.dat)
  titles <- list("Days since first observation", "Days since first observation", "pH", "Turbidity (NTU)")
  names(titles) <- names(WaterRunoff.dat)[c(5,7,11:12)]
  current.asr <- asreml(fixed = log.Turbidity ~ Benches + Sources + Type + Species + 
                          Sources:Type + Sources:Species + Sources:Species:xDay + 
                          Sources:Species:Date, 
                        data = WaterRunoff.dat, keep.order = TRUE)
  current.asrt <- asrtests(current.asr, NULL, NULL)
  testthat::expect_output(diff.list <- pred.present.asreml("Date:Sources:Species", 
                                   asreml.obj = current.asrt$asreml.obj, 
                                   wald.tab = current.asrt$wald.tab, 
                                   x.num = "xDay", x.fac = "Date", 
                                   x.pred.values=sort(unique(WaterRunoff.dat$xDay)),
                                   x.plot.values=sort(unique(WaterRunoff.dat$Day)),
                                   plots = "predictions", 
                                   error.intervals = "StandardError", 
                                   titles = titles, 
                                   transform.power = 0, 
                                   present = c("Type","Species","Sources"), 
                                   tables = "differences", levels.length = 6), 
                          regexp="All pairwise differences between predicted values")
  testthat::expect_equal(length(diff.list), 1)
  testthat::expect_match(names(diff.list), "Date.Sources.Species")
})