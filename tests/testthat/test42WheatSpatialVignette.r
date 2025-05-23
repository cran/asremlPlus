#devtools::test("asremlPlus")
context("model_selection")
if (Sys.getenv("NOT_CRAN") == "true") require(asreml)
library(asremlPlus)

cat("#### Test for wheat76 spatial example with asreml42\n")
test_that("Wheat_spatial_asreml42", {
  skip_if_not_installed("asreml")
  skip_on_cran()
  library(asreml)
  library(asremlPlus)
  library(qqplotr)
  ## use asremlPlus to analyse the wheat (barley) example from section 8.6 of the asreml manual (Butler et al. 2010)
  data(Wheat.dat)
  asreml::asreml.options(extra = 5, ai.sing = TRUE, fail = "soft")
  
  #Add row and column covariates
  tmp.dat <- within(Wheat.dat, 
                    {
                      cColumn <- dae::as.numfac(Column)
                      cColumn <- cColumn  - mean(unique(cColumn))
                      cRow <- dae::as.numfac(Row)
                      cRow <- cRow - mean(unique(cRow))
                    })

  current.asr <- do.call(asreml, 
                         list(yield ~ Rep + WithinColPairs + Variety, 
                              random = ~ Row + Column,
                              residual = ~ Row:Column,
                              data=tmp.dat, maxit = 10))
  summary(current.asr)$varcomp
  info <- infoCriteria(current.asr, IClikelihood = "full")
  testthat::expect_equal(info$varDF, 3)
  testthat::expect_lt(abs(info$AIC - 1720.891), 0.10)

  # Load init fit into an asrtests object
  current.asrt <- as.asrtests(current.asr, NULL, NULL, IClikelihood = "full", 
                              label = "Initial model")
  testthat::expect_lt(abs(current.asrt$test.summary$AIC - 1720.891), 0.50)


  # Check for and remove any boundary terms
  current.asrt <- rmboundary(current.asrt, IClikelihood = "full")


  #Check term for within Column pairs
  current.asrt <- changeModelOnIC(current.asrt, dropFixed = "WithinColPairs", 
                                  label = "Try dropping withinColPairs", IClikelihood = "full")
  print(current.asrt)

  #Try corb - worst fit - had to set nugget.variance and allow.corrsJointFit, but not fitting now
  corb.asrt <- addSpatialModelOnIC(current.asrt, spatial.model = "corr", 
                                   row.covar = "cRow", col.covar = "cColumn", 
                                   row.factor = "Row", col.factor = "Column", 
                                   corr.funcs = c("corb", "corb"), corr.orders = c(0,0),
                                   nugget.variance = TRUE, allow.corrsJointFit = TRUE, 
                                   IClikelihood = "full")
  corb.asrt <- rmboundary(corb.asrt, IClikelihood = "full")
  inf <- infoCriteria(corb.asrt$asreml.obj, IClikelihood = "full")
  testthat::expect_equal(inf$varDF, 6)
  testthat::expect_true(abs(inf$AIC - 1666.329) < 0.1)
  
  #Fit autocorrelation model
  spatialEach.asrts <- list()
  spatialEach.asrts[["corr"]] <- addSpatialModelOnIC(current.asrt, spatial.model = "corr", 
                                                     row.covar = "cRow", col.covar = "cColumn", 
                                                     row.factor = "Row", col.factor = "Column", 
                                                     IClikelihood = "full")
  spatialEach.asrts[["corr"]] <- rmboundary(spatialEach.asrts[["corr"]], IClikelihood = "full")

  spatialEach.asrts[["TPNCSS"]] <- addSpatialModelOnIC(current.asrt, spatial.model = "TPNCSS", 
                                                       row.covar = "cRow", col.covar = "cColumn", 
                                                       row.factor = "Row", col.factor = "Column", 
                                                       dropRandom = "Row + Column",
                                                       IClikelihood = "full")
  spatialEach.asrts[["TPNCSS"]] <- rmboundary(spatialEach.asrts[["TPNCSS"]], IClikelihood = "full")
  
  spatialEach.asrts[["TPPSC2"]] <- addSpatialModelOnIC(current.asrt, spatial.model = "TPPS", 
                                                      row.covar = "cRow", col.covar = "cColumn", 
                                                      row.factor = "Row", col.factor = "Column", 
                                                      dropRandom = "Row + Column",
                                                      degree = c(3,3), difforder = c(2,2), 
                                                      rotateX = TRUE, ngridangles = NULL, 
                                                      asreml.option = "grp", 
                                                      IClikelihood = "full")
  spatialEach.asrts[["TPPSC2"]] <- rmboundary(spatialEach.asrts[["TPPSC2"]], IClikelihood = "full")
  
  spatialEach.asrts[["TPPSL1"]] <- addSpatialModelOnIC(current.asrt, spatial.model = "TPPS", 
                                                      row.covar = "cRow", col.covar = "cColumn", 
                                                      row.factor = "Row", col.factor = "Column", 
                                                      dropRandom = "Row + Column",
                                                      degree = c(1,1), difforder = c(1,1),
                                                      asreml.option = "grp", 
                                                      IClikelihood = "full")
  spatialEach.asrts[["TPPSL1"]] <- rmboundary(spatialEach.asrts[["TPPSL1"]], IClikelihood = "full")
  
  
  infoEach <- do.call(rbind, 
                      lapply(spatialEach.asrts, 
                             function(asrt) infoCriteria(asrt$asreml.obj, IClikelihood = "full")))
  (infoEach)
  
  #Choose  spatial model
  spatial.asrts <- chooseSpatialModelOnIC(current.asrt, 
                                          row.covar = "cRow", col.covar = "cColumn",
                                          row.factor = "Row", col.factor = "Column",
                                          dropRandom = "Row + Column",
                                          rotateX = TRUE, ngridangles = NULL, 
                                          asreml.option = "mbf", return.asrts = "all")
  
  #Note that the fits of addSpatialModelOnIC and chooseSpatialModelOnIC differ for TPPSL1;
  #The fit for addSpatialModelOnIC has an extra variance parameter, but big changes 
  #   on last iteration; Very strange!
  print(spatial.asrts$spatial.IC)
  print(spatial.asrts$asrts$TPNCSS)
  testthat::expect_equal(length(spatial.asrts$asrts), 4)
  testthat::expect_equal(spatial.asrts$spatial.IC$varDF, c(3,5,6,7,3))
  testthat::expect_true(all(abs(spatial.asrts$spatial.IC$AIC - 
                                  c(1718.609, 1651.314, 1639.489, 1642.838, 1710.225) ) < 1e-02))
  testthat::expect_true(all.equal(spatial.asrts$spatial.IC[2:4,], infoEach[1:3 ,-3], 
                                  tolerance = 0.5))
  #theta.opt == c(0,0) because rotation Unswapped
  testthat::expect_true(all(abs(attr(spatial.asrts$asrts$TPSC2$asreml.obj, which = "theta.opt")[[1]] 
                            - c(20.20269, 64.97291)) < 1e-04))
  
  current.asr <- spatial.asrts$asrts$TPNCSS$asreml.obj
  printFormulae(current.asr)
  
  ## Get current fitted asreml object and update to include standardized residuals
  
  current.asr <- update(current.asr, aom=TRUE)
  Wheat.dat$res <- residuals(current.asr, type = "stdCond")
  Wheat.dat$fit <- fitted(current.asr)

  ## Do diagnostic checking
  
  ### Do residuals-versus-fitted values plot
  
  with(Wheat.dat, plot(fit, res))

  ### Plot variofaces
  
  variofaces(current.asr, V=NULL, units="addtores", 
             maxit=50, update = FALSE,
             ncores = parallel::detectCores())

  ### Plot normal quantile plot
  
  ggplot(data = Wheat.dat, mapping = aes(sample = res)) +
    qqplotr::stat_qq_band(bandType = "ts") + 
    qqplotr::stat_qq_line() + 
    qqplotr::stat_qq_point() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
         title = "Normal probability plot") +
    theme(plot.title = element_text(size = 12, face = "bold")) + theme_bw()

  ## Get Variety predictions and all pairwise prediction differences and p-values
  Var.diffs <- predictPlus(classify = "Variety", 
                           asreml.obj=current.asr, 
                           error.intervals="halfLeast",
                           wald.tab=current.asrt$wald.tab, 
                           sortFactor = "Variety",
                           tables = "predictions")
  
  ## Plot the Variety predictions, with halfLSD intervals, and the p-values
  
  plotPredictions(Var.diffs$predictions, 
                  classify = "Variety", y = "predicted.value", 
                  error.intervals = "half")
  plotPvalues(Var.diffs)
})
