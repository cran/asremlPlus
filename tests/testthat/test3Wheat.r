#devtools::test("asremlPlus")
context("model_selection3")
asr3.lib <- "D:\\Analyses\\R oldpkg" 
  
cat("#### Test using wheat example with asreml3\n")
test_that("Wheat_asreml3", {
  skip_if_not_installed("asreml")
  skip_on_cran()
  library(dae)
  library(asremlPlus)
  loadASRemlVersion(3, lib.loc = asr3.lib)
  ## use asremlPlus to analyse the wheat (barley) example from section 8.6 of the asreml manual (Butler et al. 2010)
  data(Wheat.dat)
  
  # Fit initial model
  current.asr <- asreml(yield ~ Rep + WithinColPairs + Variety, 
                        random = ~ Row + Column + units,
                        rcov = ~ ar1(Row):ar1(Column), 
                        data=Wheat.dat)
  summary(current.asr)
  info <- infoCriteria(current.asr)
  testthat::expect_equal(info$DF, 5)
  testthat::expect_lt(abs(info$AIC - 1346.76), 1e-02)
  
  # Load current fit into an asrtests object
  current.asrt <- as.asrtests(current.asr, NULL, NULL)
  
  # Check for and remove any boundary terms
  current.asrt <- rmboundary(current.asrt)
  
  #Check term for within Column pairs
  current.asrt <- testranfix(current.asrt, "WithinColPairs", drop.fix.ns=TRUE)
  
  # Test nugget term
  current.asrt <- testranfix(current.asrt, "units", positive=TRUE)
  
  # Test Row autocorrelation
  current.asrt <- testresidual(current.asrt, "~ Row:ar1(Column)", 
                               label="Row autocorrelation", simpler=TRUE)
  
  # Test Col autocorrelation (depends on whether Row autocorrelation retained)
  k <- match("Row autocorrelation", current.asrt$test.summary$terms)
  p <- current.asrt$test.summary$p
  { if (p[k] <= 0.05)
    current.asrt <- testresidual(current.asrt, "~ ar1(Row):Column", 
                                 label="Col autocorrelation", simpler=TRUE,
                                 update=FALSE)
    else
      current.asrt <- testresidual(current.asrt, "~ Row:Column", 
                                   label="Col autocorrelation", simpler=TRUE,
                                   update=FALSE)
  }
  print(current.asrt)
  testthat::expect_equal(length(current.asrt), 3)
  testthat::expect_equal(nrow(current.asrt$wald.tab), 3)
  testthat::expect_equal(nrow(current.asrt$test.summary), 5)
  info <- infoCriteria(current.asrt$asreml.obj)
  testthat::expect_equal(info$DF, 5)
  testthat::expect_lt(abs(info$AIC - 1353.762), 1e-03)
  
  # Get current fitted asreml object
  current.asr <- current.asrt$asreml.obj
  current.asr <- update(current.asr, aom=TRUE)
  
  
  # Do residuals-versus-fitted values plot
  plot(fitted(current.asr),residuals(current.asr))

  # Form variance matrix based on estimated variance parameters
  s2 <- current.asr$sigma2
  gamma.Row <- current.asr$gammas[1]
  gamma.unit <- current.asr$gammas[2]
  rho.r <- current.asr$gammas[4]
  rho.c <- current.asr$gammas[5]
  row.ar1 <- mat.ar1(order=10, rho=rho.r)
  col.ar1 <- mat.ar1(order=15, rho=rho.c)
  V <- fac.vcmat(Wheat.dat$Row, gamma.Row) + 
    gamma.unit * diag(1, nrow=150, ncol=150) + 
    mat.dirprod(row.ar1, col.ar1)
  testthat::expect_equal(nrow(V), 150)
  testthat::expect_equal(ncol(V), 150)
  V <- s2*V
  
  #Produce variogram and variogram faces plot (Stefanaova et al, 2009)
  if (requireNamespace("lattice"))
  {
    plot.asrVariogram(variogram(current.asr))
  }
  faces <- variofaces(current.asr, V=V, maxiter=50, units="addtores")
  testthat::expect_equal(nrow(faces$face1), 10)
  testthat::expect_equal(nrow(faces$face2), 15)
  
  #Get Variety predictions and all pairwise prediction differences and p-values
  Var.diffs <- predictPlus(classify = "Variety", 
                           asreml.obj=current.asr, 
                           error.intervals="halfLeast",
                           wald.tab=current.asrt$wald.tab,
                           tables = "predictions", 
                           sortFactor = "Variety")
  testthat::expect_equal(nrow(Var.diffs$predictions), 25)
  testthat::expect_equal(ncol(Var.diffs$predictions), 6)
  testthat::expect_equal(nrow(Var.diffs$differences), 25)
  testthat::expect_equal(ncol(Var.diffs$differences), 25)
  testthat::expect_equal(nrow(Var.diffs$p.differences), 25)
  testthat::expect_equal(ncol(Var.diffs$p.differences), 25)
  testthat::expect_equal(length(Var.diffs$LSD), 3)
  testthat::expect_true("lower.halfLeastSignificant.limit" %in% names(Var.diffs$predictions))
  testthat::expect_equal(Var.diffs$backtransforms, NULL)
  testthat::expect_equal(as.character(Var.diffs$predictions$Variety[[1]]),"10")
  testthat::expect_silent(plotPvalues(Var.diffs))
})

