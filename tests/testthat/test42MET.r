
cat("#### Test estimateV at, fa, rr functions for MET data with asreml42\n")
test_that("MET_estimateV_asreml42", {
  skip_if_not_installed("asreml")
  skip_on_cran()
  library(dae)
  library(asreml)
  library(asremlPlus)
  
  #Function too replace design matrix colnames when computing V
  replaceColnames <- function(des)
  { 
    colnam <- colnames(des)
    colnam <- strsplit(colnam, split = ":")
    colnam <- lapply(colnam, strsplit, split = "_")
    colnam <- lapply(colnam, 
                     function(nam) 
                       paste(sapply(nam, function(fac) fac[1]), collapse = ":"))
    colnames(des) <- colnam
    return(des)
  }
  
  data(MET)
  asreml.options(design = TRUE, keep.order=TRUE)
  #Test at
  asreml.obj <-asreml(fixed = GY.tha ~  + at(expt, c(1:5)):rep + at(expt, c(1)):vrow + 
                        at(expt, c(2,3,6,7)):colblocks + 
                        at(expt, c(1:5,7)):vcol + Genotype*Condition*expt,
                      random = ~  at(expt, c(1)):dev(vrow) + at(expt, c(2)):spl(vcol) +  
                        at(expt, c(3,5,7)):dev(vcol) + at(expt, c(7)):units,
                      data=comb.dat, maxit = 100, workspace = "1Gb")
  
  summary(asreml.obj)$varcomp
  ranterms <- names(asreml.obj$G.param)
  n <- nrow(comb.dat)
  design <- asreml.obj$design
  design <- replaceColnames(design)
  V.g <- matrix(0, nrow = n, ncol = n)
  for (term in ranterms)
  {
    cols <- grep(term, colnames(design), fixed = TRUE)
    V.g <- V.g + asreml.obj$vparameters[term] * (design[, cols] %*% 
                                                   t(as.matrix(design[, cols])))
  }
  V.g <- as.matrix(asreml.obj$sigma2 * (V.g + mat.I(n)))
  Vat <- estimateV(asreml.obj)
  testthat::expect_true(all.equal(Vat, V.g))
  R2.adj <- R2adj(asreml.obj)
  testthat::expect_true(all(abs(R2.adj - 86.26097) < 1e-04))
  set.daeTolerance(1e-04, 1e-04)
  R2.adj <- R2adj(asreml.obj, orthogonalize = "eigenmethods", 
                  include.which.fixed = as.formula(paste("~", 
                                                         paste0("at(expt, '", levels(comb.dat$expt)[1:5], 
                                                                "'):rep", collapse = " + "))))
  testthat::expect_true(all(abs(R2.adj - 50.50252) < 1e-04))
  R2.adj <- R2adj(asreml.obj, include.which.fixed = NULL, 
                  include.which.random = ~ at(expt, 'tcnue10'):dev(vcol))
  testthat::expect_true(all(abs(R2.adj - 0.7028481) < 1e-04))
  R2.adj <- R2adj(asreml.obj, include.which.fixed = NULL, 
                  include.which.random = ~ at(expt, 'tcnue10'):dev(vcol) + 
                    at(expt, 'rsnue11'):dev(vcol) + at(expt, 'tarlee13'):dev(vcol))
  testthat::expect_true(all(abs(R2.adj - 1.019012) < 1e-04))
  
  
  #Test fa
  asreml.obj <-asreml(fixed = GY.tha ~  + at(expt, c(1:5)):rep + at(expt, c(1)):vrow + 
                        at(expt, c(2,3,6,7)):colblocks + 
                        at(expt, c(1:5,7)):vcol + Condition*expt,
                      random = ~  fa(exptCond, k = 2):Genotype + 
                        at(expt, c(1)):dev(vrow) + at(expt, c(2)):spl(vcol) +  
                        at(expt, c(3,5,7)):dev(vcol) + at(expt, c(7)):units,
                      data=comb.dat, maxit = 100, workspace = "1Gb")
  
  summary(asreml.obj)$varcomp
  ranterms <- names(asreml.obj$G.param)
  n <- nrow(comb.dat)
  V.g <- matrix(0, nrow = n, ncol = n)
  # for (term in ranterms[2:7])
  # {
  #   cols <- grep(term, colnames(design), fixed = TRUE)
  #   print(length(cols))
  #   V.g <- V.g + asreml.obj$vparameters[term] * (asreml.obj$design[, cols] %*% 
  #                                                  t(as.matrix(asreml.obj$design[, cols])))
  # }
  # term <- ranterms[1]
  # term <- "fa(exptCond, k = 2)"
  # cols <- grep(term, colnames(asreml.obj$design), fixed = TRUE)[1:1364]
  # vp <- asreml.obj$vparameters[names(asreml.obj$vparameters)[grep(term, 
  #                                                                 names(asreml.obj$vparameters), fixed = TRUE)]]
  # spec.var <- diag(vp[grepl("!var", names(vp), fixed = TRUE)])
  # loads <- matrix(vp[grepl("!fa", names(vp), fixed = TRUE)], ncol = 2)
  # Gfa <- loads %*% t(loads) + spec.var
  # V.g <- V.g + (asreml.obj$design[, cols] %*% kronecker(Gfa, mat.I(62)) %*%
  #                 t(as.matrix(asreml.obj$design[, cols])))
  # V.g <- asreml.obj$sigma2 * (V.g + mat.I(n))
  
  design <- asreml.obj$design
  design <- replaceColnames(design)
  for (term in ranterms[2:7])
  {
    cols <- grep(term, colnames(design), fixed = TRUE)
    V.g <- V.g + asreml.obj$vparameters[term] * (design[, cols] %*% t(as.matrix(design[, cols])))
  }
  term <- ranterms[1]
  cols <- grep(term, colnames(design), fixed = TRUE)[1:1364]
  vp <- asreml.obj$vparameters[names(asreml.obj$vparameters)[grep(term, 
                                           names(asreml.obj$vparameters), fixed = TRUE)]]
  spec.var <- diag(vp[grepl("!var", names(vp), fixed = TRUE)])
  loads <- matrix(vp[grepl("!fa", names(vp), fixed = TRUE)], ncol = 2)
  Gfa <- loads %*% t(loads) + spec.var
  V.g <- V.g + (design[, cols] %*% kronecker(Gfa, mat.I(62)) %*%
                  t(as.matrix(design[, cols])))
  V.g <- asreml.obj$sigma2 * (V.g + mat.I(n))
  Vfa <- estimateV(asreml.obj)
  testthat::expect_true(all(abs(Vfa - V.g) < 1e-06))

  #Test rr
  asreml.obj <-asreml(fixed = GY.tha ~  + at(expt, c(1:5)):rep + at(expt, c(1)):vrow + 
                        at(expt, c(2,3,6,7)):colblocks + 
                        at(expt, c(1:5,7)):vcol + Condition*expt,
                      random = ~  rr(exptCond, k = 2):Genotype + 
                        at(expt, c(1)):dev(vrow) + at(expt, c(2)):spl(vcol) +  
                        at(expt, c(3,5,7)):dev(vcol) + at(expt, c(7)):units,
                      data=comb.dat, maxit = 100, workspace = "1Gb")
  
  summary(asreml.obj)$varcomp
  ranterms <- names(asreml.obj$G.param)
  n <- nrow(comb.dat)
  V.g <- matrix(0, nrow = n, ncol = n)
  design <- asreml.obj$design
  design <- replaceColnames(design)
  for (term in ranterms[2:7])
  {
    cols <- grep(term, colnames(design), fixed = TRUE)
    V.g <- V.g + asreml.obj$vparameters[term] * (design[, cols] %*% t(as.matrix(design[, cols])))
  }
  term <- ranterms[1]
  cols <- grep(term, colnames(design), fixed = TRUE)[1:1364]
  vp <- asreml.obj$vparameters[names(asreml.obj$vparameters)[grep(term, 
                                                                  names(asreml.obj$vparameters), fixed = TRUE)]]
  loads <- matrix(vp[grepl("!fa", names(vp), fixed = TRUE)], ncol = 2)
  Gfa <- loads %*% t(loads)
  V.g <- V.g + (design[, cols] %*% kronecker(Gfa, mat.I(62)) %*%
                  t(as.matrix(design[, cols])))
  V.g <- asreml.obj$sigma2 * (V.g + mat.I(n))
  Vrr <- estimateV(asreml.obj)
  testthat::expect_true(all(abs(Vrr - V.g) < 1e-06))
  
  asreml.options(design = FALSE) 

})





