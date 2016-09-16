#devtools::test("asremlPlus")
context("model_selection")

cat("#### Test for choose.model.asrtests\n")
test_that("choose.model.asrtests", {
  skip_if_not_installed("asreml")
  skip_on_cran()
  library(dae)
  library(asreml)
  library(asremlPlus)
  data(WaterRunoff.dat)
  current.asr <- asreml(log.Turbidity ~ Benches + (Sources * (Type + Species)) * Date, 
                        random = ~Benches:MainPlots:SubPlots:spl(xDay), 
                        data = WaterRunoff.dat, keep.order = TRUE)
  current.asrt <- asrtests(current.asr, NULL, NULL)
  terms.treat <- c("Sources", "Type", "Species", 
                   "Sources:Type", "Sources:Species")
  terms <- sapply(terms.treat, 
                  FUN=function(term){paste("Date:",term,sep="")}, 
                  simplify=TRUE)
  terms <- c("Date", terms)
  terms <- unname(terms)
  marginality <-  matrix(c(1,0,0,0,0,0, 1,1,0,0,0,0,  1,0,1,0,0,0, 
                           1,0,1,1,0,0, 1,1,1,0,1,0, 1,1,1,1,1,1), nrow=6)
  rownames(marginality) <- terms
  colnames(marginality) <- terms
  choose <- choose.model.asrtests(marginality, current.asrt, denDF="algebraic")
  current.asrt <- choose$asrtests.obj
  sig.terms <- choose$sig.terms
  testthat::expect_equal(length(sig.terms), 2)
  testthat::expect_equal(sig.terms[[1]], "Date:Species")
  testthat::expect_equal(sig.terms[[2]], "Date:Sources")
})

cat("#### Test for sig.devn.reparam.asrtests\n")
test_that("sig.devn.reparam.asrtests", {
  skip_if_not_installed("asreml")
  skip_on_cran()
  library(dae)
  library(asreml)
  library(asremlPlus)
  data(WaterRunoff.dat)
  current.asr <- asreml(fixed = log.Turbidity ~ Benches + Sources + Type + Species + 
                          Sources:Type + Sources:Species + Sources:Species:xDay + 
                          Sources:Species:Date, 
                        data = WaterRunoff.dat, keep.order = TRUE)
  current.asrt <- asrtests(current.asr, NULL, NULL)

  #Examine terms that describe just the interactions of Date and the treatment factors
  terms.treat <- c("Sources", "Type", "Species", "Sources:Type", "Sources:Species")
  date.terms <- sapply(terms.treat, 
                       FUN=function(term){paste("Date:",term,sep="")}, 
                       simplify=TRUE)
  date.terms <- c("Date", date.terms)
  date.terms <- unname(date.terms)
  treat.marginality <-  matrix(c(1,0,0,0,0,0, 1,1,0,0,0,0,  1,0,1,0,0,0, 
                                 1,0,1,1,0,0, 1,1,1,0,1,0, 1,1,1,1,1,1), nrow=6)
  rownames(treat.marginality) <- date.terms
  colnames(treat.marginality) <- date.terms
  choose <- choose.model.asrtests(treat.marginality, current.asrt, denDF="algebraic")
  current.asrt <- choose$asrtests.obj
  current.asr <- current.asrt$asreml.obj
  sig.date.terms <- choose$sig.terms

  #Remove all Date terms left in the fixed model
  terms <- "(Date/(Sources * (Type + Species)))"
  current.asrt <- addrm.terms.asrtests(terms, current.asrt)
  #if there are significant date terms, reparameterize to xDays + spl(xDays) + Date
  if (length(sig.date.terms) != 0)
  { #add lin + spl + devn for each to fixed and random models
    trend.date.terms <- sapply(sig.date.terms, 
                               FUN=function(term){sub("Date","xDay",term)}, 
                               simplify=TRUE)
    trend.date.terms <- paste(trend.date.terms,  collapse=" + ")
    current.asrt <- addrm.terms.asrtests(trend.date.terms, current.asrt, add=TRUE)
    trend.date.terms <- sapply(sig.date.terms, 
                               FUN=function(term){sub("Date","spl(xDay)",term)}, 
                               simplify=TRUE)
    trend.date.terms <- c(trend.date.terms, sig.date.terms)
    trend.date.terms <- paste(trend.date.terms,  collapse=" + ")
    current.asrt <- addrm.terms.asrtests(trend.date.terms, current.asrt, 
                                         add=TRUE, random=TRUE)
    current.asrt <- rmboundary.asrtests(current.asrt)
  }
  
  #Now test terms for sig date terms
  spl.terms <- sapply(terms.treat, 
                      FUN=function(term){paste("spl(xDay):",term,sep="")}, 
                      simplify=TRUE)
  spl.terms <- c("spl(xDay)",spl.terms)
  lin.terms <- sapply(terms.treat, 
                      FUN=function(term){paste(term,":xDay",sep="")}, 
                      simplify=TRUE)
  lin.terms <- c("xDay",lin.terms)
  systematic.terms <- c(terms.treat, lin.terms, spl.terms, date.terms)
  systematic.terms <- unname(systematic.terms)
  treat.marginality <-  matrix(c(1,0,0,0,0,0, 1,1,0,0,0,0,  1,0,1,0,0,0, 
                                 1,0,1,1,0,0, 1,1,1,1,1,0, 1,1,1,1,1,1), nrow=6)
  systematic.marginality <- kronecker(matrix(c(1,0,0,0, 1,1,0,0, 
                                               1,1,1,0, 1,1,1,1), nrow=4), 
                                      treat.marginality)
  systematic.marginality <- systematic.marginality[-1, -1]
  rownames(systematic.marginality) <- systematic.terms
  colnames(systematic.marginality) <- systematic.terms
  choose <- choose.model.asrtests(systematic.marginality, current.asrt, 
                                  denDF="algebraic", pos=TRUE)
  current.asrt <- choose$asrtests.obj

  #Check if any deviations are significant and, for those that are, go back to 
  #fixed dates
  current.asrt <- sig.devn.reparam.asrtests(choose$sig.terms, current.asrt, 
                                            trend.num = "xDay", devn.fac = "Date", 
                                            denDF = "algebraic")
  k <- match("Sources:Species:Date",rownames(current.asrt$wald.tab))
  testthat::expect_equal(nrow(current.asrt$wald.tab), 9)
  testthat::expect_equal(nrow(current.asrt$test.summary), 7)
  testthat::expect_true(!is.na(k))
})