\newcommand{\packageVer}{\Sexpr[results=rd,stage=build]{utils::packageDescription("#1", fields="Version")}}
\newcommand{\packageDate}{\Sexpr[results=rd,stage=build]{utils::packageDescription("#1", fields="Date")}}
\name{asremlPlus-package}
\alias{asremlPlus-package}
\alias{asremlPlus}
\docType{package}
\title{
\packageTitle{asremlPlus}
}
\description{\packageDescription{asremlPlus}

\bold{Version:} {\packageVer{asremlPlus}}

\bold{Date:} \packageDate{asremlPlus}
}

\section{Index}{
\tabular{ll}{
(i) Data \cr\cr

\code{\link{Wheat.dat}} 
      \tab  Data for an experiment to investigate 25 varieties of \cr
      \tab  wheat. \cr
\code{\link{WaterRunoff.dat}}
      \tab Data for an experiment to investigate the quality of \cr
      \tab water runoff over time\cr
\cr
(ii) Object manipulation \cr\cr

\code{\link{alldiffs}}
      \tab Forms an object of S3-class 'alldiffs' that stores the \cr
      \tab predictions for a model fitted using asreml, along \cr
      \tab with statistics for all pairwise differences.\cr
\code{\link{asrtests}} 
      \tab Forms an object of S3-class 'asrtests' that stores a \cr
      \tab fitted asreml object, a pseudo-anova table for the \cr
      \tab fixed and a history of changes and hypthesis testing \cr
      \tab used in obtaining the model. \cr
\code{\link{print.alldiffs}}
      \tab Prints the values in an 'alldiffs' object in a nice format. \cr
                          
\code{\link{print.asrtests}}
      \tab Prints the values in an 'asrtests' object. \cr
\cr
(iii) Model modification \cr\cr

\code{\link{addrm.terms.asrtests}}
      \tab  Adds or removes the specified set terms from either \cr
      \tab  the fixed or random model and records the change in a \cr
      \tab  data.frame.\cr
\code{\link{newfit.asreml}} 
      \tab  Refits an asreml model with modified model formula \cr
      \tab  using either a call to 'update.asreml' or a direct \cr
      \tab  call to 'asreml'. \cr
\code{\link{newrcov.asrtests}}  
      \tab  Fits a new rcov formula using asreml. \cr
\code{\link{rmboundary.asrtests}} 
       \tab Removes any boundary or singular variance components \cr
       \tab from the fit stored in 'asreml.obj' and records their \cr
       \tab removal in a data.frame. \cr
\code{\link{setvarianceterms.asreml}} 
       \tab Allows the seting of constraints and initial values \cr
       \tab for terms in the 'random' and 'rcov' arguments of an \cr
       \tab 'asreml' call. \cr
\code{\link{sig.devn.reparam.asrtests}}
       \tab This function reparamterizes each random (deviations) \cr
       \tab term involving 'devn.fac' to a fixed term and ensures \cr
       \tab that the same term, with 'trend.num' replacing \cr
       \tab 'devn.fac', is included if any other term with \cr
       \tab 'trend.num' is included in 'terms'. \cr
\cr
(iv) Model testing \cr\cr

\code{\link{choose.model.asrtests}}
       \tab Determines the set of significant terms taking into \cr
       \tab account hierarchy or marginality relations. \cr
\code{\link{info.crit.asreml}}
       \tab        Computes AIC and BIC for a model. \cr
\code{\link{recalc.wald.tab.asrtests}}
       \tab Recalculates the denDF, F.inc and P values for a table \cr
       \tab of Wald test statistics obtained using 'wald.asreml'. \cr
\code{\link{reml.lrt.asreml}} 
       \tab Performs REML likelihood ratio test. \cr
\code{\link{testranfix.asrtests}}
       \tab Tests for a single fixed or random term in model \cr
       \tab fitted using 'asreml'. \cr
\code{\link{testrcov.asrtests}}
       \tab Fits a new rcov formula using 'asreml' and tests \cr
       \tab whether the change is significant. \cr
\code{\link{testswapran.asrtests}}
       \tab Tests, using a REMLRT, whether the difference between \cr
       \tab current random model and one in which oldterms are \cr
       \tab dropped and newterms are added is significant. \cr
\cr
(v) Model diagnostics \cr\cr

\code{\link{variofaces.asreml}}
       \tab Plot empirical variogram faces, including envelopes, \cr
       \tab as described by Stefanova, Smith & Cullis (2009). \cr
\cr
(vi) Prediction production and presentation \cr\cr

\code{\link{pred.present.asreml}}
       \tab This function forms the predictions for each \cr
       \tab significant term and presents them in tables and/or \cr
       \tab graphs. \cr
\code{\link{predictiondiffs.asreml}}
       \tab Forms all pairwise differences between a set of \cr
       \tab predictions, their standard errors and p-values for a \cr
       \tab test of whether the differences are significantly \cr
       \tab different from zero. \cr
\code{\link{predictionplot.asreml}}
       \tab This function plots the predictions for a term, \cr
       \tab possibly with error bars. \cr
\code{\link{predictparallel.asreml}}
       \tab This function forms the predictions and associated \cr
       \tab statistics for a term, taking into account that a \cr
       \tab numeric vector and a factor having parallel values may \cr
       \tab occur in the model. It stores the results in an object \cr
       \tab of class 'alldifffs' and prints the results. It can be \cr
       \tab used when there are not parallel values. \cr
\cr
(vii) Response transformation \cr\cr

\code{\link{angular}}
       \tab Applies the angular transformation to proportions. \cr
\code{\link{angular.mod}}
       \tab Applies the modified angular transformation to a \cr
       \tab vector of counts. \cr
\code{\link{power.transform}}
       \tab Perform linear and power transformations on a variable \cr
       \tab whose name is given as a character string in \cr
       \tab 'var.name'. The transformed variable is stored in the \cr
       \tab 'data.frame data'. \cr
\cr
(viii) Miscellaneous \cr\cr

\code{\link{num.recode}}
       \tab Recodes the unique values of a vector using the values  \cr
       \tab in a new vector. \cr
\code{\link{permute.square}}  
       \tab          Permutes the rows and columns of a square matrix. \cr
\code{\link{permute.to.zero.lowertri}}
       \tab Permutes a square matrix until all the lower \cr
       \tab triangular elements are zero. \cr
}
The functions whose names end in 'asrtests', which are most of the model functions, utilize an \code{asrtests} object that stores:  
  (i) the currently fitted model in \code{asreml.obj}, 
  (ii) the table of test statistics for the fixed effects in \code{wald.tab}, and 
  (iii) a data frame that contains a history of the changes made to the model in \code{test.summary}. 
}
\author{
\packageAuthor{asremlPlus}

Maintainer: \packageMaintainer{asremlPlus}
}
\references{Butler, D. G., et al. (2010). \emph{Analysis of Mixed Models 
            for {S} language environments: {ASReml-R} reference manual.}  
            Brisbane, DPI Publications.
            
            Stefanova, K. T., Smith, A. B. & Cullis, B. R. (2009) Enhanced diagnostics for the 
            spatial analysis of field trials. \emph{Journal of Agricultural, Biological, 
            and Environmental Statistics}, \bold{14}, 392--410.}
\keyword{ package }
\seealso{\code{asreml}}
\examples{\dontrun{
## Analyse wheat dat using asreml and asremlPlus
## Set up for analysis
library(dae)
library(asreml)
## use ?Wheat.dat for data set details
data(Wheat.dat)

# Fit initial model
current.asr <- asreml(yield ~ Rep + WithinColPairs + Variety, 
                      random = ~ Row + Column + units,
                      rcov = ~ ar1(Row):ar1(Column), 
                      data=Wheat.dat)
summary(current.asr)

# Load current fit into an asrtests object
current.asrt <- asrtests(current.asr, NULL, NULL)

# Check for and remove any boundary terms
current.asrt <- rmboundary.asrtests(current.asrt)

#Check term for within Column pairs
current.asrt <- testranfix.asrtests("WithinColPairs", current.asrt, drop.fix.ns=TRUE)

# Test nugget term
current.asrt <- testranfix.asrtests("units", current.asrt, positive=TRUE)

# Test Row autocorrelation
current.asrt <- testrcov.asrtests("~ Row:ar1(Column)", current.asrt, 
                                  label="Row autocorrelation", simpler=TRUE)

# Test Col autocorrelation (depends on whether Row autocorrelation retained)
k <- match("Row autocorrelation", current.asrt$test.summary$terms)
p <- current.asrt$test.summary$p
{if (p[k] <= 0.05)
  current.asrt <- testrcov.asrtests("~ ar1(Row):Column", current.asrt, 
                                    label="Col autocorrelation", simpler=TRUE,
                                    update=FALSE)
 else
   current.asrt <- testrcov.asrtests("~ Row:Column", current.asrt, 
                                     label="Col autocorrelation", simpler=TRUE,
                                     update=FALSE)
}
print(current.asrt)

# Get current fitted asreml object
current.asr <- current.asrt$asreml.obj
current.asr <- update(current.asr, aom=TRUE)


# Do residuals-versus-fitted values plot
plot(fitted.values(current.asr),residuals(current.asr))

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
V <- s2*V

#Produce variogram and variogram faces plot (Stefanaova et al, 2009)
plot.asrVariogram(variogram(current.asr))
variofaces.asreml(current.asr, V=V)

#Get Variety predictionsand all pairwise prediction differences and p-values
Var.diffs <- predictparallel.asreml(classify = "Variety", 
                                    asreml.obj=current.asr,
                                    error.intervals="halfLeast",
                                    wald.tab=current.asrt$wald.tab,
                                    tables = "predictions")
print(Var.diffs, which = c("differences", "p.differences"))
}}
