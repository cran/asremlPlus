\name{asremlPlus-package}
\alias{asremlPlus-package}
\alias{asremlPlus}
\docType{package}
\title{Augments the use of asreml-R in fitting mixed models}
\description{Provides functions that assist in automating the testing of terms 
  in mixed models when 'asreml' is used to fit the models. The package 'asreml' 
  is marketed by 'VSNi' (http://www.vsni.co.uk/) as 'asreml-R' and provides a 
  computationally efficient algorithm for fitting mixed models using 
  Residual Maximum Likelihood. The content falls into the following natural 
  groupings: (i) Data, (ii) Object manipulation functions, (iii) Model modification 
  functions, (iv) Model testing functions, (v) Model diagnostics functions, 
  (vi) Prediction production and presentation functions, (vii) Response 
  transformation functions, and (viii) Miscellaneous functions. 
  A history of the fitting of a sequence of models is kept in a data frame. 
  Procedures are available for choosing models that conform to the hierarchy or 
  marginality principle and for displaying predictions for significant terms 
  in tables and graphs.
}
\details{
\tabular{ll}{
Package: \tab asremlPlus\cr
Version: \tab 1.10-16\cr
Date: \tab 2015-06-06\cr
Depends: \tab R (>= 2.10.0), dae, ggplot2\cr
Enhances: \tab asreml\cr
License: \tab GPL (>=2)\cr
Lazyload: \tab yes\cr
URL: \tab http://chris.brien.name\cr
}

Index:
\preformatted{
(i) Data

Wheat.dat               Data for an experiment to investigate 25 varieties of 
                        wheat.

(ii) Object manipulation

alldiffs                Forms an object of S3-class 'alldiffs' that stores the 
                        predictions for a model fitted using asreml, along with 
                        statistics for all pairwise differences.
asrtests                Forms an object of S3-class 'asrtests' that stores a 
                        fitted asreml object, a pseudo-anova table for the fixed 
                        and a history of changes and hypthesis testing used in 
                        obtaining the model.
print.alldiffs          Prints the values in an 'alldiffs' object in a nice 
                        format.
print.asrtests          Prints the values in an 'asrtests' object 

(iii) Model modification

addrm.terms.asreml      Adds or removes the specified set terms from either the 
                        fixed or random model and records the change in a 
                        data.frame.
newfit.asreml           Refits an asreml model with modified model formula 
                        using either a call to 'update.asreml' or a direct call 
                        to 'asreml'.
rmboundary.asreml       Removes any boundary or singular variance components 
                        from the fit stored in 'asreml.obj' and records their 
                        removal in a data.frame.
setvarianceterms.asreml Allows the seting of constraints and initial values for 
                        terms in the 'random' and 'rcov' arguments of an 
                        'asreml' call.
sig.devn.reparam.asreml This function reparamterizes each random (deviations) 
                        term involving 'devn.fac' to a fixed term and ensures 
                        that the same term, with 'trend.num' replacing 
                        'devn.fac', is included if any other term with 
                        'trend.num' is included in 'terms'.

(iv) Model testing

choose.model.asreml     Determines the set of significant terms taking into 
                        account hierarchy or marginality relations.
info.crit.asreml        Computes AIC and BIC for a model.
recalc.wald.tab.asreml  Recalculates the denDF, F.inc and P values for a table 
                        of Wald test statistics obtained using 'wald.asreml'.
reml.lrt.asreml         Performs REML likelihood ratio test.
testranfix.asreml       Tests for a single fixed or random term in model fitted 
                        using 'asreml'.
testrcov.asreml         Fits a new rcov formula using 'asreml' and tests whether 
                        the change is significant.
testswapran.asreml      Tests, using a REMLRT, whether the difference between 
                        current random model and one in which oldterms are 
                        dropped and newterms are added is significant.

(v) Model diagnostics

variofaces.asreml       Plot empirical variogram faces, including envelopes, as 
                        described by Stefanova, Smith & Cullis (2009)

(vi) Prediction production and presentation

pred.present.asreml     This function forms the predictions for each 
                        significant term and presents them in tables and/or 
                        graphs.
predictiondiffs.asreml  Forms all pairwise differences between a set of 
                        predictions, their standard errors and p-values for a 
                        test of whether the differences are significantly 
                        different from zero.
predictionplot.asreml   This function plots the predictions for a term, 
                        possibly with error bars.
predictparallel.asreml  This function forms the predictions and associated 
                        statistics for a term, taking into account that a 
                        numeric vector and a factor having parallel values may 
                        occur in the model. It stores the results in an object 
                        of class 'alldifffs' and prints the results. It can be 
                        used when there are not parallel values.

(vii) Response transformation

angular                 Applies the angular transformation to proportions.
angular.mod             Applies the modified angular transformation to a vector 
                        of counts.
power.transform         Perform linear and power transformations on a variable 
                        whose name is given as a character string in 'var.name'. 
                        The transformed variable is stored in the 'data.frame data'.

(viii) Miscellaneous

num.recode              Recodes the unique values of a vector using the values 
                        in a new vector.
permute.square          Permutes the rows and columns of a square matrix.
permute.to.zero.lowertri
                        Permutes a square matrix until all the lower triangular 
                        elements are zero.
}
Most of the model functions utilize an \code{asrtests} object that stores:  
  (i) the currently fitted model, 
  (ii) the table of test statistics for the fixed effects, and 
  (iii) a data frame that contains a history of the changes made to the model. 
}
\author{Chris Brien <Chris.Brien@unisa.edu.au>.}
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
current.asrt <- rmboundary.asreml(current.asrt)

#Check term for within Column pairs
current.asrt <- testranfix.asreml("WithinColPairs", current.asrt, drop.fix.ns=TRUE)

# Test nugget term
current.asrt <- testranfix.asreml("units", current.asrt, positive=TRUE)

# Test Row autocorrelation
current.asrt <- testrcov.asreml("~ Row:ar1(Column)", current.asrt, 
                                label="Row autocorrelation", simpler=TRUE)

# Test Col autocorrelation (depends on whether Row autocorrelation retained)
k <- match("Row autocorrelation", current.asrt$test.summary$terms)
p <- current.asrt$test.summary$p
{if (p[k] <= 0.05)
  current.asrt <- testrcov.asreml("~ ar1(Row):Column", current.asrt, 
                                  label="Col autocorrelation", simpler=TRUE,
                                  update=FALSE)
 else
   current.asrt <- testrcov.asreml("~ Row:Column", current.asrt, 
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
