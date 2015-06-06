\name{predictiondiffs.asreml}
\alias{predictiondiffs.asreml}
\title{Uses information in a supplied alldiffs object to forms all pairwise differences 
       between a set of predictions, the p-values for a test of whether the differences 
       are significantly different from zero, and the minimum, mean and maximum LSD 
       values, provided they are not present in the suppplied alldiffs object.}
\description{Uses predictions and standard errors of pairwise differences from an 
  \code{\link{alldiffs}} object to form, for those components not already present, 
  (i) a table of all pairwise differences of the predictions in an \code{\link{alldiffs}} 
  object, (ii) the p-values of each pairwise difference, and (iii) the minimum, mean and 
  maximum LSD values. Predictions that are aliased (or nonestimable) are removed 
  from the \code{predictions} component of the \code{\link{alldiffs}} object and 
  standarard errors of differences involving them are removed from the \code{sed} component. 

  Each p-value is computed as the probability of a t-statistic as large as or larger than 
  the absolute value of the observed difference divided by its standard error. The 
  p-values are stored in the \code{p.differences} component. The degrees of freedom of 
  the t-distribution is the degrees of freedom stored in the \code{tdf} attribute of the 
  \code{\link{alldiffs}} object. This t-distibrution is also used in calculating the 
  LSD statistics stored in the \code{\link{alldiffs}} object.}
\usage{predictiondiffs.asreml(classify, alldiffs.obj, 
                       x.num = NULL, x.fac = NULL,  
                       levels.length = NA, 
                       pairwise = TRUE, alpha = 0.05,
                       inestimable.rm = TRUE)}
\arguments{
 \item{classify}{a character string giving the variables that define the margins 
                 of the multiway table to be predicted. Multiway tables are 
                 specified by forming an interaction type term from the 
                 classifying variables, that is, separating the variable names 
                 with the \code{:} operator.}
 \item{alldiffs.obj}{An \code{\link{alldiffs}} object for a fitted model. Note that the 
               attribute \code{tdf}, being the degrees of freedom for the critical 
               t-value to be used incomputing p-values, should be set to an appropriate value.}
 \item{x.num}{A character string giving the name of the numeric covariate that 
              corresponds to \code{x.fac}, is potentially included in terms in the 
              fitted model and which corresponds to the x-axis variable. It should 
              have the same number of unique values as the number of levels in 
              \code{x.fac}.}
 \item{x.fac}{A character string giving the name of the factor that corresponds to 
              \code{x.num}, is potentially included in terms in the fitted model and 
              which corresponds to the x-axis variable. It should have the same 
              number of levels as the number of unique values in \code{x.num}. 
              The levels of \code{x.fac} must be in the order in which they are to 
              be plotted - if they are dates, then they should be in the form 
              yyyymmdd which can be achieved using \code{as.Date}.}
 \item{levels.length}{The maximum number of characters from the the levels of 
            factors to use in the row and column labels of the tables of 
            pairwise differences and their p-values and standard errors.}
 \item{pairwise}{A logical indicating whether all pairwise differences of the
                \code{predictions} and their standard errors and p-values are to be 
                computed and stored. If \code{FALSE}, the components \code{differences} 
                and \code{p.differences} will be \code{NULL} in the returned 
                \code{\link{alldiffs}} object.}
 \item{alpha}{The significance level for an LSD to compare a pair of predictions.}
 \item{inestimable.rm}{A \code{logical} indicating whether rows for  predictions that 
                       are not estimable are to be removed from the components of the
                       \code{\link{alldiffs}} object.}
}
\value{An \code{\link{alldiffs}} object that is a list with components 
       \code{predictions} containing the predictions and 
       their standard errors, \code{differences} containing all pairwise 
       differences between the predictions, \code{p.differences} containing 
       p-values for all pairwise differences between the predictions, 
       \code{sed} containing the standard errors of all pairwise differences 
       between the predictions, and,an \code{LSD} containing the mean, minimum 
       and maximum LSDs.}


\seealso{\code{\link{asremlPlus-package}}, \code{\link{alldiffs}}, 
         \code{\link{print.alldiffs}}, \code{\link{predictionplot.asreml}}, \cr
         \code{\link{predictparallel.asreml}}, \code{\link{pred.present.asreml}}}
\examples{\dontrun{
  Var.pred <- predict(current.asr, classify="Variety", sed=TRUE)$predictions
  wald.tab <-  current.asrt$wald.tab
  den.df <- wald.tab[match("Variety", rownames(wald.tab)), "denDF"]
  Var.diffs <- alldiffs(predictions = Var.pred$pvals, 
                        sed = Var.pred$sed, 
                        tdf = den.df)
  Var.diffs <- predictiondiffs.asreml(classify = "Variety", 
                                      alldiffs.obj=Var.diffs)
  print.alldiffs(Var.diffs, which="differences")
}}
\keyword{asreml}