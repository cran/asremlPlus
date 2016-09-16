\name{testrcov.asrtests}
\alias{testrcov.asrtests}
\title{Fits a new rcov formula using \code{asreml} and tests whether the change 
       is significant.}
\description{Fits a new rcov formula using \code{asreml} and tests whether the 
       change is significant. If \code{simpler = FALSE} the model to be fitted 
       must be more complex than the one whose fit has been stored in 
       \code{asrtests.obj}. That is, the new model must have more parameters. 
       However, if \code{simpler = TRUE} the model to be fitted must be simpler 
       than the one whose fit has been stored in \code{asrtests.obj} in that it 
       must have fewer parameters. Any boundary terms are removed using 
       \code{\link{rmboundary.asrtests}}, which may mean that the models are not 
       nested. The test is a REML likelihood ratio test that is performed using 
       \code{\link{reml.lrt.asreml}}, which is only valid if the models are nested. 
       It compares the newly fitted model with the fit of the model in 
       \code{asrtest.obj}. A row is added to the \code{test.summary} 
       \code{data.frame} using the supplied \code{label}.}
\usage{
testrcov.asrtests(terms=NULL, asrtests.obj, label = "R model", 
                  simpler = FALSE, alpha = 0.05, 
                  positive.zero = FALSE, bound.test.parameters = "none", 
                  denDF="default", update = TRUE, trace = FALSE,
                  set.terms = NULL, ignore.suffices = TRUE, 
                  constraints = "P", initial.values = NA, ...)}
\arguments{
 \item{terms}{a model for the \code{rcov} argument in \code{asreml}, stored 
             as a \code{character}.}
 \item{asrtests.obj}{an \code{\link{asrtests}} object for a fitted model that is a list 
                containing an \code{asreml} object, a \code{wald.tab} 
                \code{data.frame} with 4 columns, and a \code{data.frame} with 
                5 columns that records any previous changes and tests in the 
                fitted model.}
 \item{label}{a character string to use as the label in \code{test.summary} and 
              which indicates what is being tested.}
 \item{simpler}{a logical indicating whether the new model to be fitted is 
                simpler than the already fitted model whose fit is stored in 
                \code{asrtests.obj}.}
 \item{alpha}{the significance level for the test.}
 \item{positive.zero}{Indicates whether the hypothesized values for the 
                      variance components being tested are on the boundary 
                      of the parameter space. For example, this is true 
                      for positively-constrained variance components that, 
                      under the reduced model, are zero. This argument does 
                      not need to be set if \code{bound.test.parameters} is set.}
 \item{bound.test.parameters}{Indicates whether for the variance components 
                      being tested, at least some of the hypothesized values
                      are on the boundary of the parameter space. The default 
                      is \code{"none"}. Other possibilities are 
                      \code{"onlybound"} and \code{"one-and-one"}. The latter 
                      signifies that there are two parameters being tested, 
                      one of which is bound and the other is not. For example, 
                      the latter is true for testing a covariance and 
                      a positively-constrained variance component that, 
                      under the reduced model, are zero.}
 \item{denDF}{Specifies the method to use in computing approximate denominator 
              degrees of freedom when \code{wald.asreml} is called. Can be \code{none} 
              to suppress the computations, \code{numeric} for numerical methods,
              \code{algebraic} for algebraic methods or \code{default}, the default, 
              to autommatically choose numeric or algebraic computations depending 
              on problem size. The denominator degrees of freedom are calculated 
              according to Kenward and Roger (1997) for fixed terms in the dense 
              part of the model.}
 \item{update}{if \code{TRUE }then \code{update.asreml} is called to fit the model with 
              the \code{rcov} model supplied in \code{terms}. In doing this the arguments 
              \code{R.param} and \code{G.param} are set to those in the \code{asreml} 
              object stored in \code{asrtests.obj} so that the values from the previous 
              model are used as starting values. If \code{FALSE} then a call is made to 
              \code{asreml} in which the only changes from the previous call are that 
              (i) \code{rcov} model is that specified in \code{terms} and 
              (ii) modifications specified via \code{...} are made.}
 \item{trace}{if TRUE then partial iteration details are displayed when ASReml-R 
              functions are invoked; if FALSE then no output is displayed.}
 \item{set.terms}{a character vector specifying the terms that are to have 
                  constraints and/or initial values set prior to fitting.}
 \item{ignore.suffices}{a logical vector specifying whether the suffices of the 
                    \code{asreml}-assigned names of the variance terms (i.e. the 
                    information to the right of an "!", other than "R!") is to 
                    be ignored in matching elements of \code{terms}. 
                    If \code{TRUE} for an element of \code{terms}, the suffices 
                    are stripped from the \code{asreml}-assigned names. 
                    If \code{FALSE} for an element of \code{terms}, the element 
                    must exactly match an \code{asreml}-assigned name for a 
                    variance term. This vector must be of length one or the 
                    same length as \code{terms}. If it is of length one then 
                    the same action is applied to the \code{asreml}-assigned 
                    suffices for all the terms in \code{terms}.}
 \item{constraints}{a character vector specifying the constraints to be applied 
                    to the terms specified in \code{terms}. This vector 
                    must be of length one or the same length as \code{terms}. 
                    If it is of length one then the same constraint is 
                    applied to all the terms in \code{terms}. 
                    If any of the constraints are equal to NA then they are 
                    left unchanged for those terms.}
 \item{initial.values}{a character vector specifying the initial values for  
                    the terms specified in \code{terms}. This vector 
                    must be of length one or the same length as \code{terms}. 
                    If it is of length one then the same initial value is 
                    applied to all the terms in \code{terms}. 
                    If any of the initial.values are equal to NA then they are 
                    left unchanged for those terms.}
 \item{...}{further arguments passed to \code{asreml} and to \code{wald.asreml}.}
}
\value{An \code{\link{asrtests}} object, which is a list containing:
       \enumerate{
         \item \code{asreml.obj}: an \code{asreml} object containing the fit 
               after the \code{term} has been omitted from the model;
         \item \code{wald.tab}: a 4-column \code{data.frame} containing a 
               pseudo-anova table for the fixed terms produced by \code{wald.asreml};
         \item \code{test.summary}: a \code{data.frame} with columns \code{term}, 
                \code{DF}, \code{denDF}, \code{p} and \code{action}. A row is added to 
                it for each term 
                that is dropped, added or tested or a note that several terms have been
                added or removed. A row contains the name of the term, the 
                DF, the p-value and the action taken. Possible codes are: 
                \code{Dropped}, \code{Retained}, \code{Swapped}, \code{Unswapped},   
                \code{Significant}, \code{Nonsignificant}, \code{Absent}, \code{Added}, 
                \code{Removed} and \code{Boundary}. If the changed model did not
                 converge, \code{Unconverged} will be added to the code. 
                 Note that the logical \code{asreml.obj$converge} also
                reflects whether there is convergence.}
       If the \code{term} is not in the model, then the supplied \code{asreml} 
       object will be returned. Also, \code{reml.test} will have the likelihood 
       ratio and the p-value set to \code{NA} and the degrees of freedom to zero.
       Similarly, the row of \code{test.summary} for the \code{term} will have 
       its name, a p-value set to \code{NA}, and action set to Absent.
}

\seealso{\code{\link{asremlPlus-package}}, \code{\link{asrtests}}, \code{\link{newrcov.asrtests}}, 

         \code{\link{choose.model.asrtests}}, 
         \code{\link{reml.lrt.asreml}}, \code{\link{rmboundary.asrtests}}, \cr
         \code{\link{newfit.asreml}}, \code{\link{testswapran.asrtests}}, 
         \code{\link{addrm.terms.asrtests}}, \cr
         \code{\link{sig.devn.reparam.asrtests}}}
\examples{\dontrun{
data(Wheat.dat)
current.asr <- asreml(yield ~ Rep + WithinColPairs + Variety, 
                      random = ~ Row + Column + units,
                      rcov = ~ ar1(Row):ar1(Column), 
                      data=Wheat.dat)
current.asrt <- asrtests(current.asr, NULL, NULL)
current.asrt <- rmboundary.asrtests(current.asrt)
# Test Row autocorrelation
current.asrt <- testrcov.asrtests("~ Row:ar1(Column)", current.asrt, 
                                  label="Row autocorrelation", simpler=TRUE)
print(current.asrt)
}}
\keyword{asreml}
\keyword{htest}