\name{testranfix.asrtests}
\alias{testranfix.asrtests}
\title{Tests for a single fixed or random term in model fitted using \code{asreml}}
\description{Tests for a single term, using a REML LRT for a random term or based 
  on Wald statistics for a fixed term. The term must be in the fitted model. 
  A random term is removed from the model fit and a REML likelihood ratio test is 
  performed using \code{\link{reml.lrt.asreml}}. It compares the fit of the model in 
  \code{asreml.obj} and the newly fitted model without the \code{term}. 
  If the newly fitted model is retained, any boundary terms are then removed
  using \code{\link{rmboundary.asrtests}}. 
  For a fixed term, the probability of the Wald 
  statistics is extracted from the pseudo-anova table produced by 
  \code{wald.asreml}. If this is available in the \code{\link{asrtests}} object, it is 
  used; otherwise \code{wald.asreml} is called to add it to the \code{\link{asrtests}} 
  object. Whether nonsignificant terms are dropped is controlled by \code{drop.ran.ns}  
  for random terms and \code{drop.fix.ns} for fixed terms. A row is 
  added to the \code{test.summary} \code{data.frame} for the term that is tested.}
\usage{testranfix.asrtests(term=NULL, asrtests.obj, alpha = 0.05, 
                    drop.ran.ns = TRUE, positive.zero = FALSE, 
                    drop.fix.ns = FALSE, denDF="default", dDF.na = "none", 
                    dDF.values = NULL, trace = FALSE, update = TRUE,
                    set.terms = NULL, ignore.suffices = TRUE, 
                    constraints = "P", initial.values = NA, ...)}
\arguments{
 \item{term}{a single model term that is valid in \code{asreml}, stored 
             as a \code{character}.}
 \item{asrtests.obj}{an \code{\link{asrtests}} object for a fitted model that is a list 
                containing an \code{asreml} object, a \code{wald.tab} 
                \code{data.frame} with 4 columns, and a \code{data.frame} with 
                5 columns that records any previous changes and tests in the 
                fitted model.}
 \item{alpha}{the significance level for the test.}
 \item{drop.ran.ns}{a logical indicating whether to drop a random 
                   term from the model when it is nonsignificant .}
 \item{positive.zero}{a logical indicating whether the hypothesized values for the 
                      varaince components being tested are on the boundary 
                      of the parameter space. For example, this is be true 
                      for positively-constrained variance components that, 
                      under the reduced model, are zero.}
 \item{drop.fix.ns}{a logical indicating whether to drop a fixed  
                   term from the model when it is nonsignificant }
 \item{denDF}{Specifies the enthod to use in computing approximate denominator 
              degrees of freedom when \code{wald.asreml} is called. Can be \code{none} 
              to suppress the computations, \code{numeric} for numerical methods,
              \code{algebraic} for algebraic methods or \code{default}, the default, 
              to autommatically choose numeric or algebraic computations depending 
              on problem size. The denominator degrees of freedom are calculated 
              according to Kenward and Roger (1997) for fixed terms in the dense 
              part of the model.}
 \item{dDF.na}{the method to use to obtain substitute denominator degrees of freedom. 
              when the numeric or algebraic methods produce an \code{NA}. If 
              \code{dDF.na = "none"}, no subtitute denominator degrees of freedom 
              are employed; if \code{dDF.na = "residual"}, 
              the residual degrees of freedom from \code{asreml.obj$nedf} are used; 
              if \code{dDF.na = "maximum"}, the maximum of those denDF that are available, 
              excluding that for the Intercept, is used; if all denDF are \code{NA}, 
              \code{asreml.obj$nedf} is used. If \code{dDF.na = "supplied"}, 
              a \code{vector} of values for the denominator degrees of freedom is to 
              be supplied in \code{dDF.values}. Any other setting is ignored and 
              a warning message produced. Generally, substituting these degrees of freedom is 
              anticonservative in that it is likely that the degrees of freedom used will be
              too large.}
 \item{dDF.values}{A \code{vector} of values to be used when \code{dDF.na = "supplied"}. 
              Its values will be used when \code{denDF} in a test for a fixed effect
              is \code{NA}. This vector must be the same length as the number of 
              fixed terms, including (Intercept) whose value could be \code{NA}.}
 \item{trace}{if TRUE then partial iteration details are displayed when ASReml-R 
              functions are invoked; if FALSE then no output is displayed.}
 \item{update}{if \code{TRUE} then \code{update.asreml} is called to fit the model 
               to be tested. In doing this the arguments \code{R.param} and 
              \code{G.param} are set to those in the \code{asreml} 
              object stored in \code{asrtests.obj} so that the values from the previous 
              model are used as starting values. If \code{FALSE} then a call is made to 
              \code{asreml} in which the only changes from the previous call are that 
              (i) models are modifed for the supplied \code{terms} and 
              (ii) modifications specified via \code{...} are made.}
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
               after the \code{term} has been tested; it will be a new model if 
               the \code{term} is nonsignificant and the appropriate argument 
               out of \code{drop.ran.ns}  and \code{drop.fix.ns} is 
               \code{TRUE};
         \item \code{wald.tab}: a 4-column \code{data.frame} containing a 
               pseudo-anova table for the fixed terms produced by \code{wald.asreml};
         \item \code{test.summary}: a \code{data.frame} with columns \code{term}, 
               \code{DF}, \code{denDF}, \code{p} and \code{action}. A row is added to 
                it for each term that is tested, the row containing the name of the term, 
                the degrees of freedom (numerator DF for a Wald test and the number of 
                extra paramters for a REML ratio tests), the p-value and a for the 
                action taken. Possible codes are: 
                \code{Dropped}, \code{Retained}, \code{Significant}, 
                \code{Nonsignificant}, \code{Absent}, \code{Added}, 
                \code{Removed} and \code{Boundary}. If the changed model did not 
                 converge, \code{Unconverged} will be added to the code.
                 Note that the logical \code{asreml.obj$converge} also
                reflects whether there is convergence.}
       If the \code{term} is not in the model, then the supplied \code{asreml} 
       object will be returned. Also, \code{reml.test} will have the likelihood 
       ratio and the p-value set to \code{NA} and the degrees of freedom to zero.
       Similarly, the row of \code{test.summary} for the \code{term} will have 
       its name, DF set to NA, p-value set to \code{NA}, and action set to Absent.
}

\seealso{\code{\link{asremlPlus-package}}, \code{\link{asrtests}}, 
         \code{\link{choose.model.asrtests}}, \code{\link{reml.lrt.asreml}}, 
         \code{\link{rmboundary.asrtests}}, \cr
         \code{\link{newfit.asreml}}, \code{\link{sig.devn.reparam.asrtests}}, 
         \code{\link{addrm.terms.asrtests}}}
\examples{\dontrun{
data(Wheat.dat)
current.asr <- asreml(yield ~ Rep + WithinColPairs + Variety, 
                      random = ~ Row + Column + units,
                      rcov = ~ ar1(Row):ar1(Column), 
                      data=Wheat.dat)
current.asrt <- asrtests(current.asr, NULL, NULL)
current.asrt <- rmboundary.asrtests(current.asrt)
# Test nugget term
current.asrt <- testranfix.asrtests("units", current.asrt, positive=TRUE)
}}
\keyword{asreml}
\keyword{htest}