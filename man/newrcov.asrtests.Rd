\name{newrcov.asrtests}
\alias{newrcov.asrtests}
\title{Fits a new rcov formula using \code{asreml}.}
\description{Fits a new rcov formula using \code{asreml}. Any boundary terms resulting 
       after fitting the new \code{rcov} model are removed using  
       \code{\link{rmboundary.asrtests}}. A row is added to the \code{test.summary} 
       \code{data.frame} using the supplied \code{label} and the \code{action} 
       \code{Set}.}
\usage{
newrcov.asrtests(terms=NULL, asrtests.obj, label = "R model", 
                 update = TRUE, trace = FALSE, denDF="default", 
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
 \item{denDF}{Specifies the method to use in computing approximate denominator 
              degrees of freedom when \code{wald.asreml} is called. Can be \code{none} 
              to suppress the computations, \code{numeric} for numerical methods,
              \code{algebraic} for algebraic methods or \code{default}, the default, 
              to autommatically choose numeric or algebraic computations depending 
              on problem size. The denominator degrees of freedom are calculated 
              according to Kenward and Roger (1997) for fixed terms in the dense 
              part of the model.}
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

\seealso{\code{\link{asremlPlus-package}}, \code{\link{asrtests}}, \code{\link{testrcov.asrtests}}, 

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
# Fit Row autocorrelation
current.asrt <- newrcov.asrtests("~ Row:ar1(Column)", current.asrt, 
                                 label="Row autocorrelation")
print(current.asrt)
}}
\keyword{asreml}
\keyword{htest}