\name{addrm.terms.asrtests}
\alias{addrm.terms.asrtests}
\title{Adds or removes the specified set terms from either the fixed or random 
       model and records the change in a data.frame.}
\description{The specified terms are simply added or removed from either the 
       fixed or random model. No hypothesis testing is performed and no check 
       is made for boundary or singular terms. A row is added to the 
       \code{test.summary} \code{data.frame} stating whether fixed or random terms 
       have been added or removed. Convergence in fitting the model is checked 
       and a note included in the action if there was not. All components of the 
       \code{\link{asrtests}} object are updated, although \code{wald.tab} is only updated 
       if it is present in the supplied \code{\link{asrtests}} object.}
\usage{
addrm.terms.asrtests(terms = NULL, asrtests.obj, add = FALSE, 
                     random = FALSE, label = NULL,
                     denDF = "default", trace = FALSE, 
                     update = TRUE, set.terms = NULL, ignore.suffices = TRUE, 
                     constraints = "P", initial.values = NA, ...)}
\arguments{
 \item{terms}{a single character string in the form of a formula which, after expansion, 
               specifies the sum of a set of terms to be added or dropped.}
 \item{asrtests.obj}{an \code{\link{asrtests}} object for a fitted model that is a list 
                containing an \code{asreml} object, a \code{wald.tab} 
                \code{data.frame} with 4 columns, and a \code{data.frame} with 
                5 columns that records any previous changes and tests in the 
                fitted model.}
 \item{add}{whether to add or remove terms from the model.}
 \item{random}{whether terms are to added or removed from the fixed or random model.}
 \item{label}{a character string to use as the label in \code{test.summary} and 
              which indicates what is being tested. If \code{label} is \code{NULL}, 
              either \code{Fixed terms} or \code{Random terms} is used, depending on 
              whether \code{random} is \code{TRUE} or \code{FALSE}.}
 \item{denDF}{Specifies the enthod to use in computing approximate denominator 
              degrees of freedom when \code{wald.asreml} is called. Can be \code{none} 
              to suppress the computations, \code{numeric} for numerical methods,
              \code{algebraic} for algebraic methods or \code{default}, the default, 
              to autommatically choose numeric or algebraic computations depending 
              on problem size. The denominator degrees of freedom are calculated 
              according to Kenward and Roger (1997) for fixed terms in the dense 
              part of the model.}
\item{trace}{if TRUE then partial iteration details are displayed when ASReml-R 
              functions are invoked; if FALSE then no output is displayed.}
\item{update}{if \code{TRUE }then \code{update.asreml} is called in removing and 
              adding terms to the model. In doing this the arguments \code{R.param} 
              and \code{G.param} are set to those in the \code{asreml} object stored 
              in the supplied \code{asrtests.obj} so that the values from the previous 
              model are used as starting values. If \code{FALSE} then calls are made to 
              \code{asreml} in which the only changes from the previous call are 
              (i) that the \code{random} model is updated and (ii) modifications 
              specified via \code{...} are made.}
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
               of the model after all boundary and singular terms have been removed;
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
}

\seealso{\code{\link{asrtests}}, \code{\link{rmboundary.asrtests}}, 
         \code{\link{testranfix.asrtests}}, \code{\link{testrcov.asrtests}}, \cr
         \code{\link{newfit.asreml}}, \code{\link{sig.devn.reparam.asrtests}}, 
         \code{\link{choose.model.asrtests}}}
\examples{\dontrun{
  terms <- "(Date/(Sources * (Type + Species)))"
  current.asrt <- addrm.terms.asrtests(terms, current.asrt, add = TRUE)

  current.asrt <- addrm.terms.asrtests("A + B", current.asrt, denDF = "algebraic")
}}
\keyword{asreml}
