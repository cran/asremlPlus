\name{choose.model.asrtests}
\alias{choose.model.asrtests}
\title{Determines the set of significant terms taking into account the hierarchy or marginality relations.}
\description{Performs a series of hypothesis tests taking into account the 
  marginality of terms. In particular, a term will not be tested if it is 
  marginal to (or nested in) one that is significant. For example, if A:B is significant, then 
  neither A nor B will be tested. For a random term, the term is removed from 
  the model fit, any boundary terms are removed using \code{\link{rmboundary.asrtests}} 
  and a REML likelihood ratio test is performed using \code{\link{reml.lrt.asreml}}.  
  If it is not significant and \code{drop.ran.ns} is TRUE, the term is permanently removed 
  from the model. Note that if boundary terms are removed, the reduced model may not 
  be nested in the full model in which case the test is not valid. For fixed terms, 
  the Wald tests are performed and the p-value for the term obtained. If it is not 
  significant and \code{drop.fix.ns} is TRUE, the term is permanently removed 
  from the model. A row is added to 
  \code{test.summary} for each term that is tested. }
\usage{choose.model.asrtests(terms.marginality=NULL, asrtests.obj, 
                      alpha = 0.05, drop.ran.ns=TRUE, positive.zero = FALSE, 
                      drop.fix.ns=FALSE, denDF = "default",  dDF.na = "none", 
                      dDF.values = NULL, trace = FALSE, update = TRUE, 
                      set.terms = NULL, ignore.suffices = TRUE, 
                      constraints = "P", initial.values = NA, ...)
}
\arguments{
 \item{terms.marginality}{a square matrix of ones and zeros with row and column names 
  being the names of the terms. The diagonal elements should be one, indicating 
  that a term is marginal to itself. Elements should be one if the row term is 
  marginal to the column term. All other elements should be zero.}
 \item{asrtests.obj}{an \code{\link{asrtests}} object for a fitted model that is a list 
                containing an \code{asreml} object, a \code{wald.tab} 
                \code{data.frame} with 4 columns, and a \code{data.frame} with 
                5 columns that records any previous changes and tests in the 
                fitted model.}
 \item{alpha}{the significance level for the test.}
 \item{drop.ran.ns}{a logical indicating whether to drop nonsignificant random 
                    terms from the model.}
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
\item{update}{if \code{TRUE }then \code{update.asreml} is called in testing models. 
              In doing this the arguments \code{R.param} and \code{G.param} are 
              set to those in the \code{asreml} object stored in \code{asrtests.obj} 
              so that the values from the previous model are used as starting values. 
              If \code{FALSE} then a call is made to \code{asreml} in which the 
              only changes to the \code{asreml.obj} stored in the supplied 
              \code{asrtests.obj} are (i) to the terms in the fixed and random models 
              corresponding to terms in \code{terms.marginality} and (ii) those 
              modifications specified via \code{...}.}
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
 \item{...}{further arguments passed to \code{asreml} and \code{wald.asreml} via 
               \code{\link{testranfix.asrtests}}.}
}
\value{A list containing:
       \enumerate{
         \item \code{asrtests.obj}: an \code{\link{asrtests}} object, containing   
               the \code{asreml} object correspondiing to the final fit, 
               a \code{wald.tab} \code{data.frame}, and a \code{test.summary} 
               \code{data.frame} that contains a record of the testing of the terms 
               (see \code{\link{asrtests}} for more details);
         \item \code{sig.tests}: a \code{character vector} whose elements are the 
                the significant terms amongst those tested.}
}

\seealso{\code{\link{asrtests}}, \code{\link{testranfix.asrtests}}, 
         \code{\link{testrcov.asrtests}}, \code{\link{reml.lrt.asreml}}, 
         \code{\link{rmboundary.asrtests}}, \cr 
         \code{\link{newfit.asreml}}, \code{\link{addrm.terms.asrtests}}, 
         \code{\link{sig.devn.reparam.asrtests}}}
\examples{
terms.treat <- c("Sources", "Type", "Species", 
                 "Sources:Type", "Sources:Species")
terms <- sapply(terms.treat, 
                FUN=function(term){paste("spl(xDay):",term,sep="")}, 
                simplify=TRUE)
terms <- c("spl(xDay)",terms)
marginality <-  matrix(c(1,0,0,0,0,0, 1,1,0,0,0,0,  1,0,1,0,0,0, 
                         1,0,1,1,0,0, 1,1,1,1,1,0, 1,1,1,1,1,1), nrow=6)
rownames(marginality) <-terms
colnames(marginality) <-terms
\dontrun{choose <- choose.model.asrtests(marginality, current.asrt, 
                                       pos=TRUE)
         current.asrt <- choose$asrtests.obj
}}
\keyword{asreml}
\keyword{htest}