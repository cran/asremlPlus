\name{newfit.asreml}
\alias{newfit.asreml}
\title{refits an asreml model with modified model formula using either 
       a call to \code{update.asreml} or a direct call to \code{asreml}.}
\description{Extracts the call from the \code{asreml.obj} and 
             evaluates that call, replacing any arguments with changed values. 
             If \code{update} is \code{TRUE} and \code{set.terms} is not set, 
             the call is evaluated using \code{update.asreml}; otherwise, 
             it is evaluated using a direct call to \code{asreml}. 
             The principal difference is that the latter does not enforce the 
             use of previous values of the variance parameters as initial 
             values; it sets \code{G.param} and \code{R.param} to \code{NULL} 
             or to values as specified for \code{set.terms}. 
             The \code{...} argument can be used to pass \code{G.param} 
             and/or \code{R.param}, provided \code{update} is \code{FALSE} 
             and \code{set.terms} is not set.}
\usage{newfit.asreml(asreml.obj, fixed., random., sparse., rcov., 
              update = TRUE, keep.order = TRUE, set.terms = NULL, 
               ignore.suffices = TRUE, constraints = "P", initial.values = NA, ...)}
\arguments{
 \item{asreml.obj}{a valid \code{asreml} object with with 
                   a component named \code{call} (from a previous 
                   call to either \code{asreml} or \code{update.asreml}).}
 \item{fixed.}{a character or formula specifying changes to the fixed 
               formula. This is a two-sided formula 
               where "." is substituted for existing components in the 
               fixed component of \code{asreml.obj$call}.}
 \item{random.}{a character or formula specifying changes to the random 
                formula. This is a one-sided formula 
                where "." is substituted for existing components in the 
                random component of \code{asreml.obj$call}.}
 \item{sparse.}{a character or formula specifying changes to the sparse 
                formula. This is a one-sided formula 
                where "." is substituted for existing components in the 
                sparse component of \code{asreml.obj$call}.}
 \item{rcov.}{a character or formula specifying changes to the error 
                formula. This is a one-sided formula 
                where "." is substituted for existing components in the 
                rcov component of \code{asreml.obj$call}.}
 \item{update}{a logical indicated whether to use \code{update.asreml} or 
               \code{asreml} to evaluate the modified call. 
               If \code{TRUE},  use \code{update.asreml} to evaluate the 
               modified \code{call}. In doing this the arguments 
               \code{R.param} and \code{G.param} are set to those in the 
               \code{asreml.obj} so that the values from the previous 
               model are used as starting values. If \code{FALSE} then a 
               call is made to \code{asreml} itself, in which the only 
               changes from the previous call are those specified in the 
               arguments to \code{newfit.asreml}.}
 \item{keep.order}{a logical value indicating whether the terms should keep 
               their positions. If \code{FALSE} the terms are reordered so 
               that main effects come first, followed by the interactions, 
               all second-order, all third-order and so on. Effects of a 
               given order are kept in the order specified.}
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
 \item{...}{additional arguments to the call, or arguments with changed 
            values.}
}
\value{An \code{asreml} object.}


\references{Butler, D. G., et al. (2010). \emph{Analysis of Mixed Models 
            for {S} language environments: {ASReml-R} reference manual.}  
            Brisbane, DPI Publications.}

\seealso{\code{update.asreml}, \code{\link{setvarianceterms.asreml}}}
\examples{\dontrun{
    m2.asreml <- newfit.asreml(m1.asreml, random. = "~ . - Blocks:Plots", maxiter=75)
}}
\keyword{asreml}