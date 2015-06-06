\name{print.alldiffs}
\alias{print.alldiffs}
\title{Prints the values in an \code{\link{alldiffs}} object in a nice format.}
\description{Prints the predictions and standard errors from using asreml 
  to fit models in the same way as \code{asreml} prints them. Also prints out all 
  pairwise differences between the predictions to 2 signigicant figures,  
  along with their p-values and standard errors to 4 decimal places. If LSDs are 
  requested the mean. minimum and maximum LSDs will be printed.}
\usage{\method{print}{alldiffs}(x, which = "all", ...)}
\arguments{
 \item{x}{A \code{list} returned from \code{asreml.predictiondiffs}.}
 \item{which}{A \code{character vector} containing a combination of 
              \code{predictions}, \code{backtransforms}, \code{differences}, 
              \code{p.differences}, \code{sed}, \code{LSD} and \code{all}.
              These nominate which components of the \code{\link{alldiffs}} 
              object to print.}
 \item{...}{further arguments passed to or from other methods.}
}
\value{No value is returned, but the elements of the list in \code{x},  
       returned from \code{asreml.predictiondiffs}, are printed.}


\seealso{\code{\link{alldiffs}}, \code{\link{predictiondiffs.asreml}}}
\examples{\dontrun{
print.alldiffs(predictiondiffs.asreml(asreml=splitplot.asr, wald=wald,
               present=c("Type","Sources","Species"), 
               factors=list("Species")))
}}
\keyword{asreml}