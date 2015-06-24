\name{info.crit.asreml}
\alias{info.crit.asreml}
\title{Computes AIC and BIC for a model.}
\description{Computes Akiake and Bayesian (Schwarz) Information Criteria for a model. The function \code{info.crit} is provided for backwards compatibility.}
\usage{info.crit.asreml(asreml.obj)}
\arguments{
 \item{asreml.obj}{An \code{asreml} object resulting from the fitting of 
               a model using REML.}
}
\value{A data frame containing the Residual degrees of freedom, 
       AIC, BIC and log of the REML value.}
\seealso{\code{\link{reml.lrt.asreml}}}
\examples{\dontrun{
   data(Wheat.dat)
   current.asr <- asreml(yield ~ Rep + WithinColPairs + Variety, 
                         random = ~ Row + Column + units,
                         rcov = ~ ar1(Row):ar1(Column), 
                         data=Wheat.dat)
    info.crit.asreml(current.asr)
}}
\keyword{htest}
