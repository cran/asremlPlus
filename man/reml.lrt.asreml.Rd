\name{reml.lrt.asreml}
\alias{reml.lrt.asreml}
\title{Performs REML likelihood ratio test.}
\description{Extracts the REML log likelhood and number of variance 
  parameters from two \code{asreml} objects. It assumes that the second 
  \code{asreml} object is the result of fitting a model that is a 
  reduced version of the model for the first object. In the case 
  where the reduced model is obtained by setting positively-constrained 
  variance parameters in the full model to zero, the \code{positive.zero} 
  argument should be set to \code{TRUE} so that 
  the p-value is computed using a mixture of chi-square distributions as 
  described in Self and Liang (1987).
  
  The function shecks that the models do not differ in either their fixed 
  or sparse models.
  
  The function \code{reml.lrt} is provided for backwards compatibility.}
\usage{
reml.lrt.asreml(full.asreml.obj, reduced.asreml.obj, 
                positive.zero = FALSE, bound.test.parameters = "none")}
\arguments{
 \item{full.asreml.obj}{\code{asreml} object for the full model.}
 \item{reduced.asreml.obj}{\code{asreml} object for the reduced model.}
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
}
\value{A data frame containing the log of the likelihood ratio, its degrees of 
       freedom and its p-value.}

\note{The degrees of freedom for the test are computed as the difference 
      between the two models in the number of variance parameters that are 
      unfixed, nonsingular and unconstrained by relationships among them.  \cr

      This procedure is only approriate when the null hypothesis is that 
      all parameters are on the boundary of the parameter space or that all 
      parameters are in the interior of the parameter space. Mixed cases 
      have been discussed by Self and Liang (1987), but are not implemented 
      here.}

\references{Self, S.G., and Liang, K-Y. (1987) Asymptotic Properties of 
            Maximum Likelihood Estimators and Likelihood Ratio Tests Under 
            Nonstandard Conditions. \emph{Journal of the American 
            Statistical Association}, \bold{82}, 605-10.}

\seealso{\code{\link{info.crit.asreml}}, \code{\link{testranfix.asrtests}}}
\examples{\dontrun{
    reml.lrt.asreml(ICV.max, ICV.red, bound.test.parameters = "onlybound")
}}
\keyword{htest}
\keyword{asreml}