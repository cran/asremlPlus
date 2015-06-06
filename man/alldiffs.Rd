\name{alldiffs}
\alias{alldiffs}
\title{Forms an object of S3-class \code{alldiffs} that stores the predictions for a model 
       fitted using asreml, along with supplied statistics for all pairwise differences.}
\description{Creates an object of S3-class \code{alldiffs} that consists of a \code{list} 
             containing the following components: \code{predictions}, \code{differences}, 
             \code{p.differences}, \code{sed}, \code{LSD} and \code{backtransforms}. 
             Predictions must be supplied to the functions while the others will be set 
             only if they are supplies; those not supplied are set to \code{NULL}. 
             It also has attributes \code{response}, 
             \code{response.title}, \code{term}, \code{classify} and \code{tdf}, 
             which will be set to the values supplied or \code{NULL} if none are supplied.}

\usage{alldiffs(predictions, differences = NULL, p.differences = NULL, 
         sed = NULL, LSD = NULL, backtransforms = NULL, 
         response = NULL, response.title = NULL, 
         term = NULL, classify = NULL, tdf = NULL)}
\arguments{
 \item{predictions}{a \code{data.frame} containing the predicted values that is consistent with an 
                    object of class \code{asremlPredict} such as 
                    is stored in the \code{pvals} component of the \code{prediction} component 
                    of the value produced by \code{predict.asreml}. That is, in addition to 
                    variables classifying the predictions, it will include columns named 
                    \code{predicted.value}, \code{standard.error} and \code{est.status}; 
                    each row contains a single predicted value. It may also contain
                    columns for the lower and upper confidence limits for the predictions. 
                    If \code{LSD} is not \code{NULL}, the mean LSD will be added as an attribute 
                    named \code{meanLSD}.}
 \item{differences}{a \code{matrix} containing all pairwise differences between
                    the predictions; it should have the same number of rows and columns as there are
                    rows in \code{predictions}.}
 \item{p.differences}{a \code{matrix} containing p-values for all pairwise differences 
                    between the predictions; each p-value is computed as the probability of a t-statistic 
                    as large as or larger than the observed difference divided by its standard error. 
                    The degrees of freedom of the t distribution for computing it are computed as 
                    the denominator degrees of freedom of the F value for the fixed term, if available; 
                    otherwise, the degrees of freedom stored in the attribute \code{tdf} are used; 
                    the matrix should be of the same size as that for \code{differences}.}
 \item{sed}{a \code{matrix} containing the standard errors of all pairwise differences 
                    between the predictions; they are used in computing the p-values.}
 \item{LSD}{a \code{data.frame} containing the mean, minimum and maximum LSD for determining             
                    the significance of pairwise differences.}
 \item{backtransforms}{a \code{data.frame} containing the backtransformed values of the predicted values 
                    that is consistent with an object of class \code{asremlPredict} such as 
                    is stored in the \code{pvals} component of the \code{prediction} component 
                    of the value produced by \code{predict.asreml}. That is, in addition to 
                    variables classifying the predictions, it will include columns named 
                    \code{backtransformed.predictions} and \code{est.status}; it may also contain 
                    columns for the lower and upper confidence limits; 
                    each row contains a single predicted value.}
 \item{response}{a \code{character} specifying the response variable for the 
                    predictions. It is stored as an attribute to the \code{alldiffs} 
                    object.}
 \item{response.title}{a \code{character} specifying the title for the response variable 
                    for the predictions. It is stored as an attribute to the \code{alldiffs} 
                    object.}
 \item{term}{a character string giving the variables that define the term 
                 that was fitted using \code{asreml} and that corresponds 
                 to \code{classify}. It is often the same as \code{classify}. 
                 It is stored as an attribute to the \code{alldiffs} object.}
 \item{classify}{a character string giving the variables that define the margins
                 of the multiway table used in the prediction. Multiway tables are 
                 specified by forming an interaction type term from the 
                 classifying variables, that is, separating the variable names 
                 with the \code{:} operator. It is stored as an attribute to the 
                 \code{alldiffs} object.}
 \item{tdf}{an \code{integer} specifying the degrees of freedom of the standard error. It is used as 
                the degrees of freedom for the t-distribution on which p-values and confidence 
                intervals are based. 
                It is stored as an attribute to the \code{alldiffs} object.}
}
\value{An object of S3-class \code{alldiffs} with attributes \code{response}, 
                \code{response.title}, \code{term}, \code{classify} and \code{tdf}.}


\seealso{\code{\link{asremlPlus-package}}, \code{\link{print.alldiffs}}, 
         \code{\link{predictparallel.asreml}},\code{\link{predictionplot.asreml}}, \cr
        \code{\link{predictiondiffs.asreml}}, \code{\link{pred.present.asreml}}}
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
\keyword{htest}