\name{predictparallel.asreml}
\alias{predictparallel.asreml}
\title{Uses an asreml object and a wald.tab to form the predictions and 
       associated statistics for a term. It stores the results in an object of class 
       alldifffs and may print the results. 
       It can be used when a numeric vector and a factor that has parallel values    
       both occur in the model and need to be taken into account.}
\description{This function forms the predictions for \code{term} using \code{classify} 
             and the supplied \code{asreml} object and stores them in an \code{\link{alldiffs}} 
             object. If \code{x.num} is supplied, the 
             predictions will be obtained for the values supplied in \code{x.pred.values} 
             and, if supplied, \code{x.plot.values} will replace them in the 
             \code{\link{alldiffs}} object that is returned. If \code{x.fac}, but not 
             \code{x.num}, is specified, predictions will involve it and, if supplied, 
             \code{x.plot.values} will replace the levels of \code{x.fac} in the 
             \code{\link{alldiffs}} object that is returned.
              In order to get the correct predictions you may need to supply
              additional arguments to predict through \code{...} e.g. \code{present}. Any 
              aliased predictions will be removed, as will any standard error of pairwise 
              differences involving them.

              Also calculated are the approximate degrees of freedom of the standard errors 
              of the predictions. If the deominator degrees of freedom for \code{term} are 
              available in \code{wald.tab}, they are used. Otherwise the residual degrees 
              of freedom or the maximum of the denominator degrees in \code{wald.tab}, 
              excluding the Intercept, are used. Which is used depends on the setting of 
              \code{dDF.na}. These degrees of freedom are used for the t-distribution on 
              which p-values and confidence intervals are based. It is stored as an 
              attribute to the \code{alldiffs} object. The degrees of freedom are also 
              used in valculating the minimum, mean and maximum LSD fro comparing pairs 
              of predictions, which are also stored in the \code{alldiffs} object.

              If \code{pairwise = TRUE}, all pairwise differences between the \code{predictions},   
              their standard errors, p-values and LSD statistics are computed using 
              \code{\link{predictiondiffs.asreml}}. This adds them to the \code{alldiffs} 
              object as additional \code{list} components named \code{differences}, 
              \code{sed}, \code{p.differences} and \code{LSD}.

              If a transformation has been applied (any one of \code{transform.power} is not one, 
              \code{scale} is  not one and \code{offset} is nonzero), the back-transforms of 
              the predicted values and their lower and upper confidence intervals are added 
              to a \code{data.frame} that is consistent with an object of class 
              \code{asremlPredict}, such as is stored in the \code{pvals} component of the 
              \code{prediction} component of the value produced by \code{predict.asreml}. 
              This \code{data.frame} is added to the \code{alldiffs} object as a \code{list} 
              component called \code{backtransforms}.

              The printing of the components produced is controlled by the \code{tables} 
              argument.
}
\usage{predictparallel.asreml(classify, term = NULL, asreml.obj = NULL, titles = NULL,  
                       x.num = NULL, x.fac = NULL,  
                       x.pred.values = NULL, x.plot.values = NULL, 
                       error.intervals = "Confidence", avsed.tolerance = 0.25, 
                       pairwise = TRUE, tables = "all" , levels.length = NA, 
                       transform.power = 1, offset = 0, scale = 1, 
                       inestimable.rm = TRUE, wald.tab = NULL,
                       alpha = 0.05, dDF.na = "residual",  dDF.values = NULL, 
                       trace = FALSE, ...)
}
\arguments{
 \item{classify}{a character string giving the variables that define the margins
                 of the multiway table to be predicted. Multiway tables are 
                 specified by forming an interaction type term from the 
                 classifying variables, that is, separating the variable names 
                 with the \code{:} operator.}
 \item{term}{a character string giving the variables that define the term 
                 that was fitted using \code{asreml} and that corresponds 
                 to \code{classify}. It only needs to be specified when 
                 it is different to \code{classify}.}
 \item{asreml.obj}{\code{asreml} object for a fitted model.}
 \item{titles}{A \code{list}, each component of which is named for an object name and 
               contains a \code{character string} giving a title to use in output 
              (e.g. tables and graphs) for the object. Here they will be used for 
              table headings.}
 \item{x.num}{A character string giving the name of the numeric covariate that 
              (i) corresponds to \code{x.fac}, (ii)is potentially included in terms in the 
              fitted model, and (iii) which corresponds to the x-axis variable. It should 
              have the same number of unique values as the number of levels in 
              \code{x.fac}.}
 \item{x.fac}{A character string giving the name of the factor that (i) corresponds to 
              \code{x.num}, (ii) is potentially included in terms in the fitted model, and 
              (iii) which corresponds to the x-axis variable. It should have the same 
              number of levels as the number of unique values in \code{x.num}. 
              The levels of \code{x.fac} must be in the order in which they are to 
              be plotted - if they are dates, then they should be in the form 
              yyyymmdd which can be achieved using \code{as.Date}.}
 \item{x.pred.values}{The values of \code{x.num} for which predicted values are required.}
 \item{x.plot.values}{The actual values to be plotted on the x axis. They are 
               needed when values different to those in \code{x.num} are to be 
               plotted or \code{x.fac} is to be plotted because there is no 
               \code{x.num} term corresponding to the same term with \code{x.fac}.} 
 \item{error.intervals}{A character string indicating the type of error interval, if any, 
             to calculate and plot in order to indicate uncertainty in the results. 
             Possible values are \code{"none"}, \code{"StandardError"}, \code{"Confidence"} 
             and \code{"halfLeastSignificant"}. The default is for confidence limits to 
             be used. The \code{"halfLeastSignificant"} option results in half the 
             mean Least Significant Difference (LSD) being added and subtracted to the 
             predictions, the mean LSD being calculated using the average of the 
             standard errors of all pairwise differences (SEDs) between the predictions. 
             However, if the range of the SEDs divided by the average of the SEDs exceeds 
             \code{avsed.tolerance}, calculations and plotting will revert to confidence 
             intervals. Also, half LSDs cannot be used for backtransformed values and so 
             confidence intervals will be used instead.}
 \item{avsed.tolerance}{The values of the range of the SEDs divided by the average of the 
             SEDs that, if exceeded, will cause calculations and plotting to revert to 
             confidence intervals. It should be a value between 0 and 1.}
 \item{pairwise}{A logical indicating whether all pairwise differences of the
                \code{predictions} and their standard errors and p-values are to be 
                computed and stored. If \code{tables} is equal to \code{"differences"} 
                or \code{"all"} or \code{error.intervals} is equal to 
                \code{"halfLeastSignificant"}, they will be stored irrespective of the 
                value of \code{pairwise}.}
 \item{tables}{A \code{character vector} containing a combination of \code{none}, 
              \code{predictions}, \code{backtransforms}, \code{differences}, 
              \code{p.differences}, \code{sed}, \code{LSD} and \code{all}.
              These nominate which components of the \code{\link{alldiffs}} 
              object to print.}
 \item{levels.length}{The maximum number of characters from the the levels of 
            factors to use in the row and column labels of the tables of 
            pairwise differences and their p-values and standard errors.}
 \item{transform.power}{A number specifying the power of a transformation, if one 
              has been applied to the response variable. Unless it is equal to 1, the default, 
              back-transforms of the predictions will be obtained and presented in tables 
              or graphs as appropriate. The back-transformation raises the predictions to the 
              power equal to the reciprocal of \code{transform.power}, unless it equals 0 in 
              which case the exponential of the predictions is taken.} 
 \item{offset}{A number that has been added to each value of the response after any scaling 
               and before applying any power transformation.}
 \item{scale}{A number by which each value of the response has been multiply before adding 
               any offset and applying any power transformation.}
 \item{inestimable.rm}{A \code{logical} indicating whether rows for  predictions that 
                       are not estimable are to be removed from the components of the
                       \code{\link{alldiffs}} object.}
 \item{wald.tab}{a \code{data frame} containing the pseudo-anova table for the 
                 fixed terms produced by a call to \code{wald.asreml}. The main 
                 use of it here is in determinining the degrees of freedom of thew standard errors
                 of the predictions. denominator degrees of freedom when
                 p-values or confidence intervals are to be calculated.}
 \item{alpha}{the significance level for a test or one minus the confidence level 
              for confidence intervals.}
 \item{dDF.na}{the method to use to obtain approximate denominator degrees of freedom. 
              when the numeric or algebraic methods produce an \code{NA}. Consistent with 
              when no denDF are available, the default is \code{"residual"} and so the residual 
              degrees of freedom from \code{asreml.obj$nedf} are used. If 
              \code{dDF.na = "none"}, no subtitute denominator degrees of freedom 
              are employed; if \code{dDF.na = "maximum"}, the maximum of those denDF 
              that are available, excluding that for the Intercept, is used; if all 
              denDF are \code{NA}, \code{asreml.obj$nedf} is used. If 
              \code{dDF.na = "supplied"}, a \code{vector} of values for the 
              denominator degrees of freedom is to be supplied in \code{dDF.values}. 
              Any other setting is ignored and a warning message produced. Generally, 
              substituting these degrees of freedom is anticonservative in that it 
              is likely that the degrees of freedom used will be too large.}
 \item{dDF.values}{A \code{vector} of values to be used when \code{dDF.na = "supplied"}. 
              Its values will be used when \code{denDF} in a test for a fixed effect
              is \code{NA}. This vector must be the same length as the number of 
              fixed terms, including (Intercept) whose value could be \code{NA}.}
 \item{trace}{if TRUE then partial iteration details are displayed when ASReml-R 
              functions are invoked; if FALSE then no output is displayed.}
 \item{...}{further arguments passed to \code{predict.asreml}.}
}
\value{An \code{\link{alldiffs}} object with predictions and their standard errors and, 
           depending on the settings 
           of the arguments, all pairwise differences between predictions, their 
           standard errors and p-values and LSD statistics. If \code{power.transform} is 
           not one, it will 
           contain a \code{data.frame} with the backtransformed predictions. If 
           \code{error.intervals} is not \code{"none"}, then the \code{predictions} 
           component and, if present, the \code{backtransforms} component will contain 
           columns for the lower and upper values of the limits for the interval. 
           The names of these columns will consist of three parts separated 
           by full stops: 1) the first part will be \code{lower} or \code{upper}; 
           2) the second part will be one of \code{Confidence}, \code{StandardError} 
           or \code{halfLeastSignificant}; 3) the third component will be \code{limits}.

           The name of the \code{response}, 
           the \code{term}, the \code{classify} and \code{tdf}, as well as the degrees of freedom 
           of the standard error, will be set as attributes to the object.}

\seealso{\code{\link{alldiffs}}, \code{\link{print.alldiffs}}, \code{\link{predictiondiffs.asreml}}, 
         \code{\link{pred.present.asreml}}, \code{as.Date}, \cr 
         \code{\link{predictionplot.asreml}}, \code{\link{pred.present.asreml}}, \code{predict.asreml}}
\examples{\dontrun{
current.asr <- asreml(fixed = pH ~ Benches + (Sources * (Type + Species)), 
                      random = ~ Benches:MainPlots,
                      keep.order=TRUE, data= Runoff.longi.dat)
diffs <- predictparallel.asreml(classify = "Sources:Type", 
                                asreml.obj = current.asr, 
                                x.num = "xDay", x.fac = "Date", 
                                x.pred.values=sort(unique(Runoff.longi.dat$xDay)),
                                x.plot.values=c(0,28,56,84))
}}
\keyword{asreml}