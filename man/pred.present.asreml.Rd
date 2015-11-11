\name{pred.present.asreml}
\alias{pred.present.asreml}
\title{This function forms the predictions for each significant term and presents 
       them in tables and/or graphs.}
\description{This function forms the predictions for each term in \code{terms} 
             using a supplied \code{asreml} object and 
             \code{\link{predictparallel.asreml}}. 
             Tables are produced using \code{\link{predictparallel.asreml}}, 
             in conjunction with \code{\link{predictiondiffs.asreml}}, 
             with the argument \code{tables} specifying which tables are printed. 
             The argument \code{plots}, along with \code{transform.power}, 
             controls which plots are produced. The plots are 
             produced using \code{\link{predictionplot.asreml}}, with 
             line plots produced when variables involving \code{x.num} or \code{x.fac} 
             are involved in \code{classify} for the predictions and bar charts 
             otherwise.  
             In order to get the correct predictions you may need to supply 
             additional arguments to predict through \code{...} e.g. \code{present}.}
\usage{pred.present.asreml(terms, asreml.obj = NULL, 
                    wald.tab = NULL, dDF.na = "residual", dDF.values = NULL, 
                    x.num = NULL, x.fac = NULL, nonx.fac.order = NULL,  
                    x.pred.values = NULL, x.plot.values = NULL, 
                    plots = "predictions", panels = "multiple", 
                    graphics.device = NULL, 
                    error.intervals = "Confidence", avsed.tolerance = 0.25, 
                    titles = NULL, colour.scheme = "colour", save.plots = FALSE, 
                    transform.power = 1, offset = 0, scale = 1, 
                    pairwise = TRUE, tables = "all", levels.length = NA, 
                    alpha = 0.05, inestimable.rm = TRUE, trace = FALSE, ...)
}
\arguments{
 \item{terms}{a \code{character vector} giving the terms for which predictions 
                 are required.}
 \item{asreml.obj}{\code{asreml} object for a fitted model.}
 \item{wald.tab}{a \code{data frame} containing the pseudo-anova table for the 
                 fixed terms produced by a call to \code{wald.asreml}. The main 
                 use of it here is in getting denominator degrees of freedom when
                 confidence intervals are to be plotted.}
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
 \item{x.num}{A character string giving the name of the numeric covariate that 
              corresponds to \code{x.fac}, is potentially included in terms in the 
              fitted model and which corresponds to the x-axis variable. It should 
              have the same number of unique values as the number of levels in 
              \code{x.fac}.}
 \item{x.fac}{A \code{character} string giving the name of the factor that corresponds to 
              \code{x.num}, is potentially included in terms in the fitted model and 
              which corresponds to the x-axis variable. It should have the same 
              number of levels as the number of unique values in \code{x.num}. 
              The levels of \code{x.fac} must be in the order in which they are to 
              be plotted - if they are dates, then they should be in the form 
              yyyymmdd which can be achieved using \code{as.Date}.}
 \item{nonx.fac.order}{A \code{character vector} giving the order in which factors other 
              than \code{x.fac} are to be plotted in plots with multiple panels 
              (i.e. where the number of non-x factors is greater than 1). 
              The first factor in the vector 
              will be plotted on the X axis (if there is no \code{x.num} or 
              \code{x.fac}. Otherwise, the order of plotting the factors is in 
              columns (X facets) and then rows (Y facets). By default the order is 
              in decreasing order for the numbers of levels of the non x factors.}
 \item{x.pred.values}{The values of \code{x.num} for which predicted values are required.}
 \item{x.plot.values}{The actual values to be plotted on the x axis or in the labels of 
               tables. They are 
               needed when values different to those in \code{x.num} are to be 
               plotted or \code{x.fac} is to be plotted because there is no 
               \code{x.num} term corresponding to the same term with \code{x.fac}.} 
 \item{plots}{Possible values are \code{"none"}, \code{"predictions"}, 
              \code{"backtransforms"} and \code{"both"}. Plots are not produced 
              if the value is \code{"none"}. If data are not transformed for 
              analysis (\code{transform.power} = 1), a plot of the predictions 
              is produced provided \code{plots} is not \code{"none"}. If the 
              data are transformed, the value of \code{plots} determines what 
              is produced.}
 \item{panels}{Possible values are \code{"single"} and \code{"multiple"}. 
              When line plots are to be produced, because variables involving 
              \code{x.num} or \code{x.fac} are involved in \code{classify} for 
              the predictions, \code{panels} determines whether or not a single 
              panel or multiple panels in a single window are produced. The 
              \code{panels} argument is ignored for for bar charts.}
 \item{graphics.device}{A \code{character} specifying a graphics device for plotting. 
              The default is \code{graphics.device = NULL}, which will result 
              in plots being produced on the current graphics device. Setting it to 
              \code{"windows"}, for example,  will result in a windows graphics 
              device being  opened.}
 \item{error.intervals}{A character string indicating the type of error interval, if any, 
             to calculate and plot in order to indicate uncertainty in the results. 
             Possible values are \code{"none"}, \code{"StandardError"}, \code{"Confidence"} 
             and \code{"halfLeastSignificant"}. The default is for confidence limits to 
             be used. The \code{"halfLeastSignificant"} option results in half the 
             Least Significant Difference (LSD) being added and subtracted to the 
             predictions, the LSD being calculated using the average of the 
             standard errors of all pairwise differences (SEDs) between the predictions. 
             However, if the range of the SEDs divided by the average of the SEDs exceeds 
             \code{avsed.tolerance}, calculations and plotting will revert to confidence 
             intervals. Also, half LSDs cannot be used for backtansformed values and so 
             confidence intervals will be used instead.}
 \item{avsed.tolerance}{The values of the range of the SEDs divided by the average of the 
             SEDs that, if exceeded, will cause calculations and plotting to revert to 
             confidence intervals. It should be a value between 0 and 1.}
 \item{titles}{A \code{list}, each component of which is named for an object name and
               contains a \code{character string} giving a title to use in output 
              (e.g. tables and graphs) for the object. Here they will be used for 
              axis labels.}
 \item{colour.scheme}{A character string specifying the colour scheme for the plots. 
              The default is \code{"colour"} which produces coloured lines and bars, 
              a grey background and white gridlines. A value of \code{"black"} 
              results in black lines, grey bars and gridlines and a white background.}
 \item{save.plots}{A \code{logical} that determines whether any plots will be saved. 
              If they are to be saved, a file name will be generated that consists of the 
              following elements separated by full stops: the response variable name with 
              \code{.back} if backtransformed values are being plotted, 
              the classify term, \code{Bar} or \code{Line} and, if \code{error.intervals} 
              is not \code{"none"}, one of \code{SE}, \code{CI} or \code{LSI}. The 
              file will be saved as a `png' file in the current work directory.}
 \item{transform.power}{A number specifying the power of a transformation, if one 
              has been applied to the response variable. Unless it is equal to 1, the default, 
              back-transforms of the predictions will be obtained and presented in tables 
              or graphs as appropriate. The back-transform will raise the predictions to the 
              power equal to the reciprocal of \code{transform.power}, unless it equals 0 in 
              which case the exponential will be taken. Any scaling and offsetting will also be 
              taken into account in the backtransformation.} 
 \item{offset}{A number that has been added to each value of the response after any scaling 
               and before applying any power transformation. Unless it is equal to 0, the 
               default, back-transforms of the predictions will be obtained and presented in 
               tables or graphs as appropriate. The back-transform will, after backtransforming 
               for any power transformation and scaling for any scale transformation, subtract 
               the \code{offset}.}
 \item{scale}{A number by which each value of the response has been multiply before adding 
               any offset and applying any power transformation. Unless it is equal to 1, the
               default, back-transforms of the predictions will be obtained and presented in 
               tables or graphs as appropriate. The back-transform will, after backtransforming 
               for any power transformation, divide by the \code{scale}.}
 \item{pairwise}{A logical indicating whether all pairise differences of the
                \code{predictions} and their standard errors and p-values are to be 
                computed and stored. If \code{tables} is equal to \code{"differences"} 
                or \code{"all"} or \code{error.intervals} is equal to 
                \code{"halfLeastSignificant"}, they will be stored irrespective of the 
                value of \code{pairwise}.}
 \item{tables}{The elements of the \code{\link{alldiffs}} object to print. Possible
             values are \code{"none"}, \code{"predictions"}, \code{"backtransforms"}, 
             \code{"nodifferences"}, \code{"differences"} and \code{"all"}. 
             The option \code{"nodifferences"} will result in \code{"predictions"} and 
             \code{"backtransforms"} being printed. The option \code{"differences"} will 
             also result in the printing of the \code{predictions} from the 
             \code{\link{alldiffs}} object.}
 \item{levels.length}{The maximum number of characters from the the levels of 
            factors to use in the row and column labels of the tables produced by 
            \code{\link{predictiondiffs.asreml}}.}
 \item{alpha}{the significance level for the test or 1 - alpha is the confidence level 
              for confidence intervals.}
 \item{inestimable.rm}{A \code{logical} indicating whether rows for  predictions that 
                       are not estimable are to be removed from the components of the
                       \code{\link{alldiffs}} object.}
 \item{trace}{if TRUE then partial iteration details are displayed when ASReml-R 
              functions are invoked; if FALSE then no output is displayed.}
 \item{...}{further arguments passed to \code{predict.asreml} via 
            \code{\link{predictparallel.asreml}} and to \code{ggplot} 
            via \code{\link{predictionplot.asreml}}.}
}
\value{a list containing a \code{\link{alldiffs}} object for each \code{term} for 
             which tables are produced. The names of the components of this list are 
             the \code{terms} with full-stops (\code{.}) replacing colons (\code{:}).}

\seealso{\code{\link{predictparallel.asreml}}, \code{\link{predictiondiffs.asreml}}, 
         \code{\link{predictionplot.asreml}}, 
         \code{\link{print.alldiffs}}, \cr
         \code{as.Date}, \code{Devices}}
\examples{\dontrun{
  pred.present.asreml(choose$sig.terms, current.asrt$asreml.obj, current.asrt$wald.tab, 
                      x.fac = "Date", x.num = "xDay", 
                      x.pred.values=sort(unique(Runoff.longi.dat$xDay)),
                      x.plot.values=c(0,28,56,84),
                      x.title = "Days since first observation",
                      y.title = "Predicted log(Turbidity)",
                      present = c("Type","Species","Sources"))
}}
\keyword{asreml}