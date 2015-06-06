\name{predictionplot.asreml}
\alias{predictionplot.asreml}
\title{This function plots the predictions for a term, possibly with error bars.}
\description{This function plots the predictions \code{y} that are based on 
             \code{classify} stored in the \code{data.frame data}. The package 
             \code{ggplot2} is used to produce the plots. Line plots are 
             produced when variables involving \code{x.num} or \code{x.fac} are 
             involved in \code{classify} for the predictions; otherwise, bar 
             charts are produced. Further, for line charts, the argument \code{panels} 
             determines whether a single plot or multiple plots in a single window 
             are produced; for bar charts, the argument \code{panels} is 
             ignored.}
\usage{predictionplot.asreml(classify, y, data, 
                      x.num = NULL, x.fac = NULL, nonx.fac.order = NULL,  
                      colour.scheme = "colour", panels = "multiple", 
                      graphics.device = "windows",
                      error.intervals = "Confidence",  titles = NULL, 
                      y.title = NULL, filestem = NULL, ...)
}
\arguments{
 \item{classify}{a character string giving the combinations of the independent 
                 variables on which the predictions are based. It is 
                 an interaction type term formed from the independent variables, 
                 that is, separating the variable names with the \code{:} operator.}
 \item{y}{a character string giving the name of the variable that is to be plotted 
                 on the Y axis.}
 \item{data}{a \code{data.frame} containing the values of the variables to be plotted. It 
                 should be consistent with an object of class \code{asremlPredict} such as 
                 is stored in the \code{pvals} component of the \code{predictions} component 
                 of the value produced by \code{predict.asreml}; that is, in addition to 
                 variables classifying the predictions, it will include a column 
                 with the name specified in the \code{y} argument, usually 
                 \code{predicted.value} or \code{backtransformed.predictions}; 
                 each row contains a single predicted value. If \code{error.intervals} 
                 is not \code{"none"}, then the \code{predictions} component and, 
                 if present, the \code{backtransforms} component should contain 
                 columns for the lower and upper values of the limits for the interval 
                 with names that begin with \code{lower} and \code{upper}, 
                 respectively. The second part of the name must be one of 
                 \code{Confidence}, \code{StandardError} or 
                 \code{halfLeastSignificant}. The last part needs to be consistent 
                 between the lower and upper limits.}
 \item{x.num}{A character string giving the name of the numeric covariate that 
              corresponds to \code{x.fac}, is potentially included in terms in the 
              fitted model and which corresponds to the x-axis variable. It should 
              have the same number of unique values as the number of levels in 
              \code{x.fac}.}
 \item{x.fac}{A character string giving the name of the factor that corresponds to 
              \code{x.num}, is potentially included in terms in the fitted model and 
              which corresponds to the x-axis variable. It should have the same 
              number of levels as the number of unique values in \code{x.num}. 
              The levels of \code{x.fac} must be in the order in which they are to 
              be plotted - if they are dates, then they should be in the form 
              yyyymmdd which can be achieved using \code{as.Date}.}
 \item{nonx.fac.order}{A character vector giving the order in which factors other 
              than \code{x.fac} are to be plotted in facetted plots (i.e. where the 
              number of non x factors is greater than 1). The first factor in the vector 
              will be plotted on the X axis (if there is no \code{x.num} or 
              \code{x.fac}. Otherwise, the order of plotting the factors is in 
              columns (X facets) and then rows (Y facets). By default the order is 
              in decreasing order for the numbers of levels of the non x factors.}
 \item{colour.scheme}{A character string specifying the colour scheme for the plots. 
              The default is \code{"colour"} which produces coloured lines and bars, 
              a grey background and white gridlines. A value of \code{"black"} 
              results in black lines, grey bars and gridlines and a white background.}
 \item{panels}{Possible values are \code{"single"} and \code{"multiple"}. 
              When line plots are to be produced, because variables involving 
              \code{x.num} or \code{x.fac} are involved in \code{classify} for 
              the predictions, \code{panels} determines whether or not a single 
              panel or multiple panels in a single window are produced. The 
              \code{panels} argument is ignored for bar charts.}
 \item{graphics.device}{A \code{character} specifying a graphics device for plotting. 
              The default is \code{"windows"}, which will result in a windows graphics 
              device being  opened. Setting \code{graphics.device = NULL} will result 
              in plots being produced on the current graphics device.}
\item{error.intervals}{A character string indicating the type of error interval, if any, 
             to calculate and plot in order to indicate uncertainty in the results. 
             Possible values are \code{"none"}, \code{"StandardError"}, \code{"Confidence"} 
             and \code{"halfLeastSignificant"}. Here, any option other than \code{"none"} 
             will result in the interval limits contained in \code{data} being plotted. }
 \item{titles}{A \code{list}, each component of which is named for an object name and 
               contains a \code{character string} giving a title to use in output 
              (e.g. tables and graphs) for the object. Here they will be used for 
              axis labels.}
 \item{filestem}{A character sting giving the beginning of the name of the file in which to 
              save the plot. If \code{filestem = NULL}, the plot is not saved. The remainder 
              of the file name will be generated automatically and consists of the 
              following elements separated by full stops: the classify term, 
              \code{Bar} or \code{Line} and, if \code{error.intervals} 
              is not \code{"none"}, one of \code{SE}, \code{CI} or \code{LSI}. The
              file will be saved as a `png' file in the current work directory.}
 \item{y.title}{The title to be displayed on the y axis of any plot.} 
 \item{...}{further arguments passed to \code{ggplot}.}
}
\value{no values are returned.}

\seealso{\code{\link{predictiondiffs.asreml}}, \code{\link{pred.present.asreml}}, 
         \code{ggplot}, \code{Devices}}
\examples{\dontrun{
current.asr <- asreml(fixed = log.Turbidity ~ Benches + Sources + Type + Species +
                       Sources:Type + Sources:Species + 
                       Sources:xDay + Species:xDay + Species:Date,
                       data = Runoff.longi.dat, keep.order = TRUE)
predictions <- predict(current.asr, class="Species:Date:xDay", 
                       present=c("Type","Species","Sources"),
                       levels=list(xDay=unique(Runoff.longi.dat$xDay)))$predictions$pvals
predictions <- predictions[predictions$est.status == "Estimable",]
predictionplot.asreml(classify="Species:Date:xDay", diffs$predictions, current.asrt$wald.tab, 
               x.num = "xDay", x.fac = "Date", 
               x.title = "Days since first observation",
               y.title = "Predicted log(Turbidity)",
               present = c("Type","Species","Sources"))


diffs <- predictparallel.asreml(classify="Species:Date:xDay", 
                                present=c("Type","Species","Sources"), 
                                asreml.obj = current.asr, 
                                x.num = "xDay", x.fac = "Date", 
                                x.pred.values=sort(unique(Runoff.longi.dat$xDay)),
                                x.plot.values=c(0,28,56,84))
predictionplot.asreml(diffs$predictions, current.asrt$wald.tab, 
                                x.num = "xDay", x.fac = "Date", 
                                x.title = "Days since first observation",
                                y.title = "Predicted log(Turbidity)"))
}}
\keyword{asreml}