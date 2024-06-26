\name{plotPredictions.data.frame}
\alias{plotPredictions.data.frame}
\alias{plotPredictions}
\title{Plots the predictions for a term, possibly with error bars.}
\description{This function plots the predictions \code{y} that are based on 
             \code{classify} and stored in the \code{data.frame} \code{data}. The package 
             \code{ggplot2} is used to produce the plots. Line plots are 
             produced when variables involving \code{x.num} or \code{x.fac} are 
             involved in \code{classify} for the predictions; otherwise, bar 
             charts are produced. Further, for line charts, the argument \code{panels} 
             determines whether a single plot or multiple plots in a single window 
             are produced; for bar charts, the argument \code{panels} is 
             ignored.}
\usage{
\method{plotPredictions}{data.frame}(data, classify, y, 
                x.num = NULL, x.fac = NULL, nonx.fac.order = NULL,  
                colour.scheme = "colour", panels = "multiple", 
                graphics.device = NULL,
                error.intervals = "Confidence", interval.annotate = TRUE,
                titles = NULL, y.title = NULL, 
                filestem = NULL, printPlot = TRUE, ggplotFuncs = NULL, ...)
}
\arguments{
 \item{data}{A \code{\link{predictions.frame}}, or \code{data.frame}, containing the 
                 values of the variables to be plotted. 
                 Generally, it should contain the variables classifying the predictions 
                 and include a column with the name specified in the \code{y} argument, 
                 usually \code{predicted.value} or \code{backtransformed.predictions}; 
                 each row contains a single predicted value.  It should also include 
                 columns for the \code{standard.error} and \code{est.status}. 
                 The number of rows should equal the number of unique combinations of 
                 the classifying variables. While such a \code{data.frame} 
                 can be constructed from the beginning, the \code{pvals} component 
                 of the value produced by \code{predict.asreml} is a suitable value to 
                 supply for this argument. Note that the names \code{standard.error} and 
                 \code{est.status} have been changed to \code{std.error} and \code{status} 
                 in the \code{pvals} component produced by \code{asreml-R4}; if the new names 
                 are in the \code{data.frame} supplied to \code{predictions}, they will be 
                 returned to the previous names. 

                 If \code{error.intervals} 
                 is not \code{"none"}, then the \code{predictions} component and, 
                 if present, the \code{backtransforms} component should contain 
                 columns for the lower and upper values of the limits for the interval 
                 with names that begin with \code{lower} and \code{upper}, 
                 respectively. The second part of the name must be one of 
                 \code{Confidence}, \code{StandardError} or 
                 \code{halfLeastSignificant}. The last part needs to be consistent 
                 between the lower and upper limits.}
 \item{classify}{A character string giving the combinations of the independent 
                 variables on which the predictions are based. It is 
                 an interaction type term formed from the independent variables, 
                 that is, separating the variable names with the \code{:} operator. 
                 To predict the overall mean, set the \code{classify} to 
                 "\code{(Intercept)}".}
 \item{y}{A character string giving the name of the variable that is to be plotted 
                 on the Y axis.}
\item{x.num}{A \code{\link{character}} string giving the name of the numeric covariate that 
              (i) is potentially included in terms in the fitted model and (ii) is the 
              x-axis variable for plots. Its values will not be converted to a \code{\link{factor}}.}
 \item{x.fac}{A character string giving the name of the factor that corresponds to 
              \code{x.num}, is potentially included in terms in the fitted model and 
              which corresponds to the x-axis variable. It should have the same 
              number of levels as the number of unique values in \code{x.num}. 
              The levels of \code{x.fac} must be in the order in which they are to 
              be plotted - if they are dates, then they should be in the form 
              yyyymmdd, which can be achieved using \code{as.Date}. However, the levels
              can be non-numeric in nature, provided that \code{x.num} is also set.}
 \item{nonx.fac.order}{A character vector giving the order in which factors other 
              than \code{x.fac} are to be plotted in faceted plots (i.e. where the 
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
              The default is \cr\code{graphics.device = NULL}, which will result 
              in plots being produced on the current graphics device. Setting it to 
              \code{"windows"}, for example,  will result in a windows graphics 
              device being  opened.}
 \item{error.intervals}{A character string indicating the type of error interval, if any, 
             to plot in order to indicate uncertainty in the results. 
             Possible values are \code{"none"}, \code{"StandardError"}, \code{"Confidence"} 
             and \code{"halfLeastSignificant"}. Here, any option other than \code{"none"} 
             will result in the interval limits contained in \code{data} being plotted. }
 \item{interval.annotate}{A \code{logical} indicating whether the plot annotation indicating the 
             type of \code{error.interval} is to be included in the plot.}
 \item{titles}{A \code{list}, each component of which is named for a column in 
               the \code{data.frame} for the \code{asreml.obj} used in making the predictions 
               and contains a \code{character string} giving a title to use 
               in output (e.g. tables and graphs). Here they will be used for 
              axis labels for nonresponse variables. For response variable labels 
              see \code{y.title}.}
 \item{y.title}{The title to be displayed on the y axis of any plot.} 
 \item{filestem}{A character sting giving the beginning of the name of the file in which to 
              save the plot. If \code{filestem = NULL}, the plot is not saved. The remainder 
              of the file name will be generated automatically and consists of the 
              following elements separated by full stops: the classify term, 
              \code{Bar} or \code{Line} and, if \code{error.intervals} 
              is not \code{"none"}, one of \code{SE}, \code{CI} or \code{LSI}. The
              file will be saved as a `png' file in the current work directory.}
 \item{printPlot}{A \code{logical} indicating whether or not the a plot is to be printed. This would 
                be used when just the returned \code{ggplot} object is required.}
 \item{ggplotFuncs}{A \code{\link{list}}, each element of which contains the 
                results of evaluating a \code{ggplot2} function. 
                It is created by calling the \code{\link{list}} function with 
                a \code{ggplot2} function call for each element.
                These functions are applied in creating the \code{ggplot} object for plotting.}
 \item{\dots}{further arguments passed to \code{ggplot}.}
}
\value{An object of class \code{ggplot}, which can be plotted using \code{print} or otherwise manipulated.}

\author{Chris Brien}
\seealso{\code{\link{allDifferences.data.frame}}, \code{\link{predictPresent.asreml}}, 
         \code{\link{redoErrorIntervals.alldiffs}},  \cr 
         \code{\link{recalcLSD.alldiffs}}, \code{ggplot}, \code{Devices}}
\examples{\dontrun{
data(WaterRunoff.dat)
asreml.options(keep.order = TRUE) #required for asreml-R4 only
current.asr <- asreml(fixed = log.Turbidity ~ Benches + Sources + Type + Species +
                                Sources:Type + Sources:Species + 
                                Sources:xDay + Species:xDay + Species:Date,
                      data = WaterRunoff.dat, keep.order = TRUE)
current.asrt <- as.asrtests(current.asr, NULL, NULL)

#### Get the observed combinations of the factors and variables in classify
class.facs <- c("Species","Date","xDay")
levs <- as.data.frame(table(WaterRunoff.dat[class.facs]))
levs <- as.list(levs[levs$Freq != 0, class.facs])
levs$xDay <- as.numfac(levs$xDay)

predictions <- predict(current.asr, classify="Species:Date:xDay", 
                       parallel = TRUE, levels = levs, 
                       present = c("Type","Species","Sources"))
#### for asreml-R3
predictions <- predictions$predictions$pvals
predictions <- predictions[predictions$est.status == "Estimable",]
#### for asreml-R4
predictions <- predictions$pvals
predictions <- predictions[predictions$status == "Estimable",]
#### end
plotPredictions(classify="Species:Date:xDay", y = "predicted.value", 
                data = predictions, 
                x.num = "xDay", x.fac = "Date", 
                x.title = "Days since first observation",
                y.title = "Predicted log(Turbidity)",
                present = c("Type","Species","Sources"),
                error.intervals = "none",
                ggplotFuncs = list(ggtitle("Transformed turbidity over time")))
  
diffs <- predictPlus(classify="Species:Date:xDay", 
                     present=c("Type","Species","Sources"), 
                     asreml.obj = current.asr, tables = "none", 
                     x.num = "xDay", x.fac = "Date", 
                     parallel = TRUE, levels = levs, 
                     x.plot.values=c(0,28,56,84),
                     wald.tab = current.asrt$wald.tab)
x.title <- "Days since first observation"
names(x.title) <- "xDay"
plotPredictions(classify="Species:Date:xDay", y = "predicted.value", 
               data = diffs$predictions, 
               x.num = "xDay", x.fac = "Date", 
               titles = x.title,
               y.title = "Predicted log(Turbidity)")
}

## Use lmerTest and emmmeans to get predictions and associated statistics
if (requireNamespace("lmerTest", quietly = TRUE) & 
    requireNamespace("emmeans", quietly = TRUE))
{
  data(Ladybird.dat)
  m1.lmer <- lmerTest::lmer(logitP ~ Host*Cadavers*Ladybird + (1|Run),
                            data=Ladybird.dat)
  HCL.emm <- emmeans::emmeans(m1.lmer, specs = ~ Host:Cadavers:Ladybird)
  HCL.preds <- summary(HCL.emm)
  den.df <- min(HCL.preds$df)
  ## Modify HCL.preds to be compatible with a predictions.frame
  HCL.preds <- as.predictions.frame(HCL.preds, predictions = "emmean", 
                                   se = "SE", interval.type = "CI", 
                                   interval.names = c("lower.CL", "upper.CL"))
  ## Plot the predictions
  plotPredictions(HCL.preds, y = "predicted.value", "Host:Cadavers:Ladybird")
}
}
\keyword{asreml}