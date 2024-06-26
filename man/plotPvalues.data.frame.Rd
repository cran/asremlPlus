\name{plotPvalues.data.frame}
\alias{plotPvalues.data.frame}
\title{Plots a heat map of p-values for pairwise differences between predictions.}
\description{Produces a heat-map plot of the p-values for pairwise differences between 
             predictions that is in a \code{data.frame}. The \code{data.frame} includes 
             two factors whose levels specify, for each p-value, which factor levels are 
             being compared.}
\usage{\method{plotPvalues}{data.frame}(object, p = "p", x, y, 
            gridspacing = 0, show.sig = FALSE, alpha = 0.10, 
            sig.size = 3, sig.colour = "black", 
            sig.face = "plain", sig.family = "",  
            triangles = "both", 
            title = NULL, axis.labels = NULL, axis.text.size = 12, 
            colours = RColorBrewer::brewer.pal(3, "Set2"), 
            ggplotFuncs = NULL, printPlot = TRUE, ...)}
\arguments{
 \item{object}{A \code{data.frame} containing the three columns specified by \code{p}, \code{x} 
             and \code{y}.}
 \item{p}{A \code{character} giving the name of the column in \code{object} that contains
             the p-values to be plotted.}
 \item{x}{A \code{character} giving the name of the column in \code{object} that contains 
             the factor whose levels index the p-values that are to be plotted in the same column.}
 \item{y}{A \code{character} giving the name of the column in \code{object} that contains 
             the labels of the p-values that are to be plotted as the rows.}
 \item{gridspacing}{A \code{numeric} specifying the number(s) of rows and columns 
             that form groups in the grid of differences. This is most useful when  
             two or more factors index the rows and columns. If a single, nonzero 
             number, \emph{k} say, is given then a grid line is placed after every 
             \emph{k}th  row and column. If a vector of values is given then the 
             number of grid lines is the length of the vector and the spacing between 
             each is specified by the elements of the vector.}
 \item{show.sig}{A \code{logical} that specifies whether asterisks indicating the level 
             of significance are to be added to the plot. If they are then 
             `***' indicates that \eqn{p \leq 0.001}{p <= 0.001}, 
             `**' that \eqn{ 0.001 < p \leq 0.01}{0.001 < p <= 0.01}, 
             `*' that \eqn{ 0.01 < p \leq 0.05}{0.01 < p <= 0.05}
             `.' that \eqn{ 0.05 < p \leq 0.10}{0.05 < p <= 0.10}. The last is only 
             included for \code{alpha} = 0.10.}
 \item{alpha}{A \code{\link{numeric}} giving the significance level for testing 
             pairwise differences; must be 0.05 or 0.10.}
 \item{sig.size}{A \code{\link{numeric}} specifying the size, in pts, of the 
             significance asterisks.}
 \item{sig.colour}{A \code{\link{character}} specifying the colour to use for the 
             significance asterisks.}
 \item{sig.face}{A \code{\link{character}} specifying the font face for the significance 
             asterisks (\code{"plain"}, \code{"italic"}, \code{"bold"}, \code{"bold.italic"}).}
 \item{sig.family}{A \code{\link{character}} specifying the font family for the significance 
             asterisks. The font families that are available depends on the system. For font 
             families other than the basic Postscript fonts, see the \code{extrafont} package.}
 \item{triangles}{A \code{character} indicating whether the plot should include the 
             \code{lower}, \code{upper} or \code{both} traingle(s). Here it is only used 
              to adjust gridlines for the omission of the diagonal.}
 \item{title}{A \code{character} string giving the main title for the plot.}
 \item{axis.labels}{A \code{character} string giving the label to use for both the x- and y-axis.}
 \item{axis.text.size}{A \code{\link{numeric}} giving the size of the \code{labels} on the axes   
                  of the \code{heatmap}.}
 \item{colours}{A vector of of colours to be passed to the \code{ggplot} function 
                \code{scale\_colour\_gradientn}.}
 \item{ggplotFuncs}{A \code{\link{list}}, each element of which contains the 
                results of evaluating a \code{ggplot2} function. 
                It is created by calling the \code{\link{list}} function with 
                a \code{ggplot2} function call for each element. 
                These functions are applied in creating the \code{ggplot} object.}
 \item{printPlot}{A \code{logical} indicating whether or not the a plot is to be printed. This would 
                be used when just the returned \code{ggplot} object is required.}
 \item{\dots}{Provision for passsing arguments to functions called internally - 
            not used at present.}
 }
\value{An object of class "\code{ggplot}", which can be plotted using \code{print} or otherwise manipulated.}
\author{Chris Brien}
\seealso{\code{\link{plotPvalues.alldiffs}}, \code{\link{allDifferences.data.frame}}, \code{ggplot}}
\examples{
##Subset WaterRunoff data to reduce time to execute
data(WaterRunoff.dat)
tmp <- subset(WaterRunoff.dat, Date == "05-18")

##Use asreml to get predictions and associated statistics

\dontrun{
asreml.options(keep.order = TRUE) #required for asreml-R4 only
current.asr <- asreml(fixed = pH ~ Benches + (Sources * (Type + Species)), 
                      random = ~ Benches:MainPlots,
                      keep.order=TRUE, data= tmp))
current.asrt <- as.asrtests(current.asr, NULL, NULL)
SS.diffs <- predictPlus.asreml(classify = "Sources:Type", 
                               asreml.obj = current.asr, tables = "none", 
                               wald.tab = current.asrt$wald.tab, 
                               present = c("Type","Species","Sources"))
}

## Use lmeTest and emmmeans to get predictions and associated statistics

if (requireNamespace("lmerTest", quietly = TRUE) & 
    requireNamespace("emmeans", quietly = TRUE))
{
  m1.lmer <- lmerTest::lmer(pH ~ Benches + (Sources * (Type + Species)) + 
                              (1|Benches:MainPlots),
                            data=na.omit(WaterRunoff.dat))
  TS.emm <- emmeans::emmeans(m1.lmer, specs = ~ Sources:Type)
  TS.preds <- summary(TS.emm)
  den.df <- min(TS.preds$df, na.rm = TRUE)
  ## Modify TS.preds to be compatible with a predictions.frame
  TS.preds <- as.predictions.frame(TS.preds, predictions = "emmean", 
                                   se = "SE", interval.type = "CI", 
                                   interval.names = c("lower.CL", "upper.CL"))
   
  ## Form an all.diffs object and check its validity
  TS.vcov <- vcov(TS.emm)
  TS.diffs <- allDifferences(predictions = TS.preds, classify = "Sources:Type", 
                             vcov = TS.vcov, tdf = den.df)
  validAlldiffs(TS.diffs)
}  

## Plot p-values for predictions obtained using asreml or lmerTest
if (exists("TS.diffs"))
{
  p <- within(reshape2::melt(TS.diffs$p.differences), 
              { 
                Var1 <- factor(Var1, levels=dimnames(TS.diffs$p.differences)[[1]])
                Var2 <- factor(Var2, levels=levels(Var1))
              })
  names(p) <- c("Rows","Columns","p")
  plotPvalues(p, x = "Rows", y = "Columns", 
              gridspacing = rep(c(3,4), c(4,2)), show.sig = TRUE)
}
}
\keyword{hplot}
\keyword{dplot}
\keyword{asreml}
