\name{geomerge}
             zonal.fun=sum, assignment="max(area)",population.data = NA,
             point.agg = "cnt",t_unit="days",silent=FALSE)

\code{geomerge} assumes that all inputs of \code{SpatialPolygonsDataFrame} and \code{RasterLayer} are static and contemporary. If polygons or raster are changing, we advise to simply rerun \code{geomerge} for each interval in which data are static and contemporary. The package allows for dynamic integration of all inputs that are a \code{SpatialPointsDataFrame}, i.e., one can, for example, automatically generate the counts of events that occur within a specific unit of \code{target} within a specific time period. Further details are given below.

Values for inputs of type \code{SpatialPointsDataFrame} are either calculated statically across the entire frame if \code{time = NA} or dynamically within a given time period defined by \code{time = c(start_date, end_date, interval_length)}. Default for \code{interval_length} is  a numerical value for number of \code{t_unit = "days"}. The package also accepts inputs of \kbd{"secs"}, \kbd{"mins"}, \kbd{"hours"}, \kbd{"months"} or \kbd{"years"}.
Returns an object of class \kbd{"geomerge"}.

The functions \code{summary}, \code{print}, \code{plot} overload the standard outputs for objects of type \code{geomerge} providing summary information and and visualizations specific to the output object. An object of class \kbd{"geomerge"} is a list containing the following three components:

\item{data}{\code{SpatialPolygonsDataFrame} that contains all information merged with the \code{target} layer. Column names are assigned the name of the input data object separated by "." from a short description of the calculation, as well as modifiers such as ".1st" and ".2nd" for first- and second-order neighborhoods of \code{target}. In the case of dynamic point data aggregation, ".t_1" and ".t_2" are used to label first- and second-order temporal lags. For example, if \code{geomerge} is told to use a \code{SpatialPointsDataFrame} called \kbd{"vio"} to count incidents of conflict contained within units of \code{target}, the default output would include columns named \kbd{"vio.cnt"}, \kbd{"vio.cnt.t_1"}, \kbd{"vio.cnt.t_2"}, \kbd{"vio.cnt.1st"}, \kbd{"vio.cnt.1st.t_1"}, \kbd{"vio.cnt.1st.t_2"}, \kbd{"vio.cnt.2nd"}, \kbd{"vio.cnt.2nd.t_1"}, \kbd{"vio.cnt.2nd.t_2"}.

\item{inputData}{List containing the spatial objects used as input.}

\item{parameters}{List containing information on all input parameters used during integration.}

}

Andrew M. Linke, Karsten Donnay. (2017). "Scale Variability Misclassification: The Impact of Spatial Resolution on Effect Estimates in the Geographic Analysis of Foreign Aid and Conflict." Paper presented at the \emph{International Studies Association Annual Meeting}, February 22-25 2017, Baltimore.
}
data(geomerge)

# 1) Simple static integration of polygon data
output <- geomerge(geoEPR,target=counties,silent=TRUE)
summary (output)
# 2) Static integration for point, polygon, raster data
ACLED.events <- ACLED[,names(ACLED)%in%c('EVENT_TYPE')]
AidData.projects <- AidData[,names(AidData)%in%c('project_id')]
output <- geomerge(ACLED.events,AidData.projects,geoEPR,
		   gpw,na.rm=TRUE,target=counties)
summary(output)
plot(output)

# 3) Dynamic point data integration for numeric variables
ACLED.fatalities <- ACLED[,names(ACLED)%in%c('timestamp','FATALITIES')]
AidData.commitment <- AidData[,names(AidData)%in%c('timestamp','commitme_1')]
output <- geomerge(ACLED.fatalities,AidData.commitment,geoEPR,
		   target=counties,time=c("2011-01-01", "2011-12-31","1"),
		   t_unit='months',point.agg='sum')
summary(output)
plot(output)

# 4) Population weighted assignment
output <- geomerge(geoEPR,target=counties,assignment='max(pop)',
		   population.data = gpw)
summary(output)
plot(output)