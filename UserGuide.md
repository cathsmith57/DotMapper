# DotMapper User Guide

## Install packages
Make sure that the following packages are installed using install.packages("package_name")

Running app and generating interactive maps and plots:
- shiny
- leaflet
- RcolorBrewer
- ggplot2

Aggregating data and producing summary tables:
- plyr

Formatting dates:
- lubridate
- zoo
- epitools

Reshaping data:
- tidyr
- reshape2

Geocoding points:
- ggmap 


## Data requirements
DotMapper uses two data files, examples of which are provided in the [Example data folder](https://github.com/cathsmith57/DotMapper/tree/master/Example%20data):

### cases 
Details of cases in clusters, using one row per individual. Columns are variables of four types:

- Plotting variables 
	- These are the characteristics of the cases that will be used to colour code plotted points, for example age, sex, ethnicity, risk factors. 
	- Must be formatted as **factors**.
	- Must be the first columns in the data set.
	-- Variables can have any names but must have no spaces (e.g. "AgeGroup" instead of "Age Group")
- IDs 
	- *id* is the unique identifier for each case. 
	- *clusterid* is the name of the cluster. DotMapper can be used to display one or multiple clusters.
- Dates 
	- *date* is the date of case report or notification. 
	- Must be formatted day/month/year, for example "20/03/2015".
- Geographic information 
	- *lon* and *lat* are the longitude and latitude of case locations. 
	- Alternatively, *loc* is a character string specifying a location, such as postcode (uses geocode function from ggmap package to generate lon and lat columns).

### venues (optional)
Locations of contextual venues, such as clinics, food outlets, etc. Columns are variables of two types:

- IDs
	- *id* is the unique identifier of each location. These must be different to the IDs used for cases.
	- *name* is the name of the location.
	- *type* is the type of location.
- Geographic information 
	- *lon* and *lat* are the longitude and latitude of venue locations. 
	- Alternatively, *loc* is a character string specifying a location, such as postcode (uses geocode function from ggmap package to generate lon and lat columns).


**NB: ID, date and geographic information columns must be named as stated in *italics* above.**

**Missing data should be coded as NA.**

## Running App


1. Create a new folder to act as your working directory.
2. Download the [FormatData.R](https://github.com/cathsmith57/DotMapper/blob/master/FormatData.R) script, and the [App](https://github.com/cathsmith57/DotMapper/tree/master/App) folder and save in your working directory.
3. Open FormatData.R in R and locate the *Specify data parameters* section. There is one value that has to be set by the user:
	- plVar - number of *Plotting variables* in the case data. For example, if you wish to be able to plot Age, Sex and Ethnicity, these are the first three columns in the cases data set, and plVar should be set to 3. In the example data set there are seven plotting variables, so plVar = 7.
4. Run FormatData.R script. This will format the data and then run the shiny app.

## Notes

- If coordinates are not valid or location names fail to geocode, data for these cases will be included in summary tables and epidemic curves, but cases will not be plotted.


