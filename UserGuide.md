# DotMapper User Guide

## Installing the app


To use DotMapper, you need to have [R](https://www.r-project.org/) installed on your machine. 

You then need to install the Shiny package: ```install.pacakges("shiny")```, and load it: ```library(shiny)```.

Then, run the app using the R command: 


```runGitHub("DotMapper", "cathsmith57", launch.browser=T)```

The application is also hosted on the shinyapps.io server: [https://cathsmith57.shinyapps.io/DotMapper/](https://cathsmith57.shinyapps.io/DotMapper/). Please note that this is intended for demonstration purposes only and should not be used for upload of sensitive data. 

## Data sets

DotMapper uses two data files, cases and venues. They must be saved as .csv files with variable names in column headers and one row per individual case or venue.

### Cases 

Details of cases of disease, using one row per individual. Must include:

- Unique patient identifier. 
- Date of case report or notification, formatted as dd/mm/yyy (e.g. 25/05/2005).
- Latitude and lohgitude of case location.

Optionally, it can also include characteristics of cases (e.g. age, sex, risk factors). These must be categorical variables. 

**Missing data should be coded as NA.**

### Venues (optional)

Locations of contextual venues, such as clinics, food outlets, etc. If used, must contain:

- Unique venue identifier.
- Name of venue.
- Type of venue.
- Latitude and longitude of venue.

### Example data

[Example data sets](https://github.com/cathsmith57/DotMapper/tree/master/Example%20data), based on dummy daat, can be used to run the application. 

## Running the app

### 1. Load data

Click 'Browse' and navigate to the file stored on your computer.

### 2. Preview data

When you have loaded a file, it will appear in the 'Preview data' section. 

### 3. Identify variables

Use the drop-down lists to identify the columns in your data set that include the key variables to run the app. When they are selected, click 'Go' to view the interactive visualisations.

## Using the app

The app has four tabs: Data input; Map; Table and Bar chart. 

Controls have options for:

- Filtering data according to time period and any categorical variables included. 
- Changing the time period that data are grouped into (day, month, quarter, year).
- Displaying contextual venues on the map. 
- Changing the size of the bar chart. 


## Notes

- If coordinates are not valid, data for these cases will be included in summary tables and epidemic curves, but cases will not be plotted.
- This app uses the R packages: Leaflet, dplyr, tidyr, lubridate, zoo, epitools, shinyjs, shinydashboard, RColorBrewer, ggplot2.
- More information can be found in in [Smith CM and Hayward AC (2016). DotMapper: an open source tool for creating interactive disease point maps. BMC Infectious Diseases 16:145. doi:10.1186/s12879-016-1475-5](https://bmcinfectdis.biomedcentral.com/articles/10.1186/s12879-016-1475-5)

