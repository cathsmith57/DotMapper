# DotMapper

DotMapper is an [R Shiny](https://shiny.rstudio.com/) app.

- App is described in [Smith CM and Hayward AC (2016). DotMapper: an open source tool for creating interactive disease point maps. BMC Infectious Diseases 16:145. doi:10.1186/s12879-016-1475-5](https://bmcinfectdis.biomedcentral.com/articles/10.1186/s12879-016-1475-5)

- [User guide](https://github.com/cathsmith57/DotMapper/blob/master/UserGuide.md)

## Features
- Plot locations of cases and (optionally) associated venues of interest.
- Plot points colour coded according to any categorical variable.
- Interactively display subsets of data according to multiple variables.
- Select points by date using slider.
- Display key details of individual cases or venues by clicking on points. 
- Display summary table of key statistics.
- Display epidemic curve by year, quarter, month, week or day.


## Running the app

To use DotMapper, you need to have [R](https://www.r-project.org/) installed on your machine. 

You then need to install the Shiny package: ```install.pacakges("shiny")```, and load it: ```library(shiny)```.

Then, run the app using the R command: 


```runGitHub("DotMapper", "cathsmith57", launch.browser=T)```


