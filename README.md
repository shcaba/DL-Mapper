# The data-limited mapping tool (DLMapper)

A tool to help articulate your "data-limited" situation.
Explore the data and resource constraints that affect your support of fisheries management
Compare and constrast to other fisheries to understand how your limitations relate to others.

Please go to the online tool at:
https://connect.fisheries.noaa.gov/DLMapper/

If using it offline, download the repository and be sure to install the following libraries:

```R
packages<-c("devtools","shiny","ggplot2","reshape2","dplyr","viridis","plotly","GGally","gghighlight")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

Running the tool can be accomplished in any of the following ways:
1) shiny::runApp(ENTER HERE USER PATH TO FOLDER CONTAINING THE SS-DL files)
2) Open the server.r or ui.r files in RStudio and push the "Run App" button (top rigt corner of the source panel). 
	I recommend using the "Run External" option within the "Run App" button (see small arrow in button to change options)
3) runGitHub("DL-Mapper", "shcaba",destdir=mydir) where mydir is the path you chose to obtain results.
```
