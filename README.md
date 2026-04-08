# NTS AVT Survey

Data dashboard for the results of ancient, veteran and notable tree surveys on seven National Trust for Scotland properties 2025-26. 

This is a Shiny app which displays an interactive Leaflet map of the survey results, an interactive datatable of the AVT results with the option to download the dataset as a .csv file, and several interactive graphs introducing the data. 

The published application can be found [here](https://cleslie.shinyapps.io/NTS_AVT_Output/).

## Use

Run datacleaning.R to process the raw results in the /data folder. The processed results are saved in the /output subfolder. 

Then run ntsoutput7.R. This R script is dependent on the gomap.js and styles.css scripts. 

## Important note

The application depends on a large number of images which are not included in this repository. However, these are included in the [published application](https://cleslie.shinyapps.io/NTS_AVT_Output/) hosted at shinyapps.io. 

## Licensing

Survey results in /data folder copyright National Trust for Scotland 2026

All other code copyleft under [GNU GPL 3.0 (or later)](https://www.gnu.org/licenses/gpl-3.0.txt)

Contact: calum.leslie@pm.me
