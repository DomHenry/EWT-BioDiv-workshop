# EWT biodiversity data analysis workshop

Materials for the EWT Biodiversity Data Analysis workshop held at the University of the Western Cape from 30th October to 1st November 2019.

In order to run all the workshop exercise on your local machine you'll need to run the following R code to install the necessary packages:  

`install.packages(c("tidyverse","sf","rJava","dismo",
"raster","lwgeom","usdm","gridExtra","latticeExtra","rasterVis"))`

To set up MaxEnt first download the [maxent.jar](https://biodiversityinformatics.amnh.org/open_source/maxent/) file. Then move the  `maxent.jar` file into the `java` folder of the `dismo` package. That is the folder returned by running this command in R: `system.file("java", package="dismo")`.