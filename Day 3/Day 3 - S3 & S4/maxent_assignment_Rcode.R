# Solutions

# Load the packages required for the analysis:  
library(tidyverse)
library(dismo)
library(raster)
library(sp)
library(sf)

# Create a CRS string in geographic and UTM projection.  

latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
utmproj <- "+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs"

# Read in the species occurrence csv file (Pachydactylus geitje.csv) and convert it to a simple feature object. Convert the spatial object to UTM projection. You will have to specify the names of the columns containing the coordinate data. You can do this step using the pipe operator or by creating each object one at a time.  

spp_occ_all <- read_csv("data/spp_data/Pachydactylus geitje.csv") %>% 
  st_as_sf(., coords = c("decimallon", "decimallat"), crs = latlongCRS) %>% 
  st_transform(crs = utmproj)

# **Q1:** Print out the column names of the sf object. How many fields does the occurrence data object have? Use the length() function.  

names(spp_occ_all)
length(names(spp_occ_all))


# Using dplyr::rename, rename the following columns: common_nam to common_name; taxon_name to scientific_name; Year to year and; red_data_s to red_list_status.  
spp_occ_all <- spp_occ_all %>% 
  rename(common_name = common_nam,
         scientific_name = taxon_name,
         red_list_status = red_data_s,
         year = Year)

# **Q2:** Print out the column names again to confirm changes  
names(spp_occ_all)

# There are a number of points which have duplicates geographical coordinates. Remove the duplicates using dplyr::distinct(geometry, .keep_all = TRUE) so that the resulting object contains unique spatial occurrence records. 
spp_occ_all <- spp_occ_all %>% 
  distinct(geometry, .keep_all = TRUE)

# **Q3:** How many points were removed from the dataset?  
728 - 387

# Using unique() check if there are any spelling mistakes in the species name (*Pachydactylus geitje*).  
unique(spp_occ_all$scientific_name)

# **Q4:** Using which(), correct any spelling mistakes and re-run unique to confirm corrections. 
which(spp_occ_all$scientific_name == "Pachydactyylus geitje")
which(spp_occ_all$scientific_name == "Pachydactylus getje")

spp_occ_all$scientific_name[3] <- "Pachydactylus geitje"
spp_occ_all$scientific_name[59] <- "Pachydactylus geitje"

unique(spp_occ_all$scientific_name)

# The occurrence data have been collected over a number of years and some records do not have any date information.   

# **Q5:** Check the unique values of the year variable.  
unique(spp_occ_all$year)

# **Q6:** Using table() check the frequency of each year, how many year values are equal to 0?  
table(spp_occ_all$year)

# **Q7:** Using is.na(), check how many year values are NA.  
length(which(is.na(spp_occ_all$year)))

# Filter the spatial occurrence dataset to remove 0 values, remove NA values and keep only records collected in 1960 or later. Create a new object with the filtered data set.  
spp_occ <- spp_occ_all %>% 
  filter(year != 0) %>% 
  filter(!is.na(year)) %>% 
  filter(year >= 1960) 

# **Q8:**  How many rows were removed during the filtering process (use nrow())?  
nrow(spp_occ_all) - nrow(spp_occ)

# **Q9:**  Print the year column as a vector to look for any obvious mistakes.   
spp_occ$year

# **Q10:**  Using ggplot() create a histograms of the year and month variables. Customise the axis labels, plot title, binwidth, color and fill for each plot. Write both plots to PDF (remember to use dev.off()).  
p1 <- spp_occ %>% 
  ggplot(aes(x = year))+
  geom_histogram(binwidth = 1, fill = "white", col = "black")+
  labs(x = "Observation year",
       y = "Frequency",
       title = "Histogram of dates for Ocellated Gecko records")

p2 <- spp_occ %>% 
  ggplot(aes(x = month))+
  geom_histogram(binwidth = 1, fill = "white", col = "black")+
  labs(x = "Observation month",
       y = "Frequency",
       title = "Histogram of dates for Ocellated Gecko records")

pdf("data/year_historgam.pdf")
p1
dev.off()

pdf("data/month_historgam.pdf")
p2
dev.off()

# Import the Western Cape provincial shapefile and project the data sot that the layers matches the coordinate reference system of the occurrence points (UTM projection).
wc <- sf::st_read("data/Western_Cape.shp", crs = latlongCRS) %>% 
  st_transform(crs = utmproj)

# **Q11:**  Create a plot of the occurrence points overlaid onto the provincial shapefile using ggplot. Add a plot title and customise the size and color of occurrence points and change the theme to black and white. Write the plot to PDF.   
p3 <- ggplot()+
  geom_sf(data = wc)+
  geom_sf(data = spp_occ, size = 1.2, col = "blue")+
  theme_bw()+
  labs(title = "Occurrence points of Pachydactylus geitje")

pdf("data/occ_points_map.pdf")
p3
dev.off()

# Using dir(), create a list of all the environmental rasters in the environmenal_data folder. Import all the rasters into a single RasterStack. Use the projectRaster function to convert the raster stack to UTM projection
env_list <- dir("data/environmental_data/", pattern = ".tif", full.names = TRUE)
env_stack <- raster::stack(env_list)
env_stack <- projectRaster(env_stack, crs = utmproj)

# **Q12:**  Plot the raster stack.  
plot(env_stack)

# **Q13:**  Print out the names of each raster layer using names().    
names(env_stack)

# Using usdm::vifcor, reduce collinearity in the predictor variables (specify your own threshold).  
var_reduced <- usdm::vifcor(env_stack, th=0.6) 

# **Q14:**  Print the results of the step above. Which variables have been retained?  
var_reduced

# Using usdm::exclude, exclude the collinear variables identified in the previous step from the RasterStack.  
env_stack<- usdm::exclude(env_stack,var_reduced)

# **Q15:** Re-plot the raster data  
plot(env_stack)

# Create a sample of 10 000 background points from the RSA provincial shapefile.
n_points <- 10000
bck_points <- st_sample(wc, n_points) %>% 
  st_sf()

# **Q16:** Plot the background points and presence points on the RSA map using ggplot(). Use different colors for the different classes of points. Adjust the size of the background points to improve the plot aesthetics.
ggplot()+
  geom_sf(data = wc, size = 2)+
  geom_sf(data = bck_points, size = 0.7)+
  geom_sf(data = spp_occ, col = "red")

# Build a Maxent SDM using 10% of the occurrence points for model testing. Remember that sf objects need to be converted to SpatialPointsDataFrame objects prior to inclusion into maxent models. Customise the output path for the maxent model results.  

## Withold a 10% sample of occ points for testing the model
fold <- kfold(spp_occ, k=10)
occtest <- spp_occ[fold == 1, ]
occtrain <- spp_occ[fold != 1, ]

# Convert to spatialpointsdataframe
occtest <- as(occtest, "Spatial")
occtrain <- as(occtrain, "Spatial")
bck_points <- as(bck_points, "Spatial")

# Run maxent 
spp_ME <- dismo::maxent(
  x = env_stack, 
  p = occtrain,  
  a = bck_points,
  removeDuplicates=TRUE, 
  path = paste0(getwd(),"/data/SDM results"), 
  args = c("randomtestpoints=30", "betamultiplier=1", 
           "linear=true", 
           "quadratic=true", "product=true", "threshold=true", 
           "hinge=true", "threads=2", "responsecurves=true", 
           "jackknife=true","askoverwrite=false"))

# Download the maxent model results folder and open the maxent HTML file to view the results.  
# **Q17:**  What is the AUC of the training data?  
# **Q18:**  What are the top 3 predictor variables in terms of their proportional contribution?    
# **Q19:**  What is the shape of the response curves of these variables?   
  
# Use the testing data to evaluate the performance of the model and assign the results to an object.  
eval_me <- dismo::evaluate(spp_ME, x = env_stack, p = occtest, a = bck_points)

# **Q20:** How many presence records are included in the testing data set?  
# **Q21:**  What is the AUC value of the test data set?  
# **Q22:**  What is the maxSSS threshold?  
eval_me

# **Q23:**  Create a plot of the AUC curve.  
plot(eval_me, "ROC")

# Assign the maxSSS value to an object  
threshold(eval_me)
mSSS <- threshold(eval_me)[["spec_sens"]]

# Create a predictive SDM map across the geographical extent of environmental RasterStack.   
spp_predict <- dismo::predict(spp_ME, env_stack, progress = "text") 

# **Q24:**  Plot the SDM map and customise the title. Write the plot to PDF.  
p4 <- plot(spp_predict, main = "Pachydactylus geitje SDM")

pdf("data/SDM results/spp_occSDM.pdf")
p4
dev.off()

# Create another plot and add the training and testing presence points to the map (each in a different color).  
# **Q25:**  Save the map to PDF.  

pdf("data/SDM results/spp_occSDM_2.pdf")
plot(spp_predict, main = "Pachydactylus geitje SDM")
plot(occtrain, pch = 16, cex = 0.5, add = TRUE)
plot(occtest, pch = 16, cex = 1, add = TRUE, col = "red")
dev.off()

# Create a binary predictive map using the mSSS value.  
mSSS_matrix <- matrix(c(0,mSSS,0,
                        mSSS,1,1), ncol=3, byrow=TRUE)
binary_prediction <- reclassify(spp_predict, mSSS_matrix)

# **Q26:** Plot the binary map, customise the title and write it to PDF.  
plot(binary_prediction, main= "Pachydactylus geitje binary SDM map")

# **Q27:** Write the predictive raster in a TIFF format and workspace to the results directory.  
raster::writeRaster(spp_predict, 
                    filename = ("data/SDM results/spp_occSDM.tif"), 
                    format="GTiff", 
                    overwrite=TRUE) 
save.image("data/SDM results/SDM_workspace.RData")
