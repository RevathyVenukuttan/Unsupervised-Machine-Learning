#############################################################
################## DATA MANIPULATION ########################
#############################################################


# Load necessary libraries
library(tidyr)      # data manipulation
library(dplyr)      # data manipulation
library(ggplot2)    # plotting images 
library(eegkit)     # Fourier Transform 
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)  # grid graphics
library(grid)       # grid graphics
library(lattice)    # grid graphics
library(clusteval)  # ARI testing 
library(dendextend) # for comparing two dendrograms
library(clusteval)  # for ARI (external validation)
library(fBasics)    # table formatting
library(kableExtra) # table formatting


# Set up directory
setwd('/Users/milenaradoman/Dropbox/Final Project Machine Learning/Final Paper/Final Code/')

# Read csv file
Epilepsy.Data <- read.csv('data_internal.csv')

Epilepsy.Data$Block<-factor(Epilepsy.Data$Block,
                            levels = c("1","3"),
                            labels = c("Seizure","Healthy Area"))

# Convert wide to long format
Epilepsy.Data.Long <- Epilepsy.Data %>% gather(TimePoint, Voltage, X1:X178)

# Order the file by block and chunk
Epilepsy.Data.Long.Ordered <- Epilepsy.Data.Long[order(Epilepsy.Data.Long$subID,Epilepsy.Data.Long$Chunk),]

# Add a new variable called Time 
Epilepsy.Data.Long.Ordered$Time <- rep(1:4094)

# Remove columns "chunk" and "timepoint" 
Epilepsy.Data.Long.Ordered <- Epilepsy.Data.Long.Ordered[-c(1,4)]

# Order the dataset by Block and Time 
Epilepsy.Data.Long.Ordered <- Epilepsy.Data.Long.Ordered[order(Epilepsy.Data.Long.Ordered$subID, 
                                                               Epilepsy.Data.Long.Ordered$Time),]

# Write the file in csv format 
write.csv(Epilepsy.Data.Long.Ordered, file = "Epilepsy_Data_Long.csv")


#############################################################
###################### PLOTTING DATA ########################
#############################################################


#Plotting exemplary data (a single subject per block; entire time series)
Epilepsy.Data <- read.csv('Exemplary_Data.csv')

Epilepsy.Data.Plot <- ggplot(data=Epilepsy.Data, aes(x=Time, y=Voltage, group=Block)) +
  geom_line()+
  xlab('Time [ms]')+
  ylab('Voltage [uV]')+
  theme_bw()+
  facet_grid(facets = Block ~.)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),        
        legend.title = element_blank(),
        text=element_text(family="Times", face="bold", size=14),
        legend.text = element_text(family = "Times New Roman",
                                   face="bold", size = 10))

Epilepsy.Data.Plot

ggsave("Exemplary_Data.png")


#Plotting exemplary data (a single subject per block; subset of time series; 1000ms)
Epilepsy.Data.Subset <- read.csv('Exemplary_Data_Subset.csv')

Epilepsy.Data.SubsetPlot <- ggplot(data=Epilepsy.Data.Subset, aes(x=Time, y=Voltage, group=Block)) +
  geom_line()+
  xlab('Time [ms]')+
  ylab('Voltage [uV]')+
  theme_bw()+
  facet_grid(facets = Block ~.)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),        
        legend.title = element_blank(),
        text=element_text(family="Times", face="bold", size=14),
        legend.text = element_text(family = "Times New Roman",
                                   face="bold", size = 10))

Epilepsy.Data.SubsetPlot

ggsave("Exemplary_Data.Subset.png")
