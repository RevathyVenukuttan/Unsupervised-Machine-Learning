#############################################################
##################### FOURIER TRANSFORM #####################
#############################################################


# Change from Long to Wide format
Epilepsy.Data.Wide.Ordered <- Epilepsy.Data.Long.Ordered %>% spread(Time, Voltage)

#Create a composite variable name (Block + subID)
Epilepsy.Data.Wide.Ordered$BlockID <- paste(Epilepsy.Data.Wide.Ordered$Block,Epilepsy.Data.Wide.Ordered$subID)
Epilepsy.Data.Wide.Ordered <- Epilepsy.Data.Wide.Ordered[-c(1:2)]

#Move Group to the front
Epilepsy.Data.Wide.Ordered = Epilepsy.Data.Wide.Ordered[,c(ncol(Epilepsy.Data.Wide.Ordered),1:(ncol(Epilepsy.Data.Wide.Ordered)-1))]

#Label the rows
rownames(Epilepsy.Data.Wide.Ordered) <- Epilepsy.Data.Wide.Ordered[,1]

#Remove the composite variable name (Block + subID)
Epilepsy.Data.Wide.Ordered <- Epilepsy.Data.Wide.Ordered[-c(1)]

# Write the file in csv format 
write.csv(Epilepsy.Data.Wide.Ordered, file = "Epilepsy_Data_Wide.csv")

# Transpose the dataset to get it ready for Fourier Transform (cols are subIDs, rows are timepoints)
Epilepsy.Data.Wide.Ordered.Transposed <- t(Epilepsy.Data.Wide.Ordered)

# Different frequency ranges

# DELTA (0-4Hz)
output_delta <- eegfft(Epilepsy.Data.Wide.Ordered.Transposed,178,0,4)

# THETA (4-8Hz)
output_theta <- eegfft(Epilepsy.Data.Wide.Ordered.Transposed,178,4,8)

# ALPHA (8-16Hz)
output_alpha <- eegfft(Epilepsy.Data.Wide.Ordered.Transposed,178,8,16)

# BETA (16-32Hz)
output_beta <- eegfft(Epilepsy.Data.Wide.Ordered.Transposed,178,16,32)

# GAMMA (32-64Hz)
output_gamma <- eegfft(Epilepsy.Data.Wide.Ordered.Transposed,178,32,64)


#############################################################
################# PLOTS OF FOURIER TRANSFORM ################
#############################################################


# DELTA (0-4Hz)

# Getting all the frequency and amplitude values in the right format 
output.delta <- as.data.frame(output_delta$strength)
output.delta.long <- output.delta %>% gather(BlockID, Amplitude, 1:198)
output.delta.long$Frequency <- rep(output_delta$frequency)
write.csv(output.delta.long, "output_delta_long.csv")

# THETA (4-8Hz) 

# Getting all the frequency and amplitude values in the right format 
output.theta <- as.data.frame(output_theta$strength)
output.theta.long <- output.theta %>% gather(BlockID, Amplitude, 1:198)
output.theta.long$Frequency <- rep(output_theta$frequency)
write.csv(output.theta.long, "output_theta_long.csv")

# ALPHA (8-16Hz) 

# Getting all the frequency and amplitude values in the right format 
output.alpha <- as.data.frame(output_alpha$strength)
output.alpha.long <- output.alpha %>% gather(BlockID, Amplitude, 1:198)
output.alpha.long$Frequency <- rep(output_alpha$frequency)
write.csv(output.alpha.long, "output_alpha_long.csv")

# BETA (16-32Hz) 

# Getting all the frequency and amplitude values in the right format 
output.beta <- as.data.frame(output_beta$strength)
output.beta.long <- output.beta %>% gather(BlockID, Amplitude, 1:198)
output.beta.long$Frequency <- rep(output_beta$frequency)
write.csv(output.beta.long, "output_beta_long.csv")


# GAMMA (32-64Hz) 

# Getting all the frequency and amplitude values in the right format 
output.gamma <- as.data.frame(output_gamma$strength)
output.gamma.long <- output.gamma %>% gather(BlockID, Amplitude, 1:297)
output.gamma.long$Frequency <- rep(output_gamma$frequency)
write.csv(output.gamma.long, "output_gamma_long.csv")


# FOURIER GRAPH

fourier.output.graph <- read.csv("output_wave_long_graph.csv")

fourier.output.graph <- ggplot(data=fourier.output.graph, aes(x=Frequency, y=Amplitude, group=Block)) +
  geom_line()+
  xlab('Frequency [Hz]')+
  ylab('Amplitude')+
  scale_y_continuous(limits = c(0,200)) +
  theme_bw()+
  facet_grid(facets = Block ~ Wave, scales = "free")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),        
        legend.title = element_blank(),
        text=element_text(family="Times", face="bold", size=14),
        legend.text = element_text(family = "Times New Roman",
                                   face="bold", size = 10))

fourier.output.graph

ggsave("FourierGraph.png")
