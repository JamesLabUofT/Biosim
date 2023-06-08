#####################################
#                                   #
#   BioSIM SBW and GM Summer 2022   #
#                                   #
#####################################

#set working directory
setwd("~/James Lab/Projects/BioSIM sites")


#############
#           #
#    SBW    #
#           #
#############

# Date when loop was run
today = "4-7-22"
year = 2023
# range of confidence intervals
CI = c(66,95,99)

# Load data
  fir <- read.csv(paste("SBW_BioSIM_Output_Summer_",year,"_Fir_",today,".csv",sep=""))
  spruce <- read.csv(paste("SBW_BioSIM_Output_Summer_",year,"_Spruce_",today,".csv",sep=""))
 
# List sites
# Run twice, 1 for each and copy paste output in a text file
  restype = c("Balsam_Fir", "White_Spruce") # "Fir" or "Spruce"
  
#select lifestage of interest
 lifestage = "Pupae"

# Run Code
 {
  # Saving vector
  vec_dates = "resource site minmax CI dates"
 
  for(z in 1:length(restype)){
    if (restype[z] == "Balsam_Fir"){
      resource = fir
    }else{
      resource = spruce
    }
    
    # fir or spruce
    sites <- unique(resource$Name)
    
    
    
    # Subset each site
    
    for (m in 1: length(unique(resource$Name))){
      
      subsite <- subset(resource, Name == sites[m])
      subsite <- subset(subsite, subsite[,lifestage] != 0)
      subsite$date <-paste( subsite$Year,"-", subsite$Month,"-", subsite$Day, sep ="") 
      
      
      
      #### Build loop #adjust for column of interest
      
      
      dates_sampling <- vector(mode = "list", length = length(sites))
      
      for (l in 1: length(subsite[,lifestage]) ) {
        subsite$sum [l] = sum(subsite[,lifestage][1:l])
      }
      
      
      min =c()
      max=c()
      for(i in 1:length(CI)){
        min[i] = sum(subsite[,lifestage])*(((100-CI[i])/2)/100)
        max[i] = sum(subsite[,lifestage])*((CI[i]+((100-CI[i])/2))/100) 
        
        
        for (j in 1: length(subsite[,lifestage])){
          if(subsite$sum[j] < min[i]){
            j =j+1
          }else{
            # print(paste(restype," ",subsite$Name[j]," min date CI",CI[i],": ",subsite$date[j], sep= ""))
            tempmin = paste(restype," ",subsite$Name[j]," min_date CI",CI[i]," ",subsite$date[j], sep= "")
            vec_dates = c(vec_dates, tempmin)
            break
          }
        } 
        
        for (k in 1: length(subsite[,lifestage])){
          if(subsite$sum[k] <= max[i]){
            k =k+1
          }else{
            #print(paste(restype," ",subsite$Name[k-1]," max date CI",CI[i],": ",subsite$date[k-1], sep= ""))
            tempmax = paste(restype," ",subsite$Name[k-1]," max_date CI",CI[i]," ",subsite$date[k-1], sep= "")
            vec_dates = c(vec_dates, tempmax)
            break
          }
        }
      }
    }
  }
  tabledates = strsplit(vec_dates, split = " ")
  tabledates = as.data.frame(Reduce(rbind, tabledates))
  colnames(tabledates) = c("resource","site","minmax", "CI", "dates")
  rownames(tabledates) = 1:nrow(tabledates)
  
  write.csv(tabledates,paste("SBW_",lifestage,"_BioSIM_Summer_",year,"_dates_",today,".csv",sep=""))
  
  
  ############################
  #                          #
  #     SBW final tables     #
  #                          #
  ############################
  
  #Before loading data separate columns in excel
  
  data <- read.csv(paste("SBW_",lifestage,"_BioSIM_Summer_",year,"_dates_",today,".csv",sep=""))
  data <- data[-1,-1]
  
  resourcetype = c("Balsam_Fir","White_Spruce")
  sites = unique(data$site)
  
  #Prepare output file
  final <- vector(mode = "list", length = length(resourcetype))
  
  
  
  for(i in 1:length(resourcetype)){
    resdata <- subset(data,resource == resourcetype[i])
    
    
    #CI66
    data66 <- subset(resdata, CI == "CI66")
    min_data66 <- subset(data66, minmax == "min_date")
    max_data66 <- subset(data66, minmax == "max_date")
    
    
    matrix66 <- data.frame(matrix(0, ncol = 4, nrow = length(unique(data$site))))
    colnames(matrix66) <- c("resource","site","min66","max66")
    
    for (j in 1: length(unique(data$site))){
      matrix66[j,1] <- data66$resource[j]
      matrix66[j,2] <- sites[j]
      matrix66[j,3] <- min_data66$dates[j]
      matrix66[j,4] <- max_data66$dates[j]
    }
    
    #CI95
    data95 <- subset(resdata, CI == "CI95")
    min_data95 <- subset(data95, minmax == "min_date")
    max_data95 <- subset(data95, minmax == "max_date")
    
    
    matrix95 <- data.frame(matrix(0, ncol = 4, nrow = length(unique(data$site))))
    colnames(matrix95) <- c("resource","site","min95","max95")
    
    for (k in 1: length(unique(data$site))){
      matrix95[k,1] <- data95$resource[k]
      matrix95[k,2] <- sites[k]
      matrix95[k,3] <- min_data95$dates[k]
      matrix95[k,4] <- max_data95$dates[k]
    }
    
    #CI99
    data99 <- subset(resdata, CI == "CI99")
    min_data99 <- subset(data99, minmax == "min_date")
    max_data99 <- subset(data99, minmax == "max_date")
    
    
    matrix99 <- data.frame(matrix(0, ncol = 4, nrow = length(unique(data$site))))
    colnames(matrix99) <- c("resource","site","min99","max99")
    
    for (l in 1: length(unique(data$site))){
      matrix99[l,1] <- data99$resource[l]
      matrix99[l,2] <- sites[l]
      matrix99[l,3] <- min_data99$dates[l]
      matrix99[l,4] <- max_data99$dates[l]
    }
    final[[i]] <- cbind(matrix66,matrix95,matrix99)
  }
  
  tablefinal <- data.frame(Reduce(rbind, final))
  tablefinal <- tablefinal[,c("resource","site","Start66","End66","Start95","End95","Start99","End99")]
  
  # save file
  write.csv(tablefinal,paste("SBW_",lifestage,"_BioSIM_Summer_",year,"_Sampling_dates_",today,".csv",sep=""))
}

####################
#                  #
#    SpongyMoth    #
#                  #
####################

# today's date and year
today = "5-6-23"
year = 2023
# range of confidence intervals
CI = c(66,95,99)

# Load BioSIM data
# Make sure data is in multiple columns
# Make sure names matches
gm <- read.csv(paste("SpongyMoth_BioSIM_Output_Summer",year,"_",today,".csv",sep=""),h=T)

# Date when loop was run
#today = "7-6-22"
{
  # Adjust restype (SBW = fir and spruce, GM = GM)
  restype = "GM"
  
  #select lifestage of interest
  lifestage = "Pupae"
  
  # Saving vector
  vec_dates = "resource site minmax CI dates"
  
  resource = gm
  
  # fir or spruce
  sites <- unique(resource$Name)
  
  
  
  # Subset each site
  
  for (m in 1: length(unique(resource$Name))){
    
    subsite <- subset(resource, Name == sites[m])
    subsite <- subset(subsite, subsite[,lifestage] != 0)
    subsite$date <-paste( subsite$Year,"-", subsite$Month,"-", subsite$Day, sep ="") 
    
    
    
    #### Build loop
    
    
    dates_sampling <- vector(mode = "list", length = length(sites))
    
    for (l in 1: length(subsite[,lifestage]) ) {
      subsite$sum [l] = sum(subsite[,lifestage][1:l])
    }
    
    
    min =c()
    max=c()
    for(i in 1:length(CI)){
      min[i] = sum(subsite[,lifestage])*(((100-CI[i])/2)/100)
      max[i] = sum(subsite[,lifestage])*((CI[i]+((100-CI[i])/2))/100) 
      
      
      for (j in 1: length(subsite[,lifestage])){
        if(subsite$sum[j] < min[i]){
          j =j+1
        }else{
          # print(paste(restype," ",subsite$Name[j]," min date CI",CI[i],": ",subsite$date[j], sep= ""))
          tempmin = paste(restype," ",subsite$Name[j]," min_date CI",CI[i]," ",subsite$date[j], sep= "")
          vec_dates = c(vec_dates, tempmin)
          break
        }
      } 
      
      for (k in 1: length(subsite[,lifestage])){
        if(subsite$sum[k] <= max[i]){
          k =k+1
        }else{
          #print(paste(restype," ",subsite$Name[k-1]," max date CI",CI[i],": ",subsite$date[k-1], sep= ""))
          tempmax = paste(restype," ",subsite$Name[k-1]," max_date CI",CI[i]," ",subsite$date[k-1], sep= "")
          vec_dates = c(vec_dates, tempmax)
          break
        }
      }
    }
  }
  tabledates = strsplit(vec_dates, split = " ")
  tabledates = as.data.frame(Reduce(rbind, tabledates))
  colnames(tabledates) = c("resource","site","minmax", "CI", "dates")
  rownames(tabledates) = 1:nrow(tabledates)
  write.csv(tabledates,paste("SpongyMoth_",lifestage,"_BioSIM_Summer_",year,"_dates_",today,".csv",sep=""))
  
  # Copy paste both outputs into a csv file (one after the other vertically) and add top line
  
  
  ###########################
  #                         #
  #     GM final tables     #
  #                         #
  ###########################
  
  #Before loading data separate columns in excel
  
  data <- read.csv(paste("SpongyMoth_",lifestage,"_BioSIM_Summer_",year,"_dates_",today,".csv",sep=""))
  data <- data[-1,-1]
  resourcetype = "GM"
  sites = unique(data$site)
  
  #Prepare output file
  final <- vector(mode = "list", length = length(resourcetype))
  
  
  
  for(i in 1:length(resourcetype)){
    resdata <- subset(data,resource == resourcetype[i])
    
    
    #CI66
    data66 <- subset(resdata, CI == "CI66")
    min_data66 <- subset(data66, minmax == "min_date")
    max_data66 <- subset(data66, minmax == "max_date")
    
    
    matrix66 <- data.frame(matrix(0, ncol = 4, nrow = length(unique(data$site))))
    colnames(matrix66) <- c("resource","site","min66","max66")
    
    for (j in 1: length(unique(data$site))){
      matrix66[j,1] <- data66$resource[j]
      matrix66[j,2] <- sites[j]
      matrix66[j,3] <- min_data66$dates[j]
      matrix66[j,4] <- max_data66$dates[j]
    }
    
    #CI95
    data95 <- subset(resdata, CI == "CI95")
    min_data95 <- subset(data95, minmax == "min_date")
    max_data95 <- subset(data95, minmax == "max_date")
    
    
    matrix95 <- data.frame(matrix(0, ncol = 4, nrow = length(unique(data$site))))
    colnames(matrix95) <- c("resource","site","min95","max95")
    
    for (k in 1: length(unique(data$site))){
      matrix95[k,1] <- data95$resource[k]
      matrix95[k,2] <- sites[k]
      matrix95[k,3] <- min_data95$dates[k]
      matrix95[k,4] <- max_data95$dates[k]
    }
    
    #CI99
    data99 <- subset(resdata, CI == "CI99")
    min_data99 <- subset(data99, minmax == "min_date")
    max_data99 <- subset(data99, minmax == "max_date")
    
    
    matrix99 <- data.frame(matrix(0, ncol = 4, nrow = length(unique(data$site))))
    colnames(matrix99) <- c("resource","site","min99","max99")
    
    for (l in 1: length(unique(data$site))){
      matrix99[l,1] <- data99$resource[l]
      matrix99[l,2] <- sites[l]
      matrix99[l,3] <- min_data99$dates[l]
      matrix99[l,4] <- max_data99$dates[l]
    }
    final[[i]] <- cbind(matrix66,matrix95,matrix99)
  }
  
  tablefinal <- data.frame(Reduce(rbind, final))
  tablefinal <- tablefinal[,c("resource","site","Start66","End66","Start95","End95","Start99","End99")]
  
  # save file
  write.csv(tablefinal,paste("SpongyMoth_",lifestage,"_BioSIM_Summer_",year,"_Sampling_dates_",today,".csv",sep=""))
}

