##By John H. Kim 2014
#Write USGS water quality and flow data into LOADEst input files
#Run LOADEst program and summarize the results (Calcium load in rivers)
#For large watersheds in the Mississippi river basin
rm(list = ls(all = TRUE))
rm(list=ls())
library(stringr)

setwd("C:/Users/John/Desktop/Recent USGS alkalinity data")

Data <- read.csv(file="USGS merged water quality and stream flow data in large watersheds in hydrological units 10 and 11 by river groups.csv",sep=",",header=TRUE) #This data file contains water flow and water quality data from stream monitoring stations. The data had been merged to have the flow and quality data from each station on the same dates. The file path is for my PC laptop but for portability, uncomment the lines below to read input files.
#Data <- read.csv(file.choose(), header=TRUE)
Data$Date <- as.Date(Data$Date)
Data$Time <- as.character("12:00") #time data is not given for most data points
Data$Time <- gsub(pattern = ":", replacement = "",  x = Data$Time)
Data <- Data[!is.na(Data$discharge_va),] #this is discharge (flow) data. We remove all lines that do not have a value for column
FlowData <- read.csv(file="USGS IBWC large Texas basins daily flow data with estimates.csv",sep=",",header=TRUE) #this file contains all flow data from the region of interest
FlowData$Date <- as.Date(FlowData$Date)
FlowData$Time <- as.character("12:00")
FlowData$Time <- gsub(pattern = ":", replacement = "",  x = FlowData$Time)
FlowData <- FlowData[!is.na(FlowData$discharge_va),]
FlowData <- FlowData[order(FlowData$River,FlowData$Date),]
#same level of factor for Data and FlowData
Data$River.x <- factor(Data$River.x, levels=levels(FlowData$River))
SitesAll <- unique(Data$River.x)
EstFile <- scan("est.inp", character(0), sep = "\n") # separate each line
CalibFile <- scan("calib.inp", character(0), sep = "\n") # separate each line
ControlFile <- scan("control.inp", character(0), sep = "\n") # separate each line
HeaderFile <- scan("header.inp", character(0), sep = "\n") # separate each line

Parameters <- c("Alkalinity", "Carbonate", "Bicarbonate", "Hardness", "Carbonate Hardness",
  "Noncarbonate Hardness", "Calcium") #these are water quality parameters we are interested in
ParameterShortNames <- c("Alkal", "Carb", "Bicarb", "Hard", "CHard", "NCHard", "Calc")
ParameterNames <- c("Alkalinity        ", "Carbonate         ", "Bicarbonate       ",
  "Total Hardness    ", "Carbonate Hardness", "NonCarbonate Hard ", "Calcium           ")
OutputNames<-c("Alkalinity","Carbonate","Bicarbonate","TotalHardness",
  "CarbonateHardness","NonCarbonateHard","calcium")
flist <- list.files("C:/Users/John/Desktop/Loadest/", "loadest.exe", full.names = TRUE) #loadest is a free program downloadable from the USGS website
LoadStats<-c()

#input files
for (i in 1:length(Parameters)) {
  setwd("C:/Users/John/Desktop/Recent USGS alkalinity data")
  MatchingData <- Data[!is.na(Data[,7+i]),]
  MatchingData$Date <- as.Date(MatchingData$Date)
  MatchingData$Time <- as.character("12:00")
  MatchingData$Time <- gsub(pattern = ":", replacement = "",  x = MatchingData$Time)
  MatchingData$Time[which(nchar(MatchingData$Time) == 3)] <- paste("0",MatchingData$Time[which(nchar(MatchingData$Time) == 3)],sep="")
  MatchingData$discharge_va[which(nchar(MatchingData$discharge_va) > 7)] <- signif(MatchingData$discharge_va[which(nchar(MatchingData$discharge_va) > 7)],digits = 5)

  for (j in 1:length(unique(MatchingData$River.x))) {
    MDataOneSite <- MatchingData[which(MatchingData$River.x == unique(MatchingData$River.x)[j]),]
    MDataOneSite$discharge_va[which(MDataOneSite$discharge_va == 0)] <- unique(sort(MDataOneSite$discharge_va))[2]
    FDataOneSite <- FlowData[which(FlowData$River == unique(MatchingData$River.x)[j]),]
    FDataOneSite$discharge_va[which(FDataOneSite$discharge_va == 0)] <- unique(sort(FDataOneSite$discharge_va))[2]
    print(c(dim(MDataOneSite)[1],dim(FDataOneSite)[1]))
    #run loadest depending on data availability
    if (dim(MDataOneSite)[1] >= 50) {
      mainDir <- paste("C:/Users/John/Desktop/Loadest/",Parameters[i],sep="")
      subDir <- paste(unique(MatchingData$River)[j])
      dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
      setwd(file.path(mainDir, subDir))
      #add the executable file to the folder
      file.copy(flist, file.path(mainDir, subDir))

      #calib.inp (flow and concentration)
      Year <- as.numeric(format(MDataOneSite$Date, "%Y"))
      Month <- as.character(format(MDataOneSite$Date, "%m"))
      Day <- as.character(format(MDataOneSite$Date, "%d"))
      Date <- paste(Year,Month,Day, sep="")
      Time <- MDataOneSite$Time
      Conc <- as.character(MDataOneSite[,7+i])
      Flow <- as.character(MDataOneSite$discharge_va)
      Flow[which(MDataOneSite$discharge_va==trunc(MDataOneSite$discharge_va))] <-
      paste(Flow[which(MDataOneSite$discharge_va==trunc(MDataOneSite$discharge_va))],".",sep="")
      Flow <- gsub(pattern = "e+05", replacement = "00000", x = Flow, fixed = TRUE)
      Conc[which(MDataOneSite[,7+i]==trunc(MDataOneSite[,7+i]))] <-
      paste(Conc[which(MDataOneSite[,7+i]==trunc(MDataOneSite[,7+i]))],".",sep="")
      ArrangedData <- paste(Date,Time,sep=c("    "))
      ArrangedData <- paste(ArrangedData,Flow,sep=c("      "))
      ArrangedData[which(nchar(Flow)==1)] <- paste(ArrangedData[which(nchar(Flow)==1)],Conc[which(nchar(Flow)==1)],sep=c("         "))
      ArrangedData[which(nchar(Flow)==2)] <- paste(ArrangedData[which(nchar(Flow)==2)],Conc[which(nchar(Flow)==2)],sep=c("        "))
      ArrangedData[which(nchar(Flow)==3)] <- paste(ArrangedData[which(nchar(Flow)==3)],Conc[which(nchar(Flow)==3)],sep=c("       "))
      ArrangedData[which(nchar(Flow)==4)] <- paste(ArrangedData[which(nchar(Flow)==4)],Conc[which(nchar(Flow)==4)],sep=c("      "))
      ArrangedData[which(nchar(Flow)==5)] <- paste(ArrangedData[which(nchar(Flow)==5)],Conc[which(nchar(Flow)==5)],sep=c("     "))
      ArrangedData[which(nchar(Flow)==6)] <- paste(ArrangedData[which(nchar(Flow)==6)],Conc[which(nchar(Flow)==6)],sep=c("    "))
      ArrangedData[which(nchar(Flow)==7)] <- paste(ArrangedData[which(nchar(Flow)==7)],Conc[which(nchar(Flow)==7)],sep=c("   "))
      SiteDescription = paste("USGS",unique(MatchingData$River)[j],Parameters[i])
      NewFile <- sub(pattern = "Canadian Rv nr Amarillo, TX USGS 07227500", replacement = SiteDescription, x = CalibFile)
      NewFile <- c(NewFile[1:15],ArrangedData)
      cat(NewFile,file=paste("USGS",unique(MatchingData$River)[j],ParameterShortNames[i],"calib.inp"),sep="\n")

      #control.inp
      NewCFile <- ControlFile
      NewCFile[13] <- paste("USGS",unique(MatchingData$River)[j],ParameterShortNames[i],"header.inp")
      NewCFile[14] <- paste("USGS",unique(MatchingData$River)[j],ParameterShortNames[i],"calib.inp")
      NewCFile[15] <- paste("USGS",unique(MatchingData$River)[j],"est.inp")
      cat(NewCFile,file="control.inp",sep="\n")

      #header.inp
      NewHFile <- sub(pattern = "Canadian Rv nr Amarillo, TX USGS 07227500", replacement = SiteDescription, x = HeaderFile)
      NewHFile <- sub(pattern = "Calcium           ", replacement = ParameterNames[i], x = NewHFile)
      cat(NewHFile,file=paste("USGS",unique(MatchingData$River)[j],ParameterShortNames[i],"header.inp"),sep="\n")

      #est.inp (flow only)
      FYear <- as.numeric(format(FDataOneSite$Date, "%Y"))
      FMonth <- as.character(format(FDataOneSite$Date, "%m"))
      FDay <- as.character(format(FDataOneSite$Date, "%d"))
      FDate <- paste(FYear,FMonth,FDay, sep="")
      FTime <- FDataOneSite$Time
      Flow <- as.character(FDataOneSite$discharge_va)
      Flow[which(FDataOneSite$discharge_va==trunc(FDataOneSite$discharge_va))] <-
      paste(Flow[which(FDataOneSite$discharge_va==trunc(FDataOneSite$discharge_va))],".",sep="")
      Flow <- gsub(pattern = "e+05", replacement = "00000", x = Flow, fixed = TRUE)
      FArrangedData <- paste(FDate,FTime,sep=c("    "))
      FArrangedData <- paste(FArrangedData,Flow,sep=c("      "))
      FSiteDescription = paste("USGS",unique(MatchingData$River)[j])
      FNewFile <- sub(pattern = "Canadian Rv nr Amarillo, TX USGS 07227500", replacement = FSiteDescription, x = EstFile)
      FNewFile <- c(FNewFile[1:21],FArrangedData)
      cat(FNewFile,file=paste("USGS",unique(MatchingData$River)[j],"est.inp"),sep="\n")

      system("loadest.exe") #run the loadest program given all the input values

      OutputFile <- scan(paste(OutputNames[i],".out",sep=""), character(0), sep = "\n") # separate each line
      #sampledata <- readLines("calcium.out")
      a<-grep(pattern = " Bp ", OutputFile)
      b<-grep(pattern = "[%]", OutputFile)
      BpLine<-intersect(a,b)
      RSquaredLine<-grep(pattern = "R-Squared", OutputFile)
      EstPeriodLine<-grep(pattern = "Est. Period", OutputFile)

      Bps <- str_trim(substr(OutputFile[BpLine],10,17))
      RSquares <- str_trim(substr(OutputFile[RSquaredLine],34,40))
      MeanLoads <- str_trim(unlist(strsplit(substr(OutputFile[EstPeriodLine],13,75), "\\ ")))
      MeanLoads <- MeanLoads[which(MeanLoads!="")]
      MeanLoads <- c(MeanLoads[1:6],MeanLoads[8:9],MeanLoads[11:12])

      if(length(Bps)>=2) {
        Temp <- cbind.data.frame(unique(MatchingData$River)[j],Parameters[i],t(Bps),t(RSquares),t(MeanLoads),
        median(as.numeric(format(MDataOneSite$Date, "%Y"))),t(range(as.numeric(format(MDataOneSite$Date, "%Y")))),t(range(as.numeric(format(FDataOneSite$Date, "%Y")))))
      } else {
        Bps <- c(NA,NA); RSquares <- c(NA,NA)
        Temp <- cbind.data.frame(unique(MatchingData$River)[j],Parameters[i],t(Bps),t(RSquares),t(MeanLoads),
        median(as.numeric(format(MDataOneSite$Date, "%Y"))),t(range(as.numeric(format(MDataOneSite$Date, "%Y")))),t(range(as.numeric(format(FDataOneSite$Date, "%Y")))))
      }
      LoadStats <- rbind(LoadStats,Temp)
    }
  }
}
colnames(LoadStats) <- c("River","Parameter","Bp (load)","Bp (conc)","R-square (load)",
"R-square (conc)","N","Mean Load (AMLE)","Lower 95% CI","Upper 95% CI",
"SE Prediction","SE (AMLE)","Mean Load (MLE)","SE (MLE)","Mean Load (LAD)","SE (LAD)",
"Median year of matching data","Beginning year of matching data","End year of matching data","Beginning year of flow data","End year of flow data")
write.csv(LoadStats,"C:/Users/John/Desktop/Recent USGS alkalinity data/USGS large basins LOADest stats summaries.csv")
