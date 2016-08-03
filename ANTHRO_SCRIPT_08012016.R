

anthro <- function() {
  
  options(java.parameters = "-Xmx1000m")
  
  if (!require("rJava")) { 
    install.packages("rJava") 
  } 
  library(rJava) 
  if (!require("xlsxjars")) { 
    install.packages("xlsxjars") 
  } 
  library(xlsxjars) 
  if (!require("xlsx")) { 
    install.packages("xlsx") 
  } 
  library(xlsx) 
  
  if (!require("openxlsx")) { 
    install.packages("openxlsx") 
  } 
  library(openxlsx)
  if (!require("lubridate")) { 
    install.packages("lubridate") 
  } 
  library(lubridate) 
  options(lubridate.verbose = FALSE)
  if (!require("ggplot2")) { 
    install.packages("ggplot2") 
  } 
  library(ggplot2)
  
  
  print("Input the four letters that signify the patient we are doing calculations for") 
  print("Example: FILA") 
  patient <<- readline(prompt="Enter here: ") 
  
  
  print("Input the directory that you wish to draw this patient's ANTHROPOMETRICS file from") 
  print("Example: C:/Folder_Name/") 
  directory <<- readline(prompt="Enter here: ") 
  setwd(directory) 
  
  data <- "ANTHROPOMETRICS_SOURCE.xlsx" 
  data <- gsub(" ","",paste(patient,"_",data)) 
  Anthropometrics <- read.xlsx(data,sheet=1,detectDates=TRUE) 
  Anthropometrics <- Anthropometrics[!is.na(Anthropometrics$MRNUMBER),]
  
  return(Anthropometrics)
}
##################################################################

Anthropometrics <- anthro()

#using references tables
reference <- "G:/MySQL Database/Anthropometrics"
setwd(reference)
CDC.References <- read.csv('ANTHROPOMETRICS_CDC_REFERENCES_SOURCE_08012016.txt', header=TRUE, sep="\t", na.strings=c("","NA"))
NHANES.References <- read.csv('ANTHROPOMETRICS_NHANES_REFERENCES_SOURCE_08012016.txt', header=TRUE, sep="\t", na.strings=c("","NA"))
WHO.References <- read.csv('ANTHROPOMETRICS_WHO_REFERENCES_SOURCE_08012016.txt', header=TRUE, sep="\t", na.strings=c("","NA"))


setwd("G:/MySQL Database/Demographics/")
DEMOGRAPHICS_SOURCE <- "DEMOGRAPHICS_SOURCE.xlsx"
DEMOGRAPHICS_SOURCE <- read.xlsx(DEMOGRAPHICS_SOURCE,sheet=1, detectDates = TRUE)
Demographics.Identified <- DEMOGRAPHICS_SOURCE[which(DEMOGRAPHICS_SOURCE$MRNUMBER==Anthropometrics$MRNUMBER), ]

#saves in patient's folder
setwd(directory)

#Calculating Age to use in equations
i=1:dim(Anthropometrics)[1]
Bday <- as.Date(Demographics.Identified$DOB, format= "%m/%d/%Y") #First convert classes from factor to date
Date <- as.Date(Anthropometrics$DATE[i], format = "%m/%d/%Y")
library(lubridate)
span <- interval(Bday, Date)
AGE <- as.period(span)
AGE <- as.numeric(year(AGE))
AGE_YEARS <- AGE
AGE_MO <- as.period(span, "months")
AGE_MO <- as.numeric(month(AGE_MO))


#Gender
SEX <- ifelse(Demographics.Identified$GENDER[1] == "M", 1, 0)

#BMI
BMI <- Anthropometrics$WT/(Anthropometrics$HT/100)^2

#Upper Arm Muscle Circumference
AMC <- Anthropometrics$UAC-(pi*Anthropometrics$TSF/10)

#Upper Arm Area
UAA <- 0.785*((Anthropometrics$UAC/pi)^2)

#Upper Arm Muscle Area
AMA <- ((Anthropometrics$UAC-(pi*Anthropometrics$TSF/10))^2)/12.57

#Upper Arm Fat Area
AFA <- UAA-AMA
#this would have to be used after calculating UAA and AMA


#Visceral Cavity Area
VCA <- ((Anthropometrics$UC-(pi*((Anthropometrics$USF/10)+(2*Anthropometrics$SISF/10)+(Anthropometrics$MBSF/10))/4))^2)/(4*pi)


#Visceral Cavity Area Percentage
VC_PCTG <- ((Anthropometrics$UC*10-(pi*((Anthropometrics$USF+2*Anthropometrics$SISF+Anthropometrics$MBSF)/4)))^2/(Anthropometrics$UC*10)^2)*100


#Impedence
Z <- ((Anthropometrics$X^2)+(Anthropometrics$R^2))^0.5

#Phase Angle
P <- (tan(Anthropometrics$X/Anthropometrics$R))*180/pi

#Arpadi Fat Free Mass
ARPADI_FFM <- ((3.474+(0.459*(Anthropometrics$HT^2/Anthropometrics$R))+(0.064*Anthropometrics$WT))/(0.769 -(0.009*AGE)-(0.016*SEX)))

#Goran Fat Free Mass
GORAN_FFM <- ((0.59*((Anthropometrics$HT^2)/Anthropometrics$R))+(0.065*Anthropometrics$WT)+0.04)/(0.769-(0.0025*AGE)-(0.019*SEX))

#ARPADI_TOTAL_BODY_WATER
ARPADI_TBW <- 0.725+((0.475*(Anthropometrics$HT^2))/Anthropometrics$R)+(0.14*Anthropometrics$WT)

#SCHAEFER_FAT_FREE_MASS
SCHAEFER_FFM <- (((0.65)*(Anthropometrics$HT^2))/(Z))+(0.68*AGE)+0.15

#KOTLER_FAT_FREE_MASS
KOTLER_FFM <- (0.88/22.22)*((Anthropometrics$HT^1.97)/(Z^0.49))+0.081*Anthropometrics$WT+0.07

#BODY_FAT_PERCENTAGE
FFM <- ifelse(AGE < 18, FFM <- SCHAEFER_FFM, FFM <- KOTLER_FFM)
#age <18 arpadi, >18 Kotler, what if age is equal to 18
BODY_FAT_PCTG <- ((Anthropometrics$WT-FFM)/Anthropometrics$WT)*100


#can run below to double check what has been calculated so far
#table <- cbind(AGE, BMI, AMC, UAA, AMA, AFA, VCA, VC_PCTG, Z, P, ARPADI_FFM, GORAN_FFM, ARPADI_TBW, SCHAEFER_FFM, KOTLER_FFM, BODY_FAT_PCTG)





#Race for NHANES
RACE <- if (Demographics.Identified$RACE[1] == "White"){RACE <- 2} else if (Demographics.Identified$RACE[1] == "Asian"){RACE <- 2}  else if (Demographics.Identified$RACE[1] == "African-American"){RACE <- 1} else if (Demographics.Identified$RACE[1] == "Hispanic"){RACE <- 2}


#Gender for NHANES
SEX <- ifelse(Demographics.Identified$GENDER[1] == "M", 1, 2)

#newHT
HT <- floor(Anthropometrics$HT)
HT1 <- HT #for output table

#NHANES
MEAN_NHANES_HT <- c()
SD_NHANES_HT <- c()
MEAN_NHANES_WT <- c()
SD_NHANES_WT <- c()
MEAN_NHANES_BMI <- c()
SD_NHANES_BMI <- c()
MEAN_NHANES_UAC <- c()
SD_NHANES_UAC <- c()
MEAN_NHANES_TSF <- c()
SD_NHANES_TSF <- c()
MEAN_NHANES_UAA <- c()
SD_NHANES_UAA <- c()
MEAN_NHANES_AMA <- c()
SD_NHANES_AMA <- c()
MEAN_NHANES_AFA <- c()
SD_NHANES_AFA <- c()
MEAN_NHANES_SSF <- c()
SD_NHANES_SSF <- c()
MEAN_NHANES_UC <- c()
SD_NHANES_UC <- c()
MEAN_NHANES_WT_FOR_HT <- c()
SD_NHANES_WT_FOR_HT <- c()
L_NHANES_UC <- c()
M_NHANES_UC <- c()
S_NHANES_UC <- c()

for (i in seq(length(AGE))) {
  MEAN_NHANES_HT <- c(MEAN_NHANES_HT, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_HT_AGE==AGE[i] & NHANES.References$SEX_NHANES_HT_AGE==SEX & NHANES.References$RACE_NHANES_HT_AGE==RACE, select=c("MEAN_NHANES_HT_AGE")))
  SD_NHANES_HT <- c(SD_NHANES_HT, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_HT_AGE==AGE[i] & NHANES.References$SEX_NHANES_HT_AGE==SEX & NHANES.References$RACE_NHANES_HT_AGE==RACE, select=c("SD_NHANES_HT_AGE")))
  MEAN_NHANES_WT <- c(MEAN_NHANES_WT, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_WT_AGE==AGE[i] & NHANES.References$SEX_NHANES_WT_AGE==SEX & NHANES.References$RACE_NHANES_WT_AGE==RACE, select=c("MEAN_NHANES_WT_AGE")))
  SD_NHANES_WT <- c(SD_NHANES_WT, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_WT_AGE==AGE[i] & NHANES.References$SEX_NHANES_WT_AGE==SEX & NHANES.References$RACE_NHANES_WT_AGE==RACE, select=c("SD_NHANES_WT_AGE")))
  MEAN_NHANES_BMI <- c(MEAN_NHANES_BMI, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_BMI_AGE==AGE[i] & NHANES.References$SEX_NHANES_BMI_AGE==SEX & NHANES.References$RACE_NHANES_BMI_AGE==RACE, select=c("MEAN_NHANES_BMI_AGE")))
  SD_NHANES_BMI <- c(SD_NHANES_BMI, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_BMI_AGE==AGE[i] & NHANES.References$SEX_NHANES_BMI_AGE==SEX & NHANES.References$RACE_NHANES_BMI_AGE==RACE, select=c("SD_NHANES_BMI_AGE")))
  MEAN_NHANES_UAC <- c(MEAN_NHANES_UAC, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_UAC_AGE==AGE[i] & NHANES.References$SEX_NHANES_UAC_AGE==SEX & NHANES.References$RACE_NHANES_UAC_AGE==RACE, select=c("MEAN_NHANES_UAC_AGE")))
  SD_NHANES_UAC <- c(SD_NHANES_UAC, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_UAC_AGE==AGE[i] & NHANES.References$SEX_NHANES_UAC_AGE==SEX & NHANES.References$RACE_NHANES_UAC_AGE==RACE, select=c("SD_NHANES_UAC_AGE")))
  MEAN_NHANES_TSF <- c(MEAN_NHANES_TSF, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_TSF_AGE==AGE[i] & NHANES.References$SEX_NHANES_TSF_AGE==SEX & NHANES.References$RACE_NHANES_TSF_AGE==RACE, select=c("MEAN_NHANES_TSF_AGE")))
  SD_NHANES_TSF <- c(SD_NHANES_TSF, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_TSF_AGE==AGE[i] & NHANES.References$SEX_NHANES_TSF_AGE==SEX & NHANES.References$RACE_NHANES_TSF_AGE==RACE, select=c("SD_NHANES_TSF_AGE")))
  MEAN_NHANES_UAA <- c(MEAN_NHANES_UAA, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_UAA_AGE==AGE[i] & NHANES.References$SEX_NHANES_UAA_AGE==SEX & NHANES.References$RACE_NHANES_UAA_AGE==RACE, select=c("MEAN_NHANES_UAA_AGE")))
  SD_NHANES_UAA <- c(SD_NHANES_UAA, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_UAA_AGE==AGE[i] & NHANES.References$SEX_NHANES_UAA_AGE==SEX & NHANES.References$RACE_NHANES_UAA_AGE==RACE, select=c("SD_NHANES_UAA_AGE")))
  MEAN_NHANES_AMA <- c(MEAN_NHANES_AMA, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_AMA_AGE==AGE[i] & NHANES.References$SEX_NHANES_AMA_AGE==SEX & NHANES.References$RACE_NHANES_AMA_AGE==RACE, select=c("MEAN_NHANES_AMA_AGE")))
  SD_NHANES_AMA <- c(SD_NHANES_AMA, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_AMA_AGE==AGE[i] & NHANES.References$SEX_NHANES_AMA_AGE==SEX & NHANES.References$RACE_NHANES_AMA_AGE==RACE, select=c("SD_NHANES_AMA_AGE")))
  MEAN_NHANES_AFA <- c(MEAN_NHANES_AFA, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_AFA_AGE==AGE[i] & NHANES.References$SEX_NHANES_AFA_AGE==SEX & NHANES.References$RACE_NHANES_AFA_AGE==RACE, select=c("MEAN_NHANES_AFA_AGE")))
  SD_NHANES_AFA <- c(SD_NHANES_AFA, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_AFA_AGE==AGE[i] & NHANES.References$SEX_NHANES_AFA_AGE==SEX & NHANES.References$RACE_NHANES_AFA_AGE==RACE, select=c("SD_NHANES_AFA_AGE")))
  MEAN_NHANES_SSF <- c(MEAN_NHANES_SSF, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_SSSF_AGE==AGE[i] & NHANES.References$SEX_NHANES_SSSF_AGE==SEX & NHANES.References$RACE_NHANES_SSSF_AGE==RACE, select=c("MEAN_NHANES_SSSF_AGE")))
  SD_NHANES_SSF <- c(SD_NHANES_SSF, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_SSSF_AGE==AGE[i] & NHANES.References$SEX_NHANES_SSSF_AGE==SEX & NHANES.References$RACE_NHANES_SSSF_AGE==RACE, select=c("SD_NHANES_SSSF_AGE")))
  
}

MEAN_NHANES_WT_FOR_HT <- rep(NA,length(AGE))
SD_NHANES_WT_FOR_HT <- rep(NA,length(AGE))
WT_FOR_HT <- cbind.data.frame(AGE,MEAN_NHANES_WT_FOR_HT,SD_NHANES_WT_FOR_HT)

#For NHANES WT for HT
for (i in seq(length(AGE))) {
  if (AGE[i] < 12 & AGE[i] >= 2 & SEX ==1) {
    WT_FOR_HT$MEAN_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==HT[i], select=c("MEAN_NHANES_WT_FOR_HT1"))
    WT_FOR_HT$SD_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==HT[i], select=c("SD_NHANES_WT_FOR_HT1"))
  } 
  else if (AGE[i] >= 12 & AGE[i] < 18 & SEX==1) {
    WT_FOR_HT$MEAN_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==HT[i], select=c("MEAN_NHANES_WT_FOR_HT2"))
    WT_FOR_HT$SD_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==HT[i], select=c("SD_NHANES_WT_FOR_HT2"))
  }
  else if (AGE[i] >= 18 & AGE[i] < 75) {
    WT_FOR_HT$MEAN_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT3==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT3==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT3==HT[i], select=c("MEAN_NHANES_WT_FOR_HT3"))
    WT_FOR_HT$SD_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT3==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT3==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT3==HT[i], select=c("SD_NHANES_WT_FOR_HT3"))
  }
  else if (AGE[i] < 11 & AGE[i] >= 2 & SEX==2 ) {
    WT_FOR_HT$MEAN_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==HT[i], select=c("MEAN_NHANES_WT_FOR_HT1"))
    WT_FOR_HT$SD_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==HT[i], select=c("SD_NHANES_WT_FOR_HT1"))
  } 
  else if (AGE[i] >= 11 & AGE[i] < 18 & SEX==2) {
    WT_FOR_HT$MEAN_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==HT[i], select=c("MEAN_NHANES_WT_FOR_HT2"))
    WT_FOR_HT$SD_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==HT[i], select=c("SD_NHANES_WT_FOR_HT2"))
  }
}


MEAN_NHANES_WT_FOR_HT <- WT_FOR_HT$MEAN_NHANES_WT_FOR_HT
SD_NHANES_WT_FOR_HT <- WT_FOR_HT$SD_NHANES_WT_FOR_HT
AGE <- WT_FOR_HT$AGE


#For NHANES UC (waist circumference)
a <- (AGE <= 4 | AGE >= 19)
AGE_UC <- ifelse(AGE <= 4 | AGE >= 19, AGE, AGE_MO)

for (i in which(AGE<=4 | AGE>=19)) {
  RACE <- if (Demographics.Identified$RACE[1] == "White"){RACE <- 2} else if (Demographics.Identified$RACE[1] == "Asian"){RACE <- 2}  else if (Demographics.Identified$RACE[1] == "African-American"){RACE <- 1} else if (Demographics.Identified$RACE[1] == "Hispanic"){RACE <- 3}  
  MEAN_NHANES_UC <- c(MEAN_NHANES_UC, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_UC_AGE==AGE_UC[i] & NHANES.References$SEX_NHANES_UC_AGE==SEX & NHANES.References$RACE_NHANES_UC_AGE==RACE, select=c("MEAN_NHANES_UC_AGE")))
  SD_NHANES_UC <- c(SD_NHANES_UC, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_UC_AGE==AGE_UC[i] & NHANES.References$SEX_NHANES_UC_AGE==SEX & NHANES.References$RACE_NHANES_UC_AGE==RACE, select=c("SD_NHANES_UC_AGE")))
  
  
}

for (i in which(!(AGE<=4 | AGE>=19))) {
  L_NHANES_UC <- c(L_NHANES_UC, subset(NHANES.References, NHANES.References$AGE_MO_NHANES_UC_AGE==AGE_UC[i] & NHANES.References$SEX_NHANES_UC_AGE2==SEX, select=c("L_NHANES_UC")))
  M_NHANES_UC <- c(M_NHANES_UC, subset(NHANES.References, NHANES.References$AGE_MO_NHANES_UC_AGE==AGE_UC[i] & NHANES.References$SEX_NHANES_UC_AGE2==SEX, select=c("M_NHANES_UC")))
  S_NHANES_UC <- c(S_NHANES_UC, subset(NHANES.References, NHANES.References$AGE_MO_NHANES_UC_AGE==AGE_UC[i] & NHANES.References$SEX_NHANES_UC_AGE2==SEX, select=c("S_NHANES_UC")))
}





MEAN_NHANES_HT <- as.numeric(MEAN_NHANES_HT)
SD_NHANES_HT <- as.numeric(SD_NHANES_HT)
MEAN_NHANES_WT <- as.numeric(MEAN_NHANES_WT)
SD_NHANES_WT <- as.numeric(SD_NHANES_WT)
MEAN_NHANES_BMI <- as.numeric(MEAN_NHANES_BMI)
SD_NHANES_BMI <- as.numeric(SD_NHANES_BMI)
MEAN_NHANES_UAC <- as.numeric(MEAN_NHANES_UAC)
SD_NHANES_UAC <- as.numeric(SD_NHANES_UAC)
MEAN_NHANES_TSF <- as.numeric(MEAN_NHANES_TSF)
SD_NHANES_TSF <- as.numeric(SD_NHANES_TSF)
MEAN_NHANES_UAA <- as.numeric(MEAN_NHANES_UAA)
SD_NHANES_UAA <- as.numeric(SD_NHANES_UAA)
MEAN_NHANES_AMA <- as.numeric(MEAN_NHANES_AMA)
SD_NHANES_AMA <- as.numeric(SD_NHANES_AMA)
MEAN_NHANES_AFA <- as.numeric(MEAN_NHANES_AFA)
SD_NHANES_AFA <- as.numeric(SD_NHANES_AFA)
MEAN_NHANES_SSF <- as.numeric(MEAN_NHANES_SSF)
SD_NHANES_SSF <- as.numeric(SD_NHANES_SSF)
MEAN_NHANES_UC <- as.numeric(MEAN_NHANES_UC)
SD_NHANES_UC <- as.numeric(SD_NHANES_UC)
MEAN_NHANES_WT_FOR_HT <- as.numeric(MEAN_NHANES_WT_FOR_HT)
SD_NHANES_WT_FOR_HT <- as.numeric(SD_NHANES_WT_FOR_HT)
L_NHANES_UC <- as.numeric(L_NHANES_UC)
M_NHANES_UC <- as.numeric(M_NHANES_UC)
S_NHANES_UC <- as.numeric(S_NHANES_UC)

#for some reason ifelse does not work in this case
#so instead convert true/false to numeric, and replace
b <- as.numeric(a)
b[b==0] <- L_NHANES_UC
b[b==1] <- NA
L_NHANES_UC <- b
b <- as.numeric(a)
b[b==0] <- M_NHANES_UC
b[b==1] <- NA
M_NHANES_UC <- b
b <- as.numeric(a)
b[b==0] <- S_NHANES_UC
b[b==1] <- NA
S_NHANES_UC <- b
b <- as.numeric(a)
b[b==1] <- MEAN_NHANES_UC
b[b==0] <- NA
MEAN_NHANES_UC <- b
b <- as.numeric(a)
b[b==1] <- SD_NHANES_UC
b[b==0] <- NA
SD_NHANES_UC <- b

#table2 <- cbind(AGE, MEAN_NHANES_SSF, SD_NHANES_SSF, MEAN_NHANES_AFA, SD_NHANES_AFA, MEAN_NHANES_AMA, SD_NHANES_AMA, MEAN_NHANES_BMI, SD_NHANES_BMI, MEAN_NHANES_HT, SD_NHANES_HT, MEAN_NHANES_TSF, SD_NHANES_TSF, MEAN_NHANES_UAC, SD_NHANES_UAC, MEAN_NHANES_UAA, SD_NHANES_UAC, MEAN_NHANES_UC, SD_NHANES_UC, MEAN_NHANES_WT, SD_NHANES_WT, MEAN_NHANES_WT_FOR_HT, SD_NHANES_WT_FOR_HT)


#NHANES Percentiles and Z scores

#NHANES_HEIGHT_Z_SCORE
NHANES_HT_Z <- (Anthropometrics$HT-MEAN_NHANES_HT)/SD_NHANES_HT

#NHANES_HEIGHT_PERCENTILE
NHANES_HT_PCTL <- (pnorm(NHANES_HT_Z))*100

#NHANES_WEIGHT_Z_SCORE
NHANES_WT_Z <- (Anthropometrics$WT-MEAN_NHANES_WT)/SD_NHANES_WT

#NHANES_WEIGHT_PERCENTILE
NHANES_WT_PCTL <- (pnorm(NHANES_WT_Z))*100

#NHANES_BMI_Z_SCORE
NHANES_BMI_Z <- (BMI-MEAN_NHANES_BMI)/SD_NHANES_BMI

#NHANES_BMI_PERCENTILE
NHANES_BMI_PCTL <- (pnorm(NHANES_BMI_Z))*100

#NHANES_SUBSCAPULAR_SKINFOLD_Z_SCORE
NHANES_SSF_Z <- (Anthropometrics$SSF-MEAN_NHANES_SSF)/SD_NHANES_SSF

#NHANES_SUBSCAPULAR_SKINFOLD_PERCENTILE
NHANES_SSF_PCTL<- (pnorm(NHANES_SSF_Z))*100

#NHANES_TRICEPS_SKINFOLD_Z_SCORE
NHANES_TSF_Z <- (Anthropometrics$TSF-MEAN_NHANES_TSF)/SD_NHANES_TSF

#NHANES_TRICEPS_SKINFOLD_PERCENTILE
NHANES_TSF_PCTL <- (pnorm(NHANES_TSF_Z))*100

#NHANES_UPPER_ARM_AREA_Z_SCORE
NHANES_UAA_Z <- (UAA-MEAN_NHANES_UAA)/SD_NHANES_UAA

#NHANES_UPPER_ARM_AREA_PERCENTILE
NHANES_UAA_PCTL <- (pnorm(NHANES_UAA_Z))*100

#NHANES_UPPER_ARM_CIRCUMFERANCE_Z_SCORE
NHANES_UAC_Z <- (Anthropometrics$UAC-MEAN_NHANES_UAC)/SD_NHANES_UAC

#NHANES_UPPER_ARM_CIRCUMFERANCE__PERCENTILE
NHANES_UAC_PCTL <- (pnorm(NHANES_UAC_Z))*100

#NHANES_UPPER_ARM_FAT_AREA_Z_SCORE
NHANES_AFA_Z <- (AFA-MEAN_NHANES_AFA)/SD_NHANES_AFA

#NHANES_UPPER_ARM_FAT_AREA_PERCENTILE
NHANES_AFA_PCTL <- (pnorm(NHANES_AFA_Z))*100

#NHANES_UPPER_ARM_MUSCLE_AREA_Z_SCORE
NHANES_AMA_Z <- (AMA-MEAN_NHANES_AMA)/SD_NHANES_AMA

#NHANES_UPPER_ARM_MUSCLE_AREA_PERCENTILE
NHANES_AMA_PCTL <- (pnorm(NHANES_AMA_Z))*100

#NHANES_WAIST_CIRCUMFERENCE_Z_SCORE
NHANES_UC_Z1 <- (((Anthropometrics$UC/M_NHANES_UC)^L_NHANES_UC)-1)/(L_NHANES_UC*S_NHANES_UC)
NHANES_UC_Z2 <- (Anthropometrics$UC-MEAN_NHANES_UC)/SD_NHANES_UC
NHANES_UC_Z1[is.na(NHANES_UC_Z1)] <- " "
NHANES_UC_Z2[is.na(NHANES_UC_Z2)] <- " "
NHANES_UC_Z <- as.numeric(paste(NHANES_UC_Z1, NHANES_UC_Z2))

#NHANES_WAIST_CIRCUMFERENCE_PERCENTILE
NHANES_UC_PCTL <- (pnorm(NHANES_UC_Z))*100

#NHANES_WEIGHT_FOR_HEIGHT_Z_SCORE

NHANES_WT_HT_Z <- (Anthropometrics$WT-MEAN_NHANES_WT_FOR_HT)/SD_NHANES_WT_FOR_HT 


#NHANES_WEIGHT_FOR_HEIGHT_PERCENTILE
NHANES_WT_HT_PCTL <- (pnorm(NHANES_WT_HT_Z))*100



#table3 <- cbind(NHANES_HT_PCTL, NHANES_HT_Z, NHANES_WT_PCTL, NHANES_WT_Z, NHANES_BMI_PCTL, NHANES_BMI_Z, NHANES_UAC_PCTL, NHANES_UAC_Z, NHANES_TSF_PCTL, NHANES_TSF_Z, NHANES_UAA_PCTL, NHANES_UAA_Z, NHANES_AMA_PCTL, NHANES_AMA_Z, NHANES_AFA_PCTL, NHANES_AFA_Z, NHANES_SSF_PCTL, NHANES_SSF_Z, NHANES_UC_PCTL, NHANES_UC_Z, NHANES_WT_HT_Z, NHANES_WT_HT_PCTL)


#CDC
#same SEX as NHANES, need to calculate Age in months
i=1:dim(Anthropometrics)[1]
Bday <- as.Date(Demographics.Identified$DOB[1], format= "%m/%d/%Y") #First convert classes from factor to date
Date <- as.Date(Anthropometrics$DATE[i], format = "%m/%d/%Y")
span <- interval(Bday, Date)
AGE <- as.period(span, "months")
AGE <- as.numeric(month(AGE))
#to remove unwanted variables
rm(i, Bday, span)


#LMS for CDC

L_CDC_WT <- c()
M_CDC_WT <- c()
S_CDC_WT <- c()
L_CDC_HT <- c()
M_CDC_HT <- c()
S_CDC_HT <- c()
L_CDC_BMI <- c()
M_CDC_BMI <- c()
S_CDC_BMI <- c()
L_CDC_WT_HT <- c()
M_CDC_WT_HT <- c()
S_CDC_WT_HT <- c()
L_CDC_HC <- c()
M_CDC_HC <- c()
S_CDC_HC <- c()

for (i in seq(length(AGE))) {
  
  L_CDC_WT <- c(L_CDC_WT, subset(CDC.References, CDC.References$AGE_MO_CDC_WT_AGE==AGE[i] & CDC.References$SEX_CDC_WT_AGE==SEX, select=c("L_CDC_WT_AGE")))
  M_CDC_WT <- c(M_CDC_WT, subset(CDC.References, CDC.References$AGE_MO_CDC_WT_AGE==AGE[i] & CDC.References$SEX_CDC_WT_AGE==SEX, select=c("M_CDC_WT_AGE")))
  S_CDC_WT <- c(S_CDC_WT, subset(CDC.References, CDC.References$AGE_MO_CDC_WT_AGE==AGE[i] & CDC.References$SEX_CDC_WT_AGE==SEX, select=c("S_CDC_WT_AGE")))
  L_CDC_HT <- c(L_CDC_HT, subset(CDC.References, CDC.References$AGE_MO_CDC_HT_AGE==AGE[i] & CDC.References$SEX_CDC_HT_AGE==SEX, select=c("L_CDC_HT_AGE")))
  M_CDC_HT <- c(M_CDC_HT, subset(CDC.References, CDC.References$AGE_MO_CDC_HT_AGE==AGE[i] & CDC.References$SEX_CDC_HT_AGE==SEX, select=c("M_CDC_HT_AGE")))
  S_CDC_HT <- c(S_CDC_HT, subset(CDC.References, CDC.References$AGE_MO_CDC_HT_AGE==AGE[i] & CDC.References$SEX_CDC_HT_AGE==SEX, select=c("S_CDC_HT_AGE")))
  L_CDC_BMI <- c(L_CDC_BMI, subset(CDC.References, CDC.References$AGE_MO_CDC_BMI_AGE==AGE[i] & CDC.References$SEX_CDC_BMI_AGE==SEX, select=c("L_CDC_BMI_AGE")))
  M_CDC_BMI <- c(M_CDC_BMI, subset(CDC.References, CDC.References$AGE_MO_CDC_BMI_AGE==AGE[i] & CDC.References$SEX_CDC_BMI_AGE==SEX, select=c("M_CDC_BMI_AGE")))
  S_CDC_BMI <- c(S_CDC_BMI, subset(CDC.References, CDC.References$AGE_MO_CDC_BMI_AGE==AGE[i] & CDC.References$SEX_CDC_BMI_AGE==SEX, select=c("S_CDC_BMI_AGE")))
  L_CDC_WT_HT <-c(L_CDC_WT_HT, subset(CDC.References, CDC.References$HEIGHT_CDC_WT_FOR_HT==HT[i] & CDC.References$SEX_CDC_WT_FOR_HT==SEX, select=c("L_CDC_WT_FOR_HT")))
  M_CDC_WT_HT <- c(M_CDC_WT_HT, subset(CDC.References, CDC.References$HEIGHT_CDC_WT_FOR_HT==HT[i] & CDC.References$SEX_CDC_WT_FOR_HT==SEX, select=c("M_CDC_WT_FOR_HT")))
  S_CDC_WT_HT <- c(S_CDC_WT_HT, subset(CDC.References, CDC.References$HEIGHT_CDC_WT_FOR_HT==HT[i] & CDC.References$SEX_CDC_WT_FOR_HT==SEX, select=c("S_CDC_WT_FOR_HT")))
  L_CDC_HC <- c(L_CDC_HC, subset(CDC.References, CDC.References$AGE_MO_CDC_HC_AGE==AGE[i] & CDC.References$SEX_CDC_HC_AGE==SEX, select=c("L_CDC_HC_AGE")))
  M_CDC_HC <- c(M_CDC_HC, subset(CDC.References, CDC.References$AGE_MO_CDC_HC_AGE==AGE[i] & CDC.References$SEX_CDC_HC_AGE==SEX, select=c("M_CDC_HC_AGE")))
  S_CDC_HC <- c(S_CDC_HC, subset(CDC.References, CDC.References$AGE_MO_CDC_HC_AGE==AGE[i] & CDC.References$SEX_CDC_HC_AGE==SEX, select=c("S_CDC_HC_AGE")))
}

L_CDC_HT <- as.numeric(L_CDC_HT)
M_CDC_HT <- as.numeric(M_CDC_HT)
S_CDC_HT <- as.numeric(S_CDC_HT)
L_CDC_WT <- as.numeric(L_CDC_WT)
M_CDC_WT <- as.numeric(M_CDC_WT)
S_CDC_WT <- as.numeric(S_CDC_WT)
L_CDC_BMI <- as.numeric(L_CDC_BMI)
M_CDC_BMI <- as.numeric(M_CDC_BMI)
S_CDC_BMI <- as.numeric(S_CDC_BMI)
L_CDC_WT_HT <- as.numeric(L_CDC_WT_HT)
M_CDC_WT_HT <- as.numeric(M_CDC_WT_HT)
S_CDC_WT_HT <- as.numeric(S_CDC_WT_HT)
L_CDC_HC <- as.numeric(L_CDC_HC)
M_CDC_HC <- as.numeric(M_CDC_HC)
S_CDC_HC <- as.numeric(S_CDC_HC)
#CDC_LMS <- cbind(AGE, L_CDC_HT, M_CDC_HT, S_CDC_HT, L_CDC_WT, M_CDC_WT, S_CDC_WT, L_CDC_BMI, M_CDC_BMI, S_CDC_BMI, L_CDC_WT_HT, M_CDC_WT_HT, S_CDC_WT_HT, L_CDC_HC, M_CDC_HC, S_CDC_HC)

#CDC_WEIGHT_FOR_HEIGHT_Z_SCORE
CDC_WT_HT_Z <- (((Anthropometrics$WT/M_CDC_WT_HT)^L_CDC_WT_HT)-1)/(L_CDC_WT_HT*S_CDC_WT_HT)

#CDC_WEIGHT_FOR_HEIGHT_PERCENTILE
CDC_WT_HT_PCTL <- (pnorm(CDC_WT_HT_Z))*100

#CDC_WEIGHT_Z_SCORE
CDC_WT_Z <- (((Anthropometrics$WT/M_CDC_WT)^L_CDC_WT)-1)/(L_CDC_WT*S_CDC_WT)

#CDC_WEIGHT_PERCENTILE
CDC_WT_PCTL <- (pnorm(CDC_WT_Z))*100

#CDC_BMI_Z_SCORE
CDC_BMI_Z <- (((BMI/M_CDC_BMI)^L_CDC_BMI)-1)/(L_CDC_BMI*S_CDC_BMI)

#CDC_BMI_PERCENTILE
CDC_BMI_PCTL <- (pnorm(CDC_BMI_Z))*100

#CDC_HEAD_CIRCUMFERENCE_Z_SCORE
CDC_HC_Z <- (((Anthropometrics$HC/M_CDC_HC)^L_CDC_HC)-1)/(L_CDC_HC*S_CDC_HC)

#CDC_HEAD_CIRCUMFERENCE_PERCENTILE
CDC_HC_PCTL <- (pnorm(CDC_HC_Z))*100

#CDC_HEIGHT_Z_SCORE
CDC_HT_Z <- (((Anthropometrics$HT/M_CDC_HT)^L_CDC_HT)-1)/(L_CDC_HT*S_CDC_HT)

#CDC_HEIGHT_PERCENTILE
CDC_HT_PCTL <- (pnorm(CDC_HT_Z))*100

#CDC_Z_AND_PCTL <- cbind(AGE, CDC_HT_Z, CDC_HT_PCTL, CDC_WT_Z, CDC_WT_PCTL, CDC_BMI_Z, CDC_BMI_PCTL, CDC_WT_HT_Z, CDC_WT_HT_PCTL, CDC_HC_Z, CDC_HC_PCTL)

#same SEX as NHANES, need to calculate Age in days
i=1:dim(Anthropometrics)[1]
Bday <- as.Date(Demographics.Identified$DOB[1], format= "%m/%d/%Y") #First convert classes from factor to date
Date <- as.Date(Anthropometrics$DATE[i], format = "%m/%d/%Y")
span <- interval(Bday, Date)
AGE_DOL <- as.period(span, "days")
AGE_DOL <- as.numeric(day(AGE_DOL))
#to remove unwanted variables
rm(i, Bday, span)

#HT_DAY for WHO, because need to have HT to the nearest tenth
Anthropometrics$ROUND_HT <- round(Anthropometrics$HT, digits=1)

#WHO References
L_WHO_HT <- c()
M_WHO_HT <- c()
S_WHO_HT <- c()
L_WHO_HT2 <- c()
M_WHO_HT2 <- c()
S_WHO_HT2 <- c()
L_WHO_BMI <- c()
M_WHO_BMI <- c()
S_WHO_BMI <- c()
L_WHO_BMI2 <- c()
M_WHO_BMI2<- c()
S_WHO_BMI2 <- c()
L_WHO_HC <- c()
M_WHO_HC <- c()
S_WHO_HC <- c()
L_WHO_SSF <- c()
M_WHO_SSF <- c()
S_WHO_SSF <- c()
L_WHO_TSF <- c()
M_WHO_TSF <- c()
S_WHO_TSF <- c()
L_WHO_UAC <- c()
M_WHO_UAC <- c()
S_WHO_UAC <- c()
L_WHO_WT <- c()
M_WHO_WT <- c()
S_WHO_WT <- c()
L_WHO_WT2 <- c()
M_WHO_WT2 <- c()
S_WHO_WT2 <- c()
L_WHO_WT_HT <- c()
M_WHO_WT_HT <- c()
S_WHO_WT_HT <- c()

for (i in seq(length(AGE_DOL))) {
  L_WHO_HC <- c(L_WHO_HC, subset(WHO.References, WHO.References$AGE_DAY_WHO_HC_AGE==AGE_DOL[i] & WHO.References$SEX_WHO_HC_AGE==SEX, select = c("L_WHO_HC_AGE")))
  M_WHO_HC <- c(M_WHO_HC, subset(WHO.References, WHO.References$AGE_DAY_WHO_HC_AGE==AGE_DOL[i] & WHO.References$SEX_WHO_HC_AGE==SEX, select = c("M_WHO_HC_AGE")))
  S_WHO_HC <- c(S_WHO_HC, subset(WHO.References, WHO.References$AGE_DAY_WHO_HC_AGE==AGE_DOL[i] & WHO.References$SEX_WHO_HC_AGE==SEX, select = c("S_WHO_HC_AGE")))
  L_WHO_SSF <- c(L_WHO_SSF, subset(WHO.References, WHO.References$AGE_DAY_WHO_SSF_AGE==AGE_DOL[i] & WHO.References$SEX_WHO_SSF_AGE==SEX, select=c("L_WHO_SSF_AGE")))
  M_WHO_SSF <- c(M_WHO_SSF, subset(WHO.References, WHO.References$AGE_DAY_WHO_SSF_AGE==AGE_DOL[i] & WHO.References$SEX_WHO_SSF_AGE==SEX, select=c("M_WHO_SSF_AGE")))
  S_WHO_SSF <- c(S_WHO_SSF, subset(WHO.References, WHO.References$AGE_DAY_WHO_SSF_AGE==AGE_DOL[i] & WHO.References$SEX_WHO_SSF_AGE==SEX, select=c("S_WHO_SSF_AGE")))
  L_WHO_TSF <- c(L_WHO_TSF, subset(WHO.References, WHO.References$AGE_DAY_WHO_TSF_AGE==AGE_DOL[i] & WHO.References$SEX_WHO_TSF_AGE==SEX, select=c("L_WHO_TSF_AGE")))
  M_WHO_TSF <- c(M_WHO_TSF, subset(WHO.References, WHO.References$AGE_DAY_WHO_TSF_AGE==AGE_DOL[i] & WHO.References$SEX_WHO_TSF_AGE==SEX, select=c("M_WHO_TSF_AGE")))
  S_WHO_TSF <- c(S_WHO_TSF, subset(WHO.References, WHO.References$AGE_DAY_WHO_TSF_AGE==AGE_DOL[i] & WHO.References$SEX_WHO_TSF_AGE==SEX, select=c("S_WHO_TSF_AGE")))
  L_WHO_UAC <- c(L_WHO_UAC, subset(WHO.References, WHO.References$AGE_DAY_WHO_UAC_AGE==AGE_DOL[i] & WHO.References$SEX_WHO_UAC_AGE==SEX, select=c("L_WHO_UAC_AGE")))
  M_WHO_UAC <- c(M_WHO_UAC, subset(WHO.References, WHO.References$AGE_DAY_WHO_UAC_AGE==AGE_DOL[i] & WHO.References$SEX_WHO_UAC_AGE==SEX, select=c("M_WHO_UAC_AGE")))
  S_WHO_UAC <- c(S_WHO_UAC, subset(WHO.References, WHO.References$AGE_DAY_WHO_UAC_AGE==AGE_DOL[i] & WHO.References$SEX_WHO_UAC_AGE==SEX, select=c("S_WHO_UAC_AGE")))
  L_WHO_WT_HT <- c(L_WHO_WT_HT, subset(WHO.References, WHO.References$HEIGHT_WHO_WT_FOR_HT==Anthropometrics$ROUND_HT[i] & WHO.References$SEX_WHO_WT_FOR_HT==SEX, select=c("L_WHO_WT_FOR_HT")))
  M_WHO_WT_HT <- c(M_WHO_WT_HT, subset(WHO.References, WHO.References$HEIGHT_WHO_WT_FOR_HT==Anthropometrics$ROUND_HT[i] & WHO.References$SEX_WHO_WT_FOR_HT==SEX, select=c("M_WHO_WT_FOR_HT")))
  S_WHO_WT_HT <- c(S_WHO_WT_HT, subset(WHO.References, WHO.References$HEIGHT_WHO_WT_FOR_HT==Anthropometrics$ROUND_HT[i] & WHO.References$SEX_WHO_WT_FOR_HT==SEX, select=c("S_WHO_WT_FOR_HT")))
}

#For WHO parameters which can be calculated in days (0-1856) or months (61+)
a <- (AGE_DOL <= 1856)
AGE_WHO <- ifelse(AGE_DOL <= 1856 , AGE_DOL, AGE)

#find appropriate references for WHO height based on age
for (i in which(AGE_DOL <= 1856)) {
  L_WHO_HT2 <- c(L_WHO_HT2, subset(WHO.References, WHO.References$AGE_DAY_WHO_HT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_HT_AGE2==SEX, select=c("L_WHO_HT_AGE2")))
  M_WHO_HT2 <- c(M_WHO_HT2, subset(WHO.References, WHO.References$AGE_DAY_WHO_HT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_HT_AGE2==SEX, select=c("M_WHO_HT_AGE2")))
  S_WHO_HT2 <- c(S_WHO_HT2, subset(WHO.References, WHO.References$AGE_DAY_WHO_HT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_HT_AGE2==SEX, select=c("S_WHO_HT_AGE2")))
  
}

for (i in which(!(AGE_DOL <= 1856))) {
  L_WHO_HT <- c(L_WHO_HT, subset(WHO.References, WHO.References$AGE_MO_WHO_HT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_HT_AGE==SEX, select=c("L_WHO_HT_AGE")))
  M_WHO_HT <- c(M_WHO_HT, subset(WHO.References, WHO.References$AGE_MO_WHO_HT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_HT_AGE==SEX, select=c("M_WHO_HT_AGE")))
  S_WHO_HT <- c(S_WHO_HT, subset(WHO.References, WHO.References$AGE_MO_WHO_HT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_HT_AGE==SEX, select=c("S_WHO_HT_AGE")))
}

#find appropriate references for WHO weight based on age
for (i in which(AGE_DOL <= 1856)) {
  L_WHO_WT2 <- c(L_WHO_WT2, subset(WHO.References, WHO.References$AGE_DAY_WHO_WT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_WT_AGE2==SEX, select=c("L_WHO_WT_AGE2")))
  M_WHO_WT2 <- c(M_WHO_WT2, subset(WHO.References, WHO.References$AGE_DAY_WHO_WT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_WT_AGE2==SEX, select=c("M_WHO_WT_AGE2")))
  S_WHO_WT2 <- c(S_WHO_WT2, subset(WHO.References, WHO.References$AGE_DAY_WHO_WT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_WT_AGE2==SEX, select=c("S_WHO_WT_AGE2")))
  
}

for (i in which(!(AGE_DOL <= 1856))) {
  L_WHO_WT <- c(L_WHO_WT, subset(WHO.References, WHO.References$AGE_MO_WHO_WT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_WT_AGE==SEX, select=c("L_WHO_WT_AGE")))
  M_WHO_WT <- c(M_WHO_WT, subset(WHO.References, WHO.References$AGE_MO_WHO_WT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_WT_AGE==SEX, select=c("M_WHO_WT_AGE")))
  S_WHO_WT <- c(S_WHO_WT, subset(WHO.References, WHO.References$AGE_MO_WHO_WT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_WT_AGE==SEX, select=c("S_WHO_WT_AGE")))
}

#find appropriate references for WHO BMI based on age
for (i in which(AGE_DOL <= 1856)) {
  L_WHO_BMI2 <- c(L_WHO_BMI2, subset(WHO.References, WHO.References$AGE_DAY_WHO_BMI_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_BMI_AGE2==SEX, select=c("L_WHO_BMI_AGE2")))
  M_WHO_BMI2 <- c(M_WHO_BMI2, subset(WHO.References, WHO.References$AGE_DAY_WHO_BMI_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_BMI_AGE2==SEX, select=c("M_WHO_BMI_AGE2")))
  S_WHO_BMI2 <- c(S_WHO_BMI2, subset(WHO.References, WHO.References$AGE_DAY_WHO_BMI_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_BMI_AGE2==SEX, select=c("S_WHO_BMI_AGE2")))
  
}

for (i in which(!(AGE_DOL <= 1856))) {
  L_WHO_BMI <- c(L_WHO_BMI, subset(WHO.References, WHO.References$AGE_MO_WHO_BMI_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_BMI_AGE==SEX, select=c("L_WHO_BMI_AGE")))
  M_WHO_BMI <- c(M_WHO_BMI, subset(WHO.References, WHO.References$AGE_MO_WHO_BMI_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_BMI_AGE==SEX, select=c("M_WHO_BMI_AGE")))
  S_WHO_BMI <- c(S_WHO_BMI, subset(WHO.References, WHO.References$AGE_MO_WHO_BMI_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_BMI_AGE==SEX, select=c("S_WHO_BMI_AGE")))
}


L_WHO_HT <- as.numeric(L_WHO_HT)
M_WHO_HT <- as.numeric(M_WHO_HT)
S_WHO_HT <- as.numeric(S_WHO_HT)
L_WHO_HT2 <- as.numeric(L_WHO_HT2)
M_WHO_HT2 <- as.numeric(M_WHO_HT2)
S_WHO_HT2 <- as.numeric(S_WHO_HT2)
L_WHO_BMI <- as.numeric(L_WHO_BMI)
M_WHO_BMI <- as.numeric(M_WHO_BMI)
S_WHO_BMI <- as.numeric(S_WHO_BMI)
L_WHO_BMI2 <- as.numeric(L_WHO_BMI2)
M_WHO_BMI2 <- as.numeric(M_WHO_BMI2)
S_WHO_BMI2 <- as.numeric(S_WHO_BMI2)
L_WHO_HC <- as.numeric(L_WHO_HC)
M_WHO_HC <- as.numeric(M_WHO_HC)
S_WHO_HC <- as.numeric(S_WHO_HC)
L_WHO_SSF <- as.numeric(L_WHO_SSF)
M_WHO_SSF <- as.numeric(M_WHO_SSF)
S_WHO_SSF <- as.numeric(S_WHO_SSF)
L_WHO_TSF <- as.numeric(L_WHO_TSF)
M_WHO_TSF <- as.numeric(M_WHO_TSF)
S_WHO_TSF <- as.numeric(S_WHO_TSF)
L_WHO_UAC <- as.numeric(L_WHO_UAC)
M_WHO_UAC <- as.numeric(M_WHO_UAC)
S_WHO_UAC <- as.numeric(S_WHO_UAC)
L_WHO_WT <- as.numeric(L_WHO_WT)
M_WHO_WT <- as.numeric(M_WHO_WT)
S_WHO_WT <- as.numeric(S_WHO_WT)
L_WHO_WT2 <- as.numeric(L_WHO_WT2)
M_WHO_WT2 <- as.numeric(M_WHO_WT2)
S_WHO_WT2 <- as.numeric(S_WHO_WT2)
L_WHO_WT_HT <- as.numeric(L_WHO_WT_HT)
M_WHO_WT_HT <- as.numeric(M_WHO_WT_HT)
S_WHO_WT_HT <- as.numeric(S_WHO_WT_HT)

#for some reason ifelse does not work in this case
#so instead convert true/false to numeric, and replace


b <- as.numeric(a)
b[b==0] <- L_WHO_WT
b[b==1] <- NA
L_WHO_WT <- b
b <- as.numeric(a)
b[b==0] <- M_WHO_WT
b[b==1] <- NA
M_WHO_WT <- b
b <- as.numeric(a)
b[b==0] <- S_WHO_WT
b[b==1] <- NA
S_WHO_WT <- b
b <- as.numeric(a)
b[b==1] <- L_WHO_WT2
b[b==0] <- NA
L_WHO_WT2 <- b
b <- as.numeric(a)
b[b==1] <- M_WHO_WT2
b[b==0] <- NA
M_WHO_WT2 <- b
b <- as.numeric(a)
b[b==1] <- S_WHO_WT2
b[b==0] <- NA
S_WHO_WT2 <- b

b <- as.numeric(a)
b[b==0] <- L_WHO_BMI
b[b==1] <- NA
L_WHO_BMI <- b
b <- as.numeric(a)
b[b==0] <- M_WHO_BMI
b[b==1] <- NA
M_WHO_BMI <- b
b <- as.numeric(a)
b[b==0] <- S_WHO_BMI
b[b==1] <- NA
S_WHO_BMI <- b
b <- as.numeric(a)
b[b==1] <- L_WHO_BMI2
b[b==0] <- NA
L_WHO_BMI2 <- b
b <- as.numeric(a)
b[b==1] <- M_WHO_BMI2
b[b==0] <- NA
M_WHO_BMI2 <- b
b <- as.numeric(a)
b[b==1] <- S_WHO_BMI2
b[b==0] <- NA
S_WHO_BMI2 <- b


b <- as.numeric(a)
b[b==1] <- 2
b[b==0] <- L_WHO_HT
b[b==2] <- NA
L_WHO_HT <- b
b <- as.numeric(a)
b[b==1] <- 2
b[b==0] <- M_WHO_HT
b[b==2] <- NA
M_WHO_HT <- b
b <- as.numeric(a)
b[b==1] <- 2
b[b==0] <- S_WHO_HT
b[b==2] <- NA
S_WHO_HT <- b
b <- as.numeric(a)
b[b==1] <- 2
b[b==2] <- L_WHO_HT2
b[b==0] <- NA
L_WHO_HT2 <- b
b <- as.numeric(a)
b[b==1] <- 2
b[b==2] <- M_WHO_HT2
b[b==0] <- NA
M_WHO_HT2 <- b
b <- as.numeric(a)
b[b==1] <- 2
b[b==2] <- S_WHO_HT2
b[b==0] <- NA
S_WHO_HT2 <- b


#WHO_LMS <- cbind(AGE_MO,AGE_DOL,L_WHO_HT, M_WHO_HT, S_WHO_HT,L_WHO_HT2, M_WHO_HT2, S_WHO_HT2, L_WHO_WT, M_WHO_WT, S_WHO_WT,L_WHO_WT2, M_WHO_WT2, S_WHO_WT2, L_WHO_BMI, M_WHO_BMI, S_WHO_BMI,L_WHO_BMI2, M_WHO_BMI2, S_WHO_BMI2, L_WHO_HC, M_WHO_HC, S_WHO_HC, L_WHO_UAC, M_WHO_UAC, S_WHO_UAC, L_WHO_TSF, M_WHO_TSF, S_WHO_TSF, L_WHO_SSF, M_WHO_SSF, S_WHO_SSF, L_WHO_WT_HT, M_WHO_WT_HT, S_WHO_WT_HT)

#WHO_WEIGHT_Z_SCORE
WHO_WT_Z1 <- (((Anthropometrics$WT/M_WHO_WT)^L_WHO_WT)-1)/(L_WHO_WT*S_WHO_WT)
WHO_WT_Z2 <- (((Anthropometrics$WT/M_WHO_WT2)^L_WHO_WT2)-1)/(L_WHO_WT2*S_WHO_WT2)
WHO_WT_Z1[is.na(WHO_WT_Z1)] <- " "
WHO_WT_Z2[is.na(WHO_WT_Z2)] <- " "
WHO_WT_Z <- as.numeric(paste(WHO_WT_Z1, WHO_WT_Z2))

#WHO_WEIGHT_PERCENTILE
WHO_WT_PCTL <- (pnorm(WHO_WT_Z))*100

#WHO_BMI_Z_SCORE
WHO_BMI_Z1 <- (((BMI/M_WHO_BMI)^L_WHO_BMI)-1)/(L_WHO_BMI*S_WHO_BMI)
WHO_BMI_Z2 <- (((BMI/M_WHO_BMI2)^L_WHO_BMI2)-1)/(L_WHO_BMI2*S_WHO_BMI2)
WHO_BMI_Z1[is.na(WHO_BMI_Z1)] <- " "
WHO_BMI_Z2[is.na(WHO_BMI_Z2)] <- " "
WHO_BMI_Z <- as.numeric(paste(WHO_BMI_Z1, WHO_BMI_Z2))

#WHO_BMI_PERCENTILE
WHO_BMI_PCTL <- (pnorm(WHO_BMI_Z))*100

#WHO_HEAD_CIRCUMFERENCE_Z_SCORE
WHO_HC_Z <- (((Anthropometrics$HC/M_WHO_HC)^L_WHO_HC)-1)/(L_WHO_HC*S_WHO_HC)

#WHO_HEAD_CIRCUMFERENCE_PERCENTILE
WHO_HC_PCTL <- (pnorm(WHO_HC_Z))*100

#WHO_HEIGHT_Z_SCORE
WHO_HT_Z1 <- (((Anthropometrics$HT/M_WHO_HT)^L_WHO_HT)-1)/(L_WHO_HT*S_WHO_HT)
WHO_HT_Z2 <- (((Anthropometrics$HT/M_WHO_HT2)^L_WHO_HT2)-1)/(L_WHO_HT2*S_WHO_HT2)
WHO_HT_Z1[is.na(WHO_HT_Z1)] <- " "
WHO_HT_Z2[is.na(WHO_HT_Z2)] <- " "
WHO_HT_Z <- as.numeric(paste(WHO_HT_Z1, WHO_HT_Z2))

#WHO_HEIGHT_PERCENTILE
WHO_HT_PCTL <- (pnorm(WHO_HT_Z))*100

#WHO_SUBSCAPULAR_SKINFOLD_Z_SCORE
WHO_SSF_Z <- (((Anthropometrics$SSF/M_WHO_SSF)^L_WHO_SSF)-1)/(L_WHO_SSF*S_WHO_SSF)

#WHO_SUBSCAPULAR_SKINFOLD_PERCENTILE
WHO_SSF_PCTL <- (pnorm(WHO_SSF_Z))*100

#WHO_TRICEPS_SKINFOLD_Z_SCORE
WHO_TSF_Z <- (((Anthropometrics$TSF/M_WHO_TSF)^L_WHO_TSF)-1)/(L_WHO_TSF*S_WHO_TSF)

#WHO_TRICEPS_SKINFOLD_PERCENTILE
WHO_TSF_PCTL <- (pnorm(WHO_TSF_Z))*100

#WHO_UPPER_ARM_CIRCUMFERANCE_Z_SCORE
WHO_UAC_Z <- (((Anthropometrics$UAC/M_WHO_UAC)^L_WHO_UAC)-1)/(L_WHO_UAC*S_WHO_UAC)

#WHO_UPPER_ARM_CIRCUMFERANCE__PERCENTILE
WHO_UAC_PCTL <- (pnorm(WHO_UAC_Z))*100

#WHO_WEIGHT_FOR_HEIGHT_Z_SCORE
WHO_WT_HT_Z <- (((Anthropometrics$WT/M_WHO_WT_HT)^L_WHO_WT_HT)-1)/(L_WHO_WT_HT*S_WHO_WT_HT)

#WHO_WEIGHT_FOR_HEIGHT_PERCENTILE
WHO_WT_HT_PCTL <- (pnorm(WHO_WT_HT_Z))*100

#WHO_Z_AND_PCTL <- cbind(WHO_HT_PCTL, WHO_HT_Z, WHO_WT_PCTL, WHO_WT_Z, WHO_BMI_PCTL, WHO_BMI_Z, WHO_WT_HT_PCTL, WHO_WT_HT_Z, WHO_HC_PCTL, WHO_HC_Z, WHO_UAC_PCTL, WHO_UAC_Z, WHO_TSF_PCTL, WHO_TSF_Z, WHO_SSF_PCTL, WHO_SSF_Z)     


#Gator Circle
GC <- (NHANES_UC_Z* VC_PCTG)/100

#define column headers for excel sheet
MRNUMBER <- Anthropometrics$MRNUMBER
DATE <- as.Date(Anthropometrics$DATE, format= "%m/%d/%Y")
DAY_TYPE <- Anthropometrics$DAY_TYPE
SOURCE <- Anthropometrics$SOURCE
HT <- Anthropometrics$HT
WT <- Anthropometrics$WT
HC <- Anthropometrics$HC
UAC <- Anthropometrics$UAC
TSF <- Anthropometrics$TSF
SSF <- Anthropometrics$SSF
USF <- Anthropometrics$USF
SISF <- Anthropometrics$SISF
MBSF <- Anthropometrics$MBSF
UC <- Anthropometrics$UC
R <- Anthropometrics$R
X <- Anthropometrics$X
CP_DAY <- Anthropometrics$CP
PA_DAY <- Anthropometrics$PA
COMMENTS <- Anthropometrics$COMMENTS

anthrotable <- cbind.data.frame(MRNUMBER, DATE, DAY_TYPE, SOURCE, HT, WT, HC, UAC, TSF, SSF, USF, SISF, MBSF, UC, R, X, CDC_HT_PCTL, CDC_HT_Z, WHO_HT_PCTL, WHO_HT_Z, NHANES_HT_PCTL, NHANES_HT_Z, CDC_WT_PCTL, CDC_WT_Z, WHO_WT_PCTL, WHO_WT_Z, NHANES_WT_PCTL, NHANES_WT_Z, BMI,  CDC_BMI_PCTL, CDC_BMI_Z, WHO_BMI_PCTL, WHO_BMI_Z, NHANES_BMI_PCTL, NHANES_BMI_Z, CDC_WT_HT_PCTL, CDC_WT_HT_Z, WHO_WT_HT_PCTL, WHO_WT_HT_Z, NHANES_WT_HT_PCTL, NHANES_WT_HT_Z, CDC_HC_PCTL, CDC_HC_Z, WHO_HC_PCTL, WHO_HC_Z, WHO_UAC_PCTL, WHO_UAC_Z, NHANES_UAC_PCTL, NHANES_UAC_Z, WHO_TSF_PCTL, WHO_TSF_Z, NHANES_TSF_PCTL, NHANES_TSF_Z, UAA, NHANES_UAA_PCTL, NHANES_UAA_Z, AMC, AMA, NHANES_AMA_PCTL, NHANES_AMA_Z, AFA, NHANES_AFA_PCTL, NHANES_AFA_Z, WHO_SSF_PCTL, WHO_SSF_Z, NHANES_SSF_PCTL, NHANES_SSF_Z, NHANES_UC_PCTL, NHANES_UC_Z, VCA, VC_PCTG, GC, Z, P, ARPADI_FFM, GORAN_FFM, ARPADI_TBW, SCHAEFER_FFM, KOTLER_FFM, BODY_FAT_PCTG, CP_DAY, PA_DAY, COMMENTS)
output <- cbind.data.frame(DATE, AGE_YEARS, AGE_MO, AGE_DOL, RACE, SEX, HT1, MEAN_NHANES_SSF, SD_NHANES_SSF, MEAN_NHANES_AFA, SD_NHANES_AFA, MEAN_NHANES_AMA, SD_NHANES_AMA, MEAN_NHANES_BMI, SD_NHANES_BMI, MEAN_NHANES_HT, SD_NHANES_HT, MEAN_NHANES_TSF, SD_NHANES_TSF, MEAN_NHANES_UAC, SD_NHANES_UAC, MEAN_NHANES_UAA, SD_NHANES_UAA, MEAN_NHANES_UC, SD_NHANES_UC, L_NHANES_UC, M_NHANES_UC, S_NHANES_UC, MEAN_NHANES_WT, SD_NHANES_WT, MEAN_NHANES_WT_FOR_HT, SD_NHANES_WT_FOR_HT,
                           L_CDC_HT, M_CDC_HT, S_CDC_HT, L_CDC_WT, M_CDC_WT, S_CDC_WT, L_CDC_BMI, M_CDC_BMI, S_CDC_BMI, L_CDC_WT_HT, M_CDC_WT_HT, S_CDC_WT_HT, L_CDC_HC, M_CDC_HC, S_CDC_HC,
                           L_WHO_HT, M_WHO_HT, S_WHO_HT,L_WHO_HT2, M_WHO_HT2, S_WHO_HT2, L_WHO_WT, M_WHO_WT, S_WHO_WT,L_WHO_WT2, M_WHO_WT2, S_WHO_WT2, L_WHO_BMI, M_WHO_BMI, S_WHO_BMI,L_WHO_BMI2, M_WHO_BMI2, S_WHO_BMI2, L_WHO_HC, M_WHO_HC, S_WHO_HC, L_WHO_UAC, M_WHO_UAC, S_WHO_UAC, L_WHO_TSF, M_WHO_TSF, S_WHO_TSF, L_WHO_SSF, M_WHO_SSF, S_WHO_SSF, L_WHO_WT_HT, M_WHO_WT_HT, S_WHO_WT_HT)



#Interpolation
anthro <- Anthropometrics
demo <- Demographics.Identified
prodate <- Demographics.Identified$PKT_PROSPECTIVE_DATE
naive <- ifelse(Demographics.Identified$STRATA[1] == "N", TRUE, FALSE)


if (naive == TRUE) {
  sub <- anthro[anthro$SOURCE!=1,]
  anthro <- anthro[!(as.numeric(rownames(anthro)) %in% as.numeric(rownames(sub))),]
} else if (naive == FALSE) {
  sub1 <- anthro[anthro$DATE>=prodate & anthro$SOURCE!=1,]
  sub2 <- anthro[anthro$DATE<prodate & (anthro$SOURCE==3),]
  anthro <- anthro[!(as.numeric(rownames(anthro)) %in% as.numeric(rownames(sub1))),]
  anthro <- anthro[!(as.numeric(rownames(anthro)) %in% as.numeric(rownames(sub2))),]
}

y1 <- as.Date(anthro$DATE[1], format="%m/%d/%Y")
y2 <- as.Date(anthro$DATE[length(anthro[,1])], format="%m/%d/%Y")
DATE <- seq(y1, y2, by="days")


#HT
table <- anthro[complete.cases(anthro$HT),]
HT <- as.numeric(na.omit(anthro$HT))
i <- 1:(length(HT)-1)
x1 <- HT[i]
x2 <- HT[i+1]
diffheight <- (x2 - x1)
Date1 <- as.Date(table$DATE[i], format="%m/%d/%Y")
Date2 <- as.Date(table$DATE[i+1], format="%m/%d/%Y")
x <- Date2 - Date1
difftotal1 <- as.numeric(x, units="days")

HT_DAY <- c()
for (i in seq(length(HT)-1)) {
  HT_DAY <- c(HT_DAY, ((1:difftotal1[i]/difftotal1[i])*diffheight[i])+x1[i])
}
HT_DAY <- c(table$HT[1], HT_DAY)

#WT
table <- anthro[complete.cases(anthro$WT),]
y <- as.numeric(na.omit(anthro$WT))
i <- 1:(length(y)-1)
x1 <- y[i]
x2 <- y[i+1]
diffx <- (x2 - x1)
Date1 <- as.Date(table$DATE[i], format="%m/%d/%Y")
Date2 <- as.Date(table$DATE[i+1], format="%m/%d/%Y")
x <- Date2 - Date1
difftotal <- as.numeric(x, units="days")

WT_DAY <- c()
for (i in seq(length(y)-1)) {
  WT_DAY <- c(WT_DAY, ((1:difftotal[i]/difftotal[i])*diffx[i])+x1[i])
}
WT_DAY <- c(table$WT[1], WT_DAY)

#HC
table <- anthro[complete.cases(anthro$HC),]
y <- as.numeric(na.omit(anthro$HC))
i <- 1:(length(y)-1)
x1 <- y[i]
x2 <- y[i+1]
diffx <- (x2 - x1)
Date1 <- as.Date(table$DATE[i], format="%m/%d/%Y")
Date2 <- as.Date(table$DATE[i+1], format="%m/%d/%Y")
x <- Date2 - Date1
difftotal <- as.numeric(x, units="days")

HC_DAY <- c()
for (i in seq(length(y)-1)) {
  HC_DAY <- c(HC_DAY, ((1:difftotal[i]/difftotal[i])*diffx[i])+x1[i])
}
HC_DAY <- c(table$HC[1], HC_DAY)

#UAC
table <- anthro[complete.cases(anthro$UAC),]
y <- as.numeric(na.omit(anthro$UAC))
i <- 1:(length(y)-1)
x1 <- y[i]
x2 <- y[i+1]
diffx <- (x2 - x1)
Date1 <- as.Date(table$DATE[i], format="%m/%d/%Y")
Date2 <- as.Date(table$DATE[i+1], format="%m/%d/%Y")
x <- Date2 - Date1
difftotal <- as.numeric(x, units="days")

UAC_DAY <- c()
for (i in seq(length(y)-1)) {
  UAC_DAY <- c(UAC_DAY, ((1:difftotal[i]/difftotal[i])*diffx[i])+x1[i])
}
UAC_DAY <- c(table$UAC[1], UAC_DAY)

#TSF
table <- anthro[complete.cases(anthro$TSF),]
y <- as.numeric(na.omit(anthro$TSF))
i <- 1:(length(y)-1)
x1 <- y[i]
x2 <- y[i+1]
diffx <- (x2 - x1)
Date1 <- as.Date(table$DATE[i], format="%m/%d/%Y")
Date2 <- as.Date(table$DATE[i+1], format="%m/%d/%Y")
x <- Date2 - Date1
difftotal <- as.numeric(x, units="days")

TSF_DAY <- c()
for (i in seq(length(y)-1)) {
  TSF_DAY <- c(TSF_DAY, ((1:difftotal[i]/difftotal[i])*diffx[i])+x1[i])
}
TSF_DAY <- c(table$TSF[1], TSF_DAY)

#SSF
table <- anthro[complete.cases(anthro$SSF),]
y <- as.numeric(na.omit(anthro$SSF))
i <- 1:(length(y)-1)
x1 <- y[i]
x2 <- y[i+1]
diffx <- (x2 - x1)
Date1 <- as.Date(table$DATE[i], format="%m/%d/%Y")
Date2 <- as.Date(table$DATE[i+1], format="%m/%d/%Y")
x <- Date2 - Date1
difftotal <- as.numeric(x, units="days")

SSF_DAY <- c()
for (i in seq(length(y)-1)) {
  SSF_DAY <- c(SSF_DAY, ((1:difftotal[i]/difftotal[i])*diffx[i])+x1[i])
}
SSF_DAY <- c(table$SSF[1], SSF_DAY)

#USF
table <- anthro[complete.cases(anthro$USF),]
y <- as.numeric(na.omit(anthro$USF))
i <- 1:(length(y)-1)
x1 <- y[i]
x2 <- y[i+1]
diffx <- (x2 - x1)
Date1 <- as.Date(table$DATE[i], format="%m/%d/%Y")
Date2 <- as.Date(table$DATE[i+1], format="%m/%d/%Y")
x <- Date2 - Date1
difftotal <- as.numeric(x, units="days")

USF_DAY <- c()
for (i in seq(length(y)-1)) {
  USF_DAY <- c(USF_DAY, ((1:difftotal[i]/difftotal[i])*diffx[i])+x1[i])
}
USF_DAY <- c(table$USF[1], USF_DAY)

#SISF
table <- anthro[complete.cases(anthro$SISF),]
y <- as.numeric(na.omit(anthro$SISF))
i <- 1:(length(y)-1)
x1 <- y[i]
x2 <- y[i+1]
diffx <- (x2 - x1)
Date1 <- as.Date(table$DATE[i], format="%m/%d/%Y")
Date2 <- as.Date(table$DATE[i+1], format="%m/%d/%Y")
x <- Date2 - Date1
difftotal <- as.numeric(x, units="days")

SISF_DAY <- c()
for (i in seq(length(y)-1)) {
  SISF_DAY <- c(SISF_DAY, ((1:difftotal[i]/difftotal[i])*diffx[i])+x1[i])
}
SISF_DAY <- c(table$SISF[1], SISF_DAY)

#MBSF
table <- anthro[complete.cases(anthro$MBSF),]
y <- as.numeric(na.omit(anthro$MBSF))
i <- 1:(length(y)-1)
x1 <- y[i]
x2 <- y[i+1]
diffx <- (x2 - x1)
Date1 <- as.Date(table$DATE[i], format="%m/%d/%Y")
Date2 <- as.Date(table$DATE[i+1], format="%m/%d/%Y")
x <- Date2 - Date1
difftotal <- as.numeric(x, units="days")

MBSF_DAY <- c()
for (i in seq(length(y)-1)) {
  MBSF_DAY <- c(MBSF_DAY, ((1:difftotal[i]/difftotal[i])*diffx[i])+x1[i])
}
MBSF_DAY <- c(table$MBSF[1], MBSF_DAY)

#UC
table <- anthro[complete.cases(anthro$UC),]
y <- as.numeric(na.omit(anthro$UC))
i <- 1:(length(y)-1)
x1 <- y[i]
x2 <- y[i+1]
diffx <- (x2 - x1)
Date1 <- as.Date(table$DATE[i], format="%m/%d/%Y")
Date2 <- as.Date(table$DATE[i+1], format="%m/%d/%Y")
x <- Date2 - Date1
difftotal <- as.numeric(x, units="days")

UC_DAY <- c()
for (i in seq(length(y)-1)) {
  UC_DAY <- c(UC_DAY, ((1:difftotal[i]/difftotal[i])*diffx[i])+x1[i])
}
UC_DAY <- c(table$UC[1], UC_DAY)

#R
table <- anthro[complete.cases(anthro$R),]
y <- as.numeric(na.omit(anthro$R))
i <- 1:(length(y)-1)
x1 <- y[i]
x2 <- y[i+1]
diffx <- (x2 - x1)
Date1 <- as.Date(table$DATE[i], format="%m/%d/%Y")
Date2 <- as.Date(table$DATE[i+1], format="%m/%d/%Y")
x <- Date2 - Date1
difftotal <- as.numeric(x, units="days")

R_DAY <- c()
for (i in seq(length(y)-1)) {
  R_DAY <- c(R_DAY, ((1:difftotal[i]/difftotal[i])*diffx[i])+x1[i])
}
R_DAY <- c(table$R[1], R_DAY)

#X
table <- anthro[complete.cases(anthro$X),]
y <- as.numeric(na.omit(anthro$X))
i <- 1:(length(y)-1)
x1 <- y[i]
x2 <- y[i+1]
diffx <- (x2 - x1)
Date1 <- as.Date(table$DATE[i], format="%m/%d/%Y")
Date2 <- as.Date(table$DATE[i+1], format="%m/%d/%Y")
x <- Date2 - Date1
difftotal <- as.numeric(x, units="days")

X_DAY <- c()
for (i in seq(length(y)-1)) {
  X_DAY <- c(X_DAY, ((1:difftotal[i]/difftotal[i])*diffx[i])+x1[i])
}
X_DAY <- c(table$X[1], X_DAY)

rm(y,i,x1,x2,diffx, Date1, Date2, x, difftotal, y1, y2, diffheight, difftotal1, HT)

#for length of interpolated data so it can fit in the anthro_interpolation table
z <- length(DATE)
a <- which(!is.na(anthro$HT))[1]
b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
c <- which(DATE==b)
HT_DAY <- c(rep(NA, c-1), HT_DAY)
length(HT_DAY) <- z


a <- which(!is.na(anthro$WT))[1]
b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
c <- which(DATE==b)
WT_DAY <- c(rep(NA, c-1), WT_DAY)
length(WT_DAY) <- z

a <- which(!is.na(anthro$HC))[1]
b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
c <- which(DATE==b)
HC_DAY <- c(rep(NA, c-1), HC_DAY)
length(HC_DAY) <- z

a <- which(!is.na(anthro$UAC))[1]
b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
c <- which(DATE==b)
UAC_DAY <- c(rep(NA, c-1), UAC_DAY)
length(UAC_DAY) <- z

a <- which(!is.na(anthro$TSF))[1]
b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
c <- which(DATE==b)
TSF_DAY <- c(rep(NA, c-1), TSF_DAY)
length(TSF_DAY) <- z

a <- which(!is.na(anthro$SSF))[1]
b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
c <- which(DATE==b)
SSF_DAY <- c(rep(NA, c-1), SSF_DAY)
length(SSF_DAY) <- z

a <- which(!is.na(anthro$USF))[1]
b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
c <- which(DATE==b)
USF_DAY <- c(rep(NA, c-1), USF_DAY)
length(USF_DAY) <- z

a <- which(!is.na(anthro$SISF))[1]
b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
c <- which(DATE==b)
SISF_DAY <- c(rep(NA, c-1), SISF_DAY)
length(SISF_DAY) <- z

a <- which(!is.na(anthro$MBSF))[1]
b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
c <- which(DATE==b)
MBSF_DAY <- c(rep(NA, c-1), MBSF_DAY)
length(MBSF_DAY) <- z


a <- which(!is.na(anthro$UC))[1]
b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
c <- which(DATE==b)
UC_DAY <- c(rep(NA, c-1), UC_DAY)
length(UC_DAY) <- z

a <- which(!is.na(anthro$R))[1]
b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
c <- which(DATE==b)
R_DAY <- c(rep(NA, c-1), R_DAY)
length(R_DAY) <- z

a <- which(!is.na(anthro$X))[1]
b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
c <- which(DATE==b)
X_DAY <- c(rep(NA, c-1), X_DAY)
length(X_DAY) <- z


MRNUMBER <- cbind(rep(unique(anthro$MRNUMBER),z))

anthro_interpolation <- cbind.data.frame(MRNUMBER, DATE, HT_DAY, WT_DAY, HC_DAY, UAC_DAY, TSF_DAY, SSF_DAY, USF_DAY, SISF_DAY, MBSF_DAY, UC_DAY, R_DAY, X_DAY)

#to use for AGE
y1 <- as.Date(anthro$DATE[1], format="%m/%d/%Y")
y2 <- as.Date(anthro$DATE[length(anthro[,1])], format="%m/%d/%Y")
DATE <- seq(y1, y2, by="days")
DATE1 <- DATE #for output table


#Calculating Age to use in equations
#right here I'm defining the interpolated graph as 'anthro_interpolated'
i=1:dim(anthro_interpolation)[1]
Bday <- as.Date(Demographics.Identified$DOB, format= "%m/%d/%Y") #First convert classes from factor to date
span <- interval(Bday, DATE)
AGE_DAY <- as.period(span)
AGE_DAY <- as.numeric(year(AGE_DAY))
AGE_DAY1 <- AGE_DAY #for output table
#to remove unwanted variables
AGE_MO <- as.period(span, "months")
AGE_MO <- as.numeric(month(AGE_MO))
span2 <- interval(Bday, DATE+1)
AGE_DOL1 <- as.period(span, "days")
AGE_DOL1 <- as.numeric(day(AGE_DOL1))
#below defined to use for final table


#Gender
SEX <- ifelse(Demographics.Identified$GENDER[1] == "M", 1, 0)

#BMI
BMI_DAY <- WT_DAY/(HT_DAY/100)^2

#Upper Arm Muscle Circumference
AMC_DAY <- UAC_DAY-(pi*TSF_DAY/10)

#Upper Arm Area
UAA_DAY <- 0.785*((UAC_DAY/pi)^2)

#Upper Arm Muscle Area
AMA_DAY <- ((UAC_DAY-(pi*TSF_DAY/10))^2)/12.57

#Upper Arm Fat Area
AFA_DAY <- UAA_DAY-AMA_DAY
#this would have to be used after calculating UAA and AMA


#Visceral Cavity Area
VCA_DAY <- ((UC_DAY-(pi*((USF_DAY/10)+(2*SISF_DAY/10)+(MBSF_DAY/10))/4))^2)/(4*pi)


#Visceral Cavity Area Percentage
VC_PCTG_DAY <- ((UC_DAY*10-(pi*((USF_DAY+2*SISF_DAY+MBSF_DAY)/4)))^2/(UC_DAY*10)^2)*100


#Impedence
Z_DAY <- ((X_DAY^2)+(R_DAY^2))^0.5

#Phase Angle
P_DAY <- (tan(X_DAY/R_DAY))*180/pi

#Arpadi Fat Free Mass
ARPADI_FFM_DAY <- ((3.474+(0.459*(HT_DAY^2/R_DAY))+(0.064*WT_DAY))/(0.769 -(0.009*AGE_DAY)-(0.016*SEX)))

#Goran Fat Free Mass
GORAN_FFM_DAY <- ((0.59*((HT_DAY^2)/R_DAY))+(0.065*WT_DAY)+0.04)/(0.769-(0.0025*AGE_DAY)-(0.019*SEX))

#ARPADI_TOTAL_BODY_WATER
ARPADI_TBW_DAY <- 0.725+((0.475*(HT_DAY^2))/R_DAY)+(0.14*WT_DAY)

#SCHAEFER_FAT_FREE_MASS
SCHAEFER_FFM_DAY <- (((0.65)*(HT_DAY^2))/(Z_DAY))+(0.68*AGE_DAY)+0.15

#KOTLER_FAT_FREE_MASS
KOTLER_FFM_DAY <- (0.88/22.22)*((HT_DAY^1.97)/(Z_DAY^0.49))+0.081*WT_DAY+0.07

#BODY_FAT_PERCENTAGE
FFM_DAY <- ifelse(AGE_DAY < 18, FFM_DAY <- SCHAEFER_FFM_DAY, FFM_DAY <- KOTLER_FFM_DAY)
#age <18 arpadi, >18 Kotler, what if age is equal to 18
BODY_FAT_PCTG_DAY <- ((WT_DAY-FFM_DAY)/WT_DAY)*100


#can run below to double check what has been calculated so far
#table <- cbind.data.frame(DATE, AGE_DAY, BMI_DAY, AMC_DAY, UAA_DAY, AMA_DAY, AFA_DAY, VCA_DAY, VC_PCTG_DAY, Z_DAY, P_DAY, ARPADI_FFM_DAY, GORAN_FFM_DAY, ARPADI_TBW_DAY, SCHAEFER_FFM_DAY, KOTLER_FFM_DAY, BODY_FAT_PCTG_DAY)



#Race for NHANES
RACE <- if (Demographics.Identified$RACE[1] == "White"){RACE <- 2} else if (Demographics.Identified$RACE[1] == "Asian"){RACE <- 2}  else if (Demographics.Identified$RACE[1] == "African-American"){RACE <- 1} else if (Demographics.Identified$RACE[1] == "Hispanic"){RACE <- 2}


#Gender for NHANES
SEX <- ifelse(Demographics.Identified$GENDER[1] == "M", 1, 2)

#newHT
HT <- floor(HT_DAY)
HT1 <- HT #for output table

#NHANES
MEAN_NHANES_HT <- c()
SD_NHANES_HT <- c()
MEAN_NHANES_WT <- c()
SD_NHANES_WT <- c()
MEAN_NHANES_BMI <- c()
SD_NHANES_BMI <- c()
MEAN_NHANES_UAC <- c()
SD_NHANES_UAC <- c()
MEAN_NHANES_TSF <- c()
SD_NHANES_TSF <- c()
MEAN_NHANES_UAA <- c()
SD_NHANES_UAA <- c()
MEAN_NHANES_AMA <- c()
SD_NHANES_AMA <- c()
MEAN_NHANES_AFA <- c()
SD_NHANES_AFA <- c()
MEAN_NHANES_SSF <- c()
SD_NHANES_SSF <- c()
MEAN_NHANES_UC <- c()
SD_NHANES_UC <- c()
MEAN_NHANES_WT_FOR_HT <- c()
SD_NHANES_WT_FOR_HT <- c()
L_NHANES_UC <- c()
M_NHANES_UC <- c()
S_NHANES_UC <- c()


for (i in seq(length(AGE_DAY))) {
  MEAN_NHANES_HT <- c(MEAN_NHANES_HT, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_HT_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_HT_AGE==SEX & NHANES.References$RACE_NHANES_HT_AGE==RACE, select=c("MEAN_NHANES_HT_AGE")))
  SD_NHANES_HT <- c(SD_NHANES_HT, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_HT_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_HT_AGE==SEX & NHANES.References$RACE_NHANES_HT_AGE==RACE, select=c("SD_NHANES_HT_AGE")))
  MEAN_NHANES_WT <- c(MEAN_NHANES_WT, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_WT_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_WT_AGE==SEX & NHANES.References$RACE_NHANES_WT_AGE==RACE, select=c("MEAN_NHANES_WT_AGE")))
  SD_NHANES_WT <- c(SD_NHANES_WT, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_WT_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_WT_AGE==SEX & NHANES.References$RACE_NHANES_WT_AGE==RACE, select=c("SD_NHANES_WT_AGE")))
  MEAN_NHANES_BMI <- c(MEAN_NHANES_BMI, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_BMI_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_BMI_AGE==SEX & NHANES.References$RACE_NHANES_BMI_AGE==RACE, select=c("MEAN_NHANES_BMI_AGE")))
  SD_NHANES_BMI <- c(SD_NHANES_BMI, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_BMI_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_BMI_AGE==SEX & NHANES.References$RACE_NHANES_BMI_AGE==RACE, select=c("SD_NHANES_BMI_AGE")))
  MEAN_NHANES_UAC <- c(MEAN_NHANES_UAC, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_UAC_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_UAC_AGE==SEX & NHANES.References$RACE_NHANES_UAC_AGE==RACE, select=c("MEAN_NHANES_UAC_AGE")))
  SD_NHANES_UAC <- c(SD_NHANES_UAC, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_UAC_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_UAC_AGE==SEX & NHANES.References$RACE_NHANES_UAC_AGE==RACE, select=c("SD_NHANES_UAC_AGE")))
  MEAN_NHANES_TSF <- c(MEAN_NHANES_TSF, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_TSF_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_TSF_AGE==SEX & NHANES.References$RACE_NHANES_TSF_AGE==RACE, select=c("MEAN_NHANES_TSF_AGE")))
  SD_NHANES_TSF <- c(SD_NHANES_TSF, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_TSF_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_TSF_AGE==SEX & NHANES.References$RACE_NHANES_TSF_AGE==RACE, select=c("SD_NHANES_TSF_AGE")))
  MEAN_NHANES_UAA <- c(MEAN_NHANES_UAA, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_UAA_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_UAA_AGE==SEX & NHANES.References$RACE_NHANES_UAA_AGE==RACE, select=c("MEAN_NHANES_UAA_AGE")))
  SD_NHANES_UAA <- c(SD_NHANES_UAA, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_UAA_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_UAA_AGE==SEX & NHANES.References$RACE_NHANES_UAA_AGE==RACE, select=c("SD_NHANES_UAA_AGE")))
  MEAN_NHANES_AMA <- c(MEAN_NHANES_AMA, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_AMA_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_AMA_AGE==SEX & NHANES.References$RACE_NHANES_AMA_AGE==RACE, select=c("MEAN_NHANES_AMA_AGE")))
  SD_NHANES_AMA <- c(SD_NHANES_AMA, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_AMA_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_AMA_AGE==SEX & NHANES.References$RACE_NHANES_AMA_AGE==RACE, select=c("SD_NHANES_AMA_AGE")))
  MEAN_NHANES_AFA <- c(MEAN_NHANES_AFA, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_AFA_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_AFA_AGE==SEX & NHANES.References$RACE_NHANES_AFA_AGE==RACE, select=c("MEAN_NHANES_AFA_AGE")))
  SD_NHANES_AFA <- c(SD_NHANES_AFA, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_AFA_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_AFA_AGE==SEX & NHANES.References$RACE_NHANES_AFA_AGE==RACE, select=c("SD_NHANES_AFA_AGE")))
  MEAN_NHANES_SSF <- c(MEAN_NHANES_SSF, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_SSSF_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_SSSF_AGE==SEX & NHANES.References$RACE_NHANES_SSSF_AGE==RACE, select=c("MEAN_NHANES_SSSF_AGE")))
  SD_NHANES_SSF <- c(SD_NHANES_SSF, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_SSSF_AGE==AGE_DAY[i] & NHANES.References$SEX_NHANES_SSSF_AGE==SEX & NHANES.References$RACE_NHANES_SSSF_AGE==RACE, select=c("SD_NHANES_SSSF_AGE")))
}


#NHANES WT for HT
MEAN_NHANES_WT_FOR_HT <- rep(NA,length(AGE_DAY))
SD_NHANES_WT_FOR_HT <- rep(NA,length(AGE_DAY))
WT_FOR_HT <- cbind.data.frame(AGE_DAY,MEAN_NHANES_WT_FOR_HT,SD_NHANES_WT_FOR_HT)

for (i in seq(length(AGE_DAY))) {
  if (AGE_DAY[i] < 12 & AGE_DAY[i] >= 2 & SEX ==1) {
    WT_FOR_HT$MEAN_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==HT[i], select=c("MEAN_NHANES_WT_FOR_HT1"))
    WT_FOR_HT$SD_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==HT[i], select=c("SD_NHANES_WT_FOR_HT1"))
  } 
  else if (AGE_DAY[i] >= 12 & AGE_DAY[i] < 18 & SEX==1) {
    WT_FOR_HT$MEAN_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==HT[i], select=c("MEAN_NHANES_WT_FOR_HT2"))
    WT_FOR_HT$SD_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==HT[i], select=c("SD_NHANES_WT_FOR_HT2"))
  }
  else if (AGE_DAY[i] >= 18 & AGE_DAY[i] < 75) {
    WT_FOR_HT$MEAN_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT3==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT3==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT3==HT[i], select=c("MEAN_NHANES_WT_FOR_HT3"))
    WT_FOR_HT$SD_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT3==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT3==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT3==HT[i], select=c("SD_NHANES_WT_FOR_HT3"))
  }
  else if (AGE_DAY[i] <= 11 & AGE_DAY[i] >= 2 & SEX==2 ) {
    WT_FOR_HT$MEAN_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==HT[i], select=c("MEAN_NHANES_WT_FOR_HT1"))
    WT_FOR_HT$SD_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==HT[i], select=c("SD_NHANES_WT_FOR_HT1"))
  } 
  else if (AGE_DAY[i] >= 11 & AGE_DAY[i] < 18 & SEX==2) {
    WT_FOR_HT$MEAN_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==HT[i], select=c("MEAN_NHANES_WT_FOR_HT2"))
    WT_FOR_HT$SD_NHANES_WT_FOR_HT[i] <- subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==HT[i], select=c("SD_NHANES_WT_FOR_HT2"))
  }

}

MEAN_NHANES_WT_FOR_HT <- WT_FOR_HT$MEAN_NHANES_WT_FOR_HT
SD_NHANES_WT_FOR_HT <- WT_FOR_HT$SD_NHANES_WT_FOR_HT
AGE_DAY <- WT_FOR_HT$AGE_DAY

  
#For NHANES UC (waist circumference)
a <- (AGE_DAY <= 4 | AGE_DAY >= 19)
AGE_UC <- ifelse(AGE_DAY <= 4 | AGE_DAY >= 19, AGE_DAY, AGE_MO)

for (i in which(AGE_DAY<=4 | AGE_DAY>=19)) {
  RACE <- if (Demographics.Identified$RACE[1] == "White"){RACE <- 2} else if (Demographics.Identified$RACE[1] == "Asian"){RACE <- 2}  else if (Demographics.Identified$RACE[1] == "African-American"){RACE <- 1} else if (Demographics.Identified$RACE[1] == "Hispanic"){RACE <- 3}  
  MEAN_NHANES_UC <- c(MEAN_NHANES_UC, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_UC_AGE==AGE_UC[i] & NHANES.References$SEX_NHANES_UC_AGE==SEX & NHANES.References$RACE_NHANES_UC_AGE==RACE, select=c("MEAN_NHANES_UC_AGE")))
  SD_NHANES_UC <- c(SD_NHANES_UC, subset(NHANES.References, NHANES.References$AGE_YEAR_NHANES_UC_AGE==AGE_UC[i] & NHANES.References$SEX_NHANES_UC_AGE==SEX & NHANES.References$RACE_NHANES_UC_AGE==RACE, select=c("SD_NHANES_UC_AGE")))
  
  
}

for (i in which(!(AGE_DAY<=4 | AGE_DAY>=19))) {
  L_NHANES_UC <- c(L_NHANES_UC, subset(NHANES.References, NHANES.References$AGE_MO_NHANES_UC_AGE==AGE_UC[i] & NHANES.References$SEX_NHANES_UC_AGE2==SEX, select=c("L_NHANES_UC")))
  M_NHANES_UC <- c(M_NHANES_UC, subset(NHANES.References, NHANES.References$AGE_MO_NHANES_UC_AGE==AGE_UC[i] & NHANES.References$SEX_NHANES_UC_AGE2==SEX, select=c("M_NHANES_UC")))
  S_NHANES_UC <- c(S_NHANES_UC, subset(NHANES.References, NHANES.References$AGE_MO_NHANES_UC_AGE==AGE_UC[i] & NHANES.References$SEX_NHANES_UC_AGE2==SEX, select=c("S_NHANES_UC")))
}




MEAN_NHANES_HT <- as.numeric(MEAN_NHANES_HT)
SD_NHANES_HT <- as.numeric(SD_NHANES_HT)
MEAN_NHANES_WT <- as.numeric(MEAN_NHANES_WT)
SD_NHANES_WT <- as.numeric(SD_NHANES_WT)
MEAN_NHANES_BMI <- as.numeric(MEAN_NHANES_BMI)
SD_NHANES_BMI <- as.numeric(SD_NHANES_BMI)
MEAN_NHANES_UAC <- as.numeric(MEAN_NHANES_UAC)
SD_NHANES_UAC <- as.numeric(SD_NHANES_UAC)
MEAN_NHANES_TSF <- as.numeric(MEAN_NHANES_TSF)
SD_NHANES_TSF <- as.numeric(SD_NHANES_TSF)
MEAN_NHANES_UAA <- as.numeric(MEAN_NHANES_UAA)
SD_NHANES_UAA <- as.numeric(SD_NHANES_UAA)
MEAN_NHANES_AMA <- as.numeric(MEAN_NHANES_AMA)
SD_NHANES_AMA <- as.numeric(SD_NHANES_AMA)
MEAN_NHANES_AFA <- as.numeric(MEAN_NHANES_AFA)
SD_NHANES_AFA <- as.numeric(SD_NHANES_AFA)
MEAN_NHANES_SSF <- as.numeric(MEAN_NHANES_SSF)
SD_NHANES_SSF <- as.numeric(SD_NHANES_SSF)
MEAN_NHANES_UC <- as.numeric(MEAN_NHANES_UC)
SD_NHANES_UC <- as.numeric(SD_NHANES_UC)
MEAN_NHANES_WT_FOR_HT <- as.numeric(MEAN_NHANES_WT_FOR_HT)
SD_NHANES_WT_FOR_HT <- as.numeric(SD_NHANES_WT_FOR_HT)
MEAN_NHANES_UC <- as.numeric(MEAN_NHANES_UC)
SD_NHANES_UC <- as.numeric(SD_NHANES_UC)
L_NHANES_UC <- as.numeric(L_NHANES_UC)
M_NHANES_UC <- as.numeric(M_NHANES_UC)
S_NHANES_UC <- as.numeric(S_NHANES_UC)
#for some reason ifelse does not work in this case
#so instead convert true/false to numeric, and replace
b <- as.numeric(a)
b[b==0] <- L_NHANES_UC
b[b==1] <- NA
L_NHANES_UC <- b
b <- as.numeric(a)
b[b==0] <- M_NHANES_UC
b[b==1] <- NA
M_NHANES_UC <- b
b <- as.numeric(a)
b[b==0] <- S_NHANES_UC
b[b==1] <- NA
S_NHANES_UC <- b
b <- as.numeric(a)
b[b==1] <- MEAN_NHANES_UC
b[b==0] <- NA
MEAN_NHANES_UC <- b
b <- as.numeric(a)
b[b==1] <- SD_NHANES_UC
b[b==0] <- NA
SD_NHANES_UC <- b



#table2 <- cbind(AGE_DAY, MEAN_NHANES_SSF, SD_NHANES_SSF, MEAN_NHANES_AFA, SD_NHANES_AFA, MEAN_NHANES_AMA, SD_NHANES_AMA, MEAN_NHANES_BMI, SD_NHANES_BMI, MEAN_NHANES_HT, SD_NHANES_HT, MEAN_NHANES_TSF, SD_NHANES_TSF, MEAN_NHANES_UAC, SD_NHANES_UAC, MEAN_NHANES_UAA, SD_NHANES_UAA, MEAN_NHANES_UC, SD_NHANES_UC, MEAN_NHANES_WT, SD_NHANES_WT, MEAN_NHANES_WT_FOR_HT, SD_NHANES_WT_FOR_HT)


#NHANES Percentiles and Z scores

#NHANES_HEIGHT_Z_SCORE
NHANES_HT_Z_DAY <- (HT_DAY-MEAN_NHANES_HT)/SD_NHANES_HT

#NHANES_HEIGHT_PERCENTILE
NHANES_HT_PCTL_DAY <- (pnorm(NHANES_HT_Z_DAY))*100

#NHANES_WEIGHT_Z_SCORE
NHANES_WT_Z_DAY <- (WT_DAY-MEAN_NHANES_WT)/SD_NHANES_WT

#NHANES_WEIGHT_PERCENTILE
NHANES_WT_PCTL_DAY <- (pnorm(NHANES_WT_Z_DAY))*100

#NHANES_BMI_Z_SCORE
NHANES_BMI_Z_DAY <- (BMI_DAY-MEAN_NHANES_BMI)/SD_NHANES_BMI

#NHANES_BMI_PERCENTILE
NHANES_BMI_PCTL_DAY <- (pnorm(NHANES_BMI_Z_DAY))*100

#NHANES_SUBSCAPULAR_SKINFOLD_Z_SCORE
NHANES_SSF_Z_DAY <- (SSF_DAY-MEAN_NHANES_SSF)/SD_NHANES_SSF

#NHANES_SUBSCAPULAR_SKINFOLD_PERCENTILE
NHANES_SSF_PCTL_DAY <- (pnorm(NHANES_SSF_Z_DAY))*100

#NHANES_TRICEPS_SKINFOLD_Z_SCORE
NHANES_TSF_Z_DAY <- (TSF_DAY-MEAN_NHANES_TSF)/SD_NHANES_TSF

#NHANES_TRICEPS_SKINFOLD_PERCENTILE
NHANES_TSF_PCTL_DAY <- (pnorm(NHANES_TSF_Z_DAY))*100

#NHANES_UPPER_ARM_AREA_Z_SCORE
NHANES_UAA_Z_DAY <- (UAA_DAY-MEAN_NHANES_UAA)/SD_NHANES_UAA

#NHANES_UPPER_ARM_AREA_PERCENTILE
NHANES_UAA_PCTL_DAY <- (pnorm(NHANES_UAA_Z_DAY))*100

#NHANES_UPPER_ARM_CIRCUMFERANCE_Z_SCORE
NHANES_UAC_Z_DAY <- (UAC_DAY-MEAN_NHANES_UAC)/SD_NHANES_UAC

#NHANES_UPPER_ARM_CIRCUMFERANCE__PERCENTILE
NHANES_UAC_PCTL_DAY <- (pnorm(NHANES_UAC_Z_DAY))*100

#NHANES_UPPER_ARM_FAT_AREA_Z_SCORE
NHANES_AFA_Z_DAY <- (AFA_DAY-MEAN_NHANES_AFA)/SD_NHANES_AFA

#NHANES_UPPER_ARM_FAT_AREA_PERCENTILE
NHANES_AFA_PCTL_DAY <- (pnorm(NHANES_AFA_Z_DAY))*100

#NHANES_UPPER_ARM_MUSCLE_AREA_Z_SCORE
NHANES_AMA_Z_DAY <- (AMA_DAY-MEAN_NHANES_AMA)/SD_NHANES_AMA

#NHANES_UPPER_ARM_MUSCLE_AREA_PERCENTILE
NHANES_AMA_PCTL_DAY <- (pnorm(NHANES_AMA_Z_DAY))*100

#NHANES_WAIST_CIRCUMFERENCE_Z_SCORE
NHANES_UC_Z_DAY1 <- (((UC_DAY/M_NHANES_UC)^L_NHANES_UC)-1)/(L_NHANES_UC*S_NHANES_UC)
NHANES_UC_Z_DAY2 <- (UC_DAY-MEAN_NHANES_UC)/SD_NHANES_UC
NHANES_UC_Z_DAY1[is.na(NHANES_UC_Z_DAY1)] <- " "
NHANES_UC_Z_DAY2[is.na(NHANES_UC_Z_DAY2)] <- " "
NHANES_UC_Z_DAY <- as.numeric(paste(NHANES_UC_Z_DAY1, NHANES_UC_Z_DAY2))

#NHANES_WAIST_CIRCUMFERENCE_PERCENTILE
NHANES_UC_PCTL_DAY <- (pnorm(NHANES_UC_Z_DAY))*100

#NHANES_WEIGHT_FOR_HEIGHT_Z_SCORE
NHANES_WT_HT_Z_DAY <- (WT_DAY-MEAN_NHANES_WT_FOR_HT)/SD_NHANES_WT_FOR_HT

#NHANES_WEIGHT_FOR_HEIGHT_PERCENTILE
NHANES_WT_HT_PCTL_DAY <- (pnorm(NHANES_WT_HT_Z_DAY))*100



#for checking NHANES UC: 
tab <- cbind(AGE_DAY, AGE_UC, UC_DAY, MEAN_NHANES_UC, SD_NHANES_UC, L_NHANES_UC, M_NHANES_UC, S_NHANES_UC, NHANES_UC_Z_DAY1, NHANES_UC_Z_DAY2, NHANES_UC_Z_DAY)

#table3 <- cbind(NHANES_HT_PCTL_DAY, NHANES_HT_Z_DAY, NHANES_WT_PCTL_DAY, NHANES_WT_Z_DAY, NHANES_BMI_PCTL_DAY, NHANES_BMI_Z_DAY, NHANES_UAC_PCTL_DAY, NHANES_UAC_Z_DAY, NHANES_TSF_PCTL_DAY, NHANES_TSF_Z_DAY, NHANES_UAA_PCTL_DAY, NHANES_UAA_Z_DAY, NHANES_AMA_PCTL_DAY, NHANES_AMA_Z_DAY, NHANES_AFA_PCTL_DAY, NHANES_AFA_Z_DAY, NHANES_SSF_PCTL_DAY, NHANES_SSF_Z_DAY, NHANES_UC_PCTL_DAY, NHANES_UC_Z_DAY, NHANES_WT_HT_Z_DAY, NHANES_WT_HT_PCTL_DAY)


#CDC R script

#same SEX as NHANES, need to calculate Age in months
i=1:dim(anthro_interpolation)[1]
Bday <- as.Date(Demographics.Identified$DOB[1], format= "%m/%d/%Y") #First convert classes from factor to date
span <- interval(Bday, DATE)
AGE_DAY <- as.period(span, "months")
AGE_DAY <- as.numeric(month(AGE_DAY))
#to remove unwanted variables
rm(i, Bday, span)


#LMS for CDC

L_CDC_WT <- c()
M_CDC_WT <- c()
S_CDC_WT <- c()
L_CDC_HT <- c()
M_CDC_HT <- c()
S_CDC_HT <- c()
L_CDC_BMI <- c()
M_CDC_BMI <- c()
S_CDC_BMI <- c()
L_CDC_WT_HT <- c()
M_CDC_WT_HT <- c()
S_CDC_WT_HT <- c()
L_CDC_HC <- c()
M_CDC_HC <- c()
S_CDC_HC <- c()

for (i in seq(length(AGE_DAY))) {
  
  L_CDC_WT <- c(L_CDC_WT, subset(CDC.References, CDC.References$AGE_MO_CDC_WT_AGE==AGE_DAY[i] & CDC.References$SEX_CDC_WT_AGE==SEX, select=c("L_CDC_WT_AGE")))
  M_CDC_WT <- c(M_CDC_WT, subset(CDC.References, CDC.References$AGE_MO_CDC_WT_AGE==AGE_DAY[i] & CDC.References$SEX_CDC_WT_AGE==SEX, select=c("M_CDC_WT_AGE")))
  S_CDC_WT <- c(S_CDC_WT, subset(CDC.References, CDC.References$AGE_MO_CDC_WT_AGE==AGE_DAY[i] & CDC.References$SEX_CDC_WT_AGE==SEX, select=c("S_CDC_WT_AGE")))
  L_CDC_HT <- c(L_CDC_HT, subset(CDC.References, CDC.References$AGE_MO_CDC_HT_AGE==AGE_DAY[i] & CDC.References$SEX_CDC_HT_AGE==SEX, select=c("L_CDC_HT_AGE")))
  M_CDC_HT <- c(M_CDC_HT, subset(CDC.References, CDC.References$AGE_MO_CDC_HT_AGE==AGE_DAY[i] & CDC.References$SEX_CDC_HT_AGE==SEX, select=c("M_CDC_HT_AGE")))
  S_CDC_HT <- c(S_CDC_HT, subset(CDC.References, CDC.References$AGE_MO_CDC_HT_AGE==AGE_DAY[i] & CDC.References$SEX_CDC_HT_AGE==SEX, select=c("S_CDC_HT_AGE")))
  L_CDC_BMI <- c(L_CDC_BMI, subset(CDC.References, CDC.References$AGE_MO_CDC_BMI_AGE==AGE_DAY[i] & CDC.References$SEX_CDC_BMI_AGE==SEX, select=c("L_CDC_BMI_AGE")))
  M_CDC_BMI <- c(M_CDC_BMI, subset(CDC.References, CDC.References$AGE_MO_CDC_BMI_AGE==AGE_DAY[i] & CDC.References$SEX_CDC_BMI_AGE==SEX, select=c("M_CDC_BMI_AGE")))
  S_CDC_BMI <- c(S_CDC_BMI, subset(CDC.References, CDC.References$AGE_MO_CDC_BMI_AGE==AGE_DAY[i] & CDC.References$SEX_CDC_BMI_AGE==SEX, select=c("S_CDC_BMI_AGE")))
  L_CDC_WT_HT <-c(L_CDC_WT_HT, subset(CDC.References, CDC.References$HEIGHT_CDC_WT_FOR_HT==HT[i] & CDC.References$SEX_CDC_WT_FOR_HT==SEX, select=c("L_CDC_WT_FOR_HT")))
  M_CDC_WT_HT <- c(M_CDC_WT_HT, subset(CDC.References, CDC.References$HEIGHT_CDC_WT_FOR_HT==HT[i] & CDC.References$SEX_CDC_WT_FOR_HT==SEX, select=c("M_CDC_WT_FOR_HT")))
  S_CDC_WT_HT <- c(S_CDC_WT_HT, subset(CDC.References, CDC.References$HEIGHT_CDC_WT_FOR_HT==HT[i] & CDC.References$SEX_CDC_WT_FOR_HT==SEX, select=c("S_CDC_WT_FOR_HT")))
  L_CDC_HC <- c(L_CDC_HC, subset(CDC.References, CDC.References$AGE_MO_CDC_HC_AGE==AGE_DAY[i] & CDC.References$SEX_CDC_HC_AGE==SEX, select=c("L_CDC_HC_AGE")))
  M_CDC_HC <- c(M_CDC_HC, subset(CDC.References, CDC.References$AGE_MO_CDC_HC_AGE==AGE_DAY[i] & CDC.References$SEX_CDC_HC_AGE==SEX, select=c("M_CDC_HC_AGE")))
  S_CDC_HC <- c(S_CDC_HC, subset(CDC.References, CDC.References$AGE_MO_CDC_HC_AGE==AGE_DAY[i] & CDC.References$SEX_CDC_HC_AGE==SEX, select=c("S_CDC_HC_AGE")))
}

L_CDC_HT <- as.numeric(L_CDC_HT)
M_CDC_HT <- as.numeric(M_CDC_HT)
S_CDC_HT <- as.numeric(S_CDC_HT)
L_CDC_WT <- as.numeric(L_CDC_WT)
M_CDC_WT <- as.numeric(M_CDC_WT)
S_CDC_WT <- as.numeric(S_CDC_WT)
L_CDC_BMI <- as.numeric(L_CDC_BMI)
M_CDC_BMI <- as.numeric(M_CDC_BMI)
S_CDC_BMI <- as.numeric(S_CDC_BMI)
L_CDC_WT_HT <- as.numeric(L_CDC_WT_HT)
M_CDC_WT_HT <- as.numeric(M_CDC_WT_HT)
S_CDC_WT_HT <- as.numeric(S_CDC_WT_HT)
L_CDC_HC <- as.numeric(L_CDC_HC)
M_CDC_HC <- as.numeric(M_CDC_HC)
S_CDC_HC <- as.numeric(S_CDC_HC)
#CDC_LMS <- cbind(AGE_DAY, L_CDC_HT, M_CDC_HT, S_CDC_HT, L_CDC_WT, M_CDC_WT, S_CDC_WT, L_CDC_BMI, M_CDC_BMI, S_CDC_BMI, L_CDC_WT_HT, M_CDC_WT_HT, S_CDC_WT_HT, L_CDC_HC, M_CDC_HC, S_CDC_HC)

#CDC_WEIGHT_FOR_HEIGHT_Z_SCORE
CDC_WT_HT_Z_DAY <- (((WT_DAY/M_CDC_WT_HT)^L_CDC_WT_HT)-1)/(L_CDC_WT_HT*S_CDC_WT_HT)

#CDC_WEIGHT_FOR_HEIGHT_PERCENTILE
CDC_WT_HT_PCTL_DAY <- (pnorm(CDC_WT_HT_Z_DAY))*100

#CDC_WEIGHT_Z_SCORE
CDC_WT_Z_DAY <- (((WT_DAY/M_CDC_WT)^L_CDC_WT)-1)/(L_CDC_WT*S_CDC_WT)

#CDC_WEIGHT_PERCENTILE
CDC_WT_PCTL_DAY <- (pnorm(CDC_WT_Z_DAY))*100

#CDC_BMI_Z_SCORE
CDC_BMI_Z_DAY <- (((BMI_DAY/M_CDC_BMI)^L_CDC_BMI)-1)/(L_CDC_BMI*S_CDC_BMI)

#CDC_BMI_PERCENTILE
CDC_BMI_PCTL_DAY <- (pnorm(CDC_BMI_Z_DAY))*100

#CDC_HEAD_CIRCUMFERENCE_Z_SCORE
CDC_HC_Z_DAY <- (((HC_DAY/M_CDC_HC)^L_CDC_HC)-1)/(L_CDC_HC*S_CDC_HC)

#CDC_HEAD_CIRCUMFERENCE_PERCENTILE
CDC_HC_PCTL_DAY <- (pnorm(CDC_HC_Z_DAY))*100

#CDC_HEIGHT_Z_SCORE
CDC_HT_Z_DAY <- (((HT_DAY/M_CDC_HT)^L_CDC_HT)-1)/(L_CDC_HT*S_CDC_HT)

#CDC_HEIGHT_PERCENTILE
CDC_HT_PCTL_DAY <- (pnorm(CDC_HT_Z_DAY))*100

#CDC_Z_AND_PCTL <- cbind(AGE_DAY, CDC_HT_Z_DAY, CDC_HT_PCTL_DAY, CDC_WT_Z_DAY, CDC_WT_PCTL_DAY, CDC_BMI_Z_DAY, CDC_BMI_PCTL_DAY, CDC_WT_HT_Z_DAY, CDC_WT_HT_PCTL_DAY, CDC_HC_Z_DAY, CDC_HC_PCTL_DAY)

#HT_DAY for WHO, because need to have HT to the nearest tenth
HT <- round(HT_DAY, digits=1)


#Calculating Age to use in WHO equations
i=1:dim(anthro_interpolation)[1]
Bday <- as.Date(Demographics.Identified$DOB[1], format= "%m/%d/%Y") #First convert classes from factor to date
span <- interval(Bday, DATE)
AGE_DOL2 <- as.period(span, "days")
AGE_DOL2 <- as.numeric(day(AGE_DOL2))
#to remove unwanted variables
rm(i, Bday, span)

#WHO References
L_WHO_HT <- c()
M_WHO_HT <- c()
S_WHO_HT <- c()
L_WHO_HT2 <- c()
M_WHO_HT2 <- c()
S_WHO_HT2 <- c()
L_WHO_BMI <- c()
M_WHO_BMI <- c()
S_WHO_BMI <- c()
L_WHO_BMI2 <- c()
M_WHO_BMI2<- c()
S_WHO_BMI2 <- c()
L_WHO_HC <- c()
M_WHO_HC <- c()
S_WHO_HC <- c()
L_WHO_SSF <- c()
M_WHO_SSF <- c()
S_WHO_SSF <- c()
L_WHO_TSF <- c()
M_WHO_TSF <- c()
S_WHO_TSF <- c()
L_WHO_UAC <- c()
M_WHO_UAC <- c()
S_WHO_UAC <- c()
L_WHO_WT <- c()
M_WHO_WT <- c()
S_WHO_WT <- c()
L_WHO_WT2 <- c()
M_WHO_WT2 <- c()
S_WHO_WT2 <- c()
L_WHO_WT_HT <- c()
M_WHO_WT_HT <- c()
S_WHO_WT_HT <- c()

for (i in seq(length(AGE_DOL2))) {
  L_WHO_HC <- c(L_WHO_HC, subset(WHO.References, WHO.References$AGE_DAY_WHO_HC_AGE==AGE_DOL2[i] & WHO.References$SEX_WHO_HC_AGE==SEX, select = c("L_WHO_HC_AGE")))
  M_WHO_HC <- c(M_WHO_HC, subset(WHO.References, WHO.References$AGE_DAY_WHO_HC_AGE==AGE_DOL2[i] & WHO.References$SEX_WHO_HC_AGE==SEX, select = c("M_WHO_HC_AGE")))
  S_WHO_HC <- c(S_WHO_HC, subset(WHO.References, WHO.References$AGE_DAY_WHO_HC_AGE==AGE_DOL2[i] & WHO.References$SEX_WHO_HC_AGE==SEX, select = c("S_WHO_HC_AGE")))
  L_WHO_SSF <- c(L_WHO_SSF, subset(WHO.References, WHO.References$AGE_DAY_WHO_SSF_AGE==AGE_DOL2[i] & WHO.References$SEX_WHO_SSF_AGE==SEX, select=c("L_WHO_SSF_AGE")))
  M_WHO_SSF <- c(M_WHO_SSF, subset(WHO.References, WHO.References$AGE_DAY_WHO_SSF_AGE==AGE_DOL2[i] & WHO.References$SEX_WHO_SSF_AGE==SEX, select=c("M_WHO_SSF_AGE")))
  S_WHO_SSF <- c(S_WHO_SSF, subset(WHO.References, WHO.References$AGE_DAY_WHO_SSF_AGE==AGE_DOL2[i] & WHO.References$SEX_WHO_SSF_AGE==SEX, select=c("S_WHO_SSF_AGE")))
  L_WHO_TSF <- c(L_WHO_TSF, subset(WHO.References, WHO.References$AGE_DAY_WHO_TSF_AGE==AGE_DOL2[i] & WHO.References$SEX_WHO_TSF_AGE==SEX, select=c("L_WHO_TSF_AGE")))
  M_WHO_TSF <- c(M_WHO_TSF, subset(WHO.References, WHO.References$AGE_DAY_WHO_TSF_AGE==AGE_DOL2[i] & WHO.References$SEX_WHO_TSF_AGE==SEX, select=c("M_WHO_TSF_AGE")))
  S_WHO_TSF <- c(S_WHO_TSF, subset(WHO.References, WHO.References$AGE_DAY_WHO_TSF_AGE==AGE_DOL2[i] & WHO.References$SEX_WHO_TSF_AGE==SEX, select=c("S_WHO_TSF_AGE")))
  L_WHO_UAC <- c(L_WHO_UAC, subset(WHO.References, WHO.References$AGE_DAY_WHO_UAC_AGE==AGE_DOL2[i] & WHO.References$SEX_WHO_UAC_AGE==SEX, select=c("L_WHO_UAC_AGE")))
  M_WHO_UAC <- c(M_WHO_UAC, subset(WHO.References, WHO.References$AGE_DAY_WHO_UAC_AGE==AGE_DOL2[i] & WHO.References$SEX_WHO_UAC_AGE==SEX, select=c("M_WHO_UAC_AGE")))
  S_WHO_UAC <- c(S_WHO_UAC, subset(WHO.References, WHO.References$AGE_DAY_WHO_UAC_AGE==AGE_DOL2[i] & WHO.References$SEX_WHO_UAC_AGE==SEX, select=c("S_WHO_UAC_AGE")))
  L_WHO_WT_HT <- c(L_WHO_WT_HT, subset(WHO.References, WHO.References$HEIGHT_WHO_WT_FOR_HT==HT[i] & WHO.References$SEX_WHO_WT_FOR_HT==SEX, select=c("L_WHO_WT_FOR_HT")))
  M_WHO_WT_HT <- c(M_WHO_WT_HT, subset(WHO.References, WHO.References$HEIGHT_WHO_WT_FOR_HT==HT[i] & WHO.References$SEX_WHO_WT_FOR_HT==SEX, select=c("M_WHO_WT_FOR_HT")))
  S_WHO_WT_HT <- c(S_WHO_WT_HT, subset(WHO.References, WHO.References$HEIGHT_WHO_WT_FOR_HT==HT[i] & WHO.References$SEX_WHO_WT_FOR_HT==SEX, select=c("S_WHO_WT_FOR_HT")))
}


#For WHO parameters which can be calculated in days (0-1856) or months (61+)
a <- (AGE_DOL2 <= 1856)
AGE_WHO <- ifelse(AGE_DOL2 <= 1856 , AGE_DOL2, AGE_DAY)

#find appropriate references for WHO height based on age
for (i in which(AGE_DOL2 <= 1856)) {
  L_WHO_HT2 <- c(L_WHO_HT2, subset(WHO.References, WHO.References$AGE_DAY_WHO_HT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_HT_AGE2==SEX, select=c("L_WHO_HT_AGE2")))
  M_WHO_HT2 <- c(M_WHO_HT2, subset(WHO.References, WHO.References$AGE_DAY_WHO_HT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_HT_AGE2==SEX, select=c("M_WHO_HT_AGE2")))
  S_WHO_HT2 <- c(S_WHO_HT2, subset(WHO.References, WHO.References$AGE_DAY_WHO_HT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_HT_AGE2==SEX, select=c("S_WHO_HT_AGE2")))
  
}

for (i in which(!(AGE_DOL2 <= 1856))) {
  L_WHO_HT <- c(L_WHO_HT, subset(WHO.References, WHO.References$AGE_MO_WHO_HT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_HT_AGE==SEX, select=c("L_WHO_HT_AGE")))
  M_WHO_HT <- c(M_WHO_HT, subset(WHO.References, WHO.References$AGE_MO_WHO_HT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_HT_AGE==SEX, select=c("M_WHO_HT_AGE")))
  S_WHO_HT <- c(S_WHO_HT, subset(WHO.References, WHO.References$AGE_MO_WHO_HT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_HT_AGE==SEX, select=c("S_WHO_HT_AGE")))
}

#find appropriate references for WHO weight based on age
for (i in which(AGE_DOL2 <= 1856)) {
  L_WHO_WT2 <- c(L_WHO_WT2, subset(WHO.References, WHO.References$AGE_DAY_WHO_WT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_WT_AGE2==SEX, select=c("L_WHO_WT_AGE2")))
  M_WHO_WT2 <- c(M_WHO_WT2, subset(WHO.References, WHO.References$AGE_DAY_WHO_WT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_WT_AGE2==SEX, select=c("M_WHO_WT_AGE2")))
  S_WHO_WT2 <- c(S_WHO_WT2, subset(WHO.References, WHO.References$AGE_DAY_WHO_WT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_WT_AGE2==SEX, select=c("S_WHO_WT_AGE2")))
  
}

for (i in which(!(AGE_DOL2 <= 1856))) {
  L_WHO_WT <- c(L_WHO_WT, subset(WHO.References, WHO.References$AGE_MO_WHO_WT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_WT_AGE==SEX, select=c("L_WHO_WT_AGE")))
  M_WHO_WT <- c(M_WHO_WT, subset(WHO.References, WHO.References$AGE_MO_WHO_WT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_WT_AGE==SEX, select=c("M_WHO_WT_AGE")))
  S_WHO_WT <- c(S_WHO_WT, subset(WHO.References, WHO.References$AGE_MO_WHO_WT_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_WT_AGE==SEX, select=c("S_WHO_WT_AGE")))
}

#find appropriate references for WHO BMI based on age
for (i in which(AGE_DOL2 <= 1856)) {
  L_WHO_BMI2 <- c(L_WHO_BMI2, subset(WHO.References, WHO.References$AGE_DAY_WHO_BMI_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_BMI_AGE2==SEX, select=c("L_WHO_BMI_AGE2")))
  M_WHO_BMI2 <- c(M_WHO_BMI2, subset(WHO.References, WHO.References$AGE_DAY_WHO_BMI_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_BMI_AGE2==SEX, select=c("M_WHO_BMI_AGE2")))
  S_WHO_BMI2 <- c(S_WHO_BMI2, subset(WHO.References, WHO.References$AGE_DAY_WHO_BMI_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_BMI_AGE2==SEX, select=c("S_WHO_BMI_AGE2")))
  
}

for (i in which(!(AGE_DOL2 <= 1856))) {
  L_WHO_BMI <- c(L_WHO_BMI, subset(WHO.References, WHO.References$AGE_MO_WHO_BMI_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_BMI_AGE==SEX, select=c("L_WHO_BMI_AGE")))
  M_WHO_BMI <- c(M_WHO_BMI, subset(WHO.References, WHO.References$AGE_MO_WHO_BMI_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_BMI_AGE==SEX, select=c("M_WHO_BMI_AGE")))
  S_WHO_BMI <- c(S_WHO_BMI, subset(WHO.References, WHO.References$AGE_MO_WHO_BMI_AGE==AGE_WHO[i] & WHO.References$SEX_WHO_BMI_AGE==SEX, select=c("S_WHO_BMI_AGE")))
}


L_WHO_HT <- as.numeric(L_WHO_HT)
M_WHO_HT <- as.numeric(M_WHO_HT)
S_WHO_HT <- as.numeric(S_WHO_HT)
L_WHO_HT2 <- as.numeric(L_WHO_HT2)
M_WHO_HT2 <- as.numeric(M_WHO_HT2)
S_WHO_HT2 <- as.numeric(S_WHO_HT2)
L_WHO_BMI <- as.numeric(L_WHO_BMI)
M_WHO_BMI <- as.numeric(M_WHO_BMI)
S_WHO_BMI <- as.numeric(S_WHO_BMI)
L_WHO_BMI2 <- as.numeric(L_WHO_BMI2)
M_WHO_BMI2 <- as.numeric(M_WHO_BMI2)
S_WHO_BMI2 <- as.numeric(S_WHO_BMI2)
L_WHO_HC <- as.numeric(L_WHO_HC)
M_WHO_HC <- as.numeric(M_WHO_HC)
S_WHO_HC <- as.numeric(S_WHO_HC)
L_WHO_SSF <- as.numeric(L_WHO_SSF)
M_WHO_SSF <- as.numeric(M_WHO_SSF)
S_WHO_SSF <- as.numeric(S_WHO_SSF)
L_WHO_TSF <- as.numeric(L_WHO_TSF)
M_WHO_TSF <- as.numeric(M_WHO_TSF)
S_WHO_TSF <- as.numeric(S_WHO_TSF)
L_WHO_UAC <- as.numeric(L_WHO_UAC)
M_WHO_UAC <- as.numeric(M_WHO_UAC)
S_WHO_UAC <- as.numeric(S_WHO_UAC)
L_WHO_WT <- as.numeric(L_WHO_WT)
M_WHO_WT <- as.numeric(M_WHO_WT)
S_WHO_WT <- as.numeric(S_WHO_WT)
L_WHO_WT2 <- as.numeric(L_WHO_WT2)
M_WHO_WT2 <- as.numeric(M_WHO_WT2)
S_WHO_WT2 <- as.numeric(S_WHO_WT2)
L_WHO_WT_HT <- as.numeric(L_WHO_WT_HT)
M_WHO_WT_HT <- as.numeric(M_WHO_WT_HT)
S_WHO_WT_HT <- as.numeric(S_WHO_WT_HT)

#for some reason ifelse does not work in this case
#so instead convert true/false to numeric, and replace

b <- as.numeric(a)
b[b==0] <- L_WHO_WT
b[b==1] <- NA
L_WHO_WT <- b
b <- as.numeric(a)
b[b==0] <- M_WHO_WT
b[b==1] <- NA
M_WHO_WT <- b
b <- as.numeric(a)
b[b==0] <- S_WHO_WT
b[b==1] <- NA
S_WHO_WT <- b
b <- as.numeric(a)
b[b==1] <- L_WHO_WT2
b[b==0] <- NA
L_WHO_WT2 <- b
b <- as.numeric(a)
b[b==1] <- M_WHO_WT2
b[b==0] <- NA
M_WHO_WT2 <- b
b <- as.numeric(a)
b[b==1] <- S_WHO_WT2
b[b==0] <- NA
S_WHO_WT2 <- b

b <- as.numeric(a)
b[b==0] <- L_WHO_BMI
b[b==1] <- NA
L_WHO_BMI <- b
b <- as.numeric(a)
b[b==0] <- M_WHO_BMI
b[b==1] <- NA
M_WHO_BMI <- b
b <- as.numeric(a)
b[b==0] <- S_WHO_BMI
b[b==1] <- NA
S_WHO_BMI <- b
b <- as.numeric(a)
b[b==1] <- L_WHO_BMI2
b[b==0] <- NA
L_WHO_BMI2 <- b
b <- as.numeric(a)
b[b==1] <- M_WHO_BMI2
b[b==0] <- NA
M_WHO_BMI2 <- b
b <- as.numeric(a)
b[b==1] <- S_WHO_BMI2
b[b==0] <- NA
S_WHO_BMI2 <- b

b <- as.numeric(a)
b[b==1] <- 2
b[b==0] <- L_WHO_HT
b[b==2] <- NA
L_WHO_HT <- b
b <- as.numeric(a)
b[b==1] <- 2
b[b==0] <- M_WHO_HT
b[b==2] <- NA
M_WHO_HT <- b
b <- as.numeric(a)
b[b==1] <- 2
b[b==0] <- S_WHO_HT
b[b==2] <- NA
S_WHO_HT <- b
b <- as.numeric(a)
b[b==1] <- 2
b[b==2] <- L_WHO_HT2
b[b==0] <- NA
L_WHO_HT2 <- b
b <- as.numeric(a)
b[b==1] <- 2
b[b==2] <- M_WHO_HT2
b[b==0] <- NA
M_WHO_HT2 <- b
b <- as.numeric(a)
b[b==1] <- 2
b[b==2] <- S_WHO_HT2
b[b==0] <- NA
S_WHO_HT2 <- b


#WHO_LMS <- cbind(AGE_DAY, L_WHO_HT, M_WHO_HT, S_WHO_HT, L_WHO_WT, M_WHO_WT, S_WHO_WT, L_WHO_BMI, M_WHO_BMI, S_WHO_BMI, L_WHO_HC, M_WHO_HC, S_WHO_HC, L_WHO_UAC, M_WHO_UAC, S_WHO_UAC, L_WHO_TSF, M_WHO_TSF, S_WHO_TSF, L_WHO_SSF, M_WHO_SSF, S_WHO_SSF, L_WHO_WT_HT, M_WHO_WT_HT, S_WHO_WT_HT)


#WHO_WEIGHT_Z_SCORE
WHO_WT_Z1_DAY <- (((WT_DAY/M_WHO_WT)^L_WHO_WT)-1)/(L_WHO_WT*S_WHO_WT)
WHO_WT_Z2_DAY <- (((WT_DAY/M_WHO_WT2)^L_WHO_WT2)-1)/(L_WHO_WT2*S_WHO_WT2)
WHO_WT_Z1_DAY[is.na(WHO_WT_Z1_DAY)] <- " "
WHO_WT_Z2_DAY[is.na(WHO_WT_Z2_DAY)] <- " "
WHO_WT_Z_DAY <- as.numeric(paste(WHO_WT_Z1_DAY, WHO_WT_Z2_DAY))

#WHO_WEIGHT_PERCENTILE
WHO_WT_PCTL_DAY <- (pnorm(WHO_WT_Z_DAY))*100

#WHO_BMI_Z_SCORE
WHO_BMI_Z1_DAY <- (((BMI_DAY/M_WHO_BMI)^L_WHO_BMI)-1)/(L_WHO_BMI*S_WHO_BMI)
WHO_BMI_Z2_DAY <- (((BMI_DAY/M_WHO_BMI2)^L_WHO_BMI2)-1)/(L_WHO_BMI2*S_WHO_BMI2)
WHO_BMI_Z1_DAY[is.na(WHO_BMI_Z1_DAY)] <- " "
WHO_BMI_Z2_DAY[is.na(WHO_BMI_Z2_DAY)] <- " "
WHO_BMI_Z_DAY <- as.numeric(paste(WHO_BMI_Z1_DAY, WHO_BMI_Z2_DAY))


#WHO_BMI_PERCENTILE
WHO_BMI_PCTL_DAY <- (pnorm(WHO_BMI_Z_DAY))*100

#WHO_HEAD_CIRCUMFERENCE_Z_SCORE
WHO_HC_Z_DAY <- (((HC_DAY/M_WHO_HC)^L_WHO_HC)-1)/(L_WHO_HC*S_WHO_HC)

#WHO_HEAD_CIRCUMFERENCE_PERCENTILE
WHO_HC_PCTL_DAY <- (pnorm(WHO_HC_Z_DAY))*100

#WHO_HEIGHT_Z_SCORE
WHO_HT_Z1_DAY <- (((HT_DAY/M_WHO_HT)^L_WHO_HT)-1)/(L_WHO_HT*S_WHO_HT)
WHO_HT_Z2_DAY <- (((HT_DAY/M_WHO_HT2)^L_WHO_HT2)-1)/(L_WHO_HT2*S_WHO_HT2)
WHO_HT_Z1_DAY[is.na(WHO_HT_Z1_DAY)] <- " "
WHO_HT_Z2_DAY[is.na(WHO_HT_Z2_DAY)] <- " "
WHO_HT_Z_DAY <- as.numeric(paste(WHO_HT_Z1_DAY, WHO_HT_Z2_DAY))


#WHO_HEIGHT_PERCENTILE
WHO_HT_PCTL_DAY <- (pnorm(WHO_HT_Z_DAY))*100

#WHO_SUBSCAPULAR_SKINFOLD_Z_SCORE
WHO_SSF_Z_DAY <- (((SSF_DAY/M_WHO_SSF)^L_WHO_SSF)-1)/(L_WHO_SSF*S_WHO_SSF)

#WHO_SUBSCAPULAR_SKINFOLD_PERCENTILE
WHO_SSF_PCTL_DAY <- (pnorm(WHO_SSF_Z_DAY))*100

#WHO_TRICEPS_SKINFOLD_Z_SCORE
WHO_TSF_Z_DAY <- (((TSF_DAY/M_WHO_TSF)^L_WHO_TSF)-1)/(L_WHO_TSF*S_WHO_TSF)

#WHO_TRICEPS_SKINFOLD_PERCENTILE
WHO_TSF_PCTL_DAY <- (pnorm(WHO_TSF_Z_DAY))*100

#WHO_UPPER_ARM_CIRCUMFERANCE_Z_SCORE
WHO_UAC_Z_DAY <- (((UAC_DAY/M_WHO_UAC)^L_WHO_UAC)-1)/(L_WHO_UAC*S_WHO_UAC)

#WHO_UPPER_ARM_CIRCUMFERANCE__PERCENTILE
WHO_UAC_PCTL_DAY <- (pnorm(WHO_UAC_Z_DAY))*100

#WHO_WEIGHT_FOR_HEIGHT_Z_SCORE
WHO_WT_HT_Z_DAY <- (((WT_DAY/M_WHO_WT_HT)^L_WHO_WT_HT)-1)/(L_WHO_WT_HT*S_WHO_WT_HT)

#WHO_WEIGHT_FOR_HEIGHT_PERCENTILE
WHO_WT_HT_PCTL_DAY <- (pnorm(WHO_WT_HT_Z_DAY))*100

#WHO_Z_AND_PCTL <- cbind(WHO_HT_PCTL_DAY, WHO_HT_Z_DAY, WHO_WT_PCTL_DAY, WHO_WT_Z_DAY, WHO_BMI_PCTL_DAY, WHO_BMI_Z_DAY, WHO_WT_HT_PCTL_DAY, WHO_WT_HT_Z_DAY, WHO_HC_PCTL_DAY, WHO_HC_Z_DAY, WHO_UAC_PCTL_DAY, WHO_UAC_Z_DAY, WHO_TSF_PCTL_DAY, WHO_TSF_Z_DAY, WHO_SSF_PCTL_DAY, WHO_SSF_Z_DAY)     

#Gator Circle
GC_DAY <- (NHANES_UC_Z_DAY* VC_PCTG_DAY)/100



#finaltable <- cbind.data.frame(MRNUMBER, DATE, DAY_TYPE, SOURCE, HT, WT, HC, UAC, TSF, SSF, USF, SISF, MBSF, UC, R, X, CDC_HT_PCTL, CDC_HT_Z, WHO_HT_PCTL, WHO_HT_Z, NHANES_HT_PCTL, NHANES_HT_Z, CDC_WT_PCTL, CDC_WT_Z, WHO_WT_PCTL, WHO_WT_Z, NHANES_WT_PCTL, NHANES_WT_Z, BMI,  CDC_BMI_PCTL, CDC_BMI_Z, WHO_BMI_PCTL, WHO_BMI_Z, NHANES_BMI_PCTL, NHANES_BMI_Z, CDC_WT_HT_PCTL, CDC_WT_HT_Z, WHO_WT_HT_PCTL, WHO_WT_HT_Z, NHANES_WT_HT_PCTL, NHANES_WT_HT_Z, CDC_HC_PCTL, CDC_HC_Z, WHO_HC_PCTL, WHO_HC_Z, WHO_UAC_PCTL, WHO_UAC_Z, NHANES_UAC_PCTL, NHANES_UAC_Z, WHO_TSF_PCTL, WHO_TSF_Z, NHANES_TSF_PCTL, NHANES_TSF_Z, UAA, NHANES_UAA_PCTL, NHANES_UAA_Z, AMC, AMA, NHANES_AMA_PCTL, NHANES_AMA_Z, AFA, NHANES_AFA_PCTL, NHANES_AFA_Z, WHO_SSF_PCTL, WHO_SSF_Z, NHANES_SSF_PCTL, NHANES_SSF_Z, NHANES_UC_PCTL, NHANES_UC_Z, VCA, VC_PCTG, GC, Z, P, ARPADI_FFM, GORAN_FFM, SCHAEFER_FFM, KOTLER_FFM, BODY_FAT_PCTG, HT_DAY, WT_DAY, HC_DAY, UAC_DAY, TSF_DAY, SSF_DAY, USF_DAY, SISF_DAY, MBSF_DAY, UC_DAY, R_DAY, X_DAY, CDC_HT_PCTL_DAY, CDC_HT_Z_DAY, WHO_HT_PCTL_DAY, WHO_HT_Z_DAY, NHANES_HT_PCTL_DAY, NHANES_HT_Z_DAY, CDC_WT_PCTL_DAY, CDC_WT_Z_DAY, WHO_WT_PCTL_DAY, WHO_WT_Z_DAY, NHANES_WT_PCTL_DAY, NHANES_WT_Z_DAY, BMI_DAY, CDC_BMI_PCTL_DAY, CDC_BMI_Z_DAY, WHO_BMI_PCTL_DAY, WHO_BMI_Z_DAY, NHANES_BMI_PCTL_DAY, NHANES_BMI_Z_DAY, CDC_WT_HT_PCTL_DAY, CDC_WT_HT_Z_DAY, WHO_WT_HT_PCTL_DAY, WHO_WT_HT_Z_DAY, NHANES_WT_HT_PCTL_DAY, NHANES_WT_HT_Z_DAY, CDC_HC_PCTL_DAY, CDC_HC_Z_DAY, WHO_HC_PCTL_DAY, WHO_HC_Z_DAY, WHO_UAC_PCTL_DAY, WHO_UAC_Z_DAY, NHANES_UAC_PCTL_DAY, NHANES_UAC_Z_DAY, WHO_TSF_PCTL_DAY, WHO_TSF_Z_DAY, NHANES_TSF_PCTL_DAY, NHANES_TSF_Z_DAY, UAA_DAY, NHANES_UAA_PCTL_DAY, NHANES_UAA_Z_DAY, AMC_DAY, AMA_DAY, NHANES_AMA_PCTL_DAY, NHANES_AMA_Z_DAY, AFA_DAY, NHANES_AFA_PCTL_DAY, NHANES_AFA_Z_DAY, WHO_SSF_PCTL_DAY, WHO_SSF_Z_DAY, NHANES_SSF_PCTL_DAY, NHANES_SSF_Z_DAY, NHANES_UC_PCTL_DAY, NHANES_UC_Z_DAY, VCA_DAY, VC_PCTG_DAY, GC_DAY, Z_DAY, P_DAY, ARPADI_FFM_DAY, GORAN_FFM_DAY, ARPADI_TBW_DAY, SCHAEFER_FFM_DAY, KOTLER_FFM_DAY, BODY_FAT_PCTG_DAY)
output2 <- cbind.data.frame(DATE1, AGE_DAY1,AGE_DOL1, AGE_MO, RACE, SEX, HT1, MEAN_NHANES_SSF, SD_NHANES_SSF, MEAN_NHANES_AFA, SD_NHANES_AFA, MEAN_NHANES_AMA, SD_NHANES_AMA, MEAN_NHANES_BMI, SD_NHANES_BMI, MEAN_NHANES_HT, SD_NHANES_HT, MEAN_NHANES_TSF, SD_NHANES_TSF, MEAN_NHANES_UAC, SD_NHANES_UAC, MEAN_NHANES_UAA, SD_NHANES_UAA, MEAN_NHANES_UC, SD_NHANES_UC, L_NHANES_UC, M_NHANES_UC, S_NHANES_UC, MEAN_NHANES_WT, SD_NHANES_WT, MEAN_NHANES_WT_FOR_HT, SD_NHANES_WT_FOR_HT,
                            L_CDC_HT, M_CDC_HT, S_CDC_HT, L_CDC_WT, M_CDC_WT, S_CDC_WT, L_CDC_BMI, M_CDC_BMI, S_CDC_BMI, L_CDC_WT_HT, M_CDC_WT_HT, S_CDC_WT_HT, L_CDC_HC, M_CDC_HC, S_CDC_HC,
                            L_WHO_HT, M_WHO_HT, S_WHO_HT, L_WHO_HT2, M_WHO_HT2, S_WHO_HT2, L_WHO_WT, M_WHO_WT, S_WHO_WT,L_WHO_WT2, M_WHO_WT2, S_WHO_WT2, L_WHO_BMI, M_WHO_BMI, S_WHO_BMI,L_WHO_BMI2, M_WHO_BMI2, S_WHO_BMI2, L_WHO_HC, M_WHO_HC, S_WHO_HC, L_WHO_UAC, M_WHO_UAC, S_WHO_UAC, L_WHO_TSF, M_WHO_TSF, S_WHO_TSF, L_WHO_SSF, M_WHO_SSF, S_WHO_SSF, L_WHO_WT_HT, M_WHO_WT_HT, S_WHO_WT_HT
)

interpolatedtable <- cbind.data.frame(DATE, HT_DAY, WT_DAY, HC_DAY, UAC_DAY, TSF_DAY, SSF_DAY, USF_DAY, SISF_DAY, MBSF_DAY, UC_DAY, R_DAY, X_DAY, CDC_HT_PCTL_DAY, CDC_HT_Z_DAY, WHO_HT_PCTL_DAY, WHO_HT_Z_DAY, NHANES_HT_PCTL_DAY, NHANES_HT_Z_DAY, CDC_WT_PCTL_DAY, CDC_WT_Z_DAY, WHO_WT_PCTL_DAY, WHO_WT_Z_DAY, NHANES_WT_PCTL_DAY, NHANES_WT_Z_DAY, BMI_DAY, CDC_BMI_PCTL_DAY, CDC_BMI_Z_DAY, WHO_BMI_PCTL_DAY, WHO_BMI_Z_DAY, NHANES_BMI_PCTL_DAY, NHANES_BMI_Z_DAY, CDC_WT_HT_PCTL_DAY, CDC_WT_HT_Z_DAY, WHO_WT_HT_PCTL_DAY, WHO_WT_HT_Z_DAY, NHANES_WT_HT_PCTL_DAY, NHANES_WT_HT_Z_DAY, CDC_HC_PCTL_DAY, CDC_HC_Z_DAY, WHO_HC_PCTL_DAY, WHO_HC_Z_DAY, WHO_UAC_PCTL_DAY, WHO_UAC_Z_DAY, NHANES_UAC_PCTL_DAY, NHANES_UAC_Z_DAY, WHO_TSF_PCTL_DAY, WHO_TSF_Z_DAY, NHANES_TSF_PCTL_DAY, NHANES_TSF_Z_DAY, UAA_DAY, NHANES_UAA_PCTL_DAY, NHANES_UAA_Z_DAY, AMC_DAY, AMA_DAY, NHANES_AMA_PCTL_DAY, NHANES_AMA_Z_DAY, AFA_DAY, NHANES_AFA_PCTL_DAY, NHANES_AFA_Z_DAY, WHO_SSF_PCTL_DAY, WHO_SSF_Z_DAY, NHANES_SSF_PCTL_DAY, NHANES_SSF_Z_DAY, NHANES_UC_PCTL_DAY, NHANES_UC_Z_DAY, VCA_DAY, VC_PCTG_DAY, GC_DAY, Z_DAY, P_DAY, ARPADI_FFM_DAY, GORAN_FFM_DAY, ARPADI_TBW_DAY, SCHAEFER_FFM_DAY, KOTLER_FFM_DAY, BODY_FAT_PCTG_DAY)
finaltable <- merge(anthrotable, interpolatedtable, by=c('DATE'), all.x=TRUE, all.y=TRUE)

#for AGE in the final excel table
Bday <- as.Date(Demographics.Identified$DOB, format= "%m/%d/%Y")
DATE <- finaltable$DATE
span <- interval(Bday, DATE)
AGE <- time_length(span, "year")
AGE <- floor(AGE*10)/10

finaltable$SOURCE[which(is.na(finaltable$SOURCE))] <- 4


day1 <- as.Date(Demographics.Identified$PKT_INITIATED_DATE)
lastday <- as.Date(Demographics.Identified$PKT_STOPPED_DATE)

#before first day
sub1 <- finaltable$DATE < day1
a <- finaltable$DAY_TYPE[sub1]
finaltable$DAY_TYPE[sub1] <- ifelse(is.na(a), 3, a)
a <- ifelse(is.na(a), 3, a)
finaltable$DAY_TYPE[sub1] <- ifelse(a!=1 & a!=4, 3, a)

#next
sub2 <- finaltable$DATE >= day1
b <- finaltable$DAY_TYPE[sub2]
finaltable$DAY_TYPE[sub2] <- ifelse(is.na(b), 2, b)
b <- ifelse(is.na(b), 2, b)
finaltable$DAY_TYPE[sub2] <- ifelse(b!=1 & b!=4, 2, b)

if (is.na(lastday)) {
  c <- 'NA'
} else if (!(is.na(lastday))) {
  sub3 <- finaltable$DATE > lastday
  c <- finaltable$DAY_TYPE[sub3]
  finaltable$DAY_TYPE[sub3] <- 3
}

#for CP and PA
#CP
finaltable$CP_DAY <- c(NA, finaltable$CP_DAY[!is.na(finaltable$CP_DAY)])[cumsum(!is.na(finaltable$CP_DAY)) + 1]
#PA
Bday <- as.Date(Demographics.Identified$DOB, format= "%m/%d/%Y") #First convert classes from factor to date
span <- interval(Bday, finaltable$DATE)
AGE_DAY <- as.period(span)
AGE_DAY <- as.numeric(year(AGE_DAY))
#to remove unwanted variables
rm(Bday, span)

sub <- AGE_DAY < 3
finaltable$PA_DAY[sub] <- NA
a <- which(!is.na(finaltable$PA_DAY))[1]
b <- finaltable$PA_DAY[a]
sub2 <- AGE_DAY > 2
sub2 <- sub2[1:a-1]
finaltable$PA_DAY[1:a-1] <- ifelse(sub2==TRUE, b, finaltable$PA_DAY)


c <- which((!is.na(finaltable$PA_DAY) & AGE_DAY > 18)==TRUE)[1]
d <- finaltable$PA_DAY[c]
sub3 <- AGE_DAY > 18
sub3 <- sub3[1:c-1]
finaltable$PA_DAY[1:c-1] <- ifelse(sub3==TRUE, d, finaltable$PA_DAY)

finaltable$PA_DAY <- c(NA, finaltable$PA_DAY[!is.na(finaltable$PA_DAY)])[cumsum(!is.na(finaltable$PA_DAY)) + 1]


#final
z <- dim(finaltable)[1]
finaltable$MRNUMBER <- rep.int(finaltable$MRNUMBER[1], z)
finaltable <- finaltable[ , c(2, 1, 3:ncol(finaltable)) ]
finaltable <- finaltable[, c(1:82, 84:ncol(finaltable), 83)] #putting comments at the end of the table
finaltable <- as.data.frame(append(finaltable, list(AGE = AGE), after=4))


#finaltable <- as.data.frame.numeric(finaltable)
#finaltable[is.na(finaltable)] <- " "
xlsx <- "ANTHROPOMETRICS_CLINICAL.xlsx"
xlsx <- gsub(" ","", paste(patient,"_", xlsx))
write.xlsx2(finaltable,file=xlsx,row.names=FALSE, showNA=FALSE)

#for checking reference purposes
xlsx <- "ANTHROPOMETRICS_REFERENCE.xlsx"
xlsx <- gsub(" ","", paste(patient,"_", xlsx))
write.xlsx2(output,file=xlsx,row.names=FALSE, showNA=FALSE)


xlsx <- "ANTHROPOMETRICS_REFERENCE_DAY.xlsx"
xlsx <- gsub(" ","", paste(patient,"_", xlsx))
write.xlsx2(output2,file=xlsx,row.names=FALSE, showNA=FALSE)

#check z-scores for flags. These flags represent z-scores which are unlikely for the PKT population and 
# indicates source data should be reviewed.

varsToCheck <- c("CDC_HT_Z","WHO_HT_Z","NHANES_HT_Z","CDC_WT_Z","WHO_WT_Z","NHANES_WT_Z","CDC_BMI_Z","WHO_BMI_Z","NHANES_BMI_Z","CDC_WT_HT_Z","WHO_WT_HT_Z","NHANES_WT_HT_Z","CDC_HC_Z","WHO_HC_Z","WHO_UAC_Z","NHANES_UAC_Z","WHO_TSF_Z","NHANES_TSF_Z","NHANES_UAA_Z","NHANES_AMA_Z","NHANES_AFA_Z","WHO_SSF_Z","NHANES_SSF_Z","NHANES_UC_Z")
zmin = -5
zmax = 3

n <- nrow(finaltable)*length(varsToCheck);
anthropometricsCheck <- data.frame(MRNUMBER = integer(n),DATE, FLAG_COLUMN = character(n), Z_SCORE = numeric(n), stringsAsFactors = FALSE)
DATE<-as.Date(anthropometricsCheck$DATE,format="%m/%d/%Y")

for(rowIdx in 1:nrow(finaltable)){
  flags = 0;
  if (finaltable[rowIdx,which(colnames(finaltable)=="SOURCE")]!=4){
    for (colIdx in 1:length(varsToCheck)){
      colVarID = which(colnames(finaltable)==varsToCheck[colIdx])
      if (!is.na(finaltable[rowIdx,colVarID]<=zmin)){
        if ((finaltable[rowIdx,colVarID]<=zmin)||(finaltable[rowIdx,colVarID]>=zmax)){
          # copy mr#, date, and flagged data columns that falls outside of range into excel sheet
          flags = flags+1;
          anthropometricsCheck$MRNUMBER[flags] <- finaltable[rowIdx,1]
          anthropometricsCheck$DATE[flags] <- finaltable[rowIdx,2]
          anthropometricsCheck$FLAG_COLUMN[flags] <- varsToCheck[colIdx]
          anthropometricsCheck$Z_SCORE[flags] <- finaltable[rowIdx,colVarID]
        }
      }
    }
  }
}

anthropometricsCheck[anthropometricsCheck==0]<-NA
na.omit(anthropometricsCheck)

# write Jurate's special excel document if there is a data flag
xlsx <- "ANTHROPOMETRICS_CHECK.xlsx"
xlsx <- gsub(" ","", paste(patient,"_", xlsx))
write.xlsx2(anthropometricsCheck,file=xlsx,row.names=FALSE, showNA=FALSE)

#Anthropometric GRAPH
#Graph is different depending on if patient is naive or experienced
naive <- ifelse(Demographics.Identified$STRATA[1] == "N", TRUE, FALSE)


if (naive == TRUE) {
  graphdata <- finaltable[finaltable$SOURCE==1 & finaltable$DAY_TYPE!=3,]
} else if (naive == FALSE) {
  before <- finaltable$DATE<Demographics.Identified$PKT_PROSPECTIVE_DATE
  graphdata1 <- finaltable[before,]
  graphdata1 <- graphdata1[graphdata1$SOURCE==2,]
  after <- (finaltable$DATE)>=(Demographics.Identified$PKT_PROSPECTIVE_DATE)
  graphdata2 <- finaltable[after,]
  graphdata2 <- graphdata2[graphdata2$SOURCE==1,]
  graphdata <- rbind(graphdata1, graphdata2)
  graphdata <- graphdata[graphdata$DAY_TYPE!=3,]
  
}


AGE <- graphdata$AGE

HT_Z_SCORE <- ifelse(AGE >= 2 & AGE <= 20, graphdata$CDC_HT_Z_DAY, ifelse(AGE < 2, graphdata$WHO_HT_Z_DAY, graphdata$NHANES_HT_Z_DAY))
WT_Z_SCORE <- ifelse(AGE >= 2 & AGE <= 20, graphdata$CDC_WT_Z_DAY, ifelse(AGE < 2, graphdata$WHO_WT_Z_DAY, graphdata$NHANES_WT_Z_DAY))
BMI_Z_SCORE <- ifelse(AGE >= 2 & AGE <= 20, graphdata$CDC_BMI_Z_DAY, ifelse(AGE < 2, graphdata$WHO_BMI_Z_DAY, graphdata$NHANES_BMI_Z_DAY))

HT_PCTL <- ifelse(AGE >= 2 & AGE <= 20, graphdata$CDC_HT_PCTL_DAY, ifelse(AGE < 2, graphdata$WHO_HT_PCTL_DAY, graphdata$NHANES_HT_PCTL_DAY))
WT_PCTL <- ifelse(AGE >= 2 & AGE <= 20, graphdata$CDC_WT_PCTL_DAY, ifelse(AGE < 2, graphdata$WHO_WT_PCTL_DAY, graphdata$NHANES_WT_PCTL_DAY))
BMI_PCTL <- ifelse(AGE >= 2 & AGE <= 20, graphdata$CDC_BMI_PCTL_DAY, ifelse(AGE < 2, graphdata$WHO_BMI_PCTL_DAY, graphdata$NHANES_BMI_PCTL_DAY))



#To see what values are being graphed:
DATE <- graphdata$DATE
MRNUMBER <- MRNUMBER[1:dim(graphdata)[1]]
data1 <- cbind.data.frame(DATE, HT_Z_SCORE, WT_Z_SCORE, BMI_Z_SCORE)
data2 <- cbind.data.frame(MRNUMBER, DATE, AGE, HT_Z_SCORE, WT_Z_SCORE, BMI_Z_SCORE, HT_PCTL, WT_PCTL, BMI_PCTL)

#Creating output table:
xlsx <- "ANTHROPOMETRICS_GRAPH_VALUES.xlsx"
xlsx <- gsub(" ","", paste(patient,"_", xlsx))
write.xlsx2(data2,file=xlsx,row.names=FALSE, showNA=FALSE)


#for y-axis labels
z <- c(data1$HT_Z_SCORE, data1$WT_Z_SCORE, data1$BMI_Z_SCORE)
z1 <- floor((min(z))/0.5)*0.5
z2 <- ceiling((max(z))/0.5)*0.5

datebreaks <- seq.Date(min(DATE), max(DATE), length.out=8)

p <- ggplot(data1, aes(x=DATE))
anthrograph <- p + geom_line(aes(y=HT_Z_SCORE, colour="Height Z-score"), size=1.5) + 
  geom_point(aes(y=HT_Z_SCORE, shape="Height Z-score", color="Height Z-score"), size=5) +
  geom_line(aes(y=WT_Z_SCORE, colour="Weight Z-score"), size=1.5) + 
  geom_point(aes(y=WT_Z_SCORE, shape="Weight Z-score", color="Weight Z-score"), size=4) +
  geom_line(aes(y=BMI_Z_SCORE, colour="BMI Z-score"), size=1.5) + 
  geom_point(aes(y=BMI_Z_SCORE, shape="BMI Z-score", color="BMI Z-score"), size=4) +
  xlab("Clinic Date") + 
  ylab("Z-score") +
  labs(title="Anthropometric Z-Scores Graph") +
  geom_hline(yintercept=seq(z1, z2, by=0.5)) +
  scale_y_continuous(breaks=seq(z1, z2, by=0.5)) +
  scale_x_date(breaks=datebreaks, date_labels= "%m/%d/%y") +
  theme_bw() +
  theme(axis.text.x = element_text(size=11),
        axis.title.x = element_text(size=12),
        axis.text.y = element_text(size=11),
        axis.title.y = element_text(size=12),
        title = element_text(size=12),
        legend.position="bottom",
        legend.background=element_rect(fill="white", colour="black"),
        legend.text=element_text(size=11),
        legend.key.width=unit(2, "line")) +
  scale_colour_manual(name = "", values=c("Height Z-score" = "orange", "Weight Z-score"="purple", "BMI Z-score" = "green3")) +
  scale_shape_manual(name = "", values=c("Height Z-score" = 18, "Weight Z-score"=15, "BMI Z-score"=17))


#save in patient folder, need to fix title
png <- "ANTHROPOMETRICS_GRAPH.png"
png <- gsub(" ","", paste(patient,"_", png))
ggsave(anthrograph, file=png, height=4.5, width=6.61, units='in', dpi=600)

#upload to MySQL function, need to run upload_anthros_database first
database <- function() {
  print("Would you like to upload anthropometrics data into a MySQL database?")
  print("Type 'YES' to do so, else type 'NO'")
  rl <- " "
  while (tolower(rl)!="yes" && tolower(rl)!="no") {
    rl <- readline(prompt="Enter here: ")
  }
  if (tolower(rl)=="yes") {
    finaltable<-subset(finaltable,select=-c(AGE,COMMENTS))
    uploadanthros(finaltable)
  }
}
database()


#use rm below to clean working directory so that you can automatically go to next patient?
rm(list=ls())
