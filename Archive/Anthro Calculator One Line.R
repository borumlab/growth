#for patient's with only one line of data
#temporary, will have to fix actual calculator script to take one line data into account


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
  Anthropometrics <<- read.xlsx(data,sheet=1,detectDates=TRUE) 
  
  
}


############################################################################

#using references tables
reference <- "G:/MySQL Database/Anthropometrics"
setwd(reference)
CDC.References <- read.csv('ANTHROPOMETRICS_CDC_REFERENCES_SOURCE.txt', header=TRUE, sep="\t", na.strings=c("","NA"))
NHANES.References <- read.csv('ANTHROPOMETRICS_NHANES_REFERENCES_SOURCE.txt', header=TRUE, sep="\t", na.strings=c("","NA"))
WHO.References <- read.csv('ANTHROPOMETRICS_WHO_REFERENCES_SOURCE.txt', header=TRUE, sep="\t", na.strings=c("","NA"))


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


#For NHANES WT for HT
for (i in seq(length(AGE))) {
  if (AGE[i] <= 11 & SEX ==1) {
    MEAN_NHANES_WT_FOR_HT <- c(MEAN_NHANES_WT_FOR_HT, subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==HT[i], select=c("MEAN_NHANES_WT_FOR_HT1")))
    SD_NHANES_WT_FOR_HT <- c(SD_NHANES_WT_FOR_HT, subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==HT[i], select=c("SD_NHANES_WT_FOR_HT1")))
  } 
  else if (AGE[i] > 11 & AGE[i] < 18 & SEX==1) {
    MEAN_NHANES_WT_FOR_HT <- c(MEAN_NHANES_WT_FOR_HT, subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==HT[i], select=c("MEAN_NHANES_WT_FOR_HT2")))
    SD_NHANES_WT_FOR_HT <- c(SD_NHANES_WT_FOR_HT, subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==HT[i], select=c("SD_NHANES_WT_FOR_HT2")))
  }
  else if (AGE[i] >= 18) {
    MEAN_NHANES_WT_FOR_HT <- c(MEAN_NHANES_WT_FOR_HT, subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT3==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT3==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT3==HT[i], select=c("MEAN_NHANES_WT_FOR_HT3")))
    SD_NHANES_WT_FOR_HT <- c(SD_NHANES_WT_FOR_HT, subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT3==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT3==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT3==HT[i], select=c("SD_NHANES_WT_FOR_HT3")))
  }
  else if (AGE[i] <= 10 & SEX==2 ) {
    MEAN_NHANES_WT_FOR_HT <- c(MEAN_NHANES_WT_FOR_HT, subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==HT[i], select=c("MEAN_NHANES_WT_FOR_HT1")))
    SD_NHANES_WT_FOR_HT <- c(SD_NHANES_WT_FOR_HT, subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==HT[i], select=c("SD_NHANES_WT_FOR_HT1")))
  } 
  else if (AGE[i] > 10 & AGE[i] < 18 & SEX==2) {
    MEAN_NHANES_WT_FOR_HT <- c(MEAN_NHANES_WT_FOR_HT, subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==HT[i], select=c("MEAN_NHANES_WT_FOR_HT2")))
    SD_NHANES_WT_FOR_HT <- c(SD_NHANES_WT_FOR_HT, subset(NHANES.References, NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE & NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX & NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==HT[i], select=c("SD_NHANES_WT_FOR_HT2")))
  }
}



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


#WHO References
L_WHO_HT <- c()
M_WHO_HT <- c()
S_WHO_HT <- c()
L_WHO_BMI <- c()
M_WHO_BMI <- c()
S_WHO_BMI <- c()
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
L_WHO_WT_HT <- c()
M_WHO_WT_HT <- c()
S_WHO_WT_HT <- c()

for (i in seq(length(AGE))) {
  L_WHO_HT <- c(L_WHO_HT, subset(WHO.References, WHO.References$AGE_MO_WHO_HT_AGE==AGE[i] & WHO.References$SEX_WHO_HT_AGE==SEX, select=c("L_WHO_HT_AGE")))
  M_WHO_HT <- c(M_WHO_HT, subset(WHO.References, WHO.References$AGE_MO_WHO_HT_AGE==AGE[i] & WHO.References$SEX_WHO_HT_AGE==SEX, select=c("M_WHO_HT_AGE")))
  S_WHO_HT <- c(S_WHO_HT, subset(WHO.References, WHO.References$AGE_MO_WHO_HT_AGE==AGE[i] & WHO.References$SEX_WHO_HT_AGE==SEX, select=c("S_WHO_HT_AGE")))
  L_WHO_BMI <- c(L_WHO_BMI, subset(WHO.References, WHO.References$AGE_MO_WHO_BMI_AGE==AGE[i] & WHO.References$SEX_WHO_BMI_AGE==SEX, select=c("L_WHO_BMI_AGE")))
  M_WHO_BMI <- c(M_WHO_BMI, subset(WHO.References, WHO.References$AGE_MO_WHO_BMI_AGE==AGE[i] & WHO.References$SEX_WHO_BMI_AGE==SEX, select=c("M_WHO_BMI_AGE")))
  S_WHO_BMI <- c(S_WHO_BMI, subset(WHO.References, WHO.References$AGE_MO_WHO_BMI_AGE==AGE[i] & WHO.References$SEX_WHO_BMI_AGE==SEX, select=c("S_WHO_BMI_AGE")))
  L_WHO_HC <- c(L_WHO_HC, subset(WHO.References, WHO.References$AGE_MO_WHO_HC_AGE==AGE[i] & WHO.References$SEX_WHO_HC_AGE==SEX, select = c("L_WHO_HC_AGE")))
  M_WHO_HC <- c(M_WHO_HC, subset(WHO.References, WHO.References$AGE_MO_WHO_HC_AGE==AGE[i] & WHO.References$SEX_WHO_HC_AGE==SEX, select = c("M_WHO_HC_AGE")))
  S_WHO_HC <- c(S_WHO_HC, subset(WHO.References, WHO.References$AGE_MO_WHO_HC_AGE==AGE[i] & WHO.References$SEX_WHO_HC_AGE==SEX, select = c("S_WHO_HC_AGE")))
  L_WHO_SSF <- c(L_WHO_SSF, subset(WHO.References, WHO.References$AGE_MO_WHO_SSF_AGE==AGE[i] & WHO.References$SEX_WHO_SSF_AGE==SEX, select=c("L_WHO_SSF_AGE")))
  M_WHO_SSF <- c(M_WHO_SSF, subset(WHO.References, WHO.References$AGE_MO_WHO_SSF_AGE==AGE[i] & WHO.References$SEX_WHO_SSF_AGE==SEX, select=c("M_WHO_SSF_AGE")))
  S_WHO_SSF <- c(S_WHO_SSF, subset(WHO.References, WHO.References$AGE_MO_WHO_SSF_AGE==AGE[i] & WHO.References$SEX_WHO_SSF_AGE==SEX, select=c("S_WHO_SSF_AGE")))
  L_WHO_TSF <- c(L_WHO_TSF, subset(WHO.References, WHO.References$AGE_MO_WHO_TSF_AGE==AGE[i] & WHO.References$SEX_WHO_TSF_AGE==SEX, select=c("L_WHO_TSF_AGE")))
  M_WHO_TSF <- c(M_WHO_TSF, subset(WHO.References, WHO.References$AGE_MO_WHO_TSF_AGE==AGE[i] & WHO.References$SEX_WHO_TSF_AGE==SEX, select=c("M_WHO_TSF_AGE")))
  S_WHO_TSF <- c(S_WHO_TSF, subset(WHO.References, WHO.References$AGE_MO_WHO_TSF_AGE==AGE[i] & WHO.References$SEX_WHO_TSF_AGE==SEX, select=c("S_WHO_TSF_AGE")))
  L_WHO_UAC <- c(L_WHO_UAC, subset(WHO.References, WHO.References$AGE_MO_WHO_UAC_AGE==AGE[i] & WHO.References$SEX_WHO_UAC_AGE==SEX, select=c("L_WHO_UAC_AGE")))
  M_WHO_UAC <- c(M_WHO_UAC, subset(WHO.References, WHO.References$AGE_MO_WHO_UAC_AGE==AGE[i] & WHO.References$SEX_WHO_UAC_AGE==SEX, select=c("M_WHO_UAC_AGE")))
  S_WHO_UAC <- c(S_WHO_UAC, subset(WHO.References, WHO.References$AGE_MO_WHO_UAC_AGE==AGE[i] & WHO.References$SEX_WHO_UAC_AGE==SEX, select=c("S_WHO_UAC_AGE")))
  L_WHO_WT <- c(L_WHO_WT, subset(WHO.References, WHO.References$AGE_MO_WHO_WT_AGE==AGE[i] & WHO.References$SEX_WHO_WT_AGE==SEX, select=c("L_WHO_WT_AGE")))
  M_WHO_WT <- c(M_WHO_WT, subset(WHO.References, WHO.References$AGE_MO_WHO_WT_AGE==AGE[i] & WHO.References$SEX_WHO_WT_AGE==SEX, select=c("M_WHO_WT_AGE")))
  S_WHO_WT <- c(S_WHO_WT, subset(WHO.References, WHO.References$AGE_MO_WHO_WT_AGE==AGE[i] & WHO.References$SEX_WHO_WT_AGE==SEX, select=c("S_WHO_WT_AGE")))
  L_WHO_WT_HT <- c(L_WHO_WT_HT, subset(WHO.References, WHO.References$HEIGHT_WHO_WT_FOR_HT==Anthropometrics$HT[i] & WHO.References$SEX_WHO_WT_FOR_HT==SEX, select=c("L_WHO_WT_FOR_HT")))
  M_WHO_WT_HT <- c(M_WHO_WT_HT, subset(WHO.References, WHO.References$HEIGHT_WHO_WT_FOR_HT==Anthropometrics$HT[i] & WHO.References$SEX_WHO_WT_FOR_HT==SEX, select=c("M_WHO_WT_FOR_HT")))
  S_WHO_WT_HT <- c(S_WHO_WT_HT, subset(WHO.References, WHO.References$HEIGHT_WHO_WT_FOR_HT==Anthropometrics$HT[i] & WHO.References$SEX_WHO_WT_FOR_HT==SEX, select=c("S_WHO_WT_FOR_HT")))
}


L_WHO_HT <- as.numeric(L_WHO_HT)
M_WHO_HT <- as.numeric(M_WHO_HT)
S_WHO_HT <- as.numeric(S_WHO_HT)
L_WHO_BMI <- as.numeric(L_WHO_BMI)
M_WHO_BMI <- as.numeric(M_WHO_BMI)
S_WHO_BMI <- as.numeric(S_WHO_BMI)
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
L_WHO_WT_HT <- as.numeric(L_WHO_WT_HT)
M_WHO_WT_HT <- as.numeric(M_WHO_WT_HT)
S_WHO_WT_HT <- as.numeric(S_WHO_WT_HT)


#WHO_LMS <- cbind(AGE, L_WHO_HT, M_WHO_HT, S_WHO_HT, L_WHO_WT, M_WHO_WT, S_WHO_WT, L_WHO_BMI, M_WHO_BMI, S_WHO_BMI, L_WHO_HC, M_WHO_HC, S_WHO_HC, L_WHO_UAC, M_WHO_UAC, S_WHO_UAC, L_WHO_TSF, M_WHO_TSF, S_WHO_TSF, L_WHO_SSF, M_WHO_SSF, S_WHO_SSF, L_WHO_WT_HT, M_WHO_WT_HT, S_WHO_WT_HT)

#WHO_WEIGHT_Z_SCORE
WHO_WT_Z <- (((Anthropometrics$WT/M_WHO_WT)^L_WHO_WT)-1)/(L_WHO_WT*S_WHO_WT)

#WHO_WEIGHT_PERCENTILE
WHO_WT_PCTL <- (pnorm(WHO_WT_Z))*100

#WHO_BMI_Z_SCORE
WHO_BMI_Z <- (((BMI/M_WHO_BMI)^L_WHO_BMI)-1)/(L_WHO_BMI*S_WHO_BMI)

#WHO_BMI_PERCENTILE
WHO_BMI_PCTL <- (pnorm(WHO_BMI_Z))*100

#WHO_HEAD_CIRCUMFERENCE_Z_SCORE
WHO_HC_Z <- (((Anthropometrics$HC/M_WHO_HC)^L_WHO_HC)-1)/(L_WHO_HC*S_WHO_HC)

#WHO_HEAD_CIRCUMFERENCE_PERCENTILE
WHO_HC_PCTL <- (pnorm(WHO_HC_Z))*100

#WHO_HEIGHT_Z_SCORE
WHO_HT_Z <- (((Anthropometrics$HT/M_WHO_HT)^L_WHO_HT)-1)/(L_WHO_HT*S_WHO_HT)

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


output <- cbind.data.frame(DATE, AGE_YEARS, AGE_MO, RACE, SEX, HT1, MEAN_NHANES_SSF, SD_NHANES_SSF, MEAN_NHANES_AFA, SD_NHANES_AFA, MEAN_NHANES_AMA, SD_NHANES_AMA, MEAN_NHANES_BMI, SD_NHANES_BMI, MEAN_NHANES_HT, SD_NHANES_HT, MEAN_NHANES_TSF, SD_NHANES_TSF, MEAN_NHANES_UAC, SD_NHANES_UAC, MEAN_NHANES_UAA, SD_NHANES_UAA, MEAN_NHANES_UC, SD_NHANES_UC, L_NHANES_UC, M_NHANES_UC, S_NHANES_UC, MEAN_NHANES_WT, SD_NHANES_WT, MEAN_NHANES_WT_FOR_HT, SD_NHANES_WT_FOR_HT,
                           L_CDC_HT, M_CDC_HT, S_CDC_HT, L_CDC_WT, M_CDC_WT, S_CDC_WT, L_CDC_BMI, M_CDC_BMI, S_CDC_BMI, L_CDC_WT_HT, M_CDC_WT_HT, S_CDC_WT_HT, L_CDC_HC, M_CDC_HC, S_CDC_HC,
                           L_WHO_HT, M_WHO_HT, S_WHO_HT, L_WHO_WT, M_WHO_WT, S_WHO_WT, L_WHO_BMI, M_WHO_BMI, S_WHO_BMI, L_WHO_HC, M_WHO_HC, S_WHO_HC, L_WHO_UAC, M_WHO_UAC, S_WHO_UAC, L_WHO_TSF, M_WHO_TSF, S_WHO_TSF, L_WHO_SSF, M_WHO_SSF, S_WHO_SSF, L_WHO_WT_HT, M_WHO_WT_HT, S_WHO_WT_HT)


HT_DAY <- HT	
WT_DAY <- WT	
HC_DAY <- HC	
UAC_DAY <- UAC	
TSF_DAY <- TSF	
SSF_DAY <- SSF	
USF_DAY <- USF	
SISF_DAY <- SISF	
MBSF_DAY <- MBSF	
UC_DAY <- UC	
R_DAY <- R	
X_DAY <- X	
CDC_HT_PCTL_DAY <- CDC_HT_PCTL
CDC_HT_Z_DAY	<- CDC_HT_Z
WHO_HT_PCTL_DAY	<- WHO_HT_PCTL
WHO_HT_Z_DAY	<- WHO_HT_Z
NHANES_HT_PCTL_DAY	<- NHANES_HT_PCTL
NHANES_HT_Z_DAY	<- NHANES_HT_Z
CDC_WT_PCTL_DAY	<- CDC_WT_PCTL
CDC_WT_Z_DAY	<- CDC_WT_Z
WHO_WT_PCTL_DAY	<- WHO_WT_PCTL
WHO_WT_Z_DAY <- WHO_WT_Z	
NHANES_WT_PCTL_DAY	<- NHANES_WT_PCTL
NHANES_WT_Z_DAY	<- NHANES_WT_Z
BMI_DAY <- BMI	
CDC_BMI_PCTL_DAY	<- CDC_BMI_PCTL
CDC_BMI_Z_DAY	<- CDC_BMI_Z
WHO_BMI_PCTL_DAY	<- WHO_BMI_PCTL
WHO_BMI_Z_DAY	<- WHO_BMI_Z
NHANES_BMI_PCTL_DAY	<- NHANES_BMI_PCTL
NHANES_BMI_Z_DAY	<- NHANES_BMI_Z
CDC_WT_HT_PCTL_DAY	<- CDC_WT_HT_PCTL
CDC_WT_HT_Z_DAY	<- CDC_WT_HT_Z
WHO_WT_HT_PCTL_DAY	<- WHO_WT_HT_PCTL
WHO_WT_HT_Z_DAY	<- WHO_WT_HT_Z
NHANES_WT_HT_PCTL_DAY	<- NHANES_WT_HT_PCTL
NHANES_WT_HT_Z_DAY	<- NHANES_WT_HT_Z
CDC_HC_PCTL_DAY	<- CDC_HC_PCTL
CDC_HC_Z_DAY	<- CDC_HC_Z
WHO_HC_PCTL_DAY	<- WHO_HC_PCTL
WHO_HC_Z_DAY	<- WHO_HC_Z
WHO_UAC_PCTL_DAY	<- WHO_UAC_PCTL
WHO_UAC_Z_DAY	<- WHO_UAC_Z
NHANES_UAC_PCTL_DAY	<- NHANES_UAC_PCTL
NHANES_UAC_Z_DAY	<- NHANES_UAC_Z
WHO_TSF_PCTL_DAY	<- WHO_TSF_PCTL
WHO_TSF_Z_DAY	<- WHO_TSF_Z
NHANES_TSF_PCTL_DAY	<- NHANES_TSF_PCTL
NHANES_TSF_Z_DAY	<- NHANES_TSF_Z
UAA_DAY	<- UAA
NHANES_UAA_PCTL_DAY	<- NHANES_UAA_PCTL
NHANES_UAA_Z_DAY	<- NHANES_UAA_Z
AMC_DAY	<- AMC
AMA_DAY	<- AMA
NHANES_AMA_PCTL_DAY	<- NHANES_AMA_PCTL
NHANES_AMA_Z_DAY	<- NHANES_AMA_Z
AFA_DAY	<- AFA
NHANES_AFA_PCTL_DAY	<- NHANES_AFA_PCTL
NHANES_AFA_Z_DAY	<- NHANES_AFA_Z
WHO_SSF_PCTL_DAY	<- WHO_SSF_PCTL
WHO_SSF_Z_DAY	<- WHO_SSF_Z
NHANES_SSF_PCTL_DAY	<- NHANES_SSF_PCTL
NHANES_SSF_Z_DAY	<- NHANES_SSF_Z
NHANES_UC_PCTL_DAY	<- NHANES_UC_PCTL
NHANES_UC_Z_DAY	<- NHANES_UC_Z
VCA_DAY	<- VCA
VC_PCTG_DAY	<- VC_PCTG
GC_DAY	<- GC
Z_DAY	<- Z
P_DAY	<- P
ARPADI_FFM_DAY	<- ARPADI_FFM
GORAN_FFM_DAY	<- GORAN_FFM
ARPADI_TBW_DAY	<- ARPADI_TBW
SCHAEFER_FFM_DAY	<- SCHAEFER_FFM
KOTLER_FFM_DAY	<- KOTLER_FFM
BODY_FAT_PCTG_DAY	<- BODY_FAT_PCTG
COMMENTS <- Anthropometrics$COMMENTS

#for AGE in the final excel table
Bday <- as.Date(Demographics.Identified$DOB, format= "%m/%d/%Y")
DATE <- anthrotable$DATE
span <- interval(Bday, DATE)
AGE <- time_length(span, "year")
AGE <- floor(AGE*10)/10


anthrotable <- cbind.data.frame(MRNUMBER, DATE, DAY_TYPE, SOURCE, AGE, HT, WT, HC, UAC, TSF, SSF, USF, SISF, MBSF, UC, R, X, CDC_HT_PCTL, CDC_HT_Z, WHO_HT_PCTL, WHO_HT_Z, NHANES_HT_PCTL, NHANES_HT_Z, CDC_WT_PCTL, CDC_WT_Z, WHO_WT_PCTL, WHO_WT_Z, NHANES_WT_PCTL, NHANES_WT_Z, BMI,  CDC_BMI_PCTL, CDC_BMI_Z, WHO_BMI_PCTL, WHO_BMI_Z, NHANES_BMI_PCTL, NHANES_BMI_Z, CDC_WT_HT_PCTL, CDC_WT_HT_Z, WHO_WT_HT_PCTL, WHO_WT_HT_Z, NHANES_WT_HT_PCTL, NHANES_WT_HT_Z, CDC_HC_PCTL, CDC_HC_Z, WHO_HC_PCTL, WHO_HC_Z, WHO_UAC_PCTL, WHO_UAC_Z, NHANES_UAC_PCTL, NHANES_UAC_Z, WHO_TSF_PCTL, WHO_TSF_Z, NHANES_TSF_PCTL, NHANES_TSF_Z, UAA, NHANES_UAA_PCTL, NHANES_UAA_Z, AMC, AMA, NHANES_AMA_PCTL, NHANES_AMA_Z, AFA, NHANES_AFA_PCTL, NHANES_AFA_Z, WHO_SSF_PCTL, WHO_SSF_Z, NHANES_SSF_PCTL, NHANES_SSF_Z, NHANES_UC_PCTL, NHANES_UC_Z, VCA, VC_PCTG, GC, Z, P, ARPADI_FFM, GORAN_FFM, SCHAEFER_FFM, KOTLER_FFM, BODY_FAT_PCTG, CP_DAY, PA_DAY,
                                HT_DAY, WT_DAY, HC_DAY, UAC_DAY, TSF_DAY, SSF_DAY, USF_DAY, SISF_DAY, MBSF_DAY, UC_DAY, R_DAY, X_DAY, CDC_HT_PCTL_DAY, CDC_HT_Z_DAY, WHO_HT_PCTL_DAY, WHO_HT_Z_DAY, NHANES_HT_PCTL_DAY, NHANES_HT_Z_DAY, CDC_WT_PCTL_DAY, CDC_WT_Z_DAY, WHO_WT_PCTL_DAY, WHO_WT_Z_DAY, NHANES_WT_PCTL_DAY, NHANES_WT_Z_DAY, BMI_DAY, CDC_BMI_PCTL_DAY, CDC_BMI_Z_DAY, WHO_BMI_PCTL_DAY, WHO_BMI_Z_DAY, NHANES_BMI_PCTL_DAY, NHANES_BMI_Z_DAY, CDC_WT_HT_PCTL_DAY, CDC_WT_HT_Z_DAY, WHO_WT_HT_PCTL_DAY, WHO_WT_HT_Z_DAY, NHANES_WT_HT_PCTL_DAY, NHANES_WT_HT_Z_DAY, CDC_HC_PCTL_DAY, CDC_HC_Z_DAY, WHO_HC_PCTL_DAY, WHO_HC_Z_DAY, WHO_UAC_PCTL_DAY, WHO_UAC_Z_DAY, NHANES_UAC_PCTL_DAY, NHANES_UAC_Z_DAY, WHO_TSF_PCTL_DAY, WHO_TSF_Z_DAY, NHANES_TSF_PCTL_DAY, NHANES_TSF_Z_DAY, UAA_DAY, NHANES_UAA_PCTL_DAY, NHANES_UAA_Z_DAY, AMC_DAY, AMA_DAY, NHANES_AMA_PCTL_DAY, NHANES_AMA_Z_DAY, AFA_DAY, NHANES_AFA_PCTL_DAY, NHANES_AFA_Z_DAY, WHO_SSF_PCTL_DAY, WHO_SSF_Z_DAY, NHANES_SSF_PCTL_DAY, NHANES_SSF_Z_DAY, NHANES_UC_PCTL_DAY, NHANES_UC_Z_DAY, VCA_DAY, VC_PCTG_DAY, GC_DAY, Z_DAY, P_DAY, ARPADI_FFM_DAY, GORAN_FFM_DAY, ARPADI_TBW_DAY, SCHAEFER_FFM_DAY, KOTLER_FFM_DAY, BODY_FAT_PCTG_DAY, COMMENTS)






#finaltable <- as.data.frame.numeric(finaltable)
#finaltable[is.na(finaltable)] <- " "
xlsx <- "ANTHROPOMETRICS_CLINICAL.xlsx"
xlsx <- gsub(" ","", paste(patient,"_", xlsx))
write.xlsx2(anthrotable,file=xlsx,row.names=FALSE, showNA=FALSE)

#for checking reference purposes
xlsx <- "ANTHROPOMETRICS_REFERENCE.xlsx"
xlsx <- gsub(" ","", paste(patient,"_", xlsx))
write.xlsx2(output,file=xlsx,row.names=FALSE, showNA=FALSE)







