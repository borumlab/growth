#' Calculate Anthropometrics
#'
#' This function allows you to calculate anthropometric calculations, z-scores, percentiles, and it creates a clinical anthropometric graph. It also gives you the option to create an excel file to check the values that are used to calculate the z-scores and percentiles, and the option to save another excel file to check for any data flags.
#' @keywords anthropometrics
#' @export
#' @import RMySQL
#' @import ggplot2
#' @import openxlsx
#' @import lubridate
#' @examples
#' calculate_anthro()



# Purpose - This script will take the anthropometric measurements from the FILA_ANTHROPOMETRICS_SOURCE file
#           and calculate z-scores and percentiles from the CDC, NHANES, and WHO files. It then interpolates
#           these values daily.
#           The script will give you as the user the option to specify the four letter initials signifying
#           the patient you wish to run this script for, and it will ask for the file path of the source sheet.
#           When the script finishes, you will be asked if you wish to save a FILA_ANTHROPOMETRICS_REFERENCE and
#           FILA_ANTHROPOMETRICS_REFERENCE_DAY to double-check the calculate z-scores and percentiles. You will also
#           be asked if you would like to check for a data flag, and if you would like to upload to MySQL.
#
#


calculate_anthro <- function() {

  #making enough memory for R to use for patients with larger data sheets
  options(java.parameters = "-Xmx10000m")


  #uploading needed packages
  library(rJava)
  library(openxlsx)
  library(xlsx)

  #input patient's initals
  print("Input the four letters that signify the patient we are doing calculations for")
  print("Example: FILA")
  patient <<- readline(prompt="Enter here: ")

  #input file path of ANTHROPOMETRIC_SOURCE sheet
  print("Input the directory that you wish to draw this patient's ANTHROPOMETRICS file from")
  print("Example: C:/Folder_Name/")
  directory <<- readline(prompt="Enter here: ")
  setwd(directory)

  data <- "ANTHROPOMETRICS_SOURCE.xlsx"
  data <- gsub(" ","",paste(patient,"_",data))
  Anthropometrics <- read.xlsx(data,sheet=1,detectDates=TRUE)
  Anthropometrics <- Anthropometrics[!is.na(Anthropometrics$MRNUMBER),]


  #uploading anthropometric references tables
  reference <- "G:/MySQL Database/Anthropometrics"
  setwd(reference)
  CDC.References <- read.csv('ANTHROPOMETRICS_CDC_REFERENCES_SOURCE_08012016.txt', header=TRUE, sep="\t", na.strings=c("","NA"))
  NHANES.References <- read.csv('ANTHROPOMETRICS_NHANES_REFERENCES_SOURCE_11182016.txt', header=TRUE, sep="\t", na.strings=c("","NA"))
  WHO.References <- read.csv('ANTHROPOMETRICS_WHO_REFERENCES_SOURCE_08012016.txt', header=TRUE, sep="\t", na.strings=c("","NA"))

  #uploading Demographics sheet
  setwd("G:/MySQL Database/Demographics/")
  DEMOGRAPHICS_SOURCE <- "DEMOGRAPHICS_SOURCE.xlsx"
  DEMOGRAPHICS_SOURCE <- read.xlsx(DEMOGRAPHICS_SOURCE,sheet=1, detectDates = TRUE)
  Demographics.Identified <<- DEMOGRAPHICS_SOURCE[which(DEMOGRAPHICS_SOURCE$MRNUMBER==Anthropometrics$MRNUMBER[1]), ]

  #saves in patient's folder
  setwd(directory)

  print("Starting calculations. Please wait...")

  #Calculating Age to use in equations
  i=1:dim(Anthropometrics)[1]
  Bday <- as.Date(Demographics.Identified$DOB, format= "%m/%d/%Y") #First convert class from factor to date
  Date <- as.Date(Anthropometrics$DATE[i], format = "%m/%d/%Y")
  #library(lubridate)
  span <- interval(Bday, Date)
  AGE <- as.period(span)
  AGE <- as.numeric(year(AGE))
  AGE_YEARS <- AGE
  AGE_MO <- as.period(span, "months")
  AGE_MO <- as.numeric(month(AGE_MO))


  #Gender
  #if patient is male then sex=1, if female sex=0
  SEX <- ifelse(Demographics.Identified$GENDER[1] == "M", 1, 0)

  #BMI
  BMI <- Anthropometrics$WT/(Anthropometrics$HT/100)^2

  #Upper Arm Muscle Circumference
  AMC <- Anthropometrics$UAC-(pi*Anthropometrics$TSF/10)

  #Upper Arm Area
  UAA <- 0.785*((Anthropometrics$UAC/pi)^2)

  #Upper Arm Muscle Area
  AMA <- ((Anthropometrics$UAC-(pi*Anthropometrics$TSF/10))^2)/(4*pi)

  #Upper Arm Fat Area
  AFA <- UAA-AMA

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

  #BODY_FAT_PERCENTAGE, this is calculated based on Age of Patient
  #If patient is less than 18, then SCHARFER_FFM is used for BODY_FAT_PCTG
  #IF patient is 18 or greater, KOTLER_FFM is used for BODY_FAT_PCTG
  FFM <- ifelse(AGE < 18, FFM <- SCHAEFER_FFM, FFM <- KOTLER_FFM)
  BODY_FAT_PCTG <- ((Anthropometrics$WT-FFM)/Anthropometrics$WT)*100


  #can run below to double check what has been calculated so far
  #ANTHRO <- cbind(AGE, BMI, AMC, UAA, AMA, AFA, VCA, VC_PCTG, Z, P, ARPADI_FFM, GORAN_FFM, ARPADI_TBW, SCHAEFER_FFM, KOTLER_FFM, BODY_FAT_PCTG)

  #Race for NHANES
  #if White then Race = 2, if Asian then Race = 2, if African-American then Race = 1, if Hispanic then Race = 2
  RACE <- if (Demographics.Identified$RACE[1] == "White"){RACE <- 2} else if (Demographics.Identified$RACE[1] == "Asian"){RACE <- 2}  else if (Demographics.Identified$RACE[1] == "African-American"){RACE <- 1} else if (Demographics.Identified$RACE[1] == "Hispanic"){RACE <- 2}

  #Gender for NHANES
  SEX <- ifelse(Demographics.Identified$GENDER[1] == "M", 1, 2)

  #newHT
  HT <- floor(Anthropometrics$HT)
  HT1 <- HT #for output table

  #NHANES using apply function
  #using the AGE, SEX, and RACE of the patient, the apply function grabs the appropriate mean and standard deviation from the ANTHROPOMETRICS_SOURCE_NHANES sheet
  #in the apply function, age is used as a function of x because age changes in value, while sex and race are fixed values

  MEAN_NHANES_HT <- as.numeric(sapply(AGE, function(x) subset(NHANES.References,
                                                              NHANES.References$AGE_YEAR_NHANES_HT_AGE==x &
                                                                NHANES.References$SEX_NHANES_HT_AGE==SEX &
                                                                NHANES.References$RACE_NHANES_HT_AGE==RACE,
                                                              select=c("MEAN_NHANES_HT_AGE"))))

  SD_NHANES_HT <- as.numeric(sapply(AGE, function(x) subset(NHANES.References,
                                                            NHANES.References$AGE_YEAR_NHANES_HT_AGE==x &
                                                              NHANES.References$SEX_NHANES_HT_AGE==SEX &
                                                              NHANES.References$RACE_NHANES_HT_AGE==RACE,
                                                            select=c("SD_NHANES_HT_AGE"))))

  MEAN_NHANES_WT <- as.numeric(sapply(AGE, function(x) subset(NHANES.References,
                                                              NHANES.References$AGE_YEAR_NHANES_WT_AGE==x &
                                                                NHANES.References$SEX_NHANES_WT_AGE==SEX &
                                                                NHANES.References$RACE_NHANES_WT_AGE==RACE,
                                                              select=c("MEAN_NHANES_WT_AGE"))))

  SD_NHANES_WT <- as.numeric(sapply(AGE, function(x) subset(NHANES.References,
                                                            NHANES.References$AGE_YEAR_NHANES_WT_AGE==x &
                                                              NHANES.References$SEX_NHANES_WT_AGE==SEX &
                                                              NHANES.References$RACE_NHANES_WT_AGE==RACE,
                                                            select=c("SD_NHANES_WT_AGE"))))

  MEAN_NHANES_BMI <- as.numeric(sapply(AGE, function(x) subset(NHANES.References,
                                                               NHANES.References$AGE_YEAR_NHANES_BMI_AGE==x &
                                                                 NHANES.References$SEX_NHANES_BMI_AGE==SEX &
                                                                 NHANES.References$RACE_NHANES_BMI_AGE==RACE,
                                                               select=c("MEAN_NHANES_BMI_AGE"))))

  SD_NHANES_BMI <- as.numeric(sapply(AGE, function(x) subset(NHANES.References,
                                                             NHANES.References$AGE_YEAR_NHANES_BMI_AGE==x &
                                                               NHANES.References$SEX_NHANES_BMI_AGE==SEX &
                                                               NHANES.References$RACE_NHANES_BMI_AGE==RACE,
                                                             select=c("SD_NHANES_BMI_AGE"))))
  ####################
  #NHANES UAC
  #Using different references. So for patients older than 20, use mean and standard deviation.
  #For patients 20 and younger, use LMS.
  MEAN_NHANES_UAC <- rep(NA,length(AGE))
  SD_NHANES_UAC <- rep(NA,length(AGE))
  AGE_UAC <- ifelse(AGE_MO <= 240, AGE_MO, AGE)
  a <- which((AGE_MO <= 240) == FALSE)
  MEAN_NHANES_UAC[a] <- as.numeric(sapply(AGE_UAC[a], function(x) subset(NHANES.References,
                                                                         NHANES.References$AGE_YEAR_NHANES_UAC_AGE==x &
                                                                           NHANES.References$SEX_NHANES_UAC_AGE==SEX &
                                                                           NHANES.References$RACE_NHANES_UAC_AGE==RACE,
                                                                         select=c("MEAN_NHANES_UAC_AGE"))))

  SD_NHANES_UAC[a] <- as.numeric(sapply(AGE_UAC[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_YEAR_NHANES_UAC_AGE==x &
                                                                         NHANES.References$SEX_NHANES_UAC_AGE==SEX &
                                                                         NHANES.References$RACE_NHANES_UAC_AGE==RACE,
                                                                       select=c("SD_NHANES_UAC_AGE"))))

  a <- which((AGE_MO <= 240) == TRUE)
  L_NHANES_UAC <- rep(NA,length(AGE))
  M_NHANES_UAC <- rep(NA,length(AGE))
  S_NHANES_UAC <- rep(NA,length(AGE))
  L_NHANES_UAC[a] <- as.numeric(sapply(AGE_UAC[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_UAC_AGE==x &
                                                                        NHANES.References$SEX_NHANES_UAC_AGE2==SEX,
                                                                      select=c("L_NHANES_UAC"))))

  M_NHANES_UAC[a] <- as.numeric(sapply(AGE_UAC[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_UAC_AGE==x &
                                                                        NHANES.References$SEX_NHANES_UAC_AGE2==SEX,
                                                                      select=c("M_NHANES_UAC"))))

  S_NHANES_UAC[a] <-  as.numeric(sapply(AGE_UAC[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_MONTHS_NHANES_UAC_AGE==x &
                                                                         NHANES.References$SEX_NHANES_UAC_AGE2==SEX,
                                                                       select=c("S_NHANES_UAC"))))
  #####################
  #NHANES TSF
  #Using different references. So for patients older than 239 months, use mean and standard deviation.
  #For patients 239 months and younger, use LMS.
  MEAN_NHANES_TSF <- rep(NA,length(AGE))
  SD_NHANES_TSF <- rep(NA,length(AGE))
  AGE_TSF <- ifelse(AGE_MO <= 239, AGE_MO, AGE)
  a <- which((AGE_MO <= 239) == FALSE)
  MEAN_NHANES_TSF[a] <- as.numeric(sapply(AGE_TSF[a], function(x) subset(NHANES.References,
                                                                         NHANES.References$AGE_YEAR_NHANES_TSF_AGE==x &
                                                                           NHANES.References$SEX_NHANES_TSF_AGE==SEX &
                                                                           NHANES.References$RACE_NHANES_TSF_AGE==RACE,
                                                                         select=c("MEAN_NHANES_TSF_AGE"))))

  SD_NHANES_TSF[a] <- as.numeric(sapply(AGE_TSF[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_YEAR_NHANES_TSF_AGE==x &
                                                                         NHANES.References$SEX_NHANES_TSF_AGE==SEX &
                                                                         NHANES.References$RACE_NHANES_TSF_AGE==RACE,
                                                                       select=c("SD_NHANES_TSF_AGE"))))

  a <- which((AGE_MO <= 239) == TRUE)
  L_NHANES_TSF <- rep(NA,length(AGE))
  M_NHANES_TSF <- rep(NA,length(AGE))
  S_NHANES_TSF <- rep(NA,length(AGE))
  L_NHANES_TSF[a] <- as.numeric(sapply(AGE_TSF[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_TSF_AGE2==x &
                                                                        NHANES.References$SEX_NHANES_TSF_AGE2==SEX,
                                                                      select=c("L_NHANES_TSF_AGE"))))

  M_NHANES_TSF[a] <- as.numeric(sapply(AGE_TSF[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_TSF_AGE2==x &
                                                                        NHANES.References$SEX_NHANES_TSF_AGE2==SEX,
                                                                      select=c("M_NHANES_TSF_AGE"))))

  S_NHANES_TSF[a] <-  as.numeric(sapply(AGE_TSF[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_MONTHS_NHANES_TSF_AGE2==x &
                                                                         NHANES.References$SEX_NHANES_TSF_AGE2==SEX,
                                                                       select=c("S_NHANES_TSF_AGE"))))

  ################

  MEAN_NHANES_UAA <- as.numeric(sapply(AGE, function(x) subset(NHANES.References,
                                                               NHANES.References$AGE_YEAR_NHANES_UAA_AGE==x &
                                                                 NHANES.References$SEX_NHANES_UAA_AGE==SEX &
                                                                 NHANES.References$RACE_NHANES_UAA_AGE==RACE,
                                                               select=c("MEAN_NHANES_UAA_AGE"))))

  SD_NHANES_UAA <- as.numeric(sapply(AGE, function(x) subset(NHANES.References,
                                                             NHANES.References$AGE_YEAR_NHANES_UAA_AGE==x &
                                                               NHANES.References$SEX_NHANES_UAA_AGE==SEX &
                                                               NHANES.References$RACE_NHANES_UAA_AGE==RACE,
                                                             select=c("SD_NHANES_UAA_AGE"))))
  ################
  #NHANES AMA
  #Using different references. So for patients older than 20, use mean and standard deviation.
  #For patients 20 and younger, use LMS.
  MEAN_NHANES_AMA <- rep(NA,length(AGE))
  SD_NHANES_AMA <- rep(NA,length(AGE))
  AGE_AMA <- ifelse(AGE_MO <= 240, AGE_MO, AGE)
  a <- which((AGE_MO <= 240) == FALSE)
  MEAN_NHANES_AMA[a] <- as.numeric(sapply(AGE_AMA[a], function(x) subset(NHANES.References,
                                                                         NHANES.References$AGE_YEAR_NHANES_AMA_AGE==x &
                                                                           NHANES.References$SEX_NHANES_AMA_AGE==SEX &
                                                                           NHANES.References$RACE_NHANES_AMA_AGE==RACE,
                                                                         select=c("MEAN_NHANES_AMA_AGE"))))

  SD_NHANES_AMA[a] <- as.numeric(sapply(AGE_AMA[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_YEAR_NHANES_AMA_AGE==x &
                                                                         NHANES.References$SEX_NHANES_AMA_AGE==SEX &
                                                                         NHANES.References$RACE_NHANES_AMA_AGE==RACE,
                                                                       select=c("SD_NHANES_AMA_AGE"))))

  a <- which((AGE_MO <= 240) == TRUE)
  L_NHANES_AMA <- rep(NA,length(AGE))
  M_NHANES_AMA <- rep(NA,length(AGE))
  S_NHANES_AMA <- rep(NA,length(AGE))
  L_NHANES_AMA[a] <- as.numeric(sapply(AGE_AMA[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_AMA_AGE==x &
                                                                        NHANES.References$SEX_NHANES_AMA_AGE2==SEX,
                                                                      select=c("L_NHANES_AMA"))))

  M_NHANES_AMA[a] <- as.numeric(sapply(AGE_AMA[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_AMA_AGE==x &
                                                                        NHANES.References$SEX_NHANES_AMA_AGE2==SEX,
                                                                      select=c("M_NHANES_AMA"))))

  S_NHANES_AMA[a] <-  as.numeric(sapply(AGE_AMA[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_MONTHS_NHANES_AMA_AGE==x &
                                                                         NHANES.References$SEX_NHANES_AMA_AGE2==SEX,
                                                                       select=c("S_NHANES_AMA"))))
  #######################
  #NHANES AFA
  #Using different references. So for patients older than 20, use mean and standard deviation.
  #For patients 20 and younger, use LMS.
  MEAN_NHANES_AFA <- rep(NA,length(AGE))
  SD_NHANES_AFA <- rep(NA,length(AGE))
  AGE_AFA <- ifelse(AGE_MO <= 240, AGE_MO, AGE)
  a <- which((AGE_MO <= 240) == FALSE)
  MEAN_NHANES_AFA[a] <- as.numeric(sapply(AGE_AFA[a], function(x) subset(NHANES.References,
                                                                         NHANES.References$AGE_YEAR_NHANES_AFA_AGE==x &
                                                                           NHANES.References$SEX_NHANES_AFA_AGE==SEX &
                                                                           NHANES.References$RACE_NHANES_AFA_AGE==RACE,
                                                                         select=c("MEAN_NHANES_AFA_AGE"))))

  SD_NHANES_AFA[a] <- as.numeric(sapply(AGE_AFA[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_YEAR_NHANES_AFA_AGE==x &
                                                                         NHANES.References$SEX_NHANES_AFA_AGE==SEX &
                                                                         NHANES.References$RACE_NHANES_AFA_AGE==RACE,
                                                                       select=c("SD_NHANES_AFA_AGE"))))

  a <- which((AGE_MO <= 240) == TRUE)
  L_NHANES_AFA <- rep(NA,length(AGE))
  M_NHANES_AFA <- rep(NA,length(AGE))
  S_NHANES_AFA <- rep(NA,length(AGE))
  L_NHANES_AFA[a] <- as.numeric(sapply(AGE_AFA[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_AFA_AGE==x &
                                                                        NHANES.References$SEX_NHANES_AFA_AGE2==SEX,
                                                                      select=c("L_NHANES_AFA"))))

  M_NHANES_AFA[a] <- as.numeric(sapply(AGE_AFA[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_AFA_AGE==x &
                                                                        NHANES.References$SEX_NHANES_AFA_AGE2==SEX,
                                                                      select=c("M_NHANES_AFA"))))

  S_NHANES_AFA[a] <-  as.numeric(sapply(AGE_AFA[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_MONTHS_NHANES_AFA_AGE==x &
                                                                         NHANES.References$SEX_NHANES_AFA_AGE2==SEX,
                                                                       select=c("S_NHANES_AFA"))))
  #################

  #NHANES SSF
  #Using different references. So for patients older than 239 months, use mean and standard deviation.
  #For patients 239 months and younger, use LMS.
  MEAN_NHANES_SSF <- rep(NA,length(AGE))
  SD_NHANES_SSF <- rep(NA,length(AGE))
  AGE_SSF <- ifelse(AGE_MO <= 239, AGE_MO, AGE)
  a <- which((AGE_MO <= 239) == FALSE)
  MEAN_NHANES_SSF[a] <- as.numeric(sapply(AGE_SSF[a], function(x) subset(NHANES.References,
                                                                         NHANES.References$AGE_YEAR_NHANES_SSSF_AGE==x &
                                                                           NHANES.References$SEX_NHANES_SSSF_AGE==SEX &
                                                                           NHANES.References$RACE_NHANES_SSSF_AGE==RACE,
                                                                         select=c("MEAN_NHANES_SSSF_AGE"))))

  SD_NHANES_SSF[a] <- as.numeric(sapply(AGE_SSF[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_YEAR_NHANES_SSSF_AGE==x &
                                                                         NHANES.References$SEX_NHANES_SSSF_AGE==SEX &
                                                                         NHANES.References$RACE_NHANES_SSSF_AGE==RACE,
                                                                       select=c("SD_NHANES_SSSF_AGE"))))

  a <- which((AGE_MO <= 239) == TRUE)
  L_NHANES_SSF <- rep(NA,length(AGE))
  M_NHANES_SSF <- rep(NA,length(AGE))
  S_NHANES_SSF <- rep(NA,length(AGE))
  L_NHANES_SSF[a] <- as.numeric(sapply(AGE_SSF[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_SSSF_AGE2==x &
                                                                        NHANES.References$SEX_NHANES_SSSF_AGE2==SEX,
                                                                      select=c("L_NHANES_SSSF_AGE"))))

  M_NHANES_SSF[a] <- as.numeric(sapply(AGE_SSF[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_SSSF_AGE2==x &
                                                                        NHANES.References$SEX_NHANES_SSSF_AGE2==SEX,
                                                                      select=c("M_NHANES_SSSF_AGE"))))

  S_NHANES_SSF[a] <-  as.numeric(sapply(AGE_SSF[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_MONTHS_NHANES_SSSF_AGE2==x &
                                                                         NHANES.References$SEX_NHANES_SSSF_AGE2==SEX,
                                                                       select=c("S_NHANES_SSSF_AGE"))))

  #################


  #NHANES WT FOR HT
  #creating empty data sets with the appropriate length to fill
  MEAN_NHANES_WT_FOR_HT <- rep(NA,length(AGE))
  SD_NHANES_WT_FOR_HT <- rep(NA,length(AGE))

  #find appropriate references for NHANES weight for height based on age, race, sex, and height
  num <- which((AGE < 12 & AGE >=2 & SEX==1) == TRUE)
  MEAN_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                              NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE &
                                                                                NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX &
                                                                                NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==x,
                                                                              select=c("MEAN_NHANES_WT_FOR_HT1"))))

  SD_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                            NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE &
                                                                              NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX &
                                                                              NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==x,
                                                                            select=c("SD_NHANES_WT_FOR_HT1"))))

  #############################
  num <- which((AGE >= 12 & AGE < 18 & SEX==1) == TRUE)
  MEAN_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                              NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE &
                                                                                NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX &
                                                                                NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==x,
                                                                              select=c("MEAN_NHANES_WT_FOR_HT2"))))


  SD_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                            NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE &
                                                                              NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX &
                                                                              NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==x,
                                                                            select=c("SD_NHANES_WT_FOR_HT2"))))
  #############################
  num <- which((AGE >= 18 & AGE < 75) == TRUE)
  MEAN_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                              NHANES.References$RACE_NHANES_WT_FOR_HT3==RACE &
                                                                                NHANES.References$SEX_NHANES_WT_FOR_HT3==SEX &
                                                                                NHANES.References$HEIGHT_NHANES_WT_FOR_HT3==x,
                                                                              select=c("MEAN_NHANES_WT_FOR_HT3"))))


  SD_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                            NHANES.References$RACE_NHANES_WT_FOR_HT3==RACE &
                                                                              NHANES.References$SEX_NHANES_WT_FOR_HT3==SEX &
                                                                              NHANES.References$HEIGHT_NHANES_WT_FOR_HT3==x,
                                                                            select=c("SD_NHANES_WT_FOR_HT3"))))

  #############################
  num <- which((AGE < 11 & AGE >= 2 & SEX == 2)  == TRUE)
  MEAN_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                              NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE &
                                                                                NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX &
                                                                                NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==x,
                                                                              select=c("MEAN_NHANES_WT_FOR_HT1"))))

  SD_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                            NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE &
                                                                              NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX &
                                                                              NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==x,
                                                                            select=c("SD_NHANES_WT_FOR_HT1"))))

  #############################
  num <- which((AGE >=11 & AGE < 18 & SEX==2) == TRUE)
  MEAN_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                              NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE &
                                                                                NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX &
                                                                                NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==x,
                                                                              select=c("MEAN_NHANES_WT_FOR_HT2"))))



  SD_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                            NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE &
                                                                              NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX &
                                                                              NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==x,
                                                                            select=c("SD_NHANES_WT_FOR_HT2"))))
  ###########################


  #NHANES UC (waist circumference)
  #redefining race for NHANES UC
  RACE <- if (Demographics.Identified$RACE[1] == "White"){RACE <- 2} else if (Demographics.Identified$RACE[1] == "Asian"){RACE <- 2}  else if (Demographics.Identified$RACE[1] == "African-American"){RACE <- 1} else if (Demographics.Identified$RACE[1] == "Hispanic"){RACE <- 3}
  MEAN_NHANES_UC <- rep(NA,length(AGE))
  SD_NHANES_UC <- rep(NA,length(AGE))
  AGE_UC <- ifelse(AGE <= 4 | AGE >= 19, AGE, AGE_MO)
  a <- which((AGE <= 4 | AGE >= 19) == TRUE)
  MEAN_NHANES_UC[a] <- as.numeric(sapply(AGE_UC[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_YEAR_NHANES_UC_AGE==x &
                                                                         NHANES.References$SEX_NHANES_UC_AGE==SEX &
                                                                         NHANES.References$RACE_NHANES_UC_AGE==RACE,
                                                                       select=c("MEAN_NHANES_UC_AGE"))))


  SD_NHANES_UC[a] <- as.numeric(sapply(AGE_UC[a], function(x) subset(NHANES.References,
                                                                     NHANES.References$AGE_YEAR_NHANES_UC_AGE==x &
                                                                       NHANES.References$SEX_NHANES_UC_AGE==SEX &
                                                                       NHANES.References$RACE_NHANES_UC_AGE==RACE,
                                                                     select=c("SD_NHANES_UC_AGE"))))

  L_NHANES_UC <- rep(NA,length(AGE))
  M_NHANES_UC <- rep(NA,length(AGE))
  S_NHANES_UC <- rep(NA,length(AGE))
  a <- which((AGE <= 4 | AGE >= 19) == FALSE)
  L_NHANES_UC[a] <- as.numeric(sapply(AGE_UC[a], function(x) subset(NHANES.References,
                                                                    NHANES.References$AGE_MO_NHANES_UC_AGE==x &
                                                                      NHANES.References$SEX_NHANES_UC_AGE2==SEX,
                                                                    select=c("L_NHANES_UC"))))

  M_NHANES_UC[a] <- as.numeric(sapply(AGE_UC[a], function(x) subset(NHANES.References,
                                                                    NHANES.References$AGE_MO_NHANES_UC_AGE==x &
                                                                      NHANES.References$SEX_NHANES_UC_AGE2==SEX,
                                                                    select=c("M_NHANES_UC"))))

  S_NHANES_UC[a] <-  as.numeric(sapply(AGE_UC[a], function(x) subset(NHANES.References,
                                                                     NHANES.References$AGE_MO_NHANES_UC_AGE==x &
                                                                       NHANES.References$SEX_NHANES_UC_AGE2==SEX,
                                                                     select=c("S_NHANES_UC"))))




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
  NHANES_SSF_Z1 <- (((Anthropometrics$SSF/M_NHANES_SSF)^L_NHANES_SSF)-1)/(L_NHANES_SSF*S_NHANES_SSF)
  NHANES_SSF_Z2 <- (Anthropometrics$SSF-MEAN_NHANES_SSF)/SD_NHANES_SSF
  NHANES_SSF_Z1[is.na(NHANES_SSF_Z1)] <- " "
  NHANES_SSF_Z2[is.na(NHANES_SSF_Z2)] <- " "
  NHANES_SSF_Z <- as.numeric(paste(NHANES_SSF_Z1, NHANES_SSF_Z2))

  #NHANES_SUBSCAPULAR_SKINFOLD_PERCENTILE
  NHANES_SSF_PCTL<- (pnorm(NHANES_SSF_Z))*100

  #NHANES_TRICEPS_SKINFOLD_Z_SCORE
  NHANES_TSF_Z1 <- (((Anthropometrics$TSF/M_NHANES_TSF)^L_NHANES_TSF)-1)/(L_NHANES_TSF*S_NHANES_TSF)
  NHANES_TSF_Z2 <- (Anthropometrics$TSF-MEAN_NHANES_TSF)/SD_NHANES_TSF
  NHANES_TSF_Z1[is.na(NHANES_TSF_Z1)] <- " "
  NHANES_TSF_Z2[is.na(NHANES_TSF_Z2)] <- " "
  NHANES_TSF_Z <- as.numeric(paste(NHANES_TSF_Z1, NHANES_TSF_Z2))

  #NHANES_TRICEPS_SKINFOLD_PERCENTILE
  NHANES_TSF_PCTL <- (pnorm(NHANES_TSF_Z))*100

  #NHANES_UPPER_ARM_AREA_Z_SCORE
  NHANES_UAA_Z <- (UAA-MEAN_NHANES_UAA)/SD_NHANES_UAA

  #NHANES_UPPER_ARM_AREA_PERCENTILE
  NHANES_UAA_PCTL <- (pnorm(NHANES_UAA_Z))*100

  #NHANES_UPPER_ARM_CIRCUMFERANCE_Z_SCORE
  NHANES_UAC_Z1 <- (((Anthropometrics$UAC/M_NHANES_UAC)^L_NHANES_UAC)-1)/(L_NHANES_UAC*S_NHANES_UAC)
  NHANES_UAC_Z2 <- (Anthropometrics$UAC-MEAN_NHANES_UAC)/SD_NHANES_UAC
  NHANES_UAC_Z1[is.na(NHANES_UAC_Z1)] <- " "
  NHANES_UAC_Z2[is.na(NHANES_UAC_Z2)] <- " "
  NHANES_UAC_Z <- as.numeric(paste(NHANES_UAC_Z1, NHANES_UAC_Z2))

  #NHANES_UPPER_ARM_CIRCUMFERANCE__PERCENTILE
  NHANES_UAC_PCTL <- (pnorm(NHANES_UAC_Z))*100

  #NHANES_UPPER_ARM_FAT_AREA_Z_SCORE
  NHANES_AFA_Z1 <- (((AFA/M_NHANES_AFA)^L_NHANES_AFA)-1)/(L_NHANES_AFA*S_NHANES_AFA)
  NHANES_AFA_Z2 <- (AFA-MEAN_NHANES_AFA)/SD_NHANES_AFA
  NHANES_AFA_Z1[is.na(NHANES_AFA_Z1)] <- " "
  NHANES_AFA_Z2[is.na(NHANES_AFA_Z2)] <- " "
  NHANES_AFA_Z <- as.numeric(paste(NHANES_AFA_Z1, NHANES_AFA_Z2))

  #NHANES_UPPER_ARM_FAT_AREA_PERCENTILE
  NHANES_AFA_PCTL <- (pnorm(NHANES_AFA_Z))*100

  #NHANES_UPPER_ARM_MUSCLE_AREA_Z_SCORE
  NHANES_AMA_Z1 <- (((AMA/M_NHANES_AMA)^L_NHANES_AMA)-1)/(L_NHANES_AMA*S_NHANES_AMA)
  NHANES_AMA_Z2 <- (AMA-MEAN_NHANES_AMA)/SD_NHANES_AMA
  NHANES_AMA_Z1[is.na(NHANES_AMA_Z1)] <- " "
  NHANES_AMA_Z2[is.na(NHANES_AMA_Z2)] <- " "
  NHANES_AMA_Z <- as.numeric(paste(NHANES_AMA_Z1, NHANES_AMA_Z2))

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

  #NHANES_Z_AND_PCTL <- cbind(NHANES_HT_PCTL, NHANES_HT_Z, NHANES_WT_PCTL, NHANES_WT_Z, NHANES_BMI_PCTL, NHANES_BMI_Z, NHANES_UAC_PCTL, NHANES_UAC_Z, NHANES_TSF_PCTL, NHANES_TSF_Z, NHANES_UAA_PCTL, NHANES_UAA_Z, NHANES_AMA_PCTL, NHANES_AMA_Z, NHANES_AFA_PCTL, NHANES_AFA_Z, NHANES_SSF_PCTL, NHANES_SSF_Z, NHANES_UC_PCTL, NHANES_UC_Z, NHANES_WT_HT_Z, NHANES_WT_HT_PCTL)

  #CDC

  #same SEX as NHANES, Age is now calculated in months
  i=1:dim(Anthropometrics)[1]
  Bday <- as.Date(Demographics.Identified$DOB[1], format= "%m/%d/%Y") #First convert classes from factor to date
  Date <- as.Date(Anthropometrics$DATE[i], format = "%m/%d/%Y")
  span <- interval(Bday, Date)
  AGE <- as.period(span, "months")
  AGE <- as.numeric(month(AGE))
  #to remove unwanted variables
  rm(i, Bday, span)


  #CDC using apply function
  #using the AGE and SEX of the patient, the apply function grabs the appropriate LMS values from the ANTHROPOMETRICS_SOURCE_CDC sheet
  #in the apply function, age is used as a function of x because age changes in value, while sex is a fixed value


  L_CDC_WT <- as.numeric(sapply(AGE, function(x) subset(CDC.References,
                                                        CDC.References$AGE_MO_CDC_WT_AGE==x &
                                                          CDC.References$SEX_CDC_WT_AGE==SEX,
                                                        select=c("L_CDC_WT_AGE"))))

  M_CDC_WT <- as.numeric(sapply(AGE, function(x) subset(CDC.References,
                                                        CDC.References$AGE_MO_CDC_WT_AGE==x &
                                                          CDC.References$SEX_CDC_WT_AGE==SEX,
                                                        select=c("M_CDC_WT_AGE"))))

  S_CDC_WT <- as.numeric(sapply(AGE, function(x) subset(CDC.References,
                                                        CDC.References$AGE_MO_CDC_WT_AGE==x &
                                                          CDC.References$SEX_CDC_WT_AGE==SEX,
                                                        select=c("S_CDC_WT_AGE"))))

  L_CDC_HT <- as.numeric(sapply(AGE, function(x) subset(CDC.References,
                                                        CDC.References$AGE_MO_CDC_HT_AGE==x &
                                                          CDC.References$SEX_CDC_HT_AGE==SEX,
                                                        select=c("L_CDC_HT_AGE"))))

  M_CDC_HT <- as.numeric(sapply(AGE, function(x) subset(CDC.References,
                                                        CDC.References$AGE_MO_CDC_HT_AGE==x &
                                                          CDC.References$SEX_CDC_HT_AGE==SEX,
                                                        select=c("M_CDC_HT_AGE"))))

  S_CDC_HT <- as.numeric(sapply(AGE, function(x) subset(CDC.References,
                                                        CDC.References$AGE_MO_CDC_HT_AGE==x &
                                                          CDC.References$SEX_CDC_HT_AGE==SEX,
                                                        select=c("S_CDC_HT_AGE"))))

  L_CDC_BMI <- as.numeric(sapply(AGE, function(x) subset(CDC.References,
                                                         CDC.References$AGE_MO_CDC_BMI_AGE==x &
                                                           CDC.References$SEX_CDC_BMI_AGE==SEX,
                                                         select=c("L_CDC_BMI_AGE"))))

  M_CDC_BMI <- as.numeric(sapply(AGE, function(x) subset(CDC.References,
                                                         CDC.References$AGE_MO_CDC_BMI_AGE==x &
                                                           CDC.References$SEX_CDC_BMI_AGE==SEX,
                                                         select=c("M_CDC_BMI_AGE"))))

  S_CDC_BMI <- as.numeric(sapply(AGE, function(x) subset(CDC.References,
                                                         CDC.References$AGE_MO_CDC_BMI_AGE==x &
                                                           CDC.References$SEX_CDC_BMI_AGE==SEX,
                                                         select=c("S_CDC_BMI_AGE"))))

  L_CDC_WT_HT <- as.numeric(sapply(HT, function(x) subset(CDC.References,
                                                          CDC.References$HEIGHT_CDC_WT_FOR_HT==x &
                                                            CDC.References$SEX_CDC_WT_FOR_HT ==SEX,
                                                          select=c("L_CDC_WT_FOR_HT"))))

  M_CDC_WT_HT <- as.numeric(sapply(HT, function(x) subset(CDC.References,
                                                          CDC.References$HEIGHT_CDC_WT_FOR_HT==x &
                                                            CDC.References$SEX_CDC_WT_FOR_HT ==SEX,
                                                          select=c("M_CDC_WT_FOR_HT"))))

  S_CDC_WT_HT <- as.numeric(sapply(HT, function(x) subset(CDC.References,
                                                          CDC.References$HEIGHT_CDC_WT_FOR_HT==x &
                                                            CDC.References$SEX_CDC_WT_FOR_HT ==SEX,
                                                          select=c("S_CDC_WT_FOR_HT"))))

  L_CDC_HC <- as.numeric(sapply(AGE, function(x) subset(CDC.References,
                                                        CDC.References$AGE_MO_CDC_HC_AGE==x &
                                                          CDC.References$SEX_CDC_HC_AGE==SEX,
                                                        select=c("L_CDC_HC_AGE"))))

  M_CDC_HC <- as.numeric(sapply(AGE, function(x) subset(CDC.References,
                                                        CDC.References$AGE_MO_CDC_HC_AGE==x &
                                                          CDC.References$SEX_CDC_HC_AGE==SEX,
                                                        select=c("M_CDC_HC_AGE"))))

  S_CDC_HC <- as.numeric(sapply(AGE, function(x) subset(CDC.References,
                                                        CDC.References$AGE_MO_CDC_HC_AGE==x &
                                                          CDC.References$SEX_CDC_HC_AGE==SEX,
                                                        select=c("S_CDC_HC_AGE"))))


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

  #WHO
  #same SEX as NHANES, need to calculate Age in days
  i=1:dim(Anthropometrics)[1]
  Bday <- as.Date(Demographics.Identified$DOB[1], format= "%m/%d/%Y") #First convert classes from factor to date
  Date <- as.Date(Anthropometrics$DATE[i], format = "%m/%d/%Y")
  span <- interval(Bday, Date)
  AGE_DOL <- as.period(span, "days")
  AGE_DOL <- as.numeric(day(AGE_DOL))
  #to remove unwanted variables
  rm(i, Bday, span)

  #define HT for WHO, because need to have HT to the nearest tenth
  Anthropometrics$ROUND_HT <- round(Anthropometrics$HT, digits=1)

  #WHO References
  #using the AGE and SEX of the patient, the apply function grabs the appropriate LMS values from the ANTHROPOMETRICS_SOURCE_WHO sheet
  #in the apply function, age is used as a function of x because age changes in value, while sex is a fixed value

  L_WHO_HC <- as.numeric(sapply(AGE_DOL, function(x) subset(WHO.References,
                                                            WHO.References$AGE_DAY_WHO_HC_AGE==x &
                                                              WHO.References$SEX_WHO_HC_AGE==SEX,
                                                            select = c("L_WHO_HC_AGE"))))

  M_WHO_HC <- as.numeric(sapply(AGE_DOL, function (x) subset(WHO.References,
                                                             WHO.References$AGE_DAY_WHO_HC_AGE==x &
                                                               WHO.References$SEX_WHO_HC_AGE==SEX,
                                                             select = c("M_WHO_HC_AGE"))))

  S_WHO_HC <- as.numeric(sapply(AGE_DOL, function(x) subset(WHO.References,
                                                            WHO.References$AGE_DAY_WHO_HC_AGE==x &
                                                              WHO.References$SEX_WHO_HC_AGE==SEX,
                                                            select = c("S_WHO_HC_AGE"))))

  L_WHO_SSF <- as.numeric(sapply(AGE_DOL, function(x) subset(WHO.References,
                                                             WHO.References$AGE_DAY_WHO_SSF_AGE==x &
                                                               WHO.References$SEX_WHO_SSF_AGE==SEX,
                                                             select=c("L_WHO_SSF_AGE"))))

  M_WHO_SSF <- as.numeric(sapply(AGE_DOL, function(x) subset(WHO.References,
                                                             WHO.References$AGE_DAY_WHO_SSF_AGE==x &
                                                               WHO.References$SEX_WHO_SSF_AGE==SEX,
                                                             select=c("M_WHO_SSF_AGE"))))


  S_WHO_SSF <- as.numeric(sapply(AGE_DOL, function(x) subset(WHO.References,
                                                             WHO.References$AGE_DAY_WHO_SSF_AGE==x &
                                                               WHO.References$SEX_WHO_SSF_AGE==SEX,
                                                             select=c("S_WHO_SSF_AGE"))))

  L_WHO_TSF <- as.numeric(sapply(AGE_DOL, function(x) subset(WHO.References,
                                                             WHO.References$AGE_DAY_WHO_TSF_AGE==x &
                                                               WHO.References$SEX_WHO_TSF_AGE==SEX,
                                                             select=c("L_WHO_TSF_AGE"))))


  M_WHO_TSF <- as.numeric(sapply(AGE_DOL, function(x) subset(WHO.References,
                                                             WHO.References$AGE_DAY_WHO_TSF_AGE==x &
                                                               WHO.References$SEX_WHO_TSF_AGE==SEX,
                                                             select=c("M_WHO_TSF_AGE"))))

  S_WHO_TSF <- as.numeric(sapply(AGE_DOL, function(x) subset(WHO.References,
                                                             WHO.References$AGE_DAY_WHO_TSF_AGE==x &
                                                               WHO.References$SEX_WHO_TSF_AGE==SEX,
                                                             select=c("S_WHO_TSF_AGE"))))

  L_WHO_UAC <- as.numeric(sapply(AGE_DOL, function(x) subset(WHO.References,
                                                             WHO.References$AGE_DAY_WHO_UAC_AGE==x &
                                                               WHO.References$SEX_WHO_UAC_AGE==SEX,
                                                             select=c("L_WHO_UAC_AGE"))))

  M_WHO_UAC <- as.numeric(sapply(AGE_DOL, function(x) subset(WHO.References,
                                                             WHO.References$AGE_DAY_WHO_UAC_AGE==x &
                                                               WHO.References$SEX_WHO_UAC_AGE==SEX,
                                                             select=c("M_WHO_UAC_AGE"))))

  S_WHO_UAC <- as.numeric(sapply(AGE_DOL, function(x) subset(WHO.References,
                                                             WHO.References$AGE_DAY_WHO_UAC_AGE==x &
                                                               WHO.References$SEX_WHO_UAC_AGE==SEX,
                                                             select=c("S_WHO_UAC_AGE"))))


  L_WHO_WT_HT <- as.numeric(sapply(Anthropometrics$ROUND_HT, function(x) subset(WHO.References,
                                                                                WHO.References$HEIGHT_WHO_WT_FOR_HT==x &
                                                                                  WHO.References$SEX_WHO_WT_FOR_HT==SEX,
                                                                                select=c("L_WHO_WT_FOR_HT"))))

  M_WHO_WT_HT <- as.numeric(sapply(Anthropometrics$ROUND_HT, function(x) subset(WHO.References,
                                                                                WHO.References$HEIGHT_WHO_WT_FOR_HT==x &
                                                                                  WHO.References$SEX_WHO_WT_FOR_HT==SEX,
                                                                                select=c("M_WHO_WT_FOR_HT"))))


  S_WHO_WT_HT <- as.numeric(sapply(Anthropometrics$ROUND_HT, function(x) subset(WHO.References,
                                                                                WHO.References$HEIGHT_WHO_WT_FOR_HT==x &
                                                                                  WHO.References$SEX_WHO_WT_FOR_HT==SEX,
                                                                                select=c("S_WHO_WT_FOR_HT"))))


  #For WHO parameters which can be calculated in days (0-1856) or months (61+)
  #creating empty data sets with the appropriate length
  L_WHO_HT <- rep(NA, length(AGE))
  M_WHO_HT <- rep(NA, length(AGE))
  S_WHO_HT <- rep(NA, length(AGE))
  L_WHO_WT <- rep(NA, length(AGE))
  M_WHO_WT <- rep(NA, length(AGE))
  S_WHO_WT <- rep(NA, length(AGE))
  L_WHO_BMI <- rep(NA, length(AGE))
  M_WHO_BMI <- rep(NA, length(AGE))
  S_WHO_BMI <- rep(NA, length(AGE))

  AGE_WHO <- ifelse(AGE_DOL <= 1856 , AGE_DOL, AGE)
  #find appropriate references for WHO HT, WT, and BMI when AGE_DOL is less than 1856
  t <- which((AGE_DOL <= 1856) == TRUE)
  L_WHO_HT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_DAY_WHO_HT_AGE==x &
                                                                    WHO.References$SEX_WHO_HT_AGE2==SEX,
                                                                  select=c("L_WHO_HT_AGE2"))))
  M_WHO_HT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_DAY_WHO_HT_AGE==x &
                                                                    WHO.References$SEX_WHO_HT_AGE2==SEX,
                                                                  select=c("M_WHO_HT_AGE2"))))
  S_WHO_HT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_DAY_WHO_HT_AGE==x &
                                                                    WHO.References$SEX_WHO_HT_AGE2==SEX,
                                                                  select=c("S_WHO_HT_AGE2"))))

  L_WHO_WT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_DAY_WHO_WT_AGE==x &
                                                                    WHO.References$SEX_WHO_WT_AGE2==SEX,
                                                                  select=c("L_WHO_WT_AGE2"))))
  M_WHO_WT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_DAY_WHO_WT_AGE==x &
                                                                    WHO.References$SEX_WHO_WT_AGE2==SEX,
                                                                  select=c("M_WHO_WT_AGE2"))))
  S_WHO_WT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_DAY_WHO_WT_AGE==x &
                                                                    WHO.References$SEX_WHO_WT_AGE2==SEX,
                                                                  select=c("S_WHO_WT_AGE2"))))

  L_WHO_BMI[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                   WHO.References$AGE_DAY_WHO_BMI_AGE==x &
                                                                     WHO.References$SEX_WHO_BMI_AGE2==SEX,
                                                                   select=c("L_WHO_BMI_AGE2"))))
  M_WHO_BMI[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                   WHO.References$AGE_DAY_WHO_BMI_AGE==x &
                                                                     WHO.References$SEX_WHO_BMI_AGE2==SEX,
                                                                   select=c("M_WHO_BMI_AGE2"))))
  S_WHO_BMI[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                   WHO.References$AGE_DAY_WHO_BMI_AGE==x &
                                                                     WHO.References$SEX_WHO_BMI_AGE2==SEX,
                                                                   select=c("S_WHO_BMI_AGE2"))))

  #find appropriate references for WHO HT, WT, and BMI when AGE_DOL is not less than 1856
  t <- which((AGE_DOL <= 1856)==FALSE)
  L_WHO_HT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_MO_WHO_HT_AGE==x &
                                                                    WHO.References$SEX_WHO_HT_AGE==SEX,
                                                                  select=c("L_WHO_HT_AGE"))))
  M_WHO_HT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_MO_WHO_HT_AGE==x &
                                                                    WHO.References$SEX_WHO_HT_AGE==SEX,
                                                                  select=c("M_WHO_HT_AGE"))))
  S_WHO_HT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_MO_WHO_HT_AGE==x &
                                                                    WHO.References$SEX_WHO_HT_AGE==SEX,
                                                                  select=c("S_WHO_HT_AGE"))))

  L_WHO_WT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_MO_WHO_WT_AGE==x &
                                                                    WHO.References$SEX_WHO_WT_AGE==SEX,
                                                                  select=c("L_WHO_WT_AGE"))))
  M_WHO_WT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_MO_WHO_WT_AGE==x &
                                                                    WHO.References$SEX_WHO_WT_AGE==SEX,
                                                                  select=c("M_WHO_WT_AGE"))))
  S_WHO_WT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_MO_WHO_WT_AGE==x &
                                                                    WHO.References$SEX_WHO_WT_AGE==SEX,
                                                                  select=c("S_WHO_WT_AGE"))))


  L_WHO_BMI[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                   WHO.References$AGE_MO_WHO_BMI_AGE==x &
                                                                     WHO.References$SEX_WHO_BMI_AGE==SEX,
                                                                   select=c("L_WHO_BMI_AGE"))))
  M_WHO_BMI[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                   WHO.References$AGE_MO_WHO_BMI_AGE==x &
                                                                     WHO.References$SEX_WHO_BMI_AGE==SEX,
                                                                   select=c("M_WHO_BMI_AGE"))))
  S_WHO_BMI[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                   WHO.References$AGE_MO_WHO_BMI_AGE==x &
                                                                     WHO.References$SEX_WHO_BMI_AGE==SEX,
                                                                   select=c("S_WHO_BMI_AGE"))))


  #WHO_LMS <- cbind(AGE_MO,AGE_DOL,L_WHO_HT, M_WHO_HT, S_WHO_HT,L_WHO_HT2, M_WHO_HT2, S_WHO_HT2, L_WHO_WT, M_WHO_WT, S_WHO_WT,L_WHO_WT2, M_WHO_WT2, S_WHO_WT2, L_WHO_BMI, M_WHO_BMI, S_WHO_BMI,L_WHO_BMI2, M_WHO_BMI2, S_WHO_BMI2, L_WHO_HC, M_WHO_HC, S_WHO_HC, L_WHO_UAC, M_WHO_UAC, S_WHO_UAC, L_WHO_TSF, M_WHO_TSF, S_WHO_TSF, L_WHO_SSF, M_WHO_SSF, S_WHO_SSF, L_WHO_WT_HT, M_WHO_WT_HT, S_WHO_WT_HT)

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

  WHO_Z_AND_PCTL <- cbind(WHO_HT_PCTL, WHO_HT_Z, WHO_WT_PCTL, WHO_WT_Z, WHO_BMI_PCTL, WHO_BMI_Z, WHO_WT_HT_PCTL, WHO_WT_HT_Z, WHO_HC_PCTL, WHO_HC_Z, WHO_UAC_PCTL, WHO_UAC_Z, WHO_TSF_PCTL, WHO_TSF_Z, WHO_SSF_PCTL, WHO_SSF_Z)


  #Gator Circle
  GC <- (NHANES_UC_Z* VC_PCTG)/100

  #defining column headers for excel sheet
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
  output <- cbind.data.frame(DATE, AGE_YEARS, AGE_MO, AGE_DOL, RACE, SEX, HT1, L_NHANES_SSF, M_NHANES_SSF, S_NHANES_SSF, MEAN_NHANES_SSF, SD_NHANES_SSF, L_NHANES_AFA, M_NHANES_AFA, S_NHANES_AFA, MEAN_NHANES_AFA, SD_NHANES_AFA, L_NHANES_AMA, M_NHANES_AMA, S_NHANES_AMA, MEAN_NHANES_AMA, SD_NHANES_AMA, MEAN_NHANES_BMI, SD_NHANES_BMI, MEAN_NHANES_HT, SD_NHANES_HT, L_NHANES_TSF, M_NHANES_TSF, S_NHANES_TSF, MEAN_NHANES_TSF, SD_NHANES_TSF, L_NHANES_UAC, M_NHANES_UAC, S_NHANES_UAC, MEAN_NHANES_UAC, SD_NHANES_UAC, MEAN_NHANES_UAA, SD_NHANES_UAA, MEAN_NHANES_UC, SD_NHANES_UC, L_NHANES_UC, M_NHANES_UC, S_NHANES_UC, MEAN_NHANES_WT, SD_NHANES_WT, MEAN_NHANES_WT_FOR_HT, SD_NHANES_WT_FOR_HT,
                             L_CDC_HT, M_CDC_HT, S_CDC_HT, L_CDC_WT, M_CDC_WT, S_CDC_WT, L_CDC_BMI, M_CDC_BMI, S_CDC_BMI, L_CDC_WT_HT, M_CDC_WT_HT, S_CDC_WT_HT, L_CDC_HC, M_CDC_HC, S_CDC_HC,
                             L_WHO_HT, M_WHO_HT, S_WHO_HT, L_WHO_WT, M_WHO_WT, S_WHO_WT, L_WHO_BMI, M_WHO_BMI, S_WHO_BMI,L_WHO_HC, M_WHO_HC, S_WHO_HC, L_WHO_UAC, M_WHO_UAC, S_WHO_UAC, L_WHO_TSF, M_WHO_TSF, S_WHO_TSF, L_WHO_SSF, M_WHO_SSF, S_WHO_SSF, L_WHO_WT_HT, M_WHO_WT_HT, S_WHO_WT_HT)


  print("Starting interpolated calculations. Please wait...")


  #Interpolations
  anthro <- Anthropometrics
  demo <- Demographics.Identified
  prodate <- Demographics.Identified$PKT_PROSPECTIVE_DATE

  if (anyNA(Demographics.Identified$STRATA[1]) == FALSE) {
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
  } else if (anyNA(Demographics.Identified$STRATA[1]) == TRUE) {
    anthro <- Anthropometrics
  }

  y1 <- as.Date(anthro$DATE[1], format="%m/%d/%Y")
  y2 <- as.Date(anthro$DATE[length(anthro[,1])], format="%m/%d/%Y")
  DATE <- seq(y1, y2, by="days")

  z <- length(DATE)


  #HT
  if ((sum(!is.na(anthro$HT)) > 1) == TRUE ) {

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

    a <- which(!is.na(anthro$HT))[1]
    b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
    c <- which(DATE==b)
    HT_DAY <- c(rep(NA, c-1), HT_DAY)
    length(HT_DAY) <- z

  } else if (all(is.na(anthro$HT)) == TRUE) {
    HT_DAY <- NA
    length(HT_DAY) <- z
  } else if (sum(!is.na(anthro$HT)) == 1) {
    a <- which(DATE == anthro$DATE[which(anthro$HT == (anthro$HT[!(is.na(anthro$HT))]))])
    HT_DAY <- c()
    HT_DAY[a] <- anthro$HT[!(is.na(anthro$HT))]
    length(HT_DAY) <- z


  }


  #WT
  if ((sum(!is.na(anthro$WT)) > 1) == TRUE) {

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

    a <- which(!is.na(anthro$WT))[1]
    b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
    c <- which(DATE==b)
    WT_DAY <- c(rep(NA, c-1), WT_DAY)
    length(WT_DAY) <- z

  }  else if (all(is.na(anthro$WT)) == TRUE) {
    WT_DAY <- NA
    length(WT_DAY) <- z
  } else if (sum(!is.na(anthro$WT)) == 1) {
    a <- which(DATE == anthro$DATE[which(anthro$WT == (anthro$WT[!(is.na(anthro$WT))]))])
    WT_DAY <- c()
    WT_DAY[a] <- anthro$WT[!(is.na(anthro$WT))]
    length(WT_DAY) <- z
  }


  #HC
  if ((sum(!is.na(anthro$HC)) > 1) == TRUE) {
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

    a <- which(!is.na(anthro$HC))[1]
    b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
    c <- which(DATE==b)
    HC_DAY <- c(rep(NA, c-1), HC_DAY)
    length(HC_DAY) <- z

  }  else if (all(is.na(anthro$HC)) == TRUE) {
    HC_DAY <- NA
    length(HC_DAY) <- z
  } else if (sum(!is.na(anthro$HC)) == 1) {
    a <- which(DATE == anthro$DATE[which(anthro$HC == (anthro$HC[!(is.na(anthro$HC))]))])
    HC_DAY <- c()
    HC_DAY[a] <- anthro$HC[!(is.na(anthro$HC))]
    length(HC_DAY) <- z


  }

  #UAC

  if ((sum(!is.na(anthro$UAC)) > 1) == TRUE) {


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

    a <- which(!is.na(anthro$UAC))[1]
    b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
    c <- which(DATE==b)
    UAC_DAY <- c(rep(NA, c-1), UAC_DAY)
    length(UAC_DAY) <- z

  }else if (all(is.na(anthro$UAC)) == TRUE) {
    UAC_DAY <- NA
    length(UAC_DAY) <- z
  } else if (sum(!is.na(anthro$UAC)) == 1) {
    a <- which(DATE == anthro$DATE[which(anthro$UAC == (anthro$UAC[!(is.na(anthro$UAC))]))])
    UAC_DAY <- c()
    UAC_DAY[a] <- anthro$UAC[!(is.na(anthro$UAC))]
    length(UAC_DAY) <- z
  }

  #TSF

  if ((sum(!is.na(anthro$TSF)) > 1) == TRUE) {

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

    a <- which(!is.na(anthro$TSF))[1]
    b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
    c <- which(DATE==b)
    TSF_DAY <- c(rep(NA, c-1), TSF_DAY)
    length(TSF_DAY) <- z


  } else if (all(is.na(anthro$TSF)) == TRUE) {
    TSF_DAY <- NA
    length(TSF_DAY) <- z
  } else if (sum(!is.na(anthro$TSF)) == 1) {
    a <- which(DATE == anthro$DATE[which(anthro$TSF == (anthro$TSF[!(is.na(anthro$TSF))]))])
    TSF_DAY <- c()
    TSF_DAY[a] <- anthro$TSF[!(is.na(anthro$TSF))]
    length(TSF_DAY) <- z
  }


  #SSF
  if ((sum(!is.na(anthro$SSF)) > 1) == TRUE) {
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

    a <- which(!is.na(anthro$SSF))[1]
    b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
    c <- which(DATE==b)
    SSF_DAY <- c(rep(NA, c-1), SSF_DAY)
    length(SSF_DAY) <- z

  } else if (all(is.na(anthro$SSF)) == TRUE) {
    SSF_DAY <- NA
    length(SSF_DAY) <- z
  } else if (sum(!is.na(anthro$SSF)) == 1) {
    a <- which(DATE == anthro$DATE[which(anthro$SSF == (anthro$SSF[!(is.na(anthro$SSF))]))])
    SSF_DAY <- c()
    SSF_DAY[a] <- anthro$SSF[!(is.na(anthro$SSF))]
    length(SSF_DAY) <- z
  }

  #USF
  if ((sum(!is.na(anthro$USF)) > 1) == TRUE) {
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

    a <- which(!is.na(anthro$USF))[1]
    b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
    c <- which(DATE==b)
    USF_DAY <- c(rep(NA, c-1), USF_DAY)
    length(USF_DAY) <- z

  } else if (all(is.na(anthro$USF)) == TRUE) {
    USF_DAY <- NA
    length(USF_DAY) <- z
  }  else if (sum(!is.na(anthro$USF)) == 1) {
    a <- which(DATE == anthro$DATE[which(anthro$USF == (anthro$USF[!(is.na(anthro$USF))]))])
    USF_DAY <- c()
    USF_DAY[a] <- anthro$USF[!(is.na(anthro$USF))]
    length(USF_DAY) <- z
  }

  #SISF
  if ((sum(!is.na(anthro$SISF)) > 1) == TRUE) {
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

    a <- which(!is.na(anthro$SISF))[1]
    b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
    c <- which(DATE==b)
    SISF_DAY <- c(rep(NA, c-1), SISF_DAY)
    length(SISF_DAY) <- z

  } else if (all(is.na(anthro$SISF)) == TRUE) {
    SISF_DAY <- NA
    length(SISF_DAY) <- z
  } else if (sum(!is.na(anthro$SISF)) == 1) {
    a <- which(DATE == anthro$DATE[which(anthro$SISF == (anthro$SISF[!(is.na(anthro$SISF))]))])
    SISF_DAY <- c()
    SISF_DAY[a] <- anthro$SISF[!(is.na(anthro$SISF))]
    length(SISF_DAY) <- z
  }

  #MBSF
  if ((sum(!is.na(anthro$MBSF)) > 1) == TRUE) {
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

    a <- which(!is.na(anthro$MBSF))[1]
    b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
    c <- which(DATE==b)
    MBSF_DAY <- c(rep(NA, c-1), MBSF_DAY)
    length(MBSF_DAY) <- z

  } else if (all(is.na(anthro$MBSF)) == TRUE) {
    MBSF_DAY <- NA
    length(MBSF_DAY) <- z
  } else if (sum(!is.na(anthro$MBSF)) == 1) {
    a <- which(DATE == anthro$DATE[which(anthro$MBSF == (anthro$MBSF[!(is.na(anthro$MBSF))]))])
    MBSF_DAY <- c()
    MBSF_DAY[a] <- anthro$MBSF[!(is.na(anthro$MBSF))]
    length(MBSF_DAY) <- z
  }

  #UC
  if ((sum(!is.na(anthro$UC)) > 1) == TRUE) {
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

    a <- which(!is.na(anthro$UC))[1]
    b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
    c <- which(DATE==b)
    UC_DAY <- c(rep(NA, c-1), UC_DAY)
    length(UC_DAY) <- z

  } else if (all(is.na(anthro$UC)) == TRUE) {
    UC_DAY <- NA
    length(UC_DAY) <- z
  } else if (sum(!is.na(anthro$UC)) == 1) {
    a <- which(DATE == anthro$DATE[which(anthro$UC == (anthro$UC[!(is.na(anthro$UC))]))])
    UC_DAY <- c()
    UC_DAY[a] <- anthro$UC[!(is.na(anthro$UC))]
    length(UC_DAY) <- z
  }


  #R
  if ((sum(!is.na(anthro$R)) > 1) == TRUE) {
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

    a <- which(!is.na(anthro$R))[1]
    b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
    c <- which(DATE==b)
    R_DAY <- c(rep(NA, c-1), R_DAY)
    length(R_DAY) <- z

  } else if (all(is.na(anthro$R)) == TRUE) {
    R_DAY <- NA
    length(R_DAY) <- z
  } else if (sum(!is.na(anthro$R)) == 1) {
    a <- which(DATE == anthro$DATE[which(anthro$R == (anthro$R[!(is.na(anthro$R))]))])
    R_DAY <- c()
    R_DAY[a] <- anthro$R[!(is.na(anthro$R))]
    length(R_DAY) <- z
  }

  #X
  if ((sum(!is.na(anthro$X)) > 1) == TRUE) {
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

    a <- which(!is.na(anthro$X))[1]
    b <- as.Date(anthro$DATE[a], format="%m/%d/%Y")
    c <- which(DATE==b)
    X_DAY <- c(rep(NA, c-1), X_DAY)
    length(X_DAY) <- z

  } else if (all(is.na(anthro$X)) == TRUE) {
    X_DAY <- NA
    length(X_DAY) <- z
  } else if (sum(!is.na(anthro$X)) == 1) {
    a <- which(DATE == anthro$DATE[which(anthro$X == (anthro$X[!(is.na(anthro$X))]))])
    X_DAY <- c()
    X_DAY[a] <- anthro$X[!(is.na(anthro$X))]
    length(X_DAY) <- z
  }

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
  AMA_DAY <- ((UAC_DAY-(pi*TSF_DAY/10))^2)/(4*pi)

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

  #NHANES Interpolated



  MEAN_NHANES_HT <- as.numeric(sapply(AGE_DAY, function(x) subset(NHANES.References,
                                                                  NHANES.References$AGE_YEAR_NHANES_HT_AGE==x &
                                                                    NHANES.References$SEX_NHANES_HT_AGE==SEX &
                                                                    NHANES.References$RACE_NHANES_HT_AGE==RACE,
                                                                  select=c("MEAN_NHANES_HT_AGE"))))

  SD_NHANES_HT <- as.numeric(sapply(AGE_DAY, function(x) subset(NHANES.References,
                                                                NHANES.References$AGE_YEAR_NHANES_HT_AGE==x &
                                                                  NHANES.References$SEX_NHANES_HT_AGE==SEX &
                                                                  NHANES.References$RACE_NHANES_HT_AGE==RACE,
                                                                select=c("SD_NHANES_HT_AGE"))))

  MEAN_NHANES_WT <- as.numeric(sapply(AGE_DAY, function(x) subset(NHANES.References,
                                                                  NHANES.References$AGE_YEAR_NHANES_WT_AGE==x &
                                                                    NHANES.References$SEX_NHANES_WT_AGE==SEX &
                                                                    NHANES.References$RACE_NHANES_WT_AGE==RACE,
                                                                  select=c("MEAN_NHANES_WT_AGE"))))

  SD_NHANES_WT <- as.numeric(sapply(AGE_DAY, function(x) subset(NHANES.References,
                                                                NHANES.References$AGE_YEAR_NHANES_WT_AGE==x &
                                                                  NHANES.References$SEX_NHANES_WT_AGE==SEX &
                                                                  NHANES.References$RACE_NHANES_WT_AGE==RACE,
                                                                select=c("SD_NHANES_WT_AGE"))))

  MEAN_NHANES_BMI <- as.numeric(sapply(AGE_DAY, function(x) subset(NHANES.References,
                                                                   NHANES.References$AGE_YEAR_NHANES_BMI_AGE==x &
                                                                     NHANES.References$SEX_NHANES_BMI_AGE==SEX &
                                                                     NHANES.References$RACE_NHANES_BMI_AGE==RACE,
                                                                   select=c("MEAN_NHANES_BMI_AGE"))))

  SD_NHANES_BMI <- as.numeric(sapply(AGE_DAY, function(x) subset(NHANES.References,
                                                                 NHANES.References$AGE_YEAR_NHANES_BMI_AGE==x &
                                                                   NHANES.References$SEX_NHANES_BMI_AGE==SEX &
                                                                   NHANES.References$RACE_NHANES_BMI_AGE==RACE,
                                                                 select=c("SD_NHANES_BMI_AGE"))))

  ####################
  #NHANES UAC
  MEAN_NHANES_UAC <- rep(NA,length(AGE_DAY))
  SD_NHANES_UAC <- rep(NA,length(AGE_DAY))
  AGE_UAC <- ifelse(AGE_MO <= 240, AGE_MO, AGE_DAY)
  a <- which((AGE_MO <= 240) == FALSE)
  MEAN_NHANES_UAC[a] <- as.numeric(sapply(AGE_UAC[a], function(x) subset(NHANES.References,
                                                                         NHANES.References$AGE_YEAR_NHANES_UAC_AGE==x &
                                                                           NHANES.References$SEX_NHANES_UAC_AGE==SEX &
                                                                           NHANES.References$RACE_NHANES_UAC_AGE==RACE,
                                                                         select=c("MEAN_NHANES_UAC_AGE"))))

  SD_NHANES_UAC[a] <- as.numeric(sapply(AGE_UAC[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_YEAR_NHANES_UAC_AGE==x &
                                                                         NHANES.References$SEX_NHANES_UAC_AGE==SEX &
                                                                         NHANES.References$RACE_NHANES_UAC_AGE==RACE,
                                                                       select=c("SD_NHANES_UAC_AGE"))))

  a <- which((AGE_MO <= 240) == TRUE)
  L_NHANES_UAC <- rep(NA,length(AGE_DAY))
  M_NHANES_UAC <- rep(NA,length(AGE_DAY))
  S_NHANES_UAC <- rep(NA,length(AGE_DAY))
  L_NHANES_UAC[a] <- as.numeric(sapply(AGE_UAC[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_UAC_AGE==x &
                                                                        NHANES.References$SEX_NHANES_UAC_AGE2==SEX,
                                                                      select=c("L_NHANES_UAC"))))

  M_NHANES_UAC[a] <- as.numeric(sapply(AGE_UAC[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_UAC_AGE==x &
                                                                        NHANES.References$SEX_NHANES_UAC_AGE2==SEX,
                                                                      select=c("M_NHANES_UAC"))))

  S_NHANES_UAC[a] <-  as.numeric(sapply(AGE_UAC[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_MONTHS_NHANES_UAC_AGE==x &
                                                                         NHANES.References$SEX_NHANES_UAC_AGE2==SEX,
                                                                       select=c("S_NHANES_UAC"))))
  #######################
  #NHANES TSF
  MEAN_NHANES_TSF <- rep(NA,length(AGE_DAY))
  SD_NHANES_TSF <- rep(NA,length(AGE_DAY))
  AGE_TSF <- ifelse(AGE_MO <= 239, AGE_MO, AGE_DAY)
  a <- which((AGE_MO <= 239) == FALSE)
  MEAN_NHANES_TSF[a] <- as.numeric(sapply(AGE_TSF[a], function(x) subset(NHANES.References,
                                                                         NHANES.References$AGE_YEAR_NHANES_TSF_AGE==x &
                                                                           NHANES.References$SEX_NHANES_TSF_AGE==SEX &
                                                                           NHANES.References$RACE_NHANES_TSF_AGE==RACE,
                                                                         select=c("MEAN_NHANES_TSF_AGE"))))

  SD_NHANES_TSF[a] <- as.numeric(sapply(AGE_TSF[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_YEAR_NHANES_TSF_AGE==x &
                                                                         NHANES.References$SEX_NHANES_TSF_AGE==SEX &
                                                                         NHANES.References$RACE_NHANES_TSF_AGE==RACE,
                                                                       select=c("SD_NHANES_TSF_AGE"))))

  a <- which((AGE_MO <= 239) == TRUE)
  L_NHANES_TSF <- rep(NA,length(AGE_DAY))
  M_NHANES_TSF <- rep(NA,length(AGE_DAY))
  S_NHANES_TSF <- rep(NA,length(AGE_DAY))
  L_NHANES_TSF[a] <- as.numeric(sapply(AGE_TSF[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_TSF_AGE2==x &
                                                                        NHANES.References$SEX_NHANES_TSF_AGE2==SEX,
                                                                      select=c("L_NHANES_TSF_AGE"))))

  M_NHANES_TSF[a] <- as.numeric(sapply(AGE_TSF[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_TSF_AGE2==x &
                                                                        NHANES.References$SEX_NHANES_TSF_AGE2==SEX,
                                                                      select=c("M_NHANES_TSF_AGE"))))

  S_NHANES_TSF[a] <-  as.numeric(sapply(AGE_TSF[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_MONTHS_NHANES_TSF_AGE2==x &
                                                                         NHANES.References$SEX_NHANES_TSF_AGE2==SEX,
                                                                       select=c("S_NHANES_TSF_AGE"))))

  ###########################

  MEAN_NHANES_UAA <- as.numeric(sapply(AGE_DAY, function(x) subset(NHANES.References,
                                                                   NHANES.References$AGE_YEAR_NHANES_UAA_AGE==x &
                                                                     NHANES.References$SEX_NHANES_UAA_AGE==SEX &
                                                                     NHANES.References$RACE_NHANES_UAA_AGE==RACE,
                                                                   select=c("MEAN_NHANES_UAA_AGE"))))

  SD_NHANES_UAA <- as.numeric(sapply(AGE_DAY, function(x) subset(NHANES.References,
                                                                 NHANES.References$AGE_YEAR_NHANES_UAA_AGE==x &
                                                                   NHANES.References$SEX_NHANES_UAA_AGE==SEX &
                                                                   NHANES.References$RACE_NHANES_UAA_AGE==RACE,
                                                                 select=c("SD_NHANES_UAA_AGE"))))
  #######################
  #NHANES AMA
  MEAN_NHANES_AMA <- rep(NA,length(AGE_DAY))
  SD_NHANES_AMA <- rep(NA,length(AGE_DAY))
  AGE_AMA <- ifelse(AGE_MO <= 240, AGE_MO, AGE_DAY)
  a <- which((AGE_MO <= 240) == FALSE)
  MEAN_NHANES_AMA[a] <- as.numeric(sapply(AGE_AMA[a], function(x) subset(NHANES.References,
                                                                         NHANES.References$AGE_YEAR_NHANES_AMA_AGE==x &
                                                                           NHANES.References$SEX_NHANES_AMA_AGE==SEX &
                                                                           NHANES.References$RACE_NHANES_AMA_AGE==RACE,
                                                                         select=c("MEAN_NHANES_AMA_AGE"))))

  SD_NHANES_AMA[a] <- as.numeric(sapply(AGE_AMA[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_YEAR_NHANES_AMA_AGE==x &
                                                                         NHANES.References$SEX_NHANES_AMA_AGE==SEX &
                                                                         NHANES.References$RACE_NHANES_AMA_AGE==RACE,
                                                                       select=c("SD_NHANES_AMA_AGE"))))

  a <- which((AGE_MO <= 240) == TRUE)
  L_NHANES_AMA <- rep(NA,length(AGE_DAY))
  M_NHANES_AMA <- rep(NA,length(AGE_DAY))
  S_NHANES_AMA <- rep(NA,length(AGE_DAY))
  L_NHANES_AMA[a] <- as.numeric(sapply(AGE_AMA[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_AMA_AGE==x &
                                                                        NHANES.References$SEX_NHANES_AMA_AGE2==SEX,
                                                                      select=c("L_NHANES_AMA"))))

  M_NHANES_AMA[a] <- as.numeric(sapply(AGE_AMA[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_AMA_AGE==x &
                                                                        NHANES.References$SEX_NHANES_AMA_AGE2==SEX,
                                                                      select=c("M_NHANES_AMA"))))

  S_NHANES_AMA[a] <-  as.numeric(sapply(AGE_AMA[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_MONTHS_NHANES_AMA_AGE==x &
                                                                         NHANES.References$SEX_NHANES_AMA_AGE2==SEX,
                                                                       select=c("S_NHANES_AMA"))))
  ####################
  #NHANES AFA
  MEAN_NHANES_AFA <- rep(NA,length(AGE_DAY))
  SD_NHANES_AFA <- rep(NA,length(AGE_DAY))
  AGE_AFA <- ifelse(AGE_MO <= 240, AGE_MO, AGE_DAY)
  a <- which((AGE_MO <= 240) == FALSE)
  MEAN_NHANES_AFA[a] <- as.numeric(sapply(AGE_AFA[a], function(x) subset(NHANES.References,
                                                                         NHANES.References$AGE_YEAR_NHANES_AFA_AGE==x &
                                                                           NHANES.References$SEX_NHANES_AFA_AGE==SEX &
                                                                           NHANES.References$RACE_NHANES_AFA_AGE==RACE,
                                                                         select=c("MEAN_NHANES_AFA_AGE"))))

  SD_NHANES_AFA[a] <- as.numeric(sapply(AGE_AFA[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_YEAR_NHANES_AFA_AGE==x &
                                                                         NHANES.References$SEX_NHANES_AFA_AGE==SEX &
                                                                         NHANES.References$RACE_NHANES_AFA_AGE==RACE,
                                                                       select=c("SD_NHANES_AFA_AGE"))))

  a <- which((AGE_MO <= 240) == TRUE)
  L_NHANES_AFA <- rep(NA,length(AGE_DAY))
  M_NHANES_AFA <- rep(NA,length(AGE_DAY))
  S_NHANES_AFA <- rep(NA,length(AGE_DAY))
  L_NHANES_AFA[a] <- as.numeric(sapply(AGE_AFA[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_AFA_AGE==x &
                                                                        NHANES.References$SEX_NHANES_AFA_AGE2==SEX,
                                                                      select=c("L_NHANES_AFA"))))

  M_NHANES_AFA[a] <- as.numeric(sapply(AGE_AFA[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_AFA_AGE==x &
                                                                        NHANES.References$SEX_NHANES_AFA_AGE2==SEX,
                                                                      select=c("M_NHANES_AFA"))))

  S_NHANES_AFA[a] <-  as.numeric(sapply(AGE_AFA[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_MONTHS_NHANES_AFA_AGE==x &
                                                                         NHANES.References$SEX_NHANES_AFA_AGE2==SEX,
                                                                       select=c("S_NHANES_AFA"))))

  ################

  #NHANES SSF
  MEAN_NHANES_SSF <- rep(NA,length(AGE_DAY))
  SD_NHANES_SSF <- rep(NA,length(AGE_DAY))
  AGE_SSF <- ifelse(AGE_MO <= 239, AGE_MO, AGE_DAY)
  a <- which((AGE_MO <= 239) == FALSE)
  MEAN_NHANES_SSF[a] <- as.numeric(sapply(AGE_SSF[a], function(x) subset(NHANES.References,
                                                                         NHANES.References$AGE_YEAR_NHANES_SSSF_AGE==x &
                                                                           NHANES.References$SEX_NHANES_SSSF_AGE==SEX &
                                                                           NHANES.References$RACE_NHANES_SSSF_AGE==RACE,
                                                                         select=c("MEAN_NHANES_SSSF_AGE"))))

  SD_NHANES_SSF[a] <- as.numeric(sapply(AGE_SSF[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_YEAR_NHANES_SSSF_AGE==x &
                                                                         NHANES.References$SEX_NHANES_SSSF_AGE==SEX &
                                                                         NHANES.References$RACE_NHANES_SSSF_AGE==RACE,
                                                                       select=c("SD_NHANES_SSSF_AGE"))))

  a <- which((AGE_MO <= 239) == TRUE)
  L_NHANES_SSF <- rep(NA,length(AGE_DAY))
  M_NHANES_SSF <- rep(NA,length(AGE_DAY))
  S_NHANES_SSF <- rep(NA,length(AGE_DAY))
  L_NHANES_SSF[a] <- as.numeric(sapply(AGE_SSF[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_SSSF_AGE2==x &
                                                                        NHANES.References$SEX_NHANES_SSSF_AGE2==SEX,
                                                                      select=c("L_NHANES_SSSF_AGE"))))

  M_NHANES_SSF[a] <- as.numeric(sapply(AGE_SSF[a], function(x) subset(NHANES.References,
                                                                      NHANES.References$AGE_MONTHS_NHANES_SSSF_AGE2==x &
                                                                        NHANES.References$SEX_NHANES_SSSF_AGE2==SEX,
                                                                      select=c("M_NHANES_SSSF_AGE"))))

  S_NHANES_SSF[a] <-  as.numeric(sapply(AGE_SSF[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_MONTHS_NHANES_SSSF_AGE2==x &
                                                                         NHANES.References$SEX_NHANES_SSSF_AGE2==SEX,
                                                                       select=c("S_NHANES_SSSF_AGE"))))

  ############################

  MEAN_NHANES_WT_FOR_HT <- rep(NA,length(AGE_DAY))
  SD_NHANES_WT_FOR_HT <- rep(NA,length(AGE_DAY))

  num <- which((AGE_DAY < 12 & AGE_DAY >=2 & SEX==1) == TRUE)
  MEAN_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                              NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE &
                                                                                NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX &
                                                                                NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==x,
                                                                              select=c("MEAN_NHANES_WT_FOR_HT1"))))

  SD_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                            NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE &
                                                                              NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX &
                                                                              NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==x,
                                                                            select=c("SD_NHANES_WT_FOR_HT1"))))


  num <- which((AGE_DAY >= 12 & AGE_DAY < 18 & SEX==1) == TRUE)
  MEAN_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                              NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE &
                                                                                NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX &
                                                                                NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==x,
                                                                              select=c("MEAN_NHANES_WT_FOR_HT2"))))


  SD_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                            NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE &
                                                                              NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX &
                                                                              NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==x,
                                                                            select=c("SD_NHANES_WT_FOR_HT2"))))

  num <- which((AGE_DAY >= 18 & AGE_DAY < 75) == TRUE)
  MEAN_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                              NHANES.References$RACE_NHANES_WT_FOR_HT3==RACE &
                                                                                NHANES.References$SEX_NHANES_WT_FOR_HT3==SEX &
                                                                                NHANES.References$HEIGHT_NHANES_WT_FOR_HT3==x,
                                                                              select=c("MEAN_NHANES_WT_FOR_HT3"))))


  SD_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                            NHANES.References$RACE_NHANES_WT_FOR_HT3==RACE &
                                                                              NHANES.References$SEX_NHANES_WT_FOR_HT3==SEX &
                                                                              NHANES.References$HEIGHT_NHANES_WT_FOR_HT3==x,
                                                                            select=c("SD_NHANES_WT_FOR_HT3"))))


  num <- which((AGE_DAY < 11 & AGE_DAY >= 2 & SEX == 2)  == TRUE)
  MEAN_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                              NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE &
                                                                                NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX &
                                                                                NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==x,
                                                                              select=c("MEAN_NHANES_WT_FOR_HT1"))))

  SD_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                            NHANES.References$RACE_NHANES_WT_FOR_HT1==RACE &
                                                                              NHANES.References$SEX_NHANES_WT_FOR_HT1==SEX &
                                                                              NHANES.References$HEIGHT_NHANES_WT_FOR_HT1==x,
                                                                            select=c("SD_NHANES_WT_FOR_HT1"))))


  num <- which((AGE_DAY >=11 & AGE_DAY < 18 & SEX==2) == TRUE)
  MEAN_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                              NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE &
                                                                                NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX &
                                                                                NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==x,
                                                                              select=c("MEAN_NHANES_WT_FOR_HT2"))))


  SD_NHANES_WT_FOR_HT[num] <- as.numeric(sapply(HT[num], function(x) subset(NHANES.References,
                                                                            NHANES.References$RACE_NHANES_WT_FOR_HT2==RACE &
                                                                              NHANES.References$SEX_NHANES_WT_FOR_HT2==SEX &
                                                                              NHANES.References$HEIGHT_NHANES_WT_FOR_HT2==x,
                                                                            select=c("SD_NHANES_WT_FOR_HT2"))))


  #AND UC here

  RACE <- if (Demographics.Identified$RACE[1] == "White"){RACE <- 2} else if (Demographics.Identified$RACE[1] == "Asian"){RACE <- 2}  else if (Demographics.Identified$RACE[1] == "African-American"){RACE <- 1} else if (Demographics.Identified$RACE[1] == "Hispanic"){RACE <- 3}
  MEAN_NHANES_UC <- rep(NA,length(AGE_DAY))
  SD_NHANES_UC <- rep(NA,length(AGE_DAY))
  AGE_UC <- ifelse(AGE_DAY <= 4 | AGE_DAY >= 19, AGE_DAY, AGE_MO)
  a <- which((AGE_DAY <= 4 | AGE_DAY >= 19) == TRUE)
  MEAN_NHANES_UC[a] <- as.numeric(sapply(AGE_UC[a], function(x) subset(NHANES.References,
                                                                       NHANES.References$AGE_YEAR_NHANES_UC_AGE==x &
                                                                         NHANES.References$SEX_NHANES_UC_AGE==SEX &
                                                                         NHANES.References$RACE_NHANES_UC_AGE==RACE,
                                                                       select=c("MEAN_NHANES_UC_AGE"))))


  SD_NHANES_UC[a] <- as.numeric(sapply(AGE_UC[a], function(x) subset(NHANES.References,
                                                                     NHANES.References$AGE_YEAR_NHANES_UC_AGE==x &
                                                                       NHANES.References$SEX_NHANES_UC_AGE==SEX &
                                                                       NHANES.References$RACE_NHANES_UC_AGE==RACE,
                                                                     select=c("SD_NHANES_UC_AGE"))))

  L_NHANES_UC <- rep(NA,length(AGE_DAY))
  M_NHANES_UC <- rep(NA,length(AGE_DAY))
  S_NHANES_UC <- rep(NA,length(AGE_DAY))
  a <- which((AGE_DAY <= 4 | AGE_DAY >= 19) == FALSE)
  L_NHANES_UC[a] <- as.numeric(sapply(AGE_UC[a], function(x) subset(NHANES.References,
                                                                    NHANES.References$AGE_MO_NHANES_UC_AGE==x &
                                                                      NHANES.References$SEX_NHANES_UC_AGE2==SEX,
                                                                    select=c("L_NHANES_UC"))))

  M_NHANES_UC[a] <- as.numeric(sapply(AGE_UC[a], function(x) subset(NHANES.References,
                                                                    NHANES.References$AGE_MO_NHANES_UC_AGE==x &
                                                                      NHANES.References$SEX_NHANES_UC_AGE2==SEX,
                                                                    select=c("M_NHANES_UC"))))

  S_NHANES_UC[a] <-  as.numeric(sapply(AGE_UC[a], function(x) subset(NHANES.References,
                                                                     NHANES.References$AGE_MO_NHANES_UC_AGE==x &
                                                                       NHANES.References$SEX_NHANES_UC_AGE2==SEX,
                                                                     select=c("S_NHANES_UC"))))
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
  NHANES_SSF_Z_DAY1 <- (((SSF_DAY/M_NHANES_SSF)^L_NHANES_SSF)-1)/(L_NHANES_SSF*S_NHANES_SSF)
  NHANES_SSF_Z_DAY2 <- (SSF_DAY-MEAN_NHANES_SSF)/SD_NHANES_SSF
  NHANES_SSF_Z_DAY1[is.na(NHANES_SSF_Z_DAY1)] <- " "
  NHANES_SSF_Z_DAY2[is.na(NHANES_SSF_Z_DAY2)] <- " "
  NHANES_SSF_Z_DAY <- as.numeric(paste(NHANES_SSF_Z_DAY1, NHANES_SSF_Z_DAY2))

  #NHANES_SUBSCAPULAR_SKINFOLD_PERCENTILE
  NHANES_SSF_PCTL_DAY <- (pnorm(NHANES_SSF_Z_DAY))*100

  #NHANES_TRICEPS_SKINFOLD_Z_SCORE
  NHANES_TSF_Z_DAY1 <- (((TSF_DAY/M_NHANES_TSF)^L_NHANES_TSF)-1)/(L_NHANES_TSF*S_NHANES_TSF)
  NHANES_TSF_Z_DAY2 <- (TSF_DAY-MEAN_NHANES_TSF)/SD_NHANES_TSF
  NHANES_TSF_Z_DAY1[is.na(NHANES_TSF_Z_DAY1)] <- " "
  NHANES_TSF_Z_DAY2[is.na(NHANES_TSF_Z_DAY2)] <- " "
  NHANES_TSF_Z_DAY <- as.numeric(paste(NHANES_TSF_Z_DAY1, NHANES_TSF_Z_DAY2))

  #NHANES_TRICEPS_SKINFOLD_PERCENTILE
  NHANES_TSF_PCTL_DAY <- (pnorm(NHANES_TSF_Z_DAY))*100

  #NHANES_UPPER_ARM_AREA_Z_SCORE
  NHANES_UAA_Z_DAY <- (UAA_DAY-MEAN_NHANES_UAA)/SD_NHANES_UAA

  #NHANES_UPPER_ARM_AREA_PERCENTILE
  NHANES_UAA_PCTL_DAY <- (pnorm(NHANES_UAA_Z_DAY))*100

  #NHANES_UPPER_ARM_CIRCUMFERANCE_Z_SCORE
  NHANES_UAC_Z_DAY1 <- (((UAC_DAY/M_NHANES_UAC)^L_NHANES_UAC)-1)/(L_NHANES_UAC*S_NHANES_UAC)
  NHANES_UAC_Z_DAY2 <- (UAC_DAY-MEAN_NHANES_UAC)/SD_NHANES_UAC
  NHANES_UAC_Z_DAY1[is.na(NHANES_UAC_Z_DAY1)] <- " "
  NHANES_UAC_Z_DAY2[is.na(NHANES_UAC_Z_DAY2)] <- " "
  NHANES_UAC_Z_DAY <- as.numeric(paste(NHANES_UAC_Z_DAY1, NHANES_UAC_Z_DAY2))

  #NHANES_UPPER_ARM_CIRCUMFERANCE__PERCENTILE
  NHANES_UAC_PCTL_DAY <- (pnorm(NHANES_UAC_Z_DAY))*100

  #NHANES_UPPER_ARM_FAT_AREA_Z_SCORE
  NHANES_AFA_Z_DAY1 <- (((AFA_DAY/M_NHANES_AFA)^L_NHANES_AFA)-1)/(L_NHANES_AFA*S_NHANES_AFA)
  NHANES_AFA_Z_DAY2 <- (AFA_DAY-MEAN_NHANES_AFA)/SD_NHANES_AFA
  NHANES_AFA_Z_DAY1[is.na(NHANES_AFA_Z_DAY1)] <- " "
  NHANES_AFA_Z_DAY2[is.na(NHANES_AFA_Z_DAY2)] <- " "
  NHANES_AFA_Z_DAY <- as.numeric(paste(NHANES_AFA_Z_DAY1, NHANES_AFA_Z_DAY2))

  #NHANES_UPPER_ARM_FAT_AREA_PERCENTILE
  NHANES_AFA_PCTL_DAY <- (pnorm(NHANES_AFA_Z_DAY))*100

  #NHANES_UPPER_ARM_MUSCLE_AREA_Z_SCORE
  NHANES_AMA_Z_DAY1 <- (((AMA_DAY/M_NHANES_AMA)^L_NHANES_AMA)-1)/(L_NHANES_AMA*S_NHANES_AMA)
  NHANES_AMA_Z_DAY2 <- (AMA_DAY-MEAN_NHANES_AMA)/SD_NHANES_AMA
  NHANES_AMA_Z_DAY1[is.na(NHANES_AMA_Z_DAY1)] <- " "
  NHANES_AMA_Z_DAY2[is.na(NHANES_AMA_Z_DAY2)] <- " "
  NHANES_AMA_Z_DAY <- as.numeric(paste(NHANES_AMA_Z_DAY1, NHANES_AMA_Z_DAY2))

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


  #CDC using apply function

  L_CDC_WT <- as.numeric(sapply(AGE_DAY, function(x) subset(CDC.References,
                                                            CDC.References$AGE_MO_CDC_WT_AGE==x &
                                                              CDC.References$SEX_CDC_WT_AGE==SEX,
                                                            select=c("L_CDC_WT_AGE"))))

  M_CDC_WT <- as.numeric(sapply(AGE_DAY, function(x) subset(CDC.References,
                                                            CDC.References$AGE_MO_CDC_WT_AGE==x &
                                                              CDC.References$SEX_CDC_WT_AGE==SEX,
                                                            select=c("M_CDC_WT_AGE"))))

  S_CDC_WT <- as.numeric(sapply(AGE_DAY, function(x) subset(CDC.References,
                                                            CDC.References$AGE_MO_CDC_WT_AGE==x &
                                                              CDC.References$SEX_CDC_WT_AGE==SEX,
                                                            select=c("S_CDC_WT_AGE"))))

  L_CDC_HT <- as.numeric(sapply(AGE_DAY, function(x) subset(CDC.References,
                                                            CDC.References$AGE_MO_CDC_HT_AGE==x &
                                                              CDC.References$SEX_CDC_HT_AGE==SEX,
                                                            select=c("L_CDC_HT_AGE"))))

  M_CDC_HT <- as.numeric(sapply(AGE_DAY, function(x) subset(CDC.References,
                                                            CDC.References$AGE_MO_CDC_HT_AGE==x &
                                                              CDC.References$SEX_CDC_HT_AGE==SEX,
                                                            select=c("M_CDC_HT_AGE"))))

  S_CDC_HT <- as.numeric(sapply(AGE_DAY, function(x) subset(CDC.References,
                                                            CDC.References$AGE_MO_CDC_HT_AGE==x &
                                                              CDC.References$SEX_CDC_HT_AGE==SEX,
                                                            select=c("S_CDC_HT_AGE"))))

  L_CDC_BMI <- as.numeric(sapply(AGE_DAY, function(x) subset(CDC.References,
                                                             CDC.References$AGE_MO_CDC_BMI_AGE==x &
                                                               CDC.References$SEX_CDC_BMI_AGE==SEX,
                                                             select=c("L_CDC_BMI_AGE"))))

  M_CDC_BMI <- as.numeric(sapply(AGE_DAY, function(x) subset(CDC.References,
                                                             CDC.References$AGE_MO_CDC_BMI_AGE==x &
                                                               CDC.References$SEX_CDC_BMI_AGE==SEX,
                                                             select=c("M_CDC_BMI_AGE"))))

  S_CDC_BMI <- as.numeric(sapply(AGE_DAY, function(x) subset(CDC.References,
                                                             CDC.References$AGE_MO_CDC_BMI_AGE==x &
                                                               CDC.References$SEX_CDC_BMI_AGE==SEX,
                                                             select=c("S_CDC_BMI_AGE"))))

  L_CDC_WT_HT <- as.numeric(sapply(HT, function(x) subset(CDC.References,
                                                          CDC.References$HEIGHT_CDC_WT_FOR_HT==x &
                                                            CDC.References$SEX_CDC_WT_FOR_HT ==SEX,
                                                          select=c("L_CDC_WT_FOR_HT"))))

  M_CDC_WT_HT <- as.numeric(sapply(HT, function(x) subset(CDC.References,
                                                          CDC.References$HEIGHT_CDC_WT_FOR_HT==x &
                                                            CDC.References$SEX_CDC_WT_FOR_HT ==SEX,
                                                          select=c("M_CDC_WT_FOR_HT"))))

  S_CDC_WT_HT <- as.numeric(sapply(HT, function(x) subset(CDC.References,
                                                          CDC.References$HEIGHT_CDC_WT_FOR_HT==x &
                                                            CDC.References$SEX_CDC_WT_FOR_HT ==SEX,
                                                          select=c("S_CDC_WT_FOR_HT"))))

  L_CDC_HC <- as.numeric(sapply(AGE_DAY, function(x) subset(CDC.References,
                                                            CDC.References$AGE_MO_CDC_HC_AGE==x &
                                                              CDC.References$SEX_CDC_HC_AGE==SEX,
                                                            select=c("L_CDC_HC_AGE"))))

  M_CDC_HC <- as.numeric(sapply(AGE_DAY, function(x) subset(CDC.References,
                                                            CDC.References$AGE_MO_CDC_HC_AGE==x &
                                                              CDC.References$SEX_CDC_HC_AGE==SEX,
                                                            select=c("M_CDC_HC_AGE"))))

  S_CDC_HC <- as.numeric(sapply(AGE_DAY, function(x) subset(CDC.References,
                                                            CDC.References$AGE_MO_CDC_HC_AGE==x &
                                                              CDC.References$SEX_CDC_HC_AGE==SEX,
                                                            select=c("S_CDC_HC_AGE"))))
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

  #WHO


  L_WHO_HC <- as.numeric(sapply(AGE_DOL2, function(x) subset(WHO.References,
                                                             WHO.References$AGE_DAY_WHO_HC_AGE==x &
                                                               WHO.References$SEX_WHO_HC_AGE==SEX,
                                                             select = c("L_WHO_HC_AGE"))))

  M_WHO_HC <- as.numeric(sapply(AGE_DOL2, function (x) subset(WHO.References,
                                                              WHO.References$AGE_DAY_WHO_HC_AGE==x &
                                                                WHO.References$SEX_WHO_HC_AGE==SEX,
                                                              select = c("M_WHO_HC_AGE"))))

  S_WHO_HC <- as.numeric(sapply(AGE_DOL2, function(x) subset(WHO.References,
                                                             WHO.References$AGE_DAY_WHO_HC_AGE==x &
                                                               WHO.References$SEX_WHO_HC_AGE==SEX,
                                                             select = c("S_WHO_HC_AGE"))))

  L_WHO_SSF <- as.numeric(sapply(AGE_DOL2, function(x) subset(WHO.References,
                                                              WHO.References$AGE_DAY_WHO_SSF_AGE==x &
                                                                WHO.References$SEX_WHO_SSF_AGE==SEX,
                                                              select=c("L_WHO_SSF_AGE"))))

  M_WHO_SSF <- as.numeric(sapply(AGE_DOL2, function(x) subset(WHO.References,
                                                              WHO.References$AGE_DAY_WHO_SSF_AGE==x &
                                                                WHO.References$SEX_WHO_SSF_AGE==SEX,
                                                              select=c("M_WHO_SSF_AGE"))))


  S_WHO_SSF <- as.numeric(sapply(AGE_DOL2, function(x) subset(WHO.References,
                                                              WHO.References$AGE_DAY_WHO_SSF_AGE==x &
                                                                WHO.References$SEX_WHO_SSF_AGE==SEX,
                                                              select=c("S_WHO_SSF_AGE"))))

  L_WHO_TSF <- as.numeric(sapply(AGE_DOL2, function(x) subset(WHO.References,
                                                              WHO.References$AGE_DAY_WHO_TSF_AGE==x &
                                                                WHO.References$SEX_WHO_TSF_AGE==SEX,
                                                              select=c("L_WHO_TSF_AGE"))))


  M_WHO_TSF <- as.numeric(sapply(AGE_DOL2, function(x) subset(WHO.References,
                                                              WHO.References$AGE_DAY_WHO_TSF_AGE==x &
                                                                WHO.References$SEX_WHO_TSF_AGE==SEX,
                                                              select=c("M_WHO_TSF_AGE"))))

  S_WHO_TSF <- as.numeric(sapply(AGE_DOL2, function(x) subset(WHO.References,
                                                              WHO.References$AGE_DAY_WHO_TSF_AGE==x &
                                                                WHO.References$SEX_WHO_TSF_AGE==SEX,
                                                              select=c("S_WHO_TSF_AGE"))))

  L_WHO_UAC <- as.numeric(sapply(AGE_DOL2, function(x) subset(WHO.References,
                                                              WHO.References$AGE_DAY_WHO_UAC_AGE==x &
                                                                WHO.References$SEX_WHO_UAC_AGE==SEX,
                                                              select=c("L_WHO_UAC_AGE"))))

  M_WHO_UAC <- as.numeric(sapply(AGE_DOL2, function(x) subset(WHO.References,
                                                              WHO.References$AGE_DAY_WHO_UAC_AGE==x &
                                                                WHO.References$SEX_WHO_UAC_AGE==SEX,
                                                              select=c("M_WHO_UAC_AGE"))))

  S_WHO_UAC <- as.numeric(sapply(AGE_DOL2, function(x) subset(WHO.References,
                                                              WHO.References$AGE_DAY_WHO_UAC_AGE==x &
                                                                WHO.References$SEX_WHO_UAC_AGE==SEX,
                                                              select=c("S_WHO_UAC_AGE"))))




  L_WHO_WT_HT <- as.numeric(sapply(HT, function(x) subset(WHO.References,
                                                          WHO.References$HEIGHT_WHO_WT_FOR_HT==x &
                                                            WHO.References$SEX_WHO_WT_FOR_HT==SEX,
                                                          select=c("L_WHO_WT_FOR_HT"))))

  M_WHO_WT_HT <- as.numeric(sapply(HT, function(x) subset(WHO.References,
                                                          WHO.References$HEIGHT_WHO_WT_FOR_HT==x &
                                                            WHO.References$SEX_WHO_WT_FOR_HT==SEX,
                                                          select=c("M_WHO_WT_FOR_HT"))))


  S_WHO_WT_HT <- as.numeric(sapply(HT, function(x) subset(WHO.References,
                                                          WHO.References$HEIGHT_WHO_WT_FOR_HT==x &
                                                            WHO.References$SEX_WHO_WT_FOR_HT==SEX,
                                                          select=c("S_WHO_WT_FOR_HT"))))

  L_WHO_HT <- rep(NA, length(AGE_DAY))
  M_WHO_HT <- rep(NA, length(AGE_DAY))
  S_WHO_HT <- rep(NA, length(AGE_DAY))
  L_WHO_WT <- rep(NA, length(AGE_DAY))
  M_WHO_WT <- rep(NA, length(AGE_DAY))
  S_WHO_WT <- rep(NA, length(AGE_DAY))
  L_WHO_BMI <- rep(NA, length(AGE_DAY))
  M_WHO_BMI <- rep(NA, length(AGE_DAY))
  S_WHO_BMI <- rep(NA, length(AGE_DAY))

  AGE_WHO <- ifelse(AGE_DOL2 <= 1856 , AGE_DOL2, AGE_DAY)
  t <- which((AGE_DOL2 <= 1856) == TRUE)
  L_WHO_HT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_DAY_WHO_HT_AGE==x &
                                                                    WHO.References$SEX_WHO_HT_AGE2==SEX,
                                                                  select=c("L_WHO_HT_AGE2"))))
  M_WHO_HT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_DAY_WHO_HT_AGE==x &
                                                                    WHO.References$SEX_WHO_HT_AGE2==SEX,
                                                                  select=c("M_WHO_HT_AGE2"))))
  S_WHO_HT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_DAY_WHO_HT_AGE==x &
                                                                    WHO.References$SEX_WHO_HT_AGE2==SEX,
                                                                  select=c("S_WHO_HT_AGE2"))))

  L_WHO_WT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_DAY_WHO_WT_AGE==x &
                                                                    WHO.References$SEX_WHO_WT_AGE2==SEX,
                                                                  select=c("L_WHO_WT_AGE2"))))
  M_WHO_WT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_DAY_WHO_WT_AGE==x &
                                                                    WHO.References$SEX_WHO_WT_AGE2==SEX,
                                                                  select=c("M_WHO_WT_AGE2"))))
  S_WHO_WT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_DAY_WHO_WT_AGE==x &
                                                                    WHO.References$SEX_WHO_WT_AGE2==SEX,
                                                                  select=c("S_WHO_WT_AGE2"))))

  L_WHO_BMI[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                   WHO.References$AGE_DAY_WHO_BMI_AGE==x &
                                                                     WHO.References$SEX_WHO_BMI_AGE2==SEX,
                                                                   select=c("L_WHO_BMI_AGE2"))))
  M_WHO_BMI[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                   WHO.References$AGE_DAY_WHO_BMI_AGE==x &
                                                                     WHO.References$SEX_WHO_BMI_AGE2==SEX,
                                                                   select=c("M_WHO_BMI_AGE2"))))
  S_WHO_BMI[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                   WHO.References$AGE_DAY_WHO_BMI_AGE==x &
                                                                     WHO.References$SEX_WHO_BMI_AGE2==SEX,
                                                                   select=c("S_WHO_BMI_AGE2"))))


  t <- which((AGE_DOL2 <= 1856)==FALSE)
  L_WHO_HT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_MO_WHO_HT_AGE==x &
                                                                    WHO.References$SEX_WHO_HT_AGE==SEX,
                                                                  select=c("L_WHO_HT_AGE"))))
  M_WHO_HT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_MO_WHO_HT_AGE==x &
                                                                    WHO.References$SEX_WHO_HT_AGE==SEX,
                                                                  select=c("M_WHO_HT_AGE"))))
  S_WHO_HT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_MO_WHO_HT_AGE==x &
                                                                    WHO.References$SEX_WHO_HT_AGE==SEX,
                                                                  select=c("S_WHO_HT_AGE"))))

  L_WHO_WT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_MO_WHO_WT_AGE==x &
                                                                    WHO.References$SEX_WHO_WT_AGE==SEX,
                                                                  select=c("L_WHO_WT_AGE"))))
  M_WHO_WT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_MO_WHO_WT_AGE==x &
                                                                    WHO.References$SEX_WHO_WT_AGE==SEX,
                                                                  select=c("M_WHO_WT_AGE"))))
  S_WHO_WT[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                  WHO.References$AGE_MO_WHO_WT_AGE==x &
                                                                    WHO.References$SEX_WHO_WT_AGE==SEX,
                                                                  select=c("S_WHO_WT_AGE"))))


  L_WHO_BMI[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                   WHO.References$AGE_MO_WHO_BMI_AGE==x &
                                                                     WHO.References$SEX_WHO_BMI_AGE==SEX,
                                                                   select=c("L_WHO_BMI_AGE"))))
  M_WHO_BMI[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                   WHO.References$AGE_MO_WHO_BMI_AGE==x &
                                                                     WHO.References$SEX_WHO_BMI_AGE==SEX,
                                                                   select=c("M_WHO_BMI_AGE"))))
  S_WHO_BMI[t] <- as.numeric(sapply(AGE_WHO[t], function(x) subset(WHO.References,
                                                                   WHO.References$AGE_MO_WHO_BMI_AGE==x &
                                                                     WHO.References$SEX_WHO_BMI_AGE==SEX,
                                                                   select=c("S_WHO_BMI_AGE"))))


  #WHO_LMS <- cbind(AGE_DAY, L_WHO_HT, M_WHO_HT, S_WHO_HT, L_WHO_WT, M_WHO_WT, S_WHO_WT, L_WHO_BMI, M_WHO_BMI, S_WHO_BMI, L_WHO_HC, M_WHO_HC, S_WHO_HC, L_WHO_UAC, M_WHO_UAC, S_WHO_UAC, L_WHO_TSF, M_WHO_TSF, S_WHO_TSF, L_WHO_SSF, M_WHO_SSF, S_WHO_SSF, L_WHO_WT_HT, M_WHO_WT_HT, S_WHO_WT_HT)


  #WHO_WEIGHT_Z_SCORE
  WHO_WT_Z_DAY <- (((WT_DAY/M_WHO_WT)^L_WHO_WT)-1)/(L_WHO_WT*S_WHO_WT)

  #WHO_WEIGHT_PERCENTILE
  WHO_WT_PCTL_DAY <- (pnorm(WHO_WT_Z_DAY))*100

  #WHO_BMI_Z_SCORE
  WHO_BMI_Z_DAY <- (((BMI_DAY/M_WHO_BMI)^L_WHO_BMI)-1)/(L_WHO_BMI*S_WHO_BMI)

  #WHO_BMI_PERCENTILE
  WHO_BMI_PCTL_DAY <- (pnorm(WHO_BMI_Z_DAY))*100

  #WHO_HEAD_CIRCUMFERENCE_Z_SCORE
  WHO_HC_Z_DAY <- (((HC_DAY/M_WHO_HC)^L_WHO_HC)-1)/(L_WHO_HC*S_WHO_HC)

  #WHO_HEAD_CIRCUMFERENCE_PERCENTILE
  WHO_HC_PCTL_DAY <- (pnorm(WHO_HC_Z_DAY))*100

  #WHO_HEIGHT_Z_SCORE
  WHO_HT_Z_DAY <- (((HT_DAY/M_WHO_HT)^L_WHO_HT)-1)/(L_WHO_HT*S_WHO_HT)


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
  output2 <- cbind.data.frame(DATE1, AGE_DAY1,AGE_DOL1, AGE_MO, RACE, SEX, HT1, L_NHANES_SSF, M_NHANES_SSF, S_NHANES_SSF, MEAN_NHANES_SSF, SD_NHANES_SSF, L_NHANES_AFA, M_NHANES_AFA, S_NHANES_AFA, MEAN_NHANES_AFA, SD_NHANES_AFA, L_NHANES_AMA, M_NHANES_AMA, S_NHANES_AMA, MEAN_NHANES_AMA, SD_NHANES_AMA, MEAN_NHANES_BMI, SD_NHANES_BMI, MEAN_NHANES_HT, SD_NHANES_HT, L_NHANES_TSF, M_NHANES_TSF, S_NHANES_TSF, MEAN_NHANES_TSF, SD_NHANES_TSF, L_NHANES_UAC, M_NHANES_UAC, S_NHANES_UAC, MEAN_NHANES_UAC, SD_NHANES_UAC, MEAN_NHANES_UAA, SD_NHANES_UAA, MEAN_NHANES_UC, SD_NHANES_UC, L_NHANES_UC, M_NHANES_UC, S_NHANES_UC, MEAN_NHANES_WT, SD_NHANES_WT, MEAN_NHANES_WT_FOR_HT, SD_NHANES_WT_FOR_HT,
                              L_CDC_HT, M_CDC_HT, S_CDC_HT, L_CDC_WT, M_CDC_WT, S_CDC_WT, L_CDC_BMI, M_CDC_BMI, S_CDC_BMI, L_CDC_WT_HT, M_CDC_WT_HT, S_CDC_WT_HT, L_CDC_HC, M_CDC_HC, S_CDC_HC,
                              L_WHO_HT, M_WHO_HT, S_WHO_HT, L_WHO_WT, M_WHO_WT, S_WHO_WT,L_WHO_BMI, M_WHO_BMI, S_WHO_BMI,L_WHO_HC, M_WHO_HC, S_WHO_HC, L_WHO_UAC, M_WHO_UAC, S_WHO_UAC, L_WHO_TSF, M_WHO_TSF, S_WHO_TSF, L_WHO_SSF, M_WHO_SSF, S_WHO_SSF, L_WHO_WT_HT, M_WHO_WT_HT, S_WHO_WT_HT
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
  assign('finaltable',finaltable,envir=.GlobalEnv)


  day1 <- as.Date(Demographics.Identified$PKT_INITIATED_DATE)
  lastday <- as.Date(Demographics.Identified$PKT_STOPPED_DATE)

  if (!(is.na(day1))) {
  #before first day
  sub1 <- finaltable$DATE < day1
  a <- finaltable$DAY_TYPE[sub1]
  finaltable$DAY_TYPE[sub1] <- ifelse(is.na(a), 3, a)
  assign('finaltable',finaltable,envir=.GlobalEnv)
  a <- ifelse(is.na(a), 3, a)
  finaltable$DAY_TYPE[sub1] <- ifelse(a!=1 & a!=4, 3, a)
  assign('finaltable',finaltable,envir=.GlobalEnv)


  #next
  sub2 <- finaltable$DATE >= day1
  b <- finaltable$DAY_TYPE[sub2]
  finaltable$DAY_TYPE[sub2] <- ifelse(is.na(b), 2, b)
  assign('finaltable',finaltable,envir=.GlobalEnv)
  b <- ifelse(is.na(b), 2, b)
  finaltable$DAY_TYPE[sub2] <- ifelse(b!=1 & b!=4, 2, b)
  assign('finaltable',finaltable,envir=.GlobalEnv)

  }

  if (is.na(lastday)) {
    c <- 'NA'
  } else if (!(is.na(lastday))) {
    sub3 <- finaltable$DATE > lastday
    c <- finaltable$DAY_TYPE[sub3]
    finaltable$DAY_TYPE[sub3] <- 3
    assign('finaltable',finaltable,envir=.GlobalEnv)
  }



  #for CP and PA
  #CP
  finaltable$CP_DAY <- c(NA, finaltable$CP_DAY[!is.na(finaltable$CP_DAY)])[cumsum(!is.na(finaltable$CP_DAY)) + 1]
  assign('finaltable',finaltable,envir=.GlobalEnv)


  #PA
  Bday <- as.Date(Demographics.Identified$DOB, format= "%m/%d/%Y") #First convert classes from factor to date
  span <- interval(Bday, finaltable$DATE)
  AGE_DAY <- as.period(span)
  AGE_DAY <- as.numeric(year(AGE_DAY))
  #to remove unwanted variables
  rm(Bday, span)

  if (all(is.na(finaltable$PA_DAY)) == FALSE) {
    sub <- AGE_DAY < 3
    finaltable$PA_DAY[sub] <- NA
    assign('finaltable',finaltable,envir=.GlobalEnv)
  }

  if (all(is.na(finaltable$PA_DAY)) == FALSE) {
    a <- which(!is.na(finaltable$PA_DAY))[1]
    b <- finaltable$PA_DAY[a]
    sub2 <- AGE_DAY > 2
    sub2 <- sub2[1:a-1]
    finaltable$PA_DAY[1:a-1] <- ifelse(sub2==TRUE, b, finaltable$PA_DAY)
    assign('finaltable',finaltable,envir=.GlobalEnv)

    c <- which((!is.na(finaltable$PA_DAY) & AGE_DAY > 18)==TRUE)[1]
    if (!is.na(c)) {
      d <- finaltable$PA_DAY[c]
      sub3 <- AGE_DAY > 18
      sub3 <- sub3[1:c-1]
      finaltable$PA_DAY[1:c-1] <- ifelse(sub3==TRUE, d, finaltable$PA_DAY)
      assign('finaltable',finaltable,envir=.GlobalEnv)
    }

    finaltable$PA_DAY <- c(NA, finaltable$PA_DAY[!is.na(finaltable$PA_DAY)])[cumsum(!is.na(finaltable$PA_DAY)) + 1]
    assign('finaltable',finaltable,envir=.GlobalEnv)
  }

  #final
  z <- dim(finaltable)[1]
  finaltable$MRNUMBER <- rep.int(finaltable$MRNUMBER[1], z)
  assign('finaltable',finaltable,envir=.GlobalEnv)
  finaltable <- finaltable[ , c(2, 1, 3:ncol(finaltable)) ]
  assign('finaltable',finaltable,envir=.GlobalEnv)
  finaltable <- finaltable[, c(1:82, 84:ncol(finaltable), 83)] #putting comments at the end of the table
  assign('finaltable',finaltable,envir=.GlobalEnv)
  finaltable <- as.data.frame(append(finaltable, list(AGE = AGE), after=4))
  assign('finaltable',finaltable,envir=.GlobalEnv)

  setwd(directory)

  observe_load <- FALSE
  print("Would you like to save a temporary file for checking reference values?")
  print("Type 'yes' to save a file to look at, type 'no' to move onto next step")
  rl <- " "
  while(tolower(rl)!="yes" && tolower(rl)!="no") {
    rl <- readline(prompt="Enter here: ")
  }
  if(tolower(rl=="yes")) {
    observe_load <- TRUE
  }

  if (observe_load == TRUE) {
    print(paste("Saving file as",gsub(" ","",paste(patient,"_ANTHROPOMETRICS_REFERENCE.xlsx")),"in directory",directory))
    xlsx <- "ANTHROPOMETRICS_REFERENCE.xlsx"
    xlsx <- gsub(" ","", paste(patient,"_", xlsx))
    write.xlsx2(output,file=xlsx,row.names=FALSE, showNA=FALSE)

    options(java.parameters = "-Xmx10000m")
    print(paste("Saving file as",gsub(" ","",paste(patient,"_ANTHROPOMETRICS_REFERENCE_DAY.xlsx")),"in directory",directory))
    xlsx <- "ANTHROPOMETRICS_REFERENCE_DAY.xlsx"
    xlsx <- gsub(" ","", paste(patient,"_", xlsx))
    write.xlsx2(output2,file=xlsx,row.names=FALSE, showNA=FALSE)

  }
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

  observe_load <- FALSE
  print("Would you like to save a temporary file for checking if there is a data flag?")
  print("Type 'yes' to save a file to look at, type 'no' to move onto next step")
  rl <- " "
  while(tolower(rl)!="yes" && tolower(rl)!="no") {
    rl <- readline(prompt="Enter here: ")
  }
  if(tolower(rl=="yes")) {
    observe_load <- TRUE
  }

  if (observe_load == TRUE) {

    # write Jurate's special excel document if there is a data flag
    xlsx <- "ANTHROPOMETRICS_CHECK.xlsx"
    xlsx <- gsub(" ","", paste(patient,"_", xlsx))
    write.xlsx2(anthropometricsCheck,file=xlsx,row.names=FALSE, showNA=FALSE)
    print(paste("Saving anthropometric check as",gsub(" ","",paste(patient,"_ANTHROPOMETRICS_CHECK.xlsx")),"in directory",getwd()))


  }

  print(paste("Saving anthropometric clinical data as",gsub(" ","",paste(patient,"_ANTHROPOMETRICS_CLINICAL.xlsx")),"in directory",getwd()))

  options(java.parameters = "-Xmx10000m")
  xlsx <- "ANTHROPOMETRICS_CLINICAL.xlsx"
  xlsx <- gsub(" ","", paste(patient,"_", xlsx))
  write.xlsx2(finaltable,file=xlsx,row.names=FALSE, showNA=FALSE)

  if (is.na(Demographics.Identified$STRATA[1]) ==  FALSE) {
    anthrograph(finaltable)
  }

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

  #to remove variables in environment so that you can move onto next patient
  objs <- ls(pos = ".GlobalEnv")
  rm(list=objs, pos= ".GlobalEnv")

}

