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