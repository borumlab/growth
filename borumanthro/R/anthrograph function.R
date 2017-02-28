#' Upload to database
#'
#' This function allow you to create an anthropometric clinical graph.
#' @param finaltable
#' @keywords anthrograph
#' @import ggplot2
#function to upload anthros into MySQL
#Anthropometric GRAPH
#Graph is different depending on if patient is naive or experienced
anthrograph <- function(finaltable){

  if (!require("ggplot2")) {
    install.packages("ggplot2")
  }
  library(ggplot2)

  first <- unique(Demographics.Identified[Demographics.Identified$MRNUMBER==finaltable$MRNUMBER[1],colnames(Demographics.Identified)=="FIRST"])
  last <- unique(Demographics.Identified[Demographics.Identified$MRNUMBER==finaltable$MRNUMBER[1],colnames(Demographics.Identified)=="LAST"])
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

if (nrow(graphdata) != 0) {

AGE <- graphdata$AGE

HT_Z_SCORE <- ifelse(AGE >= 2 & AGE <= 20, graphdata$CDC_HT_Z_DAY, ifelse(AGE < 2, graphdata$WHO_HT_Z_DAY, graphdata$NHANES_HT_Z_DAY))
WT_Z_SCORE <- ifelse(AGE >= 2 & AGE <= 20, graphdata$CDC_WT_Z_DAY, ifelse(AGE < 2, graphdata$WHO_WT_Z_DAY, graphdata$NHANES_WT_Z_DAY))
BMI_Z_SCORE <- ifelse(AGE >= 2 & AGE <= 20, graphdata$CDC_BMI_Z_DAY, ifelse(AGE < 2, graphdata$WHO_BMI_Z_DAY, graphdata$NHANES_BMI_Z_DAY))

HT_PCTL <- ifelse(AGE >= 2 & AGE <= 20, graphdata$CDC_HT_PCTL_DAY, ifelse(AGE < 2, graphdata$WHO_HT_PCTL_DAY, graphdata$NHANES_HT_PCTL_DAY))
WT_PCTL <- ifelse(AGE >= 2 & AGE <= 20, graphdata$CDC_WT_PCTL_DAY, ifelse(AGE < 2, graphdata$WHO_WT_PCTL_DAY, graphdata$NHANES_WT_PCTL_DAY))
BMI_PCTL <- ifelse(AGE >= 2 & AGE <= 20, graphdata$CDC_BMI_PCTL_DAY, ifelse(AGE < 2, graphdata$WHO_BMI_PCTL_DAY, graphdata$NHANES_BMI_PCTL_DAY))

#for patients whose age is 20 but greater than 240 months, using NHANES for these patients
if (anyNA(HT_Z_SCORE) == TRUE ) {
  HT_Z_SCORE <- ifelse(AGE >= 2 & AGE < 20, graphdata$CDC_HT_Z_DAY, ifelse(AGE < 2, graphdata$WHO_HT_Z_DAY, graphdata$NHANES_HT_Z_DAY))
}

if (anyNA(WT_Z_SCORE) == TRUE) {
  WT_Z_SCORE <- ifelse(AGE >= 2 & AGE < 20, graphdata$CDC_WT_Z_DAY, ifelse(AGE < 2, graphdata$WHO_WT_Z_DAY, graphdata$NHANES_WT_Z_DAY))
}

if (anyNA(BMI_Z_SCORE) == TRUE) {
  BMI_Z_SCORE <- ifelse(AGE >= 2 & AGE < 20, graphdata$CDC_BMI_Z_DAY, ifelse(AGE < 2, graphdata$WHO_BMI_Z_DAY, graphdata$NHANES_BMI_Z_DAY))
}

if (anyNA(HT_PCTL) == TRUE ) {
  HT_PCTL <- ifelse(AGE >= 2 & AGE < 20, graphdata$CDC_HT_PCTL_DAY, ifelse(AGE < 2, graphdata$WHO_HT_PCTL_DAY, graphdata$NHANES_HT_PCTL_DAY))
}

if (anyNA(WT_PCTL) == TRUE) {
  WT_PCTL <- ifelse(AGE >= 2 & AGE < 20, graphdata$CDC_WT_PCTL_DAY, ifelse(AGE < 2, graphdata$WHO_WT_PCTL_DAY, graphdata$NHANES_WT_PCTL_DAY))
}

if (anyNA(BMI_PCTL) == TRUE) {
  BMI_PCTL <- ifelse(AGE >= 2 & AGE < 20, graphdata$CDC_BMI_PCTL_DAY, ifelse(AGE < 2, graphdata$WHO_BMI_PCTL_DAY, graphdata$NHANES_BMI_PCTL_DAY))
}


#To see what values are being graphed:
DATE <- graphdata$DATE
MRNUMBER <- finaltable$MRNUMBER[1:dim(graphdata)[1]]
data1 <- cbind.data.frame(DATE, HT_Z_SCORE, WT_Z_SCORE, BMI_Z_SCORE)
data2 <- cbind.data.frame(MRNUMBER, DATE, AGE, HT_Z_SCORE, WT_Z_SCORE, BMI_Z_SCORE, HT_PCTL, WT_PCTL, BMI_PCTL)


setwd(directory)

#Creating output table:
xlsx <- "ANTHROPOMETRICS_GRAPH_VALUES.xlsx"
xlsx <- gsub(" ","", paste(patient,"_", xlsx))
write.xlsx2(data2,file=xlsx,row.names=FALSE, showNA=FALSE)

data1 <- data1[complete.cases(data1),]

#for y-axis labels
z <- c(data1$HT_Z_SCORE, data1$WT_Z_SCORE, data1$BMI_Z_SCORE)
z1 <- floor((min(z))/0.5)*0.5
z2 <- ceiling((max(z))/0.5)*0.5

datebreaks <- seq.Date(min(DATE), max(DATE), length.out=8)


string <- paste(first,gsub(" ","",paste(last,":")),"Anthropometric Z-scores")


p <- ggplot(data1, aes(x=DATE))
anthrograph <- p + geom_line(aes(y=HT_Z_SCORE, colour="Height Z-score"), size=1.5) +
  geom_point(aes(y=HT_Z_SCORE, shape="Height Z-score", color="Height Z-score"), size=5) +
  geom_line(aes(y=WT_Z_SCORE, colour="Weight Z-score"), size=1.5) +
  geom_point(aes(y=WT_Z_SCORE, shape="Weight Z-score", color="Weight Z-score"), size=4) +
  geom_line(aes(y=BMI_Z_SCORE, colour="BMI Z-score"), size=1.5) +
  geom_point(aes(y=BMI_Z_SCORE, shape="BMI Z-score", color="BMI Z-score"), size=4) +
  xlab("Date") +
  ylab("Z-score") +
  ggtitle(string) +
  geom_hline(yintercept=seq(z1, z2, by=0.5)) +
  scale_y_continuous(breaks=seq(z1, z2, by=0.5)) +
  scale_x_date(breaks=datebreaks, date_labels= "%m/%d/%y") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(size=11, colour= "black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.y = element_text(size=11, colour = "black"),
        axis.title.y = element_text(size=12, colour = "black"),
        legend.position="bottom",
        legend.background=element_rect(fill="white", colour="black"),
        legend.text=element_text(size=11),
        legend.key.width=unit(2, "line")) +
  scale_colour_manual(name = "", values=c("Height Z-score" = "orange", "Weight Z-score"="purple", "BMI Z-score" = "green3")) +
  scale_shape_manual(name = "", values=c("Height Z-score" = 18, "Weight Z-score"=15, "BMI Z-score"=17))

setwd(directory)

#save in patient folder
png <- "ANTHROPOMETRICS_GRAPH.png"
png <- gsub(" ","", paste(patient,"_", png))
ggsave(anthrograph, file=png, height=4.5, width=6.61, units='in', dpi=600)

}
}

