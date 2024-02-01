# Working directory ----
setwd("C:/Users/jzhu5/OneDrive - Imperial College London/Project/RSTL_India/Visualization") 
setwd("/Users/jasmine/OneDrive - Imperial College London/Project/RSTL_India/Visualization")
setwd("U:/home/healthgps/results/france")
setwd("U:/home/healthgps-v1220/healthgps/results/france")
setwd("U:/home/healthgps/results/india")

# Packages ----
install.packages("readr")
install.packages("ggplot2")
install.packages("collapse")
install.packages("Rtools")
install.packages("grid")
install.packages("gridExtra")
library(readr)
library(ggplot2)
library(collapse)
library(grid)
library(gridExtra)

# Import data ----
## Single data file ----
data <- read.csv("HealthGPS.csv")
datas1 <- read.csv("HealthGPS_Result_S1.csv")
datas2 <- read.csv("HealthGPS_Result_S2.csv")
datas4 <- read.csv("HealthGPS_Result_S4.csv")
datas5 <- read.csv("HealthGPS_Result_S5.csv")

# Summarize ----
## General ----
tapply(X=(data$fat), INDEX = list(data$source, data$time), FUN = mean)
## Policy S1 ----
tapply(X=(datas1$foodcarbohydrate), INDEX = list(datas1$source, datas1$time, datas1$gender_name), FUN = mean)
tapply(X=(datas1$foodfat), INDEX = list(datas1$source, datas1$time, datas1$gender_name), FUN = mean)
tapply(X=(datas1$foodprotein), INDEX = list(datas1$source, datas1$time, datas1$gender_name), FUN = mean)
tapply(X=(datas1$foodsodium), INDEX = list(datas1$source, datas1$time, datas1$gender_name), FUN = mean)
tapply(X=(datas1$energyintake), INDEX = list(datas1$source, datas1$time, datas1$gender_name), FUN = mean)
tapply(X=(datas1$weight), INDEX = list(datas1$source, datas1$time, datas1$gender_name), FUN = mean)
tapply(X=(datas1$ischemicheartdisease), INDEX = list(datas1$source, datas1$time, datas1$gender_name), FUN = mean)
tapply(X=(datas1$diabetes), INDEX = list(datas1$source, datas1$time, datas1$gender_name), FUN = mean)
## Policy S2 ----
tapply(X=(datas2$foodcarbohydrate), INDEX = list(datas2$source, datas2$time, datas2$gender_name), FUN = mean)
tapply(X=(datas2$foodfat), INDEX = list(datas2$source, datas2$time, datas2$gender_name), FUN = mean)
tapply(X=(datas2$foodprotein), INDEX = list(datas2$source, datas2$time, datas2$gender_name), FUN = mean)
tapply(X=(datas2$foodsodium), INDEX = list(datas2$source, datas2$time, datas2$gender_name), FUN = mean)
tapply(X=(datas2$energyintake), INDEX = list(datas2$source, datas2$time, datas2$gender_name), FUN = mean)
tapply(X=(datas2$weight), INDEX = list(datas2$source, datas2$time, datas2$gender_name), FUN = mean)
tapply(X=(datas2$physicalactivity), INDEX = list(datas2$source, datas2$time, datas2$gender_name), FUN = mean)
tapply(X=(datas2$bmi), INDEX = list(datas2$source, datas2$time, datas2$gender_name), FUN = mean)
tapply(X=(datas2$ischemicheartdisease), INDEX = list(datas2$source, datas2$time, datas2$gender_name), FUN = mean)
tapply(X=(datas2$diabetes), INDEX = list(datas2$source, datas2$time, datas2$gender_name), FUN = mean)

## Generate mean values ----
meanfat <- collap(data, fat ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanprotein <- collap(data, protein ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meansodium <- collap(data, sodium ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanenergy <- collap(data, energy ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanbmi <- collap(data, bmi ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanalzheimer <- collap(data, alzheimer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanasthma <- collap(data, asthma ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meancolorectal <- collap(data, colorectalcancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meandiabetes <- collap(data, diabetes ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanlowbackpain <- collap(data, lowbackpain ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanosteoknee <- collap(data, osteoarthritisknee ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanobese <- collap(data, obese_weight ~ source + time, fmean, w = ~ count, keep.w = FALSE)

meansodium <- collap(datas1, foodsodium ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanbmi <- collap(datas1, bmi ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanihd <- collap(datas1, ischemicheartdisease ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meandb <- collap(datas1, diabetes ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanstroke <- collap(datas1, ischemicstroke ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanintra <- collap(datas1, intracerebralhemorrhage ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanasthma <- collap(datas1, asthma ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanstocancer <- collap(datas1, stomachcancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meansuba <- collap(datas1, subarachnoidhemorrhage ~ source + time, fmean, w = ~ count, keep.w = FALSE) 

meanpancrea <- collap(datas1, pancreatitis ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanparkinson <- collap(datas1, parkinson ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanreflux <- collap(datas1, reflux ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanalzheimer <- collap(datas1, alzheimer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meananxiety <- collap(datas1, anxiety ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanarthritis <- collap(datas1, arthritis ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanbreastcancer <- collap(datas1, breastcancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meancolorectalcancer <- collap(datas1, colorectalcancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meandepression <- collap(datas1, depression ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meandermatitis <- collap(datas1, dermatitis ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanesophaguscancer <- collap(datas1, esophaguscancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meangallbladder <- collap(datas1, gallbladder ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meangallbladdercancer <- collap(datas1, gallbladdercancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meangout <- collap(datas1, gout ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meankidneycancer <- collap(datas1, kidneycancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanlivercancer <- collap(datas1, livercancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanlowbackpain <- collap(datas1, lowbackpain ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanmigraine <- collap(datas1, migraine ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanosteohip <- collap(datas1, osteoarthritiship ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanosteoknee <- collap(datas1, osteoarthritisknee ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanovary <- collap(datas1, ovarycancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanpancreacancer <- collap(datas1, pancreascancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanthyroidcancer <- collap(datas1, thyroidcancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanpsoriasis <- collap(datas1, psoriasis ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanpulmonar <- collap(datas1, pulmonar ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanvision <- collap(datas1, visionimpairment ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meancataract <- collap(datas1, cataract ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meancirr <- collap(datas1, cirrhosis ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanoverweight <- collap(datas1, over_weight ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanobese <- collap(datas1, obese_weight ~ source + time, fmean, w = ~ count, keep.w = FALSE)

# Reshape data to compare numbers----
meandb_wide <- reshape(meandb,timevar="source",idvar = "time",direction = "wide")
meanasthma_wide <- reshape(meanasthma,timevar="source",idvar = "time",direction = "wide")
meanihd_wide <- reshape(meanihd,timevar="source",idvar = "time",direction = "wide")
meanintra_wide <- reshape(meanintra,timevar="source",idvar = "time",direction = "wide")
meanstocancer_wide <- reshape(meanstocancer,timevar="source",idvar = "time",direction = "wide")
meanstroke_wide <- reshape(meanstroke,timevar="source",idvar = "time",direction = "wide")
meansuba_wide <- reshape(meansuba,timevar="source",idvar = "time",direction = "wide")


# Import multiple data files at once ----
temp = list.files(pattern = "\\.csv$")
mydata =lapply(temp, read.csv)
data <- do.call(rbind,mydata)
## Alternative: separate data frames ----
# for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

# Generate mean values ----
meanfat <- collap(data, fat ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanprotein <- collap(data, protein ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meansodium <- collap(data, sodium ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanenergy <- collap(data, energy ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanbmi <- collap(data, bmi ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanalzheimer <- collap(data, alzheimer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanasthma <- collap(data, asthma ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meancolorectal <- collap(data, colorectalcancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meandiabetes <- collap(data, diabetes ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanlowbackpain <- collap(data, lowbackpain ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanosteoknee <- collap(data, osteoarthritisknee ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanobese <- collap(data, obese_weight ~ source + time, fmean, w = ~ count, keep.w = FALSE)

# Visualization ----
## New version-Main branch ----
p1new <- ggplot(data = meansodium, aes(x = time, y = sodium, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Sodium-new") + 
  xlab("Year") +
  labs(fill = "Source")
p2new <- ggplot(data = meanfat, aes(x = time, y = fat, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Fat-new") + 
  xlab("Year") +
  labs(fill = "Source")
p3new <- ggplot(data = meanprotein, aes(x = time, y = protein, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Protein-new") + 
  xlab("Year") +
  labs(fill = "Source")
p4new <- ggplot(data = meanenergy, aes(x = time, y = energy, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Energy-new") + 
  xlab("Year") +
  labs(fill = "Source")
p5new <- ggplot(data = meanbmi, aes(x = time, y = bmi, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("BMI-new") + 
  xlab("Year") +
  labs(fill = "Source")
p6new <- ggplot(data = meanobese, aes(x = time, y = obese_weight, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Obese-new") + 
  xlab("Year") +
  labs(fill = "Source")
p7new <- ggplot(data = meanalzheimer, aes(x = time, y = alzheimer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Alzheimer-new") + 
  xlab("Year") +
  labs(fill = "Source")
p8new <- ggplot(data = meanasthma, aes(x = time, y = asthma, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Asthma-new") + 
  xlab("Year") +
  labs(fill = "Source")
p9new <- ggplot(data = meancolorectal, aes(x = time, y = colorectalcancer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Colorectal cancer-new") + 
  xlab("Year") +
  labs(fill = "Source")
p10new <- ggplot(data = meandiabetes, aes(x = time, y = diabetes, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Diabetes-new") + 
  xlab("Year") +
  labs(fill = "Source")
p11new <- ggplot(data = meanlowbackpain, aes(x = time, y = lowbackpain, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Low back pain-new") + 
  xlab("Year") +
  labs(fill = "Source")
p12new <- ggplot(data = meanosteoknee, aes(x = time, y = osteoarthritisknee, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Osteoarthritis knee-new") + 
  xlab("Year") +
  labs(fill = "Source")

png("gridplot-50sims.png",width = 1420,height = 800)
gridplot <- grid.arrange(p1new,p2new,p3new,p4new,p5new,p6new,p7new,p8new,p9new,p10new,p11new,p12new,nrow=4)
dev.off()

## V1.2.2.0 ----
p1old <- ggplot(data = meansodium, aes(x = time, y = sodium, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Sodium-v1220") + 
  xlab("Year") +
  labs(fill = "Source")
p2old <- ggplot(data = meanfat, aes(x = time, y = fat, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Fat-v1220") + 
  xlab("Year") +
  labs(fill = "Source")
p3old <- ggplot(data = meanprotein, aes(x = time, y = protein, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Protein-v1220") + 
  xlab("Year") +
  labs(fill = "Source")
p4old <- ggplot(data = meanenergy, aes(x = time, y = energy, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Energy-v1220") + 
  xlab("Year") +
  labs(fill = "Source")
p5old <- ggplot(data = meanbmi, aes(x = time, y = bmi, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("BMI-v1220") + 
  xlab("Year") +
  labs(fill = "Source")
p6old <- ggplot(data = meanobese, aes(x = time, y = obese_weight, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Obese-v1220") + 
  xlab("Year") +
  labs(fill = "Source")
p7old <- ggplot(data = meanalzheimer, aes(x = time, y = alzheimer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Alzheimer-v1220") + 
  xlab("Year") +
  labs(fill = "Source")
p8old <- ggplot(data = meanasthma, aes(x = time, y = asthma, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Asthma-v1220") + 
  xlab("Year") +
  labs(fill = "Source")
p9old <- ggplot(data = meancolorectal, aes(x = time, y = colorectalcancer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Colorectal cancer-v1220") + 
  xlab("Year") +
  labs(fill = "Source")
p10old <- ggplot(data = meandiabetes, aes(x = time, y = diabetes, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Diabetes-v1220") + 
  xlab("Year") +
  labs(fill = "Source")
p11old <- ggplot(data = meanlowbackpain, aes(x = time, y = lowbackpain, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Low back pain-v1220") + 
  xlab("Year") +
  labs(fill = "Source")
p12old <- ggplot(data = meanosteoknee, aes(x = time, y = osteoarthritisknee, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Osteoarthritis knee-v1220") + 
  xlab("Year") +
  labs(fill = "Source")

png("gridplot-v1220.png",width = 1420,height = 800)
gridplot <- grid.arrange(p1old,p2old,p3old,p4old,p5old,p6old,p7old,p8old,p9old,p10old,p11old,p12old,nrow=4)
dev.off()


## Comparison ----
png("comparison-1.png",width = 1420,height = 800)
gridplot <- grid.arrange(p1old,p2old,p3old,p1new,p2new,p3new,p4old,p5old,p6old,p4new,p5new,p6new,nrow=4)
dev.off()
png("comparison-2.png",width = 1420,height = 800)
gridplot <- grid.arrange(p7old,p8old,p9old,p7new,p8new,p9new,p10old,p11old,p12old,p10new,p11new,p12new,nrow=4)
dev.off()


