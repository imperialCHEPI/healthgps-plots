library(readr)
library(ggplot2)
library(collapse)

# Import data ----
datas1 <- read.csv("HealthGPS_Result_S1.csv")
datas2 <- read.csv("HealthGPS_Result_S2.csv")
datas4 <- read.csv("HealthGPS_Result_S4.csv")
datas5 <- read.csv("HealthGPS_Result_S5.csv")

# Summarize ----
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

# Visualization ----
## Generate mean values ----
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


## Plot ----
ggplot(data = meansodium, aes(x = time, y = foodsodium, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Sodium") + 
  xlab("Year") +
  labs(fill = "Source")
ggplot(data = meanbmi, aes(x = time, y = bmi, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("BMI") + 
  xlab("Year") +
  labs(fill = "Source")
ggplot(data = meanihd, aes(x = time, y = ischemicheartdisease, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("IHD") + 
  xlab("Year") +
  labs(fill = "Source")
ggplot(data = meanasthma, aes(x = time, y = asthma, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Asthma") + 
  xlab("Year") +
  labs(fill = "Source")
ggplot(data = meandb, aes(x = time, y = diabetes, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Diabetes") + 
  xlab("Year") +
  labs(fill = "Source")
ggplot(data = meanintra, aes(x = time, y = intracerebralhemorrhage, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Intra hemorrhage") + 
  xlab("Year") +
  labs(fill = "Source")
ggplot(data = meanstocancer, aes(x = time, y = stomachcancer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Stomach cancer") + 
  xlab("Year") +
  labs(fill = "Source")
ggplot(data = meanstroke, aes(x = time, y = ischemicstroke, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Stroke") + 
  xlab("Year") +
  labs(fill = "Source")
ggplot(data = meansuba, aes(x = time, y = subarachnoidhemorrhage, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Suba hemorrhage") + 
  xlab("Year") +
  labs(fill = "Source")

ggplot(data = meanvision, aes(x = time, y = visionimpairment, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Vision impairment") + 
  xlab("Year") +
  labs(fill = "Source")


# Reshape data to compare numbers----
meandb_wide <- reshape(meandb,timevar="source",idvar = "time",direction = "wide")
meanasthma_wide <- reshape(meanasthma,timevar="source",idvar = "time",direction = "wide")
meanihd_wide <- reshape(meanihd,timevar="source",idvar = "time",direction = "wide")
meanintra_wide <- reshape(meanintra,timevar="source",idvar = "time",direction = "wide")
meanstocancer_wide <- reshape(meanstocancer,timevar="source",idvar = "time",direction = "wide")
meanstroke_wide <- reshape(meanstroke,timevar="source",idvar = "time",direction = "wide")
meansuba_wide <- reshape(meansuba,timevar="source",idvar = "time",direction = "wide")

