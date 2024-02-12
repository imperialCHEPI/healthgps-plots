library(readr)
library(ggplot2)
library(collapse)
library(grid)
library(gridExtra)

# Option 1 of importing multiple datasets: Command line - more flexible, needs to be extended to 100 datasets ----
## Check if command-line arguments are provided
if (length(commandArgs(trailingOnly = TRUE)) < 4) {
  stop("Usage: Rscript script.R <path1> <path2> <path3> <path4>")
}

## Get file paths from command-line arguments
path1 <- commandArgs(trailingOnly = TRUE)[1]
path2 <- commandArgs(trailingOnly = TRUE)[2]
path3 <- commandArgs(trailingOnly = TRUE)[3]
path4 <- commandArgs(trailingOnly = TRUE)[4]

## Read data from CSV files
data1 <- read.csv(path1)
data2 <- read.csv(path2)
data3 <- read.csv(path3)
data4 <- read.csv(path4)

## Combine all data
data <- rbind(data1,data2,data3,data4)


# Option 2 of importing multiple datasets ----
# temp = list.files(pattern = "\\.csv$")
# mydata = lapply(temp, read.csv)
# data <- do.call(rbind, mydata)

# Summarize when necessary ----
# Notes: name of nutrients can be different in old HML and new KH model
# tapply(X = (data$foodcarbohydrate), INDEX = list(data$source, data$time, data$gender_name), FUN = mean)
# tapply(X = (data$foodfat), INDEX = list(data$source, data$time, data$gender_name), FUN = mean)
# tapply(X = (data$foodprotein), INDEX = list(data$source, data$time, data$gender_name), FUN = mean)
# tapply(X = (data$foodsodium), INDEX = list(data$source, data$time, data$gender_name), FUN = mean)
# tapply(X = (data$energyintake), INDEX = list(data$source, data$time, data$gender_name), FUN = mean)
# tapply(X = (data$weight), INDEX = list(data$source, data$time, data$gender_name), FUN = mean)
# tapply(X = (data$ischemicheartdisease), INDEX = list(data$source, data$time, data$gender_name), FUN = mean)
# tapply(X = (data$diabetes), INDEX = list(data$source, data$time, data$gender_name), FUN = mean)

# Generate weighted mean values ----
# Results from new KH model can have different names of nutrients, e.g.
# meansodium <- collap(data, foodsodium ~ source + time, fmean, w = ~count, keep.w = FALSE)
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
meananxiety <- collap(data, anxiety ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meandermatitis <- collap(data, dermatitis ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meancataract <- collap(data, cataract ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanpulmonar <- collap(data, pulmonar ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanpancreatitis <- collap(data, pancreatitis ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meancirrhosis <- collap(data, cirrhosis ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meangallbladder <- collap(data, gallbladder ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanreflux <- collap(data, reflux ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meangout <- collap(data, gout ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meandepression <- collap(data, depression ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanmigraine <- collap(data, migraine ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanmultiplesclerosis <- collap(data, multiplesclerosis ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanparkinson <- collap(data, parkinson ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanpsoriasis <- collap(data, psoriasis ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanarthritis <- collap(data, arthritis ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanvisionimpairment <- collap(data, visionimpairment ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanintracerebralhemorrhage <- collap(data, intracerebralhemorrhage ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanischemicheartdisease <- collap(data, ischemicheartdisease ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanischemicstroke <- collap(data, ischemicstroke ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meansubarachnoidhemorrhage <- collap(data, subarachnoidhemorrhage ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanbreastcancer <- collap(data, breastcancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanesophaguscancer <- collap(data, esophaguscancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meangallbladdercancer <- collap(data, gallbladdercancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meankidneycancer <- collap(data, kidneycancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanlivercancer <- collap(data, livercancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanovarycancer <- collap(data, ovarycancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanpancreascancer <- collap(data, pancreascancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanstomachcancer <- collap(data, stomachcancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanthyroidcancer <- collap(data, thyroidcancer ~ source + time, fmean, w = ~ count, keep.w = FALSE)
meanobese <- collap(data, obese_weight ~ source + time, fmean, w = ~ count, keep.w = FALSE)

# Plot ----
p1new <- ggplot(data = meansodium, aes(x = time, y = sodium, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Sodium-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p2new <- ggplot(data = meanfat, aes(x = time, y = fat, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Fat-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p3new <- ggplot(data = meanprotein, aes(x = time, y = protein, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Protein-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p4new <- ggplot(data = meanenergy, aes(x = time, y = energy, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Energy-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p5new <- ggplot(data = meanbmi, aes(x = time, y = bmi, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("BMI-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p6new <- ggplot(data = meanobese, aes(x = time, y = obese_weight, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Obese-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p7new <- ggplot(data = meanalzheimer, aes(x = time, y = alzheimer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Alzheimer-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p8new <- ggplot(data = meanasthma, aes(x = time, y = asthma, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Asthma-new") + 
  xlab("Year") +
  labs(fill = "Source")
p9new <- ggplot(data = meancolorectal, aes(x = time, y = colorectalcancer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Colorectal cancer-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p10new <- ggplot(data = meandiabetes, aes(x = time, y = diabetes, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Diabetes-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p11new <- ggplot(data = meanlowbackpain, aes(x = time, y = lowbackpain, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Low back pain-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p12new <- ggplot(data = meanosteoknee, aes(x = time, y = osteoarthritisknee, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Osteoarthritis knee-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p13new <- ggplot(data = meananxiety, aes(x = time, y = anxiety, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Anxiety-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p14new <- ggplot(data = meanarthritis, aes(x = time, y = arthritis, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Arthritis-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p15new <- ggplot(data = meanbreastcancer, aes(x = time, y = breastcancer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Breast cancer-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p16new <- ggplot(data = meancataract, aes(x = time, y = cataract, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Cataract-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p17new <- ggplot(data = meancirrhosis, aes(x = time, y = cirrhosis, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Cirrhosis-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p18new <- ggplot(data = meandepression, aes(x = time, y = depression, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Depression-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p19new <- ggplot(data = meandermatitis, aes(x = time, y = dermatitis, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Dermatitis-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p20new <- ggplot(data = meanesophaguscancer, aes(x = time, y = esophaguscancer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Esophagus cancer-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p21new <- ggplot(data = meangallbladder, aes(x = time, y = gallbladder, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Gallbladder-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p22new <- ggplot(data = meangallbladdercancer, aes(x = time, y = gallbladdercancer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Gallbladder cancer-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p23new <- ggplot(data = meangout, aes(x = time, y = gout, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Gout-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p24new <- ggplot(data = meanintracerebralhemorrhage, aes(x = time, y = intracerebralhemorrhage, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Intracerebral hemorrhage-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p25new <- ggplot(data = meanischemicstroke, aes(x = time, y = ischemicstroke, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Ischemic stroke-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p26new <- ggplot(data = meankidneycancer, aes(x = time, y = kidneycancer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Kidney cancer-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p27new <- ggplot(data = meanlivercancer, aes(x = time, y = livercancer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Liver cancer-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p28new <- ggplot(data = meanmigraine, aes(x = time, y = migraine, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Migraine-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p29new <- ggplot(data = meanmultiplesclerosis, aes(x = time, y = multiplesclerosis, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Multiple sclerosis-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p30new <- ggplot(data = meanovarycancer, aes(x = time, y = ovarycancer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Ovary cancer-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p31new <- ggplot(data = meanpancreascancer, aes(x = time, y = pancreascancer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Pancreas cancer-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p32new <- ggplot(data = meanpancreatitis, aes(x = time, y = pancreatitis, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Pancreatitis-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p33new <- ggplot(data = meanparkinson, aes(x = time, y = parkinson, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Parkinson-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p34new <- ggplot(data = meanpsoriasis, aes(x = time, y = psoriasis, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Psoriasis-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p35new <- ggplot(data = meanpulmonar, aes(x = time, y = pulmonar, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Pulmonar-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p36new <- ggplot(data = meanreflux, aes(x = time, y = reflux, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Reflux-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p37new <- ggplot(data = meanstomachcancer, aes(x = time, y = stomachcancer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Stomach cancer-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p38new <- ggplot(data = meansubarachnoidhemorrhage, aes(x = time, y = subarachnoidhemorrhage, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Subarachnoid hemorrhage-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p39new <- ggplot(data = meanthyroidcancer, aes(x = time, y = thyroidcancer, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Thyroid cancer-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p40new <- ggplot(data = meanvisionimpairment, aes(x = time, y = visionimpairment, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("Vision impairment-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")
p41new <- ggplot(data = meanischemicheartdisease, aes(x = time, y = ischemicheartdisease, group = source)) + 
  geom_line(aes(col = source)) + 
  ggtitle("IHD-hlm-new") + 
  xlab("Year") +
  labs(fill = "Source")

# Combine all plots ----
png("gridplot-50sims-risk-factors.png",width = 1420,height = 800)
gridplot <- grid.arrange(p1new,p2new,p3new,p4new,p5new,p6new,nrow=3)
dev.off()
png("gridplot-50sims-disease-1.png",width = 1420,height = 800)
gridplot <- grid.arrange(p7new,p11new,p12new,p13new,p14new,p16new,p17new,p18new,p19new,nrow=3)
dev.off()
png("gridplot-50sims-disease-2.png",width = 1420,height = 800)
gridplot <- grid.arrange(p21new,p23new,p28new,p29new,p32new,p33new,p34new,p35new,p36new,p40new,nrow=4)
dev.off()
png("gridplot-50sims-cancer.png",width = 1420,height = 800)
gridplot <- grid.arrange(p9new,p15new,p20new,p22new,p26new,p27new,p30new,p31new,p39new,nrow=3)
dev.off()
png("gridplot-50sims-important-disease.png",width = 1420,height = 800)
gridplot <- grid.arrange(p41new,p8new,p10new,p25new,p37new,p38new,p24new,nrow=3)
dev.off()


# Reshape data to compare numbers when necessary (list can be extended) ----
wide_meanbmi <- reshape(meanbmi,timevar="source",idvar = "time",direction = "wide")
wide_meanobese <- reshape(meanobese,timevar="source",idvar = "time",direction = "wide")
wide_meansodium <- reshape(meansodium,timevar="source",idvar = "time",direction = "wide")
wide_meandiabetes <- reshape(meandiabetes,timevar="source",idvar = "time",direction = "wide")
wide_meanasthma <- reshape(meanasthma,timevar="source",idvar = "time",direction = "wide")
wide_meanihd <- reshape(meanischemicheartdisease,timevar="source",idvar = "time",direction = "wide")
wide_meanintra <- reshape(meanintracerebralhemorrhage,timevar="source",idvar = "time",direction = "wide")
wide_meanstomachcancer <- reshape(meanstomachcancer,timevar="source",idvar = "time",direction = "wide")
wide_meanischemicstroke <- reshape(meanischemicstroke,timevar="source",idvar = "time",direction = "wide")
wide_meansuba <- reshape(meansubarachnoidhemorrhage,timevar="source",idvar = "time",direction = "wide")


# Calculate diff (list can be extended) ----
wide_meanbmi$diff <- wide_meanbmi$bmi.baseline-wide_meanbmi$bmi.intervention
wide_meanobese$diff <- wide_meanobese$obese_weight.baseline - wide_meanobese$obese_weight.intervention
wide_meansodium$diff <- wide_meansodium$sodium.baseline - wide_meansodium$sodium.intervention
wide_meandiabetes$diff <- wide_meandiabetes$diabetes.baseline - wide_meandiabetes$diabetes.intervention
wide_meanasthma$diff <- wide_meanasthma$asthma.baseline - wide_meanasthma$asthma.intervention
wide_meanihd$diff <- wide_meanihd$ischemicheartdisease.baseline - wide_meanihd$ischemicheartdisease.intervention
wide_meanintra$diff <- wide_meanintra$intracerebralhemorrhage.baseline - wide_meanintra$intracerebralhemorrhage.intervention
wide_meanstomachcancer$diff <- wide_meanstomachcancer$stomachcancer.baseline - wide_meanstomachcancer$stomachcancer.intervention
wide_meanischemicstroke$diff <- wide_meanischemicstroke$ischemicstroke.baseline - wide_meanischemicstroke$ischemicstroke.intervention
wide_meansuba$diff <- wide_meansuba$subarachnoidhemorrhage.baseline - wide_meansuba$subarachnoidhemorrhage.intervention