###    Supporting code for McIvor et al. Mark-recapture validates the use of photo-identification for 
###    the widely distributed blue-spotted ribbontail ray, Taeniura lymma.


# install packages
install.packages("readxl")
install.packages("car")
devtools::install_github("dustinfife/flexplot", ref="development")
install.packages("ROCR")

# load packages
library(readxl)
library("dplyr")
library("flexplot")
library(ROCR)
library(ggplot2)



###################
## ROC ANALYSIS ###
###################

## FIGURE 2b ##
# read in data from an Excel file, specifying column types
stingrays_ROC <- read_excel("ROC_data.xlsx",
                     col_types = c("text","text", "numeric","numeric"))

# display the first four rows of the Raw_Score and Known_Diff columns of stingrays_ROC side by side
head(cbind(stingrays_ROC$Raw_Score, stingrays_ROC$Known_Diff),4)

# create a prediction object using the Raw_Score and Known_Diff columns of stingrays_ROC
pred <- prediction(stingrays_ROC$Raw_Score, stingrays_ROC$Known_Diff)

# determine the class of the pred object
class(pred) 

# display the slot names of the pred object
slotNames(pred)

# count the number of elements in each slot of the pred object
sn = slotNames(pred)
sapply(sn, function(x) length(slot(pred, x))) 

# display the class of each slot of the pred object
sapply(sn, function(x) class(slot(pred, x))) 

# invert the Known_Diff column of stingrays_ROC to compute the ROC curve
pred <- prediction(stingrays_ROC$Raw_Score, -stingrays_ROC$Known_Diff)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")

# plot the ROC curve, setting the frame to FALSE
plot(roc.perf, frame = FALSE)

# add a diagonal line with intercept 0 and slope 1 to the plot in red, to represent the no-discrimination line
abline(a=0, b= 1, lty=2, col ="red")

# Compute the AUC (area under the ROC curve)
auc.perf = performance(pred, measure = "auc")
auc.perf@y.values # 0.9829994


## FIGURE 2a ##

# find the optimal cut point (threshold score) for the ROC curve by weighing both sensitivity and specificity equally
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

# Print the output for the ROC curve 
print(opt.cut(roc.perf, pred)) ## this is the AUC to be added to the violin plot

# Change the "Known_Diff" column to a factor (for use in ggplot)
stingrays_ROC$Known_Diff <- as.factor(stingrays_ROC$Known_Diff)

# Create a violin plot of Raw_Score by Known_Diff with optional customization
v <- ggplot(stingrays_ROC, aes(x = Known_Diff, y = Raw_Score)) +
  geom_violin(fill = "lightgrey", colour = "black") +  
  labs(x = "", y = "Match Score") +  
  theme_minimal() +  
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.17, 
               position = position_dodge(1), colour = "white", stroke = .4) +  #
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
               geom = "pointrange", color = "red", size = 0.3) +  
  geom_hline(yintercept = 8.39, linetype = "dashed", color = "red")  # Add horizontal line at AUC value

# Display the plot
v


##############################
### SPOT PATTERN STABILITY ###
##############################

# read in data from an Excel file, specifying the column types
Spots <- read_excel("Spot_transect_data.xlsx",
                    col_types = c("text", "numeric", "numeric",
                                  "numeric", "numeric","numeric", "numeric"))

# get the column names of the Spots data frame
colnames(Spots)

# perform a Shapiro-Wilk test of normality on the Disk_Width column of Spots
shapiro.test(Spots$Disk_Width)

# perform a Shapiro-Wilk test of normality on the Mean_Diameter column of Spots
shapiro.test(Spots$Mean_Diameter)

# perform a correlation test between Mean_Diameter and Disk_Width columns of Spots
# storing the result in a variable called "cor1"
cor1 <- cor.test(Spots$Mean_Diameter, Spots$Disk_Width) 

# print out the correlation coefficient estimate from cor1
cor1$estimate 

# print out the p-value from cor1
cor1$p.value

# create linear regression showing the relationship between "Mean_Diameter" and "Disk_Width"
# standard errors represented by a shaded region around the regression line
flexplot(Mean_Diameter~Disk_Width, data=Spots, method="lm", se=TRUE)
