#logistic regression on dust data
#Set the working directory below
#setwd("[pathname]")

# Load the readxl package
install.packages('readxl')
install.packages('ggplot2')
library(readxl)

# Read in the Excel table##DUST
data <- read_excel("Table2_03.02LATEST.xlsx", sheet="DUST")
Noff<-14 #number of offices
Res<-Results(data,Noff)
#Res #to display plot
##AIR 0h
#data <- read_excel("Table2_03.02LATEST.xlsx", sheet="AIR 0 h")
#Noff<-12 #number of offices
#Res<-Results(data,Noff)

#Function
Results <-function(data,Noff){
PrSuccess<-data.frame(Nocc=numeric(),Nsamp=numeric(),Suc=numeric(), Fail=numeric())
Nocc<-as.numeric(data$'N occupants'[1:Noff])
Nsamples<-as.numeric(data$'N samples'[1:Noff])
a=0
for(i in 1:Noff){
# extract P1,P2 etc
  p_values <- na.omit(data[i, grep("^P\\d+$", colnames(data))]) 
  p_values <- as.numeric(p_values[1:Nocc[i]])
for(x in 1:Nocc[i]){ # to avoid NAs
PrSuccess[a+1,1]<-Nocc[i]
PrSuccess[a+1,2]<-Nsamples[i]
PrSuccess[a+1,3]<-(p_values[x])
PrSuccess[a+1,4]<-(Nsamples[i]-p_values[x])
a=a+1
}
}
PrSuccess$Pr<-PrSuccess$Suc/PrSuccess$Nsamp

#######Prepare binary dataframe
logdata<-data.frame(Nocc=numeric(),SucF=numeric())
a=0
for(i in 1:nrow(PrSuccess)){
for(z in 1:(PrSuccess$Suc[i])){
a=a+1
#No of occupants
logdata[a,1]<-PrSuccess[i,1]
#Successes recorded as 1
logdata[a,2]<-1
}
###record number of fails
for(z in 1:(PrSuccess$Fail[i])){
a=a+1
#No of occupants
logdata[a,1]<-PrSuccess[i,1]
#Fails recorded as 0
logdata[a,2]<-0
}
}
# Fit a logistic regression model using the data
model <- glm(SucF~Nocc, data = logdata, family = binomial)

# Load the required library for plotting
library(ggplot2)

# Fit the logistic regression model
model <- glm(SucF ~ Nocc, data = logdata, family = binomial)

# Generate predicted probabilities for the logistic regression
logdata$pred_prob <- predict(model, type = "response")

# Plot the logistic regression
my_plot<-ggplot(logdata, aes(x = Nocc, y = pred_prob)) + 
  geom_point(aes(y = SucF), position = position_jitter(width = 0.1, height = 0.05)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, fullrange=TRUE) +
  xlab("Number of occupants") +
  ylab("Probability of success")+
theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))+
scale_x_continuous(breaks = seq(1, 12), limits = c(1, 12)) +
coord_cartesian(xlim = c(1, 12))
#Find ceofficients
intercept <- coef(model)[1]
slope <- coef(model)[2]

print(my_plot)
# Calculate Pr of success for a given NOC
Nocc=30
probability_of_success = exp(intercept + slope * Nocc) / (1 + exp(intercept + slope * Nocc))
## Calculate the residual deviance
residual_deviance <- deviance(model)
## Calculate the variance from df

# Calculate the degrees of freedom
df <- df.residual(model)

# Calculate the variance
variance <- residual_deviance / df
#Collect results
return(list(intercept=intercept, slope=slope, model=model, my_plot=my_plot))
}
###############################################
########################################################
#END HERE
