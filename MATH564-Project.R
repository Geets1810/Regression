                      ################Installing Relevant packages###################
#install.packages("dplyr")
#install.packages("Hmisc")
#install.packages("tidyr")
#install.packages("tidyquant")
#install.packages("zoo")
#install.packages("ggplot2")
#install.packages("tseries")
                      ################Include Relevant libraries#####################
#library(dplyr)
#library(Hmisc)
#library(tidyr)
#library(tidyverse)
#library(tidyquant)
#library(zoo)
#library(ggplot2)
#library(car)
##############################################Read Excel for data#############################################################
#This dataset is Istanbul Stock Exchange, with 7 other international markets
##############################################################################################################################
library(readxl)
data_akbilgic_2_ <- read_excel("C:/Users/Vanathi/Desktop/data_akbilgic_2_.xlsx")
View(data_akbilgic_2_)
plot(data_akbilgic_2_)
######################################OMIT columns "NA" from dataset#################################################################################
data_akbilgic_2_<-na.omit(data_akbilgic_2_)
###########################################Linear regression model##################################################################################
df1<-select(data_akbilgic_2_,-date)
reg1 <- lm(ISE2 ~ .,data=df1)
q<-prediction(reg1)
plot(reg1)
plot(q)
summary(q)
summary(reg1)
###########################################Variance inflation factor#############################################################################
vif(reg1)
bptest(reg1)
##################################Forward Selection##########################################################################################
reg2 <-lm(ISE2~1,data=data_akbilgic_2_)
new<-stepAIC(reg2, scope=list(lower=reg2, upper=reg1), direction="forward")
######################################Backward Selection#######################################################################################
drop1(reg1, test = "F")
drop1(update(reg1, ~ . -date), test = "F")
drop1(update(reg1, ~ . -date-SP), test = "F")
drop1(update(reg1, ~ . -date-SP-DAX), test = "F")
drop1(update(reg1, ~ . -date-SP-DAX-FTSE), test = "F")
drop1(update(reg1, ~ . -date-SP-DAX-FTSE-EU), test = "F")
drop1(update(reg1, ~ . -date-ISE-SP-DAX-FTSE-NIKKEI), test = "F")
#############################################################################################################################
outlierTest(reg3) # Bonferonni p-value for most extreme obs
qqPlot(reg3, main="QQ Plot") #qq plot for studentized resid
leveragePlots(reg3) # leverage plots
###########################################################################################################################
df2<-select(df1,ISE2,EU,EM,BOVESPA)
reg3<-lm(ISE2~.,data=df2)
summary(reg3)
###########################################################################################################################
# distribution of studentized residuals
library(MASS)
sresid <- studres(reg3)
hist(sresid, freq=FALSE, 
     main="Distribution of Standerdized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
##########################################################################################################################
# Normality of Residuals
# qq plot for studentized resid
qqPlot(reg3, main="QQ Plot")
ncvTest(reg3)
spreadLevelPlot(reg3)
##########################################################################################################################
## =========================================
#             CROSS-VALIDATION
# =========================================
# 10-fold Cross-Validation will be implemented here with best subset selection. 
# We will generate 10 folds of training and validation sets. Across each fold, we will perform
# best subset selection. In the end, a 10x7 matrix of validation error scores (predictions on validation set)
# will be generated. This matrix is averaged column wise to get average validation scores
# across models with 1 predictor, 2 predictors ..... 7 predictors.
# Column with least mean validation error score is chosen for our final model. Note that selecting
# least mean validation error score only gives us the number of predictors that need to be in our
# final model but not the model itself. Once we have the count of predictors that need to 
# go into our final model, we perform best subset selection across models with that many 
# number of predictors

k = 10
set.seed(12345)
val_errors_matrix = matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

# generate data folds for validation process
folds = sample(1:k, 
               nrow(ise_df_train),
               replace = TRUE)

# outer loop iterated across folds
for(fold in 1:k)
{
  # create training and validation data
  train_data = ise_df_train[folds != fold, ]
  validation_data = ise_df_train[folds == fold, ]
  
  # perform best subset selection (training) on corresponding fold -- TRAIN phase
  # This step outputs best model for 1,2.....k predictors
  best_fit = regsubsets(USD_BASED_ISE ~ .,
                        data = train_data,
                        nvmax = 7)
  
  # VALIDATION phase
  # Use best model identified from above and evaluate CV score
  for(nPred in 1:7)
  {
    # 'regsubsets' doesn't have 'predict' method
    # we need to write our own prediction and error calculation steps
    betas = coef(best_fit, nPred)
    
    # create 'X_valid' by adding column of 1s. Let's call the column '(Intercept)' for future use
    X_valid = validation_data[,c('SP','DAX','FTSE','NIKKEI','BOVESPA','EU','EM')]
    X_valid['(Intercept)'] = 1
    X_valid = X_valid[,c('(Intercept)', 'SP', 'DAX', 'FTSE', 'NIKKEI', 'BOVESPA', 'EU', 'EM')]
    X_valid = X_valid[,c(names(betas))]
    
    # create 'y_test'
    y_valid = validation_data$USD_BASED_ISE
    
    # perform (beta * X) matrix multiplication
    y_pred = as.matrix(X_valid) %*% as.matrix(betas)
    
    # add validation error scores (MSE) to error matrix 
    val_errors_matrix[fold, nPred] = mean((y_valid-y_pred)^2)
  }  
}

# avg. validation errors column-wise and choose the index with min. avg value
mean_val_errors = apply(val_errors_matrix, 2, mean) 
plot(mean_val_errors, type = 'b', xlab = 'Number of predictors', ylab = 'Mean Error')

# Looks like number of predictors = 3 has lowest validation error score
# Select best model among all possible combinations of 3 predictor variables
best_model = regsubsets(USD_BASED_ISE ~ ., data = ise_df_train, nvmax = 7)
best_model_coef = coef(best_model, 3)
print(best_model_coef)

# THE 3 PRECICTOR VARIABLE MODEL OUTPUT BY BEST-SUBSET SELECTION CONTAINS PREDICTORS 'BOVESPA', 'EU', 'EM'

# Prediction on test set
test = ise_df_test[ , c('BOVESPA', 'EU', 'EM')]
test['(Intercept)'] = 1
test = test[ , c('(Intercept)', 'BOVESPA', 'EU', 'EM')]

y_test = ise_df_test$USD_BASED_ISE
y_pred = as.matrix(test) %*% best_model_coef

# R^2 score
test_rss = sum((y_test - y_pred)^2)
test_tss = sum((y_test - mean(y_test))^2)
test_r2 = 1 - (test_rss/test_tss)
print(test_r2)  

# adjusted R^2 score
numerator = (1-test_r2) * (dim(ise_df_test)[1] - 1)
denominator = (dim(ise_df_test)[1] - 3 - 1)
test_adj_r2 = 1 - (numerator/denominator)
print(test_adj_r2)
#
##########################################################################################################################
df2$predicted <- predict(reg3)
df2$residuals <- residuals(reg3)
df2 %>% 
gather(key = "iv", value = "x", -ISE2, -predicted, -residuals) %>%  # Get data into shape
ggplot(aes(x = x, y = ISE2)) +  # Note use of `x` here and next line
geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
geom_point(aes(color = residuals)) +
scale_color_gradient2(low = "blue", mid = "white", high = "red") +
guides(color = FALSE) +
geom_point(aes(y = predicted), shape = 1) +
facet_grid(~ iv, scales = "free_x") +  # Split panels here by `iv`
theme_bw()
##########################################################################################################################

