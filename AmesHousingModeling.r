### Regression Analysis on Ames Housing Data ----------
## Objective: Perform detailed analysis on Ames Housing Data
## to demonstrate ...

###
# Load libraries --------
###
library(ggplot2)
library(corrplot)
library(plyr)
library(GGally)
library(lattice)
library(moments)
library(dplyr)
library(tidyr)
library(purrr)
library(gridExtra)
library(kableExtra)
library(datasets)
library(forcats)
library(caret)
library(lessR)
library(calibrate)
library(knitr)
library(formattable)
library(fastDummies)
library(MASS)
library(car)
library(olsrr)
options(scipen=999)

###
# Load dataset and create useful variables ------
###

dat <- read.csv("ames_housing_data.csv", 
                   header = TRUE, sep = ",",
                   stringsAsFactors = FALSE)
str(dat)
head(dat)
names(dat)

# Useful variables
dat$TotalFloorSF <- dat$FirstFlrSF + dat$SecondFlrSF
dat$TotalSqftCalc <- dat$BsmtFinSF1 + dat$BsmtFinSF2 + dat$GrLivArea
dat$price_sqft <- dat$SalePrice/dat$TotalFloorSF
dat$HouseAge <- dat$YrSold - dat$YearBuilt
dat$QualityIndex <- dat$OverallQual * dat$OverallCond
dat$logSalePrice <- log(dat$SalePrice)

# Create target population --------

# Create waterfalls and show the number of operations dropped
waterfall1 <- dat %>%
  filter(Zoning %in% c("RH", "RL", "RP", "RM", "FV"))

waterfall2 <- waterfall1 %>%
  filter(SaleCondition == "Normal")

waterfall3 <- waterfall2 %>%
  filter(TotalFloorSF <= 4300)

waterfall_table <- tibble(
  Waterfall_Steps = c(
    "Non-Residential Zones",
    "Non-Normal Sale Condition",
    "Extremely High Square Footage"
  ),
  RowsDropped = c(
    nrow(dat) - nrow(waterfall1),
    nrow(waterfall1) - nrow(waterfall2),
    nrow(waterfall2) - nrow(waterfall3)
  ),
  RemainingObservations = c(nrow(waterfall1),
                            nrow(waterfall2),
                            nrow(waterfall3))
)

# Knit a table to display the waterfall drop conditions
kable(waterfall_table) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Subset the data to create target population
dat <- dat %>%
  filter(
    Zoning %in% c("RH", "RL", "RP", "RM", "FV"),
    SaleCondition == "Normal",
    TotalFloorSF < 4300)

# Treat Missing Values ------------

# View missing data
NAcol <- which(colSums(is.na(dat)) > 0)
sort(colSums(sapply(dat[NAcol], is.na)), decreasing = TRUE)

cat('There are', length(NAcol), 'columns with missing values.')

# If the feature with missing data is not ordinal then set back to factor

# Alley missing set to 'None' and set back to factor
dat$Alley[which(is.na(dat$Alley))] <- "None"
dat$Alley <- as.factor(dat$Alley)

# Fireplace quality missing set to 'None'
dat$FireplaceQu[is.na(dat$FireplaceQu)] <- 'None'
dat$FireplaceQu <- as.factor(dat$FireplaceQu)

# Basement features simply imputed
dat$BsmtCond[which(is.na(dat$BsmtCond))] <- "None"
dat$BsmtCond <- as.factor(dat$BsmtCond)

dat$BsmtExposure[which(is.na(dat$BsmtExposure))] <- "No"
dat$BsmtExposure <- as.factor(dat$BsmtExposure)

dat$BsmtFinSF1[which(is.na(dat$BsmtFinSF1))] <- 0
dat$BsmtFinSF2[which(is.na(dat$BsmtFinSF2))] <- 0

dat$BsmtFinType1[which(is.na(dat$BsmtFinType1))] <- "None"
dat$BsmtFinType1 <- as.factor(dat$BsmtFinType1)

dat$BsmtFinType2[which(is.na(dat$BsmtFinType2))] <- "None"
dat$BsmtFinType2 <- as.factor(dat$BsmtFinType2)

dat$BsmtFullBath[which(is.na(dat$BsmtFullBath))] <- 0
dat$BsmtHalfBath[which(is.na(dat$BsmtHalfBath))] <- 0
dat$BsmtQual[which(is.na(dat$BsmtQual))] <- "None"
dat$BsmtQual <- as.factor(dat$BsmtQual)

dat$BsmtUnfSF[which(is.na(dat$BsmtUnfSF))] <- 0
dat$TotalBsmtSF[which(is.na(dat$TotalBsmtSF))] <- 0

# Fence missing set to 'None' and then set as factor
dat$Fence[which(is.na(dat$Fence))] <- "None"
dat$Fence <- as.factor(dat$Fence)

# Lot Frontage deserves imputation due to it's importance and 
# relationship with Neighborhood
for (i in 1:nrow(dat)){
  if(is.na(dat$LotFrontage[i])){
    dat$LotFrontage[i] <- as.integer(
      median(dat$LotFrontage[dat$Neighborhood==dat$Neighborhood[i]])) 
  }
}

# A couple Lot Frontage remain missing. Set them to 0
dat$LotFrontage[which(is.na(dat$LotFrontage))] <- 0

# Garage qualities simply replaced
dat$GarageArea[which(is.na(dat$GarageArea))] <- 0
dat$GarageCars[which(is.na(dat$GarageCars))] <- 0
dat$GarageCond[which(is.na(dat$GarageCond))] <- "None"
dat$GarageCond <- as.factor(dat$GarageCond)

dat$GarageFinish[which(is.na(dat$GarageFinish))] <- "None"
dat$GarageFinish <- as.factor(dat$GarageFinish)

dat$GarageQual[which(is.na(dat$GarageQual))] <- "None"
dat$GarageQual <- as.factor(dat$GarageQual)

dat$GarageType[which(is.na(dat$GarageType))] <- "None"
dat$GarageType <- as.factor(dat$GarageType)

# Garage year built default can be the year the house was built
dat$GarageYrBlt[is.na(dat$GarageYrBlt)] <- dat$YearBuilt[is.na(dat$GarageYrBlt)]

# Mason veneer area missing set to 0
dat$MasVnrArea[which(is.na(dat$MasVnrArea))] <- 0

# Pool missing set to 'None'
dat$PoolQC[which(is.na(dat$PoolQC))] <- "None"
dat$PoolQC <- as.factor(dat$PoolQC)

# Misc Features missing set to 'None' and then set back to factor
dat$MiscFeature[is.na(dat$MiscFeature)] <- "None"
dat$MiscFeature <- as.factor(dat$MiscFeature)

cat('There are now', length(NAcol), 'columns with missing values.')

## A couple more data wrangling tasks
# Remove default ID columns
dat <- dat[,!colnames(dat) %in% c("SID", "PID")]

# Set variables that didn't have NAs back to factors
not_numeric_or_factor <- dat %>%
  select_if(negate(is.numeric)) %>%
  select_if(negate(is.factor)) %>%
  names()

# All of these are deserving to be factors except Street names
to_be_factors <- not_numeric_or_factor[not_numeric_or_factor != "Street"]

# Make these variables factors
dat[to_be_factors] <- lapply(dat[to_be_factors], factor)

# Correlation --------

# Search for highly correlated columns
highlyCor <- colnames(cor_numVar)[findCorrelation(
  cor_numVar, cutoff = 0.8, verbose = TRUE)]
highlyCor

# It doesn't make sense to remove any of these features

# Recalculate correlation
numericVars <- which(sapply(dat, is.numeric))

dat_numVar <- dat[, numericVars]
cor_numVar <- cor(dat_numVar, use="pairwise.complete.obs")

cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", 
               tl.pos = "lt", tl.cex = 0.7,
               cl.cex = .7, number.cex=.7)

# Outlier Treatment -----------

# Extreme Outliers for 3 variables
LotAreaOutliers <- boxplot.stats(dat$LotArea, coef = 3.0)$out
LotFrontageOutliers <- boxplot.stats(dat$LotFrontage, coef = 3.0)$out
GarageAreaOutliers <- boxplot.stats(dat$GarageArea, coef = 3.0)$out

sprintf("LotArea has %s outliers, 
        LotFrontage has %s outliers, 
        and GarageArea has %s outliers",
        length(LotAreaOutliers), 
        length(LotFrontageOutliers), 
        length(GarageAreaOutliers))

dat <- dat %>%
  filter(
    !LotArea %in% LotAreaOutliers,!LotFrontage %in% LotFrontageOutliers,
    !GarageArea %in% GarageAreaOutliers
  )

dat_num <- dat[sapply(dat, is.numeric)]
num_cor <- cor(dat_num, y=dat_num$SalePrice, use="na.or.complete")
num_cor

mcor <- cor(dat_num)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.5)
sprintf("There are %s remaining observations to model", nrow(dat))

# List categorical variables

factor_vars <- dat %>%
  select_if(is.factor) %>%
  names()
factor_vars

sprintf("There are %s numeric variables and %s categorical variables", 
        length(numericVars), length(factor_vars))

# Deep dive into Neighborhood, Condition1, and Exterior1

## Examine summary statistics of Neighborhood
n_mean <- aggregate(SalePrice ~ Neighborhood, data = dat, FUN = mean)
n_median <- aggregate(SalePrice ~ Neighborhood, data = dat, FUN = median)
n_sd <- aggregate(SalePrice ~ Neighborhood, data = dat, FUN = sd)
neighborhood_summary <- bind_cols(n_mean, n_median, n_sd)

neighborhood_summary <- neighborhood_summary %>%
  mutate(Mean = SalePrice,
         Median = SalePrice1,
         Std = SalePrice2) %>%
  dplyr::select(Neighborhood, Mean, Median, Std)
formattable(neighborhood_summary)

# Quick view on the R squared of Neighborhoods against the response variable
summary(lm(SalePrice ~ Neighborhood, data = dat))

# Create dummy variables for Neighborhood
dat <- dummy_cols(dat, select_columns = "Neighborhood")

## Examine summary statistics of Condition1
con_mean <- aggregate(SalePrice ~ Condition1, data = dat, FUN = mean)
con_median <- aggregate(SalePrice ~ Condition1, data = dat, FUN = median)
con_sd <- aggregate(SalePrice ~ Condition1, data = dat, FUN = sd)
condition_summary <- bind_cols(con_mean, con_median, con_sd)

condition_summary <- condition_summary %>%
  mutate(Mean = SalePrice,
         Median = SalePrice1,
         Std = SalePrice2) %>%
dplyr::select(Condition1, Mean, Median, Std)
formattable(condition_summary)

# Quick view on the R squared of Neighborhoods against the response variable
summary(lm(SalePrice ~ Condition1, data = dat))

# Not enough predictive power

## Examine summary statistics of Exterior1
ext_mean <- aggregate(SalePrice ~ Exterior1, data = dat, FUN = mean)
ext_median <- aggregate(SalePrice ~ Exterior1, data = dat, FUN = median)
ext_sd <- aggregate(SalePrice ~ Exterior1, data = dat, FUN = sd)
ext_summary <- bind_cols(ext_mean, ext_median, ext_sd)

ext_summary <- ext_summary %>%
  mutate(Mean = SalePrice,
         Median = SalePrice1,
         Std = SalePrice2) %>%
  dplyr::select(Exterior1, Mean, Median, Std)
formattable(ext_summary)

summary(lm(SalePrice ~ Exterior1, data = dat))

dat <- dummy_cols(dat, select_columns = "Exterior1")

# Train/Test split

## Split into 70/30 train/validation sets
set.seed(123)

dat$u <- runif(n = dim(dat)[1], min = 0, max = 1)
train.dat <- subset(dat, u < 0.7)
test.dat <- subset(dat, u >= 0.7)

train_test_split <- data.frame(
  "DataFrame" = c("Train", "Test"),
  "Observations" = c(nrow(train.dat), nrow(test.dat)),
  "PercentSplit" = c(nrow(train.dat) / nrow(dat), nrow(test.dat) / nrow(dat))
)
formattable(train_test_split)

## Automated variable selection ----------
# Choose 15 predictor variables

# Recalculate correlation for numerical variables
numericVars <- which(sapply(dat, is.numeric))

dat_numVar <- dat[, numericVars]
cor_numVar <- cor(dat_numVar, use="pairwise.complete.obs")

cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.3)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", 
               tl.pos = "lt", tl.cex = 0.7,
               cl.cex = .7, number.cex=.7)

train.clean <- subset(train.dat, select = c("OverallQual", 
                 "TotalSqftCalc", 
                 "GarageCars", 
                 "GarageArea", 
                 "FullBath", 
                 "YearBuilt", 
                 "QualityIndex", 
                 "price_sqft", 
                 "YearRemodel",
                 "TotRmsAbvGrd",
                 "Fireplaces",
                 "MasVnrArea",
                 "BsmtFinSF1",
                 "LotArea",
                 "OpenPorchSF",
                 "SalePrice"))
formattable(tibble(VariablePool = names(train.clean)))

upper.lm <- lm(SalePrice ~ .,data = train.clean)
summary(upper.lm)

lower.lm <- lm(SalePrice ~ 1, data = train.clean)
summary(lower.lm)

sqft.lm <- lm(SalePrice ~ TotalSqftCalc, data = train.clean)
summary(sqft.lm)

# Forward model selection
forward.lm <- stepAIC(lower.lm, 
                      scope = list(upper = formula(upper.lm), lower = ~ 1),
                      direction = "forward")
fwd_sum <- summary(forward.lm)

# Backward model selection
backward.lm <- stepAIC(upper.lm, direction = "backward")
back_sum <- summary(backward.lm)

# Stepwise model selection
stepwise.lm <- stepAIC(sqft.lm, 
                       scope = list(upper = formula(upper.lm), lower = ~ 1),
                       direction = "both")
step_sum <- summary(stepwise.lm)

# A 'junk' model for comparison
junk.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex +
                GrLivArea + TotalSqftCalc, data = train.dat)
junk_sum <- summary(junk.lm)

# Model Results (VIF, R-squared, AIC, BIC, MSE, RMSE, MAE) ---------

# Calculate VIF values from these models
sort(vif(forward.lm), decreasing=TRUE)
sort(vif(backward.lm), decreasing=TRUE)
sort(vif(stepwise.lm), decreasing=TRUE)

junk_vif <- sort(vif(junk.lm), decreasing=TRUE)

vif <- sort(vif(stepwise.lm), decreasing=TRUE)
vif <- data.frame(vif)
formattable(vif)

junk_vif <- data.frame(junk_vif)
formattable(junk_vif)


AdjRSquared <-
  c(
    fwd_sum$adj.r.squared,
    back_sum$adj.r.squared,
    step_sum$adj.r.squared,
    junk_sum$adj.r.squared
  )
AIC_list <-
  c(AIC(forward.lm),
    AIC(backward.lm),
    AIC(stepwise.lm),
    AIC(junk.lm))
BIC_list <-
  c(BIC(forward.lm),
    BIC(backward.lm),
    BIC(stepwise.lm),
    BIC(junk.lm))
MSE_list <-
  c(
    mean(forward.lm$residuals ^ 2),
    mean(backward.lm$residuals ^ 2),
    mean(stepwise.lm$residuals ^ 2),
    mean(junk.lm$residuals ^ 2)
  )
RMSE_list <-
  c(sqrt(mean(forward.lm$residuals ^ 2)), 
    sqrt(mean(backward.lm$residuals ^ 2)), 
    sqrt(mean(stepwise.lm$residuals ^ 2)), 
    sqrt(mean(junk.lm$residuals ^ 2)))
MAE_list <-
  c(mean(abs(forward.lm$residuals)), 
    mean(abs(backward.lm$residuals)), 
    mean(abs(stepwise.lm$residuals)), 
    mean(abs(junk.lm$residuals)))

comp <- data.frame(
  Model = c("Forward", "Backward", "Stepwise", "Junk"),
  AdjRSquared = AdjRSquared,
  AIC = AIC_list,
  BIC = BIC_list,
  MSE = MSE_list,
  RMSE = RMSE_list,
  MAE = MAE_list
)
formattable(comp)

# Prediction
test_forward <- predict(forward.lm, newdata = test.dat)
test_backwards <- predict(backward.lm, newdata = test.dat)
test_step <- predict(stepwise.lm, newdata = test.dat)
test_junk <- predict(junk.lm, newdata = test.dat)
test_f_mse <- mean((test.dat$SalePrice - test_forward) ^ 2)
test_b_mse <- mean((test.dat$SalePrice - test_backwards) ^ 2)
test_s_mse <- mean((test.dat$SalePrice - test_step) ^ 2)
test_j_mse <- mean((test.dat$SalePrice - test_junk) ^ 2)
test_f_mae <- mean(abs(test.dat$SalePrice - test_forward))
test_b_mae <- mean(abs(test.dat$SalePrice - test_backwards))
test_s_mae <- mean(abs(test.dat$SalePrice - test_step))
test_j_mae <- mean(abs(test.dat$SalePrice - test_junk))

test_comp <- data.frame(
  Model = c("Forward", "Backward", "Stepwise", "Junk"),
  TestMSE = c(test_f_mse, test_b_mse, test_s_mse, test_j_mse),
  TestMAE = c(test_f_mae, test_b_mae, test_s_mae, test_j_mae)
)
formattable(test_comp)

# Validation --------

# Using forward selection model, calculate absolute percent error
forward_pct <- abs(forward.lm$residuals) / train.clean$SalePrice

# Assign Prediction Grades
forward_PredictionGrade <-
  ifelse(
    forward_pct <= 0.10,
    'Grade 1: [0.0.10]',
    ifelse(
      forward_pct <= 0.15,
      'Grade 2: (0.10,0.15]',
      ifelse(forward_pct <= 0.25, 'Grade 3: (0.15,0.25]',
             'Grade 4: (0.25+]')
    )
  )
forward_trainTable <- table(forward_PredictionGrade)
forward_trainTable / sum(forward_trainTable)

# Using junk model for comparison, calculate absolute percent error
junk_pct <- abs(junk.lm$residuals) / train.clean$SalePrice
# Assign Prediction Grades
junk_PredictionGrade <-
  ifelse(
    junk_pct <= 0.10,
    'Grade 1: [0.0.10]',
    ifelse(
      junk_pct <= 0.15,
      'Grade 2: (0.10,0.15]',
      ifelse(junk_pct <= 0.25, 'Grade 3: (0.15,0.25]',
             'Grade 4: (0.25+]')
    )
  )
junk_trainTable <- table(junk_PredictionGrade)
junk_trainTable / sum(junk_trainTable)

# Absolute percent error of test set
forward_testPCT <-
  abs(test.dat$SalePrice - test_forward) / test.dat$SalePrice
backward_testPCT <-
  abs(test.dat$SalePrice - test_backwards) / test.dat$SalePrice
stepwise_testPCT <-
  abs(test.dat$SalePrice - test_step) / test.dat$SalePrice
junk_testPCT <- 
  abs(test.dat$SalePrice - test_junk) / test.dat$SalePrice

# Assign Prediction Grades;
forward_testPredictionGrade <-
  ifelse(
    forward_testPCT <= 0.10,
    'Grade 1: [0.0.10]',
    ifelse(
      forward_testPCT <= 0.15,
      'Grade 2: (0.10,0.15]',
      ifelse(
        forward_testPCT <= 0.25,
        'Grade 3: (0.15,0.25]',
        'Grade 4: (0.25+]'
      )
    )
  )
forward_testTable <- table(forward_testPredictionGrade)
forward_testTable / sum(forward_testTable)

# Assign Prediction Grades
junk_testPredictionGrade <-
  ifelse(
    junk_testPCT <= 0.10,
    'Grade 1: [0.0.10]',
    ifelse(
      junk_testPCT <= 0.15,
      'Grade 2: (0.10,0.15]',
      ifelse(
        junk_testPCT <= 0.25,
        'Grade 3: (0.15,0.25]',
        'Grade 4: (0.25+]'
      )
    )
  )
junk_testTable <- table(junk_testPredictionGrade)
junk_testTable / sum(junk_testTable)

# Reduce model complexity -----------------

reduced1 <- lm(SalePrice ~ OverallQual + TotalSqftCalc + GarageArea + 
                 FullBath + YearBuilt + price_sqft + YearRemodel +
                 TotRmsAbvGrd + Fireplaces + MasVnrArea + BsmtFinSF1 +
                 LotArea + OpenPorchSF, data = dat)
summary(reduced1)

train.clean %>%
  gather(-BsmtFinSF1, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = BsmtFinSF1)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

reduced2 <- lm(SalePrice ~ OverallQual + TotalSqftCalc + GarageArea +
                 FullBath + YearBuilt + price_sqft + YearRemodel +
                 TotRmsAbvGrd + Fireplaces + MasVnrArea + LotArea + 
                 OpenPorchSF, data = dat)
summary(reduced2)
desc2 <- "Removed BsmtFinSF1"
coeff2 <- nrow(summary(reduced2)$coefficients) - 1
adjr2 <- summary(reduced2)$adj.r.squared

# Remove YearBuilt
reduced3 <- lm(SalePrice ~ OverallQual + TotalSqftCalc + GarageArea + FullBath +
                 price_sqft + YearRemodel + TotRmsAbvGrd + Fireplaces +
                 MasVnrArea + LotArea + OpenPorchSF, data = dat)
summary(reduced3)

desc3 <- "Removed YearBuilt"
coeff3 <- nrow(summary(reduced3)$coefficients) - 1
adjr3 <- summary(reduced3)$adj.r.squared

# Remove OpenPorchSF
reduced4 <- lm(SalePrice ~ OverallQual + TotalSqftCalc + GarageArea + FullBath +
                 price_sqft + YearRemodel + TotRmsAbvGrd + Fireplaces +
                 MasVnrArea + LotArea, data = dat)
summary(reduced4)

desc4 <- "Removed OpenPorchSF"
coeff4 <- nrow(summary(reduced4)$coefficients) - 1
adjr4 <- summary(reduced4)$adj.r.squared

# Remove GarageArea
reduced5 <- lm(SalePrice ~ OverallQual + TotalSqftCalc + FullBath +
                 price_sqft + YearRemodel + TotRmsAbvGrd + Fireplaces +
                 MasVnrArea + LotArea, data = dat)
summary(reduced5)

desc5 <- "Removed GarageArea"
coeff5 <- nrow(summary(reduced5)$coefficients) - 1
adjr5 <- summary(reduced5)$adj.r.squared

# Remove Fireplaces
reduced6 <- lm(SalePrice ~ OverallQual + TotalSqftCalc + FullBath +
                 price_sqft + YearRemodel + TotRmsAbvGrd +
                 MasVnrArea + LotArea, data = dat)
summary(reduced6)

desc6 <- "Removed Fireplaces"
coeff6 <- nrow(summary(reduced6)$coefficients) - 1
adjr6 <- summary(reduced6)$adj.r.squared

# Remove YearRemodel
reduced7 <- lm(SalePrice ~ OverallQual + TotalSqftCalc + FullBath +
                 price_sqft + TotRmsAbvGrd +
                 MasVnrArea + LotArea, data = dat)
summary(reduced7)

desc7 <- "Removed YearRemodel"
coeff7 <- nrow(summary(reduced7)$coefficients) - 1
adjr7 <- summary(reduced7)$adj.r.squared

# Remove LotArea
reduced8 <- lm(SalePrice ~ OverallQual + TotalSqftCalc + FullBath +
                 price_sqft + TotRmsAbvGrd +
                 MasVnrArea, data = dat)
summary(reduced8)

desc8 <- "Removed LotArea"
coeff8 <- nrow(summary(reduced8)$coefficients) - 1
adjr8 <- summary(reduced8)$adj.r.squared

# Remove MasVnrArea
reduced9 <- lm(SalePrice ~ OverallQual + TotalSqftCalc + FullBath +
                 price_sqft + TotRmsAbvGrd, data = dat)
summary(reduced9)

desc9 <- "Removed MasVnrArea"
coeff9 <- nrow(summary(reduced9)$coefficients) - 1
adjr9 <- summary(reduced9)$adj.r.squared

# Remove TotRmsAbvGrd
reduced10 <- lm(SalePrice ~ OverallQual + TotalSqftCalc + FullBath +
                 price_sqft, data = dat)
summary(reduced10)

desc10 <- "Removed TotRmsAbvGrd"
coeff10 <- nrow(summary(reduced10)$coefficients) - 1
adjr10 <- summary(reduced10)$adj.r.squared


# Display results
descriptions <- c(desc2, desc3, desc4, desc5, desc6,
                  desc7, desc8, desc9, desc10)
coef_count <- c(coeff2, coeff3, coeff4, coeff5, coeff6,
                coeff7, coeff8, coeff9, coeff10)
adjrs <- c(adjr2, adjr3, adjr4, adjr5, adjr6, 
           adjr7, adjr8, adjr9, adjr10)
removal_sum <- cbind.data.frame(descriptions, coef_count, adjrs)

formattable(removal_sum)

# Diagnostic analysis -----------

# Using Reduced9
enhanced_hist <- function(x, title_alias) {
  par(mfrow=c(1, 2))
  skew <- round(moments::skewness(x), digits = 3)
  kurtosis <- round(moments::kurtosis(x), digits = 3)
  
  #Histogram
  hist(
    x = x,
    main = paste("Histogram of", title_alias),
    xlab = title_alias,
    col = "purple"
  )
  legend("topright",
         legend = paste("kurtosis =", kurtosis, "&", "skew =", skew))
  
  # Boxplot
  boxplot(
    x = x,
    main = paste("Boxplot of", title_alias),
    xlab = title_alias,
    col = "purple",
    outcol = "red"
  )
}
par(mfrow = c(2,2))
plot(reduced9)

enhanced_hist(reduced9$residuals, title_alias = "Final Residuals")

regression_diagnostics <-
  function(model,
           cooks_threshold = 1,
           leverage_threshold = 2) {
    # Cooks Distance
    cooks_dist <- cooks.distance(model)
    potential_outliers <- cooks_dist[cooks_dist > cooks_threshold]
    if (length(potential_outliers) == 0) {
      cooks_outliers <- 0
    } else {
      cooks_outliers <- potential_outliers
    }
    
    # Leverage
    hat_vals <- hatvalues(model)
    k <- length(model$coefficients) - 1
    n <- length(model$residuals)
    hat_outliers <-
      hat_vals[hat_vals > ((leverage_threshold * (k + 1)) / n)]
    if (length(hat_outliers) == 0) {
      hat_out <- 0
    } else {
      hat_out <- hat_outliers
    }
    return(list(
      CooksDistanceOutliers = cooks_outliers,
      LeverageOutliers = hat_out
    ))
  }
reg_diag_print <- function(rd) {
  if (sum(rd$CooksDistanceOutliers) == 0) {
    print("There are no outliers based on Cook's distance.")
  } else {
    print(paste("There are", length(rd$CooksDistanceOutliers), "potential outliers based on Cook's distance."))
  }
  if (sum(rd$LeverageOutliers) == 0) {
    print("There are no leverage outliers.")
  } else {
    print(paste("There are", length(rd$LeverageOutlier), "potential leverage outliers."))
  }
}
reg9_diag <- regression_diagnostics(reduced9)
reg_diag_print(reg9_diag)

dfits_thresh <- (2 * sqrt(length(reduced9$coefficients))) / length(reduced9$residuals)

plot(dffits(reduced9), 
     ylab = "Standardized DFITS", xlab = "Index", 
     main = paste("Standardized DfFits, \n critical value = 0.005 = +/-", round(dfits_thresh,3)))
abline(h = dfits_thresh, lty = 2,col="red")
abline(h = -dfits_thresh, lty = 2, col="red")
textxy(as.numeric(names(dffits(model_4)[which(dffits(model_4) < -dfits_thresh | dffits(model_4) > dfits_thresh)])), 
       dffits(model_4)[which(dffits(model_4) < -dfits_thresh | dffits(model_4) > dfits_thresh)], 
       as.numeric(names(dffits(model_4)[which(dffits(model_4) < -dfits_thresh | dffits(model_4) > dfits_thresh)])), cex=0.7,offset = -1)

# Another view of dffits
ols_plot_dffits(reduced9)
