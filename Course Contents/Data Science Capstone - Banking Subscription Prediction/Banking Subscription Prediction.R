# Choose Your Own Project
# HarvardX: Capstone 
# Predicting Subscription To Term Deposit Using The Bank Marketing Data Set
# Author : Dwaipayan Chatterjee
# Date: May 2019


####################################    
######## Data Processing ###########
####################################
        
        
##########################
## Installing Libraries ##  
##########################

if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(mlr)) install.packages("mlr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(doParallel)) install.packages("doParallel", repos = "http://cran.us.r-project.org")
if(!require(class)) install.packages("class", repos = "http://cran.us.r-project.org")
if(!require(gmodels)) install.packages("gmodels", repos = "http://cran.us.r-project.org")
if(!require(TSA)) install.packages("TSA", repos = "http://cran.us.r-project.org")
if(!require(FitAR)) install.packages("FitAR", repos = "http://cran.us.r-project.org")
if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
if(!require(FNN)) install.packages("FNN", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

#######################
## Loading Libraries ##  
#######################

library(readr)
library(knitr)
library(mlr)
library(ggplot2)
library(magrittr)
library(cowplot)
library(dplyr)
library(gridExtra)
library(GGally)
library(stringr)    
library(glmnet)     
library(doParallel)
library(class)
library(gmodels)
library(TSA)
library(FitAR)
library(car)
library(FNN)       
library(reshape2) 
library(e1071)
library(caret)
library(psych)
library(dplyr)

######################
## Loading Data Set ##  
######################

# Loading Data
bank <- read.csv("bank1.csv", header = TRUE, stringsAsFactors = FALSE) 

# Seeing How Data Looks
head(bank) 

######################################
## Data Cleaning and Transformation ##  
######################################

#seeing features
str(bank) 

#Sumarizing Data By Feature
summarizeColumns(bank) %>% knitr::kable( caption = "Feature Summary of Bank Data") #Sumarizing Data By Feature

#Scanning for NAs
colSums(is.na(bank))

#Scanning for White Space
bank[, sapply( bank, is.character )] <- sapply( bank[, sapply( bank, is.character )], trimws)

#Scanning for Case Errors
unique(bank$job)
unique(bank$marital)
unique(bank$education)
unique(bank$default)
unique(bank$housing)
unique(bank$loan)
unique(bank$poutcome)

# Renaming Some Variables's Values
bank$default <-ifelse(bank$default =="yes", "defaulter","no defaulter")
bank$housing <-ifelse(bank$housing =="yes", "housing loan","no housing loan")
bank$loan <-ifelse(bank$loan =="yes", "personal loan","no personal loan")

##########################################    
######## Exploratory Data Aalysis ########
##########################################

  ##############################
#### Univariate Visualization ##  
  ##############################

    ###############################
###### Categorical Visualization ##  
    ###############################

# Visualization Job
job_sum <- bank%>% group_by(job) %>% summarise(count = n())
job_sum$job <- job_sum$job %>% factor(levels = job_sum$job[order(-job_sum$count)]) 
ggplot(job_sum,aes(x = job, y = count)) + geom_bar(stat="identity") 
+ theme(axis.text.x=element_text(angle=45,hjust=1)) + labs(title = "Figure 1 - Job")

# Visualization Marital Status
marital_sum <- bank%>% group_by(marital) %>% summarise(count = n())
marital_sum$marital <- marital_sum$marital %>% factor(levels = marital_sum$marital[order(-marital_sum$count)]) 
ggplot(marital_sum,aes(x = marital, y = count)) + geom_bar(stat="identity") + labs(title = "Figure 2 - Marital Status")

# Visualization Education
education_sum <- bank%>% group_by(education) %>% summarise(count = n())
education_sum$education <- education_sum$education %>% factor(levels = education_sum$education[order(-education_sum$count)]) 
ggplot(education_sum,aes(x = education, y = count)) + geom_bar(stat="identity") + labs(title = "Figure 3 - Education")


# Visualization Default Credit
default_sum <- bank%>% group_by(default) %>% summarise(count = n())
default_sum$default <- default_sum$default %>% factor(levels = default_sum$default[order(-default_sum$count)]) 
ggplot(default_sum,aes(x = default, y = count)) + geom_bar(stat="identity") + labs(title = "Figure 4 - Default Credit")

# Visualization Housing Loan
housing_sum <- bank%>% group_by(housing) %>% summarise(count = n())
housing_sum$housing <- housing_sum$housing %>% factor(levels = housing_sum$housing[order(-housing_sum$count)]) 
ggplot(housing_sum,aes(x = housing, y = count)) + geom_bar(stat="identity") + labs(title = "Figure 5 - Housing Loan")

# Visualization Personal Loan
loan_sum <- bank%>% group_by(loan) %>% summarise(count = n())
loan_sum$loan <- factor(c(loan_sum$loan), levels = c("personal loan", "no personal loan"), ordered = TRUE)
ggplot(loan_sum,aes(x = loan, y = count)) + geom_bar(stat="identity") + labs(title = "Figure 6 - Personal loan")

# Visualization Previous Outcome
poutcome_sum <- bank%>% group_by(poutcome) %>% summarise(count = n())
poutcome_sum$poutcome <- poutcome_sum$poutcome %>% factor(levels = poutcome_sum$poutcome[order(-poutcome_sum$count)]) 
ggplot(poutcome_sum,aes(x = poutcome, y = count)) + geom_bar(stat="identity") + labs(title = "Figure 7 - Previous Outcome")

# Visualization Target Y
y_sum <- bank%>% group_by(y) %>% summarise(count = n())
y_sum$y <- y_sum$y %>% factor(levels = y_sum$y[order(-y_sum$count)]) 
ggplot(y_sum,aes(x = y, y = count)) + geom_bar(stat="identity") + labs(title = "Figure 8 - Target Y")

    #############################
###### Numerical Visualization ##  
    #############################

# Visualization Age
p <- ggplot(bank, aes(x = factor(1), y = age)) +   geom_boxplot(width = .50)
p1 <- ggplot(bank, aes(x = age)) +
  geom_density(fill = "orange", alpha = .2) +
  geom_histogram(colour="white",aes(y=..density..),alpha = 1/2) +
  geom_vline(xintercept= median(bank$age)) +
  annotate("text",label = "Median",x = 39, y = 0.045) +
  geom_vline(xintercept= mean(bank$age),linetype=2) +
  annotate("text",label = "Mean",x = 41, y = 0.05) + labs(title = "Figure 9 - Age")
plot_grid(p1, p + coord_flip() + theme(axis.title.y=element_blank(), 
                                       axis.text.y=element_blank(),
                                       axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1)) 

# Visualization Yearly Balance
p_balance <- ggplot(bank, aes(x = factor(1), y = balance)) +geom_boxplot(width = .50)

p1_balance <- ggplot(bank, aes(x = balance)) + labs(title = "Figure 10 - Average Yearly Balance") + geom_histogram(colour="white",aes(y=..count..), bins = 10)

plot_grid(p1_balance, p_balance + coord_flip() + theme(axis.title.y=element_blank(), 
                                                       axis.text.y=element_blank(),
                                                       axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1))

# Visualization Duration Of Last Call
p_duration <- ggplot(bank, aes(x = factor(1), y = duration)) +   geom_boxplot(width = .50)
p1_duration <- ggplot(bank, aes(x = duration)) +
  labs(title = "Figure 11 - Duration of last call") + geom_histogram(colour ="white", aes(y=..count..),alpha = 1/2)
plot_grid(p1_duration, p_duration + coord_flip() + theme(axis.title.y=element_blank(),                                                        axis.text.y=element_blank(),
                                                         axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1)) 

# Visualization Campaign
p_campaign <- ggplot(bank, aes(x = factor(1), y = campaign )) + labs(y ="Number of contact") + geom_boxplot(width = .50)
p1_campaign <- ggplot(bank, aes(x = campaign)) + labs(title = "Figure 12 - Campaign", x ="Number of contact") + geom_histogram(colour ="white", aes(y=..count..),alpha = 1/2)
plot_grid(p1_campaign, p_campaign + coord_flip() + theme(axis.title.y=element_blank(),                                                          axis.text.y=element_blank(),
                                                         axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1)) 

# Visualization Number of Past Days After Client Was Last Contacted
p_pdays <- ggplot(bank, aes(x = factor(1), y = pdays)) +  labs(y ="Number of days passed") + geom_boxplot(width = .50)
p1_pdays <- ggplot(bank, aes(x = pdays)) +
  labs (title = "Figure 13 - Number of days passed after the client was last contacted",x ="Number of days passed") + geom_histogram(colour ="white", aes(y=..count..),alpha = 1/2)
plot_grid(p1_pdays, p_pdays + coord_flip() + theme(axis.title.y=element_blank(),                                                          axis.text.y=element_blank(),
                                                   axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1)) 

# Visualization Number of contacts performed before this campaign
p_previous <- ggplot(bank, aes(x = factor(1), y = previous)) + labs(y = "Number of contacts") + geom_boxplot(width = .50)
p1_previous <- ggplot(bank, aes(x = previous)) +
  labs(title = "Figure 14 - Number of contacts performed before this campaign
", x= "Number of contacts") + geom_histogram(colour ="white", aes(y=..count..),alpha = 1/2)
plot_grid(p1_previous, p_previous + coord_flip() + theme(axis.title.y=element_blank(), 
                                                         axis.text.y=element_blank(),
                                                         axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1))


  ################################
#### Multivariate Visualization ##  
  ################################

#Factorise the target feature y
bank$y <- factor(c(bank$y), levels = c("yes","no"), ordered = TRUE)

#Divide the target feature into two seperate subsets with Yes and No
bank_yes <- bank %>% filter(bank$y =="yes")
bank_no <- bank %>% filter(bank$y =="no")

    #############################################
###### Numeric Features Segregated by Target Y ##  
    #############################################

# Visualization of Age and Target Y
ggplot(data=bank, aes(age, fill =y)) + geom_histogram(aes(y=..count..)) + facet_grid(~y)

# Visualization of Balance and Target Y
balance_yes <- ggplot(bank_yes,aes(balance)) + geom_histogram(binwidth = 10) + labs(title = "Term Deposits Yes by Balance", x="Balance", y="Count") + xlim(c(0,2800)) + ylim(c(0,1000))
balance_no <- ggplot(bank_no,aes(balance)) + geom_histogram(binwidth = 10) + labs(title = "Term Deposits No by Balance", x="Balance", y="Count") + xlim(c(0,2800)) + ylim(c(0,1000))
grid.arrange(balance_yes,balance_no)

# Visualization of Duration and Target Y
balance_yes <- ggplot(bank_yes,aes(duration)) + geom_histogram(binwidth = 10) + labs(title = "Term Deposits Yes by Duration", x="Duration", y="Count") + xlim(c(0,2800))
balance_no <- ggplot(bank_no,aes(duration)) + geom_histogram(binwidth = 10) + labs(title = "Term Deposits No by Duration", x="Duration", y="Count") + xlim(c(0,2800))
grid.arrange(balance_yes,balance_no)

    #################################################
###### Categorical Features Segregated by Target Y ##  
    #################################################


# Visualization of Job and Target Y
bank_yes_job <-bank_yes%>% group_by(job) %>% summarise(count = n())
bank_yes_job$y <- c(strrep("yes",1))
bank_no_job <-bank_no%>% group_by(job) %>% summarise(count = n())
bank_no_job$y <- c(strrep("no",1))
bank_job <- rbind(bank_yes_job, bank_no_job)
bank_job$y <- factor(c(bank_job$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_job,aes(x = job, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + theme(axis.text.x=element_text(angle=45,hjust=1)) 

# Visualization of Marital Status and Target Y
bank_yes_marital <- bank_yes%>% group_by(marital) %>% summarise(count = n())
bank_no_marital <- bank_no  %>% group_by(marital) %>% summarise(count = n())
bank_yes_marital$y <-c(strrep("yes",1))
bank_no_marital$y <-c(strrep("no",1))
bank_marital <- rbind(bank_yes_marital,bank_no_marital)
bank_marital$y <- factor(c(bank_marital$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_marital,aes(x = marital, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + facet_grid(~y)

# Visualization of Education and Target Y
bank_yes_education <-bank_yes%>% group_by(education) %>% summarise(count = n())
bank_no_education <-bank_no%>% group_by(education) %>% summarise(count = n())
bank_yes_education$y <- c(strrep("yes",1))
bank_no_education$y <- c(strrep("no",1))
bank_education <- rbind(bank_yes_education, bank_no_education)
bank_education$y <- factor(c(bank_education$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_education,aes(x = education, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + facet_grid(~y) + theme(axis.text.x=element_text(angle=45,hjust=1))


# Visualization of Default and Target Y
bank_yes_default <-bank_yes%>% group_by(default) %>% summarise(count = n())
bank_no_default <-bank_no%>% group_by(default) %>% summarise(count = n())
bank_yes_default$y <- c(strrep("yes",1))
bank_no_default$y <- c(strrep("no",1))
bank_default <- rbind(bank_yes_default, bank_no_default)
bank_default$y <- factor(c(bank_default$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_default,aes(x = default, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + facet_grid(~y)

# Visualization of Housing and Target Y
bank_yes_housing <-bank_yes%>% group_by(housing) %>% summarise(count = n())
bank_no_housing <-bank_no%>% group_by(housing) %>% summarise(count = n())
bank_yes_housing$y <- c(strrep("yes",1))
bank_no_housing$y <- c(strrep("no",1))
bank_housing <- rbind(bank_yes_housing, bank_no_housing)
bank_housing$y <- factor(c(bank_housing$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_housing,aes(x = housing, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + facet_grid(~y)

# Visualization of Loan and Target Y
bank_yes_loan <-bank_yes%>% group_by(loan) %>% summarise(count = n())
bank_no_loan <-bank_no%>% group_by(loan) %>% summarise(count = n())
bank_yes_loan$y <- c(strrep("yes",1))
bank_no_loan$y <- c(strrep("no",1))
bank_loan <- rbind(bank_yes_loan, bank_no_loan)
bank_loan$y <- factor(c(bank_loan$y), levels = c("yes","no"), ordered = TRUE)
ggplot(bank_loan,aes(x = loan, y = count, fill = y)) + geom_bar(stat="identity", position ="dodge") + facet_grid(~y)

    ##########################################################
###### Interaction between Categorical and Numeric Features ##  
    ##########################################################

# Visualization of Balance VS Default
ggplot(data = bank, aes(x=default, y = balance, fill = y)) + geom_boxplot(width = .50) +scale_fill_manual(values = c("red","blue"))

# Visualization of Duration VS Housing
ggplot(data = bank, aes(x=housing, y = duration, fill = y)) + geom_boxplot(width= .50) +scale_fill_manual(values = c("red","blue"))

# Visualization of Duration VS Loan
ggplot(data = bank, aes(x=loan, y = duration, fill = y)) + geom_boxplot(width= .50) +scale_fill_manual(values = c("red","blue"))

    ###########################################################
###### Scatter plot matrix of Numerical Features by Target Y ##  
    ###########################################################

# Visualization of age vs balance vs contact vs day vs month
data <- bank %>% select(age, balance, contact, day, month)
pairs.panels(data, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

################################################    
######## Model Building and Predictions ########
################################################

   ################################
#### Logistic Regression Model ####  
   ################################

# Since we're going to split our data we need to ensure the split is repeatable.
set.seed(45)

# Loading Data Again
bank <- read.csv("bank1.csv", header = TRUE, stringsAsFactors = FALSE)

# This code of chunks create extra column for variables with unknown values
bank$job_unk <- ifelse(bank$job == "unknown", 1, 0)
bank$edu_unk <- ifelse(bank$education == "unknown", 1, 0)
bank$cont_unk <- ifelse(bank$contact == "unknown", 1, 0)
bank$pout_unk <- ifelse(bank$poutcome == "unknown", 1, 0)

# This code of chunk make the character data into numeic format
bank$job <- as.numeric(as.factor(bank$job))
bank$marital <- as.numeric(as.factor(bank$marital))
bank$education <- as.numeric(as.factor(bank$education))
bank$default<- ifelse(bank$default == "yes", 1, 0)
bank$housing <- ifelse(bank$housing== "yes", 1, 0)
bank$loan<- ifelse(bank$loan== "yes", 1, 0)
bank$month <- as.numeric(as.factor(bank$month))
bank$contact <- as.numeric(as.factor(bank$contact))
bank$poutcome <- as.numeric(as.factor(bank$poutcome))
bank$y <- ifelse(bank$y== "yes", 1, 0)


# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# normalize the data to get rid of outliers if present in the data set
bank <- as.data.frame(lapply(bank, normalize))

# Creating design matrix and target vector
mydata.X <- model.matrix(y ~ -1+., data= bank)
mydata.X <- as.data.frame(mydata.X)
mydata.Y <- bank$y

#Now we split the data into training and test.  
cuts <- c(training = .8, test = .2)
g <- sample(cut(seq(nrow(mydata.X)), nrow(mydata.X)*cumsum(c(0,cuts)), labels = names(cuts)))
final.X <- split(mydata.X, g)
final.Y <- split(mydata.Y, g)

    ######################
###### Ridge Regression ##  
    ######################
bank.ridge <- cv.glmnet(x= as.matrix(final.X$training), y = as.matrix(final.Y$training), nfolds=10, 
                        type.measure="class", family='binomial', alpha = 0, nlambda=100)
print(bank.ridge$lambda.min)

# Visualization of Ridge Plot
plot(bank.ridge)

# Create a dataframe with the coefficient values
ridge.coefs <- as.data.frame(as.vector(coef(bank.ridge, s = bank.ridge$lambda.min)), 
                             row.names = rownames(coef(bank.ridge)))
names(ridge.coefs) <- 'coefficient'

# Lasso
bank.lasso <- cv.glmnet(x = as.matrix(final.X$training), y = as.matrix(final.Y$training), nfolds=10, 
                        type.measure="class", parallel=TRUE, family='binomial', alpha = 1, nlambda=100)
print(bank.lasso$lambda.min)

# Visualization of Lasso
plot(bank.lasso)

# Create a dataframe with the coefficient values
lasso.coefs <- as.data.frame(as.vector(coef(bank.lasso, s = bank.lasso$lambda.min)), 
                             row.names = rownames(coef(bank.lasso)))
print(lasso.coefs)

# Non Zero Lasso Features
names(lasso.coefs) <- 'coefficient'
features <- rownames(lasso.coefs)[lasso.coefs != 0]
print(features)

# Creates a new matrix with only the non-zero features
lasso_bank <- bank[, intersect(colnames(bank), features)]
# Re-do the split into training and test
bank <- as.matrix(lasso_bank)
bank <- as.data.frame(bank)
bank$Y <- mydata.Y
bank_1 <- split(bank, g)

# Runing Logistic Regression
model_std <- glm(Y ~ ., family = binomial(link = "logit"),  data = bank_1$training)
summary(model_std)

# Features of Logistic Regression
names(model_std$coefficients)

# Prediction and misclassification of the model
predictions <- predict.glm(model_std, newdata=bank_1$test, type= "response")
predictions[predictions > 0.5] <- 1
predictions[predictions <= 0.5] <- 0
1 - length(predictions[predictions == bank_1$test$Y]) / length(predictions)

# Confusion Matrix on Test Data
table(predictions, bank_1$test$Y)

# CrossTable For Test Data
CrossTable(predictions, bank_1$test$Y, prop.chisq = FALSE)

# Residual Analysis
residual.analysis <- function(model, std = TRUE){
  if (std == TRUE){
    res.model = rstandard(model)
  }else{
    res.model = residuals(model)
  }
  par(mfrow=c(2,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardised residuals")
  qqnorm(res.model,main="QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  acf(res.model,main="ACF of standardised residuals")
  print(shapiro.test(res.model))
}
residual.analysis(model_std)

# Checking Durbin Watson Model
durbinWatsonTest(model_std)

# Checking Variance Inflation Factor
vif(model_std)

  ################################
#### K-Nearest Neighbor Model ####  
  ################################

# Loading The Data
bank<-read.csv("bank1.csv", stringsAsFactors = FALSE, header = T)

# This code of chunks creates extra column for variables with unknown values
bank$job_unk <- ifelse(bank$job == "unknown", 1, 0)
bank$edu_unk <- ifelse(bank$education == "unknown", 1, 0)
bank$cont_unk <- ifelse(bank$contact == "unknown", 1, 0)
bank$pout_unk <- ifelse(bank$poutcome == "unknown", 1, 0)

# This code of chunk make the character data into numeric format
bank$job <- as.numeric(as.factor(bank$job))
bank$marital <- as.numeric(as.factor(bank$marital))
bank$education <- as.numeric(as.factor(bank$education))
bank$default<- ifelse(bank$default == "yes", 1, 0)
bank$housing <- ifelse(bank$housing== "yes", 1, 0)
bank$loan<- ifelse(bank$loan== "yes", 1, 0)
bank$month <- as.numeric(as.factor(bank$month))
bank$contact <- as.numeric(as.factor(bank$contact))
bank$poutcome <- as.numeric(as.factor(bank$poutcome))
bank$y <- ifelse(bank$y== "yes", 1, 0)

# Creates normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# normalize the data
bank <- as.data.frame(lapply(bank, normalize))
# We create our design matrix and target vector
mydata <- bank
mydata.X <- model.matrix(y ~ -1+., data= bank)
mydata.X <- as.data.frame(mydata.X)
mydata.Y <- bank$y

# Splitting data into training, test and validation sets.
cuts <- c(training = .7, test = .2, validation = .1)
g <- sample(cut(seq(nrow(mydata.X)), nrow(mydata.X)*cumsum(c(0,cuts)), labels = names(cuts)))
final.X <- split(mydata.X, g)
final.Y <- split(mydata.Y, g)
raw <- split(mydata, g)

# KNN Split
size <- 4000 
sp <- split(mydata, list(mydata$y))
samples <- lapply(sp, function(x) x[sample(1:nrow(x), 500, replace = FALSE), ])
mydata_sample <- do.call(rbind, samples)
row.names(mydata_sample) <- NULL

# this function creats the design matrix and target variables
mydata_sample.X <- model.matrix(y ~ -1+., data= mydata_sample)
mydata_sample.X <- as.data.frame(mydata_sample.X)
mydata_sample.Y <- mydata_sample$y

# We will split the data into training, test and validation sets.
cuts <- c(training = .6, test = .2, validation = .2)

g <- sample(cut(seq(nrow(mydata_sample.X)), nrow(mydata_sample.X)*cumsum(c(0,cuts)), labels = names(cuts)))
final_sample.X <- split(mydata_sample.X, g)
final_sample.Y <- split(mydata_sample.Y, g)
raw <- split(mydata_sample, g)

# we choose the random value for validation set 
nn <- 3 
knn.pred <- knn(final_sample.X$training, final_sample.X$validation, final_sample.Y$training,  k = nn, prob = TRUE)

error <- 1 - length(final_sample.Y$validation[final_sample.Y$validation==knn.pred]) / length(final_sample.Y$validation)
error

# Bias-Variance Tradeoff Algorithm
maxiter <- 50
bv <- data.frame(k=integer(), Training=double(), Validation=double())
for (nn in 1:maxiter){
  knn.pred1 <- knn(final_sample.X$training, final_sample.X$training, final_sample.Y$training, k=nn)
  
  knn.pred2 <- knn(final_sample.X$training, final_sample.X$validation, final_sample.Y$training, k=nn) 
  
  cat("iteration: ", include=FALSE, nn, "\n") 
  terr <- 1 - length(final_sample.Y$training[final_sample.Y$training==knn.pred1]) / length(final_sample.Y$training)
  verr <- 1 - length(final_sample.Y$validation[final_sample.Y$validation==knn.pred2]) / length(final_sample.Y$validation)
  rec <- data.frame(k=nn, Training=terr, Validation=verr)
  bv <- rbind(bv, rec)
}

# Bias- Variance Trade-off Plot
bv_melt <- melt(bv, id.vars = "k", variable.name = "Source", value.name = "Error")
title <- "Bias-Variance Tradeoff"
ggplot(bv_melt, aes(x=k, y=Error, color=Source)) +
  geom_point(shape=16) + geom_line() +
  xlab("Number of Neighbours (K)") +
  ylab("Misclassification Rate") +
  ggtitle(title) +
  theme(plot.title = element_text(color="#666666", face="bold", size=18, hjust=0)) +
  theme(axis.title = element_text(color="#666666", face="bold", size=14))

# Number of Neigbour
nn <- 4
knn.pred3 <- knn(final_sample.X$training, final_sample.X$test, final_sample.Y$training, k=nn)

# Misclassification error in the test set
error1 <- 1 - length(final_sample.Y$test[final_sample.Y$test==knn.pred3]) / length(final_sample.Y$test)
error1

# Confusion matrix from test set
table(knn.pred3, final_sample.Y$test)

# Crosstable KNN
CrossTable(knn.pred3, final_sample.Y$test, prop.chisq = FALSE)

  #########################
#### Naive Bayes Model ####  
  #########################

set.seed(45)

#Import data to R
bank<-read.csv("bank1.csv", stringsAsFactors = FALSE, header = T)
#View(bank)

# This code of chunks creates extra column for variables with unknown values
bank$job_unk <- ifelse(bank$job == "unknown", 1, 0)
bank$edu_unk <- ifelse(bank$education == "unknown", 1, 0)
bank$cont_unk <- ifelse(bank$contact == "unknown", 1, 0)
bank$pout_unk <- ifelse(bank$poutcome == "unknown", 1, 0)

# This code of chunk make the character data into data frame format
bank$job <- as.numeric(as.factor(bank$job))
bank$marital <- as.numeric(as.factor(bank$marital))
bank$education <- as.numeric(as.factor(bank$education))
bank$default<- ifelse(bank$default == "yes", 1, 0)
bank$housing <- ifelse(bank$housing== "yes", 1, 0)
bank$loan<- ifelse(bank$loan== "yes", 1, 0)
bank$month <- as.numeric(as.factor(bank$month))
bank$contact <- as.numeric(as.factor(bank$contact))
bank$poutcome <- as.numeric(as.factor(bank$poutcome))

# We need the target varible in the factor format.
bank$y <- as.factor(bank$y)

ind = sample(2, nrow(bank), replace = TRUE, prob=c(0.7, 0.3))
trainset = bank[ind == 1,]
testset = bank[ind == 2,]

# This code returns the dimention of our training and test sets. first column represents the numebr of observations and second represents number  of variables. 
dim(trainset)

# percentage of customer subscribing term deposit
pctPos <- nrow(testset[testset$y == "yes",]) / nrow(testset)
pctPos

# naive Bayes function
model <- naiveBayes(trainset[, !names(trainset) %in% c("y")],
                    trainset$y, na.action = na.pass)
# Type classifier to examine the function call, a-priori probability, and conditional
#probability
model

# Prediction On Test Set
x<- testset[, !names(testset) %in% c("y")]
y <- testset$y

# Prediction and misclassification Error 
bayes.table <- table(predict(model, x), y)
bayes.table

# Misclassification Error
1-sum(bayes.table[row(bayes.table)==col(bayes.table)])/sum(bayes.table)

# Confusion Matrix
confusionMatrix(bayes.table)


######################################################### END ##########################################





















