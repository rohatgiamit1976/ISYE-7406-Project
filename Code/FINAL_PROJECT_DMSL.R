set.seed(50)
nbadata <- read.table("C:\\Users\\Sebastián\\Downloads\\testing_dataset.csv", 
                       header=TRUE,sep=",")
cleandata <- na.omit(nbadata)
head(nbadata)
dim(nbadata)
dim(cleanda)


###############################################################################
## EDA
###############################################################################

# Total No. Players
no_players <- dim(nbadata)[1]

# Total No. Variables
no_vars <- dim(nbadata)[2]

# Avg. Salary => $5'424,224 or 5.42 million usd
avg_salary <- mean(nbadata$Salary)
avg_salary

# Highest earner in the NBA
highest_salary <- nbadata[which.max(nbadata$Salary), ]$Salary
highest_salary

# Lowest earner in the NBA
lowest_salary <- nbadata[which.min(nbadata$Salary), ]$Salary
lowest_salary

# % Players earning below mean => 66.27% of players earn below mean salary
no_salary_below_avg <- dim(nbadata[nbadata$Salary < avg_salary,])[1]
pcnt_salary_below_avg <- (no_salary_below_avg / no_players) * 100
pcnt_salary_below_avg

# Avg. Age in the NBA
avg_age <- mean(nbadata$Age)

# Oldest Player in the NBA
oldest_player <- nbadata[which.max(nbadata$Age), c("Player.Name", "Age")]

# Youngest Player in the NBA
youngest_player <- nbadata[which.min(nbadata$Age), c("Player.Name", "Age")]

# Team Salaries (BOS > SAC > NYK > CLE ..... > BRK/MIA > MIA/WAS)
nbadata$TeamShort <- substr(nbadata$Team, start = 1, stop = 3)
team_salary_total <- aggregate(Salary ~ TeamShort, data = nbadata, FUN = sum)
#team_salary_total <- aggregate(Salary ~ Team, data = nbadata, FUN = sum)
team_salary_total <- team_salary_total[order(-team_salary_total$Salary),]
############## Correlation Plot##############################################
nbacor <-- data.frame(nbadata[c("Salary","Age","GP","GS","MP","FG","FGA","X3P","X3PA","X2P","X2PA","FT","FTA","AST","STL","BLK",
                                "TOV","PF","Total.Minutes","PER","BPM","DWS","WS","VORP","PTS","ORB","DRB","TRB")])
library("corrplot")

# correlation matrix
M<-cor(nbacor)
corrplot(M, method="circle")

############################### Histograms ###################################
library("psych")

hist(nbadata$Salary, col="blue", border="white")
hist(nbadata$Age, col="blue", border="white")
hist(nbadata$FG, col="blue", border="white")
hist(nbadata$X3P, col="blue", border="white")
hist(nbadata$X2P, col="blue", border="white")
hist(nbadata$FT, col="blue", border="white")
hist(nbadata$DRB, col="blue", border="white")
hist(nbadata$STL, col="blue", border="white")
hist(nbadata$BLK, col="blue", border="white")

############################## Box Plots #####################################
boxplot(nbadata$Salary, main = "NBA Salary Boxplot", xlab = "Salary", 
        col = "lightblue", border = "black", horizontal = TRUE)


############################## Scatterplots #################################
plot(cleandata$Age, cleandata$Salary, xlab = "Age", ylab = "Salary")
#plot(cleandata$Team, cleandata$X3PA, xlab = "Age", ylab = "Salary")


###############################################################################
## Split and Process Data
###############################################################################

n = dim(cleandata)[1]
n1 = round(n/5)
flag <- sort(sample(1:n, n1))

# Z-Norm data
cleandata$NormSalary <- scale(cleandata$Salary, center = TRUE, scale = TRUE)

traindata <- cleandata[-flag,]  # 80% train data
testdata <- cleandata[flag,]    # 20% test data
dim(traindata)
dim(testdata)

# Remove Player's Name, we don't need it for our model
traindata$Player.Name <- NULL
testdata$Player.Name <- NULL

# Remove Team, need to figure out how to fix that variable since there are
# players such as Mike Mucscala with TEAM: BOS/OKC, he's the only one with 
# that team, causing issues when building models 
traindata$Team <- NULL
testdata$Team <- NULL

head(traindata)
dim(traindata)
dim(testdata)

traindata$Salary <- NULL
testdata$Salary <- NULL

# Figure out how to include all available positions for training data

traindata$Position <- NULL
testdata$Position <- NULL

# Remove high correlated Variables
traindata$FGA <- NULL
testdata$FGA <- NULL

traindata$FGP <- NULL
testdata$FGP <- NULL

traindata$X2PA <- NULL
testdata$X2PA <- NULL

traindata$PTS <- NULL
testdata$PTS <- NULL

traindata$DRB <- NULL
testdata$DRB <- NULL

head(traindata)
dim(traindata)

###### feature Importance###########
# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
library(randomForest)
# load the dataset
data(traindata)
# prepare training scheme

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(traindata[,1:42], traindata[,43], sizes=c(1:42), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

###############################################################################
## MODELS
###############################################################################

## Linear Regression with all predictors
ytrue <- testdata$NormSalary

################## Linear regression with ALL predictors ######################

lr <- lm(traindata$NormSalary ~. , data = traindata)
summary(lr)

# Model 1: Mean Squared Error
# 1:47 if no position,
pred_lr <- predict(lr, testdata[,1:42])
te1 <- mean((pred_lr - ytrue)^2)
te1

###################  Stepwise using AIC ##################################

stepwise <- step(lr)

round(coef(stepwise), 3)
summary(stepwise)

pred_sw <- predict(stepwise, testdata[,1:42])
te2 <- mean((pred_sw - ytrue)^2)
te2

########################### Ridge regression ##################################

install.packages('MASS')
library(MASS)

ridge_mod <- lm.ridge(NormSalary ~., data = traindata, lambda=seq(0,100,0.001))
# plot how the beta coeff change with lambda values
plot(ridge_mod)

# select ridge model with optimal lambda value:
# auto find the index for the optimal lambda value wrt for Ridge 

indexopt <- which.min(ridge_mod$GCV)
indexopt
ridge_mod$coef[, indexopt]

ridge.coeffs <- ridge_mod$coef[, indexopt] / ridge_mod$scales
intercept <- -sum(ridge.coeffs * colMeans(traindata[,1:42])) + mean(traindata[,43])

# see the coefficients estimated from the Ridge regression on the original data scale
c(intercept, ridge.coeffs)

# Ridge testing errors
pred_ridge <- as.matrix(testdata[,1:42]) %*% as.vector(ridge.coeffs) + intercept
te3 <- mean((pred_ridge - ytrue)^2)
te3

################################# LASSO #######################################

install.packages('lars')
library(lars)

data.lars <- lars(as.matrix(traindata[,1:42]), traindata[,43], 
                  type="lasso", trace=FALSE)

# Useful plots for LASSO for all penalty parameters lambda
plot(data.lars)

# Optimal lambda value that minimizes Mellon's CP Criterion
cp1 <- summary(data.lars)$Cp
index1 <- which.min(cp1)
index1


lasso.lambda <- data.lars$lambda[index1]
lasso.lambda

# LASSO Test error
pred_lasso <- predict(data.lars, as.matrix(testdata[,1:42]), s=lasso.lambda, 
                      type="fit", mode="lambda")
te4 <- mean((pred_lasso$fit - ytrue)^2)
te4

############################### PC Regression ################################
install.packages('pls')
library(pls)

data.pca <- pcr(NormSalary ~., data=traindata, validation="CV")
validationplot(data.pca)
summary(data.pca)

# minimum
ncompopt <- which.min(data.pca$validation$adj)
ncompopt

pred_pc <- predict(data.pca, ncomp = ncompopt, newdata=testdata[,1:42])
pred_pc <- pred_pc[,,1]
te5 <- mean((pred_pc - ytrue)^2)
te5

################################ PLS ##########################################
data.pls <- plsr(NormSalary ~., data=traindata, validation="CV")

# optimal # of components of PLS
pls_ncompopt <- which.min(data.pls$validation$adj)
pls_ncompopt

pred_pls <- predict(data.pls, ncomp = pls_ncompopt, newdata = testdata[,1:42])
pred_pls <- pred_pls[,,1]
te6 <- mean((pred_pls - ytrue)^2)
te6


###############################################################################
### Models with Monte Carlo CV
###############################################################################
