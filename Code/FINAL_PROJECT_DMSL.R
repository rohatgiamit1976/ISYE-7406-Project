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
team_salary_total <- aggregate(Salary ~ Team, data = nbadata, FUN = sum)
team_salary_total <- team_salary_total[order(-team_salary_total$Salary),]


############################### Histograms ###################################


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

head(traindata)
dim(traindata)

###############################################################################
## MODELS
###############################################################################

## Linear Regression with all predictors
ytrue <- testdata$NormSalary

################## Linear regression with ALL predictors ######################

lr <- lm(traindata$NormSalary ~. , data = traindata)
summary(lr)

# Model 1: Mean Squared Error
pred_lr <- predict(lr, testdata[,1:47])
te1 <- mean((pred_lr - ytrue)^2)
te1

###################  Stepwise using AIC ##################################

stepwise <- step(lr)

round(coef(stepwise), 3)
summary(stepwise)

pred_sw <- predict(stepwise, testdata[,1:47])
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
intercept <- -sum(ridge.coeffs * colMeans(traindata[,1:47])) + mean(traindata[,48])

# see the coefficients estimated from the Ridge regression on the original data scale
c(intercept, ridge.coeffs)

# Ridge testing errors
pred_ridge <- as.matrix(testdata[,1:47]) %*% as.vector(ridge.coeffs) + intercept
te3 <- mean((pred_ridge - ytrue)^2)
te3


