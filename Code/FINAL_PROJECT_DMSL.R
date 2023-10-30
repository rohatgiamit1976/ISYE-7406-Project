set.seed(50)
nbadata <- read.table("C:\\Users\\Sebastián\\Downloads\\testing_dataset.csv", 
                       header=TRUE,sep=",")
head(nbadata)
dim(nbadata)


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


###############################################################################
## Split Data
###############################################################################

n = dim(nbadata)[1]
n1 = round(n/5)
flag <- sort(sample(1:n, n1))

traindata <- nbadata[-flag,]  # 80% train data
testdata <- nbadata[flag,]    # 20% test data
dim(traindata)
dim(testdata)
