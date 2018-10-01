## PLAN:

## - Study the data set
## - check for missing values
## - replace NA's
## - change variables types if needed
## - create additional vaues from what is given if needed
## - try and spot patterns
## - join both datasets into one

## - create a model to predict missing Age
## - create FamSize, AgeGroup variables
## - create initial glm(), find what variables contribute the most/the least; 
## - test model against test part of a train set

## - perform text analytics on Name, Tix, Cabin
## - find matches among people in same cabins
## - does survival depends on CabinCat?
## - does survival depends on a ticket number
## - what do letters in tickets mean
## - improve model

## set working directory
setwd("C:/Users/Lenovo/Documents/R/R WD")

##read in some data; read.csv or read.table by default reads our columns as factors

titanic.train <- read.csv(file = "train.csv", header = T, na.strings = c(" "))
titanic.test <- read.csv(file = "test.csv", header = T, na.strings = c(" "))

is.factor(titanic.train$Sex)
is.factor(titanic.train$Embarked)

## for better understanding of how R interpreted categorical variables we can use contrasts() functon
## honestly, not sure how to interpret Embarked from here
contrasts(titanic.train$Sex)
contrasts(titanic.train$Embarked)

# now we need to combine both data sets
# we create a new column T/F in both sets to differentiate them later if a part of a train or test set
titanic.train$IsTrainSet <- T
titanic.test$IsTrainSet <- F

## to check that T/F populated till the end, lets run tail() function
tail(titanic.train$IsTrainSet)

## now we are going to combine them, and both data sets have to line up, 
## but train dataset has one extra column:

ncol(titanic.train)
ncol(titanic.test)

#get column names, compare they spelled the same way:
names(titanic.train)
names(titanic.test)

#we add missing column to the test dataset and fill it with NA's
titanic.test$Survived <- NA

## now if we check number of columns we will get same number for both datasets
ncol(titanic.test)

## next we will combine both sets
titanic.full <- rbind(titanic.train, titanic.test)

## lets check tail to see all the data till the end has populated
tail(titanic.full)

## lets check our full dataset isn't missing any values:
table(titanic.full$IsTrainSet)

## Lets study our data
str(titanic.full)

## There are plenty of NA's
colSums(is.na(titanic.train))
## in train set the only column contains NA is Age, 177 missing values

## lets check test set
colSums(is.na(titanic.test))
## Missing 86 values in Age, 1 in Fare

## lets make a table of Embarked column as an example, since it is a factor, it didn't show any missing values there,
## but showed 4 levels while it should be 3
table(titanic.full$Embarked)

## We will need to cleen up those missing value in a way that will help us then create a reliable model

### we build a quick filter
### titanic.full$Embarked == ''
### Next I want to query just an Embarked column of a titanic.full
### so I want to filter all the Embarked that is NA , and i want only the Embarked column to come back
### based on a table that we built previously for Embarked column, there are 2 such values
### 

## now we want to replace them with something, let's for now do the most frequent value (or median)
titanic.full[titanic.full$Embarked == '', "Embarked"] <- 'S'

##So to check, we run table() again, blank values should be replaced with S
table(titanic.full$Embarked)

##Let's check Age column, if it has a lot of missing values
table(is.na(titanic.full$Age))

## it may be helpful to find median of both datasets as well as a global one
## lets query full dataset for Age and replace with something - median right now

age.median <- median(titanic.full$Age, na.rm = T)
train.age.median <- median(titanic.train$Age, na.rm = T)
test.age.median <-median(titanic.test$Age, na.rm = T)

print(train.age.median)
print(test.age.median)
print(age.median)

titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

### titanic.train$Age[is.na(titanic.train$Age)] <- train.age.median
### titanic.test$Age[is.na(titanic.test$Age)] <- test.age.median

##if now we make a table(is.na(titanic.full$Age)) we should not have any missing values

## Lets check Fare column
table(is.na(titanic.full$Fare))

## Since we have only 1 missing value, Lets fill it in quickly with median as well

fare.median <- median(titanic.full$Fare, na.rm = T)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median

## Lets check Fare column again, to see that missing value was filled
table(is.na(titanic.full$Fare))

## let's check if any gender got luckier a surviving
table(titanic.train$Sex, titanic.train$Survived)

## let's find out if certain cabin class passengers got more chance of surviving
table (titanic.train$Pclass, titanic.train$Survived)

## Females had a better chance at surviving, in any cabin class, less so in 3rd cabin class
## equal amount of woment perished/survived in 3rd cabin class;
## so males had a smaller chance of survival anywhere, particuarly in 3rd class cabins
## Number of male survived in 1st cabin class is roughly half the amount of those who perished

## Next we'll create a column for famil size
titanic.train$FamSize <- 1 + titanic.train$SibSp + titanic.train$Parch
titanic.test$FamSize <- 1 + titanic.test$SibSp + titanic.test$Parch
titanic.full$FamSize <- 1 + titanic.full$SibSp + titanic.full$Parch

## next we'll create Age groups
titanic.full$Age_Group <- ifelse(titanic.full$Age < 18, "Child", 
                                 ifelse(titanic.full$Age < 60, "Adult", "Senior"))
titanic.full$Is_Child <- ifelse(titanic.full$Age_Group == "Child", 1, 0)

titanic.full$Is_Child_or_Woman <- ifelse(titanic.full$Age_Group == "Child"| titanic.full$Sex == "female", 1, 0)

## we select only columns from train set that we think wecan use in our model
## then we split our new dataset into two chunks, one to create a model
## and then test it on a train set

train <- titanic.full[c(2,3, 5, 6, 10, 12, 14, 15)]
train.fit <- train[1:700,]
train.test <- train[701:891,]

## now we create a model
model <- glm(Survived ~ ., family = binomial(link='logit'), data = train.fit )

## by using summary() we obtain results of our model
summary(model)

## the lower the p-value, the stronger is affect of this variable on a survival
## sex = male has the largest negative value -12.2, meaning being a male increases chances to parish
## Pclas -6.3, FamSize -3.28, Age -1.9, EmbarkedS -1.2, Child +1.8, Fare +0.5; 
## insignificant: Senior -0.03, EmbarkedQ -0.06

## next we can run anova() function to analyze the deviance
anova(model, test = "Chisq")

## we are looking to see a significant drop in deviance, 
## that indicates which variables strongly improve model performance
## Fare has a minimal deviance of 0.085 and doesn't improve our model

## Now we are going to assess the predictive ability of our model

fitted.res <- predict(model, newdata = subset(train.test, type = 'response')) 
