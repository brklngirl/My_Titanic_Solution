## set working directory
setwd("C:/Users/Lenovo/Documents/R/R WD")

install.packages("Amelia")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("scales")
install.packages("stringr")
install.packages("caTools")
install.packages("ROCR")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("tidyr")
install.packages("purrr")


library("Amelia")
library("dplyr")
library("ggplot2")
library("scales")
library("stringr")
library("caTools")
library("ROCR")
library("rpart")
library("rpart.plot")
library("randomForest")
library("tidyr")
library("purrr")

##read in some data; read.csv or read.table by default reads our columns as factors
titanic.train <- read.csv(file = "train.csv", header = T, stringsAsFactors = F, na.strings = c(" "))
titanic.test <- read.csv(file = "test.csv", header = T, stringsAsFactors = F,na.strings = c(" "))

### is.factor(titanic.train$Sex)
### is.factor(titanic.train$Embarked)

# now we will combine both data sets
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

## lets check tail again to see all the data till the end has populated
tail(titanic.full)

## lets check our full dataset isn't missing any values:
table(titanic.full$IsTrainSet)

## Lets study our data
str(titanic.full)

## There are plenty of NA's
colSums(is.na(titanic.train) | titanic.train == '')
## in train set the only column contains NA is Age, 177 missing values

## lets check test set
colSums(is.na(titanic.test) | titanic.test == '')

## let's combine those results, because when we will fill na's we will do it for the whole data frame anyways
colSums(is.na(titanic.full) | titanic.full == '')

## Cabin has the most NA's (1014), then Age (263), Emb and Fare have 2 and 1 NA respectively
## missmap function from Amelia package will visualize our missing values


missmap(titanic.full, main = "Titanic Dataset - Missing Value", col = c("green", "black"), legend = F)

## from this chart we can see that Age is stored as NA's (= populated), while Cabin was not populated
## thus stored as missing values 

## Next step, we will try and fill missing values
## we'll beging with missing Fare - only one value
## Let's extract all date for that missing value (using filter() from dplyr package)

filter(titanic.full, is.na(titanic.full$Fare)==T)

## this is a male passenger from 3rd class, who embarked in a port S
## we want to see what is a typical Fare was paid by similar passengers


ggplot(filter(titanic.full, Pclass==3 & Embarked=="S"), aes(Fare)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "darkblue", linetype = "dashed", size =2) + 
  geom_vline(aes(xintercept = mean(Fare, na.rm = T)), colour = "red", linetype = "dashed", size =2) +
  ggtitle(("Fare distribution of 3rd class passengers embarked in Southampton")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

## the mean and median are very different, hovewer, we can see that majority of people
## who fit our criteria (a huge spike) paid meadian value; 
## we will leave filling it for later, I want to do some more studying of the data and FE

## We see that some rows indicate a Fare value = 0, let's see further
filter(titanic.full, Fare == 0)

## there are 17 instances, all are male btw 19 -40 y.o, mostly 36-40, traveling alone, emb in Southhampton
## 2 are from train set, among others only one person survived
## we will leave it as is for now

## Next I want to query just a missing Embarked values of a titanic.full
## so I want to filter all the Embarked that is NA , and i want only the Embarked column to come back
## based on a table that we built previously for Embarked column, there are 2 such values
filter(titanic.full, Embarked == "NA" | Embarked == '')

## both missing values belong to 2 ladies in 1st class with the same ticket number and cabin, @ Fare = $80
## now we want to replace them with something
## let's see the frequency of embarkation at each port by passengers of Pclass 1
table(filter(titanic.full, Pclass == 1)$Embarked)

## While Southhampton is the most frequent, Cherbourg is not that far down from it
## Let's confirm, that people at which port have paid a Fare close to what our ladies paid

ggplot(filter(titanic.full, is.na(Embarked) == F & Embarked != '' & Pclass ==1), aes(Embarked, Fare)) +
  geom_boxplot(aes(colour = Embarked)) +
  geom_hline(aes(yintercept =80), colour = 'red', linetype = 'dashed', size =2) +
  ggtitle("Fare distribution among 1st Class passengers") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## according to our boxplot, passengers from S port paid on avg $50,
## while passengers from C did pay about $80 as our ladies did, so we use this info to  fill blanks

titanic.full$Embarked[titanic.full$Embarked == ''] <- "C"

##So to check, we run table() again, blank values should be replaced
table(titanic.full$Embarked)

## Now we'll get to studying Age data
summary(titanic.full$Age)

## let's see if there is any difference in age between different Pclasses

ggplot(titanic.full, aes(Pclass, Age)) +
  geom_boxplot(aes(fill=factor(Pclass), alpha = 0.5)) +
  ggtitle("Age distribution within Pclasses") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## Based on that chart, we can see that passengers in higher classes tend to be older
## Thus we want to fill NA's for each class based on avg value of that class

impute.age <- function (age, class) {
  vector <- age
  
  for ( i in 1:length(age)) {
    if(is.na(age[i])) {
      if(class[i]==1) {
        vector[i] <- round(mean(filter(titanic.full, Pclass == 1)$Age, na.rm =T), 0)
      } else if (class[i]==2) {
        vector[i] <- round(mean(filter(titanic.full, Pclass == 2)$Age, na.rm =T), 0)
    } else {
      vector[i] <- round(mean(filter(titanic.full, Pclass == 3)$Age, na.rm =T), 0)
  } 
    } else {
  vector[i] <- age[i]
    }
  } 
  return(vector)
}
  
imputed.age <- impute.age(titanic.full$Age, titanic.full$Pclass)
titanic.full$Age <- imputed.age

##if now we make a table(is.na(titanic.full$Age)) we should not have any missing values
table(is.na(titanic.full$Age))

## let's check if any gender had better chances at survival
aggregate(Survived ~ Sex, titanic.train, FUN = function(x) {sum(x)/length(x)})

## let's find out if certain cabin class passengers got more chance of surviving
## would like to see it as proportions? just a thought for a future
## prop.table(table(titanic.train$Pclass, titanic.train$Survived))

## I want to see if there is a difference btw sex survival by different Pclass

ggplot(titanic.full, aes(x = Pclass, y = Survived)) +
  geom_bar(aes(fill = factor(Sex)), stat = "identity", position="fill") +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.1)) +
  ylab("Percentage") +
  ggtitle("Difference between gender survival by PClass") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
### Some day I will add lables there too :)

## We can see visually from our stacked barplot that more females than males srvvd in all Pclasses 
ggplot(filter(titanic.full, !is.na(Survived)), aes(x= Sex, y = Survived, group = factor(Pclass))) +
  geom_bar(aes(fill = factor(Pclass)), stat = "identity", position = "stack") +
  geom_point(aes(color = factor(Pclass)))

## However, we want to see, what share of all F/M in each Pclass actually survived
aggregate(Survived ~ Pclass + Sex, data = titanic.full,FUN = function(x) {sum(x) /length (x)})

## Thus, 97%, 92% and 50% of female srvvd in each respective Pclass,
## versus 37%, 16%, 13% for male passengers

## Females had a better chance at surviving, in any class, less so in 3rd class
## equal amount of woment perished/survived in 3rd class;
## while males had a worse chances of survival, particuarly in 3rd class cabins

## ---Feature Engineering---

## Next we'll create a column for famil size
titanic.full$Oneself <- 1
titanic.full$Family <- titanic.full$SibSp + titanic.full$Parch + titanic.full$Oneself
table(titanic.full$Family)

## I want to add a column to see how many people could travel together on the same ticket #
## assuming sometimes a group of friends or a family w/nanny could travel together

titanic.full$TixNum <- ifelse(is.na((str_extract(titanic.full$Ticket, "\\d{3,}"))== T), 0, str_extract(titanic.full$Ticket, "\\d{3,}"))
titanic.full$TixText <- gsub("[.]","",str_to_upper(ifelse(is.na((str_extract(titanic.full$Ticket, "\\D[[:graph:][:space:]]*(?=[:space:]\\d{3,}?)"))== T), ##check if the result is na
                                                                      str_extract(titanic.full$Ticket,"\\D[[:graph:][:space:]]*(?![:space:]\\d{3,}?)"),   ## if yes, bring what is not followed by min 3digits
                                                                           str_extract(titanic.full$Ticket, "\\D[[:graph:][:space:]]*(?=[:space:]\\d{3,}?)") ), locale = "en")) ## if it is not na, extract text part

levels(factor(titanic.full$TixText)) 


titanic.full$TixBg <- regmatches(titanic.full$TixNum, regexpr("\\d", titanic.full$TixNum))

## Let's try to clean the text values to merge duplicates 
titanic.full$TixText <- sub(" ", "/", titanic.full$TixText)
titanic.full$TixText <- sub("A/", "A", titanic.full$TixText)
titanic.full$TixText <- sub("AQ/", "AQ", titanic.full$TixText)
titanic.full$TixText <- sub("WE/", "WE", titanic.full$TixText)
titanic.full$TixText <- sub("AH/BASLE", "AH", titanic.full$TixText)
titanic.full$TixText <- sub("SO/|STON|SOC|SCO|SOP", "SOTON", titanic.full$TixText)
titanic.full$TixText <- sub("CASOTON", "SOTON/CA", titanic.full$TixText)
titanic.full$TixText <- sub("/2", "2", titanic.full$TixText)
titanic.full$TixText <- sub("/3", "3", titanic.full$TixText)
titanic.full$TixText <- sub("SOTONPP", "SOTON/PP", titanic.full$TixText)
titanic.full$TixText <- sub("SOTONC", "SOTON", titanic.full$TixText)
titanic.full$TixText <- sub("FCC", "FC", titanic.full$TixText)
titanic.full$TixText <- sub("SO/C", "SC", titanic.full$TixText)

levels(factor(titanic.full$TixText))
## we got down to 34 levels, assuming we already cleaned some periods etc

table(titanic.full$Embarked, titanic.full$TixText)

## A..Prefixes (A5, A4, CA..etc)were assigned almost exclusively at Southhamptom port, as well
## as SOTON (and like ones: SO/C, SCO, SOC, STON, SW, WE..)
## while SC/Paric and SC/AH Basle are exclusive to Cherbourg
table(titanic.full$Pclass, titanic.full$TixText)

## Now I want to see how many persons are traveling under each ticket number
titanic.full$SameTix <- ifelse(titanic.full$TixNum == 0, 1, ave(titanic.full$PassengerId, titanic.full[, "TixNum"], FUN=length))

## next, we'd like to find out if some passengers tracel with non-family members
titanic.full$Other <- ifelse(titanic.full$SameTix >= titanic.full$Family, titanic.full$SameTix - titanic.full$Family, 0)

## Travel group will give us more precise count of people traveling together in a group
titanic.full$TravelGroup <- titanic.full$Family + titanic.full$Other
table(titanic.full$TravelGroup)
## while we see that total number of people in 11 and 8-member group are multiple of 11 and 8
## we have 37 people in a 7-member group, that means we probably missassigned someone
## we will return to it later when working with Last Names

## More than half of passengers are traveling alone, biggest family has 11 members
## we will group them together
titanic.full$Group <- ifelse(titanic.full$TravelGroup == 1, "Single", ifelse(titanic.full$TravelGroup <= 4, "Small", "Large"))
mosaicplot(~Group + Survived, data = titanic.full, main = "Survival rate based on Family Size", shade = T)

aggregate(Survived ~ Sex + TravelGroup, data = titanic.full, FUN = function(x) {sum(x)/length(x)})
aggregate(Survived ~ Sex + Pclass + Group, data = titanic.full, FUN = function(x) {sum(x)/length(x)})

## we will create a table of chance of survival based on gender, pclass and a size of travel group
SexPclassGrp <- data.frame()
titanic.full$SexPclassGrp <- paste(titanic.full$Pclass, titanic.full$Title, titanic.full$Group, sep =" ")
SexPclassGrp <- aggregate(Survived ~ SexPclassGrp, data = titanic.full, FUN = function(x) {sum(x)/length(x)})

for(i in 1:dim(titanic.full)[1]){
  
  x <- integer()
  for(x in 1:dim(SexPclassGrp)[1]){
    
    if(titanic.full$SexPclassGrp[i] == SexPclassGrp$SexPclassGrp[x]) {
      titanic.full$SPGSvv[i] <- SexPclassGrp$Survived[x]
    }
  }
}


## interestingly, Fare seems to indicate amount paid per ticket, not per person
## thus, if 5 people travel together, Fare is a what was paid for them alltogether
## lets calculate Fare per Person

titanic.full$Fare <- ifelse(titanic.full$Fare<=0, 0, titanic.full$Fare)

titanic.full$FarePP <- titanic.full$Fare/titanic.full$SameTix
summary(titanic.full$FarePP)

##Let's fill our NA with median FarePP for the passengers of 3rd class embarked in S 
titanic.full$FarePP[is.na(titanic.full$FarePP == T)] = median(filter(titanic.full, Pclass ==3 & Embarked == "S")$FarePP, na.rm =T)

## Now we'll check that it has been filled
summary(titanic.full$FarePP)  

titanic.full$FarePP <- trunc(titanic.full$FarePP)
titanic.full$FareGroup <- NA
titanic.full$FareGroup <- cut_number(titanic.full$FarePP, 4)

aggregate(Survived~ FareGroup + Pclass, data = titanic.full, FUN = function(x) {sum(x)/length(x)})
aggregate(Survived~ FareGroup + Pclass, data = titanic.full, FUN = sum)

ggplot(titanic.full, aes(FarePP)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  facet_wrap(~Pclass) +
  geom_vline(aes(xintercept = 25), colour = "darkblue", linetype = "dashed", size =1) + 
  geom_vline(aes(xintercept = 15), colour = "red", linetype = "dashed", size =1) +
  geom_vline(aes(xintercept = 10), colour = "light green", linetype = "dashed", size =1) +
  ggtitle(("Fare per person distribution by class ")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

## would love to add survival line to those charts!!!!! maybe even do a violin plot

prop.table(table(titanic.full$FareGroup, titanic.full$Survived), margin = 1)
## 94% of people with Fare less than $6 perished, but this doesnt give us any new info, 
## since these are probably passengers who have a $0 fare
## thus, 75% of people in that group perished, from people who paid $16-$80, chance are appx. equal,
## expensive tix (above $81 had 77% of survival)

## next we'll create Age groups, considering child turns adult when reaches 18 y.o
titanic.full$Age <- ifelse(titanic.full$Age <= 1, 1, round(titanic.full$Age, 0))

titanic.full$AgeGroup <- cut_interval(titanic.full$Age, 15)

aggregate(Survived ~ AgeGroup + Sex, data = titanic.full, FUN = sum) 
aggregate(Survived ~ AgeGroup + Sex + Pclass, data = titanic.full, FUN = function(x) {sum(x)/length(x)})
## all senior females survived

## Now we'll extract titles and last names from name column

titanic.full$LName <- NA
titanic.full$LName <- unlist(regmatches(x = titanic.full$Name, regexpr(pattern = "\\<\\D{1,2}[[:alpha:]]+\\>", text = titanic.full$Name))) 
titanic.full$SameLN <- ave(titanic.full$PassengerId, titanic.full[, "LName"], FUN=length)
### titanic.full$LNTixCombo <- titanic.full %>% group_by( TixNum, LName) %>% summarise(LNRepeat = n())

## we want to see how many unique Last Name & Tix combinations there are
LNRepeat <- data.frame()
LNRepeat <- titanic.full %>% group_by(LName, TixNum ) %>% summarise(LNRepeat = n()) 

## since it gives us ony unique values, we get less grouped values than our initial df
## so I want to create a loop to fill LN & Tix combinations in our main df

### for(i in 1:dim(titanic.full)[1]){
  
  x <- integer()
  for(x in 1:dim(LNRepeat)[1]){
    
    if(titanic.full$LName[i] == LNRepeat$LName[x] &
       titanic.full$TixNum[i] == LNRepeat$TixNum[x]) {
      titanic.full$LNTix[i] <- LNRepeat$LNRepeat[x]
    }
  }
### }
 
## I want to see how many different tickets we have per last name
###TixRepeat <- data.frame()
###TixRepeat<- titanic.full %>% count(LName, Tix = n_distinct(TixNum)) ## how many tickets per last name ?
###TixRepeat2<- titanic.full %>% count(LName, TixNum) ## how many tickets per last name ???

### !!!  titanic.full$TixLN <- ave(titanic.full$LName, titanic.full[, "TixNum"], FUN=length) - same as sametix; maybe do a for loop hmm, 

## I want to see if what is common between people who travels with non-family memebrs
NonFamGrp <- data.frame()
NonFamGrp <- filter(titanic.full, Other >= 1 & TravelGroup ==(titanic.full$Other +1))
## there are few cases when people with the same LName on the same ticket were marked as havin no family onboard

titanic.full$Title <- NA
titanic.full$Title <- unlist(regmatches(x = titanic.full$Name, regexpr(pattern = "[[:alpha:]]+\\.", text = titanic.full$Name))) 

## To see how many different titles we have, we need to convert our new column to factors
titanic.full$Title <- as.factor(titanic.full$Title)

## Lets see how many different titles there are, and check for validity 
str(titanic.full$Title)
table(titanic.full$Title, titanic.full$Sex)

##Now we will group outliers among titles into "other" group, and similar ones into the larger buckets
TL <- levels(titanic.full$Title)
levels(titanic.full$Title) <- gsub("[.]", "", ifelse(TL %in%  c("Don.", "Major.", "Sir.", "Rev.", "Capt.", "Col."), "Mr.",
                                     ifelse(TL %in% c("Dona.", "Countess.", "Lady.", "Mme."), "Mrs.",
                                            ifelse(TL %in% c("Ms.", "Mlle."), "Miss.", 
                                                   ifelse(TL == "Jonkheer.", "Master.", TL)))))

## Let's check, if we got the desired large groups of Title
table(titanic.full$Title, titanic.full$Sex)
filter(titanic.full, Title =="Dr" & titanic.full$Survived == 1)
## the only Dr who we don't know if survived, emb in S, paid $27, has cabin info and is 53 y.e;
## however his tix starts with 3, and he travels in a family of 3, and has a child
## let's check if child svvd
filter(titanic.full, LName == "Dodge")
## He did!!!! Since his wife is 54 y.o, 1st class ulso unknown if svvd, I will assume - they both did
titanic.full$SPGSvv[titanic.full$LName == "Dodge"] <- 1

halfsvv <- data.frame()
halfsvv <- filter(titanic.full, SPGSvv==0.5)

mosaicplot(~Title + Survived, data = titanic.full, main = "Survival rate based on Title", shade = T)

ggplot(filter(titanic.full, is.na(Survived)==F), aes(Title)) +
  geom_bar(aes(fill = factor(Survived)), alpha = 0.9, position = "fill") +
  facet_wrap(~Pclass) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1)) +
  ylab("Percentage") + 
  ggtitle("Survival Rate based on Pclass and Title") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Title Mr. has the worst survival rate among all titles 

ggplot(filter(titanic.full, is.na(Survived)==F), aes(Title)) +
  geom_bar(aes(fill = factor(Survived)), alpha = 0.9, position = "fill") +
  facet_wrap(~Group) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1)) +
  ylab("Percentage") + 
  ggtitle("Survival Rate based on Family Size and Title") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


aggregate(Survived ~ Group + Title, data = titanic.full, FUN = function(x) {sum(x)/length(x)})

## Large Groups have the worst survival rates for all titles

## lets research Cabin info
titanic.full$Deck <- NA
titanic.full$Deck <- factor(substr(titanic.full$Cabin, start =1, stop =1))
table(titanic.full$Deck)

table(titanic.full$Pclass, titanic.full$Deck)
## higher decks respond to higher Pclass

table(titanic.full$FareGroup, titanic.full$Deck)
## while higher decks also correspond to higher price point

table(titanic.full$AgeGroup, titanic.full$Deck)
## we can confirm that older people prefer higher decks/cabins

table(titanic.full$AgeGroup, titanic.full$Survived)
## the older you are the less likely you were to survive

table(titanic.full$TravelGroup, titanic.full$Deck)
## bigger groups tend to travel in higher decks

table(titanic.full$AgeGroup, titanic.full$TravelGroup)
## older people tend to travel in smaller groups, that can be explained that larger groups  very often consist of kids

table(titanic.full$AgeGroup, titanic.full$Other)
## the older the people the more they prefer to travel alone or with spouse vs friends

table(titanic.full$Title, titanic.full$Deck)
## Dr prefers higher decks, while Mr, Mrs, Miss tent to pick in B-D decks

table(titanic.full$Title, titanic.full$Embarked)

table(titanic.full$Title, titanic.full$Deck, titanic.full$Survived)
## Mr from higher decks tend to perish

table(titanic.full$Title, titanic.full$Embarked)

table(titanic.full$Embarked, titanic.full$Deck)
## people who boarded in Q mostly don't have a Cabin data; 
## availble cabin data for lower decks was recorded only at S port

table(titanic.full$TravelGroup, titanic.full$Embarked)
## very interesting, from Q boarded only singles and groups up to 3 people 
## could be that their tickets were specfic to smaller cabin sizes?
## up to 5 people boarded from C, and all the bigger groups boarded from S 
## perhaps this happened due to availability; also, did anyone share cabins with strangers?

### repeat  aggregate(Survived ~ Pclass + Deck, data=filter(titanic.full, Deck !=''),FUN = function(x) {sum(x)/length(x)})
aggregate(Survived ~ Pclass + Deck, data=filter(titanic.full, Deck !=''),FUN = function(x) {sum(x)/length(x)})
## we select only columns from train set that we think we can use in our model
## then we split our new dataset into two chunks, one to create a model
## and then test it on a train set


### titanic.full$FemSvv <- NA
### titanic.full$FemSvv <- ifelse(titanic.full$Sex == "female" & titanic.full$Survived == 1, 1, 0)
### titanic.full$FemSvv <- ifelse(is.na(titanic.full$FemSvv==T), 1, titanic.full$FemSvv)

### titanic.full$FemSvvPercent <- NA
### titanic.full$FemSvvPercent <- ifelse(titanic.full$FemSvv ==0, 0, ave(titanic.full$FemSvv, titanic.full[, "TixNum"], FUN= sum)/ave(titanic.full$Sex == "female", titanic.full[, "TixNum"], FUN= sum))

## I want to try and fill some missing Deck Info

deck.df <- subset(titanic.full, Deck != '' | is.na(Deck == T))
deck <- deck.df[, c("Survived", "PassengerId", "Cabin", "Embarked", "TixNum", "TixText", "FarePP", "Deck", "LName")] 

## let's try and fill those missing values from our df that has all available deck info

for(i in 1:dim(titanic.full)[1]){
  
  x <- integer()
  for(x in 1:dim(deck)[1]){
  
  if(titanic.full$TixNum[i] == deck$TixNum[x]) {
    titanic.full$Deck[i] <- deck$Deck[x]
  }
  }
}


table(titanic.full$Deck)
## This way we predicted deck for additional 23 passengers

titanic.full$Deck[titanic.full$CabinBg ==9] <- "G"
## I want to try to assign same cabins? to the people on the same tix


### for(i in 1:dim(titanic.full)[1]){
  
  x <- integer()
  for(x in 1:dim(deck)[1]){
    
    if(titanic.full$TixNum[i] == deck$TixNum[x]) {
      titanic.full$Cabin[i] <- deck$Cabin[x]
    }
  } ### some of the same tix have different cabins mentioned
  
combinations <- data.frame()
combinations <- expand(titanic.full, nesting(TixNum, Cabin))
fc<-filter(combinations, nchar(TixNum)==5) ## checking what is common of anything between tickets of the same lenght

## so we basically filled empty cabin info and deck info from tix data

table(titanic.full$Deck, titanic.full$TixText)
titanic.full$TixText[titanic.full$Deck %in% c("A", "B")] <- "PC"
## titanic.full$TixText[titanic.full$Deck %in% c("A", "B")] <- "PC"

titanic.full$TixBg <- regmatches(titanic.full$TixNum, regexpr("\\d", titanic.full$TixNum))
table(titanic.full$TixBg, titanic.full$Deck, titanic.full$Pclass)
### firstclass <- filter(titanic.full, Pclass ==2 & TixBg !=2)



train <- titanic.full[c("Survived", "Sex", "Pclass", "Age", "Embarked", "Group", "FarePP", "AgeGroup", "Deck", "TixNum")]
str(train)
train$Sex = factor(train$Sex)
train$Pclass = factor(train$Pclass)
train$Survived = factor(train$Survived)
train$Embarked = factor(train$Embarked)
train$Group = factor(train$Group)
train$TixNum <- as.numeric(train$TixNum)

set.seed(129)

test_og <- filter(train, is.na(Survived==T))
train_og <- filter(train, Survived == 0 |Survived == 1 )

split = sample.split(train_og$Survived, SplitRatio =0.8)

fit <- subset(train_og, split ==T)
test <- subset(train_og, split ==F) 

## now we create a model
model <- glm(Survived ~ ., family = binomial(link='logit'), data = fit )

model <- step(model)

## by using summary() we obtain results of our model
summary(model)

## next we run anova() function to analyze the deviance
anova(model, test = "Chisq")

## Now we are going to assess the predictive ability of our model
prob_pred <- predict(model, newdata = test, type = 'response') 
y_pred <- ifelse(prob_pred > 0.5,1,0)

table(test$Survived, y_pred >0.5) ## confusion matrix

error <- mean(y_pred != test$Survived) ## missclasification error
paste('Accuracy', round(1 - error, 4)) ## 

## However, keep in mind that this result is somewhat dependent on the manual split of the data 
## that I made earlier, therefore if you wish for a more precise score, 
## you would be better off running some kind of cross validation such as k-fold cross validation.
## As a last step, we are going to plot the ROC curve and calculate the AUC (area under the curve) 
## which are typical performance measurements for a binary classifier.
## The ROC is a curve generated by plotting the true positive rate (TPR) against the false positive rate
## (FPR) at various threshold settings while the AUC is the area under the ROC curve. As a rule of thumb,
## model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5.

fit_pred <- prediction(prob_pred, test$Survived)
fit_perf <- performance(fit_pred, "tpr", "fpr")
plot(fit_perf, col = "green", lwd =2, main = "ROC Curve")
abline(a=0, b=1, lwd = 2, lty =2, col = "grey")

auc <- performance(fit_pred, "auc")
auc <- auc@y.values[[1]]

round(auc, 4)

## we got pretty good result auc = 0.9149
## the closer it is to 1 - the better

##  now we predict test set results
prob_pred <- predict(model, newdata = test_og)
y_pred <-ifelse(prob_pred > 0.5, 1, 0)
results <- data.frame(PassengerID = c(892:1309), Survived = y_pred)
write.csv(results, file = "TitanicGlmPrediction 0107 09149.csv", row.names = F, quote = F)

### Decision Tree

model <- rpart(Survived~., data = fit, method = "class")
rpart.plot (model, extra =4)

y_pred = predict(model, newdata = test[, -which(names(test)== "Survived")], type ="class")
table(test$Survived, y_pred)

error <- mean(y_pred != test$Survived) ## missclasification error
paste('Accuracy', round(1 - error, 4)) ##

## our accuracy went down

### Random Forest

set.seed(737)
model = randomForest(Survived~., data = fit) 

plot(model)
## Our model has a smaller error predicting death vs survival, while verall error averages put @ ~17%
## we will leave a default of 500 random trees as our choice

y_pred = predict(model, newdata = test[, -which(names(test)== "Survived")], type ="class")
table(test$Survived, y_pred)

error <- mean(y_pred != test$Survived) ## missclasification error
paste('Accuracy', round(1 - error, 4)) 

## while our accuracy improved a bit, randomForest suffers in terms of interpretability 
## vs Decision Tree which is very visual
## let's plot mean Gini index to define important features

gini = as.data.frame(importance(model))
gini = data.frame(Feature = row.names(gini),
                  MeanGini = round(gini[, "MeanDecreaseGini"], 2))
gini = gini[order(-gini[, "MeanGini"]),]

ggplot(gini, aes(reorder(Feature, MeanGini), MeanGini, group =1)) +
  geom_point(color = "red", shape = 17, size = 2) +
  geom_line(color = "blue", size = 1) +
  scale_y_continuous(breaks = seq(0, 60, 10)) +
  xlab("Feature") +
  ggtitle("Mean Gini Index of Features") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



rm(list = ls())

