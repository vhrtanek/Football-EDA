library(ggplot2)
library(ggthemes)
library(corrgram)
library(corrplot)
library(dplyr)
library(xlsx)

setwd("C:/Users/s97gy9/R/Datasets/")

football <- read.csv(file="FootballEurope.csv")


colnames(football)

#FT Full Time
#HT Half Time


football$homeYear <- sapply(football$date, function(y) as.numeric(format(as.Date(y, format='%Y-%m-%d'),"%Y")))
football$homeMonth <- sapply(football$date, function(y) as.numeric(format(as.Date(y, format='%Y-%m-%d'),"%m")))
football$homeDay <- sapply(football$date, function(y) as.numeric(format(as.Date(y, format='%Y-%m-%d'),"%d")))

football$awayYear <- sapply(football$date, function(y) as.numeric(format(as.Date(y, format='%Y-%m-%d'),"%Y")))
football$awayMonth <- sapply(football$date, function(y) as.numeric(format(as.Date(y, format='%Y-%m-%d'),"%m")))
football$awayDay <- sapply(football$date, function(y) as.numeric(format(as.Date(y, format='%Y-%m-%d'),"%d")))

football$Totalgoal <- football$homeGoalFT + football$awayGoalFT
football$Over2.5 <- ifelse(football$Totalgoal > 2, "Yes", "No")
football$homeResult <- ifelse(football$homeGoalFT > football$awayGoalFT, "Win", ifelse(football$homeGoalFT < football$awayGoalFT, "Lose", "Draw" ))

head(football)
str(football)

#get half time data
#away_col <- sapply(colnames(football), function (x) grepl('^away', x)

half_time <- sapply(colnames(football), function (x) grepl('HT$', x))
half_time

hf <- football[,half_time]
head(hf)
#detect outliers
library(outliers)

num_col <- sapply(hf, is.numeric)

#naive approach, all outliers are replaced by median value
temp <- hf[,num_col]
outliers <- outlier(temp)
temp <- rm.outlier(temp, fill = T, median = T, opposite = F)

#Improvement - replace outliers with team, home/away median or coerce values into 95% percentile
hf1 <- temp

hf1$TotalGoas <- football$Totalgoal
hf1$Year <- football$homeYear
hf1$Month <- football$homeMonth
hf1$Day <- football$homeDay
hf1$Over2.5 <- football$Over2.5
hf1$homeTeam <- football$homeTeam
hf1$awayTeam <- football$awayTeam
hf1$division <- football$division
hf1$homeResult <- football$homeResult

str(hf1)

data.frame(hf1 %>%
          group_by(division, Over2.5) %>% 
          summarise(no.rows = length(Over2.5))) %>%
          arrange(no.rows)
sum_o2.5


data.frame(hf1 %>%
           group_by(division, homeResult) %>%
           summarise(no.rows = length(homeResult))) %>%
           arrange(division)
homeWinsByDiv

data.frame(homeWinsByDiv %>%
           group_by(division) %>%
           mutate(countD = sum(no.rows)) %>%
           group_by(homeResult, add = T) %>%
           mutate(per=paste0(round(100*no.rows/countD,2),'%')))
homeWinsByDiv1

data.frame(hf1 %>%
          group_by(homeTeam, Year, homeResult) %>%
          summarise(no.rows = length(homeResult))) %>%
          arrange(homeTeam)

homeWinsByTeam

homeWinsByTeam1 <- data.frame(homeWinsByTeam %>%
                               group_by(homeTeam, Year) %>%
                               mutate(PlayedGames = sum(no.rows)) %>%
                               group_by(homeResult, add = T) %>%
                               mutate(percentage=round(100*no.rows/PlayedGames,2))) %>%
                               filter(Year ==  2017) %>%
                               arrange(desc(percentage))
homeWinsByTeam1


pl <- ggplot(sum_o2.5, aes(x = division, y = no.rows, fill = Over2.5, label =no.rows)) + 
  geom_histogram(stat = "identity") +
  geom_text(aes( vjust=1.5, label = no.rows))
print(pl)
str(hf1)

#handling missing values for home_num dataset as part of data preparation for correlation excercies
library(mice)
library(VIM)

# delete columns with more than 30% missing values
hf1$awayDribbleSuccessHT <- NULL
hf1$awayOffsidesCaughtHT <- NULL
hf1$homeDribbleSuccessHT <- NULL

mice_plot <- aggr(hf1, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(hf1), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

temp <- mice(hf1, m=1)
hf2 <- complete(temp, 1)
temp <- NULL

# Case of still NA after mice
hf2[!complete.cases(hf2),]
a <- median(hf2$homeDribbledPastHT,  na.rm = T)
hf2[is.na(hf2$homeDribbledPastHT),"homeDribbledPastHT"] <- a
b <- median(hf2$awayDribbledPastHT,  na.rm = T)
hf2[is.na(hf2$awayDribbledPastHT),"awayDribbledPastHT"] <- b


# find correlation, chi-square ...
str(hf2)
num_col <- sapply(hf2, is.numeric)
hf3 <- hf2[,num_col]
hf3$Over2.5 <- as.factor(hf2$Over2.5)
#hf3$homResult <- NULL
hf3$homeResult <- as.factor(hf2$homeResult)

cor.home <- cor(hf3)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor.home, method='color', type = "upper",  tl.col = "black")

hf3$TotalGoas <- NULL
hf3$Over2.5 <- NULL
# No goal stats
hf3$homeGoalHT <- NULL
hf3$awayGoalHT <- NULL

hf3$homeGoalHT <- football$homeGoalHT
hf3$awayGoalHT <- football$awayGoalHT

hf3$homeTeam <- football$homeTeam
hf3$awayTean <- football$awayTeam

library(caTools)
set.seed(101)
sample <- sample.split(hf3$homeResult, SplitRatio = .7)
train <- subset(hf3, sample ==T)
test <- subset(hf3, sample == F)

###########################################################
#random forest
###########################################################
library(randomForest)
model <- randomForest(homResult ~ .,   data=train, importance=TRUE)
model

#print(model$importance)

rf.pred <- predict(model, test)
print(table(rf.pred,test$homeResult))

###################################################################
# AdaBoost 
######################################################################

library(adabag)
library(fastAdaboost)
library(tree)


train$homeResult <- as.factor(train$homeResult)
train$homeWin <- ifelse (train$homeResult == "Win", "Win", "Lose/Draw")
train$homeResult <- NULL


test$homeResult <- as.factor(test$homeResult)
test$homeWin <- ifelse (test$homeResult == "Win", "Win", "Lose/Draw")
test$homeResult <- NULL

str(train)
fastada_model <- adaboost(homeWin ~., train, 10)
print(fastada_model)
get_tree(fastada_model, 3)
fastada_pred <- predict(fastada_model, newdata = test)
#FastAdaBoost error rate 30%
fastada_pred$error


ada_model <- boosting(homeResult ~., data = train, boos = T, mfinal = 20, coeflearn = 'Breiman')
ada_model$importance
eead1 <- errorevol(ada_model, train)
ada_pred <- predict(ada_model, test)
eead2 <- errorevol(ada_model, test)
ada_pred
plot.errorevol(eead1, eead2)
eead1

t1<-ada_model$trees[[1]]
plot(t1)
text(t1,pretty = 0)

#################################################################################################


#Bestpairs<-
football %>% 
            group_by(division, homeTeam, awayTeam) %>%
            summarise(mean_goals = mean(Totalgoal, na.rm=T),  played_games = n()) %>%
            #filter(mean_goals > 2.5 ) %>%
            #filter(played_games > 2) %>%
            filter(homeTeam == "Arsenal" ) %>%
            #filter(awayTeam == "Arsenal" ) %>%
            #filter(division == "Bundesliga" ) %>%
            arrange(desc(mean_goals))
Bestpairs  

write.table(Bestpairs, "C:/Users/s97gy9/R/Datasets/BestPairs.txt", row.names = F, sep = "\t")

football %>%
            group_by(homeTeam) %>%
            summarise(mean_goals= mean(homeGoalFT, na.rm=T)) %>%
            #filter(homeTeam > 2.5 ) %>%
            filter(homeTeam == "Arsenal" ) %>%
            arrange(desc(mean_goals))
Besthome

football %>%
            group_by(awayTeam) %>%
            summarise(mean_goals= mean(awayGoalFT, na.rm=T),played_games = n()) %>%
            filter(played_games > 50 ) %>%
            filter(awayTeam == "FC Cologne" ) %>%
            arrange(mean_goals) 
WorstAway

BestAway <- football %>%
  group_by(awayTeam) %>%
  summarise(mean_goals= mean(awayGoalFT, na.rm=T),played_games = n()) %>%
  filter(played_games > 50 ) %>%
  #filter(awayTeam == "Eintracht Frankfurt" ) %>%
  arrange(desc(mean_goals))
BestAway

besthometeams <- head(Besthome$homeTeam,50)
worstawayteams <- head(WorstAway$awayTeam, 50)



SGR <- football %>%
  group_by(division, homeTeam) %>%
  summarise(homeSGR = mean(homeShotsTotalFT)/mean(homeGoalFT)) %>%
  arrange(homeSGR)

SGR

STGR <- football %>%
  group_by(division, homeTeam) %>%
  summarise(homeSGTR = mean(homeShotsOnTargetFT, na.rm=T)/mean(homeGoalFT)) %>%
  arrange(homeSGTR)

STGR





