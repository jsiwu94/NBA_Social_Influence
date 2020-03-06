

library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(dummies)
library(faraway)

#reading the dat
dat <- read.csv("~/Documents/NBA_Social_Influence/final_master_data_file.csv")
head(dat)

#getting rid of the NA rows
dat.full <- na.omit(dat)


#############Initial VISUALIZATION#############
#looking at the dist
dat.full %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


#looking at salary by position
ggplot(aes(x=POSITION,y=SALARY),data=dat.full)+
      geom_violin(trim=FALSE)+
  ggtitle("Salary Range Based on Position")+
      geom_boxplot(width=0.1, fill = "tan1")

#looking at salary by MP
dat_sal <- dat_sal[!(is.na(dat_sal$SALARY)),]
dat_sal['MP_above_avg'] <- ifelse(dat_sal$MP > mean(dat_sal$MP),1,0)
ggplot(aes(x=as.factor(MP_above_avg),y=SALARY),data=dat_sal)+
  geom_violin(trim=FALSE)+
  ggtitle("Salary Range for Below vs Above Average Minute Played")+
  geom_boxplot(width=0.1, fill = "lightskyblue3")

t.test(SALARY~MP_above_avg, data=dat_sal) ##P value showed significant differences


#adding some variables & one hot encoding
filter_df <- select(dat.full, -c(PLAYER,TEAM,Rk, pageview_sum,twitter_fav_group))
filter_df <- dummy.data.frame(filter_df, sep = '_')

head(filter_df)

#scaling var: salary, twitter, pageview
filter_df$SALARY <- (dat.full$SALARY-mean(dat.full$SALARY))/sd(dat.full$SALARY)
filter_df$TWITTER_FAVORITE_COUNT <- log(dat.full$TWITTER_FAVORITE_COUNT+1)
filter_df$TWITTER_RETWEET_COUNT <- log(dat.full$TWITTER_RETWEET_COUNT+1)
filter_df$pageview_mean <- log(dat.full$pageview_mean)


options("scipen"=100, "digits"=4)
##Overall Trash Model
ols <- lm(WINS~.,data=filter_df)
summary(ols)

col <- vif(ols)
col[col > 10]

###########OLS MODELS : DO RESIDUAL PLOTS FOR EACH MODELS
##Social Media Model
soc_med <- lm(WINS~TWITTER_FAVORITE_COUNT+TWITTER_RETWEET_COUNT+pageview_mean,data=filter_df)
summary(soc_med)
vif(soc_med)

##Salary Model
Salary <- lm(WINS~SALARY,data=filter_df)
summary(Salary)

##Time Model
Minute <- lm(WINS~MP,data=filter_df)
summary(Minute)
######Do the MP vs WINS by Position plot--> use encircle

##Defense Model
defense_df <- filter_df[,c("DRB","STL","BLK","WINS")]
defense <- lm(WINS~.,data=defense_df)
summary(defense)

##Offense Model
offense_df <- filter_df[,c("eFG.","FT.","ORB","AST","PS.G","WINS")]
offense <- lm(WINS~.,data=offense_df)
summary(offense)


#######PROPENSITY SCORE BASED ON MINUTE PLAY
# is there differences in salary based on minute play?
summary(dat_sal$MP)
cor(dat_sal$MP, dat_sal$SALARY)
table(dat_sal$MP_above_avg)


#####What is the effect of minute play in increasing the salary (use interaction term)
#vars that affect salary so far = PS/G, FG, FGA, 2P, FT

