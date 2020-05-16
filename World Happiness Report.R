
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(stringr)) install.packages("stringr")
if(!require(forcats)) install.packages("forcats")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(anytime)) install.packages("anytime", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(caTools)) install.packages("caTools")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(reshape2)) install.packages("reshape2")
if(!require(data.table)) install.packages("data.table")
if(!require(corrgram)) install.packages("corrgram")
if(!require(corrplot)) install.packages("corrplot")
if(!require(formattable)) install.packages("formattable")
if(!require(cowplot)) install.packages("cowplot")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(plotly)) install.packages("plotly")
if(!require(countrycode)) install.packages("countrycode")
if(!require(leaflet)) install.packages("leaflet")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(maps)) install.packages("maps")
if(!require(rworldmap)) install.packages("rworldmap")
if(!require(ggalt)) install.packages("ggalt")

# Loading all needed libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(forcats)
library(kableExtra)
library(caret)
library(anytime)
library(tidyverse)
library(lubridate)
library(plyr)
library(lubridate)
library(caTools)
library(ggthemes)
library(reshape2)
library(data.table)
library(corrgram)       
library(corrplot)
library(formattable)
library(cowplot)
library(ggpubr)
library(plotly)
library(countrycode)  # Gets country code 
library(leaflet)      # Interactive maps
library(rworldmap)
library(RColorBrewer) 
library(maps)
library(ggalt)



web <- "https://storage.googleapis.com/kaggle-data-sets/894/813759/bundle/archive.zip?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1589735972&Signature=KmkZOIm4EjH%2BTnC%2FxvYalGbhKrdRBntnFJ2%2FoApTxajfp3MXGpd4qHUfMUruUGq2F%2FC2HhN11l6zIH%2B7Ifju72fn8U29BScOe0Mf2KZjT3XLNE7M1zFKb3gsoL%2BmwqDpmcx3Dk0S92oPeYAFW9DtACIA8hQ9mbXdhYBKWtf67i299qs77HkPfIThYT8CYcsricI1PSueGEh7njOme6OL9%2BpKwZZICJk6ByeVcCCZOtF5i7LrimWyEVLK%2BGXi%2FwGuRGvIbgstlzELcwsV6FQI0KxaaDUMP8c8ynqgmzkVdnybDZhgI8Sy7%2B%2FaFfGuTwhRi8Z0EEEhHu4FwZIKmwt3Nw%3D%3D&response-content-disposition=attachment%3B+filename%3Dworld-happiness.zip"
download.file(web, "myFile.zip", quiet=TRUE,mode="wb", method='curl')
unzip("myFile.zip")

file.remove("myFile.zip")
Happiness <- read.csv('2019.csv')
Happiness_2015 <- read.csv('2015.csv')


# Changing the name of columns
colnames (Happiness) <- c( "Happiness.Rank","Country", "Happiness.Score",
                           "Economy", "Social.Support" ,
                          "Life.Expectancy", "Freedom", "Generosity",
                          "Trust")


Happiness$Continent <- NA

Happiness$Continent[which(Happiness$Country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                                   "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                                   "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                                   "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                                   "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                                   "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                                   "Cambodia", "Afghanistan", "Yemen", "Syria"))] <- "Asia"
Happiness$Continent[which(Happiness$Country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                                   "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                                   "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                                   "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                                   "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                                   "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                                   "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                                   "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                                   "Bulgaria", "Albania", "Ukraine"))] <- "Europe"
Happiness$Continent[which(Happiness$Country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                                   "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                                   "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                                   "Haiti"))] <- "North America"
Happiness$Continent[which(Happiness$Country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                                   "Colombia", "Ecuador", "Bolivia", "Peru",
                                                   "Paraguay", "Venezuela"))] <- "South America"
Happiness$Continent[which(Happiness$Country %in% c("New Zealand", "Australia"))] <- "Australia"
Happiness$Continent[which(is.na(Happiness$Continent))] <- "Africa"



# Changing Continent column to factor

Happiness$Continent <- as.factor(Happiness$Continent)

str(Happiness)



# Changing the name of columns
colnames (Happiness_2015) <- c("Country","Region", "Happiness.Rank", "Happiness.Score",
                               "Standard.Error", "Economy", "Family",
                               "Life.Expectancy", "Freedom", "Trust","Generosity",
                                "Dystopia.Residual")


Happiness_2015$Continent <- NA

Happiness_2015$Continent[which(Happiness_2015$Country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                                   "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                                   "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                                   "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                                   "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                                   "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                                   "Cambodia", "Afghanistan", "Yemen", "Syria"))] <- "Asia"
Happiness_2015$Continent[which(Happiness_2015$Country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                                   "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                                   "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                                   "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                                   "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                                   "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                                   "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                                   "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                                   "Bulgaria", "Albania", "Ukraine"))] <- "Europe"
Happiness_2015$Continent[which(Happiness_2015$Country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                                   "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                                   "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                                   "Haiti"))] <- "North America"
Happiness_2015$Continent[which(Happiness_2015$Country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                                   "Colombia", "Ecuador", "Bolivia", "Peru",
                                                   "Paraguay", "Venezuela"))] <- "South America"
Happiness_2015$Continent[which(Happiness_2015$Country %in% c("New Zealand", "Australia"))] <- "Australia"
Happiness_2015$Continent[which(is.na(Happiness_2015$Continent))] <- "Africa"



# Changing Continent column to factor

Happiness_2015$Continent <- as.factor(Happiness_2015$Continent)

str(Happiness_2015)


hist(Happiness$Happiness.Score, freq=TRUE, col="black", border="white", 
     main="2019 Happiness Scores", xlab="Happiness.Score", ylab="Count")


Happiness %>%
  ggplot(aes(Happiness.Score)) +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack")

#--------------------------------------



d15<-Happiness_2015 %>% select(Country,Continent,HS15=Happiness.Score)
d19<-Happiness %>% select(Country,Continent,HS19=Happiness.Score)

score<-inner_join(d15,d19)%>% mutate(score_diff= HS19-HS15)%>% filter(score_diff>0)



score$Country <- factor(score$Country, levels=as.character(score$Country))
gg <- ggplot(score, aes(x=HS15, xend=HS19, y=Country, group=Country)) + 
  geom_dumbbell(size=2, color="#e3e2e1", 
                colour_x = "#5b8124", colour_xend = "#bad744",
                dot_guide=TRUE, dot_guide_size=0.25) + 
  labs(x=NULL, 
       y=NULL, 
       
       title=" Country Happiness Scores Increased: 2015 vs 2019"
  ) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)



#Plot of country distribution 

q<-map_data("world")
colnames(q)[5] <- "Country"
df<- left_join(q,Happiness)

g1 <- ggplot()  +
  geom_polygon( aes(x = df$long, y = df$lat, group = df$group,fill= df$Happiness.Score)) + 
  coord_equal() +scale_fill_gradient(breaks=c(3,5,7,9)) +
  ggtitle("Happiness score ")+
  xlab("") + ylab("") + guides(shape=FALSE) + labs(fill="Happiness Score")

g2 <- ggplot()  +
  geom_polygon( aes(x = df$long, y = df$lat, group = df$group,fill= df$Freedom)) + 
  coord_equal() +scale_fill_gradient( low = "white", high = "black") +
  ggtitle("Freedom ")+
  xlab("") + ylab("") + guides(shape=FALSE) + labs(fill="Freedom")

g3 <- ggplot()  +
  geom_polygon( aes(x = df$long, y = df$lat, group = df$group,fill= df$Generosity)) + 
  coord_equal() +scale_fill_gradient( low = "white", high = "red") +
  ggtitle("Generosity")+
  xlab("") + ylab("") + guides(shape=FALSE) + labs(fill="Generosity")

g4 <- ggplot()  +
  geom_polygon( aes(x = df$long, y = df$lat, group = df$group,fill= df$Trust)) + 
  coord_equal() +scale_fill_gradient( low = "white", high = " dark green") +
  ggtitle("Trust")+
  xlab("") + ylab("") + guides(shape=FALSE) + labs(fill="Trust")

g5 <- ggplot()  +
  geom_polygon( aes(x = df$long, y = df$lat, group = df$group,fill= df$Economy)) + 
  coord_equal() +scale_fill_gradient( low = "white", high = "brown") +
  ggtitle("Economy")+
  xlab("") + ylab("") + guides(shape=FALSE) + labs(fill="Economy")

g6 <- ggplot()  +
  geom_polygon( aes(x = df$long, y = df$lat, group = df$group,fill= df$Life.Expectancy)) + 
  coord_equal() +scale_fill_gradient( low = "white", high = "#800080") +
  ggtitle("Life.Expectancy")+
  xlab("") + ylab("") + guides(shape=FALSE) + labs(fill="Life.Expectancy")

ggarrange(g1, g2,g3, g4,g5, g6, ncol = 1, nrow = 6)

#table with average happiness per region
avg_happiness_region <-Happiness %>%
  group_by(Continent) %>%          
  summarise(avg_happiness = mean(Happiness.Score, round(1)))

#Plotting the average happiness scores to compare regions
ggplot(avg_happiness_region, aes(y=avg_happiness, x=Continent)) + 
  geom_bar( stat="identity") + theme_bw() + geom_line(aes(y = mean(Happiness$Happiness.Score)), size = 1.5, color="red", group = 1 ,name = "world av")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average value of happiness variables for different continents", 
       y = "Average value") 



########## Correlation between variables

# Finding the correlation between numerical columns
Num.cols <- sapply(Happiness, is.numeric)
Cor.data <- cor(Happiness[, Num.cols])

corrplot(Cor.data, method = 'color')  


# Create a correlation plot
#newdatacor = cor(Happiness[c(4:11)])
#corrplot(newdatacor, method = "number")

## Comparing different continents regarding their happiness variables
Happiness.Continent <- Happiness %>%
  group_by(Continent) %>%
  summarise(Economy= mean(Economy), Social.Support = mean(Social.Support),
               Life.Expectancy = mean(Life.Expectancy), Freedom = mean(Freedom),
               Generosity = mean(Generosity), Trust = mean(Trust))


Happiness.Continent.melt <- melt(Happiness.Continent)


# Faceting
ggplot(Happiness.Continent.melt, aes(y=value, x=Continent, color=Continent, fill=Continent)) + 
  geom_bar( stat="identity") +    
  facet_wrap(~variable) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average value of happiness variables for different continents", 
       y = "Average value") 

## Correlation plot for each continent 

corrgram(Happiness %>% select(-3) %>% filter(Continent == "Africa"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for Africa")


corrgram(Happiness %>% select(-3) %>% filter(Continent == "Asia"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for Asia")


corrgram(Happiness %>% select(-3) %>% filter(Continent == "Europe"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for Europe")

corrgram(Happiness %>% select(-3) %>% filter(Continent == "North America"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for North America")

corrgram(Happiness %>% select(-3) %>% filter(Continent == "South America"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for South America")

## Happiness score comparison on different continents
####### Happiness score for each continent

gg1 <- ggplot(Happiness,
              aes(x=Continent,
                  y=Happiness.Score,
                  color=Continent))+
  geom_point() + theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8)))

gg2 <- ggplot(Happiness , aes(x = Continent, y = Happiness.Score)) +
  geom_boxplot(aes(fill=Continent)) + theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8)))

gg3 <- ggplot(Happiness,aes(x=Continent,y=Happiness.Score))+
  geom_violin(aes(fill=Continent),alpha=0.7)+ theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8)))

# Compute descriptive statistics by groups
stable <- desc_statby(Happiness, measure.var = "Happiness.Score",
                      grps = "Continent")
stable <- stable[, c("Continent","mean","median")]
names(stable) <- c("Continent", "Mean of happiness score","Median of happiness score")
# Summary table plot
stable.p <- ggtexttable(stable,rows = NULL, 
                        theme = ttheme("classic"))


ggarrange(gg1, gg2, ncol = 1, nrow = 2)


## Scatter plot with regression line  
ggplot(subset(Happiness, Happiness$Continent != "Australia"), aes(x = Life.Expectancy, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")



ggplot(subset(Happiness, Happiness$Continent != "Australia"), aes(x = Economy, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")



ggplot(subset(Happiness, Happiness$Continent != "Australia"), aes(x = Freedom, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")



ggplot(subset(Happiness, Happiness$Continent != "Australia"), aes(x = Trust, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")



ggplot(subset(Happiness, Happiness$Continent != "Australia"), aes(x = Generosity, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")


## Scatter plot colored by Continents 

ggplotRegression <- function (fit) {
  
  
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(shape=1,size=3,color="#003399")+
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("R2 = ",signif(summary(fit)$r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


gg2<-ggplotRegression(lm(Happiness.Score ~ Generosity, data = Happiness))
gg3<-ggplotRegression(lm(Happiness.Score ~ Freedom, data = Happiness))
gg4<-ggplotRegression(lm(Happiness.Score ~ Economy, data = Happiness))

gg6<-ggplotRegression(lm(Happiness.Score ~ Life.Expectancy, data = Happiness))
gg7<-ggplotRegression(lm(Happiness.Score ~ Trust, data = Happiness))


ggarrange(gg2,gg3,ncol=1,nrow=2)
ggarrange(gg4,gg6,ncol=1,nrow=2)
ggarrange(gg7,ncol=1,nrow=2)

#----

# Methods

## Model 1: The Sum of Factors

# find  predicted score by sum method and calculate the corresponding RMSE
data <- read.csv('2019.csv')
sum_model <- data %>% mutate(pred_score = GDP.per.capita +
                               Social.support +
                               Healthy.life.expectancy +
                               Freedom.to.make.life.choices + 
                               Generosity + 
                               Perceptions.of.corruption + 
                               1.85, 
                             RMSE = RMSE(Score, pred_score))

# show top results of the summation model
sum_model %>%
  filter(Overall.rank <= 5) %>%
  select(Overall.rank, Country.or.region, Score, pred_score, RMSE)


# save RMSE for the first model
mod1_rmse <- RMSE(sum_model$Score, sum_model$pred_score)


# calculate the missing dystopian residuals
sum_model <- sum_model %>% mutate(residual = Score - pred_score)

# show top results of the summation model
sum_model %>%
  filter(Overall.rank <= 5) %>%
  select(Overall.rank, Country.or.region, Score, pred_score, RMSE, residual)

## Model 2: The 2019 GLM Model

# --- test for an appropriate ratio in data partitioning
# a sequence of p's we want to test
ps <- seq(from=.30, to=.90, by=.01)

# calculate RMSEs for each p value
rmses <- sapply(ps, function(p){
  train_index <- createDataPartition(data$Score, times=1, p=p, list=FALSE)
  train <- data[train_index,]
  test <- data[-train_index,]
  fit <- glm(Score ~ GDP.per.capita +
               Social.support +
               Healthy.life.expectancy + 
               Freedom.to.make.life.choices + 
               Generosity + 
               Perceptions.of.corruption, 
             data = train)
  test <- test %>% mutate(pred_score = predict.glm(fit, newdata=test))
  RMSE(test$Score, test$pred_score)
})

# no real clear winner in terms of best accuracy in probabilities
plot(ps, rmses)


# set seed to keep partitioning consistent
set.seed(1, sample.kind = "Rounding")

# ----- Data partitioning -----
train_index <- createDataPartition(data$Score, times=1, p=0.70, list=FALSE)
train <- data[train_index,]
test <- data[-train_index,]

# --- fit our glm model, caret::glm
fit <- glm(Score ~ GDP.per.capita +
             Social.support +
             Healthy.life.expectancy + 
             Freedom.to.make.life.choices + 
             Generosity + 
             Perceptions.of.corruption, 
           data = train)

# add predicted scores to a 'results' data frame
results <- test %>% 
  mutate(pred_score = predict.glm(fit, newdata=test))

results %>%
  select(Overall.rank, Country.or.region, Score, pred_score) %>%
  head()

# show bottom five observations
results %>%
  select(Overall.rank, Country.or.region, Score, pred_score) %>%
  tail()

# plot predicted scores vs actual scores
# also plot y = x line
ggplot(data = results, aes(Score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='red')

# save model 2 RMSE
mod2_rmse_gen <- RMSE(results$Score, results$pred_score)

# save coefficients to use in equation
c <- coefficients(fit)
c[] <- lapply(c, round, 3)

# print coefficients of fitted model
fit$coefficients

# fit model without generosity
fit <- glm(Score ~ GDP.per.capita +
             Social.support +
             Healthy.life.expectancy + 
             Freedom.to.make.life.choices + 
             Perceptions.of.corruption, 
           data = train)

# add predicted scores to a 'results' data frame
results <- test %>% 
  mutate(pred_score = predict.glm(fit, newdata=test))

# plot predicted scores vs actual scores
ggplot(data = results, aes(Score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='red')

# save rmse and coefficients
mod2_rmse_nogen <- RMSE(results$Score, results$pred_score)
c <- coefficients(fit)
c[] <- lapply(c, round, 3)

# print coefficients of fitted model
fit$coefficients

## Model 3: The 2018/19 GLM Model

# add 2018 data set and keep only used columns
data18 <- read.csv("2018.csv")
data18$Overall.rank <- NULL
data18$Country.or.region <- NULL

# remove unused columns in 2019 data frame for merging
data$Overall.rank <- NULL
data$Country.or.region <- NULL

# turn corruption column from factor to numeric, turn NAs to 0
data18$Perceptions.of.corruption <- as.numeric(as.character(data18$Perceptions.of.corruption))
data18[is.na(data18)] <- 0

# build full data set of both 2019, 2018 data
full_data <- rbind(data, data18)

# show full data set
glimpse(full_data)

# set seed to keep partitioning consistent
set.seed(1, sample.kind = "Rounding")

# partition data
train_index <- createDataPartition(full_data$Score, times=1, p=0.70, list=FALSE)
train <- full_data[train_index,]
test <- full_data[-train_index,]

# fit a model (including generosity)
fit <- glm(Score ~ GDP.per.capita +
             Social.support +
             Healthy.life.expectancy + 
             Freedom.to.make.life.choices + 
             Generosity +
             Perceptions.of.corruption, 
           data = train)

# add predicted scores to our 'results' data frame
results <- test %>% 
  mutate(pred_score = predict.glm(fit, newdata=test))

# plot predicted scores vs actual scores
ggplot(data = results, aes(Score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='red')

# save rmse and coefficients
mod3_rmse_gen <- RMSE(results$Score, results$pred_score)
c <- coefficients(fit)
c[] <- lapply(c, round, 3)


# print coefficients of fitted model
fit$coefficients

# fit a model (not including generosity)
fit <- glm(Score ~ GDP.per.capita +
             Social.support +
             Healthy.life.expectancy + 
             Freedom.to.make.life.choices + 
             Perceptions.of.corruption, 
           data = train)

# add predicted scores to our 'results' data frame
results <- test %>% 
  mutate(pred_score = predict.glm(fit, newdata=test))

# plot predicted scores vs actual scores
ggplot(data = results, aes(Score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='red')

# save rmse and coefficients
mod3_rmse_nogen <- RMSE(results$Score, results$pred_score)
c <- coefficients(fit)
c[] <- lapply(c, round, 3)

# print coefficients of fitted model
fit$coefficients

# compiles the list of RMSEs
rmse_list <- list(mod1_rmse,
                  mod2_rmse_gen,
                  mod2_rmse_nogen,
                  mod3_rmse_gen,
                  mod3_rmse_nogen)
rmse_list[] <- lapply(rmse_list, round, 3)

# find our predicted score and calculate the corresponding RMSE
sum_model <- full_data %>% mutate(pred_score = GDP.per.capita +
                                    Social.support +
                                    Healthy.life.expectancy +
                                    Freedom.to.make.life.choices + 
                                    Generosity + 
                                    Perceptions.of.corruption + 
                                    1.85, 
                                  RMSE = RMSE(Score, pred_score))

# save RMSE for the first model
mod1_rmse_full <- RMSE(sum_model$Score, sum_model$pred_score)

# recompiles the list of RMSEs
rmse_list <- list(mod1_rmse,
                  mod2_rmse_gen,
                  mod2_rmse_nogen,
                  mod3_rmse_gen,
                  mod3_rmse_nogen,
                  mod1_rmse_full)
rmse_list[] <- lapply(rmse_list, round, 3)


