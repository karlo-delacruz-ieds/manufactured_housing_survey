# read excel files
library(readxl)
puf2014 <- read_xlsx("data_sources/PUF2014.xlsx")
puf2015 <- read_xlsx("data_sources/PUF2015.xlsx")
puf2016 <- read_xlsx("data_sources/puf2016.xlsx")
puf2017 <- read_xlsx("data_sources/PUF2017.xlsx")

# colnames data cleaning
names(puf2014) <- tolower(names(puf2014))
names(puf2015) <- tolower(names(puf2015))
names(puf2016) <- tolower(names(puf2016))
names(puf2017) <- tolower(names(puf2017))

names(puf2014) <- gsub("control$","controlid", names(puf2014))
names(puf2015) <- gsub("control$","controlid", names(puf2015))
names(puf2016) <- gsub("control$","controlid", names(puf2016))
names(puf2017) <- gsub("control$","controlid", names(puf2017))

names(puf2014) <- gsub("jtitled","jtitle", names(puf2014))
names(puf2015) <- gsub("jtitled","jtitle", names(puf2015))
names(puf2016) <- gsub("jtitled","jtitle", names(puf2016))
names(puf2017) <- gsub("jtitled","jtitle", names(puf2017))

names(puf2014) <- gsub("locregin","region", names(puf2014))
names(puf2015) <- gsub("locregin","region", names(puf2015))
names(puf2016) <- gsub("locregin","region", names(puf2016))
names(puf2017) <- gsub("locregin","region", names(puf2017))

#

# merge to all data 
all_data <- rbind(puf2014,puf2015,puf2016,puf2017)
all_data <- as.data.frame(all_data)

# 9 into NA values
all_data[all_data == 9] <- NA
# write file
write.csv(all_data,"pufcomplete.csv",row.names=FALSE) # backup
all_data <- read.csv("pufcomplete.csv")

################
# subset region 3
ne_data <- all_data[all_data$region == 3,]
summary(ne_data)

# impute ne_data that has missing
ne_comp_data <- ne_data[complete.cases(ne_data),]

# write file
write.csv(ne_comp_data,"pufnecomplete.csv",row.names=FALSE) # backup
ne_comp_data <- read.csv("pufnecomplete.csv")
 
# convert to factor
library(tidyr)
ne_comp_data <- ne_comp_data %>% separate(shipmonth, into = c('year', 'month'), sep = 4)
ne_comp_data$year <- factor(ne_comp_data$year, ordered = TRUE)
ne_comp_data$month <- factor(ne_comp_data$month, ordered = TRUE)
ne_comp_data$controlid <- factor(ne_comp_data$controlid)
ne_comp_data$sections <- factor(ne_comp_data$sections, ordered = TRUE)
ne_comp_data$status <- factor(ne_comp_data$status)
ne_comp_data$bedrooms <- factor(ne_comp_data$bedrooms, ordered = TRUE)
ne_comp_data$titled <- factor(ne_comp_data$titled)
ne_comp_data$location <- factor(ne_comp_data$location)
ne_comp_data$foundation <- factor(ne_comp_data$foundation)
ne_comp_data$secured <- factor(ne_comp_data$secured)

ne_comp_data$jstatus <- factor(ne_comp_data$jstatus)
ne_comp_data$jprice <- factor(ne_comp_data$jprice)
ne_comp_data$jsqft <- factor(ne_comp_data$jsqft)
ne_comp_data$jbedroom <- factor(ne_comp_data$jbedroom)
ne_comp_data$jtitle <- factor(ne_comp_data$jtitle)
ne_comp_data$jlocation <- factor(ne_comp_data$jlocation)
ne_comp_data$jfoundation <- factor(ne_comp_data$jfoundation)
ne_comp_data$jsecured <- factor(ne_comp_data$jsecured)

# regression
ne_reg <- lm(price ~ ., data = ne_comp_data[,c(3,4,5,6,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)] )
forward <- step(ne_reg, scope = list(lower=~1,upper=~., direction = "forward", trace=1))
backward <- step(ne_reg, scope = list(lower=~1,upper=~., direction = "backward", trace=1))
both <- step(ne_reg, scope = list(lower=~1,upper=~., direction = "both", trace=1))

summary(forward)

#
hist(forward$residuals)
# correlation
cor(ne_comp_data[,c(6,7,10)])

#
sum(forward$residuals)
summary(ne_reg)
sum(ne_reg$residuals)
sum(backward$residuals)
sum(both$residuals)
summary(backward)
########### visualize

library(ggplot2)
ggplot(ne_comp_data, aes(x = factor(year), y = price)) +
  geom_boxplot() + 
  xlab("Year") +
  ylab("Housing Price") +
  ggtitle("Yearly Housing Price", subtitle = "Housing Prices at Year 2014 were less expensive") 
ggsave("img/Yearly Housing Price.jpg", width = 5)

levels(ne_comp_data$sections) <- c("Single","Double","3 or More")
ggplot(ne_comp_data, aes(x = factor(sections), y = price)) +
  geom_boxplot() + 
  xlab("Home Size") +
  ylab("Housing Price") +
  ggtitle("Size of Home", subtitle = "Single Section Houses are less expensive") 
ggsave("img/Size of Home.jpg", width = 5)

ggplot(ne_comp_data, aes(x = weight, y = price)) +
  geom_point(alpha = 0.5) + 
  xlab("Weight") +
  ylab("Housing Price") +
  ggtitle("Sample Weight", subtitle = "Housing Price decreases as Weight Increases") 
ggsave("img/Sample Weight.jpg", width = 5)

ggplot(ne_comp_data, aes(x = sqft, y = price)) +
  geom_point(alpha = 0.5) + 
  xlab("Square Foot") +
  ylab("Housing Price") +
  ggtitle("Square Footage of Home", subtitle = "Housing Price increase as Area increases") 
ggsave("img/Square Footage of Home.jpg", width = 5)


levels(ne_comp_data$bedrooms) <- c("2 or less","3 or More")
ggplot(ne_comp_data, aes(x = factor(bedrooms), y = price)) +
  geom_boxplot() + 
  xlab("Bedrooms") +
  ylab("Housing Price") +
  ggtitle("Number of Bedrooms", subtitle = "Bedrooms with 2 or less are less expensive") 
ggsave("img/Number of Bedrooms.jpg", width = 5)

levels(ne_comp_data$titled) <- c("Real Estate","Personal Prop","Not Titled")
ggplot(ne_comp_data, aes(x = factor(titled), y = price)) +
  geom_boxplot() + 
  xlab("Titled Type") +
  ylab("Housing Price") +
  ggtitle("How Home is Titled", subtitle = "Personal Prop Titles were less expensive") 
ggsave("img/How Home is Titled.jpg", width = 5)

levels(ne_comp_data$location) <- c("Inside manuf. home communities","Outside manuf. home communities")
ggplot(ne_comp_data, aes(x = factor(location), y = price)) +
  geom_boxplot() + 
  xlab("Location") +
  ylab("Housing Price") +
  ggtitle("Where a Home is Placed", subtitle = "Outside manuf. home communities are more expensive") 
ggsave("img/Where a Home is Placed.jpg", width = 5)

levels(ne_comp_data$secured) <- c("Tie down straps or others","Not secured")
ggplot(ne_comp_data, aes(x = factor(secured), y = price)) +
  geom_boxplot() + 
  xlab("Secured") +
  ylab("Housing Price") +
  ggtitle("How Home is Secured", subtitle = "Unsecured homes are less expensive") 
ggsave("img/How Home is Secured.jpg", width = 5)

levels(ne_comp_data$jprice) <- c("Imputed","Reported")
ggplot(ne_comp_data, aes(x = factor(jprice), y = price)) +
  geom_boxplot() + 
  xlab("JPrice") +
  ylab("Housing Price") +
  ggtitle("Imputed Indicator of Price", subtitle = "Reported price are less expensive") 
ggsave("img/Imputed Indicator of Price.jpg", width = 5)

levels(ne_comp_data$jlocation) <- c("Imputed","Reported")
ggplot(ne_comp_data, aes(x = factor(jlocation), y = price)) +
  geom_boxplot() + 
  xlab("JLocation") +
  ylab("Housing Price") +
  ggtitle("Imputed Indicator of Location", subtitle = "Reported location are less expensive") 
ggsave("img/Imputed Indicator of Location.jpg", width = 5)

levels(ne_comp_data$jfoundation) <- c("Imputed","Reported")
ggplot(ne_comp_data, aes(x = factor(jlocation), y = price)) +
  geom_boxplot() + 
  xlab("JFoundation") +
  ylab("Housing Price") +
  ggtitle("Imputed Indicator of Foundation", subtitle = "Reported foundation are less expensive") 
ggsave("img/Imputed Indicator of Foundation.jpg", width = 5)

