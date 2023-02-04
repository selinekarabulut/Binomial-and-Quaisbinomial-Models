
### Analysis of Factors Influencing Poverty in the United States ###

# load libraries
library(knitr)
library(dplyr) 
library(stringr)
library(zoo)
library(ggplot2)
library(urbnmapr)
library(devtools)
library(boot)
library(car)
library(MASS)
library(epiDisplay)
library(fixest)
library(lme4)
library(lattice)
library(effects)

# load data
data<-read.csv("poverty.csv")
summary(data)

# delete na flips
missing_fips = which(is.na(data$county_fips))
if(length(missing_fips)>0){
  data = data[-missing_fips,]
}

for (i in 1:dim(data)[1]){
  county_fips_iter = data$county_fips[i]
  county_fips_length = str_length(county_fips_iter)
  diff = 5-county_fips_length
  if (diff<5){
    data$county_fips[i] = paste0(do.call(paste0,as.list((rep(0,diff)))), county_fips_iter)
  }
}
names(data)[names(data) == "Long_"] = "Long"


### EDA
#county_flips are unique identifier for counties
names(data)[names(data) == "FIPS"] = "county_fips"
data$county_fips = as.character(data$county_fips) ##change it to character

us_cumulative_poverty_joint<- left_join(data, counties, by = "county_fips")

#Let's see whether we can plot the whole nation (county-level)
#plot county level cumulative poverty map, log 10 scale
us_cumulative_poverty_joint %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = (Poverty)), show.legend = T) +
  geom_polygon(
    data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
    fill = NA, color = 'black', size = 1
  ) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey90",trans = "log10",
                      ###, limits = c(0.00038, 1.158)
  )+
  coord_map() +
  labs(fill = expression("Poverty")) +
  ggtitle(paste0("Cumulative poverty"))


#plot the ratio, original scale 
us_cumulative_poverty_joint %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = (Poverty*(1/100))), show.legend = T) +
  geom_polygon(
    data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
    fill = NA, color = 'black', size = 1
  ) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey90",
                      ###, limits = c(0.00038, 1.158)
  )+
  coord_map() +
  labs(fill = expression("ratio %")) +
  ggtitle(paste0("Cumulative poverty/Population"))


# get county poor population
data$povertyp <- data$Poverty/100

# get county poor poproportion
data$cpoor <- (data$povertyp*data$TotalPop)

# get the population for all US states
us_state_population = data %>%
  group_by(State) %>%
  summarise(state_population = sum(TotalPop), .groups = 'drop')

# get poor population in all US states
us_state_cumulative_poor = data %>%
  group_by(State) %>%
  summarise(across(.fns=sum,.cols= 'cpoor'), .groups = 'drop')

us_state_cumulative_poor$ratio = us_state_cumulative_poor$cpoor/us_state_population$state_population

# make sure we have the state abbv in the data -IMPORTANT
us_state_cumulative_poor$state_abbv = us_state_cumulative_poor$State

us_state_cumulative_poor_joint <- left_join(us_state_cumulative_poor, states, by = "state_abbv")


### state level map 
#plot the ratio (state-level), original scale 
us_state_cumulative_poor_joint %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = (ratio*100)), show.legend = T) +
  geom_polygon(
    data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
    fill = NA, color = 'black', size = 1
  ) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey90",
                      ###, limits = c(0.00038, 1.158)
  )+
  coord_map() +
  labs(fill = expression("ratio %")) +
  ggtitle(paste0("# of people who are under poverty level in a state/state population"))

ggplot(us_state_cumulative_poor, 
       aes(x = State, 
           y=ratio, fill=state_abbv)) + 
  geom_bar(stat = "identity")+
  geom_col(width = 0.005, position = position_dodge(1.5)) +
  scale_x_discrete(guide = guide_axis(angle = 90))+
  ylab("Ratio") +
  ggtitle("States vs % under poverty level")

boxplot(us_state_cumulative_poor$ratio~us_state_cumulative_poor$State, id=list(n=Inf))

library(vcd)
assocstats(table(us_state_cumulative_poor$State, us_state_cumulative_poor$ratio))

test <- chisq.test (data.frame(us_state_cumulative_poor$cpoor,(us_state_population$state_population-us_state_cumulative_poor$cpoor)))
test

f1 <- lm(Poverty~State, data=data)
summary(f1)

# model output
library(stargazer)
stargazer(f1, dep.var.labels = c("Poverty rate at the state level"), type = "text", out= "models.txt")


###
data$men <- (data$Men/ data$TotalPop)*100
data$women <- (data$Women/data$TotalPop)*100

par(mfrow=c(4,3))
hist(data$Poverty)
hist(data$men)
hist(data$women)
hist(data$Hispanic)
hist(data$White)
hist(data$Black)
hist(data$Native)
hist(data$Asian)
hist(data$Pacific)

par(mfrow=c(3,3))
hist(data$Professional)
hist(data$Service)
hist(data$Office)
hist(data$Construction)
hist(data$Production)

par(mfrow=c(3,3))
hist(data$Drive)
hist(data$Carpool)
hist(data$Transit)
hist(data$Walk)
hist(data$OtherTransp)
hist(data$WorkAtHome)
hist(data$MeanCommute)

par(mfrow=c(3,3))
hist(data$PrivateWork)
hist(data$PublicWork)
hist(data$SelfEmployed)
hist(data$FamilyWork)
hist(data$Unemployment)

data$cpoor <- round(data$cpoor, digits=0)

plot(data$men, data$Poverty, main="Scatterplot Example", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
# Add fit lines
abline(lm(data$Poverty~ data$men), col="red") # regression line (y~x) 
lines(lowess(data$men, data$Poverty), col="blue") # lowess line (x,y)

pairs(~Poverty+men+women+Hispanic+ White+Black+Native+Asian+Pacific, data=data, 
      main="Simple Scatterplot Matrix")

pairs(~Poverty+Professional+Service+Office+Construction+Production, data=data, 
      main="Simple Scatterplot Matrix")

pairs(~Poverty+Drive+ Carpool+ Transit+ Walk+ OtherTransp+ WorkAtHome+MeanCommute, data=data, 
      main="Simple Scatterplot Matrix")

pairs(~Poverty+PrivateWork+ PublicWork+ SelfEmployed+FamilyWork+Unemployment, data=data, 
      main="Simple Scatterplot Matrix")

pairs(~Poverty+LessThanHighSchool+ HighSchoolDiploma+ SomeCollegeOrAssociateDegree+ BachelorDegreeOrHigher+AvgAge, data=data, 
      main="Simple Scatterplot Matrix")



### OLS
fit1 <- lm(Poverty~men+White+Black+Native+Asian+Pacific+Service+Office+Construction+Production+Carpool+ Transit+Walk+OtherTransp+WorkAtHome+MeanCommute+PublicWork+SelfEmployed+FamilyWork+Unemployment+HighSchoolDiploma+SomeCollegeOrAssociateDegree+BachelorDegreeOrHigher+AvgAge, weight=TotalPop, data=data)
summary(fit1)

# Check the constant variance assumption for errors
par(mfrow=c(1,2))
plot(fitted(fit1), residuals(fit1), xlab = "Fitted", ylab = "Residuals", main= "residuals vs fitted")
abline(h = 0)
summary(lm(abs(residuals(fit1)) ~ fitted(fit1)))

plot(fitted(fit1),abs(residuals(fit1)), main= "absolute residuals vs fitted")
abline(h=0)

# Check the normality assumption
#QQ plot and Shapiro-Wilk test
qqnorm(residuals(fit1), ylab="Residuals")
qqline(residuals(fit1))
title("QQ-plot of residuals")

qqnorm(rstandard(fit1), ylab="Residuals")
qqline(rstandard(fit1))
title("QQ-plot of standarized residuals")

shapiro.test(residuals(fit1))
#OLS is not a good fit


### Binomial regression
poverty <- cbind(data$cpoor,  data$TotalPop - data$cpoor)

data1 <- subset(data, select=-c(71,104,213,362,740,1208,1748,1830,1857,2080))

fit2 <- glm(poverty~men+White+Black+Native+Asian+Pacific+Service+ Office+Construction+Production+Carpool+ Transit+Walk+OtherTransp+WorkAtHome+MeanCommute+PublicWork+SelfEmployed+FamilyWork+Unemployment+HighSchoolDiploma+SomeCollegeOrAssociateDegree+BachelorDegreeOrHigher+AvgAge, family=binomial, data=data)
summary(fit2)

fit.step <- stepAIC(fit2, direction="backward")

step(fit2, direction="both", trace=0)

fit2a<-update(fit2,~.-Pacific)
fit2a<-update(fit2a,~.-Office)
summary(fit2a)

dp <- sum(residuals(fit2a,type="pearson")^2) /df.residual(fit2a)
dp

pchisq(deviance(fit2a),df.residual(fit2a),lower=FALSE)

confint(fit2a)

library(boot)
glm.diag.plots(fit2a)



### Quasibinomial regression
# Fit a betabinomial model because of the overdispersion problem
fit2_1 <-update(fit2a, family=quasibinomial)
summary(fit2_1)

exp(coef(fit2_1))

library(boot)
glm.diag.plots(fit2_1)

library(car)
outlierTest(fit2_1)
ali <- rstudent(fit2_1)
plot(ali)

summary(fit2_1)

fit2_2 <- glm(poverty~ men + White + 
                Black + Native + Asian + Service + Construction + Production + 
                Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + 
                PublicWork + SelfEmployed + FamilyWork + Unemployment + HighSchoolDiploma + 
                SomeCollegeOrAssociateDegree + BachelorDegreeOrHigher + AvgAge +I(Unemployment^2)+I(Walk^2)+I(Construction^2)+White*PublicWork+Asian*Service,
              family = quasibinomial, data = data)
summary(fit2_2)

library(ggplot2)
library(jtools)
library(ggstance)
plot_summs(fit2, fit2a,fit2_1, fit2_2)


# model output
library(stargazer)
stargazer(fit2, fit2a,fit2_1,fit2_2, type="text",
          header = FALSE, dep.var.labels = c("poverty"), 
          covariate.labels = c("men", "White","Black", "Native", "Asian","Pacific", "Service","Office","Construction","Production","Carpool","Transit","Walk", "OtherTransp", "WorkAtHome", "MeanCommute", "PublicWork","SelfEmployed","FamilyWork", "Unemployment", "HighSchoolDiploma", "SomeCollegeOrAssociateDegree", "BachelorDegreeOrHigher", "AvgAge"), 
          single.row = FALSE, out= "models.txt")


#binomial model comparison!
anova(fit2,fit2a,test="Chi")

#quasibonimal model comparison
anova(fit2_1, fit2_2, test="F")

library(boot)
glm.diag.plots(fit2_2)

anova(fit2, fit2a, test = "Chisq") # glm models

anova(fit2_1, fit2_2, test = "F") # quasibinomial


















