library(tidyverse)
library(outliers)
library(olsrr)
library(nlme)
library(MASS)
library(car)
library(readr)
Weather_History <- read_csv("C:/Users/crist/Downloads/Weather_History.csv")

Weather_History <- Weather_History[1:350, ]
Weather_History <- Weather_History %>% 
  rename(Wind_Speed_km_per_h = `Wind_Speed_km/h`)
infinite_values <- is.infinite(Weather_History$Wind_Speed_km_per_h)
Weather_History <- Weather_History[!infinite_values, ]
summary(Weather_History)
anyNA(Weather_History)
Weather_History <- na.omit(Weather_History)
colnames(Weather_History)


colnames(Weather_History)

anyNA(Weather_History$`Temp_C`)
anyNA(Weather_History$`Wind_Speed_km_per_h`)

Weather_History$Date
plot(Weather_History$`Temp_C`, Weather_History$`Wind_Speed_km_per_h`,
     xlab = "Temperature (C)",
     ylab = "Wind Speed (km/h)",
     col = "darkgreen",
     bty = "n")

hist(Weather_History$`Visibility_km`,
     breaks = 20,
     probability = TRUE,
     main = "Graficul densitatii valorilor vizibilitatii",
     cex.main = 0.8,
     xlab = "Vizibilitatea (km)",
     ylab = "Densitatea")
lines(Weather_History$`Visibility_km`,
      col = "magenta")

hist(Weather_History$`Wind_Speed_km_per_h`,
     breaks = 20,
     probability = TRUE,
     main = "Graficul densitatii valorilor vitezei vantului",
     cex.main = 0.8,
     xlab = "Vizibilitatea (km)",
     ylab = "Densitatea")
lines(Weather_History$`Wind_Speed_km_per_h`,
      col = "magenta")


shapiro.test(Weather_History$`Humidity`)

sample = Weather_History[sample(nrow(Weather_History), 6), ]
sample

shapiro.test(sample$`Visibility_km`)

x=seq(0,19)

y=dnorm(x, mean = 5.0, sd = 1.0)
y <- y[1:length(x)]
shapiro.test(y)

dixon.test(y,opposite = TRUE)
dixon.test(y)
boxplot(y)
#length(x)
#length(y)

plot(x,y)

hist(y,
     breaks = 20,
     probability = TRUE,
     main = "Graficul densitatii valorilor vizibilitatii",
     cex.main = 0.8,
     xlab = "Vizibilitatea (km)",
     ylab = "Densitatea")
lines(y,
      col = "magenta")

sd(Weather_History$`Visibility_km`)
(summary(Weather_History$`Visibility_km`)["Mean"]-summary(Weather_History$`Visibility_km`)["Min."])/sd(Weather_History$`Visibility_km`)
 
sd(Weather_History$`Wind_Speed_km_per_h`)
(summary(Weather_History$`Wind_Speed_km_per_h`)["Mean"]-summary(Weather_History$`Wind_Speed_km_per_h`)["Min."])/sd(Weather_History$`Wind_Speed_km_per_h`)

sd(Weather_History$`Wind_Direction_Degrees`)
(summary(Weather_History$`Wind_Direction_Degrees`)["Mean"]-summary(Weather_History$`Wind_Direction_Degrees`)["Min."])/sd(Weather_History$`Wind_Direction_Degrees`)

sd(Weather_History$`Pressure_millibars`)
(summary(Weather_History$`Pressure_millibars`)["Max."]-summary(Weather_History$`Pressure_millibars`)["Mean"])/sd(Weather_History$`Pressure_millibars`)




plot(Weather_History$`Temp_C`,Weather_History$Humidity)
plot(Weather_History$`Temp_C`,log(Weather_History$`Wind_Speed_km_per_h`))



fit <- lm(Temp_C ~ log(`Wind_Speed_km_per_h`) + Humidity, data = Weather_History)

summary(fit)

equation1=function(x){coef(fit)[2]*x+coef(fit)[1]}
equation2=function(x){coef(fit)[2]*x+coef(fit)[1]+coef(fit)[3]}

ggplot(Weather_History, aes(y = `Temp_C`, x = log(`Wind_Speed_km_per_h`), color = Humidity)) +
  geom_point() +
  stat_function(fun = equation1, geom = "line", color = scales::hue_pal()(2)[1]) +
  stat_function(fun = equation2, geom = "line", color = scales::hue_pal()(2)[2])



plot(fit)




boxplot(Weather_History$`Temp_C`)
boxplot(Weather_History$Humidity)
boxplot(Weather_History$`Visibility_km`)
boxplot(Weather_History$`Pressure_millibars`)


fit_simple <- lm(y ~ x, data = Weather_History)
cooks_dist <- cooks.distance(fit_simple)
barplot(cooks.distance(fit_simple),main="Distanta Cook")
abline(h=0.04, col='cadetblue2')



ggplot(Weather_History, aes(x = `Wind_Speed_km_per_h`, y = Visibility_km)) + 
  geom_point() +
  geom_smooth(method = "lm")

ggplot(Weather_History,aes(x = Temp_C, y = App_Temp_C)) + geom_point() +geom_smooth(method = "lm")
 

fit_multiple <- lm(Temp_C ~ Wind_Direction_Degrees + Humidity, data = Weather_History)
summary(fit_multiple)

fit_simple <- lm(Temp_C ~ `Wind_Speed_km_per_h`, data = Weather_History)
studres(fit_simple)
vector <- c(Weather_History$Wind_Direction_Degrees)
vector

leveragePlot(fit_multiple, term.name = "Wind_Direction_Degrees")


X <- cbind(Weather_History$Humidity,x)
x <- seq(1, 100)

#length(y)
#length(x)
y <- seq(1, 100)
mod.0 <- lm(y ~ x, data = Weather_History)

hatvalues(mod.0)




cor(Weather_History$Temp_C,Weather_History$Humidity)



################################################################################

p1x <- 6.3
p1y <- 16
p2x <- 15
p2y <- 10.3
p3x <- 2.2
p3y <- 2


x <- c(6.06,6.14,6.08,7.31,4.68,5.97,7.11,3.62,7.68,7.56)
y <- c(9.07,8.58,9.07,9.05,9.18,8.45,9.05,9.38,11.61,9.28)

mod.0 <- lm(y~x)
mod.1 <- lm(c(y,p1y)~c(x,p1x))
mod.2 <- lm(c(y,p2y)~c(x,p2x))
mod.3 <- lm(c(y,p3y)~c(x,p3x))



plot(x,y,xlim=c(0,20),ylim=c(0,20))
points(p1x,p1y,pch=15,cex=1.5)
abline(mod.0)
abline(mod.1,lty=2)
text(6,14,labels="Outlier, low leverage, low influence",cex=1)

plot(x,y,xlim=c(0,20),ylim=c(0,20))
points(p2x,p2y,pch=15,cex=1.5)
abline(mod.0)
abline(mod.2,lty=2)
text(15,8,labels="Not outlier, high leverage, low influence",cex=1)

plot(x,y,xlim=c(0,20),ylim=c(0,20))
points(p3x,p3y,pch=15,cex=1.5)
abline(mod.0)
abline(mod.3,lty=2)
text(4,1,labels="Outlier, high leverage, high influence",cex=1)

X <- cbind(rep(1,10),x)
hii <- diag(X%*%solve(t(X)%*%X)%*%t(X))
hii
plot(hii~x,ylab="Leverage",main="",pch=4)
hatvalues(mod.0)

ols_plot_dfbetas(fit_multiple)


influencePlot(fit_simple,  
              xlab="Hat-Values", ylab="Studentized Residuals", id=TRUE)





