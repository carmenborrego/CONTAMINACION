library(readxl)
library(stats)
library(tseries)
library(moments)
library(nortest)


universidad_monterrey_nuevo_león_air_quality <- read_excel("E:/INVESTIGACION/CAMBIO CLIMATICO Y EMISONES/universidad,-monterrey, nuevo león-air-quality.xlsx")
pm25=ts(universidad_monterrey_nuevo_león_air_quality$pm25, 
        start=c(2018, 04, 05), end=c(2024, 02, 05), frequency=365)
class(pm25)
plot(pm25)
#esta es la forma más fácil de remover na¿s 
#la funcion es de tseries 
pm25sinna=na.remove(pm25)
plot(pm25sinna)
boxplot(pm25sinna)
summary(pm25sinna)
kurtosis(pm25sinna)
hist(pm25sinna)
qqnorm(pm25sinna)
jarque.bera.test(pm25sinna)
#hipotesis nula de normalidad 
#Jarque Bera Test
#data:  pm25sinna
#X-squared = 194.53, df = 2, p-value < 2.2e-16
#rechazo que sea normal
#ahora hago shapiro wilk con nula de normalidad
#Shapiro-Wilk normality test
#data:  pm25sinna
#W = 0.98027, p-value = 2.47e-15
lillie.test(pm25sinna)
shapiro.test(pm25sinna)

adf.test(pm25sinna)
#la serie es estacionaria 

library(correlation)
library(mosaicData)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
universidad=na.omit(universidad_monterrey_nuevo_león_air_quality)
correlation(universidad, method="pearson")


df <- dplyr::select_if(universidad, is.numeric)
r <- cor(df)
redondeada=round(r,2)
ggcorrplot(r)
ggcorrplot(r, 
           hc.order = TRUE, 
           type = "upper",
           lab = TRUE, 
           title="Correlación Estación Universidad",
           show.legend = TRUE, ggtheme=ggplot2::theme_void()
           ,colors=c("black", "gray", "white"))
library(ggpubr)
ggscatter(df, x = "pm25", y = "pm10", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pm25", ylab = "pm10")
ggqqplot(df$pm10, ylab = "pm10")
ggqqplot(df$pm25)
ggqqplot(df$o3)
ggqqplot(df$no2)
ggqqplot(df$so2)
ggqqplot(df$co)

res <- cor.test(df$pm10, df$pm25, 
                method = "pearson")

todas=ggplot()+ 
  geom_line(data=df, aes(x=c(1:nrow(df)), y=pm25),
            size=.5, colour="royalblue4",) +
  geom_line(data=df, aes(x=c(1:nrow(df)), y=pm10),
            size=.5, colour="aquamarine",) +
  geom_line(data=df, aes(x=c(1:nrow(df)), y=o3),
            size=.5, colour="cornflowerblue",) +
  geom_line(data=df, aes(x=c(1:nrow(df)), y=no2),
            size=.5, colour="turquoise4",) +
  geom_line(data=df, aes(x=c(1:nrow(df)), y=co),
            size=.5, colour="gray",) +
  labs(x="Time", y="Level") + theme_classic()
legend=todas + xlab("Date")+ 
  ylab("Contaminant")+
  ggtitle("Contaminantes levels")
print(legend)

