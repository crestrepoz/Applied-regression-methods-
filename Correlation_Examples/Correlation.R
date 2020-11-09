##########################################################################
#####################################################
########### UNIVERSIDAD NACIONAL DE COLOMBIA
############PROFESOR: LUIS FERNANDO GRAJALES H. ####
###### MAY.2020
####### R version 3.6.3
#### EJEMPLOS DE COEFICIENTES DE CORRELACIÓN PEARSON Y SPEARMAN
###### DECISIÓN CON BASE EN PRUEBA DE MARDIA: ACÁ, NORMALIDAD BIVARIADA
install.packages("MVN")

library(MASS)
library(MVN)

################ EJEMPLO 1: 8 GRANOLAS.
maíz <- c(2.4, 3.4, 4.6, 3.7, 2.2, 3.3,  4.0, 2.1)
Cacahuates <- c(1.33, 2.12, 1.8, 1.65, 2.0, 1.76, 2.11,1.63)
maíz
par(mfrow=c(1,2))
plot(maíz, Cacahuates,main="lfgrajalesh. Apoyo a diapositivas CORRELACIÓN")
plot(Cacahuates, maíz, main="lfgrajalesh. Apoyo a diapositivas CORRELACIÓN")
#### IDEA VISUAL DE CORRELACIÓN LINEAL POSITIVA.
##### ¿Cuál coeficiente de correlación es apropiado?
######## Primero hacemos la prueba de Mardia donde Ho es
####### Ho: el vector aleatorio distribuye normal bivariado.
####### Ha: el vector aleatorio NO distribuye normal bivariado.
GRANOLAS = data.frame (cbind(maíz, Cacahuates))
GRANOLAS
mvn(GRANOLAS) ### Multivariate Normality Tests
###### valores.p: 0.95 (Skewness) y 0.32 (Kurtosis)
###################################################
###### SOLICITANDO A R el coeficiente de correlación de Pearson. 
########## También produce el contraste de hipótesis
############# Ha: rho es distinto de cero. 
cor.test(maíz, Cacahuates,method= "pearson") 
####### rho puede ser cero (valor.p=0.4)
################# r = 0.35

################ EJEMPLO 2:CIGARRILLOS.
alquitran=c(14,17,28,17,16,13,24,25,18,31)
nicotina=c(0.9,1.1,1.6,1.8,1.0,0.8,1.5,1.4,1.2,2)
par(mfrow=c(2,1))
plot(alquitran,nicotina,main="lfgrajalesh. Apoyo a dipositivas CORRELACIÓN")
plot(nicotina, alquitran, main="lfgrajalesh. Apoyo a dipositivas CORRELACIÓN")

#### IDEA VISUAL DE CORRELACIÓN LINEAL POSITIVA.
##### ¿Cuál coeficiente de correlación es apropiado?
######## Primero hacemos la prueba de Mardia donde Ho es
####### Ho: el vector aleatorio distribuye normal bivariado.
cigarrillo = data.frame (cbind(alquitran,nicotina))
cigarrillo

mvn(cigarrillo)
####            Test         Statistic             p value Result
### 1 Mardia Skewness  13.6086560601299 0.00865472750808305     NO
### 2 Mardia Kurtosis 0.448880815916851   0.653517635731147    YES

### Conclusión nuestra: El vector no distribuye normal bivariado.  
### Estimaremos el coeficiente de correlación no paramétrico de Spearman.

###### SOLICITANDO A R el coeficiente de correlación de Spearman. 
########## También produce el contraste de hipótesis
############# Ha: rho es distinto de cero. 
cor.test(alquitran, nicotina, data="cigarrillo", method="spearman") 
### RESULTADOS
### 1) p.value= 0.002 
#### 2) sample estimates:
###      rho 
#### 0.8389097 
###### INTERPRETAR!!!!



#############################################################
###################### EJEMPLO 3. TALLA DE BEBÉS 
################# CORRELACIÓN ENTRE Torax_nacer y Peso_nacer 
###########################################################
#### REGRESIÓN tallas.bebés CALIFICAR TALLER2. EDYE Y EXAM 3 INFERENCIA
babies<-read.csv("C:/Users/yosef/OneDrive/Escritorio/New folder/babies.csv", header=T,sep=";")
babies


attach(babies)
############## regresión lineal simple
plot(Talla_nacer,ï..Talla_hoy, main="Apoyo a Diapositivas de RLS.")
baby.fit<-lm(ï..Talla_hoy~ Talla_nacer)
baby.fit








