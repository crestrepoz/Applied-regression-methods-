
##########################################################################
#####################################################
######### UNIVERSIDAD NACIONAL DE COLOMBIA, SEDE BOGOTÁ.
######### DEPARTAMENTO DE ESTADÍSTICA.
############PROFESOR: LUIS FERNANDO GRAJALES H. ####
### Enviarme correcciones o inquietudes a 
############ lfgrajalesh@unal.edu.co
###### OCTUBRE del 2020. CURSO: MODELOS DE REGRESIÓN.
####### R version 4.0.3
#### EJEMPLOS DE regresión lineal MÚLTIPLE.
library(MASS)
library(lmtest)
library(nortest)
#############################################################
###################### EJEMPLOS RLS con TALLA DE BEBÉS 
########## Base de datos (parcial) de 7 bebés recién nacidos.
########## ### Una variable respuesta: Talla_hoy (en cm),
######### 4 Covariables, continuas: Edad_días,
###### Talla_nacer, Peso_nacer y Torax_nacer.  
###### Los datos completos están en Walpole y Myers (2017).
###########################################################
#### REGRESIÓN tallas.bebés 
babies<-read.csv("C:/Users/yosef/OneDrive/Documents/Esp_Estadistica/Applied-regression-methods-/Correlation/Examples/babies.csv", header=T,sep=";")
babies
attach(babies)
head(babies)
summary(babies)
dim(babies)
library(MASS)
library(lmtest)
library(nortest)
##### MODELO DE RLS CON TORAX
baby.fit11<-lm(Talla_hoy~Torax_nacer) # 1) AJUSTAR EL MODELO 
baby.fit11     ### 2) COEFICIENTES ESTIMADOS bo y b1

# GENERACION DE RESIDUALES
resid.torax<-residuals(baby.fit11)  ### Residuales (errores estimados)
resid.torax
########## COMIENZO A VALIDAR SUPUESTOS.
shapiro.test(residuals(baby.fit11)) ## normalidad 
lillie.test(residuals(baby.fit11)) ### normalidad
bptest(Talla_hoy~ Torax_nacer) ### Homocedasticidad Breusch-Pagan 
gqtest(lm(Talla_hoy~ Torax_nacer)) ### Homocedasticidad Goldfeld-Quandt
dwtest(Talla_hoy ~ residuals(baby.fit11)) # Durbin Watson independencia
### EN RESUMEN: No hay normalidad, no hay varianza constante, sí independencia
### en nuestra estrategia: detenemos el proceso y volvemos al modelo teórico 1. 

