
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
# -----Ho dis normal, Ha dis no normal-----
shapiro.test(residuals(baby.fit11)) ## normalidad 


# p-value = 0.005126, se rechaza Ho, no vienen de una dis normal

lillie.test(residuals(baby.fit11)) ### normalidad

# p-value = 0.0109, se rechaza Ho, no vienen de una dis normal

# ----------- Encuentra varianza constante ------------------
# Ho varianza cte. Ha no hay varianza cte.

bptest(Talla_hoy~ Torax_nacer) ### Homocedasticidad Breusch-Pagan 
# p-value = 0.3808, se favorece Ho hay varianza conte

gqtest(lm(Talla_hoy~ Torax_nacer)) ### Homocedasticidad Goldfeld-Quandt

# p-value = 0.0112, se rechaza Ho, no hay varianza cte.

# como bp y gqtest no arrojan que ambos tienen varianza ctr. se dice que no la hay

# ------------ Independencia -------------------

# Ho datos independientes, Ha: datos dependientes de alguna forma
# en realidad autocorrelacion entre los datos mayor que 0
dwtest(Talla_hoy ~ residuals(baby.fit11)) # Durbin Watson independencia
# no se rechaza Ho es decir los errores son indpendientes.
### EN RESUMEN: No hay normalidad, no hay varianza constante, sí independencia
### en nuestra estrategia: detenemos el proceso y volvemos al modelo teórico 1. 

