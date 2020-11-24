##########################################################################
#####################################################
######### UNIVERSIDAD NACIOANL DE COLOMBIA, SEDE BOGOTÁ.
######### DEPARTAMENTO DE ESTADÍSTICA.
############PROFESOR: LUIS FERNANDO GRAJALES H. ####
### Enviarme correcciones o inquietudes a 
############ lfgrajalesh@unal.edu.co
###### NOVIEMBRE del 2020. CURSO: MODELOS DE REGRESIÓN.
####### R version 4.0.3
#### EJEMPLOS DE regresión lineal MÚLTIPLE.
library(MASS)
library(lmtest) ### puede requerir package zoo. (Buscar en load package).
#############################################################
###################### EJEMPLOS RLS con TALLA DE BEBÉS 
########## Base de datos (parcial) de 7 bebés recién nacidos.
########## ### Una variable respuesta: Talla_hoy (en cm),
######### 3 Covariables
###### Talla_nacer, Peso_nacer y Torax_nacer.  
###### Los datos completos están en Walpole y Myers (2017).
###########################################################
#### REGRESIÓN tallas.bebés CALIFICAR TALLER2. EDYE Y EXAM 3 INFERENCIA
babies<-read.csv("C:/Users/yosef/OneDrive/Documents/Esp_Estadistica/Applied-regression-methods-/Correlation/Examples/babies.csv", header=T,sep=";")
babies
attach(babies)
summary(babies)
dim(babies)
summary(Talla_hoy)

####################
########################## EJEMPLOS DE f(parciales)
### 1) MODELO NULO 

baby.fit0<-lm(Talla_hoy~ 1)
anova(baby.fit0)
SCE.0=anova(baby.fit0)[1,2]
SCE.0
summary(baby.fit0)
mean(Talla_hoy)
########### CÁLCULO CONCEPTUAL DE SSRnulo
media=mean(Talla_hoy)
media
SCR.nulo=sum((Talla_hoy-media)^2)
SCR.nulo 
anova(baby.fit0)
######## "Partial f Test" en Sheather (2009), p.137. 
f.critico1<-qf(0.90,1,5)
f.critico1 
########### TRES REGRESIONES LINEALES SIMPLES
baby.fit11<-lm(Talla_hoy~ Talla_nacer)
baby.fit12<-lm(Talla_hoy~ Peso_nacer)
baby.fit13<-lm(Talla_hoy~ Torax_nacer)
anova(baby.fit11)
anova(baby.fit12)
anova(baby.fit13)

###### fparcial.SHEATHER=(SCE(beta0)-SCE(beta(j),beta0))/CME(beta0,X(j))
SCE.0<-anova(baby.fit0)[1,2]
SCE.11<-anova(baby.fit11)[2,2]
SCE.12<-anova(baby.fit12)[2,2]
SCE.13<-anova(baby.fit13)[2,2]
CME.11<-anova(baby.fit11)[2,3]
CME.12<-anova(baby.fit12)[2,3]
CME.13<-anova(baby.fit13)[2,3]
cbind(SCE.11,SCE.12,SCE.13,CME.11,CME.12,CME.13)
######## CÁLCULOS DE fparciales
fparcial.11=(SCE.0-SCE.11)/CME.11
fparcial.12=(SCE.0-SCE.12)/CME.12
fparcial.13=(SCE.0-SCE.13)/CME.13
cbind(f.critico1,fparcial.11, fparcial.12, fparcial.13)
########################################
##########################################
########### PASO 2 DE FORWARD CON fparcial. GUARDAR "SCE.12"
########## SE EVALÚAN DOS MODELOS CON 2 COVARIABLES, CADA UNO
#######
f.critico2<- qf(0.9,2,4) 
f.critico2    
######### MODELOS 21 Y 22
####### fparcial2: (SCE(beta0,beta1)-SCE(X(j),beta0,beta1))/CME(X(j),beta0,beta1)
####### con Torax_nacer
baby.fit21<-lm(Talla_hoy~ Peso_nacer + Torax_nacer)
anova(baby.fit21)
SCE.21<-anova(baby.fit21)[3,2]
CME.21<-anova(baby.fit21)[3,3]

##############
####### con Talla_nacer
baby.fit22<-lm(Talla_hoy~ Peso_nacer + Talla_nacer)
SCE.22<-anova(baby.fit22)[3,2]
CME.22<-anova(baby.fit22)[3,3]
anova(baby.fit22)
cbind(SCE.21,CME.21,SCE.22,CME.22)

# el unico modelo que cuyo r parcial es mayor al F critico es el modelo 2,2
# este modelo es mejor que el modelo que solo tiene talla al nacer

##############
######## fparciales 21 y 22
fparcial.21<-(SCE.12-SCE.21)/CME.21
fparcial.21
fparcial.22<-(SCE.12-SCE.22)/CME.22
fparcial.22
cbind(f.critico2, fparcial.21, fparcial.22) 

##### conclusión: SÓLO EL MODELO 22 ES MEJOR QUE EL ACTUAL, EL 12.
###### PASO 2: MODELO CON Covariabes "Peso_nacer" y "Talla_nacer"
baby.fit22<-lm(Talla_hoy~ Peso_nacer + Talla_nacer)

######### PASO 3 DE FORWARD ... (GUARDAR SCE.22)
##### sólo queda chequear si el modelo con las 3 covariables
####### es mejor que el modelo sólo con estas dos PREVIAMENTE ESCOGIDAS.
###### TAREA
fcrítico3<-qf(0.9, 3, 3)
fcrítico3
summary(babies)
#### ajustar modelo con las 3 covariables
baby.fit3<-lm(Talla_hoy~ Peso_nacer + Talla_nacer + Torax_nacer)
anova(baby.fit3)
#### obtener SCE.3 y CME.3
SCE.3<-anova(baby.fit3)[4,2]
CME.3<-anova(baby.fit3)[4,3]
#### CALCULAR fparcial.3
fparcial.3<-(SCE.22-SCE.3)/CME.3
cbind(fcrítico3,fparcial.3)
# como el fcritico es menor que el facial el modelo de 3 covariables no es mejor
# que el modelo con 2 variables que solametne tiene Peso_ncaer + Talla a nacer

######### el mejor subconjunto de predictoras USANDO FORWARD son...
baby.fit22<-lm(Talla_hoy~ Peso_nacer + Talla_nacer)
######### Estamos USANDO alfa.input=0.10 
######################
##################### PASO SIGUIENTE:
######## VALIDAR LOS SUPUESTOS EN EL "MEJOR" MODELO  DE FORWARD.
baby.fit22
summary(baby.fit22)
anova(baby.fit22)
residuals(baby.fit22)
##### A) Durbin-Watson Test (CONTRASTAR INDEPENDENCIA).
dwtest(Talla_hoy ~ residuals(baby.fit22)) ## p.value=0.87
################ CONTRASTES DE NORMALIDAD UNIVARIADA
######## B) normalidad de errores.
shapiro.test(residuals(baby.fit22)) ## p.value=0.59
## ??lilliefors
library(nortest)
lillie.test(residuals(baby.fit22)) ## p.value=0.28
############# c) CONTRASTAR VARIANZA CONSTANTE 
############
bptest(Talla_hoy~ Peso_nacer + Talla_nacer)
## gqtest(lm(Talla_hoy~ Peso_nacer + Talla_nacer))## p.value=0.07
### Goldfeld-Quandt no aplica por pocos registros. 
###############################################
############ REPORTAR LOS valores.p de los contrastes
########### concluir si se puede (o no se puede) interpreta la tabla ANOVA...

########## HACER EL EJERCICIO NUEVAMENTE CON alpha=0.01
########## si todo queda igual 
####### SE CONCLUYE HOMOCEDASTICIDAD Y SE CUMPLEN LOS SUPUESTOS.
########### ES VÁLIDA TABLA ANOVA 
############### 1) ¿AL MENOS UNO DE LOS PARÁMETROS ES DISTINTO DE CERO? 
####¿Cómo se escribe Ho? ¿qué significa? Estadística de prueba, cáculos,
##DECISIÓN (Con respecto a Ho), CONCLUSIÓN (Con respecto al problema).
##### 
summary(baby.fit22)
###### a) INTERPRETAR valores.p < 0.001 en el Test F (o t^2).
####### Tiene sentido interpretar b1 y b2 estimados
#### c) b0.estimado: ¿tiene interpretación?
## beta0.estimado no tiene interpretación acá,
###### por peligro de extrapolación.
 #### EJEMPLO CON  b) beta1.estimado=3.18: por cada unidad de aumento en Peso_nacer, la 
### Talla_hoy aumenta en 3.18 unidades, manteniendo constante a Talla_nacer.
### E[Y/(X1=x1+1, X2=k)]- E[Y/(X1=x1, X2=k)]= beta1
######### SIMILAR INTERPRETACIÓN para b2, coeficiente de Talla_nacer.

############################################################

################## MÉTODO BACKWARD alfa.output=0.01
attach(babies)
summary(babies)
dim(babies)
#### PASO 0: AJUSTAR MODELO FULL (CON LAS TRES COVARIABLES)
baby.fit3<-lm(Talla_hoy~ Peso_nacer + Talla_nacer + Torax_nacer)

summary(baby.fit3)
SCReg.full<-sum(anova(baby.fit3)[1:3,2])
SCReg.full
anova(baby.fit3)
CME.full<-anova(baby.fit3)[4,3]
CME.full
 
anova(baby.fit3)

##################PASO 1: ELIMINAR UNA COVARIABLE, SI SE PUEDE.

############ COLOCAR f.crítico.1;;; n-r = n-(p+1)= 7-4=3
f.critico.1.back<-qf(0.99,1,3)
f.critico.1.back  # 
### SE ELIMINA LA COVARIABLE CON MENOR fparcial y menor que 34.11
#### CALCULAR 3 valores de fparcial.1.back
##### a) AJUSTAMOS LOS 3 MODELOS CON DOS COVARIABLES
######## NOTACIÓN ACÁ
########### X1: Peso_nacer;;; X2: Torax_nacer;; X3: Talla_nacer
baby.fit12<-lm(Talla_hoy~ Peso_nacer + Torax_nacer)
anova(baby.fit12)
baby.fit13<-lm(Talla_hoy~ Peso_nacer + Talla_nacer)
anova(baby.fit13)
baby.fit23<-lm(Talla_hoy~ Torax_nacer + Talla_nacer)
anova(baby.fit23)
################### b) CALCULAMOS SCReg de los 3 modelos
SCReg.12<-sum(anova(baby.fit12)[1:2,2])
SCReg.12
SCReg.13<-sum(anova(baby.fit13)[1:2,2])
SCReg.13
SCReg.23<-sum(anova(baby.fit23)[1:2,2])
SCReg.23
################# c) CALCULAMOS tres fparciales
#### fparcial.sin.3<-(SCReg.full-SCReg.12)/(1)/(SCE.full/g.l.)
fparcial.sin.3<-(SCReg.full-SCReg.12)/CME.full
fparcial.sin.3
fparcial.sin.2<-(SCReg.full-SCReg.13)/CME.full
fparcial.sin.2
fparcial.sin.1<-(SCReg.full-SCReg.23)/CME.full
fparcial.sin.1
decidir.paso1.B<-cbind(f.critico.1.back, fparcial.sin.3,fparcial.sin.2,fparcial.sin.1 )
decidir.paso1.B
######## DECISIÓN: ELIMINO LA COVARIABLE 2, Torax_nacer.
########## CONTINUAR EL PROCESO BACKWARD Y DAR CONCLUSIÓN.
########### HASTA ACÁ EL script para enviar Nov. 19. 2020 POR CLASSROOM
################### LUIS F GRAJALES 























