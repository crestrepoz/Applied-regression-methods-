
data("mtcars")
#attach(m)


forward <- function(df, y, alfa = 0.1) {
  
  # escoger solo columnas numericas y separar la respuesta de las covariables:
  df.num <- df[ , unlist(lapply(df, is.numeric)) ]
  y.id <- which(colnames(df.num)==y)
  covariables <- df.num[,-y.id]
  
  # Informacion basica:
  n <- nrow(df)
  df.ssr <- 1
  df.sse <- n - 2
  f.critico<-qf(1-alfa,df.ssr,df.sse)
  print(paste("F critico nulo es:", f.critico ))
  
  
  # Paso 0: Modelo Nulo
  fit.actual<-lm(as.formula(paste(y,"~1")), data = df)
  SSE.actual <- sum(residuals(fit.actual)^2)
  
  # Necesarias para el loop
  siga <- TRUE
  paso <- 1
  cov <- names(covariables)
  cov.sup <- names(covariables)
  #--------------------------#
  while (siga == TRUE) {
    
    print(paste("Paso",paso))
    F.parcial <- list()
    for (i in cov){
      fit <- lm(as.formula(paste(y,"~", i)), data = df)
      SSE <- sum(residuals(fit)^2)
      CME <- SSE /(n-2)
      fparcial<-(SSE.actual-SSE)/CME
      F.parcial[i] = fparcial
    }
    
    print("Los F parciales son:")
    print(unlist(F.parcial))
    contraste <- ifelse(length(F.parcial[F.parcial>f.critico]) == 0,
                        0, unlist(F.parcial[F.parcial>f.critico]))
    
    
    if (contraste == 0 ){
      siga <- FALSE
      fit <- lm(as.formula(paste(y,"~", cov.selected)), data = df)
      print(paste("Seleccion final:",cov.selected ))
      return(fit)
    }  else {
      
      cov.selected <- names(which(F.parcial == max(contraste)))
      print(paste("Covariable",paso, ":",cov.selected))
      fit.actual <- lm(as.formula(paste(y,"~", cov.selected)), data = df)
      SSE.actual <- sum(residuals(fit.actual)^2)
      
      paso <- paso + 1
      
      # construir las proximas covariables
      viejas <-  trimws(unlist(strsplit(cov.selected, "+", fixed=TRUE)))
      cov.sup <- cov.sup[-which(cov.sup%in%viejas)]
      cov <- paste(cov.selected,"+",cov.sup)
      
      # Re calcular f critico
      df.ssr <- df.ssr +1
      df.sse <- df.sse -1
      f.critico<-qf(1-alfa,df.ssr,df.sse)
      print(paste("Nuevo F critico es:", f.critico))
      f.critico 
    }
    
  }
  
}

mt <- as.data.frame(mtcars[c(1,3,4,6)])
mt

t <- forward(mtcars, "mpg")
t

babies <- read.csv("C:/Users/yosef/OneDrive/Documents/Esp_Estadistica/Applied-regression-methods-/Correlation/Examples/babies.csv", header=T,sep=";")
babies<- babies[-2]

t <- forward(babies, "Talla_hoy")
t


stateed <- read.csv("C:/Users/yosef/OneDrive/Documents/Esp_Estadistica/Applied-regression-methods-/MLR/Dataframes/stateed.csv", header=T,sep=";")

t <- forward(stateed, "STATEAID")
t


ValidarSupuestos <- function (respuesta ,modelo,confianza){
  
  # Test de normalidad
  
  shapiro <- shapiro.test(modelo$residuals)
  shapiro <- shapiro$p.value
  
  lillie <- lillie.test(modelo$residuals)
  lillie <- lillie$p.value
  
  ifelse((shapiro > confianza) & (lillie > confianza), print("Existe normalidad de los errores"), print("No existe normalidad de los errores"))
  
  # Test de homocedasticidad
  
  
  baruch <- bptest(modelo)
  baruch <- baruch$p.value
  
  golfred <- gqtest(modelo)
  golfred <- golfred$p.value
  
  ifelse((baruch > confianza) & (golfred > confianza), print("Existe homocedasticidad"), print("No existe homocedasticidad"))
  
  # Test de independencia
  
  indepenencia <- dwtest(STATEAID ~ modelo$residuals)
  indepenencia <- indepenencia$p.value
  
  ifelse((indepenencia > confianza), print("Hay independencia"), print("No hay independencia"))
  
  # Construccion de la Tabla de respuestas
  
  tabla <- rbind(shapiro, lillie, baruch, golfred, indepenencia)
  rownames(tabla) <- c("shapiro", "lillie", "baruch", "golfred", "independencia")
  colnames(tabla) <- c("p.value")
  print(tabla)
  
  
}   
  


