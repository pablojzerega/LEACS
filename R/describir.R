#' Función describir
#'
#' Esta función nos permite realizar descripciones rápidas de variables numéricas, de manera univariada ,bivariada en función de una variable categórica de hasta 3 niveles.
#' @param var.num Variable numérica a describir.
#' @param nombre.num Nombre de la variable que aparecera en outputs - opcional.
#' @param unidad Unidad de la variable que aparece en outputs - opcional.
#' @param var.cat Variable categórica a utilizar si hace un analisis bivariado.
#' @param nombre.cat Nombre de la variable categórica que aparece en outputs - opcional.
#' @param label Labels de los niveles de la variable - opcional.
#' @param redondeo Cantidad de decimales a usar en los redondeos. Default es 2.
#' @param hist Graficar o no un histograma. Default en TRUE.
#' @param bins Cantidad de bins del histograma. Por default hace una cuenta aproximada de bins óptimos.
#' @param colores Colores a utilizar en gráficos, en formato lista.
#' @param box Graficar o no un Boxplot. Default en TRUE.
#' @param qq  Graficar o no un QQplot. Default en TRUE.
#' @param shapiro Realizar o no un test de Shapiro-Wilks. Default en TRUE
#' @param guardar Guardar o no los gráficos y data.frame con medidas de resumen en el Global Environment. Default en FALSE.
#' @keywords describir leacs
#' @export
#' @examples
#' describir()



describir <- function(data,
                        var.num,
                        nombre.num = "",
                        unidad = "",
                        var.cat = NULL,
                        nombre.cat = "",
                        label = NULL,
                        redondeo = 2,
                        hist = TRUE,
                        bins = NULL,
                        colores = c("#457b9d","#2a9d8f","#2a9d9f"),
                        box =TRUE,
                        qq = TRUE,
                        shapiro = TRUE,
                        guardar = FALSE){

# ARGUMENTOS --------------------------------------------------------------

  # Esta función devuelve un resumen de la variable númerica especificada, junto con la opción de
  #            graficar un histograma, boxplot y qqplot
  #
  # ARGUMENTOS
  #
  # data:         Dataframe que contiene la variable.
  #
  # metodo:       Analisis univariado (1) o bivariado (2)
  #
  # var.num:      Variable númerica a analizar.
  #
  # unidad:       Unidad de la variable
  #
  # nombre.num:   Nombre de la variable a mostrar en los resultados y los gráficos.
  #
  # var.cat:      Variable categórica que separa en grupos
  #
  # label:        Labels de la variable categórica
  #
  # nombre.cat:   Nombre de la variable categórica
  #
  # redondeo:     Decimales a redondear de los resultados, por defecto en 2
  #
  # colores:      Colores de los gráficos
  #
  # hist:         Generar o no el histograma
  #
  # bins:         Bins del histograma
  #
  # box:          Generar o no el boxplot
  #
  # qq:           Generar o no el QQ-plot
  #
  # shapiro:      Correr o no un test de Shapiro-Wilks
  #
  # guardar:      Guardar en el Environment el df y gráficos
  #
  # RETORNO
  #
  # Medidas de resumen de la variable númerica y graficos y test de Shapiro Wilk si son especificados.


# REQUISITOS --------------------------------------------------------------

require(e1071)
require(tidyverse)
require(patchwork)
require(describer)

options(dplyr.summarise.inform = FALSE)

# TIDYEVAL ----------------------------------------------------------------
#Se detiene la evaluación de la variable
variable.enq <- enquo(var.num)
vector.variable <- data %>% pull(!!variable.enq)


if (missing(var.cat) == FALSE){
variable.enq2 <- enquo(var.cat)
  if(is.null(expr(label)) == FALSE){

    data <- data %>% mutate(!!variable.enq2 := as.factor(!!variable.enq2))

    #data <- data %>% mutate(!!variable.enq2 := factor(!!variable.enq2,
                  #                                    levels =c(0,1),
                              #                        labels = c(label[1],label[2])))
  }
vector.variable2 <- data %>% pull(!!variable.enq2)
vector.variable3 <- data %>% select(!!variable.enq,!!variable.enq2)


niveles  <- levels(vector.variable2)


if (length(colores) != length(unique(niveles))) {
  stop("Es necesario que especifiques la misma cantidad de colores que niveles de la variable categórica")
}

}





# COMPROBACIONES ----------------------------------------------------------



if (!is.numeric(vector.variable)) {
  stop("Es necesario que la variable sea numerica,
          Por favor introduzca una variable que cumpla estos criterios")
}

# WARNINGS

if(nombre.num == "" | unidad == ""){
  warning(paste("Es recomendable elegir un nombre y una unidad a la variable para
                  tener una mejor lectura de los resultados.", "\n"))
}

if(guardar == FALSE){
  warning(paste("Los resultados y los graficos no se guardaran en el GlobalEnv, para guardarlos
                  cambie el argumento guardar a TRUE.","\n"))
}

if(is.null(bins)){
  warning(paste("Si los bins automaticos del histograma no son los correctos, podes cambiarlos con el argumento bins.","\n"))
}



# FUNCION UNIVARIADA ------------------------------------------------------

  # Medidas de resumen en df.resumen
      if (missing(var.cat) == TRUE){
        df.resumen <- data %>%
          summarize(
            n=n(),
            media=mean(!!variable.enq,na.rm = TRUE),
            mediana=median(!!variable.enq,na.rm = TRUE),
            ds=sd(!!variable.enq, na.rm = TRUE),
            RIQ=IQR(!!variable.enq, na.rm = TRUE),
            p25=quantile(!!variable.enq, c(0.25), na.rm = TRUE),
            p75=quantile(!!variable.enq, c(0.75), na.rm = TRUE),
            max=max(!!variable.enq,na.rm = TRUE),
            min=min(!!variable.enq,na.rm = TRUE),
            skew=skewness(!!variable.enq, na.rm = TRUE),
            kurt=kurtosis(!!variable.enq, na.rm = TRUE))

        df.resumen <-  round(df.resumen,redondeo)


        # GENERACIÓN DEL TEST DE SHAPIRO WILK

        if(shapiro == TRUE & length(vector.variable) <=5000){
          test <-  shapiro.test(vector.variable)

          # Agrego el p valor al df df.resumen
          p.valor <- test$p.value
          df.resumen$shapiro.p <- p.valor
        }

        # Guardar o no el dataframe en el GlobaEnv
        if (guardar == TRUE){
          assign(paste(nombre.num,"resumen",sep="."),
                 envir =.GlobalEnv,
                 df.resumen)
        }

  # Graficos univariados

   #Creación de graficos en objetos p1, p2 y p3

    ## Histograma
        if (is.null(bins)){
          bins = sqrt(length(vector.variable))
        }

        if (hist == TRUE){


          p1 <-       ggplot(data, aes(x=!!variable.enq)) +
            geom_histogram(
              fill=colores[1],
              colour="black",
              bins = bins) +
            labs(
              title = paste("Histograma de la variable",nombre.num,sep=": "),
              x= paste(nombre.num,unidad, sep =" "),
              y="Frecuencia") +
            theme(
              plot.title = element_text(color="black", size=14, face="bold.italic"),
              plot.subtitle =  element_text(color="black", size=10, face="italic"),
              axis.title.x = element_text(color="black", size=12, face="bold"),
              axis.title.y = element_text(color="black", size=12, face="bold"),
              legend.position = "none")
        }

      ## Boxplot
        if(box==TRUE){

          p2 <-   ggplot(data, aes(x = !!variable.enq)) +
            geom_boxplot(fill = colores[1],
                         color = "black") +
            labs(title = paste("Boxplot de la variable",nombre.num,sep=": "),x = nombre.num) +
            theme(
              plot.title = element_text(color="black", size=14, face="bold.italic"),
              plot.subtitle =  element_text(color="black", size=10, face="italic"),
              axis.title.y = element_text(color="black", size=12, face="bold"),
              legend.position = "none")+
            coord_flip()
        }

      ## QQ-plot
        if(qq==TRUE){

          p3 <-  ggplot(data, aes(sample = !!variable.enq)) +
            geom_qq(fill = colores[1]) +
            geom_qq_line()+
            labs(title =paste("Q-Q Plot de la variable",nombre.num,sep=": "),
                 y = "Muestra",
                 x = "Muestra teórica")+
            theme(
              plot.title = element_text(color="black", size=14, face="bold.italic"),
              plot.subtitle =  element_text(color="black", size=10, face="italic"),
              axis.title.x = element_text(color="black", size=12, face="bold"),
              axis.title.y = element_text(color="black", size=12, face="bold"),
              legend.position = "none")
        }

        #Guardar gráficos si argumento guardar == TRUE
        if(guardar == TRUE & hist == TRUE){
          assign(paste(nombre.num, "Histograma", sep = "."),
                 envir = .GlobalEnv,
                 p1)
        }

        if(guardar ==TRUE & box == TRUE){
          assign(paste(nombre.num, "Boxplot", sep = "."),
                 envir = .GlobalEnv,
                 p2)
        }

        if(guardar ==TRUE & qq == TRUE){
          assign(paste(nombre.num, "QQ-Plot", sep = "."),
                 envir = .GlobalEnv,
                 p3)
        }

        ## Combinación de graficos

        if (hist == TRUE & box ==TRUE & qq == TRUE){
          print(p1/(p2+p3))
          if (guardar == TRUE){
            assign(paste(nombre.num,"graficos", sep = "."),
                   envir = .GlobalEnv,
                   p1/(p2+p3))
          }
        }
        if (hist == TRUE & box == TRUE & qq == FALSE){
          print(p1/p2)

          if (guardar == TRUE){
            assign(paste(nombre.num,"graficos", sep = "."),
                   envir = .GlobalEnv,
                   p1/p2)
          }
        }
        if (hist == FALSE & box == TRUE & qq ==TRUE){
          print(p2/p3)
          if (guardar == TRUE){
            assign(paste(nombre.num,"graficos", sep = "."),
                   envir = .GlobalEnv,
                   p2/p3)
          }
        }
        if (hist == TRUE & box == FALSE &qq ==TRUE){
          print(p1/p3)
          if (guardar == TRUE){
            assign(paste(nombre.num,"graficos", sep = "."),
                   envir = .GlobalEnv,
                   p1/p3)
          }
        }

        #Graficos individuales

        if (hist == TRUE & box == FALSE & qq == FALSE){
          print(p1)

        }
        if (hist == FALSE & box == TRUE & qq == FALSE){
          print(p2)

        }
        if (hist == FALSE & box == FALSE & qq == TRUE){
          print(p3)

        }

        # DEVOLUCIÓN DE LA FUNCIÓN UNIVARIADA

        cat("                                                     ", "\n")
        cat("##################################################################################", "\n")
        cat("                                                     ", "\n")
        cat("--- Medidas de resumen de la variable ", nombre.num, "--------------------------------------", "\n", sep="")
        cat("                                                     ", "\n")

        cat("                Medidas", "\n")
        sprintf(fmt = "%22s%48s", "n",df.resumen$n) %>%  cat("\n")
        sprintf(fmt = "%30s%40s", paste("Media",unidad,sep = " "),df.resumen$media) %>%  cat("\n")
        sprintf(fmt = "%30s%40s", paste("Mediana",unidad,sep = " "),df.resumen$mediana) %>%  cat("\n")
        sprintf(fmt = "%30s%40s", paste("Desvio estandar",unidad,sep = " "),df.resumen$ds) %>%  cat("\n")
        sprintf(fmt = "%30s%40s",paste("p25",unidad,sep = " "),df.resumen$p25) %>%  cat("\n")
        sprintf(fmt = "%30s%40s",paste("p75",unidad,sep = " "),df.resumen$p75) %>%  cat("\n")
        sprintf(fmt = "%30s%40s",paste("RIQ",unidad,sep = " "),df.resumen$RIQ) %>%  cat("\n")
        sprintf(fmt = "%30s%40s",paste("Mínimo",unidad,sep = " "),df.resumen$min) %>%  cat("\n")
        sprintf(fmt = "%30s%40s", paste("Máximo",unidad,sep = " "),df.resumen$max) %>%  cat("\n")
        cat("                                                     ", "\n")

        cat("--- Medidas de forma de la variable ", nombre.num," ----------------------------------------", "\n", sep="")
        cat("                                                     ", "\n")

        sprintf(fmt = "%25s%45s","Skewness",df.resumen$skew) %>%  cat("\n")
        sprintf(fmt = "%25s%45s","Kurtosis",df.resumen$kurt) %>%  cat("\n")
        cat("                                                     ", "\n")


        cat("--- Analisis de las medidas de resumen de la variable ", nombre.num, "----------------------", "\n", sep="")
        cat("                                                     ", "\n")

        sprintf(fmt = "%30s%40s",paste("Media - Mediana ",unidad),
                round(df.resumen$media-df.resumen$mediana,2)) %>%  cat("\n")
        sprintf(fmt = "%30s%40s", paste("p75 - p50", unidad),
                df.resumen$p75-df.resumen$mediana) %>%  cat("\n")
        sprintf(fmt = "%30s%40s",paste("p50 - p25",unidad),
                df.resumen$mediana-df.resumen$p25) %>%  cat("\n")
        cat("                                                     ", "\n")

        if(shapiro == TRUE & length(vector.variable) <=5000){
          cat("--- Test de Shapiro-Wilk de la variable: ", nombre.num,"---", "\n", sep="")
          sprintf(fmt = "%25s%55s","p-valor",df.resumen$shapiro.p) %>%  cat("\n")
          cat("                                                     ", "\n")
          cat("##################################################################################", "\n")
          cat("                                                     ", "\n")

        }
        if(shapiro == TRUE & length(vector.variable) >=5000){
          cat("---Test de Shapiro-Wilk de la variable: ", nombre.num,"---", "\n", sep="")
          cat("          El número de observaciones es igual o mayor a 5000, no se puede realizar el test de Shapiro-Wilk", "\n")
          cat("                                                     ", "\n")
          cat("##################################################################################", "\n")
          cat("                                                     ", "\n")



        }}

# FUNCION BIVARIADA -------------------------------------------------------


  if (missing(var.cat) == FALSE){
    # Se genera el dataframe que contiene las medidas de resumen bivariado en df.resumen.multi

    df.resumen.multi <- data  %>% mutate(!!variable.enq2 := as.numeric(!!variable.enq2)) %>%
      group_by(!!variable.enq2) %>%
      summarize(
        n=n(),
        media=mean(!!variable.enq,na.rm = TRUE),
        mediana=median(!!variable.enq,na.rm = TRUE),
        ds=sd(!!variable.enq, na.rm = TRUE),
        RIQ=IQR(!!variable.enq, na.rm = TRUE),
        p25=quantile(!!variable.enq, c(0.25), na.rm = TRUE),
        p75=quantile(!!variable.enq, c(0.75), na.rm = TRUE),
        max=max(!!variable.enq,na.rm = TRUE),
        min=min(!!variable.enq,na.rm = TRUE),
        skew=skewness(!!variable.enq, na.rm = TRUE),
        kurt=kurtosis(!!variable.enq, na.rm = TRUE),
      )
    df.resumen.multi$shapiro <- rep(1,times = length(unique(vector.variable3[[2]])))
    df.resumen.multi <-  round(df.resumen.multi,redondeo)
    # GENERACIÓN DEL TEST DE SHAPIRO WILK

    if(shapiro == TRUE & length(vector.variable) <=5000){
      for(i in 1:length(unique(vector.variable3[[2]]))){
        df.resumen.multi[[i,13]] <- shapiro.test(vector.variable3[1][vector.variable3[2] == niveles[i]])$p.value
      }
      }
    df.resumen.multi <-df.resumen.multi
    # Guardar o no el dataframe en el GlobaEnv
    if (guardar == TRUE){
      assign(paste(nombre.num,"-",nombre.cat,"resumen_multivariado",sep="."),
             envir =.GlobalEnv,
             df.resumen.multi)
    }

    # Graficos bivariados

    #Creación de graficos en objetos p1, p2 y p3

    ## Histograma
    if (is.null(bins)){
      bins = sqrt(length(vector.variable))
    }
    if (hist == TRUE){


      p1.bi <-ggplot(data, aes(x=!!variable.enq)) +
        geom_histogram(aes(fill=!!variable.enq2),
          colour="black",
          bins = bins) +
        labs(
          title = paste("Histogramas de la variable",nombre.num,"en función de",nombre.cat,sep=" "),
          x= paste(nombre.num,unidad, sep =" "),
          y="Frecuencia") +
       scale_fill_manual(values = colores)+
        theme(
          plot.title = element_text(color="black", size=14, face="bold.italic"),
          plot.subtitle =  element_text(color="black", size=10, face="italic"),
          axis.title.x = element_text(color="black", size=12, face="bold"),
          axis.title.y = element_text(color="black", size=12, face="bold"),
          legend.position = "none")+ facet_grid(cols = vars(!!variable.enq2))
    }
    ## Boxplot

    if(box==TRUE){

      p2.bi <-   ggplot(data, aes(x = !!variable.enq)) +
        geom_boxplot(aes(fill = !!variable.enq2),
                     color = "black") +
        labs(title = paste("Boxplots de la variable",nombre.num,"en función de",nombre.cat,sep=" "),x = nombre.num) +
        scale_fill_manual(values = colores)+
        theme(
          plot.title = element_text(color="black", size=14, face="bold.italic"),
          plot.subtitle =  element_text(color="black", size=10, face="italic"),
          axis.title.y = element_text(color="black", size=12, face="bold"),
          legend.position = "none")+
        coord_flip()+ facet_grid(vars(!!variable.enq2))
    }
    ## QQ-plot

    if(qq==TRUE){

      p3.bi <-  ggplot(data, aes(sample = !!variable.enq)) +
        geom_qq(aes(fill = !!variable.enq2)) +
        geom_qq_line()+
        labs(title =paste("Q-Q Plots de la variable",nombre.num,"en función de",nombre.cat,sep=" "),
             y = "Muestras observadas",
             x = "Muestras teóricas")+
        scale_fill_manual(values = colores)+
        theme(
          plot.title = element_text(color="black", size=14, face="bold.italic"),
          plot.subtitle =  element_text(color="black", size=10, face="italic"),
          axis.title.x = element_text(color="black", size=12, face="bold"),
          axis.title.y = element_text(color="black", size=12, face="bold"),
          legend.position = "none")+ facet_grid(vars(!!variable.enq2))
    }

    #Guardar gráficos si argumento guardar == TRUE
    if(guardar == TRUE & hist == TRUE){
      assign(paste(nombre.num, "Histograma bivariado", sep = "."),
             envir = .GlobalEnv,
             p1.bi)
    }

    if(guardar ==TRUE & box == TRUE){
      assign(paste(nombre.num, "Boxplot bivariado", sep = "."),
             envir = .GlobalEnv,
             p2.bi)
    }

    if(guardar ==TRUE & qq == TRUE){
      assign(paste(nombre.num, "QQ-Plot bivariado", sep = "."),
             envir = .GlobalEnv,
             p3.bi)
    }
    if (length(unique(vector.variable3[[2]])) == 2) {

    ## Combinación de graficos

    if (hist == TRUE & box ==TRUE & qq == TRUE){
      print(p1.bi/(p2.bi+p3.bi))
      if (guardar == TRUE){
        assign(paste(nombre.num,"graficos", sep = "."),
               envir = .GlobalEnv,
               p1.bi/(p2.bi+p3.bi))
      }
    }
    if (hist == TRUE & box == TRUE & qq == FALSE){
      print(p1.bi/p2.bi)

      if (guardar == TRUE){
        assign(paste(nombre.num,"graficos", sep = "."),
               envir = .GlobalEnv,
               p1.bi/p2.bi)
      }
    }
    if (hist == FALSE & box == TRUE & qq ==TRUE){
      print(p2.bi/p3.bi)
      if (guardar == TRUE){
        assign(paste(nombre.num,"graficos", sep = "."),
               envir = .GlobalEnv,
               p2.bi/p3.bi)
      }
    }
    if (hist == TRUE & box == FALSE &qq ==TRUE){
      print(p1.bi/p3.bi)
      if (guardar == TRUE){
        assign(paste(nombre.num,"graficos", sep = "."),
               envir = .GlobalEnv,
               p1.bi/p3.bi)
      }
    }}

    # Graficos con 3 levels

    if (length(unique(vector.variable3[[2]])) == 3 & qq == TRUE) {
      print(p3.bi)
    }

    if (length(unique(vector.variable3[[2]])) == 3 & box == TRUE) {
      print(p2.bi)
    }

    if (length(unique(vector.variable3[[2]])) == 3 & hist == TRUE) {
      print(p1.bi)
    }

    #Graficos individuales

    if (hist == TRUE & box == FALSE & qq == FALSE){
      print(p1.bi)

    }
    if (hist == FALSE & box == TRUE & qq == FALSE){
      print(p2.bi)

    }
    if (hist == FALSE & box == FALSE & qq == TRUE){
      print(p3.bi)

    }

    # DEVOLUCIÓN DE LA FUNCIÓN BIVARIADA
    if (length(unique(vector.variable3[[2]])) == 2) {
    cat("                                                     ", "\n")
    cat("######################################################################################################", "\n")
    cat("                                                     ", "\n")
    cat("--- Medidas de resumen de la variable ", nombre.num," en función de ",nombre.cat," --------------------------------------", "\n", sep="")
    cat("                                                     ", "\n")

    sprintf(fmt = "%-30s%-40s%-40s", "Medida",label[1],label[2]) %>%  cat("\n")
    sprintf(fmt = "%-30s%-40s%-40s", "n",df.resumen.multi$n[1],df.resumen.multi$n[2]) %>%  cat("\n")
    sprintf(fmt = "%-30s%-40s%-40s", paste("Media",unidad,sep = " "),df.resumen.multi$media[1],df.resumen.multi$media[2]) %>%  cat("\n")
    sprintf(fmt = "%-30s%-40s%-40s", paste("Mediana",unidad,sep = " "),df.resumen.multi$mediana[1],df.resumen.multi$mediana[2]) %>%  cat("\n")
    sprintf(fmt = "%-30s%-40s%-40s", paste("Desvio estandar",unidad,sep = " "),df.resumen.multi$ds[1],df.resumen.multi$ds[2]) %>%  cat("\n")
    sprintf(fmt = "%-30s%-40s%-40s", paste("p25",unidad,sep = " "),df.resumen.multi$p25[1],df.resumen.multi$p25[2]) %>%  cat("\n")
    sprintf(fmt = "%-30s%-40s%-40s", paste("p75",unidad,sep = " "),df.resumen.multi$p75[1],df.resumen.multi$p75[2]) %>%  cat("\n")
    sprintf(fmt = "%-30s%-40s%-40s", paste("RIQ",unidad,sep = " "),df.resumen.multi$RIQ[1],df.resumen.multi$RIQ[2]) %>%  cat("\n")
    sprintf(fmt = "%-30s%-40s%-40s", paste("Mínimo",unidad,sep = " "),df.resumen.multi$min[1],df.resumen.multi$min[2]) %>%  cat("\n")
    sprintf(fmt = "%-30s%-40s%-40s", paste("Máximo",unidad,sep = " "),df.resumen.multi$max[1],df.resumen.multi$max[2]) %>%  cat("\n")
    cat("                                                     ", "\n")

    cat("--- Medidas de forma de la variable ", nombre.num," en función de ",nombre.cat," ----------------------------------------", "\n", sep="")
    cat("                                                     ", "\n")
    sprintf(fmt = "%-30s%-40s%-40s", "Skewness",df.resumen.multi$skew[1],df.resumen.multi$skew[2]) %>%  cat("\n")
    sprintf(fmt = "%-30s%-40s%-40s", "Kurtosis",df.resumen.multi$kurt[1],df.resumen.multi$kurt[2]) %>%  cat("\n")
    cat("                                                     ", "\n")


    cat("--- Analisis de las medidas de resumen de la variable ", nombre.num," en función de ",nombre.cat," ----------------------", "\n", sep="")
    cat("                                                     ", "\n")
    sprintf(fmt = "%-30s%-40s%-40s", "Media - Mediana",
            round(df.resumen.multi$media[1]-df.resumen.multi$mediana[1],2)
            ,round(df.resumen.multi$media[2]-df.resumen.multi$mediana[2],2)) %>%  cat("\n")
    sprintf(fmt = "%-30s%-40s%-40s", paste("p75 - p50", unidad),
            df.resumen.multi$p75[1]-df.resumen.multi$mediana[1],
            df.resumen.multi$p75[2]-df.resumen.multi$mediana[2]) %>%  cat("\n")
    sprintf(fmt = "%-30s%-40s%-40s", paste("p50 - p25",unidad),
            df.resumen.multi$mediana[1]-df.resumen.multi$p25[1],
            df.resumen.multi$mediana[2]-df.resumen.multi$p25[2]) %>%  cat("\n")
    cat("                                                     ", "\n")


    if(shapiro == TRUE &
       length(vector.variable3[1][vector.variable3[2] == niveles[1]]) <=5000 &
       length(vector.variable3[1][vector.variable3[2] == niveles[2]]) <=5000)
      {
      cat("--- Test de Shapiro-Wilk de la variable ", nombre.num," en función de",nombre.cat," ---------------------------------", "\n", sep=" ")
      sprintf(fmt = "%-30s%-40s%-40s","p-valor",df.resumen.multi$shapiro[1],df.resumen.multi$shapiro[2]) %>%  cat("\n")
      cat("#######################################################################################################", "\n")
      cat("                                                     ", "\n")
    }

    if(shapiro == TRUE &
       length(vector.variable3[1][vector.variable3[2] == niveles[1]]) <=5000 &
       length(vector.variable3[1][vector.variable3[2] == niveles[2]]) >=5000)
    {
      cat("--- Test de Shapiro-Wilk de la variable", nombre.num,"se realizo solamente para el nivel",label[1],"---", "\n", sep=" ")
      cat("El nivel",label[2],"tiene 5000 o más observaciones",df.resumen.multi$shapiro[1], "\n", sep = " ")

      cat("El p-valor es:",df.resumen.multi$shapiro[1], "\n")
      cat("######################################################################################################", "\n")
      cat("                                                     ", "\n")
    }

    if(shapiro == TRUE &
       length(vector.variable3[1][vector.variable3[2] == niveles[1]]) >=5000 &
       length(vector.variable3[1][vector.variable3[2] == niveles[2]]) <=5000)
    {
      cat("--- Test de Shapiro-Wilk de la variable", nombre.num,"se realizo solamente para el nivel",label[2],"---", "\n", sep=" ")
      cat("El nivel",label[1],"tiene 5000 o más observaciones",df.resumen.multi$shapiro[1], "\n", sep = " ")

      cat("El p-valor es:",df.resumen.multi$shapiro[2], "\n")
      cat("######################################################################################################", "\n")
      cat("                                                     ", "\n")
    }

    if(shapiro == FALSE |
       length(vector.variable3[1][vector.variable3[2] == niveles[1]]) >=5000 &
       length(vector.variable3[1][vector.variable3[2] == niveles[2]]) >=5000)
    {
      cat("--- El test de Shapiro-Wilk no se realizo porque no se activo el argumento o los 2 grupos tienen mas de 5000 observaciones","---", "\n", sep=" ")
      cat("######################################################################################################", "\n")
      cat("                                                     ", "\n")
    }

    }

    # DEVOLUCIÓN DE LA FUNCIÓN BIVARIADA
    if (length(unique(vector.variable3[[2]])) == 3) {
      cat("                                                     ", "\n")
      cat("######################################################################################################", "\n")
      cat("                                                     ", "\n")
      cat("--- Medidas de resumen de la variable ", nombre.num," en función de ",nombre.cat," --------------------------------------", "\n", sep="")
      cat("                                                     ", "\n")

      sprintf(fmt = "%-30s%-30s%-30s%-30s", "Medida",
              label[1],
              label[2],
              label[3]) %>%  cat("\n")
      sprintf(fmt = "%-30s%-30s%-30s%-30s", "n",
              df.resumen.multi$n[1],
              df.resumen.multi$n[2],
              df.resumen.multi$n[3]) %>%  cat("\n")
      sprintf(fmt = "%-30s%-30s%-30s%-30s", paste("Media",unidad,sep = " "),
              df.resumen.multi$media[1],
              df.resumen.multi$media[2],
              df.resumen.multi$media[3]) %>%  cat("\n")
      sprintf(fmt = "%-30s%-30s%-30s%-30s", paste("Mediana",unidad,sep = " "),
              df.resumen.multi$mediana[1],
              df.resumen.multi$mediana[2],
              df.resumen.multi$mediana[3]) %>%  cat("\n")
      sprintf(fmt = "%-30s%-30s%-30s%-30s", paste("Desvio estandar",unidad,sep = " "),
              df.resumen.multi$ds[1],
              df.resumen.multi$ds[2],
              df.resumen.multi$ds[3]) %>%  cat("\n")
      sprintf(fmt = "%-30s%-30s%-30s%-30s", paste("p25",unidad,sep = " "),
              df.resumen.multi$p25[1],
              df.resumen.multi$p25[2],
              df.resumen.multi$p25[3]) %>%  cat("\n")
      sprintf(fmt = "%-30s%-30s%-30s%-30s", paste("p75",unidad,sep = " "),
              df.resumen.multi$p75[1],
              df.resumen.multi$p75[2],
              df.resumen.multi$p75[3]) %>%  cat("\n")
      sprintf(fmt = "%-30s%-30s%-30s%-30s", paste("RIQ",unidad,sep = " "),
              df.resumen.multi$RIQ[1],
              df.resumen.multi$RIQ[2],
              df.resumen.multi$RIQ[3]) %>%  cat("\n")
      sprintf(fmt = "%-30s%-30s%-30s%-30s",paste("Mínimo",unidad,sep = " "),
              df.resumen.multi$min[1],
              df.resumen.multi$min[2],
              df.resumen.multi$min[3]) %>%  cat("\n")
      sprintf(fmt = "%-30s%-30s%-30s%-30s", paste("Máximo",unidad,sep = " "),
              df.resumen.multi$max[1],
              df.resumen.multi$max[2],
              df.resumen.multi$max[3]) %>%  cat("\n")
      cat("                                                     ", "\n")

      cat("--- Medidas de forma de la variable ", nombre.num," en función de ",nombre.cat," ----------------------------------------", "\n", sep="")
      cat("                                                     ", "\n")
      sprintf(fmt = "%-30s%-30s%-30s%-30s", "Skewness",
              df.resumen.multi$skew[1],
              df.resumen.multi$skew[2],
              df.resumen.multi$skew[3]) %>%  cat("\n")
      sprintf(fmt = "%-30s%-30s%-30s%-30s", "Kurtosis",
              df.resumen.multi$kurt[1],
              df.resumen.multi$kurt[2],
              df.resumen.multi$kurt[3]) %>%  cat("\n")
      cat("                                                     ", "\n")


      cat("--- Analisis de las medidas de resumen de la variable ", nombre.num," en función de ",nombre.cat," ----------------------", "\n", sep="")
      cat("                                                     ", "\n")

      sprintf(fmt = "%-30s%-30s%-30s%-30s", "Media - Mediana",
              round(df.resumen.multi$media[1]-df.resumen.multi$mediana[1],2)
              ,round(df.resumen.multi$media[2]-df.resumen.multi$mediana[2],2),
              round(df.resumen.multi$media[2]-df.resumen.multi$mediana[3],2)) %>%  cat("\n")
      sprintf(fmt = "%-30s%-30s%-30s%-30s", paste("p75 - p50", unidad),
              df.resumen.multi$p75[1]-df.resumen.multi$mediana[1],
              df.resumen.multi$p75[2]-df.resumen.multi$mediana[2],
              df.resumen.multi$p75[2]-df.resumen.multi$mediana[3]) %>%  cat("\n")
      sprintf(fmt = "%-30s%-30s%-30s%-30s", paste("p50 - p25",unidad),
              df.resumen.multi$mediana[1]-df.resumen.multi$p25[1],
              df.resumen.multi$mediana[2]-df.resumen.multi$p25[2],
              df.resumen.multi$mediana[2]-df.resumen.multi$p25[3]) %>%  cat("\n")
      cat("                                                     ", "\n")


      if(shapiro == TRUE &
         length(vector.variable3[1][vector.variable3[2] == niveles[1]]) <=5000 &
         length(vector.variable3[1][vector.variable3[2] == niveles[2]]) <=5000 &
         length(vector.variable3[1][vector.variable3[2] == niveles[3]]) <=5000)
      {
        cat("--- Test de Shapiro-Wilk de la variable ", nombre.num," en función de",nombre.cat," ---------------------------------", "\n", sep=" ")
        sprintf(fmt = "%-30s%-30s%-30s%-30s","p-valor",
                df.resumen.multi$shapiro[1],
                df.resumen.multi$shapiro[2],
                df.resumen.multi$shapiro[3]) %>%  cat("\n")
        cat("#######################################################################################################", "\n")
        cat("                                                     ", "\n")

      }
      if(shapiro == TRUE &
         length(vector.variable3[1][vector.variable3[2] == niveles[1]]) >=5000 |
         length(vector.variable3[1][vector.variable3[2] == niveles[2]]) >=5000 |
         length(vector.variable3[1][vector.variable3[2] == niveles[3]]) >=5000 ) {
        cat("--- El Test de Shapiro-Wilk no se pudo realizar porque algun nivel contenia mas de 5000 observaciones")
        cat("######################################################################################################", "\n")
        cat("                                                     ", "\n")


      }
        }
          }
            }





