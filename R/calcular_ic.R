#' Función calcular_ic
#'
#' Esta función nos permite calcular intervalos de confianza para diferencias de medias y proporciones.
#' @param x Variable numérica - diferencia de medias o categórica - diferencia de proporciones.
#' @param y Variable categórica de 2 niveles.
#' @param confianza Confianza de nuestro intervalo. - En decimales.
#' @param unidad Unidad de la variable numérica.- Opcional
#' @param estadistico Definir si usar "Z" o "T" en la formula del IC, dependiendo de la distribución asumida.
#' @keywords calcular_ic leacs
#' @export
#' @examples
#' calcular_ic()
#'
calcular_ic<- function(x,y,confianza = 0.95, unidad = NULL, estadistico = "Z"){

  #### ARGUMENTOS
  # tipo = diferencia de media o diferencia de proporción
  # x = variable 1 (en formato db$variable)
  # y = variable 2 (en formato db$variable)
  # confianza = nivel de confianza (en decimal)
  # estadistico = Definir si usar Z o T en la formula del IC, dependiendo de la distribución asumida.
  # unidad = unidad de la variable numérica (opcional)


# Chequeo de variables ----------------------------------------------------

  ## Chequeo que tipo de variables son x e y
    if (is.numeric(x) == TRUE & is.numeric(y) == TRUE ){
      stop("Es necesario que la variable y sea un factor para analizar una diferencia de medias entre grupos")
    }

    if (is.numeric(x) == TRUE & is.factor(y) == TRUE ){



# Calculo de media, error estandar e IC para variables numéricas ---------------------------------------


      ## Guardo los levels de la variable y en la variable niveles.
      niveles <- levels(y)

      # Diferencias de medias
      numerica.grupo1 <- x[y == niveles[1]]
      numerica.grupo2 <- x[y == niveles[2]]

      # Calculo de diferencia de medias
      media1 <- mean(numerica.grupo1)
      media2 <- mean(numerica.grupo2)
      dif.media <- abs(media1-media2)
      n1 <- length(numerica.grupo1)
      n2 <- length(numerica.grupo2)

      # Error estandar de la media
      eem <- round(sqrt((sd(numerica.grupo1)/n1)+
                          (sd(numerica.grupo2)/n2)),2)

      # Calculo del Intervalo de confianza:
      # Guardo en la variable Z el valor tabular correspondiente al nivel de confianza del argumento confianza.
      Z <- qnorm((1 - confianza)/2, lower.tail = FALSE)
      t <- qt(1-confianza, n1+n2, lower.tail = FALSE)

      # Calculo variabilidad para la distribución de T
      n1t <- n1-1
      n2t <- n2-1
      sd12 <- sd(numerica.grupo1)^2
      sd22 <-  sd(numerica.grupo2)^2
      sp2 <- ((n1t*sd12)+(n2t*sd22))/(n1t+n2t)

      eemt <- sqrt(sp2*(1/n1+1/n2))

      # Calculo de limites
      lim.superior <- dif.media + Z*eem
      lim.superior
      lim.inferior <- dif.media - Z*eem
      lim.inferior

      # Resultado final en consola
      if ( estadistico == "Z"){

      cat(" Se calculo el IC para una diferencia de medias:","\n")
      cat("La diferencia de medias es de: ", round(dif.media,2),unidad,"\n")
      cat("El error estandar de la media es de: ", round(eem,2),unidad,"\n")

      cat("El limite inferior del IC", confianza*100,"% es de: ", round(lim.inferior,2),unidad,"\n")
      cat("El limite inferior del IC", confianza*100,"% es de: ", round(lim.superior,2),unidad,"\n")
      cat("Intervalo de confianza", confianza*100,"% = ",round(dif.media,2)," ","[",round(lim.inferior,2)," - ",round(lim.superior,2),"]","\n")
      }

      if ( estadistico == "T"){
        # Calculo de limites
        lim.superior <- dif.media + t*eemt
        lim.superior
        lim.inferior <- dif.media - t*eemt
        lim.inferior

        dif.media <- dif.media
        lim.superior <- lim.superior
        lim.inferior <- lim.inferior
        # Resultado final en consola
        cat(" Se calculo el IC para una diferencia de medias:","\n")
        cat("La diferencia de medias es de: ", round(dif.media,2),"\n")

        cat("El limite inferior del IC", confianza,"% es de: ", round(lim.inferior,2),"\n")
        cat("El limite inferior del IC", confianza,"% es de: ", round(lim.superior,2),"\n")
        cat("Intervalo de confianza", confianza,"% = ",round(dif.media,2)," ","[",round(lim.inferior,2)," - ",round(lim.superior,2),"]","\n")
      }
    }

# Calculo de diferencias de proporciones e IC para proporciones -----------

 else if (is.factor(x) == TRUE & is.factor(y ) == TRUE ){

   ## Calculo de diferencias de porcentajes
    tabla2x2 <- table(x,y)
    tabla2x2.prop <- prop.table(table(x,y),2)

    diff.prop <- abs(tabla2x2.prop[2,1]-tabla2x2.prop[2,2])

   # Calculo de medida de variabilidad

    n1 <- sum(tabla2x2[1,1]+tabla2x2[2,1])
    n2 <- sum(tabla2x2[1,2]+tabla2x2[2,2])
    p1 <- tabla2x2.prop[2,1]
    p2 <- tabla2x2.prop[2,2]

    # sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))

    var.prop.grupo1 <- (p1*(1-p1))/n1
    var.prop.grupo2 <- (p2*(1-p2))/n2

    var.prop <- sqrt(var.prop.grupo1+var.prop.grupo2)

    # Calculo del Intervalo de confianza:
    # Guardo en la variable Z el valor tabular correspondiente al nivel de confianza del argumento confianza.
    Z <- qnorm((1 - confianza)/2, lower.tail = FALSE)

    # Calculo de limites
    lim.superior <- diff.prop + Z*var.prop
    lim.superior
    lim.inferior <- diff.prop - Z*var.prop
    lim.inferior

    diff.prop <- diff.prop*100
    lim.superior <- lim.superior*100
    lim.inferior <- lim.inferior*100

    # Resultado final en consola
    cat(" Se calculo el IC para una diferencia de proporciones:","\n")
    cat("La diferencia de proporciones es de: ", round(diff.prop,2),"%","\n")

    cat("El limite inferior del IC", confianza*100,"% es de: ", round(lim.inferior,2),"%","\n")
    cat("El limite inferior del IC", confianza*100,"% es de: ", round(lim.superior,2),"%","\n")
    cat("Intervalo de confianza", confianza*100,"% = ",round(diff.prop,2)," ","[",round(lim.inferior,2)," - ",round(lim.superior,2),"]","%","\n")



 }
  else {
    stop("La variable x tiene que ser numerica y la y un factor, o ambas variables factores")
  }

}



