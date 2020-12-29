#' Función crear_tablas2
#'
#' Esta función nos permite crear tablas 2x2 para todas las variables asignadas como factores en una base de datos en función de una variable categórica principal.
#' @param data Base de datos que contiene las variables categóricas.
#' @param categorica_principal Variable categórica en la que se dividen las otras variables.
#' @param margen Elección de como se representan los porcentajes en la tabla 2x2 de proporciones..
#' @keywords crear_tablas2 leacs
#' @export
#' @examples
#' crear_tablas2()
#'

crear_tablas2 <- function(data, categorica_principal,margen = 2){
  variables <- data %>% select_if(is.factor)
  variable.enq <- enquo(categorica_principal)
  variables <- variables %>% relocate(!!variable.enq, .after = last_col())

  for(i in 1:(ncol(variables)-1)){
    cat("--- Tabla de variable:", names(variables[i]),"en función de",names(variables[ncol(variables)]), "--------------------------------------", "\n", sep=" ")

    Cantidad <- table(variables[[i]],variables[[ncol(variables)]])
    Proporcion <- round(prop.table(Cantidad,margen),2)

    cat("Valores absolutos", "\n")
    print(Cantidad)

    cat("                                                     ", "\n")
    cat("Proporciones", "\n")
    print(Proporcion)
    cat("----------------------------------------------------------------", "\n")
    cat("                                                     ", "\n")

  }
}
