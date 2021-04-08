#' Función crear_tablas
#'
#' Esta función nos permite crear tablas para todas las variables asignadas como factores en una base de datos.
#' @param data Base de datos que contiene las variables categóricas.
#' @keywords crear_tablas leacs
#' @export
#' @examples
#' crear_tablas()
#'

crear_tablas <- function(data){
  variables <- data %>% select_if(is.factor)

  for(i in 1:ncol(variables)){
    cat("--- Tabla de variable:", names(variables[i]), "--------------------------------------", "\n", sep=" ")
    cantidad <- table(variables[i])
    proporcion <- round(prop.table(cantidad),2)
    tabla<- cbind(cantidad, proporcion)
    print(tabla)
  }

}
