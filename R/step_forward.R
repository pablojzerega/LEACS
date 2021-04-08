#' Función step_forward
#'
#' Esta función nos permite extraer el p-valor y el beta de las variables a ingresar
#' en un modelo de regresión lineal multiple de forma step-forward.
#' @param data Base de datos que contiene las variables del modelo.
#' @param x Variable dependiente del modelo.
#' @param var_list Default NULL, Lista de variables independientes a ingresar al modelo. Si no se ingresa una lista, la función usa todas las variables de la base de datos.
#' @param alpha Default 0.2. Valor de alpha elegido como punto de corte para el ingreso de las variables.
#' @param guardar Por defecto FALSE. Si es TRUE se guarda un data frame con los datos.
#' @keywords step_forward leacs
#' @export
#' @examples
#' step_forward()
#'

step_forward <- function(data = data,
                              x = "NULL",
                              var_list = NULL,
                              alpha = 0.2,
                              guardar = FALSE){

  if(is.null(x) == TRUE)
    {
    stop( "El argumento x esta vacio, es necesario ingresar la variable dependiente")
  }

  if(is.character(var_list) == FALSE){
    var_list <- as.character(var_list)
  }

  if(is.null(var_list) == FALSE){
    temp_db<<- data %>% select(all_of({{var_list}}))
  } else {
    temp_db <<- data %>% select(-{{x}})
  }

  datos <-  data.frame(variable = "",
                       beta = 0,
                       p_valor = 0)

  datos2 <-  data.frame(variable = "",
                        beta = 0,
                        p_valor = 0)
  for (i in 1:ncol(temp_db)) {

    f_str <- paste(x, "~", names(temp_db[i]))
    k <- 1
    temp_mod <- lm(as.formula(f_str),data = data)

    temp_summary <- summary(temp_mod)

    if(length(temp_summary$coefficients) > 10) {
      for (j in 2:nrow(temp_summary$coefficients)) {
        print(k)
        p_valor <- temp_summary$coefficients[j,4]
        beta <- temp_summary$coefficients[j,1]
        nombre <- rownames(temp_summary$coefficients)[j]
        datos2[k,1] <- nombre
        datos2[k,2] <- beta
        datos2[k,3] <- p_valor
        k = k+1
      }} else {
        p_valor <- temp_summary$coefficients[8]
        beta <- temp_summary$coefficients[2,1]
        nombre <- names(temp_db)[i]
        datos[i,1] <- nombre
        datos[i,2] <- beta
        datos[i,3] <- p_valor
      }
  }
  datos <- rbind(datos,datos2)
  datos <- datos %>% mutate(significativo = ifelse(p_valor < alpha,"Significativo","No significativo")) %>%
    arrange(p_valor)

  if(guardar == TRUE)
  {
    datos_stepforward <<- datos
    print(datos_stepforward)
  } else {
    datos_stepforward <- datos
    print(datos_stepforward)
  }
}
