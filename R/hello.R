#' @title Muestreo aleatorio simple con error absoluto
#' @description Función que obtiene el tamaño de muestra,basado en la media, utilizando muestreo aleatorio simple.
#' @param s2 Varianza muestral
#' @param e error absoluto
#' @param N error absoluto
#' @return Número de muestra óptimo.
#' @export al_simple_media_err
#' @examples Tamaño de la población
#' al_simple_media_err(s2=3,e=5,50);
al_simple_media_err= function(s2,e,N)
{no=(((1.96^2)*s2)/(e^2))
n=no/(1+(no/N))
return(n)}



#' @title Muestreo aleatorio simple con error relativo
#' @description Función que obtiene el tamaño de muestra, basado en la media, utilizando muestreo aleatorio simple.
#' @param s2 Varianza muestral
#' @param e Error relativo
#' @param N Tamaño de la población
#' @param u Media poblacional
#' @return Número de muestra óptimo
#' @export al_simple_media_err_rel
#' @examples
#' al_simple_media_err_rel(s2=3,e=0.05,N=50, u=10);
al_simple_media_err_rel= function(s2,e,N,u)
{no=((1.96^2)*s2)/((e^2)*(u^2))
n=no/(1+(no/N))
return(n)}


#' @title Muestreo aleatorio simple con error absoluto, basado en el total de la población.
#' @description Función que obtiene el tamaño de muestra, basado en el total de la población, utilizando muestreo aleatorio simple.
#' @param s2 Varianza muestral
#' @param e Error absoluto
#' @param N Tamaño de la población
#' @return Número de muestra óptimo
#' @export al_simple_total_err
#' @examples
#' al_simple_total_err(s2=3,e=5,N=50, u=10);
al_simple_total_err= function(s2,e,N)
{no=(((1.96^2)*s2)/(e^2))
n=(N^2*no)/(1+((N)*no))
return(n)}

#' @title Muestreo aleatorio simple con error relativo, basado en el total de la población.
#' @description Función que obtiene el tamaño de muestra, basado en el total de la población, utilizando muestreo aleatorio simple.
#' @param s2 Varianza muestral
#' @param e Error relativo
#' @param u Media poblacional
#' @param N Tamaño de la población
#' @return Número de muestra óptimo
#' @export al_simple_total_err_rel
#' @examples
#' al_simple_total_err_rel(s2=3,e=0.05,N=50, u=10);
al_simple_total_err_rel= function(s2,e,N,u)
{no=((1.96^2)*s2)/((e^2)*(u^2))
n=no/(1+(no/N))
return(n)}


#' @title Muestreo aleatorio simple con error absoluto, basado en proporción polacional.
#' @description Función que obtiene el tamaño de muestra, basado en proporción polacional, utilizando muestreo aleatorio simple.
#' @param p proporción con característica de interés
#' @param e Error absoluto
#' @param N Tamaño de la población
#' @return Número de muestra óptimo
#' @export al_simple_prop_err
#' @examples
#' al_simple_prop_err(p=0.3,e=5,N=50);
al_simple_prop_err= function(p,e,N)
{no=((1.96^2)*p*(1-p))/(e^2)
n=no/(1+(no/N))
return(n)}


#' @title Muestreo aleatorio simple con error relativo, basado en proporción polacional.
#' @description Función que obtiene el tamaño de muestra, basado en proporción polacional, utilizando muestreo aleatorio simple.
#' @param p proporción con característica de interés
#' @param e Error relativo
#' @param N Tamaño de la población
#' @return Número de muestra óptimo
#' @export al_simple_prop_err_rel
#' @examples
#' al_simple_prop_err_rel(p=0.3,e=0.05,N=50);
al_simple_prop_err_rel= function(p,e,N)
{no=((1.96^2)*(1-p))/((e^2)*p)
n=no/(((N-1)/N)+(no/N))
return(n)}


#' @title Muestreo aleatorio simple con error absoluto, basado en total polacional con característica de interés.
#' @description Función que obtiene el tamaño de muestra, basado en total polacional con característica de interés, utilizando muestreo aleatorio simple.
#' @param p proporción con característica de interés
#' @param e Error absoluto
#' @param N Tamaño de la población
#' @return Número de muestra óptimo
#' @export al_simple_totcarac_err
#' @examples
#' al_simple_totcarac_err(p=0.3,e=5,N=50);
al_simple_totcarac_err= function(p,e,N)
{no=((1.96^2)*p*(1-p))/(e^2)
n2=((N^3)*no)/((N-1)+((N^2)*no))
return(n2)}




#' @title Muestreo aleatorio simple con error relativo, basado en total polacional con característica de interés.
#' @description Función que obtiene el tamaño de muestra, basado en total polacional con característica de interés, utilizando muestreo aleatorio simple.
#' @param p proporción con característica de interés
#' @param e Error relativo
#' @param N Tamaño de la población
#' @return Número de muestra óptimo
#' @export al_simple_totcarac_err_rel
#' @examples
#' al_simple_totcarac_err_rel(p=0.3,e=5,N=50);
al_simple_totcarac_err_rel= function(p,e,N)
{no=((1.96^2)*(1-p))/((e^2)*p)
n=no/(((N-1)/N)+(no/N))}



