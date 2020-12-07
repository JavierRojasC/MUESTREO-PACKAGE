# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
#MUESTREO ALEATORIO SIMPLE
#Para la media
al_simple_media_err= function(s2,e,N)
{no=(((1.96^2)*s2)/(e^2))
n=no/(1+(no/N))
return(n)}



al_simple_media_err_rel= function(s2,e,N,u)
{no=((1.96^2)*s2)/((e^2)*(u^2))
n=no/(1+(no/N))}

#Para total polacional
al_simple_total_err= function(s2,e,N)
{no=(((1.96^2)*s2)/(e^2))
n=(N^2*no)/(1+((N)*no))}

al_simple_total_err_rel= function(s2,e,N,u)
{no=((1.96^2)*s2)/((e^2)*(u^2))
n=no/(1+(no/N))}

#Para proporción polacional
al_simple_prop_err= function(p,q,e,N)
{no=((1.96^2)*p*q)/(e^2)
n=no/(1+(no/N))}

al_simple_prop_err_rel= function(p,q,e,N)
{no=((1.96^2)*q)/((e^2)*p)
n=no/(((N-1)/N)+(no/N))}

#Para total polacional con característica de interés
al_simple_totcarac_err= function(p,q,e,N)
{no=((1.96^2)*p*q)/(e^2)
n2=((N^3)*no)/((N-1)+((N^2)*no))
return(n2)}





al_simple_totcarac_err_rel= function(p,q,e,N)
{no=((1.96^2)*q)/((e^2)*p)
n=no/(((N-1)/N)+(no/N))}


