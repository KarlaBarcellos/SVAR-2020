*Modelo SVAR 
** Variables: tipo de cambio promedio, tasa de cetes 91 días nominal, tasa de INPC por objeto del gasto o inflación y tasa de desempleo.
*** Tasas mensuales, 1980-2019
*** Tratamiento: tasas, tasas en logaritmos y en primeras diferencias
*tratamiento previo de la base

*** Correr desde aquí!
clear
*abrir base
 use"C:\Users\estef\Documents\Paper 2020\SVAR var nominales.dta", clear
set more off
*log y dif
gen logtc = log(tc)
 gen login = log(ti)
 gen logpr = log(infl)
 gen logde = log(desem)
 **convertir a primera diferencia
 gen Dlogtc = D.logtc
 gen Dlogin = D.login
 gen Dlogpr = D.logpr
 gen Dlogde = D.logde
*declarar serie de tiempo
 tsset t
*---- Análisis gráfico
   #delimit ;
twoway (tsline desem, lcolor(red) lwidth(vvthin) lpattern(solid) connect(direct)) || 
(tsline tc, lcolor(yellow) lwidth(vvthin) lpattern(solid) connect(direct)) || 
(tsline ti, lcolor(black) lwidth(vvthin) lpattern(solid) connect(direct)) || 
(tsline infl, lcolor(erose) lwidth(vvthin) lpattern(solid) connect(direct)), legend(on size(small));
#delimit cr  
summarize
*en diferencias
  #delimit ;
twoway (tsline D.logde, lcolor(red) lwidth(vvthin) lpattern(solid) connect(direct)) || 
(tsline D.logtc, lcolor(yellow) lwidth(vvthin) lpattern(solid) connect(direct)) || 
(tsline D.login, lcolor(black) lwidth(vvthin) lpattern(solid) connect(direct)) || 
(tsline D.logpr, lcolor(erose) lwidth(vvthin) lpattern(solid) connect(direct)), legend(on size(small));
#delimit cr  
summarize
**Detectar si hay problemas de raíces unitarias. 
** Una serie con raíz unitaria es no estacionaria por lo que no se puede hacer trabajos de modelación con ellas.
*** Se realiza la prueba dfuller aumentada y se trata de corregir hasta tener series estacionarias.
*Ahora son series estacionarias
*prueba sobre las bases en primera diferencia
dfuller D.logtc, trend regress lags(1)
dfuller D.login, trend regress lags(1)
dfuller D.logpr, trend regress lags(1)
dfuller D.logde, trend regress lags(1)
*-------SVAR------
*determinar el orden del SVAR
varsoc Dlogde Dlogpr Dlogin  Dlogtc, maxlag(15)
*En la práctica, uno de los criterios más empleados es el AIC, sin embargo, es factible poder trabajar con el estadístico que tenga el menor número de rezagos.
*Esto es así, debido a que mientras mayor número de rezagos se apliquen en la estimación sean empleados, entonces más grados de libertad se tendrá en el sistema 
*de ecuaciones simultáneas, lo cual afecta sensiblemente la consistencia en este tipo de modelos toda vez que se tienen pocos datos.
* De la tabla 3 se puede deducir que se podrían emplear hasta 4 rezagos bajo el criterio AIC que indica el valor más bajo. 
*Es factible también mencionar que de acuerdo al HQIC y el criterio SBIC se podría desarrollar un VAR de orden 2.
*4 vectores
matrix A = (1, 0, 0, 0 \ ., 1, 0, 0 \ ., ., 1, 0 \ ., ., ., 1)
matrix list A
matrix B = (., 0, 0, 0 \ 0, ., 0, 0 \ 0, 0, ., 0 \  0, 0, 0, .)
matrix list B
*Estimación del SVAR
svar  Dlogde Dlogpr Dlogtc Dlogin, aeq(A) beq(B) dfk lags(13)
*Matriz triangular con restricciones contemporáneas de corto plazo
*asume que existe causalidad en el sentido de Granger de periodos previos hacia periodos futuros 
matrix Aest=e(A)
matrix list Aest
*Matriz diagonal de corto plazo
matrix Best=e(B)
matrix list Best
*Se obtiene finalmente la matriz P_{sr}
matrix Psr=inv(Aest)*Best
matrix list Psr
*prueba estadística de Multiplicador de Lagrange para determinar si existen problemas de autocorrelación serial en el modelo estimado
*La importancia de la tabla anterior radica en que para todos los rezagos que se estimaron se acepta la hipótesis nula, la cual demuestra 
*que no existe autocorrelación entre los residuos del modelo de ecuaciones simultáneas que se está planteando originalmente 
*el primero sí! ahhhhh 
* There is autocorrelation in the residuals when p-value at lag 1 is less than 5%
varlmar, mlag(12) 
*-----Prueba para ruido blanco de los residuos al cuadrado del modelo VAR.
predict res_tc , residuals equation(Dlogtc)
gen res22tc=res_tc^2
wntestq res22tc, lags(1)

predict res_in, residuals equation(Dlogin)
gen res22in=res_in^2
wntestq res22in, lags(1)

predict res_pr, residuals equation(Dlogpr)
gen res22Dlogpr=res_pr^2
wntestq res22Dlogpr, lags(1)

predict res_de, residuals equation(Dlogde)
gen res22Dlogde=res_de^2
wntestq res22Dlogde, lags(1)
*-----Condiciones de estabilidad
varstable
*funciones de impulso respuesta ortogonalizadas del modelo SVAR 
irf create IRF_SVAR, set(IRF_SVAR) step(60)

irf graph coirf, irf(IRF_SVAR) impulse(Dlogpr) response(Dlogtc Dlogin Dlogde)

*Pronósticos
*gráfica
irf graph coirf, irf(IRF_SVAR) impulse(Dlogpr) response( Dlogde ) xlabel(#10) xline(1) yline(1.28)
*tabla
irf table coirf, irf(IRF_SVAR) impulse(Dlogpr) response(Dlogde) step(9)


twoway (lfitci logde logpr, stdf) || (scatter logde logpr)



