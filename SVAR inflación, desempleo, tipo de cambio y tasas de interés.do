*Modelo SVAR 
** Variables: tipo de cambio promedio, tasa de cetes 28 días nominal, tasa de inflación y tasa de desempleo.
*** Tasas mensuales, 2008-2019
*** Tratamiento: tasas, tasas en logaritmos y en primeras diferencias
*tratamiento previo de la base


set more off
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
varsoc Dlogde Dlogpr Dlogin  Dlogtc, exog(postcrisis Trump obj) maxlag(15)

*4 vectores
matrix A = (1, 0, 0, 0 \ ., 1, 0, 0 \ ., ., 1, 0 \ ., ., ., 1)
matrix list A
matrix B = (., 0, 0, 0 \ 0, ., 0, 0 \ 0, 0, ., 0 \  0, 0, 0, .)
matrix list B

*Estimación del SVAR
svar  Dlogde Dlogpr Dlogtc Dlogin, aeq(A) beq(B) exog(postcrisis Trump obj)dfk lags(1/12)

*prueba estadística de Multiplicador de Lagrange para determinar si existen problemas de autocorrelación serial en el modelo estimado

* There is autocorrelation in the residuals when p-value at lag 1 is less than 5%
varlmar, mlag(12) 

varnorm, jbera
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

irf create lr, set(lrirf) step(100) replace
irf graph sirf, yline(0,lcolor(black)) xlabel(0(4)100) byopts(yrescale)

irf graph coirf, irf(IRF2c_SVAR) impulse(Dlogtc Dlogin Dlogde) response(Dlogpr)

*Pronósticos
*gráfica
irf graph coirf, irf(IRF2_SVAR) impulse(Dlogpr) response( Dlogde ) xlabel(#10) xline(1) yline(1.28)
*tabla
irf table coirf, irf(IRF2_SVAR) impulse(Dlogpr) response(Dlogde) step(9)






