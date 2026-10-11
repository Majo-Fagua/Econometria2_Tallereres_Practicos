/* - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - 
*																			   *
*						     Taller 3 Econometría 2						       *
*								Parte práctica								   *
*																			   *
* - * - * - * - * - * - * - * - *  - * - * - * - * - * - * - * - * - * - * - * */

cls
clear all
set more off

cd "C:\Users\majo_\OneDrive\Escritorio\Econometría 2\Taller 3"
global dir "C:\Users\majo_\OneDrive\Escritorio\Econometría 2\Taller 3"
		  
**# Instalación de programas 
local src "https://raw.githubusercontent.com/rdpackages"
net install rdrobust,  from("`src'/rdrobust/master/stata")  replace
net install rddensity, from("`src'/rddensity/master/stata") replace
cap ssc install estout, replace

* Si Stata no carga las funciones Mata de rdrobust, se compilan en este equipo
cap mata: mata mlib index
cap mata: mata describe rdrobust_bw()
if _rc {
    copy "`src'/rdrobust/master/stata/rdrobust_functions.do" rdrobust_functions.do, replace
    do rdrobust_functions.do
}

capture program drop rdres
program define rdres
    args lab
    di as text %-36s "`lab'" as result %9.4f e(tau_cl) %9.4f e(tau_bc) ///
       %9.4f e(se_tau_rb) %8.3f e(pv_rb) %8.2f e(h_l) %8.0f e(N_h_l) %8.0f e(N_h_r)
end

cap mkdir "Resultados"

**# Importación y labels
import delimited "licencias_camalie.csv", clear

label variable dias_corte "Días respecto al corte (0 = 1 de enero de 2005)"
label variable asignacion "Asignación al nuevo régimen (Z)"
label variable examen_nuevo "Examen nuevo presentado (D)"
label variable edad "Edad (años)"
label variable urbano "Conductor urbano"
label variable experiencia_anios "Experiencia (años)"
label variable puntos_riesgo "Puntos de demérito por riesgo (Tipo A)"
label variable puntos_habilidad "Puntos de demérito por habilidad (Tipo B)"
label variable puntos_demerito "Puntos de demérito totales"

label define l_asig 0 "Antes del corte" 1 "Después del corte"
label values asignacion l_asig
label define l_exam 0 "Examen viejo" 1 "Examen nuevo"
label values examen_nuevo l_exam

describe
summarize dias_corte asignacion examen_nuevo edad urbano experiencia_anios ///
          puntos_riesgo puntos_habilidad puntos_demerito


**# (a) Gráfico de discontinuidad
set scheme white_tableau

	rdplot puntos_demerito dias_corte, c(0) ///
	graph_options(title("Puntos de demérito al rededor del corte") ///
	xtitle("Días respecto al corte (0 = 1 de enero de 2005)") ///
	ytitle("Puntos de demérito promedio") legend(off) ///
	name(rdplot_a, replace))
	
	graph export "Resultados/figura_a.png", replace width(2000)
	
**# (b) Efecto de la asignación 

	rdrobust puntos_demerito dias_corte, c(0) p(1) kernel(triangular) bwselect(mserd)
	rdres "(b) Forma Reducida: p(1) y h óptimo"
	
	global bw_opt = e(h_l)
	display "Ancho de banda óptimo = "`$bw_opt'"
	est store rd_b
	
	local tau_b = e(tau_cl) // Será usado en el literal h
	
	local bw_txt : display %4.0f $bw_opt
	local bw_txt = trim("`bw_txt'")
   	rdplot puntos_demerito dias_corte if abs(dias_corte) <= $bw_opt, c(0) p(1) ///
    h($bw_opt) kernel(triangular) ///
    graph_options(title("Local lineal dentro de [-`bw_txt', `bw_txt'] días") ///
    xtitle("Días respecto al corte") ytitle("Puntos de demérito") legend(off) ///
    name(efectolocal_b, replace))
	graph export "Resultados/figura_b.png", replace width(2000)
	
	*Exportar tabla (los se y p-value son robustos)
	esttab rd_b using "Resultados/tabla_b.rtf", replace ///
	cells(none) stats (tau_cl tau_bc se_tau_rb pv_rb h_l h_r N_h_l N_h_r, ///
	label ("Coeficiente convencional" "Coeficiente corregido por sesgo" ///
	"Error estándar" "p-valor" "Ancho de banda izquierdo" "Ancho de banda derecho" ///
	"N efectivo izquierda" "N efectivo derecha") fmt(3 3 3 4 3 3 0 0)) ///
	mtitles("Forma reducida") nonumbers noobs ///
	title("Efecto de la asignación sobre puntos de demérito")
	
**# (c) Verificación de robustez

	local h_ang = 0.5*$bw_opt
	local h_amp = 1.5*$bw_opt

	rdrobust puntos_demerito dias_corte, c(0) p(1) kernel(triangular) h(`h_ang')
	est store c_ang
	rdres "(c) Ancho de banda angosto"
	rdrobust puntos_demerito dias_corte, c(0) p(1) kernel(triangular) h(`h_amp')
	est store c_amp
	rdres "(c) Ancho de banda ampliado"
	
	rdrobust puntos_demerito dias_corte, c(0) p(2) kernel(triangular) bwselect(mserd)
	est store c_p2
	rdres "(c) Polinomio de orden 2, con bw óptimo"
	
	*Exportar tabla (los se y p-value son robustos)
	esttab c_ang c_amp c_p2 using "Resultados/tabla_c.rtf", replace ///
	cells(none) stats (tau_cl tau_bc se_tau_rb pv_rb h_l h_r N_h_l N_h_r, ///
	label ("Coeficiente convencional" "Coeficiente corregido por sesgo" ///
	"Error estándar" "p-valor" "Ancho de banda izquierdo" "Ancho de banda derecho" ///
	"N efectivo izquierda" "N efectivo derecha") fmt(3 3 3 4 3 3 0 0)) ///
	mtitles("h reducido" "h ampliado" "Polinomio de orden 2") nonumbers noobs ///
	title("Robustez de la forma reducida")
	
	
**# (d) Efecto sobre infracciones Tipo A e infracciones Tipo B
	rdrobust puntos_riesgo dias_corte, c(0) p(1) kernel(triangular)
	est store d_riesgo
	rdres "(d) Puntos riesgo (Tipo A)"

	rdrobust puntos_habilidad dias_corte, c(0) p(1) kernel(triangular)
	est store d_habilidad
	rdres "(d) Puntos habilidad (Tipo B)"

	rdplot puntos_riesgo dias_corte, c(0) ///
    graph_options(title("Infracciones Tipo A: riesgo") ///
    xtitle("Días respecto al corte") ytitle("Puntos de riesgo") ///
    legend(off) name(rdplot_dtipoA, replace))
	graph export "Resultados/figura_dtipoA.png", replace width(2000)

	rdplot puntos_habilidad dias_corte, c(0) ///
    graph_options(title("Infracciones Tipo B: habilidad") ///
    xtitle("Días respecto al corte") ytitle("Puntos de habilidad") ///
    legend(off) name(rdplot_dtipoB, replace))
	graph export "Resultados/figura_dtipoB.png", replace width(2000)
	
	*Exportar tabla (los se y p-value son robustos)
	esttab d_riesgo d_habilidad using "Resultados/tabla_d.rtf", replace ///
	cells(none) stats (tau_cl tau_bc se_tau_rb pv_rb h_l h_r N_h_l N_h_r, ///
	label ("Coeficiente convencional" "Coeficiente corregido por sesgo" ///
	"Error estándar" "p-valor" "Ancho de banda izquierdo" "Ancho de banda derecho" ///
	"N efectivo izquierda" "N efectivo derecha") fmt(3 3 3 4 3 3 0 0)) ///
	mtitles("Tipo A: riesgo" "Tipo B: habilidad") nonumbers noobs ///
	title("Efectos por tipo de infracción")

**# (e) Prueba de balance de covariables

	foreach v in edad urbano experiencia_anios {
		
		rdrobust `v' dias_corte, c(0) p(1) kernel(triangular)
		est store balance_`v'
		rdres "(e) Balance: `v'"
		display "`v'" _col(22) "p (robusto) = " %5.3f e(pv_rb)
		local lab : variable label `v'

		rdplot `v' dias_corte, c(0) ///
			graph_options(title("Balance: `lab'") xtitle("Días respecto al corte") ///
			ytitle("`lab'") legend(off) name(balance_`v', replace))
		graph export "Resultados/figura_e_balance_`v'.png", replace width(2000)
	}
	
	*Exportar tabla (los se y p-value son robustos)
	esttab balance_edad balance_urbano balance_experiencia_anios ///
	using "Resultados/tabla_e_balances.rtf", replace ///
	cells(none) stats (tau_cl tau_bc se_tau_rb pv_rb h_l h_r N_h_l N_h_r, ///
	label ("Coeficiente convencional" "Coeficiente corregido por sesgo" ///
	"Error estándar" "p-valor" "Ancho de banda izquierdo" "Ancho de banda derecho" ///
	"N efectivo izquierda" "N efectivo derecha") fmt(3 3 3 4 3 3 0 0)) ///
	mtitles("Edad" "Conductor urbano" "Experiencia") ///
	nonumbers noobs ///
	title("Prueba de balance de covariables")

	* Prueba de densidad (manipulación de X)
	rddensity dias_corte, c(0)
	scalar dens_t  = e(T_q)
	scalar dens_p  = e(pv_q)
	scalar dens_hl = e(h_l)
	scalar dens_hr = e(h_r)
	scalar dens_nl = e(N_h_l)
	scalar dens_nr = e(N_h_r)

	matrix densidad = (dens_t, dens_p, dens_hl, dens_hr, dens_nl, dens_nr)

	putexcel set "Resultados/tabla_e_densidad.xlsx", replace
	putexcel B2=("Estadistico t robusto") C2=("p-valor robusto") D2=("h izquierdo") ///
	E2=("h derecho") F2=("N efectivo izquierda") G2=("N efectivo derecha")
	putexcel B3=matrix(densidad)

**# (f) Efecto por subgrupos

	gen joven  = (edad < 30) if !missing(edad)
	gen novato = (experiencia_anios <= 6) if !missing(experiencia_anios)
	
	rdrobust puntos_demerito dias_corte if joven == 1, c(0) p(1) kernel(triangular)
	est store f_joven
	rdres "(f) Jóvenes (edad<30)"

	rdrobust puntos_demerito dias_corte if urbano == 1, c(0) p(1) kernel(triangular)
	est store f_urbano
	rdres "(f) Urbanos"

	rdrobust puntos_demerito dias_corte if novato == 1, c(0) p(1) kernel(triangular)
	est store f_novato
	rdres "(f) Novatos (exp<=6)"

	* De la muestra completa
	rdrobust puntos_demerito dias_corte, c(0) p(1) kernel(triangular)
	est store f_comp
	rdres "(f) Muestra completa"
	
	*Exportar tabla (los se y p-value son robustos)
	esttab f_joven f_urbano f_novato f_comp using "Resultados/tabla_f.rtf", replace ///
	cells(none) stats (tau_cl tau_bc se_tau_rb pv_rb h_l h_r N_h_l N_h_r, ///
	label ("Coeficiente convencional" "Coeficiente corregido por sesgo" ///
	"Error estándar" "p-valor" "Ancho de banda izquierdo" "Ancho de banda derecho" ///
	"N efectivo izquierda" "N efectivo derecha") fmt(3 3 3 4 3 3 0 0)) ///
	mtitles("Conductores jóvenes" "Conductores urbanos" "Conductores novatos" "Muestra completa") ///
	nonumbers noobs ///
	title("Efectos de la asignación por subgrupos")
	
**# (g) Estimación de la Primera Estapa
	
	tab asignacion examen_nuevo, row
	estpost tab asignacion examen_nuevo
	est store g_grupos
	esttab g_grupos using "Resultados/tabla_g_prop.rtf", replace ///
	cells("b(fmt(0)) rowpct(fmt(1))") unstack noobs nonumber nomtitle ///
	title("Asignación y examen efectivamente presentado")

	rdrobust examen_nuevo dias_corte, c(0) p(1) kernel(triangular)
	est store g_fs
	rdres "(g) Primera etapa usando examen nuevo"

	rdplot examen_nuevo dias_corte, c(0) ///
    graph_options(title("Primera etapa: examen nuevo") ///
    xtitle("Días respecto al corte") ytitle("P(Examen nuevo = 1)") ///
    legend(off) name(rdplotfs_g, replace))
	graph export "Resultados/figura_g_fs.png", replace width(2000)
	
	*Exportar tabla (los se y p-value son robustos)
	esttab g_fs using "Resultados/tabla_g_fs.rtf", replace ///
	cells(none) stats (tau_cl tau_bc se_tau_rb pv_rb h_l h_r N_h_l N_h_r, ///
	label ("Coeficiente convencional" "Coeficiente corregido por sesgo" ///
	"Error estándar" "p-valor" "Ancho de banda izquierdo" "Ancho de banda derecho" ///
	"N efectivo izquierda" "N efectivo derecha") fmt(3 3 3 4 3 3 0 0)) ///
	mtitles("Primera etapa") nonumbers noobs ///
	title("Salto en la probabilidad de presentar el examen nuevo")

**# (h) Estimador de Wald

	rdrobust puntos_demerito dias_corte, c(0) p(1) kernel(triangular) fuzzy(examen_nuevo)
	est store h_fuzzy
	rdres "(h) FRD Wald: efecto examen nuevo"
	local h_fs = e(h_l)
	
	*Tabla del estimador de FRD (los se y p-value son robustos)
	esttab h_fuzzy using "Resultados/tabla_h_fuzzy.rtf", replace ///
	cells(none) stats (tau_cl tau_bc se_tau_rb pv_rb h_l h_r N_h_l N_h_r, ///
	label ("Coeficiente convencional" "Coeficiente corregido por sesgo" ///
	"Error estándar" "p-valor" "Ancho de banda izquierdo" "Ancho de banda derecho" ///
	"N efectivo izquierda" "N efectivo derecha") fmt(3 3 3 4 3 3 0 0)) ///
	mtitles("Efecto causal FRD") nonumbers noobs ///
	title("Efecto del examen nuevo sobre los puntos de demérito")
	

	* Cálculo manual de Wald: salto en Y / salto en D, mismo h
	qui rdrobust examen_nuevo dias_corte, c(0) p(1) kernel(triangular) h(`h_fs')
	scalar fs = e(tau_cl)
	quietly rdrobust puntos_demerito dias_corte, c(0) p(1) kernel(triangular) h(`h_fs')
	scalar rf = e(tau_cl)
	scalar wald = rf/fs
	display "Salto en Y (forma reducida) = " %9.4f rf
	display "Salto en D (primera etapa)  = " %9.4f fs
	display "Estimador de Wald           = " %9.4f wald

	* MC2E local a mano, para verificar
	gen w  = 1 - abs(dias_corte)/`h_fs' if abs(dias_corte) < `h_fs'
	gen zx = asignacion*dias_corte  // interacción
	ivregress 2sls puntos_demerito dias_corte zx (examen_nuevo = asignacion) ///
		[aw = w] if abs(dias_corte) < `h_fs', vce(robust)
		
	est store h_2sls
	estat firststage
	
	*Tabla resultados de MC2E local
	esttab h_2sls using "Resultados/tabla_h_2sls.rtf", replace ///
    keep(examen_nuevo dias_corte zx) order(examen_nuevo dias_corte zx) ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    varlabels( examen_nuevo "Examen nuevo presentado (D)" ///
	dias_corte "Días respecto al corte (0 = 1 de enero de 2005)" zx "Asignacion x dias") ///
    stats(N, labels("Observaciones") fmt(0)) ///
    title("MC2E local: comprobacion del efecto causal") ///
    addnotes("Errores estandar robustos.")
