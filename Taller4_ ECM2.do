/* - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - 
*																			   *
*						     Taller 4 Econometría 2						       *
*								Parte práctica								   *
*																			   *
* - * - * - * - * - * - * - * - *  - * - * - * - * - * - * - * - * - * - * - * */

cls
clear all
set more off

cd "C:\Users\majo_\OneDrive\Escritorio\Econometría 2\Taller 4"
global dir "C:\Users\majo_\OneDrive\Escritorio\Econometría 2\Taller 4"

use "$dir\valtierra_tdap", clear
cap ssc install estout, replace
global rta "$dir\Resultados"
cap mkdir "$rta"

label var muni_id "Identificador municipal"
label var anio "Año"
label var region "Región"
label var pct_urbano "Población urbana (%)"
label var tratado "Municipio tratado"
label var post "Periodo posterior (desde 2018)"
label var mandato "Mandato (tratado x post)"
label var cobertura_tdap "Cobertura de adolescentes vacunados"
label var tos_ferina_100k "Casos de tos ferina por 100.000 habitantes"
label var ingreso_pc "Ingreso per cápita"
label var medicos_1000 "Médicos por 1.000 habitantes"

**# (a) Declaración de la base como panel y estadísticas descriptivas

xtset muni_id anio
xtdescribe

xtsum pct_urbano cobertura_tdap tos_ferina_100k ingreso_pc ///
	medicos_1000 tratado post mandato


estpost tabstat pct_urbano cobertura_tdap tos_ferina_100k ingreso_pc ///
	medicos_1000 tratado post mandato, ///
	statistics(n mean sd min p50 max) columns(statistics)
	
esttab using "$rta\Tabla1_estadísticos.rtf", replace ///
    cells("count(fmt(0)) mean(fmt(3)) sd(fmt(3)) min(fmt(3)) p50(fmt(3)) max(fmt(3))") ///
    collabels("N" "Media" "Desv. est." "Mínimo" "Mediana" "Máximo") ///
    nonumber nomtitle noobs label ///
    title("Tabla 1. Estadísticas descriptivas between y within ") ///
	
	putexcel set "tabla1_xtsum.xlsx", replace sheet("Tabla1")
	putexcel A1 = "Variable" B1 = "Media" C1 = "Sd Overall" D1 = "DE Between" ///
			 E1 = "DE Within" F1 = "Mín" G1 = "Máx" H1 = "N" I1 = "n" J1 = "T"
			 
	local vars pct_urbano cobertura_tdap tos_ferina_100k ingreso_pc medicos_1000 tratado post mandato
	
	local i = 2
	foreach v of local vars {
		quietly xtsum `v'
		local lbl : variable label `v'
		putexcel A`i' = "`lbl'"
		putexcel B`i' = (r(mean)) C`i' = (r(sd)) D`i' = (r(sd_b)) E`i' = (r(sd_w))
		putexcel F`i' = (r(min))  G`i' = (r(max))
		putexcel H`i' = (r(N))    I`i' = (r(n))    J`i' = (r(Tbar))
		local ++i
	}

	putexcel B2:G9, nformat("0.000")
	putexcel A1:J1, bold

**# (b) Cálculo DiD

mean cobertura_tdap, over(tratado post)

qui summarize cobertura_tdap if tratado==1 & post==1 // Tratado post
scalar media_tp = r(mean)
scalar n_tp = r(N)
qui summarize cobertura_tdap if tratado==1 & post==0 // Tratado antes
scalar media_ta = r(mean)
scalar n_ta = r(N)
qui summarize cobertura_tdap if tratado==0 & post==1 // Control post
scalar media_cp = r(mean)
scalar n_cp = r(N)
qui summarize cobertura_tdap if tratado==0 & post==0 // Control antes
scalar media_ca = r(mean)
scalar n_ca = r(N)

* DiD manual
scalar did_manual = (media_tp - media_ta) - (media_cp - media_ca)
display "DiD manual = " %9.3f scalar(did_manual)
putexcel set "$rta\Tabla2_medias_DiD.xlsx", replace

putexcel A1 = ("Tabla 2. Medias de cobertura para el cálculo DiD manual")
putexcel A3 = ("Grupo") B3 = ("Tratado") C3 = ("Post") D3 = ("Observaciones") ///
        E3 = ("Media de cobertura (%)")

putexcel A4 = ("Tratados, después") B4 = (1) C4 = (1) D4 = (scalar(n_tp)) E4 = (scalar(media_tp))
putexcel A5 = ("Tratados, antes") B5 = (1) C5 = (0) D5 = (scalar(n_ta)) E5 = (scalar(media_ta))
putexcel A6 = ("Controles, después") B6 = (0) C6 = (1) D6 = (scalar(n_cp)) E6 = (scalar(media_cp))
putexcel A7 = ("Controles, antes") B7 = (0) C7 = (0) D7 = (scalar(n_ca)) E7 = (scalar(media_ca))
putexcel A9 = ("Diferencias en diferencias (DiD)") E9 = (scalar(did_manual))

* DiD mediante regresión
regress cobertura_tdap i.tratado##i.post, vce(cluster muni_id)
est store did

estadd scalar DiD_manual = scalar(did_manual)
esttab did using "$rta\Tabla_3_regresion_DiD.rtf", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    keep(1.tratado 1.post 1.tratado#1.post) ///
    order(1.tratado 1.post 1.tratado#1.post) ///
    coeflabels(1.tratado "Tratado" 1.post "Después de 2018" ///
               1.tratado#1.post "Mandato (Tratado x post)") ///
    stats(N r2 DiD_manual, fmt(0 3 3) ///
          labels("Observaciones" "R-cuadrado" "DiD manual")) ///
    title("Tabla 3. Diferencias en diferencias: cobertura de adolescentes vacunados") ///
    addnotes("Errores estándar agrupados por municipio entre paréntesis." ///
             "* p<0,10; ** p<0,05; *** p<0,01." ///
             "El coeficiente de la interacción es la doble diferencia.")

**# (c) Estimación por diversos modelos

* 1. MCO agrupado (pooled OLS): no controla por heterogeneidad fija municipal.
regress cobertura_tdap mandato ingreso_pc medicos_1000, vce(cluster muni_id)
est store pooled

* 2. Primera diferencia: cambios anuales, uso de dummies de año.

/* En PD,  D. calcula el cambio respecto al año anterior. La primera observación de cada municipio queda missing en las diferencias. */

gen dif_cob = D.cobertura_tdap
gen dif_man = D.mandato
gen dif_ingpc = D.ingreso_pc
gen dif_med = D.medicos_1000

regress dif_cob dif_man dif_ingpc dif_med, vce(cluster muni_id)
est store pd

* 3. Within / efectos fijos (EF) intragrupos: elimina los componentes municipales constantes.
xtreg cobertura_tdap mandato ingreso_pc medicos_1000, fe vce(cluster muni_id)
est store within

* 4. LSDV: MCO con dummy por municipio.
qui regress cobertura_tdap mandato ingreso_pc medicos_1000 i.muni_id, ///
    vce(cluster muni_id)
est store lsdv

esttab pooled pd within lsdv using "$rta\Tabla_4_metodos_estimación.rtf", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("MCO agrupado" "Primera diferencia" "Within / FE" "LSDV") ///
    keep(mandato ingreso_pc medicos_1000 dif_man dif_ingpc dif_med) ///
    order(mandato ingreso_pc medicos_1000 dif_man dif_ingpc dif_med) ///
    stats(N r2, fmt(0 3) labels("Observaciones" "R-cuadrado")) ///
    title("Tabla 4. Estimadores para la cobertura de adolescentes vacunados") ///
    addnotes("Errores estándar agrupados por municipio entre paréntesis." ///
             "* p<0,10; ** p<0,05; *** p<0,01.")

**# (d) Comparación y efectos de año
* (iv) Efectos fijos de año

xtreg cobertura_tdap mandato ingreso_pc medicos_1000 i.anio, fe vce(cluster muni_id)
est store fe_anio

qui regress cobertura_tdap mandato ingreso_pc medicos_1000 i.muni_id i.anio, vce(cluster muni_id)
est store lsdv_anio

esttab within fe_anio lsdv lsdv_anio using "$rta\Tabla5_efectos_fijos_anio.rtf", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Within / FE" "Within / FE + año" "LSDV" "LSDV + año") ///
    keep(mandato ingreso_pc medicos_1000) order(mandato ingreso_pc medicos_1000) ///
    coeflabels(Mandato "Mandato (tratado x post)" Ingreso_pc "Ingreso per cápita" ///
               medicos_1000 "Médicos por 1.000 habitantes") ///
    stats(N, fmt(0 3) labels("Observaciones")) ///
    title("Tabla 5. Estimadores para la cobertura de adolescentes vacunados usando efectos fijos municipales y de año") ///
    addnotes("Errores estándar agrupados por municipio entre paréntesis." ///
             "* p<0,10; ** p<0,05; *** p<0,01.")

		 
**# (e) Estimación por diversos incluyendo la proporción de población urbana

* 1. MCO agrupado (pooled OLS)
regress cobertura_tdap mandato ingreso_pc medicos_1000 pct_urbano, vce(cluster muni_id)
est store pooled_urb

* 2. Primera diferencia
gen dif_pctur = D.pct_urbano
regress dif_cob dif_man dif_ingpc dif_med dif_pctur, vce(cluster muni_id)
est store pd_urb

* 3. Efectos fijos (EF) intragrupos
xtreg cobertura_tdap mandato ingreso_pc medicos_1000 pct_urbano, fe vce(cluster muni_id)
est store within_urb

* 4. LSDV
qui regress cobertura_tdap mandato ingreso_pc medicos_1000 i.muni_id pct_urbano, vce(cluster muni_id)
est store lsdv_urb

esttab pooled_urb pd_urb within_urb lsdv_urb using "$rta\Tabla_6_metodos_estimación2.rtf", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("MCO agrupado" "Primera diferencia" "Within / FE" "LSDV") ///
    keep(mandato ingreso_pc medicos_1000 pct_urbano dif_man dif_ingpc dif_med dif_pctur) ///
    order(mandato ingreso_pc medicos_1000 pct_urbano dif_man dif_ingpc dif_med dif_pctur) ///
    stats(N r2, fmt(0 3) labels("Observaciones" "R-cuadrado")) ///
    title("Tabla 6. Estimadores para la cobertura de adolescentes vacunados incluyendo la proporción de población urbana") ///
    addnotes("Errores estándar agrupados por municipio entre paréntesis." ///
             "* p<0,10; ** p<0,05; *** p<0,01.")
			 
**# (f) Estimación por diversos incluyendo la proporción de población urbana

*Reportar el estadístico F
xtreg cobertura mandato ingreso_pc medicos_1000, fe

scalar F_municipios = e(F_f)
scalar gl1_municipios = e(N_g) - 1
scalar gl2_municipios = e(df_r)
scalar p_municipios = Ftail(gl1_municipios, gl2_municipios, F_municipios)

putdocx clear
putdocx begin
putdocx paragraph
putdocx text ("Tabla 7. Estadístico F: test de significancia conjunta heterogeneidad municipal")
putdocx table tablaF = (2,5), border(all, single)
putdocx table tablaF(1,1) = ("H0")
putdocx table tablaF(1,2) = ("Estadístico F")
putdocx table tablaF(1,3) = ("gl numerador")
putdocx table tablaF(1,4) = ("gl denominador")
putdocx table tablaF(1,5) = ("Valor p")

putdocx table tablaF(2,1) = ("Todos los efectos municipales son cero")
putdocx table tablaF(2,2) = (scalar(F_municipios)), nformat(%9.3f)
putdocx table tablaF(2,3) = (scalar(gl1_municipios))
putdocx table tablaF(2,4) = (scalar(gl2_municipios))
putdocx table tablaF(2,5) = (scalar(p_municipios)), nformat(%9.4f)

putdocx table tablaF(1,.), bold

putdocx save "$rta\Tabla_7.0_estadistico_F_FE.docx", replace

* 1. MCO agrupado (pooled OLS)
regress tos_ferina_100k mandato ingreso_pc medicos_1000 pct_urbano, vce(cluster muni_id)
est store pooled_tos

* 2. Primera diferencia
gen dif_tos_ferina_100k = D.tos_ferina_100k
regress dif_tos_ferina_100k dif_man dif_ingpc dif_med dif_pctur, vce(cluster muni_id)
est store pd_tos

* 3. Efectos fijos (EF) intragrupos
xtreg tos_ferina_100k mandato ingreso_pc medicos_1000 pct_urbano, fe vce(cluster muni_id)
est store within_tos

* 4. LSDV
regress tos_ferina_100k mandato ingreso_pc medicos_1000 i.muni_id pct_urbano, vce(cluster muni_id)
est store lsdv_tos

esttab pooled_tos pd_tos within_tos lsdv_tos using "$rta\Tabla_7_metodos_estimación3.rtf", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("MCO agrupado" "Primera diferencia" "Within / FE" "LSDV") ///
    keep(mandato ingreso_pc medicos_1000 pct_urbano dif_man dif_ingpc dif_med dif_pctur) ///
    order(mandato ingreso_pc medicos_1000 pct_urbano dif_man dif_ingpc dif_med dif_pctur) ///
    stats(N r2, fmt(0 3) labels("Observaciones" "R-cuadrado")) ///
    title("Tabla 8. Estimadores para los casos de tos ferina por cada 100.000 habitantes") ///
    addnotes("Errores estándar agrupados por municipio entre paréntesis." ///
             "* p<0,10; ** p<0,05; *** p<0,01.")
