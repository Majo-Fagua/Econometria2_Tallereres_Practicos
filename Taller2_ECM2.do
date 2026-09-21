/* - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - 
*																			   *
*						     Taller 1 Econometría 2						       *
*								Parte práctica								   *
*																			   *
* - * - * - * - * - * - * - * - *  - * - * - * - * - * - * - * - * - * - * - * */

clear all
cap log close
set more off

cd "C:\Users\majo_\OneDrive\Escritorio\Econometría 2\Taller 2"
global dir "C:\Users\majo_\OneDrive\Escritorio\Econometría 2\Taller 2"
use "$dir\choco_conectado.dta" 

	ssc install estout, replace
	ssc install outreg2, replace
	
**# 1. Fase II: Chocó conectado

**# a. Tabla de contingencia y comparación de medias
	
	tab L_i D_i, row
	
	estpost tabulate L_i D_i

	esttab . using "$dir\tab_LD.rtf", replace ///
	cells("b(fmt(0)) rowpct(fmt(2))") ///
	unstack noobs nonumber ///
	collabels("Frecuencia" "% fila") ///
	title("Tabla de contingencia entre ganar el sorteo del voucher y estar efectivamente conectado a internet")
	
	** Comparación de medias entre los obedientes y los no tomadores que ganaron.
	
	local control electricidad_i dist_cabecera_i zona_rural_i

	label var electricidad_i "Energía electrica estable (8hrs)"
	label var dist_cabecera_i "Distancia a cabecera"
	label var zona_rural_i "Zona rural"
	
	estpost ttest `control' if L_i == 1 , by(D_i)
	esttab using "$dir\ttest.rtf", ///
	cells("mu_1(fmt(3)) mu_2(fmt(3)) b(fmt(3)) p(fmt(3))") ///
    collabels("Media ganadores no conectados" "Media ganadores conectados" "Diferencia" "p-valor") ///
    label nonumber noobs ///
	title("Prueba de diferencia de medias entre ganadores") replace

**# b. Estimación por MCO el efecto de estar conectado sobre el ingreso_i

	label var D_i         "El hogar está conectado a internet"
	label var L_i         "El hogar ganó el sorteo del voucher"
	label var nbi_i       "NBI"
	label var educ_jefe_i "Educación jefe del hogar"
	label var ingreso_i   "Ingreso per cápita (COP)"

	reg ingreso_i D_i nbi_i educ_jefe_i, robust
	
	outreg2 using "$dir\reg_1b.doc", replace ///
	ctitle("Efecto de estar conectado a internet sobre el ingreso del hogar") ///
	label bdec(3) sdec(3) ///
	addnote("NBI: índice de necesidades básicas insatisfechas.")
	
**# c. Efecto de intención de tratar (ITT)

	reg ingreso_i L_i nbi_i educ_jefe_i, robust
	
	outreg2 using "$dir\reg_1b.doc", replace ///
	ctitle("Efecto de intención de tratar (ITT)") ///
	label bdec(3) sdec(3)
	
**# d. Estimación manual 2SLS: primera etapa

	reg D_i L_i nbi_i educ_jefe_i // regresión auxiliar
	predict D_hat, xb // Linear predictión. Efecto exógeno de D_i
	test L_i
	local Ftest_Li = r(F)
	
	outreg2 using "$dir\first_stage.doc", replace ///
	ctitle("Primera etapa: efecto del sorteo sobre la conexión a internet del hogar") ///
	label bdec(3) sdec(3) ///
	addnote("Estadístico F del instrumento excluído: ", `Ftest_Li' )
	
**# e. Estimación efecto D_i por MC2E

	*(i) Procedimiento manual
		
		* Primera etapa realizada en item d	
		* Segunda etapa 
		
		reg ingreso_i D_hat nbi_i educ_jefe_i
		est store manreg_2sls
		estadd scalar variance = e(rmse)^2 : manreg_2sls 
		outreg2 using "$dir\second_stage.doc", replace ///
		ctitle("Segunda etapa (manual): efecto de estar conectado a internet sobre el ingreso del hogar") ///
		label bdec(3) sdec(3)
	*(ii) Procedimiento con ivregress 
	
		ivregress 2sls ingreso_i nbi_i educ_jefe_i (D_i = L_i), first
		est store ivreg_2sls
		estadd scalar fstat = `Ftest_Li': ivreg_2sls
		estadd scalar variance = e(rmse)^2 : ivreg_2sls
		
		ivregress 2sls ingreso_i nbi_i educ_jefe_i (D_i = L_i)
		outreg2 using "$dir\second_stageiv.doc", replace ///
		ctitle("Estimación por ivreg: Efecto de estar conectado a internet sobre el ingreso del hogar") ///
		label bdec(3) sdec(3)
			
	* Tabla comparativa
	esttab manreg_2sls ivreg_2sls using "$dir\reg_1e.rtf", replace ///
	b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("MC2E Manual" "MC2E ivregres") ///
	title("Efecto de estar conectado sobre el ingreso: estimación por MC2E") ///
	rename(D_hat D_i) ///
	keep(D_i _cons) ///
	stats(variance, fmt(3) labels("Varianza del error")) ///
	label noobs
		
**# f. MC2E = ITT/ coeff 1era etapa
	
	qui reg ingreso_i L_i
	scalar b_ITT = _b[L_i]
	
	qui reg D_i L_i
	scalar coeff_1e = _b[L_i]
	
	*Estimador de Wald
	scalar b_MC2E_ratio = b_ITT / coeff_1e
	display "Estimador de Wald: " b_MC2E_ratio 
	
	* Verificación. Estimación MC2E sin controles con ivreg
	ivregress 2sls ingreso_i (D_i = L_i)
	scalar b_MC2E_iv =  _b[D_i]
	display "Estimador MC2E con IV: " b_MC2E_iv
	
	* Comprobar que son iguales
	scalar diferencia = b_MC2E_ratio - b_MC2E_iv
	display "Diferencia: " diferencia
	
	outreg2 using "$dir\Resultados_MC2E.doc", replace ///
    ctitle("MC2E con IV sin controles") bdec(3) sdec(3) label ///
    addstat("Wald chi2", e(chi2), "Estimador de Wald", b_MC2E_ratio, "Estimador MC2E con IV", b_MC2E_iv, "Diferencia", diferencia)
	
**# 2. Reunión con los cooperantes

**# a. Propuesta uso de la distancia la cabecera municipal

	label var pct_pobl_afro_i "Proporción de población afrocolombiana"

	reg D_i dist_cabecera_i nbi_i educ_jefe_i // regresión auxiliar
	test dist_cabecera_i
	local Ftest_dist = r(F)
	
	outreg2 using "$dir\2a_first_stage.doc", replace ///
	ctitle("Primera etapa: efecto de la distancia a la cabecera municipal sobre la conexión") ///
	label bdec(3) sdec(3) ///
	addstat("Estadístico F del instrumento excluído", `Ftest_dist' )
	
	ivregress 2sls ingreso_i nbi_i educ_jefe_i (D_i = dist_cabecera_i)
	est store m2a_ivreg_ls2s
	estadd scalar fstat= `Ftest_dist': m2a_ivreg_ls2s
	
	esttab m2a_ivreg_ls2s using "$dir\ivregls2s_2a.rtf", replace ///
	b(4) se(4) r2 pr2 star(* 0.10 ** 0.05 *** 0.01) ///
	title("Efecto de estar conectado a internet sobre el ingreso del hogar con la distancia a la cabecera municipal como instrumento") ///
	label noobs
	
	** Falta verificar la dirección del sesgo
	
**# b. Propuesta uso de la proporción de población afrocolombiana del entorno del hogar

	reg D_i pct_pobl_afro_i nbi_i educ_jefe_i // regresión auxiliar
	test pct_pobl_afro_i
	local Ftest_afro = r(F)
	
	outreg2 using "$dir\2b_first_stage.doc", replace ///
	ctitle("Primera etapa: efecto de la proporción de población afrocolombiana del entorno del hogar sobre la conexión") ///
	label bdec(3) sdec(3) ///
	addstat("Estadístico F del instrumento excluído: ", `Ftest_afro' )
	
	ivregress 2sls ingreso_i nbi_i educ_jefe_i (D_i = pct_pobl_afro_i)
	est store m2b_ivreg_ls2s
	estadd scalar fstat = `Ftest_afro' : m2b_ivreg_ls2s
	
	esttab m2b_ivreg_ls2s using "$dir\ivregls2s_2b.rtf", replace ///
	b(4) se(4) r2 pr2 star(* 0.10 ** 0.05 *** 0.01) ///
	title("Efecto de estar conectado a internet sobre el ingreso del hogar con la proporción de población afrocolombiana del entorno del hogar como instrumento") ///
	label noobs

**# c. MC2E con L_i y brigada_i como instrumentos de D_i y test de sobreidentificación.
	label var brigada_i "El hogar recibió la brigada de instalación técnica"

	* Primera etapa conjunta (regresión auxiliar con los dos instrumentos)
	reg D_i L_i brigada_i nbi_i educ_jefe_i
	test L_i brigada_i
	local Ftest_conj = r(F)

	outreg2 using "$dir\2c_first_stage.doc", replace ///
	ctitle("Primera etapa: efecto conjunto de L_i y brigada_i sobre la conexión") ///
	label bdec(3) sdec(3) ///
	addstat("Estadístico F conjunto de los instrumentos excluidos", `Ftest_conj')

	* MC2E sobreidentificado (z=2, g=1)
	ivregress 2sls ingreso_i nbi_i educ_jefe_i (D_i = L_i brigada_i), first
	est store m2c_ivreg_2sls
	
	* Test de sobreidentificación
	estat overid
	local sargan_chi2 = r(sargan)
	local sargan_df = r(df)
	local sargan_p = r(p_sargan)

	estadd scalar sargan = `sargan_chi2' : m2c_ivreg_2sls
	estadd scalar sargan_df = `sargan_df'   : m2c_ivreg_2sls
	estadd scalar sargan_p  = `sargan_p'    : m2c_ivreg_2sls
	
	esttab m2c_ivreg_2sls using "$dir\ivregls2s_2c.rtf", replace ///
	b(4) se(4) r2 pr2 star(* 0.10 ** 0.05 *** 0.01) ///
	title("Efecto de estar conectado sobre el ingreso: MC2E con ganar el sorteo del voucher y recibir brigada de instalación técnica como variables instrumentales") ///
	label noobs ///
	stats(sargan sargan_df sargan_p N, fmt(4 0 4 0) ///
	labels("Sargan chi2" "GDL" "Sargan p-valor" "Observaciones"))

	

	
* Tabla comparativa de instrumentos
	esttab ivreg_2sls m2a_ivreg_ls2s m2b_ivreg_ls2s m2c_ivreg_2sls ///
	using "$dir\comparación_instrumentos.rtf", replace ///
	b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Ganar el sorteo del voucher" "Distancia a cabecera" "Proporción pob, afrocolombiana") ///
	title("Efecto de estar conectado sobre el ingreso: comparación de instrumentos por MC2E") ///
	keep(D_i _cons) ///
	stats(fstat, fmt(3) ///
	labels("F instrumento(s) excluido(s)")) ///
	label noobs
	
		

