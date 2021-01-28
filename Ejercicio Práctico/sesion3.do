
***********************
***Sesion 3: FD y FE***
***********************

use kap36_daten.dta, clear /*check your stata's directory!!*/

* First one has to xtset the data 
* (though this is already saved in the dataset)
xtset id time

*****************************************
* Descriptives
*****************************************
* Listing the data
list id time wage marr, separator(6)

* Describing the data
xtdes
xtsum wage
xttab marr
xtline wage, overlay


twoway   (scatter wage time,                                          ///
             ylabel(0(1000)5000, grid angle(0))                       ///
             ymtick(500(1000)4500, grid)                              ///
			 msymbol(O) mcolor(black) c(L) lpattern(_))               ///
         (scatter wage time if marr==1,                               ///
		     msymbol(T) msize(medlarge) c(L) lpattern(l)),            /// 
		 xtitle("Tiempo (Olas)", size(large) margin(0 0 0 2))          ///
         ytitle("Salario Mensual en Euros", size(large) margin(0 2 0 0))    ///
         legend(label(1 "Antes del Matrimonio") label(2 "Después del Matrimonio")) ///
         title("")


******************************************
* Cross-sectional OLS regression at T=4
******************************************
regress  wage marr if time==4

******************************************
* Pooled OLS
******************************************
regress  wage marr                    

**********************
*First differences
**********************

sort id time

gen dwage= wage-L.wage /*if it does not work, set xtset one more time*/
gen dmarr= marr-L.marr
gen dtime= time-L.time

list id time wage marr dwage dmarr dtime

***Alternative 1 
reg dwage dmarr, noconstant
reg dwage dmarr
reg dwage dmarr dtime, noconstant

***Alternative 2
reg D.(wage marr)
reg D.(wage marr), noconstant

*******************************************
* Fixed-Effects Regression (within estimator)
*******************************************
xtreg wage marr, fe                   

* Within transformation by hand
egen      mwage = mean(wage), by(id)
egen      mmarr = mean(marr), by(id)
generate  wwage = wage - mwage
generate  wmarr = marr - mmarr

reg wwage wmarr /*SE is wrong*/

* LSDV procedure
tabulate id, gen(pers)

regress wage marr pers1-pers4, noconstant

* Plot the demeaned data
twoway  (scatter wwage wmarr if marr==0, jitter(2)                          ///
                 msymbol(O) mcolor(black))                                  ///
        (scatter wwage wmarr if marr==1, jitter(2)                          ///
		         msymbol(T) mcolor(black))                                  ///
        (lfit wwage wmarr, lcolor(black)),                                  ///
        legend(lab(1 "Antes del casamiento") lab(2 "Después del casamiento")             ///
		       lab(3 "FE línea de regresión"))                               ///
        ylabel(-400(100)400, grid angle(0))                                 ///
		xlabel(-0.5 0 0.5, format(%3.1f))                                   ///
        xtitle("Casamiento demeaned", size(large) margin(0 0 0 2))  ///
        ytitle("Salario demeaned", size(medlarge))           ///
        title("")


* Cluster SE
****************
regress wage marr, vce(cluster id)

