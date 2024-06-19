use "02A_mlogit.dta", clear
mlogit formalidad i.educf i.edadi i.est_civ i.region, baseoutcome(1)