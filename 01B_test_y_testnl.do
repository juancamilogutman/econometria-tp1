cls
clear all
use "eph_1abc.dta"
gen edad2 = edad^2

reg logSal i.educf edad edad2 i.est_civ i.region
estimates store reg1

test 2 * _b[edad2] * 50 + _b[edad] = 0
estimates store testlineal
esttab testlineal using "testlineal.tex", replace

testnl _b[edad]/ (-2 * _b[edad2]) - 50 = 0
estimates store testnolineal
esttab testlineal using "testnolineal.tex", replace
