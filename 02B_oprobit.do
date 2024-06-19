recode formalidad ("Sin_Aportes"=1 "Aportes_Propios"=2 "Aportes_Empleador"=3), generate(formalidad_ordenada)

oprobit formalidad_ordenada i.educf i.edadi i.region
margins, dydx(*)