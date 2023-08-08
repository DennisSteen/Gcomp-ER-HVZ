LIBNAME PHD '\\ka0089.loods2.org\Actuariaat\SAS Data\DennisS\PhD';
LIBNAME Model '\\ka0089.loods2.org\Actuariaat\SAS Data\DennisS\PhD\Modellen A4';

PROC SQL NOPRINT;
/*	SELECT MAX(dag90)
	INTO :maxTimePoint
	FROM Phd.COMPLETESET_90DAGEN
;*/
	SELECT DISTINCT dag90
	INTO :TimePoints SEPARATED BY " "
	FROM PhD.COMPLETESET_90DAGEN
	WHERE dag90 > 0 /*Patients have to be followd for at least a year*/
		AND dag90 < 3000 /*At the end of the database all patients will be censored*/
;
QUIT;

%macro Medication_models();
%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
%LET medication = %SCAN(&medications.,&m.);

DATA Modeldata_&medication.;
	SET PhD.COMPLETESET_90DAGEN;
	
	BY zcl_rel_nr;
	paying_lag_&medication. = LAG(paying_&medication.); /*1 if patients paid for medication between T-2 and T-1*/
	prev_user_&medication. = LAG(user_&medication.); /*1 if patient used medication between T-2 and T-1*/
	adherence_class_&medication. = PUT(adherence_&medication.,adherencefmt.);

	IF first.zcl_rel_nr THEN DO;
		prev_user_&medication. = 0;	
	END;
	IF user_&medication. = 1 AND prev_user_&medication. = 0 THEN adherence_class_&medication. = '9';

	prev_adherence_class = lag(adherence_class_&medication.);
	paying_&medication._lag = lag(paying_&medication.);
	PickUp_&medication._lag = lag(PickUP_&medication.);

	IF first.zcl_rel_nr THEN DO;
		prev_adherence_class = '9';	
	END;
	
	IF user_&medication. = 1 
		AND prev_user_&medication. = 1 THEN OUTPUT; /*Only do a prediction on patients that use medication and used medication at the previous time point*/	
RUN;

%DO t = 1 %TO %SYSFUNC(COUNTW(&TimePoints.));
%LET TimePoint = %SCAN(&TimePoints,&t.);

ODS EXCLUDE ALL;
PROC HPLOGISTIC DATA=WORK.Modeldata_&medication.(WHERE=(dag90 = &TimePoint.));
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS %IF &TimePoint. > 90 %THEN prev_adherence_class;
	MODEL adherence_class_&medication.(DESCENDING) = leeftijd_index Reuma COPD Diabetes Depressie geslacht paying pickup_&medication. BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS
			user_%SYSFUNC(TRANWRD(%SYSFUNC(TRANSTRN(&medications.,%STR(&medication. ),%sysfunc(trimn(%str())))),%str( ),%str( user_)))
			AV_INDICATOR TV_INDICATOR ER_INDICATOR HERVERZEKERING_ER FKG DKG HKG ER_REGELING 
			%IF &TimePoint. > 90 %THEN prev_adherence_class;
		/ LINK=LOGIT;
	SELECTION METHOD=BACKWARD(CHOOSE=SBC SELECT=SBC STOP=SBC);
	ODS OUTPUT ParameterEstimates=PE;
RUN;

ODS EXCLUDE NONE;
PROC SQL NOPRINT;
	SELECT DISTINCT effect
	INTO :effects SEPARATED BY " "
	FROM PE
	WHERE effect NE 'Intercept'
;
QUIT;

PROC LOGISTIC DATA=WORK.Modeldata_&medication.(WHERE=(dag90 = &TimePoint.)) DESCENDING NOPRIN;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS %IF &TimePoint. > 90 %THEN prev_adherence_class;
	;
	MODEL adherence_class_&medication. = &effects. / LINK=CUMLOGIT;
	STORE OUT=Model.Adherence_&medication._&TimePoint.; /*Save model*/
RUN; QUIT;

%END;
%END;

PROC DATASETS NOLIST LIB=WORK;
	DELETE Modeldata_:;
QUIT;
%MEND;
%Medication_models();