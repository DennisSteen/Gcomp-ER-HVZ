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
		/*AND dag90 < &maxTimePoint.*/ /*At the end of the database all patients will be censored*/
;
QUIT;

%macro Medication_models();
%LET medications = C10AA C03AA C03DA C07A C08 C09A C09C C10AB C10AC C10AD C10AX ;

%DO m = 1 %TO 1;
%LET medication = %SCAN(&medications.,&m.);

DATA Modeldata_&medication.;
	SET PhD.COMPLETESET_90DAGEN;
	
	BY zcl_rel_nr;
	paying_lag_&medication. = LAG(paying_&medication.); /*1 if patients paid for medication between T-2 and T-1*/
	prev_left_&medication. = LAG(left_&medication.); /*Amount of medication left at time T-1*/
	prev_user_&medication. = LAG(user_&medication.); /*1 if patient used medication between T-2 and T-1*/
	IF first.zcl_rel_nr THEN DO;
		prev_left_&medication. = .;
		prev_user_&medication. = 0;
	END;
	IF user_&medication. = 1 
		AND prev_user_&medication. = 1; /*Only do a prediction on patients that use medication and used medication at the previous time point*/

	offset_&medication. = COALESCE(LOG(prev_left_&medication.),0); /*Use the medication amount on t-1 as offset*/

	IF jaar = 2013 AND ER_VERPL_CUM + ER_VRIJW_CUM =< 349 + ER_BEDRAG THEN Paying = 1;
	ELSE IF jaar = 2014 AND ER_VERPL_CUM + ER_VRIJW_CUM =< 359 + ER_BEDRAG THEN Paying = 1;
	ELSE IF jaar = 2015 AND ER_VERPL_CUM + ER_VRIJW_CUM =< 374 + ER_BEDRAG THEN Paying = 1;
	ELSE IF ER_VERPL_CUM + ER_VRIJW_CUM =< 384 + ER_BEDRAG THEN Paying = 1;
	ELSE Paying = 0; /*1 If patient did not reach deductible limit, 0 otherwise*/
RUN;

%DO t = 1 %TO %SYSFUNC(COUNTW(&TimePoints.));
%LET TimePoint = %SCAN(&TimePoints,&t.);

PROC GENMOD DATA=WORK.Modeldata_&medication.(WHERE=(dag90 = &TimePoint.))
;
	CLASS geslacht INCASSO_FREQ_OMS BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS ER_REGELING
	;
	MODEL left_&medication. = leeftijd_index Reuma COPD Diabetes geslacht paying paying_lag_&medication.
			user_%SYSFUNC(TRANWRD(%SYSFUNC(TRANSTRN(&medications.,%STR(&medication. ),%sysfunc(trimn(%str())))),%str( ),%str( user_)))
			AV_INDICATOR TV_INDICATOR ER_INDICATOR HERVERZEKERING_ER FKG DKG HKG ER_REGELING INCASSO_FREQ_OMS BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS
		/ /*Predict the amount of medication at time T, based on covariates.*/
		/*LINK=LOG*/
		DIST=ZIP
		OFFSET=offset_&medication.;
		ZEROMODEL leeftijd_index Reuma COPD Diabetes geslacht paying paying_lag_&medication.
			user_%SYSFUNC(TRANWRD(%SYSFUNC(TRANSTRN(&medications.,%STR(&medication. ),%sysfunc(trimn(%str())))),%str( ),%str( user_)))
			AV_INDICATOR TV_INDICATOR ER_INDICATOR HERVERZEKERING_ER FKG DKG HKG ER_REGELING INCASSO_FREQ_OMS BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	;
	STORE OUT=Model.Pills_&medication._&TimePoint.; /*Save model*/
RUN; QUIT;
%END;
%END;
%MEND;
%Medication_models();