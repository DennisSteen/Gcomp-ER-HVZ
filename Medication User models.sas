PROC SQL NOPRINT;
	SELECT MAX(dag90)
	INTO :maxTimePoint
	FROM Phd.COMPLETESET_90DAGEN
;
	SELECT DISTINCT dag90
	INTO :TimePoints SEPARATED BY " "
	FROM PhD.COMPLETESET_90DAGEN
	WHERE dag90 > 0
		AND dag90 < 3000
;
QUIT;


%macro MedicationUser_models();
DATA Modeldata;
	SET PhD.COMPLETESET_90DAGEN;
	
	BY zcl_rel_nr;

	AV_lag = lag(AV_INDICATOR);
	TV_lag = lag(TV_INDICATOR);
	VER_lag = lag(ER_INDICATOR );
	HER_lag = lag(HERVERZEKERING_ER);
	GBER_lag = lag(ER_REGELING);
	Diabetes_lag = lag(Diabetes);
	COPD_lag = lag(COPD);
	Reuma_lag = lag(Reuma);
/*	Dialyse_lag = lag(Dialyse);*/
	Depressie_lag = lag(Depressie);
	%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
		%SCAN(&medications.,&m.)_lag = lag(user_%SCAN(&medications.,&m.));
	%END;

	IF first.zcl_rel_nr THEN DO;
		AV_lag = .;
		TV_lag = .;
		VER_lag = .;
		HER_lag = .;
		GBER_lag = .;
		Diabetes_lag = .;
		COPD_lag = .;
		Reuma_lag = .;
/*		Dialyse_lag = .;*/
		Depressie_lag = .;
		%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
			%SCAN(&medications.,&m.)_lag = .;
		%END;
	END;
RUN;

%DO t = 1 %TO %SYSFUNC(COUNTW(&TimePoints.));
	%LET TimePoint = %SCAN(&TimePoints,&t.);

	%DO i = 1 %TO %SYSFUNC(COUNTW(&medications.));
	%LET medication = %SCAN(&medications.,&i.);

	ODS EXCLUDE ALL;
	PROC HPLOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. AND &medication._lag = 0)) NAMELEN=32;
		CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;

		MODEL user_&medication.(event='1')=leeftijd_index geslacht leeftijd_index*leeftijd_index leeftijd_index*geslacht BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS BN_INKOMEN_OMS FKG DKG HKG AV_indicator TV_indicator ER_indicator HERVERZEKERING_ER ER_REGELING Diabetes Reuma COPD Depressie		
					%IF &TimePoint. > 0 %THEN %DO;
						%SYSFUNC(TRANWRD(%SYSFUNC(TRANSTRN(&medications.,%STR(&medication. ),%sysfunc(trimn(%str())))),%str( ),%str(_lag )))_lag
					%END; 
				 /LINK=LOGIT;
		SELECTION METHOD=BACKWARD(CHOOSE=SBC STOP=SBC SELECT=SBC);
		ODS output ParameterEstimates=PE;	
	RUN;

	ODS EXCLUDE NONE;
	PROC SQL NOPRINT;
		SELECT EFFECT
		INTO :EFFECTS SEPARATED BY " "
		FROM PE
		WHERE EFFECT NE 'Intercept'
	;
	QUIT;

	PROC LOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. AND &medication._lag = 0)) ;
		CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;

		MODEL user_&medication.(event='1') = &effects. / LINK=LOGIT;
		STORE model.User_&medication._&TimePoint.;
	RUN;
	%END;
%END;

PROC DATASETS NOLIST LIB=WORK;
	DELETE Modeldata;
QUIT;
%MEND;
%MedicationUser_models();