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


%macro MedicationPickUp_models();
DATA Modeldata;
	SET PhD.COMPLETESET_90DAGEN;
	
	IF jaar = 2013 AND ER_VERPL_CUM + ER_VRIJW_CUM =< 349.99 + ER_BEDRAG THEN Paying = 1;
	ELSE IF jaar = 2014 AND ER_VERPL_CUM + ER_VRIJW_CUM =< 359.99 + ER_BEDRAG THEN Paying = 1;
	ELSE IF jaar = 2015 AND ER_VERPL_CUM + ER_VRIJW_CUM =< 374.99 + ER_BEDRAG THEN Paying = 1;
	ELSE IF ER_VERPL_CUM + ER_VRIJW_CUM =< 384.99 + ER_BEDRAG THEN Paying = 1;
	ELSE Paying = 0; /*1 If patient did not reach deductible limit, 0 otherwise*/

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
		adherence_%SCAN(&medications.,&m.)_lag = lag(adherence_%SCAN(&medications.,&m.));
		paying_%SCAN(&medications.,&m.)_lag = lag(paying_%SCAN(&medications.,&m.));
		pickup_%SCAN(&medications.,&m.)_lag = lag(pickup_%SCAN(&medications.,&m.));
	%END;
	Paying_lag = lag(Paying);


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
			adherence_%SCAN(&medications.,&m.)_lag = .;
			paying_%SCAN(&medications.,&m.)_lag = .;
			pickup_%SCAN(&medications.,&m.)_lag =.;
		%END;
		Paying_lag = .;
	END;

RUN;

%DO t = 1 %TO %SYSFUNC(COUNTW(&TimePoints.));
	%LET TimePoint = %SCAN(&TimePoints,&t.);

	%DO i = 1 %TO %SYSFUNC(COUNTW(&medications.));
	%LET medication = %SCAN(&medications.,&i.);

	ODS EXCLUDE ALL;
	PROC HPLOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. AND &medication._lag = 1)) NAMELEN=32;
		CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;

		MODEL pickup_&medication.(event='1')=paying_&medication._lag leeftijd_index geslacht BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS BN_INKOMEN_OMS FKG DKG HKG AV_indicator TV_indicator ER_indicator HERVERZEKERING_ER ER_REGELING			
					adherence_&medication._lag pickup_&medication._lag
					%IF &TimePoint. > 0 %THEN %DO;
						%SYSFUNC(TRANWRD(%SYSFUNC(TRANSTRN(&medications.,%STR(&medication. ),%sysfunc(trimn(%str())))),%str( ),%str(_lag )))_lag
					%END;
				/ LINK=LOGIT START=1;
		SELECTION METHOD=FORWARD(CHOOSE=SBC SELECT=SBC STOP=SBC);
		ODS OUTPUT ParameterEstimates=PE;
	RUN;

	PROC SQL NOPRINT;
		SELECT EFFECT
		INTO :EFFECTS SEPARATED BY " "
		FROM PE
		WHERE EFFECT NE 'Intercept'
	;
	QUIT;

	ODS EXCLUDE NONE;
	PROC LOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. AND &medication._lag = 1)) PLOTS(ONLY)=NONE NOPRINT;
		CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;

		MODEL pickup_&medication.(event='1') = &effects. / LINK=LOGIT;
		STORE OUT=Model.pickup_&medication._&TimePoint.;
	RUN;

	%END;
%END;

PROC DATASETS NOLIST LIB=WORK;
	DELETE Modeldata;
QUIT;
%MEND;
%MedicationPickUp_models();