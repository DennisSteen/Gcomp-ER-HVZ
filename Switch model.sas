PROC SQL NOPRINT;
	SELECT MAX(dag90)
	INTO :maxTimePoint
	FROM Phd.COMPLETESET_90DAGEN
;
	SELECT DISTINCT dag90
	INTO :TimePoints SEPARATED BY " "
	FROM PhD.COMPLETESET_90DAGEN
	WHERE dag90 > 365 /*Will be follow for at least a year.*/
		AND dag90 < 3000
;
QUIT;


%macro Switch_model();
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
	FKG_lag = lag(FKG);
	DKG_lag = lag(DKG);
	HKG_lag = lag(HKG);
	%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
		%SCAN(&medications.,&m.)_lag = lag(user_%SCAN(&medications.,&m.));
	%END;
	Paying_lag = lag(paying);

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
		FKG_lag = .;
		DKG_lag = .;
		HKG_lag = .;
		%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
			%SCAN(&medications.,&m.)_lag = .;
		%END;
		paying_lag = .;
	END;

	IF INTCK('day',MDY(1,1,YEAR(datum)),datum) < 90
		OR Switch = 1 THEN OUTPUT;
RUN;

%DO t = 1 %TO %SYSFUNC(COUNTW(&TimePoints.));
%LET TimePoint = %SCAN(&TimePoints,&t.);

ODS EXCLUDE ALL;
PROC HPLOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint.)) NAMELEN=45;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL Switch(event='1')=leeftijd_index geslacht Leeftijd_index*geslacht BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS BN_INKOMEN_OMS 
				%IF &TimePoint. > 0 %THEN %DO;
					AV_lag TV_lag VER_lag HER_lag GBER_lag Diabetes_lag COPD_lag Reuma_lag /*Dialyse_lag*/ Depressie_lag FKG_lag DKG_lag HKG_lag Paying_lag
					%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
						%SCAN(&medications.,&m.)_lag 
					%END;
				%END;
			/ LINK=LOGIT;
	SELECTION METHOD=BACKWARD(CHOOSE=SBC STOP=SBC SELECT=SBC);
	ODS output ParameterEstimates=PE;	
RUN;

ODS EXCLUDE NONE;
PROC SQL NOPRINT;
	SELECT DISTINCT EFFECT
	INTO :EFFECTS SEPARATED BY " "
	FROM PE
	WHERE EFFECT NE 'Intercept'
;
QUIT;

PROC LOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint.)) PLOTS(ONLY)=NONE NOPRINT;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL Switch(event='1') = &Effects. / LINK=LOGIT;
	STORE OUT=Model.Switch_&TimePoint.;
RUN;
%END;

PROC DATASETS NOLIST LIB=WORK;
	DELETE Modeldata PE;
QUIT;

%MEND;
%Switch_model();