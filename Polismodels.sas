PROC SQL NOPRINT;
	SELECT MAX(dag90)
	INTO :maxTimePoint
	FROM Phd.COMPLETESET_90DAGEN
;
	SELECT DISTINCT dag90
	INTO :TimePoints SEPARATED BY " "
	FROM PhD.COMPLETESET_90DAGEN
	WHERE dag90 < 3000
;
QUIT;


%macro AVTVER_models();
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
		%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
			%SCAN(&medications.,&m.)_lag = .;
		%END;
	END;

	IF INTCK('day',MDY(1,1,YEAR(datum)),datum) < 90 OR dag90 = 0 THEN OUTPUT;
RUN;

%DO t = 1 %TO %SYSFUNC(COUNTW(&TimePoints.));
%LET TimePoint = %SCAN(&TimePoints,&t.);

/*AV model*/
ODS EXCLUDE ALL;
PROC HPLOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint.)) NAMELEN=32;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;

	MODEL AV_Indicator(event='1')=leeftijd_index geslacht leeftijd_index*leeftijd_index leeftijd_index*geslacht BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS BN_INKOMEN_OMS FKG DKG HKG
				%IF &TimePoint. > 0 %THEN %DO;
					AV_lag TV_lag VER_lag HER_lag GBER_lag Diabetes_lag COPD_lag Reuma_lag /*Dialyse_lag*/ Depressie_lag Paying_lag
					%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
						%SCAN(&medications.,&m.)_lag 
					%END;
				%END;
			/ LINK=LOGIT;
	SELECTION METHOD=BACKWARD(CHOOSE=SBC SELECT=SBC STOP=SBC);
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
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS
		;
	MODEL AV_Indicator(event='1') = &Effects. / LINK=LOGIT RIDGING=ABSOLUTE;
	STORE OUT=Model.AVind_&TimePoint.;
RUN;

/*TV Model*/
ODS EXCLUDE ALL;
PROC HPLOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint.)) NAMELEN=32;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL TV_Indicator(event='1')=leeftijd_index geslacht leeftijd_index*leeftijd_index leeftijd_index*geslacht BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS BN_INKOMEN_OMS FKG DKG HKG
				%IF &TimePoint. > 0 %THEN %DO;
					AV_lag TV_lag VER_lag HER_lag GBER_lag Diabetes_lag COPD_lag Reuma_lag /*Dialyse_lag*/ Depressie_lag Paying_lag
					%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
						%SCAN(&medications.,&m.)_lag 
					%END;
				%END;
			/ 
			LINK=LOGIT;
	SELECTION METHOD=FORWARD(CHOOSE=SBC SELECT=SBC STOP=SBC);
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
	MODEL TV_Indicator(event='1') = &Effects. / LINK=LOGIT;
	STORE OUT=Model.TVind_&TimePoint.;
RUN;

/*VER Model*/
ODS EXCLUDE ALL;
PROC HPLOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint.)) NAMELEN=32;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL ER_INDICATOR(event='1')=leeftijd_index geslacht leeftijd_index*leeftijd_index leeftijd_index*geslacht BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS BN_INKOMEN_OMS FKG DKG HKG
				%IF &TimePoint. > 0 %THEN %DO;
					AV_lag TV_lag VER_lag HER_lag GBER_lag Diabetes_lag COPD_lag Reuma_lag /*Dialyse_lag*/ Depressie_lag Paying_lag
					%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
						%SCAN(&medications.,&m.)_lag 
					%END;
				%END;
		/ 
			LINK=LOGIT;
	SELECTION METHOD=BACKWARD(CHOOSE=SBC SELECT=SBC STOP=SBC);
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
	MODEL ER_INDICATOR(event='1')= &effects. / LINK=LOGIT RIDGING=ABSOLUTE;
	STORE OUT=Model.VERind_&TimePoint.;
RUN;

/*HER model*/
ODS EXCLUDE ALL;
PROC HPLOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint.)) NAMELEN=32;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL HERVERZEKERING_ER(event='1')=leeftijd_index geslacht leeftijd_index*leeftijd_index leeftijd_index*geslacht BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS BN_INKOMEN_OMS FKG DKG HKG
				%IF &TimePoint. > 0 %THEN %DO;
					AV_lag TV_lag VER_lag HER_lag GBER_lag Diabetes_lag COPD_lag Reuma_lag /*Dialyse_lag*/ Depressie_lag Paying_lag
					%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
						%SCAN(&medications.,&m.)_lag 
					%END;
				%END;
			/ LINK=LOGIT;
	SELECTION METHOD=BACKWARD(CHOOSE=SBC SELECT=SBC STOP=SBC);
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
	MODEL HERVERZEKERING_ER(event='1')= &Effects. / LINK=LOGIT RIDGING=ABSOLUTE;
	STORE OUT=Model.HERind_&TimePoint.;
RUN;

/*GBER model*/
ODS EXCLUDE ALL;
PROC HPLOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint.)) NAMELEN=32;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL ER_REGELING(event='1')=leeftijd_index geslacht leeftijd_index*leeftijd_index leeftijd_index*geslacht BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS BN_INKOMEN_OMS FKG DKG HKG
				%IF &TimePoint. > 0 %THEN %DO;
					AV_lag TV_lag VER_lag HER_lag GBER_lag Diabetes_lag COPD_lag Reuma_lag /*Dialyse_lag*/ Depressie_lag Paying_lag
					%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
						%SCAN(&medications.,&m.)_lag 
					%END;
				%END;
			/ 
			LINK=LOGIT;
	SELECTION METHOD=BACKWARD(CHOOSE=SBC SELECT=SBC STOP=SBC);
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
	MODEL ER_REGELING(event='1')= &effects. / LINK=LOGIT RIDGING=ABSOLUTE;
	STORE OUT=Model.GBERind_&TimePoint.;
RUN;
%END;

PROC DATASETS NOLIST LIB=WORK;
	DELETE Modeldata PE;
QUIT;

%MEND;
%AVTVER_models();