LIBNAME PHD '\\ka0089.loods2.org\Actuariaat\SAS Data\DennisS\PhD';
LIBNAME Model '\\ka0089.loods2.org\Actuariaat\SAS Data\DennisS\PhD\Modellen A4';

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


%macro Comorbiditie_models();
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
	/*Dialyse_lag = lag(Dialyse);*/
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
		/*Dialyse_lag = .;*/
		Depressie_lag = .;
		%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
			%SCAN(&medications.,&m.)_lag = .;
		%END;
	END;

RUN;

%DO t = 1 %TO %SYSFUNC(COUNTW(&TimePoints.));
%LET TimePoint = %SCAN(&TimePoints,&t.);

/*Diabetes model*/
ODS EXCLUDE ALL;
PROC HPLOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. %IF &TimePoint. > 0 %THEN AND Diabetes_lag = 0;)) NAMELEN=32;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL Diabetes(event='1')=leeftijd_index geslacht leeftijd_index*leeftijd_index leeftijd_index*geslacht BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS BN_INKOMEN_OMS FKG DKG HKG AV_indicator TV_indicator ER_indicator HERVERZEKERING_ER ER_REGELING
				%IF &TimePoint. > 0 %THEN %DO;
					COPD_lag Reuma_lag /*Dialyse_lag*/ Depressie_lag
					%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
						%SCAN(&medications.,&m.)_lag 
					%END;
				%END; / LINK = LOGIT;
	SELECTION METHOD=BACKWARD(CHOOSE=SBC SELECT=SBC STOP=SBC);
	ODS OUTPUT ParameterEstimates = PE;
RUN;

ODS EXCLUDE NONE;
PROC SQL NOPRINT;
	SELECT DISTINCT effect
	INTO :Effects SEPARATED BY " "
	FROM PE
	WHERE effect NE 'Intercept'
;
QUIT;

PROC LOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. %IF &TimePoint. > 0 %THEN AND Diabetes_lag = 0;)) PLOTS(ONLY)=NONE NOPRINT;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL Diabetes(event='1')= &Effects. / LINK=LOGIT;
	STORE OUT=Model.Diabetes_&TimePoint.;
RUN;

/*COPD model*/
ODS EXCLUDE ALL;
PROC HPLOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. %IF &TimePoint. > 0 %THEN AND COPD_lag = 0;)) NAMELEN=32;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL COPD(event='1')=leeftijd_index geslacht leeftijd_index*leeftijd_index leeftijd_index*geslacht BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS BN_INKOMEN_OMS FKG DKG HKG AV_indicator TV_indicator ER_indicator HERVERZEKERING_ER ER_REGELING
				%IF &TimePoint. > 0 %THEN %DO;
					Diabetes_lag Reuma_lag /*Dialyse_lag*/ Depressie_lag
					%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
						%SCAN(&medications.,&m.)_lag 
					%END;
				%END; / LINK = LOGIT;
	SELECTION METHOD=BACKWARD(CHOOSE=SBC SELECT=SBC STOP=SBC);
	ODS OUTPUT ParameterEstimates = PE;
RUN;

ODS EXCLUDE NONE;
PROC SQL NOPRINT;
	SELECT DISTINCT effect
	INTO :Effects SEPARATED BY " "
	FROM PE
	WHERE effect NE 'Intercept'
;
QUIT;

PROC LOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. %IF &TimePoint. > 0 %THEN AND COPD_lag = 0;)) PLOTS(ONLY)=NONE NOPRINT;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL COPD(event='1')= &Effects. / LINK=LOGIT;
	STORE OUT=Model.COPD_&TimePoint.;
RUN;

/*Reuma model*/
ODS EXCLUDE ALL;
PROC HPLOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. %IF &TimePoint. > 0 %THEN AND Reuma_lag = 0;)) NAMELEN=32;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL Reuma(event='1')=leeftijd_index geslacht leeftijd_index*leeftijd_index leeftijd_index*geslacht BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS BN_INKOMEN_OMS FKG DKG HKG AV_indicator TV_indicator ER_indicator HERVERZEKERING_ER ER_REGELING
				%IF &TimePoint. > 0 %THEN %DO;
					COPD_lag Diabetes_lag /*Dialyse_lag*/ Depressie_lag
					%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
						%SCAN(&medications.,&m.)_lag 
					%END;
				%END; / LINK = LOGIT;
	SELECTION METHOD=BACKWARD(CHOOSE=SBC SELECT=SBC STOP=SBC);
	ODS OUTPUT ParameterEstimates = PE;
RUN;

ODS EXCLUDE NONE;
PROC SQL NOPRINT;
	SELECT DISTINCT effect
	INTO :Effects SEPARATED BY " "
	FROM PE
	WHERE effect NE 'Intercept'
;
QUIT;

PROC LOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. %IF &TimePoint. > 0 %THEN AND Reuma_lag = 0;)) PLOTS(ONLY)=NONE NOPRINT;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL Reuma(event='1')= &Effects. / LINK=LOGIT;
	STORE OUT=Model.Reuma_&TimePoint.;
RUN;

/*PROC LOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. %IF &TimePoint. > 0 %THEN AND Dialyse_lag = 0;)) PLOTS(ONLY)=NONE NOPRINT;*/
/*	CLASS geslacht (PARAM=EFFECT) BN_INKOMEN_OMS (PARAM=EFFECT) BN_SOCIALE_KLASSE_OMS (PARAM=EFFECT) BN_OPLEIDING_OMS (PARAM=EFFECT)*/
/*		%IF &TimePoint. > 0 %THEN GBER_lag (PARAM=EFFECT) ;*/
/*		;*/
/*	MODEL Dialyse(event='1')=leeftijd_index geslacht BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS BN_INKOMEN_OMS FKG DKG HKG AV_indicator TV_indicator ER_indicator HERVERZEKERING_ER ER_REGELING*/
/*				%IF &TimePoint. > 0 %THEN %DO;*/
/*					Diabetes_lag COPD_lag Reuma_lag Depressie_lag*/
/*					%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));*/
/*						%SCAN(&medications.,&m.)_lag */
/*					%END;*/
/*				%END;*/
/*			/ SELECTION=NONE*/
/*			LINK=LOGIT;*/
/*	STORE OUT=Model.Dialyse_&TimePoint.;*/
/*RUN;*/

ODS EXCLUDE ALL;
PROC HPLOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. %IF &TimePoint. > 0 %THEN AND Depressie_lag = 0;)) NAMELEN=32;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL Depressie(event='1')=leeftijd_index geslacht leeftijd_index*leeftijd_index leeftijd_index*geslacht BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS BN_INKOMEN_OMS FKG DKG HKG AV_indicator TV_indicator ER_indicator HERVERZEKERING_ER ER_REGELING
				%IF &TimePoint. > 0 %THEN %DO;
					COPD_lag Reuma_lag /*Dialyse_lag*/ Diabetes_lag
					%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
						%SCAN(&medications.,&m.)_lag 
					%END;
				%END; / LINK = LOGIT;
	SELECTION METHOD=BACKWARD(CHOOSE=SBC SELECT=SBC STOP=SBC);
	ODS OUTPUT ParameterEstimates = PE;
RUN;

ODS EXCLUDE NONE;
PROC SQL NOPRINT;
	SELECT DISTINCT effect
	INTO :Effects SEPARATED BY " "
	FROM PE
	WHERE effect NE 'Intercept'
;
QUIT;

PROC LOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. %IF &TimePoint. > 0 %THEN AND Depressie_lag = 0;)) PLOTS(ONLY)=NONE NOPRINT;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL Depressie(event='1')= &Effects. / LINK=LOGIT;
	STORE OUT=Model.Depressie_&TimePoint.;
RUN;

%END;

PROC DATASETS NOLIST LIB=WORK;
	DELETE Modeldata PE;
QUIT;
%MEND;
%Comorbiditie_models();