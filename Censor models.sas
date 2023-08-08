PROC SQL NOPRINT;
	SELECT MAX(dag90)
	INTO :maxTimePoint
	FROM Phd.COMPLETESET_90DAGEN
;
	SELECT DISTINCT dag90
	INTO :TimePoints SEPARATED BY " "
	FROM PhD.COMPLETESET_90DAGEN
	WHERE dag90 > 365 /*Patients have to be followed for at least one year*/
		AND dag90 < 3000 /*At the end everybody will be censored*/
;
QUIT;


%macro modellen_censor();

%DO i = 1 %TO %SYSFUNC(COUNTW(&TimePoints.));

DATA Modeldata;
	SET PhD.COMPLETESET_90DAGEN;
	BY zcl_rel_nr;

	YearPassed = MIN(INTCK('day',MDY(1,1,YEAR(datum)),datum) / 365,1);
	IF dag90 = 2970 THEN Censor = 1;
RUN;


%LET TimePoint = %SCAN(&TimePoints,&i.);
ODS EXCLUDE ALL;
PROC HPLOGISTIC DATA=WORK.ModelData(WHERE=(dag90 = &TimePoint. AND Switch = 0 AND CVE = 0 AND Death = 0)) NAMELEN=32;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL Censor(event='1')= leeftijd_index Diabetes COPD Reuma /*Dialyse*/ Depressie AV_INDICATOR TV_INDICATOR ER_INDICATOR HERVERZEKERING_ER Paying FKG DKG HKG geslacht ER_REGELING BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS
		/ LINK=LOGIT;
	SELECTION METHOD=BACKWARD(CHOOSE=SBC SELECT=SBC STOP=SBC);
	ODS OUTPUT ParameterEstimates=PE;
RUN;

ODS EXCLUDE NONE;
PROC SQL NOPRINT;
	SELECT DISTINCT Effect
	INTO :Effects SEPARATED BY " "
	FROM PE
	WHERE Effect NE 'Intercept'
;
QUIT;

PROC LOGISTIC DATA=WORK.ModelData(WHERE=(dag90 = &TimePoint. AND Switch = 0 AND CVE = 0 AND Death = 0)) NOPRINT PLOTS(ONLY)=NONE;
	;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL Censor(event='1')= &Effects. / LINK=LOGIT
	;
	STORE OUT=Model.Censor_&TimePoint.;
RUN;
%END;

PROC DATASETS NOLIST LIB=WORK;
	DELETE Modeldata;
QUIT;
%mend;

%modellen_censor();