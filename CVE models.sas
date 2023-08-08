LIBNAME Model '\\ka0089.loods2.org\Actuariaat\SAS Data\DennisS\PhD\Modellen A4';

PROC SQL NOPRINT;
/*	SELECT MAX(dag90)
	INTO :maxTimePoint
	FROM Phd.COMPLETESET_90DAGEN
;*/
	SELECT DISTINCT dag90
	INTO :TimePoints SEPARATED BY " "
	FROM PhD.COMPLETESET_90DAGEN
	WHERE dag90 > 150 /*Patients have to be followd for at least a year*/
		AND dag90 < 3000 /*At the end of the database all patients will be censored*/
;
QUIT;



%macro modellen_CVE();

%DO i = 1 %TO %SYSFUNC(COUNTW(&TimePoints.));
DATA Modeldata;
	SET PhD.COMPLETESET_90DAGEN;
	BY zcl_rel_nr;

	YearPassed = MIN(INTCK('day',MDY(1,1,YEAR(datum)),datum) / 365,1);
RUN;

%LET TimePoint = %SCAN(&TimePoints,&i.);
PROC HPLOGISTIC DATA=WORK.ModelData(WHERE=(dag90  = &TimePoint. AND Switch = 0)) NAMELEN=32;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL CVE(event='1')=  user_C03AA user_C03DA user_C07A user_C08 user_C09A user_C09C user_C10AA user_C10AZ
							user_C03AA*adherence_C03AA user_C03DA*adherence_C03DA user_C07A*adherence_C07A user_C08*adherence_C08 user_C09A*adherence_C09A user_C09C*adherence_C09C user_C10AA*adherence_C10AA user_C10AZ*adherence_C10AZ
							leeftijd_index Diabetes COPD Reuma Depressie AV_INDICATOR TV_INDICATOR ER_INDICATOR HERVERZEKERING_ER Paying FKG DKG HKG geslacht ER_REGELING BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS	
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

PROC LOGISTIC DATA=WORK.ModelData(WHERE=(dag90  = &TimePoint. AND Switch = 0)) NOPRINT PLOTS(ONLY)=NONE;
	CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
	MODEL CVE(event='1')=  &effects. / LINK=LOGIT;
	STORE OUT=Model.CVE_&TimePoint.;
RUN;
%END;

PROC DATASETS NOLIST LIB=WORK;
	DELETE Modeldata;
QUIT;
%mend;

%modellen_CVE();