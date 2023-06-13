LIBNAME Model '\\ka0089.loods2.org\Actuariaat\SAS Data\DennisS\PhD\Modellen A4';

PROC SQL NOPRINT;
	SELECT MAX(dag90)
	INTO :maxTimePoint
	FROM Phd.COMPLETESET_90DAGEN
;
	SELECT DISTINCT dag90
	INTO :TimePoints SEPARATED BY " "
	FROM PhD.COMPLETESET_90DAGEN
	WHERE dag90 > 365 /*Patients have to be followed for at least one year*/
		AND dag90 < &maxTimePoint. /*At the end everybody will be censored*/
;
QUIT;


%macro modellen_censor();

%DO i = 1 %TO %SYSFUNC(COUNTW(&TimePoints.));
%LET TimePoint = %SCAN(&TimePoints,&i.);

PROC SQL;
	CREATE TABLE ModelData AS
	SELECT CASE WHEN INTCK('day',datum,MDY(12,31,jaar)) < 90 THEN 1 ELSE 0 END as EndOfYearIndicator
			,leeftijd_index
			,geslacht
			,Diabetes
			,COPD
			,Reuma
			,Dialyse
			,Depressie
			,CVE
			,Hoofdverzekerde_JN
			,AV_indicator
			,TV_indicator
			,ER_indicator
			,Herverzekering_ER
			,ER_Regeling
			,INCASSO_FREQ_OMS
			,BN_INKOMEN_OMS
			,BN_Sociale_klasse_oms
			,bn_opleiding_oms
			,ER_verpl_cum
			,FKG,DKG,HKG
			,Censor
	FROM phd.COMPLETESET_90DAGEN
	WHERE dag90 = &TimePoint.
			AND CVE = 0 /*Patients should not have a CVE*/
			AND Death = 0 /*Patients should not be dead*/
;
QUIT;

PROC LOGISTIC DATA=WORK.ModelData
		PLOTS(ONLY)=NONE;
	;
	CLASS geslacht 	(PARAM=EFFECT) ER_REGELING 	(PARAM=EFFECT) INCASSO_FREQ_OMS 	(PARAM=EFFECT) BN_INKOMEN_OMS 	(PARAM=EFFECT) BN_SOCIALE_KLASSE_OMS 	(PARAM=EFFECT) BN_OPLEIDING_OMS 	(PARAM=EFFECT);
	MODEL Censor(event='1')= leeftijd_index Diabetes COPD Reuma Dialyse Depressie HOOFDVERZEKERDE_JN AV_INDICATOR TV_INDICATOR ER_INDICATOR HERVERZEKERING_ER ER_verpl_cum FKG DKG HKG geslacht ER_REGELING INCASSO_FREQ_OMS BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS		/
		SELECTION=NONE
		LINK=LOGIT
	;
	STORE OUT=Model.Censor_&TimePoint.;
RUN;
%END;
%mend;

%modellen_censor();