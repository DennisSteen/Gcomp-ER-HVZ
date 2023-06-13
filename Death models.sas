LIBNAME Model '\\ka0089.loods2.org\Actuariaat\SAS Data\DennisS\PhD\Modellen A4';

PROC SQL NOPRINT;
	SELECT MAX(dag90)
	INTO :maxTimePoint
	FROM Phd.COMPLETESET_90DAGEN
;
	SELECT DISTINCT dag90
	INTO :TimePoints SEPARATED BY " "
	FROM PhD.COMPLETESET_90DAGEN
	WHERE dag90 > 365 /*Patients have to be followd for at least a year*/
		AND dag90 < &maxTimePoint. /*At the end of the database all patients will be censored*/
;
QUIT;



%macro modellen_death();

%DO i = 1 %TO %SYSFUNC(COUNTW(&TimePoints.));
%LET TimePoint = %SCAN(&TimePoints,&i.);

PROC SQL;
	CREATE TABLE ModelData AS
	SELECT leeftijd_index
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
			,Death
			,user_C03AA, user_C03DA, user_C07A, user_C08, user_C09A, user_C09C, user_C10AA, user_C10AB, user_C10AC, user_C10AD, user_C10AX
	FROM phd.COMPLETESET_90DAGEN
	WHERE dag90 = &TimePoint.
			AND CVE = 0 /*Patients should not have a CVE*/
;
QUIT;

PROC LOGISTIC DATA=WORK.ModelData
		PLOTS(ONLY)=NONE;
	;
	CLASS geslacht 	(PARAM=EFFECT) ER_REGELING 	(PARAM=EFFECT) INCASSO_FREQ_OMS 	(PARAM=EFFECT) BN_INKOMEN_OMS 	(PARAM=EFFECT) BN_SOCIALE_KLASSE_OMS 	(PARAM=EFFECT) BN_OPLEIDING_OMS 	(PARAM=EFFECT);
	MODEL Death(event='1')= user_C03AA user_C03DA user_C07A user_C08 user_C09A user_C09C user_C10AA user_C10AB user_C10AC user_C10AD user_C10AX 
							leeftijd_index Diabetes COPD Reuma Dialyse Depressie CVE HOOFDVERZEKERDE_JN AV_INDICATOR TV_INDICATOR ER_INDICATOR HERVERZEKERING_ER ER_verpl_cum FKG DKG HKG geslacht ER_REGELING INCASSO_FREQ_OMS BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS		/
		SELECTION=NONE
		LINK=LOGIT
	;
	STORE OUT=Model.Death_&TimePoint.;
RUN;
%END;
%mend;

%modellen_death();