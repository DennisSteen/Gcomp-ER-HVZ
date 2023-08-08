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


%macro Paying_model();
DATA Modeldata;
	SET PhD.COMPLETESET_90DAGEN;
	
	BY zcl_rel_nr;

	Paying_lag = lag(Paying);
	IF first.zcl_rel_nr THEN Paying_lag = 1;
	
	YearPassed = MIN(INTCK('day',MDY(1,1,YEAR(datum)),datum) / 365,1);

	IF INTCK('day',MDY(1,1,YEAR(datum)),datum) < 90 OR dag90 = 0 THEN Paying_lag = 1;
RUN;

%DO t = 1 %TO %SYSFUNC(COUNTW(&TimePoints.));
	%LET TimePoint = %SCAN(&TimePoints,&t.);

	ODS EXCLUDE ALL;
	PROC HPLOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. AND Paying_lag = 1)) NAMELEN=32;
		CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
		MODEL Paying(event='1') = leeftijd_index*YearPassed geslacht*YearPassed BN_SOCIALE_KLASSE_OMS*YearPassed BN_OPLEIDING_OMS*YearPassed BN_INKOMEN_OMS*YearPassed FKG*YearPassed DKG*YearPassed HKG*YearPassed 
					AV_indicator*YearPassed TV_indicator*YearPassed ER_indicator*YearPassed HERVERZEKERING_ER*YearPassed ER_REGELING*YearPassed			
					%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
						user_%SCAN(&medications.,&m.)*YearPassed
					%END; 
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

	PROC LOGISTIC DATA=ModelData(WHERE=(dag90 = &TimePoint. AND Paying_lag = 1)) NOPRINT;
		CLASS geslacht BN_INKOMEN_OMS BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS;
		MODEL Paying(event='1')= &effects. / LINK=LOGIT;
		STORE OUT=Model.Paying_&TimePoint.;
	RUN;

%END;

PROC DATASETS NOLIST LIB=WORK;
	DELETE Modeldata;
QUIT;
%MEND;
%Paying_model();