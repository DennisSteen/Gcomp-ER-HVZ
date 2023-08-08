LIBNAME PHD '\\ka0089.loods2.org\Actuariaat\SAS Data\DennisS\PhD';
LIBNAME Model '\\ka0089.loods2.org\Actuariaat\SAS Data\DennisS\PhD\Modellen A4';

Proc Format;
  value adherencefmt
	low - 0 = 0
	0 <- 0.125 = 1
	0.125 -< 0.25 = 2
 	0.25 -< 0.375 = 3
	0.375 -< 0.5 = 4
	0.5 -< 0.625 = 5
	0.625 -< 0.75 = 6
	0.75 -< 0.875 = 7
	0.875 -< 1 = 8
	1 - high = 9;
RUN;

%LET medications = C03AA C03DA C07A C08 C09A C09C C10AA C10AZ ;

PROC SQL NOPRINT;
	SELECT MAX(dag90)
	INTO :maxTimePoint
	FROM Phd.COMPLETESET_90DAGEN
	WHERE dag90 < 3000 /*At the end of the database all patients will be censored*/
;
	SELECT DISTINCT dag90
	INTO :TimePoints SEPARATED BY " "
	FROM PhD.COMPLETESET_90DAGEN
	WHERE dag90 < 3000 /*At the end of the database all patients will be censored*/
;
QUIT;

DATA BasisSet;
	SET PhD.completeset_90dagen;
	WHERE dag90 = 0;
	KEEP ZCL_REL_NR leeftijd_index geslacht BN_SOCIALE_KLASSE_OMS BN_OPLEIDING_OMS BN_INKOMEN_OMS datum user_:;
RUN;

%macro predict(Simulate = FALSE, MedPaying = TRUE);
%DO t = 1 %TO 3;/*%SYSFUNC(COUNTW(&TimePoints.));*/
%LET TimePoint = %SCAN(&TimePoints.,&t.);

DATA VoorspelSet_&TimePoint.;
	%IF &TimePoint. = 0 %THEN %DO;
		SET BasisSet;
	%END; %ELSE %DO;
		SET VoorspelSet_%SCAN(&TimePoints.,%EVAL(&t.-1));
		%IF &Simulate. = TRUE %THEN WHERE Censor = 0 AND Death = 0 AND CVE = 0 AND Switch = 0; ;
		datum = INTNX('day',datum,90);

		AV_lag = AV_INDICATOR;
		TV_lag = TV_INDICATOR;
		VER_lag = ER_INDICATOR;
		HER_lag = HERVERZEKERING_ER;
		GBER_lag = ER_REGELING;
		Diabetes_lag = Diabetes;
		COPD_lag = COPD;
		Reuma_lag = Reuma;
	/*	Dialyse_lag = Dialyse;*/
		Depressie_lag = Depressie;
		FKG_lag = FKG;
		DKG_lag = DKG;
		HKG_lag = HKG;
		%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
			%SCAN(&medications.,&m.)_lag = user_%SCAN(&medications.,&m.);
			adherence_%SCAN(&medications.,&m.)_lag = adherence_%SCAN(&medications.,&m.);
			PickUp_%SCAN(&medications.,&m.)_lag = PickUp_%SCAN(&medications.,&m.);
			Paying_%SCAN(&medications.,&m.)_lag = Paying_%SCAN(&medications.,&m.);
		%END;
		Paying_lag = Paying;

		CVE_lag = CVE;
		Death_lag = Death;
		Censor_lag = Censor;
		Switch_lag = Switch;

		EoTCorrection = (1 - Censor);

		DROP AV_indicator TV_indicator ER_indicator Herverzekering_ER ER_regeling Diabetes COPD Reuma /*Dialyse*/ Depressie FKG DKG HKG
			%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
				user_%SCAN(&medications.,&m.)
				adherence_%SCAN(&medications.,&m.)
				PickUp_%SCAN(&medications.,&m.)
				Paying_%SCAN(&medications.,&m.)
			%END;
			Paying CVE Death Censor Switch;
	%END;

	IF datum < MDY(4,1,YEAR(Datum)) THEN Q=1;
	ELSE IF datum < MDY(7,1,YEAR(datum)) THEN Q=2;
	ELSE IF datum < MDY(10,1,YEAR(Datum)) then Q=3;
	ELSE Q=4;

	YearPassed = MIN(INTCK('day',MDY(1,1,YEAR(datum)),datum) / 365,1);
	dag90 = &TimePoint.;
	IF (0 LE INTCK('day',INTNX('day',datum,-90),MDY(12,31,YEAR(datum)-1))) AND (INTCK('day',INTNX('day',datum,-90),MDY(12,31,YEAR(datum)-1)) < 90) THEN EndOfYearIndicator = 1;
	ELSE EndOfYearIndicator =  0;
RUN;

	%IF &TimePoint. > 365 %THEN %DO;
		PROC PLM RESTORE=model.Switch_&TimePoint. NOPRINT;
			SCORE DATA=VoorspelSet_&TimePoint. OUT=Voorspelset_&TimePoint. PREDICTED=SWITCH / ILINK;
		RUN;

		DATA VoorspelSet_&TimePoint.;
			SET VoorspelSet_&TimePoint.;

			%IF &Simulate. = TRUE %THEN %DO;
				IF INTCK('day',MDY(1,1,YEAR(datum)),datum) < 90 THEN DO;
					Switch = RAND('BERNOULLI',Switch);
				END; ELSE Switch = 0;
			%END; %ELSE %DO;
				IF INTCK('day',MDY(1,1,YEAR(datum)),datum) GE 90 THEN Switch = 0;
				ELSE Switch = Switch;
			%END;
		RUN;

		DATA VoorspelSet_&TimePoint.(DROP = AV_indicator TV_indicator ER_indicator Herverzekering_ER ER_regeling Diabetes COPD Reuma /*Dialyse*/ Depressie FKG DKG HKG
								%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
									user_%SCAN(&medications.,&m.)
									adherence_%SCAN(&medications.,&m.)
									PickUp_%SCAN(&medications.,&m.)
									Paying_%SCAN(&medications.,&m.)
								%END;
								Paying CVE Death Censor)
			 Switch_&TimePoint(DROP = AV_lag TV_lag VER_lag HER_lag GBER_lag Diabetes_lag COPD_lag Reuma_lag /*Dialyse_lag*/ Depressie_lag FKG_lag DKG_lag HKG_lag
								%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
									%SCAN(&medications.,&m.)_lag
									adherence_%SCAN(&medications.,&m.)_lag
									PickUp_%SCAN(&medications.,&m.)_lag
									Paying_%SCAN(&medications.,&m.)_lag
								%END;
								Paying_lag CVE_lag Death_lag Censor_lag);
			SET VoorspelSet_&TimePoint.;

			IF Switch < 1 THEN OUTPUT VoorspelSet_&TimePoint.;
			ELSE DO;
				AV_INDICATOR = AV_lag;
				TV_INDICATOR = TV_lag;
				ER_INDICATOR = VER_lag;
				HERVERZEKERING_ER = HER_lag;
				ER_REGELING = GBER_lag;
				Diabetes = Diabetes_lag;
				COPD = COPD_lag;
				Reuma = Reuma_lag;
			/*	Dialyse = Dialyse_lag;*/
				Depressie = Depressie_lag;
				FKG = FKG_lag;
				DKG = DKG_lag;
				HKG = HKG_lag;
				%DO m = 1 %TO %SYSFUNC(COUNTW(&medications.));
					user_%SCAN(&medications.,&m.) = %SCAN(&medications.,&m.)_lag;
					adherence_%SCAN(&medications.,&m.) = adherence_%SCAN(&medications.,&m.)_lag;
					PickUp_%SCAN(&medications.,&m.) = PickUp_%SCAN(&medications.,&m.)_lag;
					Paying_%SCAN(&medications.,&m.) = Paying_%SCAN(&medications.,&m.)_lag;
				%END;
				Paying = Paying_lag;

				CVE = 0;
				Death = 0;
				Censor = 0;

				datum = MDY(12,31,YEAR(datum));

				OUTPUT Switch_&TimePoint.;	
			END;
		RUN;
	%END;

/*Cost Group models*/
	PROC PLM Restore=model.FKG_&Timepoint. NOPRINT;
		SCORE DATA=VoorspelSet_&TimePoint. out=VoorspelSet_&TimePoint. PREDICTED=FKG /ilink;
	RUN;

	PROC PLM Restore=model.DKG_&Timepoint. NOPRINT;
		SCORE DATA=VoorspelSet_&TimePoint. out=VoorspelSet_&TimePoint. PREDICTED=DKG /ilink;
	RUN;

	PROC PLM Restore=model.HKG_&Timepoint. NOPRINT;
		SCORE DATA=VoorspelSet_&TimePoint. out=VoorspelSet_&TimePoint. PREDICTED=HKG /ilink;
	RUN;

	DATA VoorspelSet_&TimePoint.;
		SET VoorspelSet_&TimePoint.;

		%IF &Simulate. = TRUE AND &TimePoint. > 0 %THEN %DO;
			IF INTCK('day',MDY(1,1,YEAR(datum)),datum) GE 90 THEN DO;
				FKG = FKG_lag;
				DKG = DKG_lag;
				HKG = HKG_lag;
			END;
			ELSE DO;
				FKG = RAND('BERNOULLI',FKG);
				DKG = RAND('BERNOULLI',DKG);
				HKG = RAND('BERNOULLI',HKG);
			END;
		%END; %ELSE %IF &Simulate. = TRUE %THEN %DO;
			FKG = RAND('BERNOULLI',FKG);
			DKG = RAND('BERNOULLI',DKG);
			HKG = RAND('BERNOULLI',HKG);
		%END; %ELSE %IF &TimePoint. > 0 %THEN %DO;
			IF INTCK('day',MDY(1,1,YEAR(datum)),datum) GE 90 THEN DO;
				FKG = FKG_lag;
				DKG = DKG_lag;
				HKG = HKG_lag;
			END;
			ELSE DO;
				FKG = FKG;
				DKG = DKG;
				HKG = HKG;
			END;
		%END;

		%IF &TimePoint. > 0 %THEN DROP FKG_lag DKG_lag HKG_lag; ;
	RUN;

/*Polis Models*/
	PROC PLM RESTORE=Model.AVind_&TimePoint. NOPRINT;
		SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=AV_Indicator / ILINK;
	RUN;

	PROC PLM RESTORE=Model.TVind_&TimePoint. NOPRINT;
		SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=TV_Indicator / ILINK;
	RUN;

	PROC PLM RESTORE=Model.VERind_&TimePoint. NOPRINT;
		SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=ER_Indicator / ILINK;
	RUN;

	PROC PLM RESTORE=Model.HERind_&TimePoint. NOPRINT;
		SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=HERVERZEKERING_ER / ILINK;
	RUN;

	PROC PLM RESTORE=Model.GBERind_&TimePoint. NOPRINT;
		SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=ER_REGELING / ILINK;
	RUN;

	DATA VoorspelSet_&TimePoint.;
		SET VoorspelSet_&TimePoint.;

		%IF &Simulate. = TRUE AND &TimePoint. > 0 %THEN %DO;
			IF INTCK('day',MDY(1,1,YEAR(datum)),datum) GE 90 THEN DO;
				AV_indicator = AV_lag;
				TV_indicator = TV_lag;
				ER_Indicator = VER_lag;
				Herverzekering_ER = HER_lag;
				ER_Regeling = GBER_lag;
			END;
			ELSE DO;
				AV_indicator = RAND('BERNOULLI',AV_indicator);
				TV_indicator = RAND('BERNOULLI',TV_indicator);
				ER_Indicator = RAND('BERNOULLI',ER_indicator);
				Herverzekering_ER = RAND('BERNOULLI',HERVERZEKERING_ER);
				ER_Regeling = RAND('BERNOULLI',ER_regeling);
			END;
		%END; %ELSE %IF &Simulate. = TRUE %THEN %DO;
			AV_indicator = RAND('BERNOULLI',AV_indicator);
			TV_indicator = RAND('BERNOULLI',TV_indicator);
			ER_Indicator = RAND('BERNOULLI',ER_indicator);
			Herverzekering_ER = RAND('BERNOULLI',HERVERZEKERING_ER);
			ER_Regeling = RAND('BERNOULLI',ER_regeling);
		%END; %ELSE %IF &TimePoint. > 0 %THEN %DO;
			IF INTCK('day',MDY(1,1,YEAR(datum)),datum) GE 90 THEN DO;
				AV_indicator = AV_lag;
				TV_indicator = TV_lag;
				ER_Indicator = VER_lag;
				Herverzekering_ER = HER_lag;
				ER_Regeling = GBER_lag;
			END;
			ELSE DO;
				AV_indicator = AV_indicator;
				TV_indicator = TV_indicator;
				ER_indicator = ER_indicator;
				Herverzekering_ER = Herverzekering_ER;
				ER_regeling = ER_regeling;
			END;
		%END;

		%IF &TimePoint. > 0 %THEN DROP AV_lag TV_lag VER_lag HER_lag GBER_lag; ;
	RUN;

/*Comorbiditie Models*/
	PROC PLM RESTORE=Model.Diabetes_&TimePoint. NOPRINT;
		SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=Diabetes / ILINK;
	RUN;
	
	PROC PLM RESTORE=Model.COPD_&TimePoint. NOPRINT;
		SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=COPD / ILINK;
	RUN;

	PROC PLM RESTORE=Model.Reuma_&TimePoint. NOPRINT;
		SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=Reuma / ILINK;
	RUN;

	PROC PLM RESTORE=Model.Depressie_&TimePoint. NOPRINT;
		SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=Depressie / ILINK;
	RUN;

	DATA VoorspelSet_&TimePoint.;
		SET VoorspelSet_&TimePoint.;

		%IF &Simulate. = TRUE AND &TimePoint. > 0 %THEN %DO;
			IF Diabetes_lag = 1 THEN Diabetes = 1;
			ELSE Diabetes = RAND('BERNOULLI',Diabetes);

			IF COPD_lag = 1 THEN COPD = 1;
			ELSE COPD = RAND('BERNOULLI',COPD);

			IF Reuma_lag = 1 THEN Reuma = 1;
			ELSE Reuma = RAND('BERNOULLI',Reuma);

			IF Depressie_lag = 1 THEN Depressie = 1;
			ELSE Depressie = RAND('BERNOULLI',Depressie);
		%END; %ELSE %IF &Simulate. = TRUE %THEN %DO;
			Diabetes = RAND('BERNOULLI',Diabetes);
			COPD = RAND('BERNOULLI',COPD);
			Reuma = RAND('BERNOULLI',Reuma);
			Depressie = RAND('BERNOULLI',Depressie);
		%END; %ELSE %IF &TimePoint. > 0 %THEN %DO;
			Diabetes = (1 - Diabetes_lag) * Diabetes + Diabetes_lag;
			COPD = (1 - COPD_lag) * COPD + COPD_lag;
			Reuma = (1 - Reuma_lag) * Reuma + Reuma_lag;
			Depressie = (1 - Depressie_lag) * Depressie + Depressie_lag;
		%END;

		%IF &TimePoint. > 0 %THEN DROP Diabetes_lag COPD_lag Reuma_lag Depressie_lag; ;
	RUN;

/*Medication user models*/
	%IF &TimePoint. > 0 %THEN %DO;
		%DO m=1 %TO %SYSFUNC(COUNTW(&medications.));
			%LET medication = %SCAN(&medications.,&m.);

			PROC PLM RESTORE=Model.user_&medication._&TimePoint. NOPRINT;
				SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=user_&medication. / ILINK;
			RUN;
		%END;

		DATA VoorspelSet_&TimePoint.;
			SET VoorspelSet_&TimePoint.;

			%IF &Simulate. = TRUE %THEN %DO;
				%DO m=1 %TO %SYSFUNC(COUNTW(&medications.));
					%LET medication = %SCAN(&medications.,&m.);
					IF &medication._lag = 1 THEN user_&medication. = 1;
					ELSE user_&medication. = RAND('BERNOULLI',user_&medication.);
				%END;
			%END; %ELSE %DO;
				%DO m=1 %TO %SYSFUNC(COUNTW(&medications.));
					%LET medication = %SCAN(&medications.,&m.);
					user_&medication. = (1 - &medication._lag) * user_&medication. + &medication._lag;
				%END;
			%END;
		RUN;
	%END;

/*Medication pick-up models*/
	%IF &TimePoint. > 0 %THEN %DO;
		%DO m=1 %TO %SYSFUNC(COUNTW(&medications.));
			%LET medication = %SCAN(&medications.,&m.);

			PROC PLM RESTORE=Model.pickup_&medication._&TimePoint. NOPRINT;
				SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=PickUp_&medication. / ILINK;
			RUN;
		%END;

		DATA VoorspelSet_&TimePoint.;
			SET VoorspelSet_&TimePoint.;

			%IF &Simulate. = TRUE %THEN %DO;
				%DO m=1 %TO %SYSFUNC(COUNTW(&medications.));
					%LET medication = %SCAN(&medications.,&m.);
					IF &medication._lag = 0 THEN PickUp_&medication. = user_&medication.;
					ELSE PickUp_&medication. = &medication._lag * RAND('BERNOULLI',PickUp_&medication.) ;
				%END;
			%END; %ELSE %DO;
				%DO m=1 %TO %SYSFUNC(COUNTW(&medications.));
					%LET medication = %SCAN(&medications.,&m.);
					IF &medication._lag = 0 THEN PickUp_&medication. = user_&medication.;
					ELSE PickUp_&medication. = &medication._lag * PickUp_&medication.;
				%END;
			%END;
		RUN;
	%END;
	%ELSE %DO;
		DATA VoorspelSet_&TimePoint.;
			SET VoorspelSet_&TimePoint.;

			%DO m=1 %TO %SYSFUNC(COUNTW(&medications.));
				%LET medication = %SCAN(&medications.,&m.);
				PickUp_&medication. = user_&medication.;
			%END;
		RUN;
	%END;

/*Medication paying models*/
	%IF &MedPaying. = TRUE %THEN %DO;
		%DO m=1 %TO %SYSFUNC(COUNTW(&medications.));
			%LET medication = %SCAN(&medications.,&m.);

			PROC PLM RESTORE=Model.Paying_&medication._&TimePoint. NOPRINT;
				SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=Paying_&medication. / ILINK;
			RUN;
		%END;

		DATA VoorspelSet_&TimePoint.;
			SET VoorspelSet_&TimePoint.;

			%IF &Simulate. = TRUE %THEN %DO;
				%DO m=1 %TO %SYSFUNC(COUNTW(&medications.));
					%LET medication = %SCAN(&medications.,&m.);
					%IF &TimePoint. > 0 %THEN %DO;
						IF INTCK('day',MDY(1,1,YEAR(datum)),datum) > 90 THEN Paying_&medication. = Paying_lag * PickUp_&medication. * RAND('BERNOULLI',COALESCE(Paying_&medication.,0));
						ELSE Paying_&medication. = PickUp_&medication. * RAND('BERNOULLI',COALESCE(Paying_&medication.,0));
					%END; %ELSE %DO;
						Paying_&medication. = PickUp_&medication. * RAND('BERNOULLI',COALESCE(Paying_&medication.,0));
					%END;
				%END;
			%END; %ELSE %DO;
				%DO m=1 %TO %SYSFUNC(COUNTW(&medications.));
					%LET medication = %SCAN(&medications.,&m.);
					IF INTCK('day',MDY(1,1,YEAR(datum)),datum) > 90 THEN Paying_&medication. = Paying_lag * PickUp_&medication. * Paying_&medication.;
					ELSE Paying_&medication. = PickUp_&medication. * Paying_&medication.;
				%END;
			%END;
		RUN;
	%END; %ELSE %DO;
		DATA VoorspelSet_&TimePoint.;
			SET VoorspelSet_&TimePoint.;
			%DO m=1 %TO %SYSFUNC(COUNTW(&medications.));
				%LET medication = %SCAN(&medications.,&m.);
				Paying_&medication. = 0;
			%END;
		RUN;
	%END;

/*Paying model*/
	PROC PLM RESTORE=Model.Paying_&TimePoint. NOPRINT;
		SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=Paying / ILINK;
	RUN;
	
	DATA VoorspelSet_&TimePoint.;
		SET VoorspelSet_&TimePoint.;
		
		%IF &TimePoint. > 0 AND &Simulate. = FALSE %THEN %DO;
			IF INTCK('day',MDY(1,1,YEAR(datum)),datum) > 90 THEN paying = (paying_lag + (1 - paying_lag) * Paying);
		%END; %ELSE %IF &TimePoint. > 0 AND &Simulate. = TRUE %THEN %DO;
			IF INTCK('day',MDY(1,1,YEAR(datum)),datum) > 90 THEN Paying = MIN(paying_lag,RAND('BERNOULLI',Paying));
			ELSE Paying = RAND('BERNOULLI',Paying);
		%END; %ELSE %IF &TimePoint. = 0 AND &Simulate. = TRUE %THEN %DO;
			Paying = RAND('BERNOULLI',Paying);
		%END; %ELSE %DO;
			Paying = Paying;
		%END;
	RUN;


/*Adherence models*/
	%IF &TimePoint. > 0 %THEN %DO;
		%DO m=1 %TO %SYSFUNC(COUNTW(&medications.));
			%LET medication = %SCAN(&medications.,&m.);

			DATA VoorspelSet_&TimePoint._&medication.;
				SET VoorspelSet_&Timepoint.;

				prev_adherence_class = PUT(adherence_&medication._lag,adherencefmt.);
			RUN;

			PROC PLM RESTORE=Model.adherence_&medication._&TimePoint. NOPRINT;
				SCORE DATA=VoorspelSet_&TimePoint._&medication. OUT=PredSetAdherence_&TimePoint._&medication. / ILINK;
			RUN;

			PROC TRANSPOSE DATA=PredSetAdherence_&TimePoint._&medication. OUT=PredSetAdherence_&TimePoint._&medication.(DROP=_NAME_ _LABEL_) PREFIX=GE_;
				BY ZCL_REL_NR user_&medication.;
				VAR PREDICTED;
				ID _LEVEL_;
			RUN;
			
			DATA PredSetAdherence_&TimePoint._&medication.;
				SET PredSetAdherence_&TimePoint._&medication.;

				%IF &Simulate. = TRUE %THEN %DO;
					WHERE user_&medication. = 1;
					adherence_&medication._class = RAND('TABLE',1-GE_1
														%DO C=1 %TO 8;
															,MAX(COALESCE(GE_&C.,0) - COALESCE(GE_%EVAL(&C.+1),0),0)
														%END; ,GE_9) - 1;
					adherence_&medication. = MAX(MIN(-0.0625 + 0.125 * adherence_&medication._class,1),0);

					DROP adherence_&medication._class;
				%END; %ELSE %DO;
					adherence_&medication. = (1 * GE_9 
					%DO C=8 %TO 1 %BY -1;
						+ (GE_&C. - GE_%EVAL(&C.+1)) * (0.0625 + 0.125 * %EVAL(&C.-1))
					%END; ) * user_&medication.;
				%END;

				DROP GE_:;
			RUN;
			

			DATA VoorspelSet_&Timepoint.;
				MERGE VoorspelSet_&Timepoint.
				PredSetAdherence_&TimePoint._&medication.;

				BY ZCL_REL_NR;

				ARRAY nums adherence_:;
				DO OVER nums;
					IF nums = . THEN nums = 0;
				END;

				IF &medication._lag = 0 AND user_&medication. = 1 THEN adherence_&medication. = 1;
				
				DROP adherence_&medication._lag &medication._lag paying_&medication._lag pickup_&medication._lag;

			RUN;
		%END;

		PROC DATASETS NOLIST LIB=WORK;
			DELETE PredSetAdherence_&TimePoint._:
					VoorspelSet_&TimePoint._: ;
		QUIT;
	%END;
	%ELSE %DO;
		DATA VoorspelSet_&Timepoint.;
			SET VoorspelSet_&Timepoint.;

			%DO m=1 %TO %SYSFUNC(COUNTW(&medications.));
				%LET medication = %SCAN(&medications.,&m.);
				IF user_&medication. = 1 THEN adherence_&medication. = 1;
				ELSE adherence_&medication. = 0;
			%END;
		RUN;
	%END;

	%IF &TimePoint. > 100 %THEN %DO;
/*CVE Model*/
		PROC PLM RESTORE=Model.CVE_&TimePoint. NOPRINT;
			SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=CVE / ILINK;
		RUN;
	%END;

	%IF &TimePoint. > 365 %THEN %DO;
/*Death Model*/
		PROC PLM RESTORE=Model.Death_&TimePoint. NOPRINT;
			SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=Death / ILINK;
		RUN;

/*Censor Model*/
		%IF &TimePoint. < &maxTimePoint. %THEN %DO;
			PROC PLM RESTORE=Model.Censor_&TimePoint. NOPRINT;
				SCORE DATA=VoorspelSet_&TimePoint. OUT=VoorspelSet_&TimePoint. PREDICTED=Censor / ILINK;
			RUN;
		%END;

		DATA VoorspelSet_&TimePoint.;
			SET VoorspelSet_&TimePoint.;

			%IF &Simulate. = TRUE %THEN %DO;
				CVE = RAND('BERNOULLI',CVE);
				Death = RAND('BERNOULLI',Death);
				%IF &TimePoint. = &maxTimePoint. %THEN %DO;
					Censor = 1;
				%END; %ELSE %DO ;
					Censor = RAND('BERNOULLI',Censor); 
				%END;
			%END;
			%ELSE %DO;
				CVE = (1 - CVE_lag) * CVE;
				Death = (1 - CVE) * (1 - Death_lag) * Death;
				Censor = (1 - Death) * (1 - Censor_lag) * Censor;
			%END;

			DROP paying_lag CVE_lag Death_lag Censor_lag;
		RUN;
		
	%END; %ELSE %IF &TimePoint. > 100 %THEN %DO;
		DATA VoorspelSet_&TimePoint.;
			SET VoorspelSet_&TimePoint.;

			%IF &Simulate. = TRUE %THEN %DO;
				CVE = RAND('BERNOULLI',CVE);
			%END;
			%ELSE %DO;
				CVE = (1 - CVE_lag) * CVE;
			%END;

			Death = 0;
			Censor = 0;

			DROP paying_lag CVE_lag Death_lag Censor_lag;
		RUN;
	%END; %ELSE %DO;
		DATA VoorspelSet_&TimePoint.;
			SET VoorspelSet_&TimePoint.;
			
			CVE = 0;
			Death = 0;
			Censor = 0;
			
			%IF &TimePoint. > 0 %THEN DROP paying_lag CVE_lag Death_lag Censor_lag; ;
		RUN;
	%END;

	DATA VoorspelSet_&TimePoint.;
		SET %IF &TimePoint. > 365 %THEN Switch_&TimePoint ;
			VoorspelSet_&TimePoint.;

		%IF &TimePoint. LE 365 %THEN Switch = 0; ;
	RUN;
	
	PROC SORT DATA=VoorspelSet_&TimePoint.;
		BY ZCL_REL_NR;
	RUN;
%END;

DATA Voorspelling;
	SET VoorspelSet_:;
RUN;

PROC DATASETS NOLIST LIB=WORK;
	DELETE VoorspelSet_: 
			Switch_:;
QUIT;

PROC SORT DATA=Voorspelling;
	BY ZCL_REL_NR dag90;
RUN;
%mend;

%predict(Simulate=FALSE, MedPaying=TRUE);