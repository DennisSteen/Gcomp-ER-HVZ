LIBNAME PHD '\\ka0089.loods2.org\Actuariaat\SAS Data\DennisS\PhD';
LIBNAME Model '\\ka0089.loods2.org\Actuariaat\SAS Data\DennisS\PhD\Modellen A4';

PROC SQL;
	CREATE TABLE C_90dagen AS
	SELECT ZCL_REL_NR
			,ROUND(dag+44,90) as dag90
			,MAX(datum) FORMAT ddmmyy10. as datum
			,leeftijd_index
			,geslacht
			,SUM(bedrag) as bedrag
			,MAX(Diabetes) as Diabetes
			,MAX(COPD) as COPD
			,MAX(Reuma) as Reuma
			,MAX(Dialyse) as Dialyse
			,MAX(Depressie) as Depressie
			,MAX(CVE) as CVE
			,MAX(Death) as Death
			,SUM(ER_verpl_herv) as er_verpl_herv
			,SUM(ER_vrijw) as ER_Vrijw
			,SUM(ER_verpl) as ER_verpl
			,SUM(aantal_C03AA) as aantal_C03AA
			,SUM(aantal_C03DA) as aantal_C03DA
			,SUM(aantal_C07A)  as aantal_C07A
			,SUM(aantal_C08)   as aantal_C08
			,SUM(aantal_C09A)  as aantal_C09A
			,SUM(aantal_C09C)  as aantal_C09C
			,SUM(aantal_C10AA) as aantal_C10AA
			,SUM(aantal_C10AB) as aantal_C10AB
			,SUM(aantal_C10AC) as aantal_C10AC
			,SUM(aantal_C10AD) as aantal_C10AD
			,SUM(aantal_C10AX) as aantal_C10AX
			,MAX(CASE WHEN aantal_C03AA > 0 AND ((ER_verpl > 0 AND ER_verpl_herv = 0) OR ER_vrijw > 0) THEN 1 ELSE 0 END) as paying_C03AA
			,MAX(CASE WHEN aantal_C03DA > 0 AND ((ER_verpl > 0 AND ER_verpl_herv = 0) OR ER_vrijw > 0) THEN 1 ELSE 0 END) as paying_C03DA
			,MAX(CASE WHEN aantal_C07A  > 0 AND ((ER_verpl > 0 AND ER_verpl_herv = 0) OR ER_vrijw > 0) THEN 1 ELSE 0 END) as paying_C07A
			,MAX(CASE WHEN aantal_C08   > 0 AND ((ER_verpl > 0 AND ER_verpl_herv = 0) OR ER_vrijw > 0) THEN 1 ELSE 0 END) as paying_C08
			,MAX(CASE WHEN aantal_C09A  > 0 AND ((ER_verpl > 0 AND ER_verpl_herv = 0) OR ER_vrijw > 0) THEN 1 ELSE 0 END) as paying_C09A
			,MAX(CASE WHEN aantal_C09C  > 0 AND ((ER_verpl > 0 AND ER_verpl_herv = 0) OR ER_vrijw > 0) THEN 1 ELSE 0 END) as paying_C09C
			,MAX(CASE WHEN aantal_C10AA > 0 AND ((ER_verpl > 0 AND ER_verpl_herv = 0) OR ER_vrijw > 0) THEN 1 ELSE 0 END) as paying_C10AA
			,MAX(MAX(CASE WHEN aantal_C10AB > 0 AND ((ER_verpl > 0 AND ER_verpl_herv = 0) OR ER_vrijw > 0) THEN 1 ELSE 0 END
						,CASE WHEN aantal_C10AC > 0 AND ((ER_verpl > 0 AND ER_verpl_herv = 0) OR ER_vrijw > 0) THEN 1 ELSE 0 END 
						,CASE WHEN aantal_C10AD > 0 AND ((ER_verpl > 0 AND ER_verpl_herv = 0) OR ER_vrijw > 0) THEN 1 ELSE 0 END 
						,CASE WHEN aantal_C10AX > 0 AND ((ER_verpl > 0 AND ER_verpl_herv = 0) OR ER_vrijw > 0) THEN 1 ELSE 0 END)) as paying_C10AZ
	FROM PhD.CompleteSet
	GROUP BY ZCL_REL_NR
			,CALCULATED dag90
			,leeftijd_index
			,geslacht
;
QUIT;

PROC SQL;
	CREATE TABLE PhD.CompleteSet_90Dagen AS
	SELECT t1.*
		,CASE WHEN dag = 0 AND t1.aantal_C03AA > 0 THEN 1 ELSE adherence_C03AA END as adherence_C03AA
		,CASE WHEN dag = 0 AND t1.aantal_C03DA > 0 THEN 1 ELSE adherence_C03DA END as adherence_C03DA
		,CASE WHEN dag = 0 AND t1.aantal_C07A  > 0 THEN 1 ELSE adherence_C07A  END as adherence_C07A
		,CASE WHEN dag = 0 AND t1.aantal_C08   > 0 THEN 1 ELSE adherence_C08   END as adherence_C08
		,CASE WHEN dag = 0 AND t1.aantal_C09A  > 0 THEN 1 ELSE adherence_C09A  END as adherence_C09A
		,CASE WHEN dag = 0 AND t1.aantal_C09C  > 0 THEN 1 ELSE adherence_C09C  END as adherence_C09C
		,CASE WHEN dag = 0 AND t1.aantal_C10AA > 0 THEN 1 ELSE adherence_C10AA END as adherence_C10AA
		,MAX(CASE WHEN dag = 0 AND t1.aantal_C10AB > 0 THEN 1 ELSE adherence_C10AB END 
				,CASE WHEN dag = 0 AND t1.aantal_C10AC > 0 THEN 1 ELSE adherence_C10AC END
				,CASE WHEN dag = 0 AND t1.aantal_C10AD > 0 THEN 1 ELSE adherence_C10AD END
				,CASE WHEN dag = 0 AND t1.aantal_C10AX > 0 THEN 1 ELSE adherence_C10AX END) as adherence_C10AZ
		,CASE WHEN t1.aantal_C03AA > 0 THEN 1 ELSE 0 END as PickUp_C03AA
		,CASE WHEN t1.aantal_C03DA > 0 THEN 1 ELSE 0 END as PickUp_C03DA
		,CASE WHEN t1.aantal_C07A  > 0 THEN 1 ELSE 0 END as PickUp_C07A
		,CASE WHEN t1.aantal_C08   > 0 THEN 1 ELSE 0 END as PickUp_C08
		,CASE WHEN t1.aantal_C09A  > 0 THEN 1 ELSE 0 END as PickUp_C09A
		,CASE WHEN t1.aantal_C09C  > 0 THEN 1 ELSE 0 END as PickUp_C09C
		,CASE WHEN t1.aantal_C10AA > 0 THEN 1 ELSE 0 END as PickUp_C10AA
		,MAX(CASE WHEN t1.aantal_C10AB > 0 THEN 1 ELSE 0 END 
				,CASE WHEN t1.aantal_C10AC > 0 THEN 1 ELSE 0 END
				,CASE WHEN t1.aantal_C10AD > 0 THEN 1 ELSE 0 END
				,CASE WHEN t1.aantal_C10AX > 0 THEN 1 ELSE 0 END) as PickUp_C10AZ
		,jaar
		,HOOFDVERZEKERDE_JN
		,AV_INDICATOR
		,TV_INDICATOR
		,ER_INDICATOR
		,HERVERZEKERING_ER
		,CASE WHEN ER_REGELING = 'J' THEN 1 ELSE 0 END as ER_REGELING
		,ER_TREDE
		,ER_BEDRAG
		,COALESCE(BN_INKOMEN_OMS,'Onbekend') AS BN_INKOMEN_OMS
		,COALESCE(BN_SOCIALE_KLASSE_OMS,'Onbekend') AS BN_SOCIALE_KLASSE_OMS
		,COALESCE(BN_OPLEIDING_OMS,'Onbekend') AS BN_OPLEIDING_OMS
		,ER_verpl_cum
		,ER_vrijw_cum
		,ER_hervz_cum
		,FKG
		,DKG
		,HKG
	FROM C_90dagen t1
	LEFT JOIN PhD.CompleteSet t2 on (t1.zcl_rel_nr = t2.zcl_rel_nr AND t1.datum = t2.datum)
	ORDER BY t1.zcl_rel_nr
			,t1.dag90
;
QUIT;

DATA PhD.COMPLETESET_90DAGEN;
	SET PhD.COMPLETESET_90DAGEN;
	BY zcl_rel_nr;
	Censor = 0;
	Switch = 0;
	IF last.zcl_rel_nr THEN DO;	
		IF CVE = 0 AND Death = 0 AND MONTH(datum) = 12 AND DAY(datum) = 31 AND YEAR(datum) < 2021 THEN Switch = 1; 
		ELSE IF CVE = 0 AND Death = 0 THEN Censor = 1;
	END;

	RETAIN user_C03AA user_C03DA user_C07A user_C08 user_C09A user_C09C user_C10AA user_C10AZ;
	IF first.zcl_rel_nr THEN DO;
		user_C03AA = 0;
		user_C03DA = 0;
		user_C07A = 0;
		user_C08 = 0;
		user_C09A = 0;
		user_C09C = 0;
		user_C10AA = 0;
		user_C10AZ = 0;
	END;

	IF aantal_C03AA > 0 THEN user_C03AA = 1;
	IF aantal_C03DA > 0 THEN user_C03DA = 1;
	IF aantal_C07A  > 0 THEN user_C07A  = 1;
	IF aantal_C08   > 0 THEN user_C08   = 1;
	IF aantal_C09A  > 0 THEN user_C09A  = 1;
	IF aantal_C09C  > 0 THEN user_C09C  = 1;
	IF aantal_C10AA > 0 THEN user_C10AA = 1;
	IF aantal_C10AB > 0 OR aantal_C10AC > 0 OR aantal_C10AD > 0 OR aantal_C10AX > 0 THEN user_C10AZ = 1;

	ARRAY nums _NUMERIC_;
	DO OVER nums;
		IF nums =. THEN nums = 0;
	END;

	user_lag_C03AA = lag(user_C03AA);
	user_lag_C03DA = lag(user_C03DA);
	user_lag_C07A  = lag(user_C07A);
	user_lag_C08   = lag(user_C08);
	user_lag_C09A  = lag(user_C09A);
	user_lag_C09C  = lag(user_C09C);
	user_lag_C10AA = lag(user_C10AA);
	user_lag_C10AZ = lag(user_C10AZ);

	IF first.zcl_rel_nr THEN DO;
		user_lag_c03AA = 0;
		user_lag_c03DA = 0;
		user_lag_c07A  = 0;
		user_lag_c08   = 0;
		user_lag_c09A  = 0;
		user_lag_c09C  = 0;
		user_lag_c10AA = 0;
		user_lag_c10AZ = 0;
	END;

	IF user_lag_C03AA = 0 AND user_C03AA THEN adherence_C03AA = 1;
	IF user_lag_C03DA = 0 AND user_C03DA THEN adherence_C03DA = 1;
	IF user_lag_C07A  = 0 AND user_C07A  THEN adherence_C07A  = 1;
	IF user_lag_C08   = 0 AND user_C08   THEN adherence_C08   = 1;
	IF user_lag_C09A  = 0 AND user_C09A  THEN adherence_C09A  = 1;
	IF user_lag_C09C  = 0 AND user_C09C  THEN adherence_C09C  = 1;
	IF user_lag_C10AA = 0 AND user_C10AA THEN adherence_C10AA = 1;
	IF user_lag_C10AZ = 0 AND user_C10AZ THEN adherence_C10AZ = 1;

	IF jaar = 2013 AND ER_VERPL_CUM + ER_VRIJW_CUM =< 349.99 + ER_BEDRAG THEN Paying = 1;
	ELSE IF jaar = 2014 AND ER_VERPL_CUM + ER_VRIJW_CUM =< 359.99 + ER_BEDRAG THEN Paying = 1;
	ELSE IF jaar = 2015 AND ER_VERPL_CUM + ER_VRIJW_CUM =< 374.99 + ER_BEDRAG THEN Paying = 1;
	ELSE IF ER_VERPL_CUM + ER_VRIJW_CUM =< 384.99 + ER_BEDRAG THEN Paying = 1;
	ELSE Paying = 0; /*1 If patient did not reach deductible limit, 0 otherwise*/

	DROP user_lag_:;
RUN;

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