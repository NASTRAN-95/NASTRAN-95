
SUBROUTINE prtprm
!
!     PRINTS NASTRAN PARAMETERS
!
!  $MIXED_FORMATS
!
   IMPLICIT NONE
   INTEGER B1 , B2 , Dumhed(96) , H1(32) , H2(32) , H3(32) , Ic , Junk1(6) , Junk2(2) , L , Ln , Lnmax , Machx , Nb , Nout , V(2)
   COMMON /blank / Ic , B1 , B2
   COMMON /machin/ Machx
   COMMON /output/ Dumhed , H1 , H2 , H3
   COMMON /system/ Nb , Nout , Junk1 , Lnmax , Junk2 , Ln
   COMMON /xvps  / V
   INTEGER bt(26) , i , k1(32) , k2(32) , k3(32) , k4(32) , k5(32) , k6(32) , kick , kt(26) , m , mi , mm , name(2) , nw , oo , r , &
         & type , val(2) , xxxx
   DOUBLE PRECISION dal(2)
   LOGICAL kickk
   INTEGER lshift , numtyp , rshift
   REAL wal(32)
   EXTERNAL lshift , rshift
   EQUIVALENCE (V(2),L) , (dal(1),val(1),wal(1))
   DATA xxxx/4HXXXX/ , oo/27/
   DATA kt/5 , 6 , 4 , 5 , 6 , 5 , 3 , 4 , 3 , 5 , 7 , 6 , 6 , 6 , 6 , 5 , 0 , 0 , 0 , 4 , 3 , 2 , 7 , 5 , 4 , 1/
   DATA k1/32*4H    /
   DATA k2/7*4H     , 4HC O  , 4HN T  , 4HE N  , 4HT S  , 4H  O  , 4HF    , 4HP A  , 4HR A  , 4HM E  , 4HT E  , 4HR    , 4HT A  ,   &
       &4HB L  , 4HE    , 11*4H    /
   DATA k3/32*4H    /
   DATA k4/32*4H    /
   DATA k5/32*4H    /
   DATA k6/32*4H    /
   DATA bt/4HSTAT , 4HINER , 4HMODE , 4HDIFF , 4HBUCK , 4HPLA  , 4HDIRC , 4HDIRF , 4HDIRT , 4HMDLC , 4HMDLF , 4HMDLT , 4HNMDS ,     &
       &4HCYCS , 4HCYCM , 4HASTA , 4HDDRM , 4HFVDA , 4HMVDA , 4HHSTA , 4HHNLI , 4HHTRD , 4HBLAD , 4HFLUT , 4HAERO , 4HDMAP/
!
!
   IF ( Ic/=0 ) THEN
!
      DO m = 1 , 32
         H1(m) = k4(m)
         H2(m) = k5(m)
         H3(m) = k6(m)
      ENDDO
      CALL page
      kick = iabs(Ic)
      DO m = 1 , 26
         IF ( B1==bt(m) ) THEN
            mm = m
            GOTO 1300
         ENDIF
      ENDDO
      Ln = Ln + 2
      WRITE (Nout,99001) B1 , B2
99001 FORMAT ('0SECOND PRTPARM PARAMETER VALUE -',2A4,'- IMPROPER.')
!
      CALL mesage(-61,0,0)
      GOTO 99999
   ELSE
      DO m = 1 , 32
         H1(m) = k1(m)
         H2(m) = k2(m)
         H3(m) = k3(m)
      ENDDO
      CALL page
      i = 3
      IF ( i>L ) THEN
!
         WRITE (Nout,99002)
99002    FORMAT (1H0,19X,'NO PARAMETERS EXIST')
         Ln = Ln + 2
         GOTO 99999
      ELSE
         kickk = .FALSE.
         IF ( B1/=xxxx .OR. B2/=xxxx ) kickk = .TRUE.
         IF ( kickk ) THEN
!
            DO WHILE ( i<=L )
               IF ( V(i)/=B1 .OR. V(i+1)/=B2 ) THEN
                  i = i + V(i+2) + 3
               ELSE
                  ASSIGN 1000 TO r
                  GOTO 1100
               ENDIF
            ENDDO
            WRITE (Nout,99003) B1 , B2
99003       FORMAT (1H0,19X,'PARAMETER NAMED ',2A4,' IS NOT IN VPS.')
            Ln = Ln + 2
            GOTO 99999
         ENDIF
      ENDIF
   ENDIF
!
 100  IF ( i>L ) GOTO 99999
   ASSIGN 200 TO r
   GOTO 1100
 200  i = i + nw + 3
   IF ( Ln>=Lnmax ) CALL page
   IF ( type==2 ) GOTO 400
   IF ( type==3 ) GOTO 500
   IF ( type==4 ) GOTO 600
   IF ( type==5 ) GOTO 700
   IF ( type==6 ) GOTO 800
 300  WRITE (Nout,99004) name(1) , name(2) , val(1)
99004 FORMAT (20X,2A4,10X,I10)
   GOTO 900
 400  WRITE (Nout,99005) name(1) , name(2) , wal(1)
99005 FORMAT (20X,2A4,10X,1P,E14.6)
   GOTO 900
 500  WRITE (Nout,99006) name(1) , name(2) , val(1) , val(2)
99006 FORMAT (20X,2A4,10X,2A4)
   GOTO 900
 600  WRITE (Nout,99007) name(1) , name(2) , dal(1)
99007 FORMAT (20X,2A4,10X,1P,D24.16)
   GOTO 900
 700  WRITE (Nout,99008) name(1) , name(2) , wal(1) , wal(2)
99008 FORMAT (20X,2A4,10X,1H(,1P,2E14.6,1H))
   IF ( Machx==5 ) WRITE (Nout,99009,ERR=900) dal(1)
99009 FORMAT (1H+,69X,'OR',D24.16)
   GOTO 900
 800  WRITE (Nout,99010) name(1) , name(2) , dal(1) , dal(2)
99010 FORMAT (20X,2A4,10X,1H(,1P,2D24.16,1H))
 900  IF ( kickk ) GOTO 99999
   Ln = Ln + 2
   GOTO 100
 1000 IF ( type==1 ) GOTO 300
   IF ( type==2 ) GOTO 400
   IF ( type==3 ) GOTO 500
   IF ( type==4 ) GOTO 600
   IF ( type==5 ) GOTO 700
   IF ( type==6 ) GOTO 800
   GOTO 99999
!
 1100 name(1) = V(i)
   name(2) = V(i+1)
   nw = V(i+2)
   DO m = 1 , nw
      mi = m + i
      val(m) = V(mi+2)
   ENDDO
   m = numtyp(val(1)) + 1
   IF ( m==2 ) THEN
      type = 1
   ELSEIF ( m==4 ) THEN
      type = 3
!            ZERO,INTG,REAL, BCD
!
   ELSEIF ( nw>1 ) THEN
      IF ( nw<4 ) THEN
!
!     THE 7094 AND 6600 SHOULD BE CORRECT
!     THE 360 AND 1108 CAN STILL HAVE SOME MISTAKES
!     VAX IS OK, OTHER UNIX MACHINES FOLLOW VAX              ** MACHX **
!     MACHINES ABOVE 12 NEED TO BE SET CORRECTLY IN NEXT GO TO STATEMENT
!
!            DUMMY  360  1108  6600   VAX  ULTRIX   SUN   AIX     HP
!             S/G   MAC  CRAY  CNVX   NEC  FUJISU    DG  AMDL  PRIME
!             486  DUMMY
!            ----  ----- ----  ----  ----  ------  ----  ----  -----
         IF ( Machx==1 ) THEN
            IF ( Machx==1 .AND. iabs(rshift(val(1),27))==oo+iabs(rshift(val(2),27)) ) THEN
               type = 4
            ELSE
               type = 5
            ENDIF
         ELSEIF ( Machx==2 ) THEN
            GOTO 1120
         ELSEIF ( Machx==3 ) THEN
            IF ( rshift(lshift(val(1),9),35)==1 .AND. rshift(lshift(val(2),9),35)==1 ) THEN
               type = 5
            ELSEIF ( val(2)==0 ) THEN
               type = 5
            ELSE
               type = 4
            ENDIF
         ELSEIF ( Machx==4 ) THEN
            IF ( iabs(rshift(val(1),48))==48+iabs(rshift(val(2),48)) ) THEN
               type = 4
            ELSE
               type = 5
            ENDIF
         ELSEIF ( Machx==12 ) THEN
!
!     ****** OH MY GOSH, HOW CAN I SOLVE THIS PROBLEM FOR THE VAX
!
            IF ( rshift(val(2),48)==0 ) THEN
               type = 4
            ELSE
               type = 5
            ENDIF
         ELSE
            GOTO 1120
!
!     ****** NEED TEST FOR RDP VS CSP.  I ASSUME CSP FOR NOW.
!
         ENDIF
         GOTO 1200
 1120    IF ( rshift(lshift(val(2),9),28)==0 .AND. val(2)/=0 ) THEN
            type = 4
         ELSE
            type = 5
         ENDIF
      ELSE
         type = 6
      ENDIF
   ELSE
      type = 2
   ENDIF
 1200 GOTO r
 1300 IF ( kick<=kt(mm) .OR. mm>26 ) THEN
      Ln = Ln + 5
      IF ( mm==1 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 1
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99011)
99011       FORMAT (//////,' STATIC ANALYSIS ERROR NO.2  MASS MATRIX REQUIRED',' FOR WEIGHT AND BALANCE CALCULATIONS.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99012)
99012       FORMAT (//////,' STATIC ANALYSIS ERROR NO.3  NO INDEPENDENT ','DEGREES OF FREEDOM HAVE BEEN DEFINED.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99013)
99013       FORMAT (//////,' STATIC ANALYSIS ERROR NO.4  NO ELEMENTS HAVE ','BEEN DEFINED.')
         ELSEIF ( kick==5 ) THEN
            WRITE (Nout,99014)
99014       FORMAT (//////,' STATIC ANALYSIS ERROR NO.5  A LOOPING PROBLEM ','RUN ON A NON-LOOPING SUBSET.')
         ELSE
            WRITE (Nout,99015)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 1
!
99015       FORMAT (//////,' STATIC ANALYSIS ERROR NO.1  ATTEMPT TO EXECUTE ','MORE THAN 360 LOOPS.')
         ENDIF
      ELSEIF ( mm==2 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 2
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99016)
99016       FORMAT (//////,' INERTIA RELIEF ERROR NO.2  ATTEMPT TO EXECUTE ','MORE THAN 360 LOOPS.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99017)
99017       FORMAT (//////,' INERTIA RELIEF ERROR NO.3  NO INDEPENDENT ','DEGREES OF FREEDOM HAVE BEEN DEFINED.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99018)
99018       FORMAT (//////,' INERTIA RELIEF ERROR NO.4  FREE BODY SUPPORTS ','ARE REQUIRED.')
         ELSEIF ( kick==5 ) THEN
            WRITE (Nout,99019)
99019       FORMAT (//////,' INERTIA RELIEF ERROR NO.5  A LOOPING PROBLEM ','RUN ON A NON-LOOPING SUBSET.')
         ELSEIF ( kick==6 ) THEN
            WRITE (Nout,99020)
99020       FORMAT (//////,' INERTIA RELIEF ERROR NO.6  NO STRUCTURAL ','ELEMENTS HAVE BEEN DEFINED.')
         ELSE
            WRITE (Nout,99021)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 2
!
99021       FORMAT (//////,' INERTIA RELIEF ERROR NO.1  MASS MATRIX REQUIRED',' FOR CALCULATION OF INERTIA LOADS.')
         ENDIF
      ELSEIF ( mm==3 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 3
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99022)
99022       FORMAT (//////,' NORMAL MODES ERROR NO.2  EIGENVALUE EXTRACTION ','DATA REQUIRED FOR REAL EIGENVALUE ANALYSIS.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99023)
99023       FORMAT (//////,' NORMAL MODES ERROR NO.3  NO INDEPENDENT DEGREES',' OF FREEDOM HAVE BEEN DEFINED.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99024)
99024       FORMAT (//////,' NORMAL MODES ERROR NO.4  NO STRUCTURAL ELEMENTS',' HAVE BEEN DEFINED.')
         ELSE
            WRITE (Nout,99025)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 3
!
99025       FORMAT (//////,' NORMAL MODES ERROR NO.1  MASS MATRIX REQUIRED ','FOR REAL EIGENVALUE ANALYSIS.')
         ENDIF
      ELSEIF ( mm==4 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 4
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99026)
99026       FORMAT (//////,' DIFFERENTIAL STIFFNESS ERROR NO.2  FREE BODY ','SUPPORTS NOT ALLOWED.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99027)
99027       FORMAT (//////,' DIFFERENTIAL STIFFNESS ERROR NO.3  NO GRID POINT',' DATA IS SPECIFIED.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99028)
99028       FORMAT (//////,' DIFFERENTIAL STIFFNESS ERROR NO.4  MASS MATRIX ','REQUIRED FOR WEIGHT AND BALANCE CALCULATIONS.')
         ELSEIF ( kick==5 ) THEN
            WRITE (Nout,99029)
99029       FORMAT (//////,' DIFFERENTIAL STIFFNESS ERROR NO.5  NO ','INDEPENDENT DEGREES OF FREEDOM HAVE BEEN DEFINED.')
         ELSE
            WRITE (Nout,99030)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 4
!
99030       FORMAT (//////,' DIFFERENTIAL STIFFNESS ERROR NO.1  NO STRUCTURAL',' ELEMENTS HAVE BEEN DEFINED.')
         ENDIF
      ELSEIF ( mm==5 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 5
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99031)
99031       FORMAT (//////,' BUCKLING ANALYSIS ERROR NO.2  FREE BODY SUPPORTS',' NOT ALLOWED.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99032)
99032       FORMAT (//////,' BUCKLING ANALYSIS ERROR NO.3  EIGENVALUE ','EXTRACTION DATA REQUIRED FOR REAL EIGENVALUE ANALYSIS.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99033)
99033       FORMAT (//////,' BUCKLING ANALYSIS ERROR NO.4  NO EIGENVALUES ','FOUND.')
         ELSEIF ( kick==5 ) THEN
            WRITE (Nout,99034)
99034       FORMAT (//////,' BUCKLING ANALYSIS ERROR NO.5  MASS MATRIX ','REQUIRED FOR WEIGHT AND BALANCE CALCULATIONS.')
         ELSEIF ( kick==6 ) THEN
            WRITE (Nout,99035)
99035       FORMAT (//////,' BUCKLING ANALYSIS ERROR NO.6  NO INDEPENDENT ','DEGREES OF FREEDOM HAVE BEEN DEFINED.')
         ELSE
            WRITE (Nout,99036)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 5
!
99036       FORMAT (//////,' BUCKLING ANALYSIS ERROR NO.1  NO STRUCTURAL ','ELEMENTS HAVE BEEN DEFINED.')
         ENDIF
      ELSEIF ( mm==6 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 6
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99037)
99037       FORMAT (//////,' PIECEWISE LINEAR ERROR NO.2  ATTEMPT TO EXECUTE',' MORE THAN 360 LOOPS.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99038)
99038       FORMAT (//////,' PIECEWISE LINEAR ERROR NO.3  MASS MATRIX ','REQUIRED FOR WEIGHT AND BALANCE CALCULATIONS.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99039)
99039       FORMAT (//////,' PIECEWISE LINEAR ERROR NO.4  NO ELEMENTS HAVE ','BEEN DEFINED.')
         ELSEIF ( kick==5 ) THEN
            WRITE (Nout,99040)
99040       FORMAT (//////,' PIECEWISE LINEAR ERROR NO.5  STIFFNESS MATRIX ','SINGULAR DUE TO MATERIAL PLASTICITY.')
         ELSE
            WRITE (Nout,99041)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 6
!
99041       FORMAT (//////,' PIECEWISE LINEAR ERROR NO.1  NO NONLINEAR ','ELEMENTS HAVE BEEN DEFINED.')
         ENDIF
      ELSEIF ( mm==7 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 7
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99042)
99042       FORMAT (//////,' DIRECT COMPLEX EIGENVALUE ERROR NO.2  ATTEMPT ','TO EXECUTE MORE THAN 100 LOOPS.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99043)
99043       FORMAT (//////,' DIRECT COMPLEX EIGENVALUE ERROR NO.3  MASS ','MATRIX REQUIRED FOR WEIGHT AND BALANCE CALCULATIONS.')
         ELSE
            WRITE (Nout,99044)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 7
!
99044       FORMAT (//////,' DIRECT COMPLEX EIGENVALUE ERROR NO.1  ','EIGENVALUE EXTRACTION DATA REQUIRED FOR COMPLEX ',            &
                   &'EIGENVALUE ANALYSIS.')
         ENDIF
      ELSEIF ( mm==8 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 8
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99045)
99045       FORMAT (//////,' DIRECT FREQUENCY RESPONSE ERROR NO.2  DYNAMIC ',                                                       &
                   &'LOADS TABLE REQUIRED FOR FREQUENCY RESPONSE CALCULATIONS')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99046)
99046       FORMAT (//////,' DIRECT FREQUENCY RESPONSE ERROR NO.3  ATTEMPT ','TO EXECUTE MORE THAN 100 LOOPS.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99047)
99047       FORMAT (//////,' DIRECT FREQUENCY RESPONSE ERROR NO.4  MASS ','MATRIX REQUIRED FOR WEIGHT AND BALANCE CALCULATIONS.')
         ELSE
            WRITE (Nout,99048)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 8
!
99048       FORMAT (//////,' DIRECT FREQUENCY RESPONSE ERROR NO.1  FREQUENCY',' RESPONSE LIST REQUIRED FOR FREQUENCY RESPONSE ',    &
                   &'CALCULATIONS.')
         ENDIF
      ELSEIF ( mm==9 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 9
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99049)
99049       FORMAT (//////,' DIRECT TRANSIENT RESPONSE ERROR NO.2  ATTEMPT ','TO EXECUTE MORE THAN 100 LOOPS.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99050)
99050       FORMAT (//////,' DIRECT TRANSIENT RESPONSE ERROR NO.3  MASS ','MATRIX REQUIRED FOR WEIGHT AND BALANCE CALCULATIONS.')
         ELSE
            WRITE (Nout,99051)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 9
!
99051       FORMAT (//////,' DIRECT TRANSIENT RESPONSE ERROR NO.1  TRANSIENT',' RESPONSE LIST REQUIRED FOR TRANSIENT RESPONSE ',    &
                   &'CALCULATIONS.')
         ENDIF
      ELSEIF ( mm==10 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 10
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99052)
99052       FORMAT (//////,' MODAL COMPLEX EIGENVALUE ERROR NO.2  EIGENVALUE',                                                      &
                   &' EXTRACTION DATA REQUIRED FOR REAL EIGENVALUE ANALYSIS.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99053)
99053       FORMAT (//////,' MODAL COMPLEX EIGENVALUE ERROR NO.3  ATTEMPT TO',' EXECUTE MORE THAN 100 LOOPS.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99054)
99054       FORMAT (//////,' MODAL COMPLEX EIGENVALUE ERROR NO.4  REAL ','EIGENVALUES REQUIRED FOR MODAL FORMULATION.')
         ELSEIF ( kick==5 ) THEN
            WRITE (Nout,99055)
99055       FORMAT (//////,' MODAL COMPLEX EIGENVALUE ERROR NO.5  NO ','STRUCTURAL ELEMENTS HAVE BEEN DEFINED.')
         ELSE
            WRITE (Nout,99056)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 10
!
99056       FORMAT (//////,' MODAL COMPLEX EIGENVALUE ERROR NO.1  MASS MATRIX',' REQUIRED FOR MODAL FORMULATION.')
         ENDIF
      ELSEIF ( mm==11 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 11
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99057)
99057       FORMAT (//////,' MODAL FREQUENCY RESPONSE ERROR NO.2  EIGENVALUE',                                                      &
                   &' EXTRACTION DATA REQUIRED FOR REAL EIGENVALUE ANALYSIS.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99058)
99058       FORMAT (//////,' MODAL FREQUENCY RESPONSE ERROR NO.3  ATTEMPT TO',' EXECUTE MORE THAN 100 LOOPS.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99059)
99059       FORMAT (//////,' MODAL FREQUENCY RESPONSE ERROR NO.4  REAL ','EIGENVALUES REQUIRED FOR MODAL FORMULATION.')
         ELSEIF ( kick==5 ) THEN
            WRITE (Nout,99060)
99060       FORMAT (//////,' MODAL FREQUENCY RESPONSE ERROR NO.5  FREQUENCY ','RESPONSE LIST REQUIRED FOR FREQUENCY RESPONSE ',     &
                   &'CALCULATIONS.')
         ELSEIF ( kick==6 ) THEN
            WRITE (Nout,99061)
99061       FORMAT (//////,' MODAL FREQUENCY RESPONSE ERROR NO.6  DYNAMIC ',                                                        &
                   &'LOADS TABLE REQUIRED FOR FREQUENCY RESPONSE CALCULATIONS')
         ELSEIF ( kick==7 ) THEN
            WRITE (Nout,99062)
99062       FORMAT (//////,' MODAL FREQUENCY RESPONSE ERROR NO.7  NO ','STRUCTURAL ELEMENTS HAVE BEEN DEFINED.')
         ELSE
            WRITE (Nout,99063)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 11
!
99063       FORMAT (//////,' MODAL FREQUENCY RESPONSE ERROR NO.1  MASS MATRIX',' REQUIRED FOR MODAL FORMULATION.')
         ENDIF
      ELSEIF ( mm==12 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 12
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99064)
99064       FORMAT (//////,' MODAL TRANSIENT RESPONSE ERROR NO.2 EIGENVALUE ',                                                      &
                   &'EXTRACTION DATA REQUIRED FOR REAL EIGENVALUE ANALYSIS.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99065)
99065       FORMAT (//////,' MODAL TRANSIENT RESPONSE ERROR NO.3 ATTEMPT TO ','EXECUTE MORE THAN 100 LOOPS.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99066)
99066       FORMAT (//////,' MODAL TRANSIENT RESPONSE ERROR NO.4 REAL ','EIGENVALUES REQUIRED FOR MODAL FORMULATION.')
         ELSEIF ( kick==5 ) THEN
            WRITE (Nout,99067)
99067       FORMAT (//////,' MODAL TRANSIENT RESPONSE ERROR NO.5 TRANSIENT ','RESPONSE LIST REQUIRED FOR TRANSIENT RESPONSE ',      &
                   &'CALCULATIONS.')
         ELSEIF ( kick==6 ) THEN
            WRITE (Nout,99068)
99068       FORMAT (//////,' MODAL TRANSIENT RESPONSE ERROR NO.6 NO ','STRUCTURAL ELEMENTS HAVE BEEN DEFINED.')
         ELSE
            WRITE (Nout,99069)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 12
!
99069       FORMAT (//////,' MODAL TRANSIENT RESPONSE ERROR NO.1  MASS MATRIX',' REQUIRED FOR MODAL FORMULATION.')
         ENDIF
      ELSEIF ( mm==13 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 13
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99070)
99070       FORMAT (//////,' NORMAL MODES WITH DIFFERENTIAL STIFFNESS ERROR ','NO.2  FREE BODY SUPPORTS NOT ALLOWED.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99071)
99071       FORMAT (//////,' NORMAL MODES WITH DIFFERENTIAL STIFFNESS ERROR ','NO.3  EIGENVALUE EXTRACTION DATA REQUIRED FOR REAL ',&
                   &'EIGENVALUE ANALYSIS.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99072)
99072       FORMAT (//////,' NORMAL MODES WITH DIFFERENTIAL STIFFNESS ERROR ','NO.4  NO EIGENVALUE FOUND.')
         ELSEIF ( kick==5 ) THEN
            WRITE (Nout,99073)
99073       FORMAT (//////,' NORMAL MODES WITH DIFFERENTIAL STIFFNESS ERROR ',                                                      &
                   &'NO. 5  MASS MATRIX REQUIRED FOR REAL EIGENVALUE ANALYSIS')
         ELSEIF ( kick==6 ) THEN
            WRITE (Nout,99074)
99074       FORMAT (//////,' NORMAL MODES WITH DIFFERENTIAL STIFFNESS ERROR ','NO. 6  NO INDEPENDENT DEGREES OF FREEDOM HAVE BEEN ',&
                   &'DEFINED.')
         ELSE
            WRITE (Nout,99075)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 13
!
99075       FORMAT (//////,' NORMAL MODES WITH DIFFERENTIAL STIFFNESS ERROR ','NO.1  NO STRUCTURAL ELEMENTS HAVE BEEN DEFINED.')
         ENDIF
      ELSEIF ( mm==14 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 14
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99076)
99076       FORMAT (//////,' STATICS WITH CYCLIC TRANSFORMATION ERROR NO. 2  ',                                                     &
                   &'MASS MATRIX REQUIRED FOR WEIGHT AND BALANCE CALCULATIONS')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99077)
99077       FORMAT (//////,' STATICS WITH CYCLIC TRANSFORMATION ERROR NO. 3 ',                                                      &
                   &' NO INDEPENDENT DEGREES OF FREEDOM HAVE BEEN DEFINED.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99078)
99078       FORMAT (//////,' STATICS WITH CYCLIC TRANSFORMATION ERROR NO. 4 ',' NO ELEMENTS HAVE BEEN DEFINED.')
         ELSEIF ( kick==5 ) THEN
            WRITE (Nout,99079)
99079       FORMAT (//////,' STATICS WITH CYCLIC TRANSFORMATION ERROR NO. 5 ',' CYCLIC TRANSFORMATION DATA ERROR.')
         ELSEIF ( kick==6 ) THEN
            WRITE (Nout,99080)
99080       FORMAT (//////,' STATICS WITH CYCLIC TRANSFORMATION ERROR NO. 6 ',' FREE BODY SUPPORTS NOT ALLOWED.')
         ELSE
            WRITE (Nout,99081)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 14
!
99081       FORMAT (//////,' STATICS WITH CYCLIC TRANSFORMATION ERROR NO. 1 ',' ATTEMPT TO EXECUTE MORE THAN 360 LOOPS.')
         ENDIF
      ELSEIF ( mm==15 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 15
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99082)
99082       FORMAT (//////,' NORMAL MODES WITH CYCLIC TRANSFORMATION ERROR ','NO. 2  EIGENVALUE EXTRACTION DATA REQUIRED FOR REAL ',&
                   &'EIGENVALUE ANALYSIS.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99083)
99083       FORMAT (//////,' NORMAL MODES WITH CYCLIC TRANSFORMATION ERROR ','NO. 3  NO INDEPENDENT DEGREES OF FREEDOM HAVE BEEN ', &
                   &'DEFINED.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99084)
99084       FORMAT (//////,' NORMAL MODES WITH CYCLIC TRANSFORMATION ERROR ','NO. 4  FREE BODY SUPPORTS NOT ALLOWED.')
         ELSEIF ( kick==5 ) THEN
            WRITE (Nout,99085)
99085       FORMAT (//////,' NORMAL MODES WITH CYCLIC TRANSFORMATION ERROR ','NO. 5  CYCLIC TRANSFORMATION DATA ERROR.')
         ELSEIF ( kick==6 ) THEN
            WRITE (Nout,99086)
99086       FORMAT (//////,' NORMAL MODES WITH CYCLIC TRANSFORMATION ERROR ','NO. 6  NO STRUCTURAL ELEMENTS HAVE BEEN DEFINED.')
         ELSE
            WRITE (Nout,99087)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 15
!
99087       FORMAT (//////,' NORMAL MODES WITH CYCLIC TRANSFORMATION ERROR ',                                                       &
                   &'NO. 1  MASS MATRIX REQUIRED FOR REAL EIGENVALUE ANALYSIS')
         ENDIF
      ELSEIF ( mm==16 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 16
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99088)
99088       FORMAT (//////,' AEROTHERMOELASTIC ERROR NO. 2  FREE BODY ','SUPPORTS NOT ALLOWED.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99089)
99089       FORMAT (//////,' AEROTHERMOELASTIC ERROR NO. 3  NO GRID POINT ','DATA IS SPECIFIED.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99090)
99090       FORMAT (//////,' AEROTHERMOELASTIC ERROR NO. 4  MASS MATRIX ','REQUIRED FOR WEIGHT AND BALANCE CALCULATIONS.')
         ELSEIF ( kick==5 ) THEN
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 17
!
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 18
!
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 19
!
            WRITE (Nout,99091)
99091       FORMAT (//////,' AEROTHERMOELASTIC ERROR NO. 5  NO INDEPENDENT ','DEGREES OF FREEDOM HAVE BEEN DEFINED.')
         ELSE
            WRITE (Nout,99092)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 16
!
99092       FORMAT (//////,' AEROTHERMOELASTIC ERROR NO. 1  NO STRUCTURAL ','ELEMENTS HAVE BEEN DEFINED.')
         ENDIF
      ELSEIF ( mm==17 ) THEN
      ELSEIF ( mm==18 ) THEN
      ELSEIF ( mm==19 ) THEN
      ELSEIF ( mm==20 ) THEN
!
!     HEAT APPROACH - RIGID FORMAT 1
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99093)
99093       FORMAT (//////,' STATIC HEAT TRANSFER ERROR NO. 2  LOOPING ','PROBLEM RUN ON A NON-LOOPING SUBSET.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99094)
99094       FORMAT (//////,' STATIC HEAT TRANSFER ERROR NO. 3  NO INDEPENDENT',' DEGREES OF FREEDOM HAVE BEEN DEFINED.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99095)
99095       FORMAT (//////,' STATIC HEAT TRANSFER ERROR NO. 4  NO ELEMENTS ','HAVE BEEN DEFINED.')
         ELSE
            WRITE (Nout,99096)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 17
!
!5800 FORMAT (//)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 18
!
!5900 FORMAT (//)
!
!     DISPLACEMENT APPROACH - RIGID FORMAT 19
!
!6000 FORMAT (//)
!
!
!     HEAT APPROACH - RIGID FORMAT 1
!
99096       FORMAT (//////,' STATIC HEAT TRANSFER ERROR NO. 1  ATTEMPT TO ','EXECUTE MORE THAN 100 LOOPS.')
         ENDIF
      ELSEIF ( mm==21 ) THEN
!
!     HEAT APPROACH - RIGID FORMAT 3
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99097)
99097       FORMAT (//////,' NONLINEAR STATIC HEAT TRANSFER ERROR NO. 2  NO ','SIMPLE STRUCTURAL ELEMENTS.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99098)
99098       FORMAT (//////,' NONLINEAR STATIC HEAT TRANSFER ERROR NO. 3  ','STIFFNESS MATRIX SINGULAR.')
         ELSE
            WRITE (Nout,99099)
!
!     HEAT APPROACH - RIGID FORMAT 3
!
99099       FORMAT (//////,' NONLINEAR STATIC HEAT TRANSFER ERROR NO. 1  NO ','INDEPENDENT DEGREES OF FREEDOM HAVE BEEN DEFINED.')
         ENDIF
      ELSEIF ( mm==22 ) THEN
!
!     HEAT APPROACH - RIGID FORMAT 9
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99100)
99100       FORMAT (//////,' TRANSIENT HEAT TRANSFER ERROR NO. 2  ATTEMPT ','TO EXECUTE MORE THAN 100 LOOPS.')
         ELSE
            WRITE (Nout,99101)
!
!     HEAT APPROACH - RIGID FORMAT 9
!
99101       FORMAT (//////,' TRANSIENT HEAT TRANSFER ERROR NO. 1  TRANSIENT ','RESPONSE LIST REQUIRED FOR TRANSIENT RESPONSE ',     &
                   &'CALCULATIONS.')
         ENDIF
      ELSEIF ( mm==23 ) THEN
!
!     AERO APPROACH - RIGID FORMAT 9
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99102)
99102       FORMAT (//////,' BLADE FLUTTER ANALYSIS ERROR NO. 2  EIGENVALUE ',                                                      &
                   &'EXTRACTION DATA REQUIRED FOR REAL EIGENVALUE ANALYSIS.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99103)
99103       FORMAT (//////,' BLADE FLUTTER ANALYSIS ERROR NO. 3  ATTEMPT TO ','EXECUTE MORE THAN 100 LOOPS.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99104)
99104       FORMAT (//////,' BLADE FLUTTER ANALYSIS ERROR NO. 4  REAL ','EIGENVALUES REQUIRED FOR MODAL FORMULATION.')
         ELSEIF ( kick==5 ) THEN
            WRITE (Nout,99105)
99105       FORMAT (//////,' BLADE FLUTTER ANALYSIS ERROR NO. 5  NO GRID ','POINT DATA IS SPECIFIED OR NO STRUCTURAL ELEMENTS HAVE '&
                  & ,'BEEN DEFINED.')
         ELSEIF ( kick==6 ) THEN
            WRITE (Nout,99106)
99106       FORMAT (//////,' BLADE FLUTTER ANALYSIS ERROR NO. 6  FREE BODY ','SUPPORTS NOT ALLOWED.')
         ELSEIF ( kick==7 ) THEN
            WRITE (Nout,99107)
99107       FORMAT (//////,' BLADE FLUTTER ANALYSIS ERROR NO. 7  CYCLIC ','TRANSFORMATION DATA ERROR.')
         ELSE
            WRITE (Nout,99108)
!
!     AERO APPROACH - RIGID FORMAT 9
!
99108       FORMAT (//////,' BLADE FLUTTER ANALYSIS ERROR NO. 1  MASS MATRIX',' REQUIRED FOR MODAL FORMULATION.')
         ENDIF
      ELSEIF ( mm==24 ) THEN
!
!     AERO APPROACH - RIGID FORMAT 10
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99109)
99109       FORMAT (//////,' MODAL FLUTTER ANALYSIS ERROR NO. 2  EIGENVALUE ',                                                      &
                   &'EXTRACTION DATA REQUIRED FOR REAL EIGENVALUE ANALYSIS.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99110)
99110       FORMAT (//////,' MODAL FLUTTER ANALYSIS ERROR NO. 3  ATTEMPT TO ','EXECUTE MORE THAN 100 LOOPS.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99111)
99111       FORMAT (//////,' MODAL FLUTTER ANALYSIS ERROR NO. 4  REAL ','EIGENVALUES REQUIRED FOR MODAL FORMULATION.')
         ELSEIF ( kick==5 ) THEN
            WRITE (Nout,99112)
99112       FORMAT (//////,' MODAL FLUTTER ANALYSIS ERROR NO. 5  NO GRID ','POINT DATA IS SPECIFIED OR NO STRUCTURAL ELEMENTS HAVE '&
                  & ,'BEEN DEFINED.')
         ELSE
            WRITE (Nout,99113)
!
!     AERO APPROACH - RIGID FORMAT 10
!
99113       FORMAT (//////,' MODAL FLUTTER ANALYSIS ERROR NO. 1  MASS MATRIX',' REQUIRED FOR MODAL FORMULATION.')
         ENDIF
      ELSEIF ( mm==25 ) THEN
!
!     AERO APPROACH - RIGID FORMAT 11
!
         IF ( kick==2 ) THEN
            WRITE (Nout,99114)
99114       FORMAT (//////,' MODAL AEROELASTIC RESPONSE ERROR NO. 2  ','EIGENVALUE EXTRACTION DATA REQUIRED FOR REAL EIGENVALUE ',  &
                   &'ANALYSIS.')
         ELSEIF ( kick==3 ) THEN
            WRITE (Nout,99115)
99115       FORMAT (//////,' MODAL AEROELASTIC RESPONSE ERROR NO. 3  NO GRID',                                                      &
                   &' POINT DATA IS SPECIFIED OR NO STRUCTURAL ELEMENTS HAVE ','BEEN DEFINED.')
         ELSEIF ( kick==4 ) THEN
            WRITE (Nout,99116)
99116       FORMAT (//////,' MODAL AEROELASTIC RESPONSE ERROR NO. 4  REAL ','EIGENVALUES REQUIRED FOR MODAL FORMULATION.')
         ELSE
            WRITE (Nout,99117)
!
!    AERO APPROACH - RIGID FORMAT 11
!
99117       FORMAT (//////,' MODAL AEROELASTIC RESPONSE ERROR NO. 1  MASS ','MATRIX REQUIRED FOR MODAL FORMULATION.')
         ENDIF
      ELSEIF ( mm==26 ) THEN
!
!     DMAP APPROACH
!
         WRITE (Nout,99118) kick
!
!     DMAP APPROACH
!
99118    FORMAT (//////,10X,'DMAP ERROR',3X,I20)
      ELSE
         GOTO 1400
      ENDIF
      IF ( Ic>=0 ) RETURN
      CALL mesage(-61,0,0)
      GOTO 99999
   ENDIF
 1400 WRITE (Nout,99119) kick
99119 FORMAT ('0PRTPARM DIAGNOSTIC',I20,' NOT IN TABLE.')
   Ln = Ln + 2
   CALL mesage(-61,0,0)
!
!
99999 RETURN
END SUBROUTINE prtprm
