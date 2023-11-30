
SUBROUTINE eqmck
   IMPLICIT NONE
   INTEGER Bgpdt , Casecc , Cstm , Eqexin , Gm , Gpl , Igrid , Iopt , K(21) , Kgg , Kload , Kmpc , Kspc , Ksystm(80) , Lama , Nout ,&
         & Nskip , Oqm , Parm(4) , Pgg , Qg , Sf(7) , Sil , Trl(7) , Ugv , Uset
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Iopt , Igrid , Nskip
   COMMON /eqmk1 / K , Kmpc , Kload , Kspc , Parm , Trl
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm
   INTEGER i , kfil(14) , l , name(2) , sfl(7)
!
!     EQMCK CREATES AN OUTPUT FILE OF MPC CONSTRAINT FORCES AND AN
!     OVERALL TOTAL OF FORCES AND MOMENTS ON THE MODEL TO PROVIDE AN
!     EQUILIBRIUM CHECK.
!     VALID ONLY FOR STATICS AND REAL EIGENVALUE ANALYSIS.
!
!     DMAP CALLING SEQUENCE (DEFAULT PARAMETERS SHOWN)
!                                                        LAMA
!     EQMCK  CASECC,EQEXIN,GPL,BGPDT,SIL,USET,KGG,GM,UGV,PGG,QG,CSTM/
!            OQM1/V,Y,OPT=0/V,Y,GRDPNT=-1/V,N,NSKIP/V,Y,SUBNAM $
!     WHERE
!     OPT .EQ. 0, CREATE OQM
!         .LT. 0, CALCULATE ST1
!         .GT. 0, CALCULATE ST1 AND CREATES OQM
!     GRDPNT - POINT ABOUT WHICH EQUILIBRIUM IS CALCULATED.
!     NSKIP  - NO. RECORDS TO SKIP ON APPENDED FILES (1 OR GREATER),
!              NEGATIVE IF EIGENVALUE PROBLEM.
!     SUBNAM - RESERVED FOR FUTURE USE
!
   !>>>>EQUIVALENCE (Ksystm(2),Nout) , (K(1),Casecc) , (K(2),Eqexin) , (K(3),Gpl) , (K(4),Bgpdt) , (K(5),Sil) , (K(6),Uset) , (K(7),Kgg) &
!>>>>    & , (K(8),Gm) , (K(9),Ugv) , (K(10),Pgg) , (K(11),Qg) , (K(12),Cstm) , (K(13),Lama) , (K(14),Oqm) , (K(15),Sf(1))
!          ... CASECC,EQEXIN,GPL,BGPDT,SIL,USET,KGG,GM ,UGV,PGG,QG,CSTM,
!          ... LAMA , OQM .....
   DATA kfil/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108 , 109 , 110 , 111 , 112 , 110 , 201/
   DATA sfl/301 , 302 , 303 , 304 , 305 , 306 , 307/
   DATA name/4HEQMC , 2HK /
!
   Oqm = 0
   Kmpc = 0
   Kspc = 0
   Kload = 0
   Parm(3) = name(1)
   Parm(4) = name(2)
   DO i = 1 , 7
      Sf(i) = sfl(i)
   ENDDO
!
   DO i = 1 , 11
      Trl(1) = kfil(i)
      CALL rdtrl(Trl)
      K(i) = Trl(1)
   ENDDO
   Lama = K(10)
   Cstm = kfil(12)
!
!     ALWAYS NECESSARY FILES
!
   Parm(2) = kfil(1)
   IF ( Casecc<0 ) GOTO 300
   Parm(2) = kfil(2)
   IF ( Eqexin<0 ) GOTO 300
   Parm(2) = kfil(13)
   IF ( Nskip<0 .AND. Lama<0 ) GOTO 300
!
!     FILES FOR OQM
!
   l = 0
   IF ( Iopt>=0 ) THEN
      IF ( Gpl>=0 .AND. Sil>=0 .AND. Uset>=0 ) Oqm = kfil(14)
!
!     MPC CONSTRAINTS
!
      IF ( Gm>=0 .AND. Ugv>=0 .AND. Kgg>=0 ) Kmpc = 1
      IF ( Kmpc<=0 .OR. Iopt<0 ) Oqm = -kfil(14)
      IF ( Oqm<=0 ) THEN
         IF ( Iopt>=0 ) THEN
            CALL page2(2)
            WRITE (Nout,99001) Uwm , name
99001       FORMAT (A25,' 2370, MULTI-POINT CONSTRAINT FORCES NOT CALCULATED',' IN ',A4,A2,' DUE TO MISSING INPUT FILE.')
            IF ( Iopt==0 ) GOTO 200
         ENDIF
      ENDIF
   ENDIF
!
!     ST1 CALCULATION
!
!WKBD 11/93 SPR93007   40 IF (IOPT  .EQ. 0) GO TO 60
!WKBD 11/93 SPR93007      IF (BGPDT .LT. 0) GO TO 50
!WKBI 11/93 SPR93007
   IF ( Pgg>=0 .AND. Nskip>=0 ) Kload = 1
   IF ( Qg>=0 ) Kspc = 1
   l = Kspc + Kmpc + Kload
!WKBNB 11/93 SPR93007
   IF ( Iopt/=0 ) THEN
      IF ( Bgpdt>=0 ) THEN
!WKBNE 11/93 SPR93007
         IF ( l>0 ) GOTO 100
      ENDIF
      CALL page2(2)
      WRITE (Nout,99002) Uwm , name
!
!     ERROR MESSAGES
!
99002 FORMAT (A25,' 2371, EQUILIBRIUM FORCES NOT CALCULATED IN ',A4,A2,' DUE TO MISSING INPUT FILE.')
      IF ( Iopt<0 ) GOTO 200
      Iopt = 0
   ENDIF
!
 100  IF ( Iopt>=0 .OR. l/=0 ) THEN
      IF ( Iopt/=0 .OR. Oqm>0 ) THEN
         IF ( Iopt<=0 .OR. l/=0 .OR. Oqm>0 ) THEN
!
!     CREATE MPC DATA AND OQM
!
            IF ( Kmpc>0 .OR. (Nskip>1 .AND. l>0) ) CALL eqmckm
            IF ( l/=0 ) THEN
!
!     CALCULATE D-T FOR ST1
!
               i = Igrid
               CALL eqmcka(Igrid,Bgpdt,Cstm,Eqexin,Sf(2),l)
               IF ( Igrid/=0 ) Igrid = i
               IF ( l==0 ) THEN
!
                  CALL page2(2)
                  WRITE (Nout,99003) Uwm , name
99003             FORMAT (A25,' 2372, ',A4,A2,' IS UNABLE TO CALCULATE RIGID BODY ','TRANSFORMATION FOR SCALAR MODEL.')
               ELSE
!
!     CALCULATE AND OUTPUT ST1
!
                  CALL eqmcks
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
 200  RETURN
 300  Parm(1) = 1
   CALL mesage(Parm(1),Parm(2),Parm(3))
   GOTO 200
END SUBROUTINE eqmck