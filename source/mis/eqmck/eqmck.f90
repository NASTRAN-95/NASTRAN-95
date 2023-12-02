!*==eqmck.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE eqmck
   USE c_blank
   USE c_eqmk1
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: bgpdt , casecc , cstm , eqexin , gm , gpl , i , kgg , l , lama , nout , oqm , pgg , qg , sil , ugv , uset
   INTEGER , DIMENSION(14) , SAVE :: kfil
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(7) :: sf
   INTEGER , DIMENSION(7) , SAVE :: sfl
   EXTERNAL eqmcka , eqmckm , eqmcks , mesage , page2 , rdtrl
!
! End of declarations rewritten by SPAG
!
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
   oqm = 0
   kmpc = 0
   kspc = 0
   kload = 0
   parm(3) = name(1)
   parm(4) = name(2)
   DO i = 1 , 7
      sf(i) = sfl(i)
   ENDDO
!
   DO i = 1 , 11
      trl(1) = kfil(i)
      CALL rdtrl(trl)
      k(i) = trl(1)
   ENDDO
   lama = k(10)
   cstm = kfil(12)
!
!     ALWAYS NECESSARY FILES
!
   parm(2) = kfil(1)
   IF ( casecc<0 ) THEN
      CALL spag_block_3
      RETURN
   ENDIF
   parm(2) = kfil(2)
   IF ( eqexin<0 ) THEN
      CALL spag_block_3
      RETURN
   ENDIF
   parm(2) = kfil(13)
   IF ( nskip<0 .AND. lama<0 ) THEN
      CALL spag_block_3
      RETURN
   ENDIF
!
!     FILES FOR OQM
!
   l = 0
   IF ( iopt>=0 ) THEN
      IF ( gpl>=0 .AND. sil>=0 .AND. uset>=0 ) oqm = kfil(14)
!
!     MPC CONSTRAINTS
!
      IF ( gm>=0 .AND. ugv>=0 .AND. kgg>=0 ) kmpc = 1
      IF ( kmpc<=0 .OR. iopt<0 ) oqm = -kfil(14)
      IF ( oqm<=0 ) THEN
         IF ( iopt>=0 ) THEN
            CALL page2(2)
            WRITE (nout,99001) uwm , name
99001       FORMAT (A25,' 2370, MULTI-POINT CONSTRAINT FORCES NOT CALCULATED',' IN ',A4,A2,' DUE TO MISSING INPUT FILE.')
            IF ( iopt==0 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
!     ST1 CALCULATION
!
!WKBD 11/93 SPR93007   40 IF (IOPT  .EQ. 0) GO TO 60
!WKBD 11/93 SPR93007      IF (BGPDT .LT. 0) GO TO 50
!WKBI 11/93 SPR93007
   IF ( pgg>=0 .AND. nskip>=0 ) kload = 1
   IF ( qg>=0 ) kspc = 1
   l = kspc + kmpc + kload
!WKBNB 11/93 SPR93007
   IF ( iopt/=0 ) THEN
      IF ( bgpdt>=0 ) THEN
!WKBNE 11/93 SPR93007
         IF ( l>0 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
      ENDIF
      CALL page2(2)
      WRITE (nout,99002) uwm , name
!
!     ERROR MESSAGES
!
99002 FORMAT (A25,' 2371, EQUILIBRIUM FORCES NOT CALCULATED IN ',A4,A2,' DUE TO MISSING INPUT FILE.')
      IF ( iopt<0 ) THEN
         CALL spag_block_2
         RETURN
      ENDIF
      iopt = 0
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
      IF ( Iopt>=0 .OR. L/=0 ) THEN
         IF ( Iopt/=0 .OR. Oqm>0 ) THEN
            IF ( Iopt<=0 .OR. L/=0 .OR. Oqm>0 ) THEN
!
!     CREATE MPC DATA AND OQM
!
               IF ( Kmpc>0 .OR. (Nskip>1 .AND. L>0) ) CALL eqmckm
               IF ( L/=0 ) THEN
!
!     CALCULATE D-T FOR ST1
!
                  I = igrid
                  CALL eqmcka(igrid,Bgpdt,Cstm,Eqexin,Sf(2),L)
                  IF ( igrid/=0 ) igrid = I
                  IF ( L==0 ) THEN
!
                     CALL page2(2)
                     WRITE (Nout,99001) Uwm , Name
99001                FORMAT (A25,' 2372, ',A4,A2,' IS UNABLE TO CALCULATE RIGID BODY ','TRANSFORMATION FOR SCALAR MODEL.')
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
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
      parm(1) = 1
      CALL mesage(parm(1),parm(2),parm(3))
      CALL spag_block_2
   END SUBROUTINE spag_block_3
END SUBROUTINE eqmck
