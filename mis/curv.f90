
SUBROUTINE curv
   IMPLICIT NONE
!
! COMMON variable declarations
!
   LOGICAL Eofos1 , Foes1g , Strain
   INTEGER File , Imsg , Indexs(108) , Ioutpt , Ip1 , Ip2 , Isysbf , Iz(1) , Lcore , Lmcsid , Loc , Logerr
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Ip1 , Ip2
   COMMON /curvtb/ Indexs
   COMMON /system/ Isysbf , Ioutpt
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Iz
!
! Local variable declarations
!
   INTEGER i , j , jsub , mcb(7) , subr(6)
!
! End of declarations
!
!
!     MAIN DRIVING ROUTINE OF MODULE -CURV-.
!
!     DMAP CALLING SEQUENCE.
!
!     CURV   OES1,MPT,CSTM,EST,SIL,GPL/OES1M,OES1G/P1/P2 $
!
!
!     COMMON /ZZCURV/ MUST BE AT THE LONGEST OF OVERLAYS WITH CURV1,
!     CURV2, AND CURV3.
!
   EQUIVALENCE (Indexs(16),Lmcsid) , (Indexs(52),Lcore) , (Indexs(79),Loc) , (Indexs(80),File) , (Indexs(81),Imsg) ,                &
    & (Indexs(100),Eofos1) , (Indexs(103),Foes1g) , (Indexs(104),Strain) , (Indexs(105),Logerr)
   DATA subr/4HCURV , 4H1    , 4HCURV , 4H2    , 4HCURV , 4H3   /
!
!
!     CHECK TO SEE IF COMPUTATIONS NEED TO BE DONE
!
   IF ( Ip1<0 ) RETURN
!
!     CHECK TO SEE IF THE INPUT FILE EXISTS
!
   mcb(1) = 101
   CALL rdtrl(mcb(1))
   IF ( mcb(1)<=0 ) RETURN
!
!     PERFORM INITIALIZATION AND CREATE ESTX ON SCRATCH FILE 1.
!
   DO i = 1 , 107
      Indexs(i) = 777777777
   ENDDO
   Imsg = 0
   jsub = 1
   CALL curv1
   IF ( Imsg==-8 ) GOTO 300
   IF ( Imsg<0 ) GOTO 200
   IF ( Lmcsid<=0 ) THEN
!
!     NO NON-ZERO MATERIAL COORDINATE SYSTEM IDS ENCOUNTERED
!
      CALL page2(3)
      WRITE (Ioutpt,99001) Uwm
99001 FORMAT (A25,' 3173, NO NON-ZERO MATERIAL COORDINATE SYSTEM IDS ','ENCOUNTERED IN MODULE CURV.')
      IF ( .NOT.Strain ) WRITE (Ioutpt,99002)
99002 FORMAT (39H STRESSES IN MATERIAL COORDINATE SYSTEM,14H NOT COMPUTED.)
      IF ( Strain ) WRITE (Ioutpt,99003)
99003 FORMAT (49H STRAINS/CURVATURES IN MATERIAL COORDINATE SYSTEM,14H NOT COMPUTED.)
   ELSE
!
!     CREATE OES1M FOR NEXT SUBCASE IF NOT AT EOF IN OES1.
!
      DO WHILE ( .NOT.(Eofos1) )
         jsub = 2
         CALL curv2
         IF ( Imsg==-8 ) GOTO 300
         IF ( Imsg<0 ) GOTO 200
!
!     IF OES1G IS TO BE FORMED CALL CURV3 OVERLAY.  PROCESS CURRENT
!     SUBCASE
!
         IF ( Foes1g ) THEN
            jsub = 3
            CALL curv3
            IF ( Imsg==-8 ) GOTO 300
            IF ( Imsg<0 ) GOTO 200
         ENDIF
      ENDDO
   ENDIF
!
!     EOF HIT IN OES1.  ALL THROUGH.
!
 100  RETURN
!
!     ERROR CONDITION IN CURV1, CURV2, OR CURV3.
!
 200  IF ( Imsg==-37 ) THEN
      WRITE (Ioutpt,99005) Sfm , jsub , Imsg , Loc , jsub , File
      WRITE (Ioutpt,99004) Indexs
99004 FORMAT (/5X,29H CONSTANTS IN COMMON /CURVTB/,/,(3X,4I15))
   ENDIF
!
!     INSURE ALL FILES CLOSED
!
   DO i = 1 , 9
      DO j = 100 , 300 , 100
         CALL close(i+j,1)
      ENDDO
   ENDDO
 300  WRITE (Ioutpt,99005) Sfm , jsub , Imsg , Loc , jsub , File
   jsub = 2*jsub - 1
   IF ( Imsg==-8 ) File = Lcore
   CALL mesage(Imsg,File,subr(jsub))
   GOTO 100
99005 FORMAT (A25,' 3174, SUBROUTINE CURV',I1,' HAS RETURNED WITH ERROR CONDITION ',I4,/5X,'LOCATION CODE = ',I4,                   &
             &' IN SUBROUTINE CURV',I1,/5X,'FILE NUMBER   = ',I4)
END SUBROUTINE curv
