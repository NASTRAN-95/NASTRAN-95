!*==curv.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE curv
   IMPLICIT NONE
   USE c_blank
   USE c_curvtb
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: eofos1 , foes1g , strain
   INTEGER :: file , i , imsg , j , jsub , lcore , lmcsid , loc , logerr
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(6) , SAVE :: subr
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Indexs(16),Lmcsid) , (Indexs(52),Lcore) , (Indexs(79),Loc) , (Indexs(80),File) , (Indexs(81),Imsg) ,                &
!>>>>    & (Indexs(100),Eofos1) , (Indexs(103),Foes1g) , (Indexs(104),Strain) , (Indexs(105),Logerr)
   DATA subr/4HCURV , 4H1    , 4HCURV , 4H2    , 4HCURV , 4H3   /
!
!
!     CHECK TO SEE IF COMPUTATIONS NEED TO BE DONE
!
   IF ( ip1<0 ) RETURN
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
      indexs(i) = 777777777
   ENDDO
   imsg = 0
   jsub = 1
   CALL curv1
   IF ( imsg==-8 ) GOTO 300
   IF ( imsg<0 ) GOTO 200
   IF ( lmcsid<=0 ) THEN
!
!     NO NON-ZERO MATERIAL COORDINATE SYSTEM IDS ENCOUNTERED
!
      CALL page2(3)
      WRITE (ioutpt,99001) uwm
99001 FORMAT (A25,' 3173, NO NON-ZERO MATERIAL COORDINATE SYSTEM IDS ','ENCOUNTERED IN MODULE CURV.')
      IF ( .NOT.strain ) WRITE (ioutpt,99002)
99002 FORMAT (39H STRESSES IN MATERIAL COORDINATE SYSTEM,14H NOT COMPUTED.)
      IF ( strain ) WRITE (ioutpt,99003)
99003 FORMAT (49H STRAINS/CURVATURES IN MATERIAL COORDINATE SYSTEM,14H NOT COMPUTED.)
   ELSE
!
!     CREATE OES1M FOR NEXT SUBCASE IF NOT AT EOF IN OES1.
!
      DO WHILE ( .NOT.(eofos1) )
         jsub = 2
         CALL curv2
         IF ( imsg==-8 ) GOTO 300
         IF ( imsg<0 ) GOTO 200
!
!     IF OES1G IS TO BE FORMED CALL CURV3 OVERLAY.  PROCESS CURRENT
!     SUBCASE
!
         IF ( foes1g ) THEN
            jsub = 3
            CALL curv3
            IF ( imsg==-8 ) GOTO 300
            IF ( imsg<0 ) GOTO 200
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
 200  IF ( imsg==-37 ) THEN
      WRITE (ioutpt,99005) sfm , jsub , imsg , loc , jsub , file
      WRITE (ioutpt,99004) indexs
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
 300  WRITE (ioutpt,99005) sfm , jsub , imsg , loc , jsub , file
   jsub = 2*jsub - 1
   IF ( imsg==-8 ) file = lcore
   CALL mesage(imsg,file,subr(jsub))
   GOTO 100
99005 FORMAT (A25,' 3174, SUBROUTINE CURV',I1,' HAS RETURNED WITH ERROR CONDITION ',I4,/5X,'LOCATION CODE = ',I4,                   &
             &' IN SUBROUTINE CURV',I1,/5X,'FILE NUMBER   = ',I4)
END SUBROUTINE curv
