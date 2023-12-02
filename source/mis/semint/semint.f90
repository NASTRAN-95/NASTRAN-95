!*==semint.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE semint(Debug1)
   IMPLICIT NONE
   USE c_ifpx1
   USE c_machin
   USE c_system
   USE c_xechox
   USE c_xmssg
   USE c_xxread
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Debug1
!
! Local variable declarations rewritten by SPAG
!
   REAL , SAVE :: bcd1 , bcd10 , bcd11 , bcd2 , bcd3 , bcd4 , bcd5 , bcd6 , bcd7 , bcd8 , bcd9
   REAL :: bcdx
   INTEGER :: iby , j , kaxif , l35 , l42 , nogo1 , nogox
   CHARACTER(6) , DIMENSION(13) , SAVE :: subr
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     SEMINT IS THE EXECUTION MONITOR FOR THE PREFACE.
!     UMF IS NO LONGER SUPPORTED.
!
!     FOR DEBUG PURPOSE, PRINT OUT GOES TO UNIT 6, NOT OUTTAP
!
   DATA bcd1 , bcd2 , bcd3 , bcd4 , bcd5 , bcd6 , bcd7/4HXCSA , 4HIFP1 , 4HXSOR , 4HXGPI , 4HGNFI , 4HTTIO , 4HTTLP/
   DATA bcd8 , bcd9 , bcd10 , bcd11/4HTTOT , 4HSOLI , 4HFLUI , 1HD/
   DATA subr/'NASCAR' , 'GNFIAT' , 'TMTSIO' , 'TMTSLP' , 'XCSA  ' , 'TMTSOT' , 'ASDMAP' , 'IFP1  ' , 'XSORT2' , 'IFP   ' ,          &
      & 'IFP3  ' , 'XGPI  ' , 'BANDIT'/
!
!
   insave = intap
!
!     READ AND PROCESS THE NASTRAN CARD (IF PRESENT).
!
!hgs disable nascar
   iby = 0
   IF ( iby==0 ) THEN
      IF ( Debug1>0 ) WRITE (6,99006) subr(1)
      CALL nascar
   ENDIF
!
!     DEFINE OPEN CORE FOR UNIVAC, VAX, AND UNIX
!
   IF ( mach==3 .OR. mach>=5 ) CALL defcor
!
!     GENERATE INITIAL FILE TABLES.
!     COMPUTE NASTRAN TIMING CONSTANTS.
!     READ EXECUTIVE CONTROL DECK AND SAVE NOGO FLAG.
!     READ CASE CONTROL DECK, SORT BULK DATA AND EXECUTE
!     INPUT FILE PROCESSOR UNLESS BULK DATA IS MISSING.
!     IF CONICAL SHELL PROBLEM, EXECUTE IFP3.
!
   CALL conmsg(bcd5,1,1)
   IF ( Debug1>0 ) WRITE (6,99006) subr(2)
   CALL gnfiat
!
!     CALL THE TIME TEST ROUTINES TO COMPUTE THE NASTRAN
!     TIMING CONSTANTS AND INITIALIZE COMMON /NTIME/
!
!     GENERATE THE I/O TIMES AND
!     CPU TIMES FOR VARIOUS TYPES OF LOOPS
!
   CALL conmsg(bcd6,1,0)
   IF ( Debug1>0 ) WRITE (6,99006) subr(3)
   CALL tmtsio(*100,Debug1)
   CALL conmsg(bcd7,1,0)
   IF ( Debug1>0 ) WRITE (6,99006) subr(4)
   CALL tmtslp
!
!     PROCESS EXECUTIVE CONTROL CARDS
!
 100  CALL conmsg(bcd1,1,1)
   IF ( Debug1>0 ) WRITE (6,99006) subr(5)
   CALL xcsa
!
!     OUTPUT THE COMMON /NTIME/ ENTRIES IF DIAG 35 IS TURNED ON
!
   CALL sswtch(35,l35)
   IF ( l35/=0 ) THEN
      CALL conmsg(bcd8,1,0)
      IF ( Debug1>0 ) WRITE (6,99006) subr(6)
      CALL tmtsot
   ENDIF
!
!     PROCESS SUBSTRUCTURING DMAP
!
   nogox = nogo
   nogo = 0
   IF ( Debug1>0 .AND. isubs/=0 ) WRITE (6,99006) subr(7)
   IF ( isubs/=0 ) CALL asdmap
!
!     PROCESS CASE CONTROL CARDS
!
   CALL conmsg(bcd2,1,1)
   IF ( Debug1>0 ) WRITE (6,99006) subr(8)
   CALL ifp1
   nogo1 = nogo
   IF ( nogo==-9 ) nogo = 1
   IF ( nogo<0 ) nogo = 0
   kaxif = 0
!
!     REVERT TO OLD XSORT TO PROCESS BULKDATA CARDS IF DIAG 42 IS
!     TURNED ON,  OTHERWISE, USE XSORT2 FOR SPEED AND EFFICIENCY
!
   CALL conmsg(bcd3,1,0)
   CALL sswtch(42,l42)
   IF ( Debug1>0 ) WRITE (6,99006) subr(9)
   IF ( l42==1 ) CALL xsort
   IF ( l42==0 ) CALL xsort2
   IF ( nogo/=-2 ) THEN
!
!     INPUT FILE PROCESSOR(S) TO CHECK EACH BULKDATA CARD
!
      IF ( Debug1>0 ) WRITE (6,99006) subr(10)
      CALL ifp
      IF ( Debug1>0 .AND. axic/=0 ) WRITE (6,99006) subr(11)
      IF ( axic/=0 ) CALL ifp3
!
!     SET KAXIF AS IFP4 WILL MODIFY AXIF
!
      kaxif = axif
      IF ( kaxif==1 .OR. kaxif==3 ) CALL ifp4
      IF ( kaxif==2 .OR. kaxif==3 ) CALL ifp5
   ENDIF
!
!     SUPPRESS NOGO FLAG IF USER REQUESTS UNDEFORMED STRUCTURE PLOT VIA
!     NASTRAN PLOTOPT CARD
!
   IF ( nogo==-2 ) nogo = 0
   IF ( nogo==0 .AND. nogo1<0 ) nogo = nogo1
   IF ( nogo>=1 .AND. nogo1<0 ) nogo = -9
   IF ( nogo1==0 ) nogo1 = nogo
!
!     NOGO FLAG CONDITIONS
!     NOGOX.NE. 0, FATAL ERROR IN EXECUTIVE CONTROL
!     NOGO .EQ.-9, FATAL ERROR IN BULKDATA AND IN PLOT COMMANDS
!     NOGO .EQ. 0, NO FATAL ERROR DETECTED IN ENTIRE INPOUT DECK
!     NOGO .GT. 0, FATAL ERROR IN BULKDATA, NO ERROR IN PLOT COMMANDS
!     NOGO .LT. 0, NO ERROR IN BULKDATA, FATAL ERROR IN PLOT COMMANDS
!
   IF ( nogox/=0 ) THEN
      CALL mesage(-61,0,0)
   ELSE
      IF ( nogo<0 ) THEN
         IF ( nogo==-9 .AND. plotf/=3 ) THEN
            CALL mesage(-61,0,0)
            GOTO 300
         ELSEIF ( plotf<=1 ) THEN
            nogo = 1
         ELSE
            nogo = 0
         ENDIF
      ELSEIF ( nogo/=0 ) THEN
         nogo = 1
      ENDIF
!
!     EXECUTE GENERAL PROBLEM INITIALIZATION IF DATA PERMITS.
!
      IF ( nogo/=0 ) CALL mesage(-61,0,0)
      CALL conmsg(bcd4,1,0)
      IF ( Debug1>0 ) WRITE (6,99006) subr(12)
      CALL xgpi
!
!     CALL BANDIT TO GENERATE GRID-POINT RE-SEQUENCE CARDS IF DATA
!     PERMITS
!
      IF ( nogo/=0 .AND. nogo1<0 ) nogo = -9
      IF ( nogo==0 .AND. nogo1/=0 ) nogo = nogo1
      IF ( isy77>=0 .AND. nogo==0 ) THEN
         IF ( axic/=0 .OR. kaxif==1 .OR. kaxif==3 ) THEN
            WRITE (outtap,99001) uim
!
99001       FORMAT (A29,' - GRID-POINT RESEQUENCING PROCESSOR BANDIT IS NOT',' USED DUE TO')
            bcdx = bcd10
            IF ( axic/=0 ) bcdx = bcd9
            WRITE (outtap,99002) bcdx , bcd11
99002       FORMAT (5X,'THE PRESENCE OF AXISYMMETRIC ',A4,A1,' DATA')
            WRITE (outtap,99003)
99003       FORMAT (1H0,10X,'**NO ERRORS FOUND - EXECUTE NASTRAN PROGRAM**')
         ELSE
            IF ( Debug1>0 ) WRITE (6,99006) subr(13)
            CALL bandit
         ENDIF
      ENDIF
!
!     TERMINATE NASTRAN IF LINK 1 ONLY IS REQUESTED BY USER
!
      IF ( isy77==-2 ) CALL pexit
!
!     EXIT ACCORDING TO PLOT OPTION REQUEST
!     SET PLOTF TO NEGATIVE ONLY IF JOB IS TO BE TERMINATED AFTER PLOTS
!     IN LINK2
!
      j = plotf + 1
      IF ( nogo==0 ) THEN
         IF ( j==1 .OR. j==2 .OR. j==5 .OR. j==6 ) GOTO 500
         IF ( j==3 .OR. j==4 ) GOTO 400
      ENDIF
      IF ( nogo>0 ) THEN
         IF ( j==1 .OR. j==2 ) THEN
            IF ( plotf>1 ) WRITE (outtap,99004)
99004       FORMAT ('0*** ATTEMPT TO PLOT UNDEFORMED MODEL IS ABANDONED DUE',' TO FATAL ERROR IN BULK DATA')
            CALL mesage(-61,0,0)
         ELSEIF ( j/=3 .AND. j/=4 .AND. j/=5 .AND. j/=6 ) THEN
            GOTO 150
         ENDIF
         GOTO 300
      ENDIF
 150  IF ( nogo<0 ) THEN
         IF ( j==1 .OR. j==2 .OR. j==3 ) THEN
            CALL mesage(-61,0,0)
         ELSEIF ( j/=4 .AND. j/=5 ) THEN
            GOTO 200
         ENDIF
         GOTO 300
      ENDIF
!                      PLOTF =   0,   1,   2,   3,   4,   5
!
 200  IF ( nogo+9/=0 ) GOTO 500
      CALL mesage(-61,0,0)
   ENDIF
 300  WRITE (outtap,99005) uwm
99005 FORMAT (A25,' - FATAL ERRORS ENCOUNTERED IN USER INPUT DECK,',/5X,'HOWEVER, NASTRAN WILL ATTEMPT TO PLOT THE UNDEFORMED',     &
             &' STRUCTURE AS REQUESTED BY USER')
 400  plotf = -plotf
 500  RETURN
99006 FORMAT (/,' -LINK1 DEBUG- SEMINT CALLING ',A6,' NEXT',/)
!
END SUBROUTINE semint
