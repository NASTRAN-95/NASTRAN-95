
SUBROUTINE semint(Debug1)
   IMPLICIT NONE
   INTEGER Axic , Axif , Hicore , Inflag , Insave , Intap , Isubs , Isy70(7) , Isy77 , Mach , Ncds , Nogo , Outtap , Plotf
   REAL Dumm15(15) , Dumm30(30) , Dumm6(6) , Dummy3(3) , Dummy4(4) , Dummy6(6) , Echo(4) , System , T1(2,370)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /ifpx1 / Ncds , T1
   COMMON /machin/ Mach , Dummy4
   COMMON /system/ System , Outtap , Nogo , Intap , Dumm15 , Plotf , Dumm6 , Axic , Dummy3 , Hicore , Dummy6 , Axif , Dumm30 ,      &
                 & Isubs , Isy70 , Isy77
   COMMON /xechox/ Echo
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /xxread/ Inflag , Insave
   INTEGER Debug1
   REAL bcd1 , bcd10 , bcd11 , bcd2 , bcd3 , bcd4 , bcd5 , bcd6 , bcd7 , bcd8 , bcd9 , bcdx
   INTEGER iby , j , kaxif , l35 , l42 , nogo1 , nogox
   CHARACTER*6 subr(13)
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
   Insave = Intap
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
   IF ( Mach==3 .OR. Mach>=5 ) CALL defcor
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
   nogox = Nogo
   Nogo = 0
   IF ( Debug1>0 .AND. Isubs/=0 ) WRITE (6,99006) subr(7)
   IF ( Isubs/=0 ) CALL asdmap
!
!     PROCESS CASE CONTROL CARDS
!
   CALL conmsg(bcd2,1,1)
   IF ( Debug1>0 ) WRITE (6,99006) subr(8)
   CALL ifp1
   nogo1 = Nogo
   IF ( Nogo==-9 ) Nogo = 1
   IF ( Nogo<0 ) Nogo = 0
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
   IF ( Nogo/=-2 ) THEN
!
!     INPUT FILE PROCESSOR(S) TO CHECK EACH BULKDATA CARD
!
      IF ( Debug1>0 ) WRITE (6,99006) subr(10)
      CALL ifp
      IF ( Debug1>0 .AND. Axic/=0 ) WRITE (6,99006) subr(11)
      IF ( Axic/=0 ) CALL ifp3
!
!     SET KAXIF AS IFP4 WILL MODIFY AXIF
!
      kaxif = Axif
      IF ( kaxif==1 .OR. kaxif==3 ) CALL ifp4
      IF ( kaxif==2 .OR. kaxif==3 ) CALL ifp5
   ENDIF
!
!     SUPPRESS NOGO FLAG IF USER REQUESTS UNDEFORMED STRUCTURE PLOT VIA
!     NASTRAN PLOTOPT CARD
!
   IF ( Nogo==-2 ) Nogo = 0
   IF ( Nogo==0 .AND. nogo1<0 ) Nogo = nogo1
   IF ( Nogo>=1 .AND. nogo1<0 ) Nogo = -9
   IF ( nogo1==0 ) nogo1 = Nogo
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
      IF ( Nogo<0 ) THEN
         IF ( Nogo==-9 .AND. Plotf/=3 ) THEN
            CALL mesage(-61,0,0)
            GOTO 300
         ELSEIF ( Plotf<=1 ) THEN
            Nogo = 1
         ELSE
            Nogo = 0
         ENDIF
      ELSEIF ( Nogo/=0 ) THEN
         Nogo = 1
      ENDIF
!
!     EXECUTE GENERAL PROBLEM INITIALIZATION IF DATA PERMITS.
!
      IF ( Nogo/=0 ) CALL mesage(-61,0,0)
      CALL conmsg(bcd4,1,0)
      IF ( Debug1>0 ) WRITE (6,99006) subr(12)
      CALL xgpi
!
!     CALL BANDIT TO GENERATE GRID-POINT RE-SEQUENCE CARDS IF DATA
!     PERMITS
!
      IF ( Nogo/=0 .AND. nogo1<0 ) Nogo = -9
      IF ( Nogo==0 .AND. nogo1/=0 ) Nogo = nogo1
      IF ( Isy77>=0 .AND. Nogo==0 ) THEN
         IF ( Axic/=0 .OR. kaxif==1 .OR. kaxif==3 ) THEN
            WRITE (Outtap,99001) Uim
!
99001       FORMAT (A29,' - GRID-POINT RESEQUENCING PROCESSOR BANDIT IS NOT',' USED DUE TO')
            bcdx = bcd10
            IF ( Axic/=0 ) bcdx = bcd9
            WRITE (Outtap,99002) bcdx , bcd11
99002       FORMAT (5X,'THE PRESENCE OF AXISYMMETRIC ',A4,A1,' DATA')
            WRITE (Outtap,99003)
99003       FORMAT (1H0,10X,'**NO ERRORS FOUND - EXECUTE NASTRAN PROGRAM**')
         ELSE
            IF ( Debug1>0 ) WRITE (6,99006) subr(13)
            CALL bandit
         ENDIF
      ENDIF
!
!     TERMINATE NASTRAN IF LINK 1 ONLY IS REQUESTED BY USER
!
      IF ( Isy77==-2 ) CALL pexit
!
!     EXIT ACCORDING TO PLOT OPTION REQUEST
!     SET PLOTF TO NEGATIVE ONLY IF JOB IS TO BE TERMINATED AFTER PLOTS
!     IN LINK2
!
      j = Plotf + 1
      IF ( Nogo==0 ) THEN
         IF ( j==1 .OR. j==2 .OR. j==5 .OR. j==6 ) GOTO 500
         IF ( j==3 .OR. j==4 ) GOTO 400
      ENDIF
      IF ( Nogo>0 ) THEN
         IF ( j==1 .OR. j==2 ) THEN
            IF ( Plotf>1 ) WRITE (Outtap,99004)
99004       FORMAT ('0*** ATTEMPT TO PLOT UNDEFORMED MODEL IS ABANDONED DUE',' TO FATAL ERROR IN BULK DATA')
            CALL mesage(-61,0,0)
         ELSEIF ( j/=3 .AND. j/=4 .AND. j/=5 .AND. j/=6 ) THEN
            GOTO 150
         ENDIF
         GOTO 300
      ENDIF
 150  IF ( Nogo<0 ) THEN
         IF ( j==1 .OR. j==2 .OR. j==3 ) THEN
            CALL mesage(-61,0,0)
         ELSEIF ( j/=4 .AND. j/=5 ) THEN
            GOTO 200
         ENDIF
         GOTO 300
      ENDIF
!                      PLOTF =   0,   1,   2,   3,   4,   5
!
 200  IF ( Nogo+9/=0 ) GOTO 500
      CALL mesage(-61,0,0)
   ENDIF
 300  WRITE (Outtap,99005) Uwm
99005 FORMAT (A25,' - FATAL ERRORS ENCOUNTERED IN USER INPUT DECK,',/5X,'HOWEVER, NASTRAN WILL ATTEMPT TO PLOT THE UNDEFORMED',     &
             &' STRUCTURE AS REQUESTED BY USER')
 400  Plotf = -Plotf
 500  RETURN
99006 FORMAT (/,' -LINK1 DEBUG- SEMINT CALLING ',A6,' NEXT',/)
!
END SUBROUTINE semint
