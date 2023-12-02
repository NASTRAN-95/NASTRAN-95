!*==optpr1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE optpr1
   IMPLICIT NONE
   USE C_BLANK
   USE C_GPTA1
   USE C_NAMES
   USE C_OPTPW1
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: b2 , ecor1 , file , i , j , kcor1 , mcor , nwds , pcor1 , pcor2 , prcor1
   INTEGER , DIMENSION(90) , SAVE :: datdty
   INTEGER , DIMENSION(21) , SAVE :: dattyp
   INTEGER , DIMENSION(90) :: dtyp
   INTEGER , DIMENSION(2) :: fnam
   INTEGER , DIMENSION(2) , SAVE :: hpop , name , none , plmh , poph
   INTEGER , SAVE :: ltype , numtyp
   REAL , DIMENSION(7) :: x
   INTEGER , DIMENSION(1) :: y
   EXTERNAL close , delset , eof , fname , gopen , klock , korsz , locate , mesage , open , optp1a , optp1b , optp1c , optp1d ,     &
          & optpx , page2 , preloc , premat , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE IS THE DRIVER FOR PROPERTY OPTIMIZATION, PHASE 1.
!
!
!     OPTPR1  MPT,EPT,ECT,DIT,EST/OPTP1/V,N,PRINT/V,N,TSTART/
!                                       V,N,COUNT $
!
!     WHERE PRINT  = OUTPUT, INTEGER = 1
!           TSTART = OUTPUT, INTEGER = TIME AT EXIT OF OPTPR1.
!           COUNT  = OUTPUT, INTEGER =-1 NOT PROPERTY OPTIMIZATION.
!                                    = 1 IS  PROPERTY OPTIMIZATION.
!     CRITERIA FOR OPTIMIZATION
!
!        1. OUTPUT FILE NOT PURGED.
!        2. BULK DATA CARD -POPT IS PRESENT.
!           AFTER THESE TESTS ALL ERRORS ARE FATAL.
!
!
!      SUBROUTINES USED
!
!      OPTP1A - READS ELEMENT DATA INTO CORE (NWDSE PER ELEMENT).
!      OPTP1B - READS PROPERTY IDS INTO CORE AND SETS ELEMENT DATA
!               POINTER (V1) TO ITS LOCATION. (NWDSP PER PROPERTY).
!      OPTP1C - READS DESIGN PROPERTIES INTO CORE.
!      OPTP1D - READS PLIMIT DATA INTO CORE AND SETS PROPERTY DATA
!               POINTER (PLIM) TO ITS LOCATION. (NWDSK PER LIMIT)
!
!
!     LOGICAL         DEBUG
   !>>>>EQUIVALENCE (X(1),Core(1)) , (X(7),Y(1))
!     DATA    DEBUG / .FALSE. /
   DATA poph , plmh/404 , 4 , 304 , 3/ , name/4H OPT , 3HPR1/ , hpop/4H   P , 4HOPT / , none/4H (NO , 4HNE) / , ltype/90/ ,         &
      & numtyp/20/
!
!     NELTYP      = NO. ELEMENT TYPES THAT MAY BE OPTIMIZED
!     LTYPE       = DIMENSION OF DATDTY AND DTYP
!     DATTYP/DTYP = ARRAY TO GIVE RELATIVE LOCATIONS OF ELEMENTS IN
!                   /GPTA1/
!
!             BR  EB  IS  QM  M1  M2  QP  Q1  Q2  RD  SH  TB  T1  T2
   DATA dattyp/34 , 81 , 80 , 16 , 62 , 63 , 15 , 19 , 18 , 1 , 4 , 7 , 6 , 17 , 73 , 9 , 8 , 3 , 64 , 83 , 0/
!             T6  TM  TP  TU  Q4  T3
!
!     SETUP DATDYP/DTYP IN ALPHABETICAL ORDER AND IN /GPTA1/ SEQUENCE
!
!             ELEMENT   RD  2  TU  SH   5  T1  TB  TP  TM  10
!             ELEMENT   11-14  QP  QM  T2  Q2  Q1  20
!             ELEMENT   21-30
!             ELEMENT   31-33  BR   35-40
!             ELEMENT   41-50
!             ELEMENT   51-60
!             ELEMENT   61 M1  M2   Q4  65-70
!             ELEMENT   71-72  T6  74-79  D8
   DATA datdty/10 , 0 , 18 , 11 , 0 , 13 , 12 , 17 , 16 , 0 , 4*0 , 7 , 4 , 14 , 9 , 8 , 0 , 10*0 , 3*0 , 1 , 6*0 , 10*0 , 10*0 ,   &
      & 0 , 5 , 6 , 19 , 6*0 , 2*0 , 15 , 6*0 , 3 , 2 , 0 , 20 , 7*0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!             ELEMENT   EB 82  T3  84-90
!
!     SET UP ELEMENT TYPES
!
         Neltyp = numtyp
         DO i = 1 , 21
            IF ( Ntypes>ltype ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Itype(i) = dattyp(i)
         ENDDO
         DO i = 1 , Ntypes
            dtyp(i) = datdty(i)
         ENDDO
!
!
         Zcor = 100
         Mpt = 101
         Ept = 102
         Ect = 103
         Dit = 104
         Est = 105
         Optp1 = 201
         Scrth1 = 301
!
!     STEP 1.  INITIALIZE AND CHECK FOR OUTPUT FILE
!
         Count = 0
         Print = 1
         CALL fname(Optp1,fnam)
         IF ( fnam(1)==none(1) .AND. fnam(2)==none(2) ) GOTO 120
!
         B1p1 = korsz(Core(1)) - Sysbuf
         b2 = B1p1 - Sysbuf
         Ycor = b2 - 7
         pcor1 = -1
         ecor1 = -1
         prcor1 = -1
         kcor1 = -1
         Nwdse = 5
         Nwdsp = 6
         Npow = Neltyp
         CALL delset
!
!     STEP 2.  FIND POPT CARD
!
         CALL preloc(*120,x(B1p1),Mpt)
         CALL locate(*100,x(B1p1),poph,i)
         CALL read(*20,*40,Mpt,x,7,1,nwds)
!
!     ILLEGAL NUMBER OF WORDS
!
 20      CALL page2(-2)
         WRITE (Outtap,99001) Sfm , name , nwds , hpop
99001    FORMAT (A25,' 2288, ',2A4,'READ INCORRECT NUMBER WORDS (',I2,2A4,2H).)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
 40      IF ( nwds/=6 ) GOTO 20
!
!     STEP 2A.  PROCESS PLIMIT CARDS ON SCRATCH FILE
!
         IF ( Ycor<=11 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Nklw = 0
         CALL locate(*60,x(B1p1),plmh,i)
         CALL gopen(Scrth1,x(b2),Nwrew)
         CALL optpx(dtyp)
         CALL close(Scrth1,Crew)
 60      CALL close(Mpt,Crew)
         IF ( Nklw>=0 ) THEN
            IF ( Count+1==0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     STEP 3.  LOAD MATERIAL DATA
!
            CALL premat(y(1),y(1),x(B1p1),Ycor,mcor,Mpt,Dit)
            pcor1 = mcor + 1
            pcor2 = pcor1 + Ntypes
            ecor1 = pcor2 + 2*(Npow+1)
            Ycor = Ycor - ecor1
            IF ( Ycor>=(Nwdse+Nwdsp) ) THEN
!
!     STEP 4.  READ ELEMENTS INTO CORE
!
               CALL gopen(Est,x(b2),0)
               CALL optp1a(y(pcor1),y(pcor2),y(ecor1),dtyp)
               CALL close(Est,Crew)
               IF ( Count+1==0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Nelw>0 ) THEN
!
!     STEP 5.  READ IN PROPERTIES IDS, SET V1.  SECOND BUFFER NOT NEEDED
!
                  prcor1 = ecor1 + Nelw
                  Ycor = Ycor - Nelw + Sysbuf
                  IF ( Ycor>=Nwdsp ) THEN
                     file = Ect
                     CALL preloc(*80,x(B1p1),Ect)
                     CALL optp1b(y(pcor1),y(pcor2),y(ecor1),y(prcor1))
                     CALL close(Ect,Crew)
                     IF ( Count+1/=0 ) THEN
                        IF ( Nprw<=0 ) THEN
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
!
!     STEP 6.  READ PROPERTY DATA INTO CORE
!
                        kcor1 = prcor1 + Nprw
                        Ycor = Ycor - Nprw
!
                        file = Ept
                        CALL preloc(*80,x(B1p1),Ept)
                        CALL optp1c(y(pcor1),y(pcor2),y(prcor1))
                        CALL close(Ept,Crew)
                        IF ( Count+1==0 ) THEN
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
!
!     STEP 7.  PROCESS PLIMIT CARDS
!
                        IF ( Nklw>0 ) THEN
                           IF ( Ycor<4 ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           CALL gopen(Scrth1,x(B1p1),Nrrew)
                           CALL optp1d(y(pcor2),y(prcor1),y(kcor1))
                           CALL close(Scrth1,Crew)
                           IF ( Nklw<0 ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( Count+1==0 ) THEN
                              spag_nextblock_1 = 3
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDIF
!
!     STEP 7.  COUNT=0, OUTPUT FILE OPTPR1
!
                        file = Optp1
                        CALL open(*80,Optp1,x(B1p1),Nwrew)
                        CALL write(Optp1,fnam,2,0)
                        CALL write(Optp1,x(1),6,1)
!
                        CALL write(Optp1,y(pcor1),Ntypes,0)
                        CALL write(Optp1,Npow,1,0)
                        CALL write(Optp1,y(pcor2),2*(Npow+1),1)
                        CALL write(Optp1,y(ecor1),Nelw,1)
                        CALL write(Optp1,y(prcor1),Nprw,1)
                        CALL write(Optp1,y(kcor1),Nklw,1)
                        CALL eof(Optp1)
                        j = 0
                        y(j+1) = Optp1
                        y(j+2) = 0
                        y(j+3) = Nelw
                        y(j+4) = Nprw
                        y(j+5) = Nklw
                        y(j+6) = 0
                        y(j+7) = Ntypes
                        CALL wrttrl(y(1))
                        CALL close(Optp1,Crew)
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     ERROR MESSAGES - FILE NOT CREATED
!
!     INSUFFICIENT CORE
!
         CALL page2(-3)
         WRITE (Outtap,99002) Ufm , name , B1p1 , pcor1 , ecor1 , prcor1 , kcor1
99002    FORMAT (A23,' 2289, ',2A4,'INSUFFICIENT CORE (',I10,2H ),/9X,I9,' = MATERIAL',I9,' = POINTERS',I9,' = ELEMENTS',I9,        &
                &' = PROPERTIES')
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(-61,Ept,name)
!
!    INPUT FILE PURGED - ILLEGALLY
!
 80      CALL mesage(-1,file,name)
!
!    OPTPR1 NOT CREATED
!
 100     CALL close(Mpt,Crew)
 120     Count = -1
         spag_nextblock_1 = 4
      CASE (4)
!
!     OPTPR1 CREATED
!
         CALL klock(Tstart)
         RETURN
      CASE (5)
!
!     ERROR MESSAGE
!
         WRITE (Outtap,99003) Sfm
99003    FORMAT (A25,', DATDTY AND DTYP ARRAYS TOO SMALL')
         CALL mesage(-37,0,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE optpr1
