!*==optpr1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE optpr1
   USE c_blank
   USE c_gpta1
   USE c_names
   USE c_optpw1
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
         neltyp = numtyp
         DO i = 1 , 21
            IF ( ntypes>ltype ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            itype(i) = dattyp(i)
         ENDDO
         DO i = 1 , ntypes
            dtyp(i) = datdty(i)
         ENDDO
!
!
         zcor = 100
         mpt = 101
         ept = 102
         ect = 103
         dit = 104
         est = 105
         optp1 = 201
         scrth1 = 301
!
!     STEP 1.  INITIALIZE AND CHECK FOR OUTPUT FILE
!
         count = 0
         print = 1
         CALL fname(optp1,fnam)
         IF ( fnam(1)==none(1) .AND. fnam(2)==none(2) ) GOTO 120
!
         b1p1 = korsz(core(1)) - sysbuf
         b2 = b1p1 - sysbuf
         ycor = b2 - 7
         pcor1 = -1
         ecor1 = -1
         prcor1 = -1
         kcor1 = -1
         nwdse = 5
         nwdsp = 6
         npow = neltyp
         CALL delset
!
!     STEP 2.  FIND POPT CARD
!
         CALL preloc(*120,x(b1p1),mpt)
         CALL locate(*100,x(b1p1),poph,i)
         CALL read(*20,*40,mpt,x,7,1,nwds)
!
!     ILLEGAL NUMBER OF WORDS
!
 20      CALL page2(-2)
         WRITE (outtap,99001) sfm , name , nwds , hpop
99001    FORMAT (A25,' 2288, ',2A4,'READ INCORRECT NUMBER WORDS (',I2,2A4,2H).)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
 40      IF ( nwds/=6 ) GOTO 20
!
!     STEP 2A.  PROCESS PLIMIT CARDS ON SCRATCH FILE
!
         IF ( ycor<=11 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nklw = 0
         CALL locate(*60,x(b1p1),plmh,i)
         CALL gopen(scrth1,x(b2),nwrew)
         CALL optpx(dtyp)
         CALL close(scrth1,crew)
 60      CALL close(mpt,crew)
         IF ( nklw>=0 ) THEN
            IF ( count+1==0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     STEP 3.  LOAD MATERIAL DATA
!
            CALL premat(y(1),y(1),x(b1p1),ycor,mcor,mpt,dit)
            pcor1 = mcor + 1
            pcor2 = pcor1 + ntypes
            ecor1 = pcor2 + 2*(npow+1)
            ycor = ycor - ecor1
            IF ( ycor>=(nwdse+nwdsp) ) THEN
!
!     STEP 4.  READ ELEMENTS INTO CORE
!
               CALL gopen(est,x(b2),0)
               CALL optp1a(y(pcor1),y(pcor2),y(ecor1),dtyp)
               CALL close(est,crew)
               IF ( count+1==0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( nelw>0 ) THEN
!
!     STEP 5.  READ IN PROPERTIES IDS, SET V1.  SECOND BUFFER NOT NEEDED
!
                  prcor1 = ecor1 + nelw
                  ycor = ycor - nelw + sysbuf
                  IF ( ycor>=nwdsp ) THEN
                     file = ect
                     CALL preloc(*80,x(b1p1),ect)
                     CALL optp1b(y(pcor1),y(pcor2),y(ecor1),y(prcor1))
                     CALL close(ect,crew)
                     IF ( count+1/=0 ) THEN
                        IF ( nprw<=0 ) THEN
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
!
!     STEP 6.  READ PROPERTY DATA INTO CORE
!
                        kcor1 = prcor1 + nprw
                        ycor = ycor - nprw
!
                        file = ept
                        CALL preloc(*80,x(b1p1),ept)
                        CALL optp1c(y(pcor1),y(pcor2),y(prcor1))
                        CALL close(ept,crew)
                        IF ( count+1==0 ) THEN
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
!
!     STEP 7.  PROCESS PLIMIT CARDS
!
                        IF ( nklw>0 ) THEN
                           IF ( ycor<4 ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           CALL gopen(scrth1,x(b1p1),nrrew)
                           CALL optp1d(y(pcor2),y(prcor1),y(kcor1))
                           CALL close(scrth1,crew)
                           IF ( nklw<0 ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( count+1==0 ) THEN
                              spag_nextblock_1 = 3
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDIF
!
!     STEP 7.  COUNT=0, OUTPUT FILE OPTPR1
!
                        file = optp1
                        CALL open(*80,optp1,x(b1p1),nwrew)
                        CALL write(optp1,fnam,2,0)
                        CALL write(optp1,x(1),6,1)
!
                        CALL write(optp1,y(pcor1),ntypes,0)
                        CALL write(optp1,npow,1,0)
                        CALL write(optp1,y(pcor2),2*(npow+1),1)
                        CALL write(optp1,y(ecor1),nelw,1)
                        CALL write(optp1,y(prcor1),nprw,1)
                        CALL write(optp1,y(kcor1),nklw,1)
                        CALL eof(optp1)
                        j = 0
                        y(j+1) = optp1
                        y(j+2) = 0
                        y(j+3) = nelw
                        y(j+4) = nprw
                        y(j+5) = nklw
                        y(j+6) = 0
                        y(j+7) = ntypes
                        CALL wrttrl(y(1))
                        CALL close(optp1,crew)
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
         WRITE (outtap,99002) ufm , name , b1p1 , pcor1 , ecor1 , prcor1 , kcor1
99002    FORMAT (A23,' 2289, ',2A4,'INSUFFICIENT CORE (',I10,2H ),/9X,I9,' = MATERIAL',I9,' = POINTERS',I9,' = ELEMENTS',I9,        &
                &' = PROPERTIES')
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(-61,ept,name)
!
!    INPUT FILE PURGED - ILLEGALLY
!
 80      CALL mesage(-1,file,name)
!
!    OPTPR1 NOT CREATED
!
 100     CALL close(mpt,crew)
 120     count = -1
         spag_nextblock_1 = 4
      CASE (4)
!
!     OPTPR1 CREATED
!
         CALL klock(tstart)
         RETURN
      CASE (5)
!
!     ERROR MESSAGE
!
         WRITE (outtap,99003) sfm
99003    FORMAT (A25,', DATDTY AND DTYP ARRAYS TOO SMALL')
         CALL mesage(-37,0,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE optpr1
