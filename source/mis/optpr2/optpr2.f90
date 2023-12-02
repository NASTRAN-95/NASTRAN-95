!*==optpr2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE optpr2
   IMPLICIT NONE
   USE C_BLANK
   USE C_GPTA1
   USE C_NAMES
   USE C_OPTPW2
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: b2 , file , i , iprn , iprnt , j , max , tg , tl
   INTEGER , DIMENSION(1) :: iy
   INTEGER , DIMENSION(2) , SAVE :: name , none
   INTEGER , DIMENSION(8) :: parm
   INTEGER , SAVE :: ptely , ptply , ptpry , ptpty , ptrry
   REAL , DIMENSION(1) :: y
   EXTERNAL close , delset , fread , gopen , klock , korsz , mesage , open , opt2a , opt2b , opt2c , opt2d , page2 , read , tmtogo
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE IS THE DRIVER FOR PROPERTY OPTIMIZATION, PHASE 2.
!
!     CALLING SEQUENCE
!
!     OPTPR2  OPTP1,OES1,EST1 / OPTP2,EST2 / V,N,PRINT / V,N,TSTART /
!                                            V,N,COUNT / V,N,CARDNO $
!     WHERE   PRINT  = INPUT/OUTPUT - INTEGER, CALL OFP IF 1, SKIP OFP
!                      IF -1
!             TSTART = INPUT - INTEGER, END TIME AT OPTPR1.
!             COUNT  = INPUT/OUTPUT - INTEGER, ITERATION LOOP COUNTER.
!             CARDNO = INPUT/OUTPUT - INTEGER, PUNCHED CARD COUNT
!
!     LOGICAL         DEBUG
   !>>>>EQUIVALENCE (y(1),iy(1),Parm(8)) , (Core(1),Max,Parm(1)) , (Parm(4),Iprn) , (Parm(7),Iprnt)
   DATA name/4H OPT , 4HPR2 / , none/4H (NO , 4HNE) /
   DATA ptpty , ptely , ptpry , ptply , ptrry/5*0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!     DATA    DEBUG / .FALSE /
!
         Optp1 = 101
         Oes1 = 102
         Est1 = 103
         Optp2 = 201
         Est2 = 202
         Zcor = 200
         Nwdse = 5
         Nwdsp = 6
!
!     LOAD /GPTA1/ ON 1108
!
         CALL delset
!
!     STEP 1.  INITIALIZE AND READ POPT DATA
!
         B1 = korsz(Core(1)) - Sysbuf + 1
         b2 = B1 - Sysbuf
         Ycor = b2 - 1
         IF ( b2>6 ) THEN
            Count = Count + 1
            Conv = 0.0
            file = Optp1
            CALL open(*120,file,parm(B1),Nrrew)
            CALL fread(Optp1,parm(1),2,0)
            CALL fread(Optp1,parm(1),6,1)
!
!     PARM NOW CONTAINS
!
!       1 = MAX  - MAX NUMBER OF ITERATIONS (I)
!       2 = EPS  - CONVERGENCE TEST (R)
!       3 = GAMA - ITERATION FACTOR (R)
!       4 = IPRN - PRINT CONTROL (I)
!     5,6 = KPUN - PUNCH CONTROL (BCD, YES OR NO)
!
!     NEW PROPERTIES ARE CALCULATED BY,
!     PNEW = (PLST*ALPH) / (ALPH + (1-ALPH)GAMA)
!
!     STEP 2. CHECK TIME TO GO
!
            IF ( Count>max ) GOTO 120
            CALL tmtogo(tg)
            IF ( tg>0 ) THEN
               CALL klock(tl)
               tl = (tl-Tstart)/Count
               IF ( tg<=tl ) Count = max
               iprnt = 0
!
!     STEP 3. READ OPTP1 INTO CORE
!
!     RECORD 1 - POINTERS
!
               Ycor = Ycor - 7
               IF ( Ycor>=Ntypes ) THEN
!
!     POINTERS TO OPTIMIZING POINTERS
!
                  CALL fread(Optp1,y(1),Ntypes,0)
!
!     NUMBER OF ELEMENT TYPES THAT MAY BE OPTIMIZED
!
                  CALL fread(Optp1,Nelop,1,0)
!
!     ELEMENT AND PROPERTY POINTERS OF (2,NELOP+1) LENGTH
!
                  Ycor = Ycor - Ntypes
                  i = 2*(Nelop+1)
                  ptpty = Ntypes + 1
                  IF ( Ycor>=i ) THEN
                     CALL fread(Optp1,y(ptpty),i,1)
!
!     RECORD 2 - ELEMENT DATA
!
                     Ycor = Ycor - i
                     ptely = ptpty + i
                     IF ( Ycor>=Nwdse+Nwdsp ) CALL read(*20,*40,Optp1,y(ptely),Ycor,1,Nelw)
                  ENDIF
               ENDIF
            ELSE
               CALL mesage(45,Count,name)
               Count = 0
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     INSUFFICIENT CORE - PRINT START OF EACH SECTION
!
!
 20      CALL page2(-3)
         i = Ntypes + 1
         WRITE (Outtap,99001) Ufm , name , B1 , i , ptpty , ptely , ptpry
99001    FORMAT (A23,' 2289, ',2A4,'INSUFFICIENT CORE (',I10,2H ),/9X,I9,' = MATERIAL',I9,' = POINTERS',I9,' = ELEMENTS',I9,        &
                &' = PROPERTIES')
         CALL close(file,Crew)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     RECORD 3 - PROPERTY DATA
!
 40      IF ( Nelw<Nwdse ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ptpry = ptely + Nelw
         Ycor = Ycor - Nelw
         IF ( Ycor>=Nwdsp ) CALL read(*20,*60,Optp1,y(ptpry),Ycor,1,Nprw)
         GOTO 20
!
!     RECORD 4 - PLIMIT DATA
!
 60      IF ( Nprw>=Nwdsp ) THEN
            ptply = ptpry + Nprw
            Ycor = Ycor - Nprw
            IF ( Ycor>=0 ) CALL read(*20,*80,Optp1,y(ptply),Ycor,1,Nklw)
            GOTO 20
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     INSUFFICIENT DATA
!
         CALL close(file,Crew)
         CALL page2(-2)
         WRITE (Outtap,99002) Ufm , name
99002    FORMAT (A23,' 2302, SUBROUTINE ',2A4,' HAS NO PROPERTY OR ','ELEMENT DATA.')
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     CLOSE OPTP1 FILE.
!     ALLOCATE AN ARRAY WITH STARTING POINTER PTRRY, OF LENGTH EQUALS TO
!     THE NO. OF PROPERTY CARDS (TO BE USED IN OPT2A, 2B, AND 2C)
!     SET VARIABLE NTOTL TO THE TOTAL LENGTH OF WORDS USED IN OPEN CORE
!     RE-ESTABLISH OPEN CORE UPPER LIMIT, YCOR
!
 80      CALL close(file,Crew)
         ptrry = ptply + Nklw
         Ntotl = ptrry + Nprw/Nwdsp + 1
         iy(Ntotl-1) = -1234567
         IF ( Ntotl>Ycor ) GOTO 20
         Ycor = b2 - 1
         DO j = Ntotl , Ycor
            iy(j) = 0
         ENDDO
!
!     READ STRESS DATA, SET ALPH
!
         file = Oes1
         CALL gopen(file,parm(B1),Nrrew)
         CALL opt2a(iy(ptpty),y(ptely),iy(ptely),y(ptpry),iy(ptpry),y(ptrry))
         IF ( iy(Ntotl-1)/=-1234567 ) THEN
!
            WRITE (Outtap,99003) Ntotl , ptrry
99003       FORMAT (32H0*** RR DIMENSION ERROR/OPTPR2  ,2I7)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL close(file,Crew)
            IF ( Count>max ) GOTO 120
!
!     SET NEW PROPERTY, CHECK FOR CONVERGENCE
!
            CALL opt2b(iy(ptpry),y(ptpry),y(ptply),y(ptrry))
!
!     CREATE EST2, PUNCH PROPERTIES IF CONVERGED
!
            Print = -1
            IF ( Count>=max .OR. Count<=1 .OR. Conv==2. ) Print = 1
            IF ( iprn<0 .AND. mod(Count,iabs(iprn))==0 ) Print = 1
            IF ( Count<=max .AND. Count>=0 ) THEN
               IF ( Count==1 .OR. Count>=max .OR. mod(Count,iabs(iprn))==0 .OR. Conv==2. ) iprnt = 1
               file = Est1
               CALL open(*100,file,parm(B1),Nrrew)
               CALL fread(file,none(1),2,1)
               file = Est2
               CALL gopen(file,parm(b2),Nwrew)
               CALL opt2c(y(ptpty),iy(ptely),iy(ptpry),y(ptpry),y(ptrry))
               CALL close(file,Crew)
               CALL close(Est1,Crew)
            ENDIF
!
!     COPY OPTPR1 TO OPTPR2 - CHANGE RECORD 3
!
            IF ( Count>max ) GOTO 120
            CALL open(*100,Optp1,parm(B1),Nrrew)
            file = Optp2
            CALL open(*100,file,parm(b2),Nwrew)
            CALL opt2d(iy(ptpry),y(ptpry))
            CALL close(file,Crew)
            CALL close(Optp1,Crew)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     FILE NOT PRESENT
!
 100     CALL mesage(-1,file,name)
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(-61,b2,name)
 120     Count = -1
         CALL close(Optp1,1)
         spag_nextblock_1 = 4
      CASE (4)
         IF ( Conv==2.0 ) Count = max
         IF ( Count<=0 ) Print = 1
         IF ( Count==0 ) Count = -1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE optpr2
