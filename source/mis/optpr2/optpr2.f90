!*==optpr2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE optpr2
   USE c_blank
   USE c_gpta1
   USE c_names
   USE c_optpw2
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
         optp1 = 101
         oes1 = 102
         est1 = 103
         optp2 = 201
         est2 = 202
         zcor = 200
         nwdse = 5
         nwdsp = 6
!
!     LOAD /GPTA1/ ON 1108
!
         CALL delset
!
!     STEP 1.  INITIALIZE AND READ POPT DATA
!
         b1 = korsz(core(1)) - sysbuf + 1
         b2 = b1 - sysbuf
         ycor = b2 - 1
         IF ( b2>6 ) THEN
            count = count + 1
            conv = 0.0
            file = optp1
            CALL open(*120,file,parm(b1),nrrew)
            CALL fread(optp1,parm(1),2,0)
            CALL fread(optp1,parm(1),6,1)
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
            IF ( count>max ) GOTO 120
            CALL tmtogo(tg)
            IF ( tg>0 ) THEN
               CALL klock(tl)
               tl = (tl-tstart)/count
               IF ( tg<=tl ) count = max
               iprnt = 0
!
!     STEP 3. READ OPTP1 INTO CORE
!
!     RECORD 1 - POINTERS
!
               ycor = ycor - 7
               IF ( ycor>=ntypes ) THEN
!
!     POINTERS TO OPTIMIZING POINTERS
!
                  CALL fread(optp1,y(1),ntypes,0)
!
!     NUMBER OF ELEMENT TYPES THAT MAY BE OPTIMIZED
!
                  CALL fread(optp1,nelop,1,0)
!
!     ELEMENT AND PROPERTY POINTERS OF (2,NELOP+1) LENGTH
!
                  ycor = ycor - ntypes
                  i = 2*(nelop+1)
                  ptpty = ntypes + 1
                  IF ( ycor>=i ) THEN
                     CALL fread(optp1,y(ptpty),i,1)
!
!     RECORD 2 - ELEMENT DATA
!
                     ycor = ycor - i
                     ptely = ptpty + i
                     IF ( ycor>=nwdse+nwdsp ) CALL read(*20,*40,optp1,y(ptely),ycor,1,nelw)
                  ENDIF
               ENDIF
            ELSE
               CALL mesage(45,count,name)
               count = 0
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     INSUFFICIENT CORE - PRINT START OF EACH SECTION
!
!
 20      CALL page2(-3)
         i = ntypes + 1
         WRITE (outtap,99001) ufm , name , b1 , i , ptpty , ptely , ptpry
99001    FORMAT (A23,' 2289, ',2A4,'INSUFFICIENT CORE (',I10,2H ),/9X,I9,' = MATERIAL',I9,' = POINTERS',I9,' = ELEMENTS',I9,        &
                &' = PROPERTIES')
         CALL close(file,crew)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     RECORD 3 - PROPERTY DATA
!
 40      IF ( nelw<nwdse ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ptpry = ptely + nelw
         ycor = ycor - nelw
         IF ( ycor>=nwdsp ) CALL read(*20,*60,optp1,y(ptpry),ycor,1,nprw)
         GOTO 20
!
!     RECORD 4 - PLIMIT DATA
!
 60      IF ( nprw>=nwdsp ) THEN
            ptply = ptpry + nprw
            ycor = ycor - nprw
            IF ( ycor>=0 ) CALL read(*20,*80,optp1,y(ptply),ycor,1,nklw)
            GOTO 20
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     INSUFFICIENT DATA
!
         CALL close(file,crew)
         CALL page2(-2)
         WRITE (outtap,99002) ufm , name
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
 80      CALL close(file,crew)
         ptrry = ptply + nklw
         ntotl = ptrry + nprw/nwdsp + 1
         iy(ntotl-1) = -1234567
         IF ( ntotl>ycor ) GOTO 20
         ycor = b2 - 1
         DO j = ntotl , ycor
            iy(j) = 0
         ENDDO
!
!     READ STRESS DATA, SET ALPH
!
         file = oes1
         CALL gopen(file,parm(b1),nrrew)
         CALL opt2a(iy(ptpty),y(ptely),iy(ptely),y(ptpry),iy(ptpry),y(ptrry))
         IF ( iy(ntotl-1)/=-1234567 ) THEN
!
            WRITE (outtap,99003) ntotl , ptrry
99003       FORMAT (32H0*** RR DIMENSION ERROR/OPTPR2  ,2I7)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL close(file,crew)
            IF ( count>max ) GOTO 120
!
!     SET NEW PROPERTY, CHECK FOR CONVERGENCE
!
            CALL opt2b(iy(ptpry),y(ptpry),y(ptply),y(ptrry))
!
!     CREATE EST2, PUNCH PROPERTIES IF CONVERGED
!
            print = -1
            IF ( count>=max .OR. count<=1 .OR. conv==2. ) print = 1
            IF ( iprn<0 .AND. mod(count,iabs(iprn))==0 ) print = 1
            IF ( count<=max .AND. count>=0 ) THEN
               IF ( count==1 .OR. count>=max .OR. mod(count,iabs(iprn))==0 .OR. conv==2. ) iprnt = 1
               file = est1
               CALL open(*100,file,parm(b1),nrrew)
               CALL fread(file,none(1),2,1)
               file = est2
               CALL gopen(file,parm(b2),nwrew)
               CALL opt2c(y(ptpty),iy(ptely),iy(ptpry),y(ptpry),y(ptrry))
               CALL close(file,crew)
               CALL close(est1,crew)
            ENDIF
!
!     COPY OPTPR1 TO OPTPR2 - CHANGE RECORD 3
!
            IF ( count>max ) GOTO 120
            CALL open(*100,optp1,parm(b1),nrrew)
            file = optp2
            CALL open(*100,file,parm(b2),nwrew)
            CALL opt2d(iy(ptpry),y(ptpry))
            CALL close(file,crew)
            CALL close(optp1,crew)
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
 120     count = -1
         CALL close(optp1,1)
         spag_nextblock_1 = 4
      CASE (4)
         IF ( conv==2.0 ) count = max
         IF ( count<=0 ) print = 1
         IF ( count==0 ) count = -1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE optpr2
