!*==qparmd.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE qparmd
USE C_BLANK
USE C_ILXXD
USE C_SYSTEM
USE C_XMSSG
USE C_XVPS
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blnk , ifirst
   REAL(REAL64) :: denom , tempd
   INTEGER :: i , ierr , iflag , iop , irtn3 , irtn4 , irtn6 , j
   INTEGER , DIMENSION(8) :: il
   INTEGER , DIMENSION(8) , SAVE :: ilx
   INTEGER , DIMENSION(1) :: ivps
   INTEGER , DIMENSION(2) , SAVE :: nam , name
   INTEGER , DIMENSION(50) , SAVE :: opcode
   REAL , SAVE :: parm
   LOGICAL :: prt
   REAL , DIMENSION(2) :: temp
   EXTERNAL fndpar , mesage , page2 , pexit , sswtch
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     MODULE PARAMD PERFORMS THE FOLLOW OP ON PARAMETERS IN DOUBLE
!     PRECISION
!     (REFERENCE - MODULE PARAMR AND SUBROUTINE QPARMR)
!
!     DMAP
!     PARAMD  / /C,N,OP/ V,N,OUTD/V,N,IND1/V,N,IND2/
!                        V,N,OUTC/V,N,INC1/V,N,INC2/V,N,FLAG $
!
!         OP        COMPUTE
!         --        -------------------------------------------
!      BY DEFAULT   FLAG = 0
!      1  ADD       OUTD = IND1 + IND2
!      2  SUB       OUTD = IND1 - IND2
!      3  MPY       OUTD = IND1 * IND2
!      4  DIV       OUTD = IND1 / IND2 (IF IND2 = 0, FLAG IS SET TO +1)
!      5  NOP       RETURN
!      6  SQRT      OUTD = DSQRT(IND1)
!      7  SIN       OUTD = DSIN(IND1) WHERE IND1 IS IN RADIANS
!      8  COS       OUTD = DCOS(IND1) WHERE IND1 IS IN RADIANS
!      9  ABS       OUTD = DABS(IND1)
!     10  EXP       OUTD = DEXP(IND1)
!     11  TAN       OUTD = DTAN(IND1) WHERE IND1 IS IN RADIANS
!     12  ADDC      OUTC = INC1 + INC2
!     13  SUBC      OUTC = INC1 - INC2
!     14  MPYC      OUTC = INC1 * INC2
!     15  DIVC      OUTC = INC1 / INC2 (IF INC2 = 0, FLAG IS SET TO +1)
!     16  COMPLEX   OUTC = (IND1,IND2)
!     17  CSQRT     OUTC = DCSQRT(INC1)
!     18  NORM      OUTD = DSQRT(OUTC(1)**2 + OUTC(2)**2)
!     19  REAL      IND1 = OUTC(1),  IND2 = OUTC(2)
!     20  POWER     OUTD = IND1**IND2
!     21  CONJ      OUTC = DCONJG(INC1)
!     22  EQ        FLAG =-1 IF IND1 COMPARES WITH IND2
!     23  GT        FLAG =-1 IF IND1 IS GT IND2
!     24  GE        FLAG =-1 IF IND1 IS GE IND2
!     25  LT        FLAG =-1 IF IND1 IS LT IND2
!     26  LE        FLAG =-1 IF IND1 IS LE IND2
!     27  NE        FLAG =-1 IF IND1 IS NE IND2
!     28  LOG       OUTD = DLOG10(IND1)
!     29  LN        OUTD = DLOG(IND1)
!     30  FIX       FLAG = OUTD
!     31  FLOAT     OUTD = FLOAT(FLAG)
!
!     NEW OP CODE ADDED IN THIS NEW VERSION, 12/1988 -
!
!     32  ERR       IF FLAG IS 0, SYSTEM NOGO FLAG IS SET TO ZERO
!                   IF FLAG IS NON-ZERO, JOB TERMINATED IF ANY PREVIOUS
!                      PARAMD (OR PARAMR) CONTAINS NON-FATAL ERROR(S)
!
   !>>>>EQUIVALENCE (Vps(1),Ivps(1)) , (tempd,temp(1)) , (Il,Il1)
   DATA name/4HQPAR , 4HMD  / , ifirst/15/
   DATA opcode/4HADD  , 4HSUB  , 4HMPY  , 4HDIV  , 4HNOP  , 4HSQRT , 4HSIN  , 4HCOS  , 4HABS  , 4HEXP  , 4HTAN  , 4HADDC , 4HSUBC , &
       &4HMPYC , 4HDIVC , 4HCOMP , 4HCSQR , 4HNORM , 4HREAL , 4HPOWE , 4HCONJ , 4HEQ   , 4HGT   , 4HGE   , 4HLT   , 4HLE   ,        &
      & 4HNE   , 4HLOG  , 4HLN   , 4HFIX  , 4HFLOA , 4HERR  , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
   DATA ilx/4H1ST  , 4H2ND  , 4H3RD  , 4H4TH  , 4H5TH  , 4H6TH  , 4H7TH  , 4H8TH /
   DATA parm , nam/4HPARM , 4H/PAR , 3HAMD/ , blnk/4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     SUPPRESS ALL PARAMETER CHECK MESSAGES IF DIAG 37 IS ON
!
         CALL sswtch(37,i)
         prt = i==0
         IF ( prt ) nam(1) = blnk
         IF ( prt ) nam(2) = blnk
!
!     COMPUTE VPS INDEXES AND PARAMETER NAMES
!
         DO i = 2 , 8
            CALL fndpar(-i,il(i))
         ENDDO
         IF ( prt ) THEN
            CALL page2(ifirst)
            ifirst = 6
            WRITE (Nout,99001) Uim , Op
99001       FORMAT (A29,' FROM PARAMD MODULE - OP CODE = ',2A4,/5X,'(ALL PARAMD MESSAGES CAN BE SUPPRESSED BY DIAG 37)')
         ENDIF
!
!     BRANCH ON OPERATION CODE
!
         iflag = Flag
         Flag = 0
         ierr = 0
!
         DO iop = 1 , 32
            IF ( Op(1)==opcode(iop) ) THEN
               IF ( iop==1 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==2 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==3 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==4 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==5 ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==6 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==7 ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==8 ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==9 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==10 ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==11 ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==12 ) THEN
                  spag_nextblock_1 = 21
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==13 ) THEN
                  spag_nextblock_1 = 22
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==14 ) THEN
                  spag_nextblock_1 = 23
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==15 ) THEN
                  spag_nextblock_1 = 24
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==16 ) THEN
                  spag_nextblock_1 = 25
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==17 ) THEN
                  spag_nextblock_1 = 26
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==18 ) THEN
                  spag_nextblock_1 = 15
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==19 ) THEN
                  spag_nextblock_1 = 28
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==20 ) THEN
                  spag_nextblock_1 = 16
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==21 ) THEN
                  spag_nextblock_1 = 27
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==22 ) THEN
                  spag_nextblock_1 = 29
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==23 ) THEN
                  spag_nextblock_1 = 30
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==24 ) THEN
                  spag_nextblock_1 = 31
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==25 ) THEN
                  spag_nextblock_1 = 32
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==26 ) THEN
                  spag_nextblock_1 = 33
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==27 ) THEN
                  spag_nextblock_1 = 34
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==28 ) THEN
                  spag_nextblock_1 = 17
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==29 ) THEN
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==30 ) THEN
                  spag_nextblock_1 = 35
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==31 ) THEN
                  spag_nextblock_1 = 19
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( iop==32 ) THEN
                  spag_nextblock_1 = 20
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
         WRITE (Nout,99002) Op(1) , nam
99002    FORMAT (22X,'UNRECOGNIZABLE OP CODE = ',A4,' (INPUT ERROR)  ',2A4)
         CALL mesage(-7,0,name)
         spag_nextblock_1 = 2
      CASE (2)
!
! *******
!     D.P. REAL NUMBER FUNCTIONS
! *******
!
!     ADD
!
         Outd = Ind1 + Ind2
         spag_nextblock_1 = 36
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
!     SUBTRACT
!
         Outd = Ind1 - Ind2
         spag_nextblock_1 = 36
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!     MULTIPLY
!
         Outd = Ind1*Ind2
         spag_nextblock_1 = 36
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
!
!     DIVIDE
!
         Outd = 0.D+0
         IF ( Ind2/=0.D+0 ) THEN
            Outd = Ind1/Ind2
            spag_nextblock_1 = 36
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         WRITE (Nout,99003) nam
99003    FORMAT (5X,'ERROR - DIVIDED BY ZERO  ',2A4)
         ierr = 1
         Flag = +1
         IF ( Il8>0 ) THEN
            ivps(Il8) = Flag
            i = Il8 - 3
            IF ( prt ) WRITE (Nout,99004) ivps(i) , ivps(i+1) , Flag
99004       FORMAT (22X,2A4,2H =,I10,'   (OUTPUT)')
         ENDIF
!
         ASSIGN 40 TO irtn6
         spag_nextblock_1 = 39
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
!
!     NOP
!
         RETURN
      CASE (8)
!
!     SQUARE ROOT
!
         IF ( Ind1>=0.D+0 ) THEN
            Outd = dsqrt(Ind1)
!
            ASSIGN 60 TO irtn3
            spag_nextblock_1 = 37
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         WRITE (Nout,99005) nam
99005    FORMAT (5X,'ERROR - OPERATING ON A NEGATIVE NUMBER  ',2A4)
         Outd = 0.D+0
         ierr = 1
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
!
!     SINE
!
         Outd = dsin(Ind1)
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (11)
!
!     COSINE
!
         Outd = dcos(Ind1)
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
!
!     ABSOLUTE VALUE
!
         Outd = dabs(Ind1)
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (13)
!
!     EXPONENTIAL
!
         Outd = dexp(Ind1)
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (14)
!
!     TANGENT
!
         Outd = dtan(Ind1)
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (15)
!
!     NORM
!
         Outd = dsqrt(Outc(1)**2+Outc(2)**2)
!
         IF ( prt ) THEN
            i = Il5 - 3
            IF ( Il5<=0 ) WRITE (Nout,99011) ilx(5) , parm , Outc
            IF ( Il5>0 ) WRITE (Nout,99011) ivps(i) , ivps(i+1) , Outc
         ENDIF
         IF ( Il5==0 ) ierr = 1
         GOTO 60
      CASE (16)
!
!     POWER
!
         Outd = Ind1**Ind2
         spag_nextblock_1 = 36
         CYCLE SPAG_DispatchLoop_1
      CASE (17)
!
!     LOG
!
         IF ( Ind1<0.D+0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Outd = dlog10(Ind1)
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (18)
!
!     NATURAL LOG
!
         IF ( Ind1<0.D+0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Outd = dlog(Ind1)
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (19)
!
!     FLOAT
!
         Outd = iflag
!
         IF ( prt ) THEN
            i = Il8 - 3
            IF ( Il8<=0 ) WRITE (Nout,99012) ilx(8) , parm , iflag
            IF ( Il8>0 ) WRITE (Nout,99012) ivps(i) , ivps(i+1) , iflag
         ENDIF
         IF ( Il8==0 ) ierr = 1
         GOTO 60
      CASE (20)
!
!     ERR
!
         IF ( iflag/=0 .AND. Ksys37/=0 ) THEN
            WRITE (Nout,99006) nam
99006       FORMAT (5X,'JOB TERMINTATED DUE TO PREVIOUS ERROR(S)  ',2A4,/)
            CALL pexit
         ELSE
            Ksys37 = 0
            Nogo = 0
            IF ( prt ) WRITE (Nout,99007)
99007       FORMAT (5X,'SYSTEM NOGO FLAG IS RESET TO INTEGER ZERO',/)
         ENDIF
         IF ( Ksys37==0 ) Ksys37 = ierr
         RETURN
      CASE (21)
!
! *******
!     COMPLEX FUNCTIONS
! *******
!
!     ADD COMPLEX
!
         Outc(1) = Inc1(1) + Inc2(1)
         Outc(2) = Inc1(2) + Inc2(2)
         ASSIGN 40 TO irtn6
         spag_nextblock_1 = 39
         CYCLE SPAG_DispatchLoop_1
      CASE (22)
!
!     SUBTRACT COMPLEX
!
         Outc(1) = Inc1(1) - Inc2(1)
         Outc(2) = Inc1(2) - Inc2(2)
         ASSIGN 40 TO irtn6
         spag_nextblock_1 = 39
         CYCLE SPAG_DispatchLoop_1
      CASE (23)
!
!     MULTIPLY COMPLEX
!
         Outc(1) = Inc1(1)*Inc2(1) - Inc1(2)*Inc2(2)
         Outc(2) = Inc1(1)*Inc2(2) + Inc1(2)*Inc2(1)
         ASSIGN 40 TO irtn6
         spag_nextblock_1 = 39
         CYCLE SPAG_DispatchLoop_1
      CASE (24)
!
!     DIVIDE COMPLEX
!
         denom = Inc2(1)**2 + Inc2(2)**2
         IF ( denom==0.D+0 ) THEN
            Outc(1) = 0.D+0
            Outc(2) = 0.D+0
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSE
            Outc(1) = (Inc1(1)*Inc2(1)+Inc1(2)*Inc2(2))/denom
            Outc(2) = (Inc1(2)*Inc2(1)-Inc1(1)*Inc2(2))/denom
            ASSIGN 40 TO irtn6
            spag_nextblock_1 = 39
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (25)
!
!     COMPLEX
!
         Outc(1) = Ind1
         Outc(2) = Ind2
!
         ASSIGN 20 TO irtn3
         ASSIGN 80 TO irtn4
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (26)
!
!     COMPLEX SQUARE ROOT
!
         Outc(1) = (Inc1(1)**2+Inc1(2)**2)**0.25D0*dcos(0.5D0*datan2(Inc1(2),Inc1(1)))
         Outc(2) = (Inc1(1)**2+Inc1(2)**2)**0.25D0*dsin(0.5D0*datan2(Inc1(2),Inc1(1)))
!
         ASSIGN 80 TO irtn6
         spag_nextblock_1 = 39
         CYCLE SPAG_DispatchLoop_1
      CASE (27)
!
!     CONJUGATE
!
         Outc(1) = Inc1(1)
         Outc(2) = -Inc1(2)
         ASSIGN 80 TO irtn6
         spag_nextblock_1 = 39
         CYCLE SPAG_DispatchLoop_1
      CASE (28)
!
!     REAL
!
         Ind1 = Outc(1)
         Ind2 = Outc(2)
!
         IF ( prt ) THEN
            i = Il5 - 3
            IF ( Il5<=0 ) WRITE (Nout,99011) ilx(5) , parm , Outc
            IF ( Il5>0 ) WRITE (Nout,99011) ivps(i) , ivps(i+1) , Outc
         ENDIF
         IF ( Il5==0 ) ierr = 1
!
!     THIRD AND FOURTH PARAMETERS - IND1, IND2
!
         IF ( Il3>0 ) THEN
            IF ( ierr/=0 ) THEN
               temp(1) = Vps(Il3)
               temp(2) = Vps(Il3+1)
               Ind1 = tempd
            ENDIF
            tempd = Ind1
            Vps(Il3) = temp(1)
            Vps(Il3+1) = temp(2)
            i = Il3 - 3
            IF ( prt ) WRITE (Nout,99014) ivps(i) , ivps(i+1) , Ind1
         ELSE
            WRITE (Nout,99015) ilx(3) , nam
            ierr = 1
         ENDIF
         IF ( Il4>0 ) THEN
            IF ( ierr/=0 ) THEN
               temp(1) = Vps(Il4)
               temp(2) = Vps(Il4+1)
               Ind2 = tempd
            ENDIF
            tempd = Ind2
            Vps(Il4) = temp(1)
            Vps(Il4+1) = temp(2)
            j = Il4 - 3
            IF ( prt ) WRITE (Nout,99014) ivps(j) , ivps(j+1) , Ind2
         ELSE
            WRITE (Nout,99015) ilx(4) , nam
            ierr = 1
         ENDIF
         spag_nextblock_1 = 40
         CYCLE SPAG_DispatchLoop_1
      CASE (29)
!
!     EQUAL
!
         IF ( Ind1==Ind2 ) Flag = -1
         spag_nextblock_1 = 38
         CYCLE SPAG_DispatchLoop_1
      CASE (30)
!
!     GREATER THAN
!
         IF ( Ind1>Ind2 ) Flag = -1
         spag_nextblock_1 = 38
         CYCLE SPAG_DispatchLoop_1
      CASE (31)
!
!     GREATER THAN OR EQUAL
!
         IF ( Ind1>=Ind2 ) Flag = -1
         spag_nextblock_1 = 38
         CYCLE SPAG_DispatchLoop_1
      CASE (32)
!
!     LESS THAN
!
         IF ( Ind1<Ind2 ) Flag = -1
         spag_nextblock_1 = 38
         CYCLE SPAG_DispatchLoop_1
      CASE (33)
!
!     LESS THAN OR EQUAL
!
         IF ( Ind1<=Ind2 ) Flag = -1
         spag_nextblock_1 = 38
         CYCLE SPAG_DispatchLoop_1
      CASE (34)
!
!     NOT EQUAL
!
         IF ( Ind1/=Ind2 ) Flag = -1
         spag_nextblock_1 = 38
         CYCLE SPAG_DispatchLoop_1
      CASE (35)
!
!     FIX
!
         Flag = Outd
!
         IF ( prt ) THEN
            i = Il2 - 3
            IF ( Il2<=0 ) WRITE (Nout,99013) ilx(2) , parm , Outd
            IF ( Il2>2 ) WRITE (Nout,99013) ivps(i) , ivps(i+1) , Outd
         ENDIF
         IF ( Il2==0 ) ierr = 1
         GOTO 100
      CASE (36)
!
! ---------------------------------------------------
!
!     INPUT PARAMETER ECHO
!
         ASSIGN 20 TO irtn3
         ASSIGN 60 TO irtn4
         spag_nextblock_1 = 37
      CASE (37)
         IF ( prt ) THEN
            i = Il3 - 3
            IF ( Il3<=0 ) WRITE (Nout,99013) ilx(3) , parm , Ind1
            IF ( Il3>0 ) WRITE (Nout,99013) ivps(i) , ivps(i+1) , Ind1
         ENDIF
         IF ( Il3==0 ) ierr = 1
         GOTO irtn3
 20      IF ( prt ) THEN
            j = Il4 - 3
            IF ( Il4<=0 ) WRITE (Nout,99013) ilx(4) , parm , Ind2
            IF ( Il4>0 ) WRITE (Nout,99013) ivps(j) , ivps(j+1) , Ind2
         ENDIF
         IF ( Il4==0 ) ierr = 1
         GOTO irtn4
      CASE (38)
!
         ASSIGN 20 TO irtn3
         ASSIGN 100 TO irtn4
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (39)
         IF ( prt ) THEN
            i = Il6 - 3
            IF ( Il6<=0 ) WRITE (Nout,99011) ilx(6) , parm , Inc1
            IF ( Il6>0 ) WRITE (Nout,99011) ivps(i) , ivps(i+1) , Inc1
            IF ( Il6==0 ) ierr = 1
         ENDIF
         GOTO irtn6
 40      IF ( prt ) THEN
            j = Il7 - 3
            IF ( Il7<=0 ) WRITE (Nout,99011) ilx(7) , parm , Inc2
            IF ( Il7>0 ) WRITE (Nout,99011) ivps(j) , ivps(j+1) , Inc2
         ENDIF
         IF ( Il7==0 ) ierr = 1
         GOTO 80
!
!     OUTPUT PARAMETER CHECK
!
!     SECOND PARAMETER - OUTD
!
 60      IF ( Il2>0 ) THEN
            IF ( ierr/=0 ) THEN
               temp(1) = Vps(Il2)
               temp(2) = Vps(Il2+1)
               Outd = tempd
            ENDIF
            tempd = Outd
            Vps(Il2) = temp(1)
            Vps(Il2+1) = temp(2)
            i = Il2 - 3
            IF ( prt ) WRITE (Nout,99014) ivps(i) , ivps(i+1) , Outd
         ELSE
            WRITE (Nout,99015) ilx(2) , nam
            ierr = 1
         ENDIF
         spag_nextblock_1 = 40
         CYCLE SPAG_DispatchLoop_1
!
!     FIFTH PARAMETER - OUTC
!
 80      IF ( Il5>0 ) THEN
            IF ( ierr/=0 ) THEN
               temp(1) = Vps(Il5)
               temp(2) = Vps(Il5+1)
               Outc(1) = tempd
               temp(1) = Vps(Il5+2)
               temp(2) = Vps(Il5+3)
               Outc(2) = tempd
            ENDIF
            tempd = Outc(1)
            Vps(Il5) = temp(1)
            Vps(Il5+1) = temp(2)
            tempd = Outc(2)
            Vps(Il5+2) = temp(1)
            Vps(Il5+3) = temp(2)
            i = Il5 - 3
            IF ( prt ) WRITE (Nout,99008) ivps(i) , ivps(i+1) , Outc
99008       FORMAT (22X,2A4,4H = (,D15.8,1H,,D15.8,')   (OUTPUT)')
         ELSE
            WRITE (Nout,99015) ilx(5) , nam
            ierr = 1
         ENDIF
         spag_nextblock_1 = 40
         CYCLE SPAG_DispatchLoop_1
!
!     EIGHTH PARAMETER - FLAG
!
 100     IF ( Il8>0 ) THEN
            IF ( ierr==0 ) ivps(Il8) = Flag
            i = Il8 - 3
            IF ( prt ) WRITE (Nout,99009) ivps(i) , ivps(i+1) , ivps(Il8)
99009       FORMAT (22X,2A4,2H =,I12,6X,'(OUTPUT)')
         ELSE
            WRITE (Nout,99015) ilx(8) , nam
            ierr = 1
         ENDIF
         spag_nextblock_1 = 40
      CASE (40)
!
         IF ( ierr/=0 ) THEN
            WRITE (Nout,99010) Uwm , nam
99010       FORMAT (A25,' - I/O ERROR, OUTPUT NOT SAVED. OUTPUT DEFAULT ','VALUE REMAINS ',2A4,/)
         ENDIF
         IF ( Ksys37==0 ) Ksys37 = ierr
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99011 FORMAT (22X,2A4,4H = (,D15.8,1H,,D15.8,')   (INPUT)')
99012 FORMAT (22X,2A4,2H =,I10,'   (INPUT)')
99013 FORMAT (22X,2A4,3H = ,D15.8,'  (INPUT)')
99014 FORMAT (22X,2A4,3H = ,D15.8,'  (OUTPUT)')
99015 FORMAT (22X,A4,'PARAMETER IS MISSING    (OUTPUT ERROR)  ',2A4)
!
END SUBROUTINE qparmd
