!*==qparmr.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE qparmr
   IMPLICIT NONE
   USE C_BLANK
   USE C_ILXXR
   USE C_SYSTEM
   USE C_XMSSG
   USE C_XVPS
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blnk , ifirst
   REAL :: denom
   INTEGER :: i , ierr , iflag , iop , irtn3 , irtn4 , irtn6 , j
   INTEGER , DIMENSION(8) :: il
   INTEGER , DIMENSION(8) , SAVE :: ilx
   INTEGER , DIMENSION(1) :: ivps
   INTEGER , DIMENSION(2) , SAVE :: nam , name
   INTEGER , DIMENSION(50) , SAVE :: opcode
   REAL , SAVE :: parm
   LOGICAL :: prt
   EXTERNAL fndpar , mesage , page2 , pexit , sswtch
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     MODULE PARAMR PERFORMS THE FOLLOW OP ON PARAMETERS IN SINGLE
!     PRECISION
!     (COMPANION MODULE PARAMD AND SUBROUTINE QPARMD)
!
!     DMAP
!     PARAMR  / /C,N,OP/ V,N,OUTR/V,N,IN1R/V,N,IN2R/
!                        V,N,OUTC/V,N,IN1C/V,N,IN2C/V,N,FLAG $
!
!         OP        COMPUTE
!         --        -------------------------------------------
!      BY DEFAULT   FLAG = 0
!      1  ADD       OUTR = IN1R + IN2R
!      2  SUB       OUTR = IN1R - IN2R
!      3  MPY       OUTR = IN1R * IN2R
!      4  DIV       OUTR = IN1R / IN2R (IF IN2R = 0, FLAG IS SET TO +1)
!      5  NOP       RETURN
!      6  SQRT      OUTR = SQRT(IN1R)
!      7  SIN       OUTR = SIN(IN1R) WHERE IN1R IS IN RADIANS
!      8  COS       OUTR = COS(IN1R) WHERE IN1R IS IN RADIANS
!      9  ABS       OUTR = ABS(IN1R)
!     10  EXP       OUTR = EXP(IN1R)
!     11  TAN       OUTR = TAN(IN1R) WHERE IN1R IS IN RADIANS
!     12  ADDC      OUTC = IN1C + IN2C
!     13  SUBC      OUTC = IN1C - IN2C
!     14  MPYC      OUTC = IN1C * IN2C
!     15  DIVC      OUTC = IN1C / IN2C (IF IN2C = 0, FLAG IS SET TO +1)
!     16  COMPLEX   OUTC = (IN1R,IN2R)
!     17  CSQRT     OUTC = CSQRT(IN1C)
!     18  NORM      OUTR = SQRT(OUTC(1)**2 + OUTC(2)**2)
!     19  REAL      IN1R = OUTC(1),   IN2R = OUTC(2)
!     20  POWER     OUTR = IN1R**IN2R
!     21  CONJ      OUTC = CONJG(IN1C)
!     22  EQ        FLAG =-1 IF IN1R COMPARES WITH IN2R
!     23  GT        FLAG =-1 IF IN1R IS GT IN2R
!     24  GE        FLAG =-1 IF IN1R IS GE IN2R
!     25  LT        FLAG =-1 IF IN1R IS LT IN2R
!     26  LE        FLAG =-1 IF IN1R IS LE IN2R
!     27  NE        FLAG =-1 IF IN1R IS NE IN2R
!     28  LOG       OUTR = ALOG10(IN1R)
!     29  LN        OUTR = ALOG(IN1R)
!     30  FIX       FLAG = OUTR
!     31  FLOAT     OUTR = FLOAT(FLAG)
!
!     NEW OP CODE ADDED IN THIS NEW VERSION, 12/1988 -
!
!     32  ERR       IF FLAG IS 0, SYSTEM NOGO FLAG IS SET TO ZERO
!                   IF FLAG IS NON-ZERO, JOB TERMINATED IF ANY PREVIOUS
!                      PARAMR (OR PARAMD) CONTAINS NON-FATAL ERROR(S)
!
   !>>>>EQUIVALENCE (Vps(1),Ivps(1)) , (Il,Il1)
   DATA name/4HQPAR , 4HMR  / , ifirst/15/
   DATA opcode/4HADD  , 4HSUB  , 4HMPY  , 4HDIV  , 4HNOP  , 4HSQRT , 4HSIN  , 4HCOS  , 4HABS  , 4HEXP  , 4HTAN  , 4HADDC , 4HSUBC , &
       &4HMPYC , 4HDIVC , 4HCOMP , 4HCSQR , 4HNORM , 4HREAL , 4HPOWE , 4HCONJ , 4HEQ   , 4HGT   , 4HGE   , 4HLT   , 4HLE   ,        &
      & 4HNE   , 4HLOG  , 4HLN   , 4HFIX  , 4HFLOA , 4HERR  , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
   DATA ilx/4H1ST  , 4H2ND  , 4H3RD  , 4H4TH  , 4H5TH  , 4H6TH  , 4H7TH  , 4H8TH /
   DATA parm , nam/4HPARM , 4H/PAR , 3HAMR/ , blnk/4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     SUPPRESSED ALL INPUT/OUTPUT CHECK MESSAGES IF DIAG 37 IS ON
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
99001       FORMAT (A29,' FROM PARAMR MODULE - OP CODE = ',2A4,/5X,'(ALL PARAMR MESSAGES CAN BE SUPPRESED BY DIAG 37)')
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
99002    FORMAT (22X,'UNRECOGNIZABLE OP CODE = ',A4,'  (INPUT ERROR) ',2A4)
         CALL mesage(-7,0,name)
         spag_nextblock_1 = 2
      CASE (2)
!
! *******
!     REAL NUMBER FUNCTIONS
! *******
!
!     ADD
!
         Outr = In1r + In2r
         spag_nextblock_1 = 36
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
!     SUBTRACT
!
         Outr = In1r - In2r
         spag_nextblock_1 = 36
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!     MULTIPLY
!
         Outr = In1r*In2r
         spag_nextblock_1 = 36
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
!
!     DIVIDE
!
         Outr = 0.0
         IF ( In2r/=0.D0 ) THEN
            Outr = In1r/In2r
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
            WRITE (Nout,99004) ivps(i) , ivps(i+1) , Flag , nam
99004       FORMAT (22X,2A4,2H =,I10,'   (OUTPUT)  ',2A4)
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
         IF ( In1r>=0.0 ) THEN
            Outr = sqrt(In1r)
!
            ASSIGN 60 TO irtn3
            spag_nextblock_1 = 37
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         WRITE (Nout,99005) nam
99005    FORMAT (5X,'ERROR - OPERATING ON A NEGATIVE NUMBER  ',2A4)
         Outr = 0.0
         ierr = 1
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
!
!     SINE
!
         Outr = sin(In1r)
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (11)
!
!     COSINE
!
         Outr = cos(In1r)
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
!
!     ABSOLUTE VALUE
!
         Outr = abs(In1r)
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (13)
!
!     EXPONENTIAL
!
         Outr = exp(In1r)
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (14)
!
!     TANGENT
!
         Outr = tan(In1r)
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (15)
!
!     NORM
!
         Outr = sqrt(Outc(1)**2+Outc(2)**2)
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
         Outr = In1r**In2r
         spag_nextblock_1 = 36
         CYCLE SPAG_DispatchLoop_1
      CASE (17)
!
!     LOG
!
         IF ( In1r<0.0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Outr = alog10(In1r)
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (18)
!
!     NATURAL LOG
!
         IF ( In1r<0.0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Outr = alog(In1r)
         ASSIGN 60 TO irtn3
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (19)
!
!     FLOAT
!
         Outr = iflag
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
            WRITE (Nout,99006)
99006       FORMAT (5X,'JOB TERMINATED DUE TO PREVIOUS ERROR(S)',/)
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
         Outc(1) = In1c(1) + In2c(1)
         Outc(2) = In1c(2) + In2c(2)
         ASSIGN 40 TO irtn6
         spag_nextblock_1 = 39
         CYCLE SPAG_DispatchLoop_1
      CASE (22)
!
!     SUBTRACT COMPLEX
!
         Outc(1) = In1c(1) - In2c(1)
         Outc(2) = In1c(2) - In2c(2)
         ASSIGN 40 TO irtn6
         spag_nextblock_1 = 39
         CYCLE SPAG_DispatchLoop_1
      CASE (23)
!
!     MULTIPLY COMPLEX
!
         Outc(1) = In1c(1)*In2c(1) - In1c(2)*In2c(2)
         Outc(2) = In1c(1)*In2c(2) + In1c(2)*In2c(1)
         ASSIGN 40 TO irtn6
         spag_nextblock_1 = 39
         CYCLE SPAG_DispatchLoop_1
      CASE (24)
!
!     DIVIDE COMPLEX
!
         denom = In2c(1)**2 + In2c(2)**2
         IF ( denom==0.0 ) THEN
            Outc(1) = 0.0
            Outc(2) = 0.0
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSE
            Outc(1) = (In1c(1)*In2c(1)+In1c(2)*In2c(2))/denom
            Outc(2) = (In1c(2)*In2c(1)-In1c(1)*In2c(2))/denom
            ASSIGN 40 TO irtn6
            spag_nextblock_1 = 39
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (25)
!
!     COMPLEX
!
         Outc(1) = In1r
         Outc(2) = In2r
!
         ASSIGN 20 TO irtn3
         ASSIGN 80 TO irtn4
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
      CASE (26)
!
!     COMPLEX SQUARE ROOT
!
         Outc(1) = (In1c(1)**2+In1c(2)**2)**0.25*cos(0.5*atan2(In1c(2),In1c(1)))
         Outc(2) = (In1c(1)**2+In1c(2)**2)**0.25*sin(0.5*atan2(In1c(2),In1c(1)))
!
         ASSIGN 80 TO irtn6
         spag_nextblock_1 = 39
         CYCLE SPAG_DispatchLoop_1
      CASE (27)
!
!     CONJUGATE
!
         Outc(1) = In1c(1)
         Outc(2) = -In1c(2)
         ASSIGN 80 TO irtn6
         spag_nextblock_1 = 39
         CYCLE SPAG_DispatchLoop_1
      CASE (28)
!
!     REAL
!
         In1r = Outc(1)
         In2r = Outc(2)
!
         IF ( prt ) THEN
            i = Il5 - 3
            IF ( Il5<=0 ) WRITE (Nout,99011) ilx(5) , parm , Outc
            IF ( Il5>0 ) WRITE (Nout,99011) ivps(i) , ivps(i+1) , Outc
         ENDIF
         IF ( Il5==0 ) ierr = 1
!
!     THIRD AND FOURTH PARAMETERS - INR1, INR2
!
         IF ( Il3>0 ) THEN
            IF ( ierr==0 ) Vps(Il3) = In1r
            i = Il3 - 3
            IF ( prt ) WRITE (Nout,99014) ivps(i) , ivps(i+1) , Vps(Il3)
         ELSE
            WRITE (Nout,99015) ilx(3) , nam
            ierr = 1
         ENDIF
         IF ( Il4>0 ) THEN
            IF ( ierr==0 ) Vps(Il4) = In2r
            j = Il4 - 3
            IF ( prt ) WRITE (Nout,99014) ivps(j) , ivps(j+1) , Vps(Il4)
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
         IF ( In1r==In2r ) Flag = -1
         spag_nextblock_1 = 38
         CYCLE SPAG_DispatchLoop_1
      CASE (30)
!
!     GREATER THAN
!
         IF ( In1r>In2r ) Flag = -1
         spag_nextblock_1 = 38
         CYCLE SPAG_DispatchLoop_1
      CASE (31)
!
!     GREATER THAN OR EQUAL
!
         IF ( In1r>=In2r ) Flag = -1
         spag_nextblock_1 = 38
         CYCLE SPAG_DispatchLoop_1
      CASE (32)
!
!     LESS THAN
!
         IF ( In1r<In2r ) Flag = -1
         spag_nextblock_1 = 38
         CYCLE SPAG_DispatchLoop_1
      CASE (33)
!
!     LESS THAN OR EQUAL
!
         IF ( In1r<=In2r ) Flag = -1
         spag_nextblock_1 = 38
         CYCLE SPAG_DispatchLoop_1
      CASE (34)
!
!     NOT EQUAL
!
         IF ( In1r/=In2r ) Flag = -1
         spag_nextblock_1 = 38
         CYCLE SPAG_DispatchLoop_1
      CASE (35)
!
!     FIX
!
         Flag = Outr
!
         IF ( prt ) THEN
            i = Il2 - 3
            IF ( Il2<=0 ) WRITE (Nout,99013) ilx(2) , parm , Outr
            IF ( Il2>0 ) WRITE (Nout,99013) ivps(i) , ivps(i+1) , Outr
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
            IF ( Il3<=0 ) WRITE (Nout,99013) ilx(3) , parm , In1r
            IF ( Il3>0 ) WRITE (Nout,99013) ivps(i) , ivps(i+1) , In1r
         ENDIF
         IF ( Il3==0 ) ierr = 1
         GOTO irtn3
 20      IF ( prt ) THEN
            j = Il4 - 3
            IF ( Il4<=0 ) WRITE (Nout,99013) ilx(4) , parm , In2r
            IF ( Il4>0 ) WRITE (Nout,99013) ivps(j) , ivps(j+1) , In2r
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
            IF ( Il6<=0 ) WRITE (Nout,99011) ilx(6) , parm , In1c
            IF ( Il6>0 ) WRITE (Nout,99011) ivps(i) , ivps(i+1) , In1c
         ENDIF
         IF ( Il6==0 ) ierr = 1
         GOTO irtn6
 40      IF ( prt ) THEN
            j = Il7 - 3
            IF ( Il7<=0 ) WRITE (Nout,99011) ilx(7) , parm , In2c
            IF ( Il7>0 ) WRITE (Nout,99011) ivps(j) , ivps(j+1) , In2c
         ENDIF
         IF ( Il7==0 ) ierr = 1
         GOTO 80
!
!     OUTPUT PARAMETER CHECK
!
!     SECOND PARAMETER - OUTR
!
 60      IF ( Il2>0 ) THEN
            IF ( ierr==0 ) Vps(Il2) = Outr
            i = Il2 - 3
            IF ( prt ) WRITE (Nout,99014) ivps(i) , ivps(i+1) , Vps(Il2)
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
            IF ( ierr/=1 ) THEN
               Vps(Il5) = Outc(1)
               Vps(Il5+1) = Outc(2)
            ENDIF
            i = Il5 - 3
            IF ( prt ) WRITE (Nout,99008) ivps(i) , ivps(i+1) , Vps(Il5) , Vps(Il5+1)
99008       FORMAT (22X,2A4,4H = (,E13.6,1H,,E13.6,')   (OUTPUT)')
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
99009       FORMAT (22X,2A4,2H =,I10,6X,'(OUTPUT)')
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
99011 FORMAT (22X,2A4,4H = (,E13.6,1H,,E13.6,')   (INPUT)')
99012 FORMAT (22X,2A4,2H =,I10,'   (INPUT)')
99013 FORMAT (22X,2A4,3H = ,E13.6,'  (INPUT)')
99014 FORMAT (22X,2A4,3H = ,E13.6,'  (OUTPUT)')
99015 FORMAT (22X,A4,'PARAMETER IS MISSING  (OUTPUT ERROR)  ',2A4)
!
END SUBROUTINE qparmr
