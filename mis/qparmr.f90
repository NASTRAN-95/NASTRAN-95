
SUBROUTINE qparmr
   IMPLICIT NONE
   REAL Dummy(33) , In1c(2) , In1r , In2c(2) , In2r , Outc(2) , Outr , Vps(1)
   INTEGER Flag , Ibuf , Il(8) , Il1 , Il2 , Il3 , Il4 , Il5 , Il6 , Il7 , Il8 , Ivps(1) , Ksys37 , Nogo , Nout , Op(2)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Op , Outr , In1r , In2r , Outc , In1c , In2c , Flag
   COMMON /ilxxr / Il1 , Il2 , Il3 , Il4 , Il5 , Il6 , Il7 , Il8
   COMMON /system/ Ibuf , Nout , Nogo , Dummy , Ksys37
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /xvps  / Vps
   INTEGER blnk , i , ierr , ifirst , iflag , ilx(8) , iop , irtn3 , irtn4 , irtn6 , j , nam(2) , name(2) , opcode(50)
   REAL denom , parm
   LOGICAL prt
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
   EQUIVALENCE (Vps(1),Ivps(1)) , (Il,Il1)
   DATA name/4HQPAR , 4HMR  / , ifirst/15/
   DATA opcode/4HADD  , 4HSUB  , 4HMPY  , 4HDIV  , 4HNOP  , 4HSQRT , 4HSIN  , 4HCOS  , 4HABS  , 4HEXP  , 4HTAN  , 4HADDC , 4HSUBC , &
       &4HMPYC , 4HDIVC , 4HCOMP , 4HCSQR , 4HNORM , 4HREAL , 4HPOWE , 4HCONJ , 4HEQ   , 4HGT   , 4HGE   , 4HLT   , 4HLE   ,        &
      & 4HNE   , 4HLOG  , 4HLN   , 4HFIX  , 4HFLOA , 4HERR  , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
   DATA ilx/4H1ST  , 4H2ND  , 4H3RD  , 4H4TH  , 4H5TH  , 4H6TH  , 4H7TH  , 4H8TH /
   DATA parm , nam/4HPARM , 4H/PAR , 3HAMR/ , blnk/4H    /
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
      CALL fndpar(-i,Il(i))
   ENDDO
   IF ( prt ) THEN
      CALL page2(ifirst)
      ifirst = 6
      WRITE (Nout,99001) Uim , Op
99001 FORMAT (A29,' FROM PARAMR MODULE - OP CODE = ',2A4,/5X,'(ALL PARAMR MESSAGES CAN BE SUPPRESED BY DIAG 37)')
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
         IF ( iop==1 ) GOTO 100
         IF ( iop==2 ) GOTO 200
         IF ( iop==3 ) GOTO 300
         IF ( iop==4 ) GOTO 400
         IF ( iop==5 ) GOTO 600
         IF ( iop==6 ) GOTO 700
         IF ( iop==7 ) GOTO 900
         IF ( iop==8 ) GOTO 1000
         IF ( iop==9 ) GOTO 1100
         IF ( iop==10 ) GOTO 1200
         IF ( iop==11 ) GOTO 1300
         IF ( iop==12 ) GOTO 2000
         IF ( iop==13 ) GOTO 2100
         IF ( iop==14 ) GOTO 2200
         IF ( iop==15 ) GOTO 2300
         IF ( iop==16 ) GOTO 2400
         IF ( iop==17 ) GOTO 2500
         IF ( iop==18 ) GOTO 1400
         IF ( iop==19 ) GOTO 2700
         IF ( iop==20 ) GOTO 1500
         IF ( iop==21 ) GOTO 2600
         IF ( iop==22 ) GOTO 2800
         IF ( iop==23 ) GOTO 2900
         IF ( iop==24 ) GOTO 3000
         IF ( iop==25 ) GOTO 3100
         IF ( iop==26 ) GOTO 3200
         IF ( iop==27 ) GOTO 3300
         IF ( iop==28 ) GOTO 1600
         IF ( iop==29 ) GOTO 1700
         IF ( iop==30 ) GOTO 3400
         IF ( iop==31 ) GOTO 1800
         IF ( iop==32 ) GOTO 1900
      ENDIF
   ENDDO
   WRITE (Nout,99002) Op(1) , nam
99002 FORMAT (22X,'UNRECOGNIZABLE OP CODE = ',A4,'  (INPUT ERROR) ',2A4)
   CALL mesage(-7,0,name)
!
! *******
!     REAL NUMBER FUNCTIONS
! *******
!
!     ADD
!
 100  Outr = In1r + In2r
   GOTO 3500
!
!     SUBTRACT
!
 200  Outr = In1r - In2r
   GOTO 3500
!
!     MULTIPLY
!
 300  Outr = In1r*In2r
   GOTO 3500
!
!     DIVIDE
!
 400  Outr = 0.0
   IF ( In2r/=0.D0 ) THEN
      Outr = In1r/In2r
      GOTO 3500
   ENDIF
 500  WRITE (Nout,99003) nam
99003 FORMAT (5X,'ERROR - DIVIDED BY ZERO  ',2A4)
   ierr = 1
   Flag = +1
   IF ( Il8>0 ) THEN
      Ivps(Il8) = Flag
      i = Il8 - 3
      WRITE (Nout,99004) Ivps(i) , Ivps(i+1) , Flag , nam
99004 FORMAT (22X,2A4,2H =,I10,'   (OUTPUT)  ',2A4)
   ENDIF
!
   ASSIGN 4000 TO irtn6
   GOTO 3900
!
!     NOP
!
 600  RETURN
!
!     SQUARE ROOT
!
 700  IF ( In1r>=0.0 ) THEN
      Outr = sqrt(In1r)
!
      ASSIGN 4100 TO irtn3
      GOTO 3600
   ENDIF
 800  WRITE (Nout,99005) nam
99005 FORMAT (5X,'ERROR - OPERATING ON A NEGATIVE NUMBER  ',2A4)
   Outr = 0.0
   ierr = 1
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     SINE
!
 900  Outr = sin(In1r)
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     COSINE
!
 1000 Outr = cos(In1r)
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     ABSOLUTE VALUE
!
 1100 Outr = abs(In1r)
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     EXPONENTIAL
!
 1200 Outr = exp(In1r)
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     TANGENT
!
 1300 Outr = tan(In1r)
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     NORM
!
 1400 Outr = sqrt(Outc(1)**2+Outc(2)**2)
!
   IF ( prt ) THEN
      i = Il5 - 3
      IF ( Il5<=0 ) WRITE (Nout,99011) ilx(5) , parm , Outc
      IF ( Il5>0 ) WRITE (Nout,99011) Ivps(i) , Ivps(i+1) , Outc
   ENDIF
   IF ( Il5==0 ) ierr = 1
   GOTO 4100
!
!     POWER
!
 1500 Outr = In1r**In2r
   GOTO 3500
!
!     LOG
!
 1600 IF ( In1r<0.0 ) GOTO 800
   Outr = alog10(In1r)
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     NATURAL LOG
!
 1700 IF ( In1r<0.0 ) GOTO 800
   Outr = alog(In1r)
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     FLOAT
!
 1800 Outr = iflag
!
   IF ( prt ) THEN
      i = Il8 - 3
      IF ( Il8<=0 ) WRITE (Nout,99012) ilx(8) , parm , iflag
      IF ( Il8>0 ) WRITE (Nout,99012) Ivps(i) , Ivps(i+1) , iflag
   ENDIF
   IF ( Il8==0 ) ierr = 1
   GOTO 4100
!
!     ERR
!
 1900 IF ( iflag/=0 .AND. Ksys37/=0 ) THEN
      WRITE (Nout,99006)
99006 FORMAT (5X,'JOB TERMINATED DUE TO PREVIOUS ERROR(S)',/)
      CALL pexit
   ELSE
      Ksys37 = 0
      Nogo = 0
      IF ( prt ) WRITE (Nout,99007)
99007 FORMAT (5X,'SYSTEM NOGO FLAG IS RESET TO INTEGER ZERO',/)
   ENDIF
   IF ( Ksys37==0 ) Ksys37 = ierr
   GOTO 99999
!
! *******
!     COMPLEX FUNCTIONS
! *******
!
!     ADD COMPLEX
!
 2000 Outc(1) = In1c(1) + In2c(1)
   Outc(2) = In1c(2) + In2c(2)
   ASSIGN 4000 TO irtn6
   GOTO 3900
!
!     SUBTRACT COMPLEX
!
 2100 Outc(1) = In1c(1) - In2c(1)
   Outc(2) = In1c(2) - In2c(2)
   ASSIGN 4000 TO irtn6
   GOTO 3900
!
!     MULTIPLY COMPLEX
!
 2200 Outc(1) = In1c(1)*In2c(1) - In1c(2)*In2c(2)
   Outc(2) = In1c(1)*In2c(2) + In1c(2)*In2c(1)
   ASSIGN 4000 TO irtn6
   GOTO 3900
!
!     DIVIDE COMPLEX
!
 2300 denom = In2c(1)**2 + In2c(2)**2
   IF ( denom==0.0 ) THEN
      Outc(1) = 0.0
      Outc(2) = 0.0
      GOTO 500
   ELSE
      Outc(1) = (In1c(1)*In2c(1)+In1c(2)*In2c(2))/denom
      Outc(2) = (In1c(2)*In2c(1)-In1c(1)*In2c(2))/denom
      ASSIGN 4000 TO irtn6
      GOTO 3900
   ENDIF
!
!     COMPLEX
!
 2400 Outc(1) = In1r
   Outc(2) = In2r
!
   ASSIGN 3700 TO irtn3
   ASSIGN 4200 TO irtn4
   GOTO 3600
!
!     COMPLEX SQUARE ROOT
!
 2500 Outc(1) = (In1c(1)**2+In1c(2)**2)**0.25*cos(0.5*atan2(In1c(2),In1c(1)))
   Outc(2) = (In1c(1)**2+In1c(2)**2)**0.25*sin(0.5*atan2(In1c(2),In1c(1)))
!
   ASSIGN 4200 TO irtn6
   GOTO 3900
!
!     CONJUGATE
!
 2600 Outc(1) = In1c(1)
   Outc(2) = -In1c(2)
   ASSIGN 4200 TO irtn6
   GOTO 3900
!
!     REAL
!
 2700 In1r = Outc(1)
   In2r = Outc(2)
!
   IF ( prt ) THEN
      i = Il5 - 3
      IF ( Il5<=0 ) WRITE (Nout,99011) ilx(5) , parm , Outc
      IF ( Il5>0 ) WRITE (Nout,99011) Ivps(i) , Ivps(i+1) , Outc
   ENDIF
   IF ( Il5==0 ) ierr = 1
!
!     THIRD AND FOURTH PARAMETERS - INR1, INR2
!
   IF ( Il3>0 ) THEN
      IF ( ierr==0 ) Vps(Il3) = In1r
      i = Il3 - 3
      IF ( prt ) WRITE (Nout,99014) Ivps(i) , Ivps(i+1) , Vps(Il3)
   ELSE
      WRITE (Nout,99015) ilx(3) , nam
      ierr = 1
   ENDIF
   IF ( Il4>0 ) THEN
      IF ( ierr==0 ) Vps(Il4) = In2r
      j = Il4 - 3
      IF ( prt ) WRITE (Nout,99014) Ivps(j) , Ivps(j+1) , Vps(Il4)
   ELSE
      WRITE (Nout,99015) ilx(4) , nam
      ierr = 1
   ENDIF
   GOTO 4400
!
!     EQUAL
!
 2800 IF ( In1r==In2r ) Flag = -1
   GOTO 3800
!
!     GREATER THAN
!
 2900 IF ( In1r>In2r ) Flag = -1
   GOTO 3800
!
!     GREATER THAN OR EQUAL
!
 3000 IF ( In1r>=In2r ) Flag = -1
   GOTO 3800
!
!     LESS THAN
!
 3100 IF ( In1r<In2r ) Flag = -1
   GOTO 3800
!
!     LESS THAN OR EQUAL
!
 3200 IF ( In1r<=In2r ) Flag = -1
   GOTO 3800
!
!     NOT EQUAL
!
 3300 IF ( In1r/=In2r ) Flag = -1
   GOTO 3800
!
!     FIX
!
 3400 Flag = Outr
!
   IF ( prt ) THEN
      i = Il2 - 3
      IF ( Il2<=0 ) WRITE (Nout,99013) ilx(2) , parm , Outr
      IF ( Il2>0 ) WRITE (Nout,99013) Ivps(i) , Ivps(i+1) , Outr
   ENDIF
   IF ( Il2==0 ) ierr = 1
   GOTO 4300
!
! ---------------------------------------------------
!
!     INPUT PARAMETER ECHO
!
 3500 ASSIGN 3700 TO irtn3
   ASSIGN 4100 TO irtn4
 3600 IF ( prt ) THEN
      i = Il3 - 3
      IF ( Il3<=0 ) WRITE (Nout,99013) ilx(3) , parm , In1r
      IF ( Il3>0 ) WRITE (Nout,99013) Ivps(i) , Ivps(i+1) , In1r
   ENDIF
   IF ( Il3==0 ) ierr = 1
   GOTO irtn3
 3700 IF ( prt ) THEN
      j = Il4 - 3
      IF ( Il4<=0 ) WRITE (Nout,99013) ilx(4) , parm , In2r
      IF ( Il4>0 ) WRITE (Nout,99013) Ivps(j) , Ivps(j+1) , In2r
   ENDIF
   IF ( Il4==0 ) ierr = 1
   GOTO irtn4
!
 3800 ASSIGN 3700 TO irtn3
   ASSIGN 4300 TO irtn4
   GOTO 3600
 3900 IF ( prt ) THEN
      i = Il6 - 3
      IF ( Il6<=0 ) WRITE (Nout,99011) ilx(6) , parm , In1c
      IF ( Il6>0 ) WRITE (Nout,99011) Ivps(i) , Ivps(i+1) , In1c
   ENDIF
   IF ( Il6==0 ) ierr = 1
   GOTO irtn6
 4000 IF ( prt ) THEN
      j = Il7 - 3
      IF ( Il7<=0 ) WRITE (Nout,99011) ilx(7) , parm , In2c
      IF ( Il7>0 ) WRITE (Nout,99011) Ivps(j) , Ivps(j+1) , In2c
   ENDIF
   IF ( Il7==0 ) ierr = 1
   GOTO 4200
!
!     OUTPUT PARAMETER CHECK
!
!     SECOND PARAMETER - OUTR
!
 4100 IF ( Il2>0 ) THEN
      IF ( ierr==0 ) Vps(Il2) = Outr
      i = Il2 - 3
      IF ( prt ) WRITE (Nout,99014) Ivps(i) , Ivps(i+1) , Vps(Il2)
   ELSE
      WRITE (Nout,99015) ilx(2) , nam
      ierr = 1
   ENDIF
   GOTO 4400
!
!     FIFTH PARAMETER - OUTC
!
 4200 IF ( Il5>0 ) THEN
      IF ( ierr/=1 ) THEN
         Vps(Il5) = Outc(1)
         Vps(Il5+1) = Outc(2)
      ENDIF
      i = Il5 - 3
      IF ( prt ) WRITE (Nout,99008) Ivps(i) , Ivps(i+1) , Vps(Il5) , Vps(Il5+1)
99008 FORMAT (22X,2A4,4H = (,E13.6,1H,,E13.6,')   (OUTPUT)')
   ELSE
      WRITE (Nout,99015) ilx(5) , nam
      ierr = 1
   ENDIF
   GOTO 4400
!
!     EIGHTH PARAMETER - FLAG
!
 4300 IF ( Il8>0 ) THEN
      IF ( ierr==0 ) Ivps(Il8) = Flag
      i = Il8 - 3
      IF ( prt ) WRITE (Nout,99009) Ivps(i) , Ivps(i+1) , Ivps(Il8)
99009 FORMAT (22X,2A4,2H =,I10,6X,'(OUTPUT)')
   ELSE
      WRITE (Nout,99015) ilx(8) , nam
      ierr = 1
   ENDIF
!
 4400 IF ( ierr/=0 ) THEN
      WRITE (Nout,99010) Uwm , nam
99010 FORMAT (A25,' - I/O ERROR, OUTPUT NOT SAVED. OUTPUT DEFAULT ','VALUE REMAINS ',2A4,/)
   ENDIF
   IF ( Ksys37==0 ) Ksys37 = ierr
99011 FORMAT (22X,2A4,4H = (,E13.6,1H,,E13.6,')   (INPUT)')
99012 FORMAT (22X,2A4,2H =,I10,'   (INPUT)')
99013 FORMAT (22X,2A4,3H = ,E13.6,'  (INPUT)')
99014 FORMAT (22X,2A4,3H = ,E13.6,'  (OUTPUT)')
99015 FORMAT (22X,A4,'PARAMETER IS MISSING  (OUTPUT ERROR)  ',2A4)
!
99999 RETURN
END SUBROUTINE qparmr
