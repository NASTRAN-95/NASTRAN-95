
SUBROUTINE qparmd
   IMPLICIT NONE
   REAL Dummy(33) , Vps(1)
   INTEGER Flag , Ibuf , Il(8) , Il1 , Il2 , Il3 , Il4 , Il5 , Il6 , Il7 , Il8 , Ivps(1) , Ksys37 , Nogo , Nout , Op(2)
   DOUBLE PRECISION Inc1(2) , Inc2(2) , Ind1 , Ind2 , Outc(2) , Outd
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Op , Outd , Ind1 , Ind2 , Outc , Inc1 , Inc2 , Flag
   COMMON /ilxxd / Il1 , Il2 , Il3 , Il4 , Il5 , Il6 , Il7 , Il8
   COMMON /system/ Ibuf , Nout , Nogo , Dummy , Ksys37
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /xvps  / Vps
   INTEGER blnk , i , ierr , ifirst , iflag , ilx(8) , iop , irtn3 , irtn4 , irtn6 , j , nam(2) , name(2) , opcode(50)
   DOUBLE PRECISION denom , tempd
   REAL parm , temp(2)
   LOGICAL prt
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
   EQUIVALENCE (Vps(1),Ivps(1)) , (tempd,temp(1)) , (Il,Il1)
   DATA name/4HQPAR , 4HMD  / , ifirst/15/
   DATA opcode/4HADD  , 4HSUB  , 4HMPY  , 4HDIV  , 4HNOP  , 4HSQRT , 4HSIN  , 4HCOS  , 4HABS  , 4HEXP  , 4HTAN  , 4HADDC , 4HSUBC , &
       &4HMPYC , 4HDIVC , 4HCOMP , 4HCSQR , 4HNORM , 4HREAL , 4HPOWE , 4HCONJ , 4HEQ   , 4HGT   , 4HGE   , 4HLT   , 4HLE   ,        &
      & 4HNE   , 4HLOG  , 4HLN   , 4HFIX  , 4HFLOA , 4HERR  , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
   DATA ilx/4H1ST  , 4H2ND  , 4H3RD  , 4H4TH  , 4H5TH  , 4H6TH  , 4H7TH  , 4H8TH /
   DATA parm , nam/4HPARM , 4H/PAR , 3HAMD/ , blnk/4H    /
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
      CALL fndpar(-i,Il(i))
   ENDDO
   IF ( prt ) THEN
      CALL page2(ifirst)
      ifirst = 6
      WRITE (Nout,99001) Uim , Op
99001 FORMAT (A29,' FROM PARAMD MODULE - OP CODE = ',2A4,/5X,'(ALL PARAMD MESSAGES CAN BE SUPPRESSED BY DIAG 37)')
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
99002 FORMAT (22X,'UNRECOGNIZABLE OP CODE = ',A4,' (INPUT ERROR)  ',2A4)
   CALL mesage(-7,0,name)
!
! *******
!     D.P. REAL NUMBER FUNCTIONS
! *******
!
!     ADD
!
 100  Outd = Ind1 + Ind2
   GOTO 3500
!
!     SUBTRACT
!
 200  Outd = Ind1 - Ind2
   GOTO 3500
!
!     MULTIPLY
!
 300  Outd = Ind1*Ind2
   GOTO 3500
!
!     DIVIDE
!
 400  Outd = 0.D+0
   IF ( Ind2/=0.D+0 ) THEN
      Outd = Ind1/Ind2
      GOTO 3500
   ENDIF
 500  WRITE (Nout,99003) nam
99003 FORMAT (5X,'ERROR - DIVIDED BY ZERO  ',2A4)
   ierr = 1
   Flag = +1
   IF ( Il8>0 ) THEN
      Ivps(Il8) = Flag
      i = Il8 - 3
      IF ( prt ) WRITE (Nout,99004) Ivps(i) , Ivps(i+1) , Flag
99004 FORMAT (22X,2A4,2H =,I10,'   (OUTPUT)')
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
 700  IF ( Ind1>=0.D+0 ) THEN
      Outd = dsqrt(Ind1)
!
      ASSIGN 4100 TO irtn3
      GOTO 3600
   ENDIF
 800  WRITE (Nout,99005) nam
99005 FORMAT (5X,'ERROR - OPERATING ON A NEGATIVE NUMBER  ',2A4)
   Outd = 0.D+0
   ierr = 1
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     SINE
!
 900  Outd = dsin(Ind1)
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     COSINE
!
 1000 Outd = dcos(Ind1)
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     ABSOLUTE VALUE
!
 1100 Outd = dabs(Ind1)
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     EXPONENTIAL
!
 1200 Outd = dexp(Ind1)
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     TANGENT
!
 1300 Outd = dtan(Ind1)
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     NORM
!
 1400 Outd = dsqrt(Outc(1)**2+Outc(2)**2)
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
 1500 Outd = Ind1**Ind2
   GOTO 3500
!
!     LOG
!
 1600 IF ( Ind1<0.D+0 ) GOTO 800
   Outd = dlog10(Ind1)
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     NATURAL LOG
!
 1700 IF ( Ind1<0.D+0 ) GOTO 800
   Outd = dlog(Ind1)
   ASSIGN 4100 TO irtn3
   GOTO 3600
!
!     FLOAT
!
 1800 Outd = iflag
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
      WRITE (Nout,99006) nam
99006 FORMAT (5X,'JOB TERMINTATED DUE TO PREVIOUS ERROR(S)  ',2A4,/)
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
 2000 Outc(1) = Inc1(1) + Inc2(1)
   Outc(2) = Inc1(2) + Inc2(2)
   ASSIGN 4000 TO irtn6
   GOTO 3900
!
!     SUBTRACT COMPLEX
!
 2100 Outc(1) = Inc1(1) - Inc2(1)
   Outc(2) = Inc1(2) - Inc2(2)
   ASSIGN 4000 TO irtn6
   GOTO 3900
!
!     MULTIPLY COMPLEX
!
 2200 Outc(1) = Inc1(1)*Inc2(1) - Inc1(2)*Inc2(2)
   Outc(2) = Inc1(1)*Inc2(2) + Inc1(2)*Inc2(1)
   ASSIGN 4000 TO irtn6
   GOTO 3900
!
!     DIVIDE COMPLEX
!
 2300 denom = Inc2(1)**2 + Inc2(2)**2
   IF ( denom==0.D+0 ) THEN
      Outc(1) = 0.D+0
      Outc(2) = 0.D+0
      GOTO 500
   ELSE
      Outc(1) = (Inc1(1)*Inc2(1)+Inc1(2)*Inc2(2))/denom
      Outc(2) = (Inc1(2)*Inc2(1)-Inc1(1)*Inc2(2))/denom
      ASSIGN 4000 TO irtn6
      GOTO 3900
   ENDIF
!
!     COMPLEX
!
 2400 Outc(1) = Ind1
   Outc(2) = Ind2
!
   ASSIGN 3700 TO irtn3
   ASSIGN 4200 TO irtn4
   GOTO 3600
!
!     COMPLEX SQUARE ROOT
!
 2500 Outc(1) = (Inc1(1)**2+Inc1(2)**2)**0.25D0*dcos(0.5D0*datan2(Inc1(2),Inc1(1)))
   Outc(2) = (Inc1(1)**2+Inc1(2)**2)**0.25D0*dsin(0.5D0*datan2(Inc1(2),Inc1(1)))
!
   ASSIGN 4200 TO irtn6
   GOTO 3900
!
!     CONJUGATE
!
 2600 Outc(1) = Inc1(1)
   Outc(2) = -Inc1(2)
   ASSIGN 4200 TO irtn6
   GOTO 3900
!
!     REAL
!
 2700 Ind1 = Outc(1)
   Ind2 = Outc(2)
!
   IF ( prt ) THEN
      i = Il5 - 3
      IF ( Il5<=0 ) WRITE (Nout,99011) ilx(5) , parm , Outc
      IF ( Il5>0 ) WRITE (Nout,99011) Ivps(i) , Ivps(i+1) , Outc
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
      IF ( prt ) WRITE (Nout,99014) Ivps(i) , Ivps(i+1) , Ind1
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
      IF ( prt ) WRITE (Nout,99014) Ivps(j) , Ivps(j+1) , Ind2
   ELSE
      WRITE (Nout,99015) ilx(4) , nam
      ierr = 1
   ENDIF
   GOTO 4400
!
!     EQUAL
!
 2800 IF ( Ind1==Ind2 ) Flag = -1
   GOTO 3800
!
!     GREATER THAN
!
 2900 IF ( Ind1>Ind2 ) Flag = -1
   GOTO 3800
!
!     GREATER THAN OR EQUAL
!
 3000 IF ( Ind1>=Ind2 ) Flag = -1
   GOTO 3800
!
!     LESS THAN
!
 3100 IF ( Ind1<Ind2 ) Flag = -1
   GOTO 3800
!
!     LESS THAN OR EQUAL
!
 3200 IF ( Ind1<=Ind2 ) Flag = -1
   GOTO 3800
!
!     NOT EQUAL
!
 3300 IF ( Ind1/=Ind2 ) Flag = -1
   GOTO 3800
!
!     FIX
!
 3400 Flag = Outd
!
   IF ( prt ) THEN
      i = Il2 - 3
      IF ( Il2<=0 ) WRITE (Nout,99013) ilx(2) , parm , Outd
      IF ( Il2>2 ) WRITE (Nout,99013) Ivps(i) , Ivps(i+1) , Outd
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
      IF ( Il3<=0 ) WRITE (Nout,99013) ilx(3) , parm , Ind1
      IF ( Il3>0 ) WRITE (Nout,99013) Ivps(i) , Ivps(i+1) , Ind1
   ENDIF
   IF ( Il3==0 ) ierr = 1
   GOTO irtn3
 3700 IF ( prt ) THEN
      j = Il4 - 3
      IF ( Il4<=0 ) WRITE (Nout,99013) ilx(4) , parm , Ind2
      IF ( Il4>0 ) WRITE (Nout,99013) Ivps(j) , Ivps(j+1) , Ind2
   ENDIF
   IF ( Il4==0 ) ierr = 1
   GOTO irtn4
!
 3800 ASSIGN 3700 TO irtn3
   ASSIGN 4300 TO irtn4
   GOTO 3600
 3900 IF ( prt ) THEN
      i = Il6 - 3
      IF ( Il6<=0 ) WRITE (Nout,99011) ilx(6) , parm , Inc1
      IF ( Il6>0 ) WRITE (Nout,99011) Ivps(i) , Ivps(i+1) , Inc1
      IF ( Il6==0 ) ierr = 1
   ENDIF
   GOTO irtn6
 4000 IF ( prt ) THEN
      j = Il7 - 3
      IF ( Il7<=0 ) WRITE (Nout,99011) ilx(7) , parm , Inc2
      IF ( Il7>0 ) WRITE (Nout,99011) Ivps(j) , Ivps(j+1) , Inc2
   ENDIF
   IF ( Il7==0 ) ierr = 1
   GOTO 4200
!
!     OUTPUT PARAMETER CHECK
!
!     SECOND PARAMETER - OUTD
!
 4100 IF ( Il2>0 ) THEN
      IF ( ierr/=0 ) THEN
         temp(1) = Vps(Il2)
         temp(2) = Vps(Il2+1)
         Outd = tempd
      ENDIF
      tempd = Outd
      Vps(Il2) = temp(1)
      Vps(Il2+1) = temp(2)
      i = Il2 - 3
      IF ( prt ) WRITE (Nout,99014) Ivps(i) , Ivps(i+1) , Outd
   ELSE
      WRITE (Nout,99015) ilx(2) , nam
      ierr = 1
   ENDIF
   GOTO 4400
!
!     FIFTH PARAMETER - OUTC
!
 4200 IF ( Il5>0 ) THEN
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
      IF ( prt ) WRITE (Nout,99008) Ivps(i) , Ivps(i+1) , Outc
99008 FORMAT (22X,2A4,4H = (,D15.8,1H,,D15.8,')   (OUTPUT)')
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
99009 FORMAT (22X,2A4,2H =,I12,6X,'(OUTPUT)')
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
99011 FORMAT (22X,2A4,4H = (,D15.8,1H,,D15.8,')   (INPUT)')
99012 FORMAT (22X,2A4,2H =,I10,'   (INPUT)')
99013 FORMAT (22X,2A4,3H = ,D15.8,'  (INPUT)')
99014 FORMAT (22X,2A4,3H = ,D15.8,'  (OUTPUT)')
99015 FORMAT (22X,A4,'PARAMETER IS MISSING    (OUTPUT ERROR)  ',2A4)
!
99999 RETURN
END SUBROUTINE qparmd
