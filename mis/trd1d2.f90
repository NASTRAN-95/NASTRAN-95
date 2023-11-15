
SUBROUTINE trd1d2
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Deltat , Sigma , Tabs , Tim , Z(1)
   INTEGER Dit , Icore , Icount , Ifrst , Ii , Iloop , Incr , Iout , Ip , Ipnl(7) , Ist , It1 , It2 , Iu , Iu1 , Iz(1) , Lcore ,    &
         & Mach , Modal , Nlft , Nlftp , Nmodes , Nout , Nrow , Nstep , Pnl , Sysbuf
   DOUBLE PRECISION Dz(1)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /machin/ Mach
   COMMON /packx / It1 , It2 , Ii , Nrow , Incr
   COMMON /system/ Sysbuf , Iout
   COMMON /trdd1 / Nlft , Dit , Nlftp , Nout , Icount , Iloop , Modal , Lcore , Icore , Iu , Ip , Ipnl , Nmodes , Nstep , Pnl ,     &
                 & Ist , Iu1 , Deltat , Ifrst , Tabs , Sigma , Tim
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Dz
!
! Local variable declarations
!
   REAL aa , ab , alpha , alphb , eta , etb , fab , fabsq , fxsp , fysp , tavga , tavgb , xh , xk , xsp
   LOGICAL dec
   INTEGER file , i , ialg , ibuf1 , icards , icrq , iflag , in1 , in2 , in3 , ip1 , ipx , itabl , itid1 , itid2 , itlist(13) ,     &
         & izl , j , k , kk , kount , l , m , mm , n , name(2) , ncards , nmtd(2) , nn , ntabl , numtb , nxx
   DOUBLE PRECISION fx , fy , h , x , y
   INTEGER numtyp
!
! End of declarations
!
!
!     THIS ROUTINE COMPUTES NON-LINEAR LOADS FOR TRANSIENT ANALYSIS
!
!     THIS ROUTINE IS SUITABLE FOR DOUBLE PRECISION OPERATION
!
   EQUIVALENCE (Z(1),Iz(1),Dz(1))
   DATA itlist/4 , 1105 , 11 , 1 , 1205 , 12 , 2 , 1305 , 13 , 3 , 1405 , 14 , 4/
   DATA name/4HNLFT , 4HTRDD/
   DATA nmtd/4HTRD1 , 4HD2  /
   DATA kount/0/
!
!     IDENTIFICATION OF VARIABLES
!
!     NLFT    NON-LINEAR FUNCTION TABLE
!     PNL     NON-LINEAR FORCES --MATRIX
!     DIT     DIRECT INPUT TABLES
!     NLFTP   NON-LINEAR FUNCTION SET SELECTION
!     NOUT    OUT PUT  EVERY NOUT TIME STEPS( PLUS 1 AND NSTEP)
!     ICOUNT  CURRENT INTERATION COUNTER
!     ILOOP   LOOP ON NUMBER OF TIME STEP CHANGES
!     MODAL   LESS THAN ZERO IMPLIES THIS IS A DIRECT FORMULATION
!     LCORE   AMOUNT OF CORE FOR TRD1D
!     ICORE   POINTER TO FIRST CELL OF OPEN CORE
!     IU      POINTER TO LATEST DISPLACEMENT VECTOR
!     IU1     POINTER TO DISPLACEMENT VECTOR -- ONE TIME STEP BACK
!     IP      POINTER TO LOAD VECTOR
!     NMODES  NUMBER OF MODES IN PROBLEM
!     NSTEP   NUMBER OF TIME STEPS
!     ITLIST  LIST OF CARD TYPES FOR DYNAMIC TABLES
!     NROW    SIZE OF SOLUTION SET
!     IBUF1   POINTER TO BUFFER
!     NCARDS  NUMBER OF LOAD CARDS IN SELECTED SET
!     ICARDS  POINTER  TO FIRST CARD
!     NTABL   NUMBER OF TABLES
!     ITABL   POINTER TO FIRST TABLE
!     IPNL    MATRIX CONTROL BLOCK FOR PNL
!
!     DESCRIPTION OF TYPES OF NON-LINEAR LOADING
!
!     TYPE    DESCRIPTION
!     ----    -----------
!
!       1     DISPLACEMENT-DEPENDENT NOLIN1 LOAD
!       2     DISPLACEMENT-DEPENDENT/DISPLACEMENT-DEPENDENT NOLIN2 LOAD
!       3     DISPLACEMENT-DEPENDENT NOLIN3 LOAD
!       4     DISPLACEMENT-DEPENDENT NOLIN4 LOAD
!       5     VELOCITY-DEPENDENT NOLIN1 LOAD
!       6     VELOCITY-DEPENDENT/DISPLACEMENT-DEPENDENT NOLIN2 LOAD
!       7     VELOCITY-DEPENDENT NOLIN3 LOAD
!       8     VELOCITY-DEPENDENT NOLIN4 LOAD
!       9     VELOCITY-DEPENDENT/VELOCITY-DEPENDENT NOLIN2 LOAD
!      10     DISPLACEMENT-DEPENDENT/VELOCITY-DEPENDENT NOLIN2 LOAD
!      11     TEMPERATURE-DEPENDENT CONVECTION NON-LINEAR LOAD (FTUBE)
!      12     TEMPERATURE-DEPENDENT EMISSIVITIES-ABSORPTIVITIES, NOLIN5
!      13     DISPLACEMENT-DEPENDENT/VELOCITY-DEPENDENT NOLIN6 LOAD
!      14     VELOCITY-DEPENDENT/DISPLACEMENT-DEPENDENT NOLIN6 LOAD
!
!     DETERMINE ENTRY NUMBER
!
   dec = Mach==5 .OR. Mach==6 .OR. Mach==21
   ipx = Ip
!
   IF ( (Iloop==1 .AND. Icount>1) .OR. (Iloop>1 .AND. Icount>0) ) GOTO 500
   IF ( Ifrst/=0 ) GOTO 500
!
!     FIRST TIME FOR TIME STEP
!
   CALL sswtch(10,ialg)
   ibuf1 = Lcore + Icore - Sysbuf
   file = Nlft
   Lcore = Lcore - Sysbuf - 1
   icrq = -Lcore
   IF ( Lcore>0 ) THEN
      CALL open(*1200,Nlft,Iz(ibuf1),0)
!
!     FIND SELECTED SET ID
!
      CALL read(*1400,*100,Nlft,Iz(Icore+1),Lcore,0,iflag)
      icrq = Lcore
   ENDIF
   GOTO 1500
 100  DO i = 3 , iflag
      k = i + Icore
      IF ( Iz(k)==Nlftp ) GOTO 200
   ENDDO
   CALL mesage(-31,Nlftp,name)
!
!     FOUND SET ID -- POSITION TO RECORD IN NLFT
!
 200  k = i - 3
   IF ( k/=0 ) THEN
      DO i = 1 , k
         CALL fwdrec(*1400,Nlft)
      ENDDO
   ENDIF
!
!     BRING IN  8 WORDS PER CARD
!     FORMAT =    TYPE,SILD,SILE,A,SILD,SILE,A OR SILD,SILE
!     CONVERT TO  TYPE,ROWP,ROWP,A,ROWP OR A
!     COUNT NUMBER OF CARDS
!
   ncards = 0
   icards = Icore + 1
   k = icards
   DO
      icrq = 8 - Lcore
      IF ( icrq>0 ) GOTO 1500
      CALL read(*1400,*300,Nlft,Iz(k),8,0,iflag)
      IF ( Modal>=0 ) THEN
!
!     MODAL FORM -- CONVERT SILE TO ROW POSITIONS AND STORE IN SILD
!
         IF ( Iz(k+2)==0 ) THEN
!
!     LOADED POINT  NOT E-POINT IN MODAL FORMULATION
!
            CALL mesage(-44,Nlftp,Iz(k))
            GOTO 99999
         ELSE
            Iz(k+1) = Iz(k+2) + Nmodes
            IF ( Iz(k+5)==0 ) THEN
               CALL mesage(-44,Nlftp,Iz(k))
               GOTO 99999
            ELSE
               Iz(k+4) = Iz(k+5) + Nmodes
               IF ( Iz(k)==2 .OR. Iz(k)==6 .OR. Iz(k)==9 .OR. Iz(k)==10 ) THEN
                  IF ( Iz(k+7)==0 ) THEN
                     CALL mesage(-44,Nlftp,Iz(k))
                     GOTO 99999
                  ELSE
                     Iz(k+6) = Iz(k+7) + Nmodes
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
!
!     MOVE UP
!
      Iz(k+2) = Iz(k+4)
      Iz(k+4) = Iz(k+6)
      k = k + 5
      Lcore = Lcore - 5
      ncards = ncards + 1
   ENDDO
!
!     END OF RECORD-- DONE
!
 300  CALL close(Nlft,1)
!
!     EXTRACT LIST OF  UNIQUE TABLES FROM CARD TYPES 1,5,11 AND 14
!
   l = icards
   ntabl = 0
   itabl = k
   DO i = 1 , ncards
      izl = Iz(l)
      IF ( izl/=1 .AND. izl/=5 .AND. (izl<11 .OR. izl>14) ) GOTO 400
      IF ( izl/=11 .AND. izl/=12 ) THEN
         itid1 = Iz(l+4)
         numtb = 1
      ELSE
         izl = Iz(l+4)
         IF ( Iz(l)/=11 ) THEN
!
!     NOLIN5 CARD
!
            nxx = numtyp(Iz(l+3))
            IF ( dec .AND. Iz(l+3)>16000 .AND. Iz(l+3)<=99999999 ) nxx = 1
            IF ( nxx==1 ) THEN
               itid1 = Iz(l+3)
               nxx = numtyp(izl)
               IF ( dec .AND. izl>16000 .AND. izl<=99999999 ) nxx = 1
               IF ( nxx/=1 ) THEN
                  numtb = 1
               ELSE
                  itid2 = Iz(l+4)
                  numtb = 2
               ENDIF
               GOTO 350
            ENDIF
         ENDIF
!
!     NFTUBE CARD
!
         nxx = numtyp(izl)
         IF ( dec .AND. izl>16000 .AND. izl<=99999999 ) nxx = 1
         IF ( nxx/=1 ) GOTO 400
         itid1 = Iz(l+4)
         numtb = 1
      ENDIF
!
!     FIND OUT IF UNIQUE TABLE
!
 350  IF ( ntabl/=0 ) THEN
         DO m = 1 , ntabl
            k = itabl + m
            IF ( Iz(k)==itid1 ) GOTO 400
         ENDDO
      ENDIF
!
!     NEW TABLE
!
      ntabl = ntabl + 1
      k = itabl + ntabl
      Iz(k) = itid1
 400  IF ( numtb==1 ) THEN
         l = l + 5
      ELSE
         numtb = 1
         itid1 = itid2
         GOTO 350
      ENDIF
   ENDDO
!
   Iz(itabl) = ntabl
   Lcore = Lcore - ntabl - 1
   icrq = -Lcore
   IF ( Lcore<=0 ) GOTO 1500
   IF ( ntabl/=0 ) THEN
!
!     INITIALIZE TABLES
!
      k = itabl + ntabl + 1
      CALL pretab(Dit,Iz(k),Iz(k),Iz(ibuf1),Lcore,l,Iz(itabl),itlist)
      Lcore = Lcore - l
      IF ( ialg/=0 ) THEN
         in1 = (k+l)/2
         in2 = in1 + Nrow
         in3 = in2 + Nrow
         Lcore = Lcore - 6*Nrow
         icrq = -Lcore
         IF ( Lcore<0 ) GOTO 1500
!
!     ZERO LOAD VECTORS
!
         DO i = 1 , Nrow
            k = in1 + i
            Dz(k) = 0.0D0
            k = in2 + i
            Dz(k) = 0.0D0
            k = in3 + i
            Dz(k) = 0.0D0
         ENDDO
      ENDIF
   ENDIF
   RETURN
!
!     COMPUTE LOADS
!
 500  k = icards + ncards*5 - 1
   IF ( ialg/=0 ) THEN
      ipx = in1
      DO i = 1 , Nrow
         l = in1 + i
         Dz(l) = 0.0D0
      ENDDO
   ENDIF
!
!     LOOP THRU EACH LOAD CARD OR COLLECTION (NOLIN5, NOLIN6)
!
   h = 1.0D0/Deltat
   i = icards
 600  fx = 0.0D0
   fy = 1.0D0
   m = Iu + Iz(i+2)
   mm = Iu + Iz(i+4)
   n = Iu1 + Iz(i+2)
   nn = Iu1 + Iz(i+4)
   x = Dz(m)
   y = (x-Dz(n))*h
   l = Iz(i)
!     L  =     1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14
   IF ( l==2 ) GOTO 700
   IF ( l==3 ) GOTO 800
   IF ( l==4 ) GOTO 900
   IF ( l==5 ) THEN
      x = y
   ELSEIF ( l==6 ) THEN
      x = y
      GOTO 700
   ELSEIF ( l==7 ) THEN
      x = y
      GOTO 800
   ELSEIF ( l==8 ) THEN
      x = y
      GOTO 900
   ELSEIF ( l==9 ) THEN
      x = y
      fx = x*(Dz(mm)-Dz(nn))*h
      GOTO 1000
   ELSEIF ( l==10 ) THEN
      fx = x*(Dz(mm)-Dz(nn))*h
      GOTO 1000
   ELSEIF ( l==11 ) THEN
!
!     NFTUBE.  LOOKUP VDOT IF NEEDED
!
      fxsp = Z(i+4)
      izl = Iz(i+4)
      nxx = numtyp(izl)
      IF ( dec .AND. izl>16000 .AND. izl<=99999999 ) nxx = 1
      IF ( nxx==1 ) CALL tab(Iz(i+4),Tim,fxsp)
      IF ( fxsp>=0.0 ) m = Iu + Iz(i+1)
      fx = fxsp*Dz(m)
      l = ipx + Iz(i+2)
      Dz(l) = Dz(l) + fx*Z(i+3)
      fy = -1.0D0
      GOTO 1000
   ELSEIF ( l==12 ) THEN
!
!     NOLIN5
!
!     A. COMPUTE SURFACE AVERAGE TEMPERATURES
!
      mm = 0
      nn = 0
      tavga = 0.0
      tavgb = 0.0
      j = 1
      DO l = 1 , 4
         IF ( l==3 ) j = 6
         m = Iz(i+j)
         IF ( m/=0 ) THEN
            m = Iu + m
            tavga = tavga + Z(m)
            mm = mm + 1
         ENDIF
         m = Iz(i+j+10)
         IF ( m/=0 ) THEN
            m = Iu + m
            tavgb = tavgb + Z(m)
            nn = nn + 1
         ENDIF
         j = j + 1
      ENDDO
      tavga = tavga/float(mm)
      tavgb = tavgb/float(nn)
      aa = Z(i+3)
      ab = Z(i+4)
      fab = Z(i+8)
      fabsq = fab*fab
      eta = Z(i+13)
      etb = Z(i+14)
      nxx = numtyp(Iz(i+13))
      IF ( dec .AND. Iz(i+13)>16000 .AND. Iz(i+13)<=99999999 ) nxx = 1
      IF ( nxx==1 ) CALL tab(Iz(i+13),tavga,eta)
      nxx = numtyp(Iz(i+14))
      IF ( dec .AND. Iz(i+14)>16000 .AND. Iz(i+14)<=99999999 ) nxx = 1
      IF ( nxx==1 ) CALL tab(Iz(i+14),tavgb,etb)
      alpha = Z(i+18)
      alphb = Z(i+19)
      nxx = numtyp(Iz(i+18))
      IF ( dec .AND. Iz(i+18)>16000 .AND. Iz(i+18)<=99999999 ) nxx = 1
      IF ( nxx==1 ) CALL tab(Iz(i+18),tavga,alpha)
      nxx = numtyp(Iz(i+19))
      IF ( dec .AND. Iz(i+19)>16000 .AND. Iz(i+19)<=99999999 ) nxx = 1
      IF ( nxx==1 ) CALL tab(Iz(i+19),tavgb,alphb)
      alpha = alpha - 1.0
      alphb = alphb - 1.0
!
!     B. COMPUTE DENOMINATOR
!
      xh = Sigma*eta*(tavga+Tabs)**4
      xk = Sigma*etb*(tavgb+Tabs)**4
      fxsp = alpha*fab*xk - aa*xh + fab*xk - (alphb*fabsq*xh)/ab
      fysp = alphb*fab*xh - ab*xk + fab*xh - (alpha*fabsq*xk)/aa
      fab = 1.0 - (alpha*alphb/aa)*(fabsq/ab)
      fx = fxsp/(fab*float(mm))
      fy = fysp/(fab*float(nn))
!
!     C. APPLY FORCES ON AREAS A AND  B
!
      j = 1
      DO l = 1 , 4
         IF ( l==3 ) j = 6
         m = Iz(i+j)
         IF ( m/=0 ) THEN
            m = ipx + m
            Dz(m) = Dz(m) + fx
         ENDIF
         m = Iz(i+j+10)
         IF ( m/=0 ) THEN
            m = ipx + m
            Dz(m) = Dz(m) + fy
         ENDIF
         j = j + 1
      ENDDO
      i = i + 20
      GOTO 1100
   ELSEIF ( l==13 ) THEN
!
!     NOLIN6
!
      x = y
      fy = x*dabs(x)
      x = Dz(m)
   ELSEIF ( l==14 ) THEN
      y = Dz(mm)
      fy = y*dabs(y)
   ENDIF
!
!     NOLIN 1
!
   xsp = x
   CALL tab(Iz(i+4),xsp,fxsp)
   fx = fxsp
   GOTO 1000
!
!     NOLIN 2
!
 700  y = Dz(mm)
   fx = x*y
   GOTO 1000
!
!     NOLIN 3
!
 800  IF ( x>0.0D0 ) fx = x**Z(i+4)
   GOTO 1000
!
!     NOLIN 4
!
 900  IF ( x<0.0D0 ) fx = -dabs(x)**Z(i+4)
!
!     FINISH APPLYING SCALE FACTOR AND ADD
!
 1000 l = ipx + Iz(i+1)
   Dz(l) = Dz(l) + fx*fy*Z(i+3)
   IF ( dabs(Dz(l))<1.0D-36 ) Dz(l) = 0.0D0
   IF ( dabs(Dz(l))>=1.0D+36 ) THEN
      kount = kount + 1
      IF ( kount==1 .OR. kount==4 ) WRITE (Iout,99001)
99001 FORMAT (/1X,28(4H****),/)
      IF ( kount<=3 ) WRITE (Iout,99002) Uwm , Dz(l)
99002 FORMAT (A25,' 3309, UNUSUALLY LARGE VALUE COMPUTED FOR NONLINEAR',' FORCING FUNCTION',5X,D15.5)
   ENDIF
   i = i + 5
 1100 IF ( i<k ) GOTO 600
!
!     END OF LOAD LOOP
!
!     DONE
!
   IF ( ialg/=0 ) THEN
      DO i = 1 , Nrow
!
!     SUM OVER LAST THREE LOADS
!
         l = Ip + i
         k = in1 + i
         m = in2 + i
         kk = in3 + i
         Dz(l) = Dz(l) + (Dz(k)+Dz(m)+Dz(kk))/3.0D0
      ENDDO
!
!     SWITCH POINTERS
!
      k = in1
      in1 = in2
      in2 = in3
      in3 = k
   ENDIF
   RETURN
!
!     ERROR MESSAGES
!
 1200 WRITE (Iout,99003) Ufm
99003 FORMAT (A23,', NON-LINEAR FORCING LOAD (NLFT) WAS NOT GENERATED',' PREVIOUSLY')
   ip1 = -37
 1300 CALL mesage(ip1,file,nmtd)
   RETURN
 1400 ip1 = -2
   GOTO 1300
 1500 ip1 = -8
   file = icrq
   GOTO 1300
99999 END SUBROUTINE trd1d2
