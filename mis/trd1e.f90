
SUBROUTINE trd1e(Mhh,Bhh,Khh,Ph,Uhv,Ngroup)
   IMPLICIT NONE
   REAL Dummy(4) , Z(1)
   INTEGER Ii , Iii , Incur , Incur1 , It1 , It2 , It3 , Iz(1) , Jj , Jjj , Ncol , Sysbuf
   COMMON /blank / Dummy , Ncol
   COMMON /packx / It1 , It2 , Ii , Jj , Incur
   COMMON /system/ Sysbuf
   COMMON /unpakx/ It3 , Iii , Jjj , Incur1
   COMMON /zzzzzz/ Z
   INTEGER Bhh , Khh , Mhh , Ngroup , Ph , Uhv
   REAL beta , betasq , bh , bi , coswh , epsi , expbh , h , ki , mi , sinwh , t1 , t2 , t3 , w , wh , wosq , wsq
   INTEGER file , i , ia , iapr , ib , ibii , ibpr , ibuf1 , ibuf2 , icrq , if , ifpr , ig , igpr , igroup , ikii , imii , ip1 ,    &
         & iphj , iphj1 , iretn , ist , iudj , iudj1 , iuhv(7) , iuj , iuj1 , j , jk , k , kd , kd1 , kd2 , kd3 , kk , kkk , l ,    &
         & lc , name(2) , nmodes , nout , nstep
   INTEGER korsz
!
!     THIS ROUTINE SOLVES TRANSIENT PROBLEM ANALYTICALLY IN CASE
!         OF UNCOUPLED MODAL WITH NO NONLINEAR LOADS
!
!
!RLBNB SPR94003 9/94
!RLBNE
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
!
   DATA name/4HTRD1 , 4HE   /
   DATA epsi/1.0E-8/
!*********
!     DEFINITION OF VARIABLES
!*********
!     IGROUP   POINTER TO TIME STEP DATA  N1,DELTAT,NO
!     NGROUP   NUMBER OF TIME STEP CHANGES
!     MHH      MODAL MASS FILE
!     KHH      MODAL STIFFNESS FILE
!     BHH      MODAL DAMPING FILE
!     PH       LOAD FILE
!     UHV      DISPLACEMENT,VELOCITY, AND ACCELERATION FILE
!     NMODES   ORDER OF MODAL FORMULATION
!     IMII     POINTER TO MASSES
!     IBII     POINTER TO DAMPING
!     IKII     POINTER TO STIFFNESS
!     IF       POINTER TO F-S
!     IFPR     POINTER TO F PRIMES
!     IG       POINTER TO G-S
!     IGPR
!     IA       POINTER TO A-S
!     IAPR
!     IB       POINTER TO B-S
!     IBPR
!     IUJ      POINTER TO OLD  DISP
!     IUJ1             TO NEW  DISP
!     IUDJ     POINTER TO  OLD VELOCITY VECTOR
!     IUDJ1                NEW VELOCITY VECTOR
!     IPHJ     POINTER TO  OLD LOAD VECTOR
!     IPHJ1                NEW LOAD VECTOR
!     NSTEP    NUMBER OF STEPS AT CURRENT INCREMENT
!     H        CURRENT DELTA T
!     NOUT     OUTPUT INCURMENT
!     EPSI     CASE SELTION TOLERANCE
!
!********    HERE WE GO --GET LOTS OF PAPER
!
   lc = korsz(Z)
   lc = lc - Ngroup*3
   igroup = lc + 1
   ist = -1
   ibuf1 = lc - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   lc = lc - 2*Sysbuf
   iuhv(1) = Mhh
   CALL rdtrl(iuhv)
   nmodes = iuhv(2)
   It1 = 1
   It2 = 1
   It3 = 1
   Incur = 1
   Incur1 = 1
   Ii = 1
   Jj = nmodes
   icrq = 17*nmodes - lc
   IF ( icrq>0 ) THEN
      ip1 = -8
      file = icrq
      GOTO 2300
   ELSE
!
!     BRING IN H MATRICES
!
!
!     BRING IN  MHH
      file = Mhh
      imii = 0
      kk = imii
      ASSIGN 100 TO iretn
      GOTO 2000
   ENDIF
!
!     BRING IN BHH
 100  DO j = 1 , nmodes
      IF ( Z(j)==0.0 ) GOTO 2400
   ENDDO
   file = Bhh
   ibii = imii + nmodes
   kk = ibii
   ASSIGN 200 TO iretn
   GOTO 2000
!
!     BRING IN KHH
 200  file = Khh
   ikii = ibii + nmodes
   kk = ikii
   ASSIGN 300 TO iretn
   GOTO 2000
!
!     ASSIGN ADDITIONAL POINTERS
!
 300  Iii = 1
   Jjj = nmodes
   if = ikii + nmodes
   ig = if + nmodes
   ia = ig + nmodes
   ib = ia + nmodes
   ifpr = ib + nmodes
   igpr = ifpr + nmodes
   iapr = igpr + nmodes
   ibpr = iapr + nmodes
   iuj = ibpr + nmodes
   iuj1 = iuj + nmodes
   iudj = iuj1 + nmodes
   iudj1 = iudj + nmodes
   iphj = iudj1 + nmodes
   iphj1 = iphj + nmodes
!RLBNB SPR94003 9/94
   IF ( Ncol<=2 ) GOTO 800
!
!     RETRIEVE OLD DISPLACEMENT AND VELOCITY
!     FROM A PREVIOUSLY CHECKPOINTED RUN
!
   CALL gopen(Uhv,Iz(ibuf1),0)
   i = 3*(Ncol-1)
   CALL skprec(Uhv,i)
!
!     RETRIEVE OLD DISPLACEMENT
!
   CALL unpack(*400,Uhv,Z(iuj1+1))
   GOTO 500
 400  DO i = 1 , nmodes
      k = iuj1 + i
      Z(k) = 0.0
   ENDDO
!
!     RETRIEVE OLD VELOCITY
!
 500  CALL unpack(*600,Uhv,Z(iudj1+1))
   GOTO 700
 600  DO i = 1 , nmodes
      k = iudj1 + i
      Z(k) = 0.0
   ENDDO
 700  CALL close(Uhv,1)
!RLBNE
!
!     READY UHV
!
!RLBR SPR94003 9/94      CALL GOPEN(UHV,IZ(IBUF1),1)
 800  CALL gopen(Uhv,Iz(ibuf1),1)
   CALL makmcb(iuhv,Uhv,nmodes,2,1)
!
!     READY LOADS
!
   CALL gopen(Ph,Iz(ibuf2),0)
   CALL unpack(*900,Ph,Z(iphj1+1))
   GOTO 1000
!
!     ZERO LOAD
!
 900  DO i = 1 , nmodes
      k = iphj1 + i
      Z(k) = 0.0
   ENDDO
!RLBNB SPR94003 9/94
 1000 IF ( Ncol<=2 ) THEN
!RLBNE
!
!     ZERO INITIAL DISPLACEMENT AND VELOCITY
!
!RLBR SPR 94003 9/94   60 DO 70 I=1,NMODES
      DO i = 1 , nmodes
         k = iuj1 + i
         Z(k) = 0.0
         k = iudj1 + i
         Z(k) = 0.0
      ENDDO
   ENDIF
!
!     BEGIN LOOP ON EACH DIFFERENT TIME STEP
!
!RLBR SPR 94003 9/94      I = 1
   i = 1
 1100 nstep = Iz(igroup)
   IF ( i==1 ) nstep = nstep + 1
   h = Z(igroup+1)
   nout = Iz(igroup+2)
   igroup = igroup + 3
   jk = 1
   IF ( i==1 ) THEN
!
!     TIME TO OUTPUT--YOU LUCKY FELLOW
!
      ASSIGN 1400 TO iretn
      GOTO 1600
   ENDIF
!
!     COMPUTE F-S ,G-S,A-S,B-S
!
 1200 DO j = 1 , nmodes
      k = imii + j
      mi = Z(k)
      IF ( mi==0.0 ) GOTO 2400
      k = ibii + j
      bi = Z(k)
      k = ikii + j
      ki = Z(k)
      wosq = ki/mi
      beta = bi/(2.0*mi)
      betasq = beta*beta
      wsq = abs(wosq-betasq)
      w = sqrt(wsq)
      IF ( sqrt(wsq+betasq)*h<1.E-6 ) THEN
!
!     CASE  4   W0 = BETA =0.0
!
         k = if + j
         Z(k) = 1.0
         k = ig + j
         Z(k) = h
         k = ia + j
         Z(k) = h*h/(3.0*mi)
         k = ib + j
         Z(k) = h*h/(6.0*mi)
         k = ifpr + j
         Z(k) = 0.0
         k = igpr + j
         Z(k) = 1.0
         t1 = h/(2.0*mi)
         k = iapr + j
         Z(k) = t1
         k = ibpr + j
         Z(k) = t1
      ELSE
         t1 = (wosq-betasq)/wosq
         IF ( t1>epsi ) THEN
!
!     CASE 1 --UNDERDAMPED
!
            wh = w*h
            expbh = exp(-beta*h)
            sinwh = sin(wh)
            coswh = cos(wh)
         ELSEIF ( t1<-epsi ) THEN
!
!     CASE  3    W0 - BETASQ L -E
!
            wh = w*h
            expbh = exp(-beta*h)
            sinwh = sinh(wh)
            coswh = cosh(wh)
            betasq = -betasq
         ELSE
!
!     CASE  3  CRITICALLY DAMPED
!
            bh = beta*h
            expbh = exp(-bh)
            t1 = h*ki
            k = if + j
!
!     COMPUTE F
!
            Z(k) = expbh*(1.0+bh)
!
!     COMPUTE  G
!
            k = ig + j
            Z(k) = h*expbh
!
!     COMPUTE A
!
            k = ia + j
            Z(k) = (2.0/beta-expbh/beta*(2.0+2.0*bh+bh*bh))/t1
!
!     COMPUTE B
!
            k = ib + j
            Z(k) = (-2.0+bh+expbh*(2.0+bh))/(bh*ki)
!
!     COMPUTE  F PRIME
!
            k = ifpr + j
            Z(k) = -betasq*h*expbh
!
!     COMPUTE  G PRIME
!
            k = igpr + j
            Z(k) = expbh*(1.0-bh)
!
!     COMPUTE A PRIME
!
            k = iapr + j
            Z(k) = (expbh*(1.0+bh+bh*bh)-1.0)/t1
!
!     COMPUTE  B PRIME
!
            k = ibpr + j
            Z(k) = (1.0-expbh*(bh+1.0))/t1
            CYCLE
         ENDIF
!
!     COMPUTE F
!
         k = if + j
         Z(k) = expbh*(coswh+beta/w*sinwh)
!
!     COMPUTE G
!
         k = ig + j
         Z(k) = expbh/w*sinwh
!
!     COMPUTE A
!
         k = ia + j
         t1 = (wsq-betasq)/wosq
         t2 = 2.0*w*beta/wosq
         t3 = wh*ki
         Z(k) = (expbh*((t1-beta*h)*sinwh-(t2+wh)*coswh)+t2)/t3
!
!     COMPUTE  B
!
         k = ib + j
         Z(k) = (expbh*(-t1*sinwh+t2*coswh)+wh-t2)/t3
!
!     COMPUTE  FPRIME
!
         k = ifpr + j
         Z(k) = -wosq/w*expbh*sinwh
!
!     COMPUTE G PRIME
!
         k = igpr + j
         Z(k) = expbh*(coswh-beta/w*sinwh)
!
!     COMPUTE A PRIME
!
         k = iapr + j
         Z(k) = (expbh*((beta+wosq*h)*sinwh+w*coswh)-w)/t3
!
!     COMPUTE B PRIME
!
         k = ibpr + j
         Z(k) = (-expbh*(beta*sinwh+w*coswh)+w)/t3
      ENDIF
   ENDDO
!
!     BEGIN LOOP ON INCREMENTS
!
!
!     COMPUTE  NEW DISPLACEMENTS
!
 1300 k = iuj1
   kk = iudj1
   DO l = 1 , nmodes
      k = k + 1
      kk = kk + 1
      Z(k) = 0.0
      Z(kk) = 0.0
      kkk = if + l
      kd = iuj + l
      Z(k) = Z(kkk)*Z(kd) + Z(k)
      kkk = ifpr + l
      Z(kk) = Z(kkk)*Z(kd) + Z(kk)
      kd = iudj + l
      kkk = ig + l
      Z(k) = Z(kkk)*Z(kd) + Z(k)
      kkk = igpr + l
      Z(kk) = Z(kkk)*Z(kd) + Z(kk)
      kd = iphj + l
      kkk = ia + l
      Z(k) = Z(kkk)*Z(kd) + Z(k)
      kkk = iapr + l
      Z(kk) = Z(kkk)*Z(kd) + Z(kk)
      kd = iphj1 + l
      kkk = ib + l
      Z(k) = Z(kkk)*Z(kd) + Z(k)
      kkk = ibpr + l
      Z(kk) = Z(kkk)*Z(kd) + Z(kk)
   ENDDO
   IF ( jk==nstep ) THEN
      ASSIGN 1500 TO iretn
      GOTO 1600
   ELSEIF ( jk/=1 .AND. mod(jk+ist,nout)/=0 ) THEN
      ASSIGN 1400 TO iretn
      GOTO 1700
   ELSE
      ASSIGN 1400 TO iretn
      GOTO 1600
   ENDIF
 1400 jk = jk + 1
   IF ( jk==2 .AND. i==1 ) GOTO 1200
   IF ( jk<=nstep ) GOTO 1300
   ASSIGN 1500 TO iretn
   GOTO 1600
 1500 i = i + 1
   ist = 0
   IF ( i<=Ngroup ) GOTO 1100
   CALL close(Ph,1)
   CALL close(Uhv,1)
   CALL wrttrl(iuhv)
   RETURN
!
!     INTERNAL SUBROUTINE FOR OUTPUT AND VELOCITY COMPUTE
!
 1600 CALL pack(Z(iuj1+1),Uhv,iuhv)
   CALL pack(Z(iudj1+1),Uhv,iuhv)
!
!     COMPUTE  ACCELERATIONS
!
   DO l = 1 , nmodes
      k = iudj + l
      kk = iphj1 + l
      kkk = imii + l
      kd = ibii + l
      kd1 = iudj1 + l
      kd2 = iuj1 + l
      kd3 = ikii + l
      Z(k) = Z(kk)/Z(kkk) - Z(kd)*Z(kd1)/Z(kkk) - Z(kd3)*Z(kd2)/Z(kkk)
   ENDDO
   CALL pack(Z(iudj+1),Uhv,iuhv)
!
!     SWITCH POINTS TO STUFF
!
 1700 kd = iuj
   iuj = iuj1
   iuj1 = kd
   kd = iudj
   iudj = iudj1
   iudj1 = kd
   kd = iphj
   iphj = iphj1
   iphj1 = kd
!
!     BRING IN NEXT LOAD VECTOR
!
   CALL unpack(*1900,Ph,Z(iphj1+1))
 1800 GOTO iretn
 1900 DO kd = 1 , nmodes
      k = iphj1 + kd
      Z(k) = 0.0
   ENDDO
   GOTO 1800
!
!     INTERNAL SUBROUTINE TO BRING  IN H MATRICES
!
 2000 CALL open(*2200,file,Iz(ibuf1),0)
   CALL skprec(file,1)
   DO kd = 1 , nmodes
      Iii = kd
      Jjj = kd
      kd1 = kk + kd
      CALL unpack(*2050,file,Z(kd1))
      CYCLE
 2050 Z(kd1) = 0.0
   ENDDO
   CALL close(file,1)
 2100 GOTO iretn
!
!      ZERO CORE FOR PURGED FILES
!
 2200 DO kd = 1 , nmodes
      kd1 = kk + kd
      Z(kd1) = 0.0
   ENDDO
   GOTO 2100
!
!     ERROR MESAGES
!
 2300 CALL mesage(ip1,file,name)
   RETURN
 2400 ip1 = -43
   file = j
   GOTO 2300
END SUBROUTINE trd1e