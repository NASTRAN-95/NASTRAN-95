!*==trd1e.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trd1e(Mhh,Bhh,Khh,Ph,Uhv,Ngroup)
   USE c_blank
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Mhh
   INTEGER :: Bhh
   INTEGER :: Khh
   INTEGER :: Ph
   INTEGER :: Uhv
   INTEGER :: Ngroup
!
! Local variable declarations rewritten by SPAG
!
   REAL :: beta , betasq , bh , bi , coswh , expbh , h , ki , mi , sinwh , t1 , t2 , t3 , w , wh , wosq , wsq
   REAL , SAVE :: epsi
   INTEGER :: file , i , ia , iapr , ib , ibii , ibpr , ibuf1 , ibuf2 , icrq , if , ifpr , ig , igpr , igroup , ikii , imii , ip1 , &
            & iphj , iphj1 , iretn , ist , iudj , iudj1 , iuj , iuj1 , j , jk , k , kd , kd1 , kd2 , kd3 , kk , kkk , l , lc ,      &
            & nmodes , nout , nstep
   INTEGER , DIMENSION(7) :: iuhv
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , gopen , korsz , makmcb , mesage , open , pack , rdtrl , skprec , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         lc = korsz(z)
         lc = lc - Ngroup*3
         igroup = lc + 1
         ist = -1
         ibuf1 = lc - sysbuf
         ibuf2 = ibuf1 - sysbuf
         lc = lc - 2*sysbuf
         iuhv(1) = Mhh
         CALL rdtrl(iuhv)
         nmodes = iuhv(2)
         it1 = 1
         it2 = 1
         it3 = 1
         incur = 1
         incur1 = 1
         ii = 1
         jj = nmodes
         icrq = 17*nmodes - lc
         IF ( icrq>0 ) THEN
            ip1 = -8
            file = icrq
            spag_nextblock_1 = 14
         ELSE
!
!     BRING IN H MATRICES
!
!
!     BRING IN  MHH
            file = Mhh
            imii = 0
            kk = imii
            ASSIGN 20 TO iretn
            spag_nextblock_1 = 12
         ENDIF
         CYCLE
!
!     BRING IN BHH
 20      DO j = 1 , nmodes
            IF ( z(j)==0.0 ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         file = Bhh
         ibii = imii + nmodes
         kk = ibii
         ASSIGN 40 TO iretn
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
!
!     BRING IN KHH
 40      file = Khh
         ikii = ibii + nmodes
         kk = ikii
         ASSIGN 60 TO iretn
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
!
!     ASSIGN ADDITIONAL POINTERS
!
 60      iii = 1
         jjj = nmodes
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
         IF ( ncol<=2 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     RETRIEVE OLD DISPLACEMENT AND VELOCITY
!     FROM A PREVIOUSLY CHECKPOINTED RUN
!
         CALL gopen(Uhv,iz(ibuf1),0)
         i = 3*(ncol-1)
         CALL skprec(Uhv,i)
!
!     RETRIEVE OLD DISPLACEMENT
!
         CALL unpack(*80,Uhv,z(iuj1+1))
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 80      DO i = 1 , nmodes
            k = iuj1 + i
            z(k) = 0.0
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
!
!     RETRIEVE OLD VELOCITY
!
         CALL unpack(*100,Uhv,z(iudj1+1))
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 100     DO i = 1 , nmodes
            k = iudj1 + i
            z(k) = 0.0
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
         CALL close(Uhv,1)
         spag_nextblock_1 = 4
      CASE (4)
!RLBNE
!
!     READY UHV
!
!RLBR SPR94003 9/94      CALL GOPEN(UHV,IZ(IBUF1),1)
         CALL gopen(Uhv,iz(ibuf1),1)
         CALL makmcb(iuhv,Uhv,nmodes,2,1)
!
!     READY LOADS
!
         CALL gopen(Ph,iz(ibuf2),0)
         CALL unpack(*120,Ph,z(iphj1+1))
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     ZERO LOAD
!
 120     DO i = 1 , nmodes
            k = iphj1 + i
            z(k) = 0.0
         ENDDO
         spag_nextblock_1 = 5
      CASE (5)
!RLBNB SPR94003 9/94
         IF ( ncol<=2 ) THEN
!RLBNE
!
!     ZERO INITIAL DISPLACEMENT AND VELOCITY
!
!RLBR SPR 94003 9/94   60 DO 70 I=1,NMODES
            DO i = 1 , nmodes
               k = iuj1 + i
               z(k) = 0.0
               k = iudj1 + i
               z(k) = 0.0
            ENDDO
         ENDIF
!
!     BEGIN LOOP ON EACH DIFFERENT TIME STEP
!
!RLBR SPR 94003 9/94      I = 1
         i = 1
         spag_nextblock_1 = 6
      CASE (6)
         nstep = iz(igroup)
         IF ( i==1 ) nstep = nstep + 1
         h = z(igroup+1)
         nout = iz(igroup+2)
         igroup = igroup + 3
         jk = 1
         IF ( i==1 ) THEN
!
!     TIME TO OUTPUT--YOU LUCKY FELLOW
!
            ASSIGN 140 TO iretn
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     COMPUTE F-S ,G-S,A-S,B-S
!
         DO j = 1 , nmodes
            k = imii + j
            mi = z(k)
            IF ( mi==0.0 ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            k = ibii + j
            bi = z(k)
            k = ikii + j
            ki = z(k)
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
               z(k) = 1.0
               k = ig + j
               z(k) = h
               k = ia + j
               z(k) = h*h/(3.0*mi)
               k = ib + j
               z(k) = h*h/(6.0*mi)
               k = ifpr + j
               z(k) = 0.0
               k = igpr + j
               z(k) = 1.0
               t1 = h/(2.0*mi)
               k = iapr + j
               z(k) = t1
               k = ibpr + j
               z(k) = t1
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
                  z(k) = expbh*(1.0+bh)
!
!     COMPUTE  G
!
                  k = ig + j
                  z(k) = h*expbh
!
!     COMPUTE A
!
                  k = ia + j
                  z(k) = (2.0/beta-expbh/beta*(2.0+2.0*bh+bh*bh))/t1
!
!     COMPUTE B
!
                  k = ib + j
                  z(k) = (-2.0+bh+expbh*(2.0+bh))/(bh*ki)
!
!     COMPUTE  F PRIME
!
                  k = ifpr + j
                  z(k) = -betasq*h*expbh
!
!     COMPUTE  G PRIME
!
                  k = igpr + j
                  z(k) = expbh*(1.0-bh)
!
!     COMPUTE A PRIME
!
                  k = iapr + j
                  z(k) = (expbh*(1.0+bh+bh*bh)-1.0)/t1
!
!     COMPUTE  B PRIME
!
                  k = ibpr + j
                  z(k) = (1.0-expbh*(bh+1.0))/t1
                  CYCLE
               ENDIF
!
!     COMPUTE F
!
               k = if + j
               z(k) = expbh*(coswh+beta/w*sinwh)
!
!     COMPUTE G
!
               k = ig + j
               z(k) = expbh/w*sinwh
!
!     COMPUTE A
!
               k = ia + j
               t1 = (wsq-betasq)/wosq
               t2 = 2.0*w*beta/wosq
               t3 = wh*ki
               z(k) = (expbh*((t1-beta*h)*sinwh-(t2+wh)*coswh)+t2)/t3
!
!     COMPUTE  B
!
               k = ib + j
               z(k) = (expbh*(-t1*sinwh+t2*coswh)+wh-t2)/t3
!
!     COMPUTE  FPRIME
!
               k = ifpr + j
               z(k) = -wosq/w*expbh*sinwh
!
!     COMPUTE G PRIME
!
               k = igpr + j
               z(k) = expbh*(coswh-beta/w*sinwh)
!
!     COMPUTE A PRIME
!
               k = iapr + j
               z(k) = (expbh*((beta+wosq*h)*sinwh+w*coswh)-w)/t3
!
!     COMPUTE B PRIME
!
               k = ibpr + j
               z(k) = (-expbh*(beta*sinwh+w*coswh)+w)/t3
            ENDIF
         ENDDO
         spag_nextblock_1 = 8
      CASE (8)
!
!     BEGIN LOOP ON INCREMENTS
!
!
!     COMPUTE  NEW DISPLACEMENTS
!
         k = iuj1
         kk = iudj1
         DO l = 1 , nmodes
            k = k + 1
            kk = kk + 1
            z(k) = 0.0
            z(kk) = 0.0
            kkk = if + l
            kd = iuj + l
            z(k) = z(kkk)*z(kd) + z(k)
            kkk = ifpr + l
            z(kk) = z(kkk)*z(kd) + z(kk)
            kd = iudj + l
            kkk = ig + l
            z(k) = z(kkk)*z(kd) + z(k)
            kkk = igpr + l
            z(kk) = z(kkk)*z(kd) + z(kk)
            kd = iphj + l
            kkk = ia + l
            z(k) = z(kkk)*z(kd) + z(k)
            kkk = iapr + l
            z(kk) = z(kkk)*z(kd) + z(kk)
            kd = iphj1 + l
            kkk = ib + l
            z(k) = z(kkk)*z(kd) + z(k)
            kkk = ibpr + l
            z(kk) = z(kkk)*z(kd) + z(kk)
         ENDDO
         IF ( jk==nstep ) THEN
            ASSIGN 160 TO iretn
         ELSEIF ( jk/=1 .AND. mod(jk+ist,nout)/=0 ) THEN
            ASSIGN 140 TO iretn
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ELSE
            ASSIGN 140 TO iretn
         ENDIF
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 140     jk = jk + 1
         IF ( jk==2 .AND. i==1 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( jk<=nstep ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ASSIGN 160 TO iretn
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 160     i = i + 1
         ist = 0
         IF ( i<=Ngroup ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL close(Ph,1)
         CALL close(Uhv,1)
         CALL wrttrl(iuhv)
         RETURN
      CASE (9)
!
!     INTERNAL SUBROUTINE FOR OUTPUT AND VELOCITY COMPUTE
!
         CALL pack(z(iuj1+1),Uhv,iuhv)
         CALL pack(z(iudj1+1),Uhv,iuhv)
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
            z(k) = z(kk)/z(kkk) - z(kd)*z(kd1)/z(kkk) - z(kd3)*z(kd2)/z(kkk)
         ENDDO
         CALL pack(z(iudj+1),Uhv,iuhv)
         spag_nextblock_1 = 10
      CASE (10)
!
!     SWITCH POINTS TO STUFF
!
         kd = iuj
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
         CALL unpack(*180,Ph,z(iphj1+1))
         spag_nextblock_1 = 11
      CASE (11)
         GOTO iretn
 180     DO kd = 1 , nmodes
            k = iphj1 + kd
            z(k) = 0.0
         ENDDO
         spag_nextblock_1 = 11
      CASE (12)
!
!     INTERNAL SUBROUTINE TO BRING  IN H MATRICES
!
         CALL open(*200,file,iz(ibuf1),0)
         CALL skprec(file,1)
         DO kd = 1 , nmodes
            iii = kd
            jjj = kd
            kd1 = kk + kd
            CALL unpack(*190,file,z(kd1))
            CYCLE
 190        z(kd1) = 0.0
         ENDDO
         CALL close(file,1)
         spag_nextblock_1 = 13
      CASE (13)
         GOTO iretn
!
!      ZERO CORE FOR PURGED FILES
!
 200     DO kd = 1 , nmodes
            kd1 = kk + kd
            z(kd1) = 0.0
         ENDDO
         spag_nextblock_1 = 13
      CASE (14)
!
!     ERROR MESAGES
!
         CALL mesage(ip1,file,name)
         RETURN
      CASE (15)
         ip1 = -43
         file = j
         spag_nextblock_1 = 14
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE trd1e
