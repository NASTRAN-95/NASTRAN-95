!*==detm3.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE detm3() !HIDESTARS (*,*,*)
USE C_DETMX
USE C_REGEAN
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: a , aa , deltak , detry , dist , dsave , gk , gk1 , h1 , h2 , h3 , hk , hk1 , hkp1 , lamdak , lamdk1 , ptry ,    &
                 & root , root1 , srp , t1 , t2 , temp2 , xlamsv
   REAL(REAL64) , DIMENSION(1) :: det , ps
   INTEGER :: i , igk , igoto , ilmk , iptry , iroot , iroot1 , it1 , it2 , n , n2ev2 , nnd , nni , nnp , npole1
   INTEGER , DIMENSION(1) :: ipdet
   EXTERNAL arrm , detdet , detm6 , eadd , sqrtm , summ , tmtogo
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     RMAX   = APPROXIMATE MAGNITUDE OF LARGEST EIGENVALUE OF INTEREST
!     RMIN   = LOWEST  NON-ZERO  EIGENVALUE
!     MZ     = NUMBER OF ZERO EIGENVALUES
!     NEV    = NUMBER OF NON-ZERO EIGENVALUES IN RANGE OF INTEREST
!     EPSI   = CONVERGENCE CRITERION
!
!     NEVM   = MAXIMUM NUMBER OF EIGENVALUES DESIRED
!     IS     = STARTING SET COUNTER
!     IC     = COUNTER FOR CHANGE OF CONVERGENCE CRITERIA
!     NFOUND = THE NUMBER OF EIGENVALUES FOUND TO DATA
!     IM     = MASS MATRIX CONTROL BLOCK
!     IK     = K MATRIX CONTROL BLOCK
!     IEV    = EIGENVECTOR CONTROL BLOCK
!
!     A      = M + P*K
!
   !>>>>EQUIVALENCE (Psave(1),Ps(1),Det(1),Ipdet(1))
!
         CALL arrm(Ps1,Det1,Ipdet1)
         aa = Ps1(3) - Ps1(2)
         dsave = 1.0E38
!
!     COPY INTO INTERATION BLOCK
!
         DO n = 1 , 3
            Detx(n) = Det1(n)
            P(n) = Ps1(n)
            Ipdetx(n) = Ipdet1(n)
         ENDDO
!
!     START INTERATION LOOP
!
         K = 1
         igoto = 1
         spag_nextblock_1 = 2
      CASE (2)
         hk1 = P(2) - P(1)
         hk = P(3) - P(2)
         lamdak = hk/hk1
         IF ( dabs(hk)<=dabs(Epsi*100.0*P(3)) ) THEN
!
!     ACCEPT PK
!
            Iffnd = 1
            RETURN
         ELSE
!
!     CHECK FOR EARLY CONVERGENCE
!
            deltak = 1.0D0 + lamdak
!
!     COMPUTE  GK
!
            CALL summ(t1,it1,Detx(1)*lamdak*lamdak,Ipdetx,Detx(2)*deltak*deltak,Ipdetx(2),-1)
            CALL summ(gk,igk,t1,it1,Detx(3)*(lamdak+deltak),Ipdetx(3),1)
!
!     COMPUTE ROOT1
!
            CALL summ(t1,it1,Detx(1)*lamdak,Ipdetx(1),Detx(2)*deltak,Ipdetx(2),-1)
            CALL summ(t2,it2,t1,it1,Detx(3),Ipdetx(3),1)
            CALL summ(root1,iroot1,gk*gk,2*igk,-4.0*deltak*lamdak*Detx(3)*t2,Ipdetx(3)+it2,1)
!
!     COMPUTE ROOT = DSQRT (ROOT1)
!
            CALL sqrtm(root,iroot,root1,iroot1)
            a = -2.0*Detx(3)*deltak
            gk1 = gk
            DO n = 1 , 2
               IF ( root1<0.0 ) THEN
!
!     T1= GK*GK + DABS(ROOT1)
!
                  CALL summ(t1,it1,gk*gk,igk+igk,dabs(root1),iroot1,1)
                  lamdk1 = a*gk/t1
                  ilmk = Ipdetx(3) + igk - it1
                  lamdk1 = lamdk1*10.0**ilmk
                  GOTO 10
               ELSE
                  temp2 = root
                  IF ( gk1/=0.0D0 ) temp2 = dsign(root,gk1)
!
                  CALL summ(t1,it1,gk,igk,temp2,iroot,1)
!
                  lamdk1 = a/t1
                  ilmk = Ipdetx(3) - it1
                  lamdk1 = lamdk1*10.0**ilmk
                  IF ( K/=1 ) GOTO 10
!
!     IF (K .EQ. 1) RECALC LK1 TO MINIMIZE DIST
!
                  dist = 0.0D0
                  DO i = 1 , 3
                     dist = dabs(Ps1(i)-Ps1(3)-lamdk1*aa) + dist
                  ENDDO
                  IF ( dist<dsave ) THEN
                     dsave = dist
                     xlamsv = lamdk1
                  ENDIF
                  gk1 = -gk1
               ENDIF
            ENDDO
            lamdk1 = xlamsv
 10         hkp1 = lamdk1*hk
            ptry = P(3) + hkp1
!
!     RANGE CHECKS
!
            IF ( ptry>Rmax ) THEN
!
!     NEW STARTING SET
!
               Ifail = 0
            ELSE
               IF ( Is/=N2ev-1 ) THEN
                  nnp = Is + Ips
                  IF ( ptry>0.45*ps(nnp+2)+0.55*ps(nnp+3) ) THEN
                     Ifail = 0
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
               IF ( ptry<Rminr ) THEN
!
!     INCREASE POLE  AT LOWEST  E. V. GEOMETRICALLY
!
                  npole1 = Npole + 1
                  Npole = 2*Npole + 1
!
!     SWEEP PREVIOUSLY EVALUATED STARTING POINTS BY POLES
!
                  n2ev2 = Nd + Iadd
                  DO n = 1 , n2ev2
                     nnd = n + Idet
                     nnp = n + Ips
                     nni = n + Ipdeta
                     ptry = 1.0D0
                     iptry = 0
                     DO i = 1 , npole1
                        ptry = ptry*(ps(nnp)-Rminr)
                        CALL detm6(ptry,iptry)
                     ENDDO
                     det(nnd) = det(nnd)/ptry
                     ipdet(nni) = ipdet(nni) - iptry
                     CALL detm6(det(nnd),ipdet(nni))
                  ENDDO
                  Ifail = 0
               ELSE
!
!     TRY FOR CONVERGENCE
!
                  CALL tmtogo(iptry)
                  IF ( iptry<=0 ) RETURN 3
                  CALL eadd(-ptry,Prec)
                  CALL detdet(detry,iptry,ptry,Sml1,Detx(3),Ipdetx(3))
                  IF ( detry==0.0D0 ) THEN
                     igoto = 2
!
!     BEGIN CONVERGENCE TESTS
!
                  ELSEIF ( K<=2 ) THEN
!
!     INTERATE AGAIN
!
                     K = K + 1
                  ELSE
                     srp = dsqrt(dabs(P(3)))
                     h1 = dabs(hk1)/srp
                     h2 = dabs(hk)/srp
                     h3 = dabs(hkp1)/srp
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         Is = Is + 1
         IF ( Is>=N2ev ) THEN
!
!      LOOK AT OLD STARTING SETS AGAIN
!
            IF ( Iffnd/=1 ) RETURN 2
            Iffnd = 0
            Is = 1
            Nstart = Nstart + 1
            RETURN 1
         ELSE
            IF ( Nstart==0 ) Iadd = Iadd + 1
            RETURN 1
         ENDIF
      CASE (4)
         Fact1 = Epsi*sqrt(Rmax)
         IF ( h1>2.E7*Fact1 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( h2>2.E4*Fact1 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( h3>h2 ) THEN
            IF ( h2>20.*Fact1 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            igoto = 2
         ELSE
            IF ( h3>2.*Fact1 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            igoto = 2
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         DO i = 1 , 2
            P(i) = P(i+1)
            Ipdetx(i) = Ipdetx(i+1)
            Detx(i) = Detx(i+1)
         ENDDO
         Ipdetx(3) = iptry
         Detx(3) = detry
         P(3) = ptry
         IF ( igoto==1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( igoto==2 ) THEN
            Iffnd = 1
            RETURN
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     FAIL TEST
!
         K = K + 1
         IF ( K<Nit ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( K==Nit ) THEN
            IF ( Ifail==1 .AND. Ic<Ne ) THEN
               Epsi = 10.0*Epsi
               Ic = Ic + 1
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         Ifail = 1
         Nfail = Nfail + 1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE detm3
