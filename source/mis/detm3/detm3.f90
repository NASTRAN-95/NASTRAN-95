!*==detm3.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE detm3(*,*,*)
   USE c_detmx
   USE c_regean
   USE c_zzzzzz
   USE iso_fortran_env
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
         CALL arrm(ps1,det1,ipdet1)
         aa = ps1(3) - ps1(2)
         dsave = 1.0E38
!
!     COPY INTO INTERATION BLOCK
!
         DO n = 1 , 3
            detx(n) = det1(n)
            p(n) = ps1(n)
            ipdetx(n) = ipdet1(n)
         ENDDO
!
!     START INTERATION LOOP
!
         k = 1
         igoto = 1
         spag_nextblock_1 = 2
      CASE (2)
         hk1 = p(2) - p(1)
         hk = p(3) - p(2)
         lamdak = hk/hk1
         IF ( dabs(hk)<=dabs(epsi*100.0*p(3)) ) THEN
!
!     ACCEPT PK
!
            iffnd = 1
            RETURN
         ELSE
!
!     CHECK FOR EARLY CONVERGENCE
!
            deltak = 1.0D0 + lamdak
!
!     COMPUTE  GK
!
            CALL summ(t1,it1,detx(1)*lamdak*lamdak,ipdetx,detx(2)*deltak*deltak,ipdetx(2),-1)
            CALL summ(gk,igk,t1,it1,detx(3)*(lamdak+deltak),ipdetx(3),1)
!
!     COMPUTE ROOT1
!
            CALL summ(t1,it1,detx(1)*lamdak,ipdetx(1),detx(2)*deltak,ipdetx(2),-1)
            CALL summ(t2,it2,t1,it1,detx(3),ipdetx(3),1)
            CALL summ(root1,iroot1,gk*gk,2*igk,-4.0*deltak*lamdak*detx(3)*t2,ipdetx(3)+it2,1)
!
!     COMPUTE ROOT = DSQRT (ROOT1)
!
            CALL sqrtm(root,iroot,root1,iroot1)
            a = -2.0*detx(3)*deltak
            gk1 = gk
            DO n = 1 , 2
               IF ( root1<0.0 ) THEN
!
!     T1= GK*GK + DABS(ROOT1)
!
                  CALL summ(t1,it1,gk*gk,igk+igk,dabs(root1),iroot1,1)
                  lamdk1 = a*gk/t1
                  ilmk = ipdetx(3) + igk - it1
                  lamdk1 = lamdk1*10.0**ilmk
                  GOTO 10
               ELSE
                  temp2 = root
                  IF ( gk1/=0.0D0 ) temp2 = dsign(root,gk1)
!
                  CALL summ(t1,it1,gk,igk,temp2,iroot,1)
!
                  lamdk1 = a/t1
                  ilmk = ipdetx(3) - it1
                  lamdk1 = lamdk1*10.0**ilmk
                  IF ( k/=1 ) GOTO 10
!
!     IF (K .EQ. 1) RECALC LK1 TO MINIMIZE DIST
!
                  dist = 0.0D0
                  DO i = 1 , 3
                     dist = dabs(ps1(i)-ps1(3)-lamdk1*aa) + dist
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
            ptry = p(3) + hkp1
!
!     RANGE CHECKS
!
            IF ( ptry>rmax ) THEN
!
!     NEW STARTING SET
!
               ifail = 0
            ELSE
               IF ( is/=n2ev-1 ) THEN
                  nnp = is + ips
                  IF ( ptry>0.45*ps(nnp+2)+0.55*ps(nnp+3) ) THEN
                     ifail = 0
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
               IF ( ptry<rminr ) THEN
!
!     INCREASE POLE  AT LOWEST  E. V. GEOMETRICALLY
!
                  npole1 = npole + 1
                  npole = 2*npole + 1
!
!     SWEEP PREVIOUSLY EVALUATED STARTING POINTS BY POLES
!
                  n2ev2 = nd + iadd
                  DO n = 1 , n2ev2
                     nnd = n + idet
                     nnp = n + ips
                     nni = n + ipdeta
                     ptry = 1.0D0
                     iptry = 0
                     DO i = 1 , npole1
                        ptry = ptry*(ps(nnp)-rminr)
                        CALL detm6(ptry,iptry)
                     ENDDO
                     det(nnd) = det(nnd)/ptry
                     ipdet(nni) = ipdet(nni) - iptry
                     CALL detm6(det(nnd),ipdet(nni))
                  ENDDO
                  ifail = 0
               ELSE
!
!     TRY FOR CONVERGENCE
!
                  CALL tmtogo(iptry)
                  IF ( iptry<=0 ) RETURN 3
                  CALL eadd(-ptry,prec)
                  CALL detdet(detry,iptry,ptry,sml1,detx(3),ipdetx(3))
                  IF ( detry==0.0D0 ) THEN
                     igoto = 2
!
!     BEGIN CONVERGENCE TESTS
!
                  ELSEIF ( k<=2 ) THEN
!
!     INTERATE AGAIN
!
                     k = k + 1
                  ELSE
                     srp = dsqrt(dabs(p(3)))
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
         is = is + 1
         IF ( is>=n2ev ) THEN
!
!      LOOK AT OLD STARTING SETS AGAIN
!
            IF ( iffnd/=1 ) RETURN 2
            iffnd = 0
            is = 1
            nstart = nstart + 1
            RETURN 1
         ELSE
            IF ( nstart==0 ) iadd = iadd + 1
            RETURN 1
         ENDIF
      CASE (4)
         fact1 = epsi*sqrt(rmax)
         IF ( h1>2.E7*fact1 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( h2>2.E4*fact1 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( h3>h2 ) THEN
            IF ( h2>20.*fact1 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            igoto = 2
         ELSE
            IF ( h3>2.*fact1 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            igoto = 2
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         DO i = 1 , 2
            p(i) = p(i+1)
            ipdetx(i) = ipdetx(i+1)
            detx(i) = detx(i+1)
         ENDDO
         ipdetx(3) = iptry
         detx(3) = detry
         p(3) = ptry
         IF ( igoto==1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( igoto==2 ) THEN
            iffnd = 1
            RETURN
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     FAIL TEST
!
         k = k + 1
         IF ( k<nit ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( k==nit ) THEN
            IF ( ifail==1 .AND. ic<ne ) THEN
               epsi = 10.0*epsi
               ic = ic + 1
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         ifail = 1
         nfail = nfail + 1
         spag_nextblock_1 = 3
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE detm3
