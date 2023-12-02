!*==detm1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE detm1() !HIDESTARS (*)
   USE c_detmx
   USE c_regean
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(1) :: det , ps
   REAL*8 :: f1 , f2 , fact , ratio , x , y
   INTEGER :: i , ix , lc , n , nn , nnd , nni , nnp , ns
   INTEGER , DIMENSION(1) :: ipdet
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: srrmax , srrmin
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
!     RMAX = APPROXIMATE MAGNITUDE OF LARGEST EIGENVALUE OF INTEREST
!
!     RMIN = LOWEST  NON-ZERO  EIGENVALUE
!
!     MZ = NUMBER OF ZERO EIGENVALUES
!
!     NEV = NUMBER OF NON-ZERO EIGENVALUES IN RANGE OF INTEREST
!
!     EPSI = CONVERGENCE CRITERION
!
!     RMINR = LOWEST EIGENVALUE OF INTEREST
!
!     NE   =  NUMBER OF PERMISSIBLE CHANGES OF EPSI
!
!     NIT = INTERATIONS TO AN EIGENVALUE
!
!     NEVM = MAXIMUM NUMBER OF EIGENVALUES DESIRED
!
!     IS  = STARTING SET COUNTER
!
!     IC  = COUNTER FOR CHANGE OF CONVERGENCE CRITERIA
!
!     NFOUND  = THE NUMBER OF EIGENVALUES FOUND TO DATA
!     NSTART = NUMBER OF TIMES THROUGH THE STARTING VALUES
!
!
!      IM = MASS MATRIX CONTROL BLOCK
!
!      IK = K MATRIX CONTROL BLOCK
!
!        A = M +P*K
!
!     IEV = EIGENVECTOR CONTROL BLOCK
!
!
!
!
!
   !>>>>EQUIVALENCE (Psave(1),Ps(1),Det(1),Ipdet(1))
!
   DATA name/4HDETM , 4H1   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! ----------------------------------------------------------------------
!
         ic = 0
!
!     CALCULATE THE NUMBER OF STARTING POINTS TO BE USED
!
         n2ev = 2*nev
         nn = n2ev
         srrmin = sqrt(rmin)
         srrmax = sqrt(rmax)
         fact = (srrmax-srrmin)/n2ev
         f1 = srrmin
         i = 0
         SPAG_Loop_1_1: DO
            i = i + 1
            f2 = f1 + fact
            x = dlog10(f2/f1)
            IF ( x>=1.0D0 ) THEN
               ix = x
               y = ix
               IF ( x/=y ) ix = ix + 1
               n2ev = n2ev + ix - 1
               f1 = f2
            ENDIF
            IF ( i>=nn ) THEN
!
!     CHECK AVAILABILITY OF CORE
!
               lc = 2*(korsz(psave)/2)
               ipsav = lc/2 - nevm
               ips = ipsav - n2ev - 1
               idet = ips - n2ev - 1
               ipdeta = 2*idet - n2ev - 2
               IF ( ipdeta<=0 ) THEN
                  CALL mesage(-8,0,name)
                  RETURN
               ELSE
                  lcore = lc - ipdeta + 1
!
!     COMPUTE THE STARTING POINTS
!
                  nn = ips + 1
                  ps(nn) = rmin
                  f1 = srrmin
                  i = 0
                  EXIT SPAG_Loop_1_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 2
      CASE (2)
         f2 = f1 + fact
         ratio = f2/f1
         x = dlog10(ratio)
         IF ( x<1.0D0 ) THEN
            i = i + 1
            nn = nn + 1
            ps(nn) = f2**2
         ELSE
            ix = x
            y = ix
            IF ( x/=y ) ix = ix + 1
            ratio = ratio**(1.0D0/ix)
            n = 0
            SPAG_Loop_1_2: DO
               n = n + 1
               i = i + 1
               nn = nn + 1
               ps(nn) = ps(nn-1)*ratio*ratio
               IF ( n>=ix ) EXIT SPAG_Loop_1_2
            ENDDO SPAG_Loop_1_2
         ENDIF
         f1 = f2
         IF ( i<n2ev ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         is = 1
         nd = 3
         iadd = 0
         isng = 0
         rmax = 1.05*rmax
         fact1 = epsi*sqrt(rmax)
!
!     CALCULATE DETERMINANTE OF FIRST 3 STARTING VALUES
!
         ENTRY detm2
         IF ( nstart==0 ) THEN
            DO n = 1 , nd
               nn = n + iadd
               nnp = nn + ips
               nnd = nn + idet
               nni = nn + ipdeta
               CALL eadd(-ps(nnp),prec)
               CALL detdet(det(nnd),ipdet(nni),ps(nnp),sml1,0.0D0,1)
            ENDDO
            IF ( nd==3 .AND. isng==3 ) RETURN 1
            IF ( is==1 ) iadd = 2
            nd = 1
         ENDIF
!
!     CALCULATE THE INITAL GUESS
!
!
!     PERMUT VALUES TO ORDER BY DETERMINANT
!
         DO n = 1 , 3
            ns = n - 1 + is
            nnd = ns + idet
            nni = ns + ipdeta
            nnp = ns + ips
            det1(n) = det(nnd)
            ipdet1(n) = ipdet(nni)
            ps1(n) = ps(nnp)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE detm1
