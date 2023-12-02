!*==detm1.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE detm1() !HIDESTARS (*)
   IMPLICIT NONE
   USE C_DETMX
   USE C_REGEAN
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(1) :: det , ps
   REAL*8 :: f1 , f2 , fact , ratio , x , y
   INTEGER :: i , ix , lc , n , nn , nnd , nni , nnp , ns
   INTEGER , DIMENSION(1) :: ipdet
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: srrmax , srrmin
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
!
! ----------------------------------------------------------------------
!
   Ic = 0
!
!     CALCULATE THE NUMBER OF STARTING POINTS TO BE USED
!
   N2ev = 2*Nev
   nn = N2ev
   srrmin = sqrt(Rmin)
   srrmax = sqrt(Rmax)
   fact = (srrmax-srrmin)/N2ev
   f1 = srrmin
   i = 0
   DO
      i = i + 1
      f2 = f1 + fact
      x = dlog10(f2/f1)
      IF ( x>=1.0D0 ) THEN
         ix = x
         y = ix
         IF ( x/=y ) ix = ix + 1
         N2ev = N2ev + ix - 1
         f1 = f2
      ENDIF
      IF ( i>=nn ) THEN
!
!     CHECK AVAILABILITY OF CORE
!
         lc = 2*(korsz(Psave)/2)
         Ipsav = lc/2 - Nevm
         Ips = Ipsav - N2ev - 1
         Idet = Ips - N2ev - 1
         Ipdeta = 2*Idet - N2ev - 2
         IF ( Ipdeta<=0 ) THEN
            CALL mesage(-8,0,name)
            GOTO 99999
         ELSE
            Lcore = lc - Ipdeta + 1
!
!     COMPUTE THE STARTING POINTS
!
            nn = Ips + 1
            ps(nn) = Rmin
            f1 = srrmin
            i = 0
            EXIT
         ENDIF
      ENDIF
   ENDDO
 100  f2 = f1 + fact
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
      DO
         n = n + 1
         i = i + 1
         nn = nn + 1
         ps(nn) = ps(nn-1)*ratio*ratio
         IF ( n>=ix ) EXIT
      ENDDO
   ENDIF
   f1 = f2
   IF ( i<N2ev ) GOTO 100
   Is = 1
   Nd = 3
   Iadd = 0
   Isng = 0
   Rmax = 1.05*Rmax
   Fact1 = Epsi*sqrt(Rmax)
!
!     CALCULATE DETERMINANTE OF FIRST 3 STARTING VALUES
!
   ENTRY detm2
   IF ( Nstart==0 ) THEN
      DO n = 1 , Nd
         nn = n + Iadd
         nnp = nn + Ips
         nnd = nn + Idet
         nni = nn + Ipdeta
         CALL eadd(-ps(nnp),Prec)
         CALL detdet(det(nnd),ipdet(nni),ps(nnp),Sml1,0.0D0,1)
      ENDDO
      IF ( Nd==3 .AND. Isng==3 ) RETURN 1
      IF ( Is==1 ) Iadd = 2
      Nd = 1
   ENDIF
!
!     CALCULATE THE INITAL GUESS
!
!
!     PERMUT VALUES TO ORDER BY DETERMINANT
!
   DO n = 1 , 3
      ns = n - 1 + Is
      nnd = ns + Idet
      nni = ns + Ipdeta
      nnp = ns + Ips
      Det1(n) = det(nnd)
      Ipdet1(n) = ipdet(nni)
      Ps1(n) = ps(nnp)
   ENDDO
   RETURN
99999 END SUBROUTINE detm1
