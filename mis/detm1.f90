
SUBROUTINE detm1(*)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION Det(1) , Det1(4) , Detx(4) , P(4) , Ps(1) , Ps1(4) , Psave(1)
   REAL Epsi , Fact1 , Rmax , Rmin , Rminr , Sml1
   INTEGER Iadd , Ic , Idet , Iev(7) , Ifail , Iffnd , Ik(7) , Im(7) , Ipdet(1) , Ipdet1(4) , Ipdeta , Ipdetx(4) , Ips , Ipsav ,    &
         & Is , Isng , K , L1 , L2 , Lama , Lcore , Mz , N2ev , Nd , Ne , Nev , Nevm , Nfail , Nfound , Nit , Npole , Nstart ,      &
         & Prec , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Scr6 , Scr7 , U2
   COMMON /detmx / P , Detx , Ps1 , Det1 , N2ev , Ipsav , Ips , Idet , Ipdeta , Prec , Nstart , U2 , Ic , L1 , L2 , Is , Nd , Iadd ,&
                 & Sml1 , Ipdetx , Ipdet1 , Ifail , K , Fact1 , Iffnd , Nfail , Npole , Isng
   COMMON /regean/ Im , Ik , Iev , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Lcore , Rmax , Rmin , Mz , Nev , Epsi , Rminr , Ne , Nit ,    &
                 & Nevm , Scr6 , Scr7 , Nfound , Lama
   COMMON /zzzzzz/ Psave
!
! Local variable declarations
!
   DOUBLE PRECISION f1 , f2 , fact , ratio , x , y
   INTEGER i , ix , lc , n , name(2) , nn , nnd , nni , nnp , ns
   INTEGER korsz
   REAL srrmax , srrmin
!
! End of declarations
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
   EQUIVALENCE (Psave(1),Ps(1),Det(1),Ipdet(1))
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
            Ps(nn) = Rmin
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
      Ps(nn) = f2**2
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
         Ps(nn) = Ps(nn-1)*ratio*ratio
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
         CALL eadd(-Ps(nnp),Prec)
         CALL detdet(Det(nnd),Ipdet(nni),Ps(nnp),Sml1,0.0D0,1)
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
      Det1(n) = Det(nnd)
      Ipdet1(n) = Ipdet(nni)
      Ps1(n) = Ps(nnp)
   ENDDO
   RETURN
99999 END SUBROUTINE detm1
