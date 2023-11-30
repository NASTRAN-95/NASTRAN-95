
SUBROUTINE detm4
   IMPLICIT NONE
   DOUBLE PRECISION Det(1) , Det1(4) , Detx(4) , P(4) , Ps(1) , Ps1(4) , Psave(1)
   REAL Epsi , Fact1 , Rmax , Rmin , Rminr , Sml1
   INTEGER Iadd , Ic , Idet , Iev(7) , Ifail , Ik(7) , Im(7) , Ipdet(1) , Ipdet1(4) , Ipdeta , Ipdetx(4) , Ips , Ipsav , Is , K ,   &
         & L2 , Lama , Lcore , Mz , N2ev , Nd , Ne , Nev , Nevm , Nfound , Nit , Nsmove , Prec , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , &
         & Scr6 , Scr7 , U1 , U2
   COMMON /detmx / P , Detx , Ps1 , Det1 , N2ev , Ipsav , Ips , Idet , Ipdeta , Prec , U1 , U2 , Ic , Nsmove , L2 , Is , Nd , Iadd ,&
                 & Sml1 , Ipdetx , Ipdet1 , Ifail , K , Fact1
   COMMON /regean/ Im , Ik , Iev , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Lcore , Rmax , Rmin , Mz , Nev , Epsi , Rminr , Ne , Nit ,    &
                 & Nevm , Scr6 , Scr7 , Nfound , Lama
   COMMON /zzzzzz/ Psave
   REAL eps1
   INTEGER i , n , n2ev2 , nfnd , nn , nnd , nni , nnp , nnz
   EQUIVALENCE (Psave(1),Ps(1),Det(1),Ipdet(1))
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
!
!     NSMOVE = THE NUMBER OF TIMES THE STATTING POINTS HAVE BEEN MOVED
!
!      IM = MASS MATRIX CONTROL BLOCK
!
!      IK = K MATRIX CONTROL BLOCK
!
!        A = M +P*K
!
!     IEV = EIGENVECTOR CONTROL BLOCK
!
   nn = Ipsav + Nfound
   Psave(nn) = P(3)
   eps1 = Fact1*dsqrt(dabs(P(3)))
   DO n = 1 , 3
      nn = n + Iadd - 2
      nnp = nn + Ips
      IF ( dabs(Ps(nnp)-P(3))<400.*eps1 ) THEN
         DO
            Ps(nnp) = Ps(nnp) + 2.E3*eps1
            Nsmove = Nsmove + 1
            IF ( Nfound/=1 ) THEN
               nfnd = Nfound - 1
               DO i = 1 , nfnd
                  nnz = Ipsav + i
                  IF ( dabs(Ps(nnp)-Psave(nnz))<=400.*eps1 ) GOTO 20
               ENDDO
            ENDIF
            nnd = nn + Idet
            nni = nn + Ipdeta
            CALL eadd(-Ps(nnp),Prec)
            CALL detdet(Det(nnd),Ipdet(nni),Ps(nnp),Sml1,0.0D0,1)
            EXIT
 20      ENDDO
      ENDIF
   ENDDO
   n2ev2 = Iadd + Nd
   DO i = 1 , n2ev2
      nnd = i + Idet
      nnp = i + Ips
      nni = i + Ipdeta
      Det(nnd) = Det(nnd)/(Ps(nnp)-P(3))
      CALL detm6(Det(nnd),Ipdet(nni))
   ENDDO
   DO i = 1 , 3
      Det1(i) = Det1(i)/(Ps1(i)-P(3))
   ENDDO
END SUBROUTINE detm4
