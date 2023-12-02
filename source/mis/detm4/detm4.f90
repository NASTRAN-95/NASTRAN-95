!*==detm4.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE detm4
USE C_DETMX
USE C_REGEAN
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: det , ps
   REAL :: eps1
   INTEGER :: i , n , n2ev2 , nfnd , nn , nnd , nni , nnp , nnz
   INTEGER , DIMENSION(1) :: ipdet
   EXTERNAL detdet , detm6 , eadd
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (Psave(1),Ps(1),Det(1),Ipdet(1))
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
      IF ( dabs(ps(nnp)-P(3))<400.*eps1 ) THEN
         SPAG_Loop_2_1: DO
            ps(nnp) = ps(nnp) + 2.E3*eps1
            Nsmove = Nsmove + 1
            IF ( Nfound/=1 ) THEN
               nfnd = Nfound - 1
               DO i = 1 , nfnd
                  nnz = Ipsav + i
                  IF ( dabs(ps(nnp)-Psave(nnz))<=400.*eps1 ) CYCLE SPAG_Loop_2_1
               ENDDO
            ENDIF
            nnd = nn + Idet
            nni = nn + Ipdeta
            CALL eadd(-ps(nnp),Prec)
            CALL detdet(det(nnd),ipdet(nni),ps(nnp),Sml1,0.0D0,1)
            EXIT SPAG_Loop_2_1
         ENDDO SPAG_Loop_2_1
      ENDIF
   ENDDO
   n2ev2 = Iadd + Nd
   DO i = 1 , n2ev2
      nnd = i + Idet
      nnp = i + Ips
      nni = i + Ipdeta
      det(nnd) = det(nnd)/(ps(nnp)-P(3))
      CALL detm6(det(nnd),ipdet(nni))
   ENDDO
   DO i = 1 , 3
      Det1(i) = Det1(i)/(Ps1(i)-P(3))
   ENDDO
END SUBROUTINE detm4
