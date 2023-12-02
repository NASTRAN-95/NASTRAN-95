!*==detm4.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE detm4
   USE c_detmx
   USE c_regean
   USE c_zzzzzz
   USE iso_fortran_env
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
   nn = ipsav + nfound
   psave(nn) = p(3)
   eps1 = fact1*dsqrt(dabs(p(3)))
   DO n = 1 , 3
      nn = n + iadd - 2
      nnp = nn + ips
      IF ( dabs(ps(nnp)-p(3))<400.*eps1 ) THEN
         SPAG_Loop_2_1: DO
            ps(nnp) = ps(nnp) + 2.E3*eps1
            nsmove = nsmove + 1
            IF ( nfound/=1 ) THEN
               nfnd = nfound - 1
               DO i = 1 , nfnd
                  nnz = ipsav + i
                  IF ( dabs(ps(nnp)-psave(nnz))<=400.*eps1 ) CYCLE SPAG_Loop_2_1
               ENDDO
            ENDIF
            nnd = nn + idet
            nni = nn + ipdeta
            CALL eadd(-ps(nnp),prec)
            CALL detdet(det(nnd),ipdet(nni),ps(nnp),sml1,0.0D0,1)
            EXIT SPAG_Loop_2_1
         ENDDO SPAG_Loop_2_1
      ENDIF
   ENDDO
   n2ev2 = iadd + nd
   DO i = 1 , n2ev2
      nnd = i + idet
      nnp = i + ips
      nni = i + ipdeta
      det(nnd) = det(nnd)/(ps(nnp)-p(3))
      CALL detm6(det(nnd),ipdet(nni))
   ENDDO
   DO i = 1 , 3
      det1(i) = det1(i)/(ps1(i)-p(3))
   ENDDO
END SUBROUTINE detm4
