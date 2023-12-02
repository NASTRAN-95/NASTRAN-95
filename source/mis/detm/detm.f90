!*==detm.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE detm
USE C_DETMX
USE C_REGEAN
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: idone , iscr7 , itime1 , itime2 , itleft
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL detm1 , detm2 , detm3 , detm4 , detm5 , fdvect , klock , mesage , rdtrl , tmtogo
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   DATA name/4HDETE , 4HRM  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
!      IM = MASS MATRIX CONTROL BLOCK
!
!      IK = K MATRIX CONTROL BLOCK
!
!        A = M +P*K
!
!     IEV = EIGENVECTOR CONTROL BLOCK
!
         Nstart = 0
         Lcore = 0
         Ndcmp = 0
         Nsmove = 0
         Npole = 0
         Iterm = 1
         Iffnd = 0
         Nfail = 0
!*****
         Prec = Ik(5)
!*****
         iscr7 = Scr7
         IF ( Mz>Nevm ) GOTO 60
         IF ( Im(1)<=0 ) THEN
!
!     MASS MATRIX PURGED -- ASSUME IDENTITY
!
            Im(1) = Ik(1)
            CALL rdtrl(Im(1))
            Im(4) = 8
         ENDIF
         CALL detm1(*80)
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            CALL klock(itime1)
            CALL detm3(*40,*60,*20)
            Nfound = Nfound + 1
            CALL fdvect(Sml1,P(3))
            idone = Nfound + 1
            IF ( Mz>0 ) idone = idone + Mz
            CALL detm4
            IF ( idone>Nevm ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL klock(itime2)
            CALL tmtogo(itleft)
            IF ( 2*(itime2-itime1)>itleft ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
!
!     INSUFFICIENT TIME TO FIND ANOTHER E. V.
!
 20      CALL mesage(45,Nevm-idone,name)
         Iterm = 3
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 40      CALL detm2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      Iterm = 2
         spag_nextblock_1 = 3
      CASE (3)
         Scr7 = iscr7
         CALL detm5
         RETURN
!
!     SINGULAR MATRIX EVERYWHERE
!
 80      Iterm = 4
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE detm
