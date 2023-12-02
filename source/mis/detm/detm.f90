!*==detm.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE detm
   USE c_detmx
   USE c_regean
   USE iso_fortran_env
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
         nstart = 0
         lcore = 0
         ndcmp = 0
         nsmove = 0
         npole = 0
         iterm = 1
         iffnd = 0
         nfail = 0
!*****
         prec = ik(5)
!*****
         iscr7 = scr7
         IF ( mz>nevm ) GOTO 60
         IF ( im(1)<=0 ) THEN
!
!     MASS MATRIX PURGED -- ASSUME IDENTITY
!
            im(1) = ik(1)
            CALL rdtrl(im(1))
            im(4) = 8
         ENDIF
         CALL detm1(*80)
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            CALL klock(itime1)
            CALL detm3(*40,*60,*20)
            nfound = nfound + 1
            CALL fdvect(sml1,p(3))
            idone = nfound + 1
            IF ( mz>0 ) idone = idone + mz
            CALL detm4
            IF ( idone>nevm ) THEN
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
 20      CALL mesage(45,nevm-idone,name)
         iterm = 3
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 40      CALL detm2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      iterm = 2
         spag_nextblock_1 = 3
      CASE (3)
         scr7 = iscr7
         CALL detm5
         RETURN
!
!     SINGULAR MATRIX EVERYWHERE
!
 80      iterm = 4
         spag_nextblock_1 = 3
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE detm
