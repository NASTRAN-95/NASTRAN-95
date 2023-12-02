!*==detdet.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE detdet(Deta,Ipowr1,P,Sml1,Oldd,Iold)
USE C_DCOMPX
USE C_DETMX
USE C_MACHIN
USE C_REGEAN
USE C_REIGKR
USE C_SFACT
USE C_SYSTEM
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: Deta
   INTEGER , DIMENSION(1) :: Ipowr1
   REAL(REAL64) , DIMENSION(1) :: P
   REAL :: Sml1
   REAL(REAL64) :: Oldd
   INTEGER :: Iold
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ii , iprod , iprt , isave , nzz , otpe
   REAL(REAL64) :: prod
   INTEGER , SAVE :: sdet
   EXTERNAL decomp , detm6 , korsz , rdtrl , sdcomp , sswtch , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!
!
!
!
!
   !>>>>EQUIVALENCE (Ksystm(2),Otpe)
!
   DATA sdet/4HSDET/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! ----------------------------------------------------------------------
!
         CALL sswtch(7,iprt)
         isave = In(4)
         In(4) = Il2
         Il2 = isave
         nzz = (korsz(Core)/2)*2 - Lc
         Ndcmp = Ndcmp + 1
         IF ( Option==sdet ) THEN
!
!     SET UP FOR SYMMETRIC DECOMPOSITION
!
            Fa1(1) = Ia
            CALL rdtrl(Fa1)
            Fl1(1) = In(1)
            Fc1(1) = In(4)
            Ichol = 0
            IF ( Ndcmp==1 ) B = 0
            Nz1 = nzz
            DO i = 2 , 5
               Fl1(i) = Fa1(i)
               Fc1(i) = Fa1(i)
            ENDDO
            Sr11 = In(3)
            Sr21 = In(2)
            Sr31 = Il1
            IF ( Mach==4 .OR. Mach==12 ) Fl1(5) = 1
            CALL sdcomp(*20,Core,Core,Core)
            Fc1(5) = Fl1(5)
            CALL wrttrl(Fc1)
            Ipowr = Ipowra
            Det = Det1(1)
            Sml1 = Sml21
         ELSE
            Fa(1) = Ia
            CALL rdtrl(Fa)
!
!     SET UP FOR UNSYMMETRIC
!
            Nz = nzz
!
!
!     PUT IN TO PREVENT REWRITE
!
!     FA1(1) = -FA1(1)
!
!WKBD 10/94 SPR94011 FA(1) = -FA(1)
            Fl(1) = In(1)
            Fc(1) = In(2)
            DO i = 2 , 5
               Fl(i) = Fa(i)
               Fc(i) = Fa(i)
            ENDDO
            Sr1 = In(3)
            Sr2 = In(4)
            Sr3 = Il1
            CALL decomp(*20,Core,Core,Core)
!WKBD 10/94 SPR94011     FC(1) = SR2
            CALL wrttrl(Fc)
         ENDIF
         prod = 1.0D0
         IF ( iprt/=0 ) WRITE (otpe,99001) P(1) , Det , Ipowr
         iprod = 0
         IF ( Mz/=0 ) THEN
            ii = iabs(Mz)
            DO i = 1 , ii
               prod = prod*P(1)
               CALL detm6(prod,iprod)
            ENDDO
         ENDIF
!
!     TAKE OUT  POLE AT  RMINR
!
         IF ( Npole/=0 ) THEN
            DO i = 1 , Npole
               prod = prod*(P(1)-Rminr)
               CALL detm6(prod,iprod)
            ENDDO
         ENDIF
         IF ( Nfound/=0 ) THEN
            DO i = 1 , Nfound
               ii = Ipaav + i
               IF ( P(1)==Core(ii) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               prod = prod*(P(1)-Core(ii))
               CALL detm6(prod,iprod)
            ENDDO
         ENDIF
         Deta(1) = Det/prod
         Sml1 = Sml2
         Ipowr1(1) = Ipowr - iprod
         CALL detm6(Deta(1),Ipowr1(1))
         spag_nextblock_1 = 2
      CASE (2)
         IF ( iprt/=0 ) WRITE (otpe,99001) P(1) , Deta(1) , Ipowr1(1)
         RETURN
 20      Deta(1) = 0.0D0
         Ipowr1(1) = 1
         Sml1 = 1.0E-8
         Ising = Ising + 1
         isave = In(4)
         In(4) = Il2
         Il2 = isave
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
!     SET DK = DK-1
!
         Deta(1) = Oldd
         Sml1 = Sml2
         Ipowr1(1) = Iold
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99001 FORMAT (2D16.7,I8)
END SUBROUTINE detdet
