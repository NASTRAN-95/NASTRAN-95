!*==detdet.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE detdet(Deta,Ipowr1,P,Sml1,Oldd,Iold)
   USE c_dcompx
   USE c_detmx
   USE c_machin
   USE c_regean
   USE c_reigkr
   USE c_sfact
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
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
         isave = in(4)
         in(4) = il2
         il2 = isave
         nzz = (korsz(core)/2)*2 - lc
         ndcmp = ndcmp + 1
         IF ( option==sdet ) THEN
!
!     SET UP FOR SYMMETRIC DECOMPOSITION
!
            fa1(1) = ia
            CALL rdtrl(fa1)
            fl1(1) = in(1)
            fc1(1) = in(4)
            ichol = 0
            IF ( ndcmp==1 ) b = 0
            nz1 = nzz
            DO i = 2 , 5
               fl1(i) = fa1(i)
               fc1(i) = fa1(i)
            ENDDO
            sr11 = in(3)
            sr21 = in(2)
            sr31 = il1
            IF ( mach==4 .OR. mach==12 ) fl1(5) = 1
            CALL sdcomp(*20,core,core,core)
            fc1(5) = fl1(5)
            CALL wrttrl(fc1)
            ipowr = ipowra
            det = det1(1)
            Sml1 = sml21
         ELSE
            fa(1) = ia
            CALL rdtrl(fa)
!
!     SET UP FOR UNSYMMETRIC
!
            nz = nzz
!
!
!     PUT IN TO PREVENT REWRITE
!
!     FA1(1) = -FA1(1)
!
!WKBD 10/94 SPR94011 FA(1) = -FA(1)
            fl(1) = in(1)
            fc(1) = in(2)
            DO i = 2 , 5
               fl(i) = fa(i)
               fc(i) = fa(i)
            ENDDO
            sr1 = in(3)
            sr2 = in(4)
            sr3 = il1
            CALL decomp(*20,core,core,core)
!WKBD 10/94 SPR94011     FC(1) = SR2
            CALL wrttrl(fc)
         ENDIF
         prod = 1.0D0
         IF ( iprt/=0 ) WRITE (otpe,99001) P(1) , det , ipowr
         iprod = 0
         IF ( mz/=0 ) THEN
            ii = iabs(mz)
            DO i = 1 , ii
               prod = prod*P(1)
               CALL detm6(prod,iprod)
            ENDDO
         ENDIF
!
!     TAKE OUT  POLE AT  RMINR
!
         IF ( npole/=0 ) THEN
            DO i = 1 , npole
               prod = prod*(P(1)-rminr)
               CALL detm6(prod,iprod)
            ENDDO
         ENDIF
         IF ( nfound/=0 ) THEN
            DO i = 1 , nfound
               ii = ipaav + i
               IF ( P(1)==core(ii) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               prod = prod*(P(1)-core(ii))
               CALL detm6(prod,iprod)
            ENDDO
         ENDIF
         Deta(1) = det/prod
         Sml1 = sml2
         Ipowr1(1) = ipowr - iprod
         CALL detm6(Deta(1),Ipowr1(1))
         spag_nextblock_1 = 2
      CASE (2)
         IF ( iprt/=0 ) WRITE (otpe,99001) P(1) , Deta(1) , Ipowr1(1)
         RETURN
 20      Deta(1) = 0.0D0
         Ipowr1(1) = 1
         Sml1 = 1.0E-8
         ising = ising + 1
         isave = in(4)
         in(4) = il2
         il2 = isave
         spag_nextblock_1 = 2
      CASE (3)
!
!     SET DK = DK-1
!
         Deta(1) = Oldd
         Sml1 = sml2
         Ipowr1(1) = Iold
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99001 FORMAT (2D16.7,I8)
END SUBROUTINE detdet
