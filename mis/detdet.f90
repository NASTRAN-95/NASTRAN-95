
SUBROUTINE detdet(Deta,Ipowr1,P,Sml1,Oldd,Iold)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER B , C , Fa(7) , Fa1(7) , Fc(7) , Fc1(7) , Fl(7) , Fl1(7) , Ia , Ibuck , Ichol , Il1 , Il2 , Im(21) , In(4) , In1(2) ,    &
         & In2(2) , In3(2) , Ipaav , Ipowr , Ipowra , Isew(33) , Isew1(5) , Isew2(20) , Ising , Ksystm(65) , Lama , Lc , Mach , Mz ,&
         & Ndcmp , Nevm , Nfound , Npole , Nsym , Nz , Nz1 , Option , Otpe , R , Sr1 , Sr11 , Sr2 , Sr21 , Sr3 , Sr31
   DOUBLE PRECISION Core(1) , Det , Det1(2) , Mindd , Sml2 , Sml21
   REAL Rminr
   COMMON /dcompx/ Fa , Fl , Fc , Sr1 , Sr2 , Sr3 , Det , Ipowr , Nz , Sml2
   COMMON /detmx / Isew , Ipaav , Isew1 , Ndcmp , Isew2 , Npole , Ising
   COMMON /machin/ Mach
   COMMON /regean/ Im , Ia , In , Lc , In1 , Mz , In2 , Rminr , In3 , Nevm , Il1 , Il2 , Nfound , Lama , Ibuck , Nsym
   COMMON /reigkr/ Option
   COMMON /sfact / Fa1 , Fc1 , Fl1 , Sr11 , Sr21 , Nz1 , Det1 , Ipowra , Sr31 , Mindd , Ichol , B , C , R , Sml21
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   INTEGER Iold
   DOUBLE PRECISION Oldd
   REAL Sml1
   DOUBLE PRECISION Deta(1) , P(1)
   INTEGER Ipowr1(1)
!
! Local variable declarations
!
   INTEGER i , ii , iprod , iprt , isave , nzz , sdet
   INTEGER korsz
   DOUBLE PRECISION prod
!
! End of declarations
!
!
!
!
!
!
!
   EQUIVALENCE (Ksystm(2),Otpe)
!
   DATA sdet/4HSDET/
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
      CALL sdcomp(*200,Core,Core,Core)
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
      CALL decomp(*200,Core,Core,Core)
!WKBD 10/94 SPR94011     FC(1) = SR2
      CALL wrttrl(Fc)
   ENDIF
   prod = 1.0D0
   IF ( iprt/=0 ) WRITE (Otpe,99001) P(1) , Det , Ipowr
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
         IF ( P(1)==Core(ii) ) GOTO 300
         prod = prod*(P(1)-Core(ii))
         CALL detm6(prod,iprod)
      ENDDO
   ENDIF
   Deta(1) = Det/prod
   Sml1 = Sml2
   Ipowr1(1) = Ipowr - iprod
   CALL detm6(Deta(1),Ipowr1(1))
 100  IF ( iprt/=0 ) WRITE (Otpe,99001) P(1) , Deta(1) , Ipowr1(1)
   RETURN
 200  Deta(1) = 0.0D0
   Ipowr1(1) = 1
   Sml1 = 1.0E-8
   Ising = Ising + 1
   isave = In(4)
   In(4) = Il2
   Il2 = isave
   GOTO 100
!
!     SET DK = DK-1
!
 300  Deta(1) = Oldd
   Sml1 = Sml2
   Ipowr1(1) = Iold
   GOTO 100
99001 FORMAT (2D16.7,I8)
END SUBROUTINE detdet
