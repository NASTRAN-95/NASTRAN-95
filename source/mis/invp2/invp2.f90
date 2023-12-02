!*==invp2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE invp2() !HIDESTARS (*)
USE C_DCOMPX
USE C_INVPWX
USE C_INVPXX
USE C_NAMES
USE C_REIGKR
USE C_SFACT
USE C_SYSTEM
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , prec
   INTEGER , SAVE :: opt2
   INTEGER , DIMENSION(1) :: q
   EXTERNAL decomp , korsz , sdcomp , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     INVP2 INITIALIZES THEN CALLS EITHER SDCOMP OR DECOMP DEPENDING ON
!     THE OPTION SELECTED ON THE EIGR CARD
!
   !>>>>EQUIVALENCE (Q(1),Z(1))
   !>>>>EQUIVALENCE (Ksystm(55),Prec)
   DATA opt2/4HUINV/
!
   Filea(1) = Scr1(1)
   IF ( Switch==1 ) THEN
      Filel(1) = Scr7
      Fileu(1) = Scr8
   ELSE
      Filel(1) = Scr2(1)
      Fileu(1) = Scr3
   ENDIF
   Sr1fil = Scr4
   Sr2fil = Scr5
   Isr3fl = Scr6
   Ichl = 0
   Filea(2) = Dum(2)
   Filea(3) = Dum(3)
   Filea(4) = Dum(4)
   Filea(5) = prec
   Filea(6) = 0
   Filea(7) = 0
   Filel(5) = prec
   IF ( Option==opt2 ) THEN
!
!     UNSYMMETRIC DECOMPOSITION SELECTED.
!
      DO i = 1 , 21
         Ia(i) = Filea(i)
      ENDDO
      Iscr1 = Scr4
      Iscr2 = Scr5
      Iscr3 = Scr6
      Mz = korsz(q)
      CALL decomp(*100,q,q,q)
      Il(3) = Il(2)
      Il(4) = Lowtri
      CALL wrttrl(Il)
      Iu(3) = Iu(2)
      Iu(4) = Uprtri
      Iu(5) = Il(5)
      CALL wrttrl(Iu)
      RETURN
   ELSE
!
!     SYMMETRIC DECOMPOSITION SELECTED.
!
      Nz = korsz(Z)
      CALL sdcomp(*100,Z,Z,Z)
      Filel(3) = Filel(2)
      Filel(4) = Lowtri
      CALL wrttrl(Filel)
      RETURN
   ENDIF
 100  RETURN 1
END SUBROUTINE invp2
