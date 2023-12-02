!*==invp2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE invp2(*)
   USE c_dcompx
   USE c_invpwx
   USE c_invpxx
   USE c_names
   USE c_reigkr
   USE c_sfact
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
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
   filea(1) = scr1(1)
   IF ( switch==1 ) THEN
      filel(1) = scr7
      fileu(1) = scr8
   ELSE
      filel(1) = scr2(1)
      fileu(1) = scr3
   ENDIF
   sr1fil = scr4
   sr2fil = scr5
   isr3fl = scr6
   ichl = 0
   filea(2) = dum(2)
   filea(3) = dum(3)
   filea(4) = dum(4)
   filea(5) = prec
   filea(6) = 0
   filea(7) = 0
   filel(5) = prec
   IF ( option==opt2 ) THEN
!
!     UNSYMMETRIC DECOMPOSITION SELECTED.
!
      DO i = 1 , 21
         ia(i) = filea(i)
      ENDDO
      iscr1 = scr4
      iscr2 = scr5
      iscr3 = scr6
      mz = korsz(q)
      CALL decomp(*100,q,q,q)
      il(3) = il(2)
      il(4) = lowtri
      CALL wrttrl(il)
      iu(3) = iu(2)
      iu(4) = uprtri
      iu(5) = il(5)
      CALL wrttrl(iu)
      RETURN
   ELSE
!
!     SYMMETRIC DECOMPOSITION SELECTED.
!
      nz = korsz(z)
      CALL sdcomp(*100,z,z,z)
      filel(3) = filel(2)
      filel(4) = lowtri
      CALL wrttrl(filel)
      RETURN
   ENDIF
 100  RETURN 1
END SUBROUTINE invp2
