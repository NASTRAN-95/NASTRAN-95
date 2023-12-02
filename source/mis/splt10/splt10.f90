!*==splt10.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE splt10(Icomp,Comps,Nc)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Icomp
   INTEGER , DIMENSION(9) :: Comps
   INTEGER :: Nc
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ic , ix , jx
   EXTERNAL sort
!
! End of declarations rewritten by SPAG
!
   IF ( Icomp==0 ) Icomp = 1
   ic = Icomp
   Nc = 0
   SPAG_Loop_1_1: DO i = 1 , 9
      ix = ic/10
      jx = ic - 10*ix
      ic = ix
      IF ( jx/=0 ) THEN
         Nc = Nc + 1
         Comps(Nc) = jx
      ENDIF
      IF ( ic==0 ) EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
   IF ( Nc==1 ) RETURN
   CALL sort(0,0,1,1,Comps,Nc)
!
!     REMOVE DUPLICATES
   ix = 1
   DO i = 2 , Nc
      IF ( Comps(i)/=Comps(i-1) ) THEN
         ix = ix + 1
         Comps(ix) = Comps(i)
      ENDIF
   ENDDO
   Nc = ix
END SUBROUTINE splt10
