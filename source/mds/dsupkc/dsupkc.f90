!*==dsupkc.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsupkc(Itin,Itout,A,B)
   USE c_system
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Itin
   INTEGER :: Itout
   REAL , DIMENSION(4) :: A
   REAL , DIMENSION(4) :: B
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(4) :: aa , bb
   INTEGER :: itout2 , iwrd1 , iwrd2 , k
   INTEGER , DIMENSION(4) , SAVE :: nwords
   REAL(REAL64) :: rd1 , rd2 , rdi1 , rdi2
   REAL :: rs1 , rs2 , ssign
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (aa,rs1,rd1) , (bb,rs2,rd2)
   !>>>>EQUIVALENCE (aa(3),rdi1) , (bb(3),rdi2)
   DATA nwords/1 , 2 , 2 , 4/
   iwrd1 = nwords(Itin)
   IF ( Itin/=Itout ) THEN
      IF ( Itout>64 ) THEN
         itout2 = Itout - 64
         iwrd2 = nwords(itout2)
         ssign = -1.0
      ELSE
         itout2 = Itout
         iwrd2 = nwords(Itout)
         ssign = 1.0
      ENDIF
!DIR$ NEXTSCALAR
      DO k = 1 , iwrd1
         aa(k) = A(k)
      ENDDO
      IF ( Itin==2 ) THEN
         IF ( itout2==2 ) THEN
            rd2 = ssign*rd1
         ELSEIF ( itout2==3 ) THEN
            bb(1) = ssign*rd1
            bb(2) = 0.
         ELSEIF ( itout2==4 ) THEN
            rd2 = ssign*rd1
            rdi2 = 0.
         ELSE
            rs2 = ssign*rd1
         ENDIF
      ELSEIF ( Itin==3 ) THEN
         IF ( itout2==2 ) THEN
            rd2 = ssign*aa(1)
         ELSEIF ( itout2==3 ) THEN
            bb(1) = ssign*aa(1)
            bb(2) = ssign*aa(2)
         ELSEIF ( itout2==4 ) THEN
            rd2 = ssign*aa(1)
            rdi2 = ssign*aa(2)
         ELSE
            rs2 = ssign*aa(1)
         ENDIF
      ELSEIF ( Itin==4 ) THEN
         IF ( itout2==2 ) THEN
            rd2 = ssign*rd1
         ELSEIF ( itout2==3 ) THEN
            bb(1) = ssign*rd1
            bb(2) = ssign*rdi1
         ELSEIF ( itout2==4 ) THEN
            rd2 = ssign*rd1
            rdi2 = ssign*rdi1
         ELSE
            rs2 = ssign*rd1
         ENDIF
      ELSEIF ( itout2==2 ) THEN
         rd2 = ssign*rs1
      ELSEIF ( itout2==3 ) THEN
         bb(1) = ssign*rs1
         bb(2) = 0.
      ELSEIF ( itout2==4 ) THEN
         rd2 = ssign*rs1
         rdi2 = 0.
      ELSE
         rs2 = ssign*rs1
      ENDIF
!DIR$ NEXTSCALAR
      DO k = 1 , iwrd2
         B(k) = bb(k)
      ENDDO
   ELSE
!DIR$ NEXTSCALAR
      DO k = 1 , iwrd1
         B(k) = A(k)
      ENDDO
   ENDIF
END SUBROUTINE dsupkc
