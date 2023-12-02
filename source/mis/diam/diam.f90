!*==diam.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE diam(Nc,Maxdeg,Nl,Nodesl,Idem,Maxlev,Ig,Ic,Ideg,Idis,Iw,Icc,Jg)
   IMPLICIT NONE
   USE C_BANDS
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nc
   INTEGER :: Maxdeg
   INTEGER :: Nl
   INTEGER , DIMENSION(1) :: Nodesl
   INTEGER :: Idem
   INTEGER :: Maxlev
   INTEGER , DIMENSION(1) :: Ig
   INTEGER , DIMENSION(1) :: Ic
   INTEGER , DIMENSION(1) :: Ideg
   INTEGER , DIMENSION(1) :: Idis
   INTEGER , DIMENSION(1) :: Iw
   INTEGER , DIMENSION(1) :: Icc
   INTEGER , DIMENSION(1) :: Jg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , md , ml
   EXTERNAL idist
!
! End of declarations rewritten by SPAG
!
!
!     DETERMINE NL STARTING POINTS AND STORE IN NODESL.
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!
   Nl = 0
   Maxlev = 600000
   DO i = 1 , Nn
      IF ( Nc==Ic(i) ) THEN
         IF ( Maxdeg>=Ideg(i) ) THEN
            md = idist(i,ml,Maxlev,Ig,Ic,Ideg,Idis,Iw,Icc,Jg)
            IF ( md<=0 ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
            IF ( ml<Maxlev ) THEN
               Maxlev = ml
               Nl = 1
               Nodesl(1) = i
            ELSEIF ( ml==Maxlev ) THEN
               IF ( Nl<Idem ) THEN
                  Nl = Nl + 1
                  Nodesl(Nl) = i
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
      ml = 1
      Nodesl(1) = i
      Maxlev = 0
   END SUBROUTINE spag_block_1
END SUBROUTINE diam
