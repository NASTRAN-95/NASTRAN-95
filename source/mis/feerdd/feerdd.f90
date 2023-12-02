!*==feerdd.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE feerdd
   IMPLICIT NONE
   USE C_FEERCX
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
   INTEGER , DIMENSION(28) , SAVE :: jfrcx
   INTEGER , DIMENSION(4) , SAVE :: kfrcx , lfrcx
   INTEGER , SAVE :: mfrcx
!
! End of declarations rewritten by SPAG
!
!*******
!
!     SUBROUTINE TO INITIALIZE COMMON /FEERCX/
!
!*******
!
!
   DATA jfrcx/101 , 6*0 , 102 , 6*0 , 201 , 6*0 , 202 , 6*0/
   DATA kfrcx/301 , 302 , 303 , 304/
   DATA lfrcx/305 , 306 , 307 , 308/
   DATA mfrcx/204/
!
   DO i = 1 , 28
      Ifrcx(i) = jfrcx(i)
   ENDDO
   DO i = 1 , 4
      Ifrcx(i+28) = kfrcx(i)
      Ifrcx(i+32) = lfrcx(i)
   ENDDO
   Ifrcx(37) = mfrcx
!
END SUBROUTINE feerdd
