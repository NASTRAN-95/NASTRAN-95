!*==feerdd.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE feerdd
   IMPLICIT NONE
   USE c_feercx
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
!
! Local variable declarations rewritten by SPAG
!
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
      ifrcx(i) = jfrcx(i)
   ENDDO
   DO i = 1 , 4
      ifrcx(i+28) = kfrcx(i)
      ifrcx(i+32) = lfrcx(i)
   ENDDO
   ifrcx(37) = mfrcx
!
END SUBROUTINE feerdd
