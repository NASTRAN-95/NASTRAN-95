
SUBROUTINE feerdd
   IMPLICIT NONE
   INTEGER Ifrcx(37)
   COMMON /feercx/ Ifrcx
   INTEGER i , jfrcx(28) , kfrcx(4) , lfrcx(4) , mfrcx
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