
SUBROUTINE xsave
   IMPLICIT NONE
   INTEGER Ioscr(7) , Ipar(1) , Ivps(1)
   COMMON /blank / Ipar
   COMMON /oscent/ Ioscr
   COMMON /xvps  / Ivps
   INTEGER i1 , i2 , j , k , l , n
!     THE PURPOSE OF THIS ROUTINE IS TO PERFORM THE FUNCTIONS ASSIGNED
!     TO THE SAVE DMAP INSTRUCTION.
!
!     GET NUMBER OF PARAMETERS FROM OSCAR
   n = Ioscr(7)*2 + 6
   DO i1 = 8 , n , 2
!     GET VPS POINTER AND POINTER TO VALUE IN BLANK COMMON.
      j = Ioscr(i1)
      k = Ioscr(i1+1)
!     GET LENGTH OF VALUE FROM VPS
      l = Ivps(j-1)
!     TRANSFER VALUE FROM BLANK COMMON TO VPS
      DO i2 = 1 , l
         Ivps(j) = Ipar(k)
         j = j + 1
         k = k + 1
      ENDDO
   ENDDO
END SUBROUTINE xsave