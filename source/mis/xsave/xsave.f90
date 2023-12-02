!*==xsave.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE xsave
   IMPLICIT NONE
   USE C_BLANK
   USE C_OSCENT
   USE C_XVPS
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i1 , i2 , j , k , l , n
!
! End of declarations rewritten by SPAG
!
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
