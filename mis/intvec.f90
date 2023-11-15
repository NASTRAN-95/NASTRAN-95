
SUBROUTINE intvec(Vector)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ncpw
   REAL Skip(40)
   COMMON /system/ Skip , Ncpw
!
! Dummy argument declarations
!
   INTEGER Vector
!
! Local variable declarations
!
   INTEGER char , i , k , n , nshape , vec(4) , vecwrd , xyzr(4)
   INTEGER klshft , krshft
!
! End of declarations
!
!
   DATA xyzr/1HX , 1HY , 1HZ , 1HR/
   DATA n/1HN/
!
   nshape = 0
   vecwrd = Vector
   IF ( vecwrd/=0 ) THEN
      DO i = 1 , 4
         vec(i) = 0
      ENDDO
!
!     SEPARATE THE FOUR CHARACTERS IN -VECWRD- (ANY COMBINATION OF THE
!     CHARACTERS X, Y, Z, AND R.
!
      DO k = 1 , 4
         char = klshft(vecwrd,(k-1))
         char = krshft(char,(Ncpw-1))
         DO i = 1 , 4
            IF ( char==krshft(xyzr(i),(Ncpw-1)) ) GOTO 20
         ENDDO
         IF ( char==krshft(n,(Ncpw-1)) ) nshape = 1
         CYCLE
 20      vec(i) = 1
      ENDDO
!
      Vector = vec(1) + 2*vec(2) + 4*vec(3) + 8*vec(4)
      IF ( Vector==8 ) Vector = 15
      IF ( nshape==1 ) Vector = -Vector
   ENDIF
END SUBROUTINE intvec
