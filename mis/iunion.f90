
FUNCTION iunion(I1,I2)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER I1 , I2
   INTEGER iunion
!
! Local variable declarations
!
   INTEGER i , ii , j , k(6,2) , kk(6) , l , r
!
! End of declarations
!
!
!     I1 AND I2 ARE .GT. 0 BUT .LE. 654321 AND CONSIST OF ANY UNIQUE
!     COMBINATION OF THE DIGITS 1 THRU 6
!
!
!              DECODE I1 INTO K(*,1)
   i = 1
   ii = I1
   ASSIGN 100 TO r
   GOTO 300
!
!              DECODE I2 INTO K(*,2)
 100  i = 2
   ii = I2
   ASSIGN 200 TO r
   GOTO 300
!
!              FORM UNION OF K(*,1) AND K(*,2) IN KK(*)
 200  DO i = 1 , 6
      kk(i) = 0
      IF ( k(i,1)==i .OR. k(i,2)==i ) kk(i) = i
   ENDDO
!
!              PACK KK(*) INTO IUNION
   j = 1
   l = 0
   DO i = 1 , 6
      IF ( kk(i)/=0 ) THEN
         IF ( l>0 ) j = 10*j
         l = l + j*i
      ENDIF
   ENDDO
!
   iunion = l
!
   RETURN
!
!
 300  DO j = 1 , 6
      k(j,i) = 0
   ENDDO
   DO j = 1 , 6
      l = ii - 10*(ii/10)
      ii = (ii-l)/10
      IF ( l==0 ) EXIT
      k(l,i) = l
   ENDDO
   GOTO r
!
END FUNCTION iunion
