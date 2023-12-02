!*==findc.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION findc(B,Bbar,N,Ix,Jx)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: findc
   INTEGER :: B
   INTEGER :: Bbar
   INTEGER :: N
   INTEGER , DIMENSION(1) :: Ix
   INTEGER , DIMENSION(1) :: Jx
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ic , icc , j , k , l
!
! End of declarations rewritten by SPAG
!
!*******
!     PICK OUT PAIRS OF NUMBERS FOR ACTIVE ROWS
!*******
   icc = 0
   j = 1
   DO i = 1 , N
      IF ( i-Ix(i)>Bbar ) THEN
         Jx(j) = i + B - 1
         Jx(j+1) = Ix(i)
         j = j + 2
      ENDIF
   ENDDO
   j = j - 1
   IF ( j/=0 ) THEN
      SPAG_Loop_1_1: DO k = 1 , j , 2
         IF ( (j-k-1)/2<icc ) EXIT SPAG_Loop_1_1
         ic = 0
         DO l = k , j , 2
            IF ( Jx(k)>=Jx(l+1) ) ic = ic + 1
         ENDDO
         icc = max0(icc,ic)
      ENDDO SPAG_Loop_1_1
   ENDIF
   findc = icc
END FUNCTION findc
