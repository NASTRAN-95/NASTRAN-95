
INTEGER FUNCTION findc(B,Bbar,N,Ix,Jx)
   IMPLICIT NONE
   INTEGER B , Bbar , N
   INTEGER Ix(1) , Jx(1)
   INTEGER i , ic , icc , j , k , l
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
      DO k = 1 , j , 2
         IF ( (j-k-1)/2<icc ) EXIT
         ic = 0
         DO l = k , j , 2
            IF ( Jx(k)>=Jx(l+1) ) ic = ic + 1
         ENDDO
         icc = max0(icc,ic)
      ENDDO
   ENDIF
   findc = icc
END FUNCTION findc