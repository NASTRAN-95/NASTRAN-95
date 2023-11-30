
SUBROUTINE fa1pka(A,M1k,M1b,Eiv,Ncore,N)
   IMPLICIT NONE
   INTEGER N , Ncore
   REAL A(1) , Eiv(1) , M1b(1) , M1k(1)
   INTEGER i , ih , ii , il , im , imb , imi , imk , iz , j , k , l39 , n2 , n4 , name(2) , nheige , nheigs
!
!     FA1PKA BUILDS THE MATRIX FOR ALLMAT
!
   DATA name/4HFA1P , 4HKA  /
   DATA nheigs , nheige/4HEIGS , 4HEIGE/
   n2 = N*2
   iz = 0
   imk = N
   imi = N*N*2
   imb = imi + N
   k = 0
   DO i = 1 , N
      DO j = 1 , N
         k = k + 1
         A(iz+j) = 0.0
         A(imk+j) = M1k(k)
         A(imb+j) = M1b(k)
         A(imi+j) = 0.0
         IF ( i==j ) A(imi+j) = 1.0
      ENDDO
      iz = iz + n2
      imk = imk + n2
      imi = imi + n2
      imb = imb + n2
   ENDDO
!
!     CALL HSBG AND ATEIG FOR EIVENVALUES
!
   n4 = n2*2
   il = 1
   ih = il + n2
   im = ih + n4
   ii = im + n4
   IF ( ii>Ncore ) CALL mesage(-8,0,name)
   CALL sswtch(39,l39)
   IF ( l39/=0 ) CALL conmsg(nheigs,1,0)
   CALL hsbg(n2,A,n2,A)
   CALL ateig(n2,A,Eiv(ih),Eiv(im),Eiv(il),n2,A,Eiv(ih),Eiv(im))
   il = 0
   DO i = 1 , n2
      Eiv(i+il) = Eiv(i+ih-1)
      Eiv(i+il+1) = Eiv(i+im-1)
      il = il + 1
   ENDDO
   IF ( l39/=0 ) CALL conmsg(nheige,1,0)
END SUBROUTINE fa1pka
