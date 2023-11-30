
SUBROUTINE egnvct(C1,C2,Eigen,C3,N1,N2,N)
   IMPLICIT NONE
   COMPLEX Eigen
   INTEGER N
   COMPLEX C1(N,N) , C2(N) , C3(N)
   INTEGER N1(N) , N2(N)
   COMPLEX d1 , d2 , d3 , d4 , d5 , d6 , d8
   INTEGER i , i1 , ii2 , ii3 , ii4 , j , j1 , k , k5 , k6
   REAL x1 , x2
!
!     SUBROUTINE TO OBTAIN EIGENVECTOR FROM REAL NON-SYMMETRIC
!     MATRICES FOR WHICH THE EIGENVALUE IS KNOWN.  THE METHOD
!     USED IS THE DIRECT METHOD OUTLINED IN ERR-FW-   BY DR.
!     A. M. CUNNINGHAM.
!
!
   ii3 = N
   ii2 = N - 1
   x1 = 0.0
   DO j = 1 , N
      N1(j) = j
      N2(j) = j
      C1(j,j) = C1(j,j) - Eigen
      DO i = 1 , N
         x2 = cabs(C1(i,j))
         IF ( x1<x2 ) THEN
            x1 = x2
            i1 = i
            j1 = j
         ENDIF
      ENDDO
   ENDDO
   DO k6 = 2 , N
      IF ( cabs(C1(i1,j1))/=0 ) THEN
!
         d1 = (1.0,0.0)/C1(i1,j1)
         d2 = C1(i1,ii3)
         d3 = C1(ii3,j1)
         d4 = C1(ii3,ii3)
         DO i = 1 , ii2
            C3(i) = C1(i,j1)
            C1(i,j1) = C1(i,ii3)
            C1(i,ii3) = -C3(i)*d1
            d5 = -C1(i1,i)*d1
            C1(i1,i) = C1(ii3,i)
            C1(ii3,i) = d5
         ENDDO
         C3(i1) = d3
         C1(i1,j1) = d4
         C1(ii3,j1) = -d2*d1
         C1(i1,ii3) = -d3*d1
         C1(ii3,ii3) = d1
         IF ( ii3/=N ) THEN
            ii4 = ii3 + 1
            DO i = ii4 , N
               d6 = C1(i1,i)
               C1(i1,i) = C1(ii3,i)
               C1(ii3,i) = d6
               C3(i) = C1(i,j1)
               C1(i,j1) = C1(i,ii3)
               C1(i,ii3) = C3(i)
            ENDDO
         ENDIF
         i = N1(j1)
         N1(j1) = N1(ii3)
         N1(ii3) = i
         i = N2(i1)
         N2(i1) = N2(ii3)
         N2(ii3) = i
         x1 = 0.0
         DO j = 1 , ii2
            d8 = C1(ii3,j)
            DO i = 1 , ii2
               C1(i,j) = C1(i,j) + C3(i)*d8
               x2 = cabs(C1(i,j))
               IF ( x1<x2 ) THEN
                  x1 = x2
                  i1 = i
                  j1 = j
               ENDIF
            ENDDO
         ENDDO
         ii3 = ii3 - 1
         ii2 = ii2 - 1
      ELSE
         k5 = k6 - 1
!
!     SINGULAR MATRIX RETURN ZERO
!
         DO i = 1 , N
            C3(i) = 0.0
         ENDDO
         GOTO 99999
      ENDIF
   ENDDO
!
   C3(2) = C1(2,1)
   C3(1) = (1.0,0.0)
   DO j = 3 , N
      C3(j) = (0.0,0.0)
      j1 = j - 1
      DO i = 1 , j1
         C3(j) = C3(j) + C3(i)*C1(j,i)
      ENDDO
   ENDDO
   IF ( cabs(C1(1,1))>=1.0E-20 ) THEN
      DO k6 = 1 , 2
!
         DO j = 1 , N
            i1 = N2(j)
            DO i = 1 , N
               IF ( i1==N1(i) ) EXIT
            ENDDO
            C2(j) = C3(i)
         ENDDO
!
         DO j = 2 , N
            i1 = N - j + 1
            j1 = i1 + 1
            DO i = 1 , i1
               C2(i) = C2(i) + C1(i,j1)*C2(j1)
            ENDDO
         ENDDO
         d1 = C1(1,1)/C2(1)
         C3(1) = (1.0,0.0)
         DO j = 2 , N
            i1 = j - 1
            C3(j) = C2(j)*C1(j,j)*d1
            DO i = 1 , i1
               C3(j) = C3(j) + C1(j,i)*C3(i)
            ENDDO
         ENDDO
      ENDDO
   ENDIF
!
!     C3(I) NOW CONTAINS THE EIGENVECTOR WHICH MUST BE RE-ARRANGED
!     ACCORDING TO THE ORDER DICTATED BY N1(I) BACK TO THE ORIGINAL
!     ORDER.
!
   DO i = 1 , N
      i1 = N1(i)
      N1(i) = i
      DO WHILE ( i1/=i )
         d1 = C3(i1)
         C3(i1) = C3(i)
         C3(i) = d1
         k = N1(i1)
         N1(i1) = i1
         i1 = k
      ENDDO
   ENDDO
   N1(1) = 2
!
99999 RETURN
END SUBROUTINE egnvct
