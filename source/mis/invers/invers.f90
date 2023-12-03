!*==invers.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE invers(Ndim,A,N,B,M,Determ,Ising,Index)
   USE c_machin
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ndim
   INTEGER :: N
   REAL , DIMENSION(Ndim,1) :: A
   REAL , DIMENSION(Ndim,1) :: B
   INTEGER :: M
   REAL :: Determ
   INTEGER :: Ising
   INTEGER , DIMENSION(N,3) :: Index
!
! Local variable declarations rewritten by SPAG
!
   REAL :: amax , pivot , swap , t
   REAL , SAVE :: epsi
   INTEGER :: i , icolum , irow , j , jcolum , jrow , k , l , l1
!
! End of declarations rewritten by SPAG
!
!
!     INVERSE, OR LINEAR EQUATIONS SOLVER
!
!     NDIM IS THE ACTUAL SIZE OF A IN CALLING PROGRAM. E.G. A(NDIM,NDIM)
!     A IS SQUARE MATRIX TO BE INVERTED.
!     N IS SIZE OF UPPER LEFT PORTION BEING INVERTED.
!     B IS COLUMN OF CONSTANTS (OPTIONAL INPUT). SUPPLY SPACE B(NDIM,1)
!     M IS THE NUMBER OF COLUMNS OF CONSTANTS
!     DETERM RETURNS THE VALUE OF DETERMINANT IF NON-SINGULAR
!     ISING RETURNS 2, IF MATRIX A(N,N) IS SINGULAR
!                   1, IF MATRIX A(N,N) IS NON-SINGULAR
!     (IF ISING IS SET TO .LT. 0 UPON INPUT, DETERM IS NOT CALCULATED)
!     INVERSE RETURNS  IN A
!     SOLUTION VECTORS RETURN IN B
!     INDEX IS WORKING STORAGE (N,3)
!
   !>>>>EQUIVALENCE (irow,jrow) , (icolum,jcolum) , (amax,t,swap)
   DATA epsi/1.0E-30/
!
!     INITIALIZE
!
   IF ( mach==5 ) epsi = 1.E-18
   Determ = 1.0
   IF ( Ising<0 ) Determ = 0.0
   DO j = 1 , N
      Index(j,3) = 0
   ENDDO
   DO i = 1 , N
!
!     SEARCH FOR PIVOT
!
      amax = 0.0
      DO j = 1 , N
         IF ( Index(j,3)/=1 ) THEN
            DO k = 1 , N
               IF ( Index(k,3)<1 ) THEN
                  IF ( abs(A(j,k))>amax ) THEN
                     irow = j
                     icolum = k
                     amax = abs(A(j,k))
                  ENDIF
               ELSEIF ( Index(k,3)/=1 ) THEN
                  CALL spag_block_2
                  RETURN
               ENDIF
            ENDDO
         ENDIF
      ENDDO
      Index(icolum,3) = Index(icolum,3) + 1
      Index(i,1) = irow
      Index(i,2) = icolum
!
!     INTERCHANGE ROWS TO PUT PIVOT ELEMENT ON DIAGONAL
!
      IF ( irow/=icolum ) THEN
         Determ = -Determ
         DO l = 1 , N
            swap = A(irow,l)
            A(irow,l) = A(icolum,l)
            A(icolum,l) = swap
         ENDDO
         IF ( M>0 ) THEN
            DO l = 1 , M
               swap = B(irow,l)
               B(irow,l) = B(icolum,l)
               B(icolum,l) = swap
            ENDDO
         ENDIF
      ENDIF
!
!     DIVIDE PIVOT ROW BY PIVOT ELEMENT
!
      pivot = A(icolum,icolum)
      Determ = Determ*pivot
      IF ( abs(pivot)<epsi ) THEN
         CALL spag_block_2
         RETURN
      ENDIF
      A(icolum,icolum) = 1.0
      DO l = 1 , N
         A(icolum,l) = A(icolum,l)/pivot
      ENDDO
      IF ( M>0 ) THEN
         DO l = 1 , M
            B(icolum,l) = B(icolum,l)/pivot
         ENDDO
      ENDIF
!
!     REDUCE NON PIVOT ROWS
!
      DO l1 = 1 , N
         IF ( l1/=icolum ) THEN
            t = A(l1,icolum)
            A(l1,icolum) = 0.0
            IF ( abs(t)>=epsi ) THEN
               DO l = 1 , N
                  A(l1,l) = A(l1,l) - A(icolum,l)*t
               ENDDO
               IF ( M>0 ) THEN
                  DO l = 1 , M
                     B(l1,l) = B(l1,l) - B(icolum,l)*t
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      ENDDO
   ENDDO
!
!     INTERCHANGE COLUMNS
!
   DO i = 1 , N
      l = N + 1 - i
      IF ( Index(l,1)/=Index(l,2) ) THEN
         jrow = Index(l,1)
         jcolum = Index(l,2)
         DO k = 1 , N
            swap = A(k,jrow)
            A(k,jrow) = A(k,jcolum)
            A(k,jcolum) = swap
         ENDDO
      ENDIF
   ENDDO
   DO k = 1 , N
      IF ( Index(k,3)/=1 ) THEN
         Ising = 2
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
   Ising = 1
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      Ising = 2
   END SUBROUTINE spag_block_2
END SUBROUTINE invers