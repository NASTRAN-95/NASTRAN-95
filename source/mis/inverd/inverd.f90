!*==inverd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE inverd(Ndim,A,N,B,M,Determ,Ising,Index)
   USE c_machin
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ndim
   INTEGER :: N
   REAL(REAL64) , DIMENSION(Ndim,1) :: A
   REAL(REAL64) , DIMENSION(Ndim,1) :: B
   INTEGER :: M
   REAL(REAL64) :: Determ
   INTEGER :: Ising
   INTEGER , DIMENSION(N,3) :: Index
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: amax , pivot , swap , t
   REAL(REAL64) , SAVE :: epsi
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
!     ISING RETURNS 2, IF MATRIX A(N,N) IS SINGULAR, 1 OTHERWISE.
!     (IF ISING IS SET TO .LT. 0 UPON INPUT, DETERM IS NO CALCULATED)
!     INVERSE RETURNS  IN A
!     SOLUTION VECTORS RETURN IN B
!     INDEX IS WORKING STORAGE (N,3)
!
   !>>>>EQUIVALENCE (irow,jrow) , (icolum,jcolum) , (amax,t,swap)
   DATA epsi/1.0D-36/
!
!     INITIALIZE
!
   IF ( mach==5 ) epsi = 1.D-18
   Determ = 1.0D0
   IF ( Ising<0 ) Determ = 0.0D0
   DO j = 1 , N
      Index(j,3) = 0
   ENDDO
   DO i = 1 , N
!
!     SEARCH FOR PIVOT
!
      amax = 0.0D0
      DO j = 1 , N
         IF ( Index(j,3)/=1 ) THEN
            DO k = 1 , N
               IF ( Index(k,3)<1 ) THEN
                  IF ( dabs(A(j,k))>amax ) THEN
                     irow = j
                     icolum = k
                     amax = dabs(A(j,k))
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
!
!     COMMENTS FROM G.CHAN/UNISYS    9/1992
!
!     THE D.P. OF VAX IS LIMITED TO 10**38. NEXT LINE COULD CAUSE
!     FLOATING POINT NUMBER OVERFLOW IN VAX IN SOME HUGE PROBLEM.
!     CHECK FIRST THAT THE CALLER REALLY WANT THE DETERMINANT TERM. IF
!     NOT, ISING SHOULD BE SET TO -1 AND THE DETERM TERM IS BY-PASSED.
!     IF DETERM IS REALLY WANTED, USE REAL*16 HERE FOR VAX, AND TURN IT
!     BACK TO D.P. BEFORE RETURN. RE-COMPILE THIS SUBROUTINE AND RE-LINK
!     NASTRAN EXECUTABLE
!
      Determ = Determ*pivot
!
      IF ( dabs(pivot)<epsi ) THEN
         CALL spag_block_2
         RETURN
      ENDIF
      A(icolum,icolum) = 1.0D0
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
            A(l1,icolum) = 0.0D0
            IF ( dabs(t)>=epsi ) THEN
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
      USE ISO_FORTRAN_ENV                 
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      USE ISO_FORTRAN_ENV                 
      Ising = 2
   END SUBROUTINE spag_block_2
END SUBROUTINE inverd