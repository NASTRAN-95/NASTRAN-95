
SUBROUTINE fvrs1e(A,K,N)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER N
   REAL A(1)
   INTEGER K(1)
!
! Local variable declarations
!
   REAL aipm , w
   INTEGER i , ikl , ipm , j , k1 , kw , m
!
! End of declarations
!
!
!     PURPOSE
!       TO SORT THE ELEMENTS OF A REAL*4 VECTOR, A, INTO ASCENDING
!       ORDER AND TO CONSTRUCT AN INTEGER*4 VECTOR, K, WHICH INDICATES
!       HOW THE ELEMENTS OF A HAVE BEEN REARRANGED.
!
!     USAGE
!       CALL FVRS1E(A,K,N)
!
!     DESCRIPTION OF PARAMETERS
!       A - REAL*4 VECTOR.
!              ON INPUT - A CONTAINS THE NUMBERS TO BE SORTED.
!              ON OUTPUT- A CONTAINS THE NUMBERS IN ASCENDING ORDER.
!       K - OUTPUT VECTOR CONTAINING INTERCHANGE INFORMATION, I.E.,
!           THE NUMBER IN A(K(I)) (OF THE INPUT A) HAS BEEN MOVED TO
!           A(I).
!       N - LENGTH OF A AND K.
!
!     SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!       NONE
!
!     REMARKS
!       THE K-VECTOR CAN BE USED IN CONJUNCTION WITH SUBROUTINE FVRS1E
!       TO REARRANGE OTHER VECTORS IN THE SAME WAY THAT THE A-VECTOR
!       HAS BEEN REARRANGED.
!
!     METHOD
!       THIS ROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE,
!       'SHELLSORT', ALGORITHM 201, 'COLLECTED ALGORITHMS FROM CACM',
!       BY J. BOOTHROYD.
!
!
   DO ikl = 1 , N
      K(ikl) = ikl
   ENDDO
   i = 1
   DO
      i = i + i
      IF ( i<N ) CYCLE
      IF ( i/=N ) i = i/2
      m = 2*i - 1
      EXIT
   ENDDO
   DO
      m = m/2
      k1 = N - m
      DO j = 1 , k1
         i = j
         DO
            ipm = i + m
            aipm = A(ipm)
            IF ( aipm>=A(i) ) EXIT
            w = A(i)
            kw = K(i)
            A(i) = aipm
            K(i) = K(ipm)
            A(ipm) = w
            K(ipm) = kw
            i = i - m
            IF ( i<1 ) EXIT
         ENDDO
      ENDDO
      IF ( m<=1 ) EXIT
   ENDDO
END SUBROUTINE fvrs1e
