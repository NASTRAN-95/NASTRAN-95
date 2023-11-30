
SUBROUTINE hsbg(N,A,Ia,B)
   IMPLICIT NONE
   INTEGER Ia , N
   DOUBLE PRECISION A(1)
   REAL B(1)
   INTEGER i , ipiv , isub , j , jk , k , kj , kl , l , l1 , l2 , lia , lj , lk , m , n2 , nia
   DOUBLE PRECISION piv , s , t
!
!     ..................................................................
!
!        SUBROUTINE HSBG
!
!        PURPOSE
!           TO REDUCE A REAL MATRIX INTO UPPER ALMOST TRIANGULAR FORM
!
!        USAGE
!           CALL HSBG(N,A,IA)
!
!        DESCRIPTION OF THE PARAMETERS
!           N      ORDER OF THE MATRIX
!           A      THE INPUT MATRIX, N BY N
!           IA     SIZE OF THE FIRST DIMENSION ASSIGNED TO THE ARRAY
!                  A IN THE CALLING PROGRAM WHEN THE MATRIX IS IN
!                  DOUBLE SUBSCRIPTED DATA STORAGE MODE.  IA=N WHEN
!                  THE MATRIX IS IN SSP VECTOR STORAGE MODE.
!
!        REMARKS
!           THE HESSENBERG FORM REPLACES THE ORIGINAL MATRIX IN THE
!           ARRAY A.
!
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!           NONE
!
!        METHOD
!           SIMILARITY TRANSFORMATIONS USING ELEMENTARY ELIMINATION
!           MATRICES, WITH PARTIAL PIVOTING.
!
!        REFERENCES
!           J.H. WILKINSON - THE ALGEBRAIC EIGENVALUE PROBLEM -
!           CLARENDON PRESS, OXFORD, 1965.
!
!     ..................................................................
!
!
!     MAKE THIS ROUTINE DOUBLE A AND B ARE SAME SPACE
!
   n2 = N*N
   k = n2
   DO i = 1 , n2
      A(k) = B(k)
      k = k - 1
   ENDDO
   l = N
   nia = l*Ia
   lia = nia - Ia
!
!        L IS THE ROW INDEX OF THE ELIMINATION
!
   DO WHILE ( l>=3 )
      lia = lia - Ia
      l1 = l - 1
      l2 = l1 - 1
!
!        SEARCH FOR THE PIVOTAL ELEMENT IN THE LTH ROW
!
      isub = lia + l
      ipiv = isub - Ia
      piv = dabs(A(ipiv))
      IF ( l>3 ) THEN
         m = ipiv - Ia
         DO i = l , m , Ia
            t = dabs(A(i))
            IF ( t>piv ) THEN
               ipiv = i
               piv = t
            ENDIF
         ENDDO
      ENDIF
      IF ( piv/=0 ) THEN
         IF ( piv>dabs(A(isub)) ) THEN
!
!        INTERCHANGE THE COLUMNS
!
            m = ipiv - l
            DO i = 1 , l
               j = m + i
               t = A(j)
               k = lia + i
               A(j) = A(k)
               A(k) = t
            ENDDO
!
!        INTERCHANGE THE ROWS
!
            m = l2 - m/Ia
            DO i = l1 , nia , Ia
               t = A(i)
               j = i - m
               A(i) = A(j)
               A(j) = t
            ENDDO
         ENDIF
!
!        TERMS OF THE ELEMENTARY TRANSFORMATION
!
         DO i = l , lia , Ia
            A(i) = A(i)/A(isub)
         ENDDO
!
!        RIGHT TRANSFORMATION
!
         j = -Ia
         DO i = 1 , l2
            j = j + Ia
            lj = l + j
            DO k = 1 , l1
               kj = k + j
               kl = k + lia
               A(kj) = A(kj) - A(lj)*A(kl)
            ENDDO
         ENDDO
!
!        LEFT TRANSFORMATION
!
         k = -Ia
         DO i = 1 , N
            k = k + Ia
            lk = k + l1
            s = A(lk)
            lj = l - Ia
            DO j = 1 , l2
               jk = k + j
               lj = lj + Ia
               s = s + A(lj)*A(jk)
            ENDDO
            A(lk) = s
         ENDDO
!
!        SET THE LOWER PART OF THE MATRIX TO ZERO
!
         DO i = l , lia , Ia
            A(i) = 0.0
         ENDDO
      ENDIF
      l = l1
   ENDDO
END SUBROUTINE hsbg
