!*==dmfgr.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dmfgr(A,M,N,Eps,Irank,Irow,Icol)
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: A
   INTEGER :: M
   INTEGER :: N
   REAL :: Eps
   INTEGER :: Irank
   INTEGER , DIMENSION(1) :: Irow
   INTEGER , DIMENSION(1) :: Icol
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: hold , piv , save
   INTEGER :: i , ic , ii , ir , j , jj , kk , l , ll , mm , ncol , nm
   REAL :: tol
!
! End of declarations rewritten by SPAG
!
!
!     DMFGR CALCULATES THE RANK AND LINEARLY INDEPENDENT ROWS AND
!     COLUMNS OF A M BY N MATRIX.  IT EXPRESSES A SUBMATRIX OF
!     MAXIMAL RANK AS A PRODUCT OF TRIANGULAR FACTORS, NONBASIC ROWS
!     IN TERMS OF BASIC ONES AND BASIC VARIABLES IN TERMS OF FREE ONES
!
!     DIMENSIONED DUMMY VARIABLES
!
!
!     TEST OF SPECIFIED DIMENSIONS
!
   IF ( M<=0 ) THEN
      Irank = -1
   ELSEIF ( N<=0 ) THEN
      Irank = -1
   ELSE
!
!     RETURN IN CASE OF FORMAL ERRORS
!
!     INITIALIZE COLUMN INDEX VECTOR
!     SEARCH FIRST PIVOT ELEMENT
!
      Irank = 0
      piv = 0.D0
      jj = 0
      DO j = 1 , N
         Icol(j) = j
         DO i = 1 , M
            jj = jj + 1
            hold = A(jj)
            IF ( dabs(piv)<dabs(hold) ) THEN
               piv = hold
               ir = i
               ic = j
            ENDIF
         ENDDO
      ENDDO
!
!     INITIALIZE ROW INDEX VECTOR
!
      DO i = 1 , M
         Irow(i) = i
      ENDDO
!
!     SET UP INTERNAL TOLERANCE
!
      tol = abs(Eps*sngl(piv))
!
!     INITIALIZE ELIMINATION LOOP
!
      nm = N*M
      SPAG_Loop_1_1: DO ncol = M , nm , M
!
!     TEST FOR FEASIBILITY OF PIVOT ELEMENT
!
         IF ( abs(sngl(piv))<=tol ) EXIT SPAG_Loop_1_1
!
!     UPDATE RANK
!
         Irank = Irank + 1
!
!     INTERCHANGE ROWS IF NECESSARY
!
         jj = ir - Irank
         IF ( jj>0 ) THEN
            DO j = Irank , nm , M
               i = j + jj
               save = A(j)
               A(j) = A(i)
               A(i) = save
            ENDDO
!
!     UPDATE ROW INDEX VECTOR
!
            jj = Irow(ir)
            Irow(ir) = Irow(Irank)
            Irow(Irank) = jj
         ENDIF
!
!     INTERCHANGE COLUMNS IF NECESSARY
!
         jj = (ic-Irank)*M
         IF ( jj>0 ) THEN
            kk = ncol
            DO j = 1 , M
               i = kk + jj
               save = A(kk)
               A(kk) = A(i)
               kk = kk - 1
               A(i) = save
            ENDDO
!
!     UPDATE COLUMN INDEX VECTOR
!
            jj = Icol(ic)
            Icol(ic) = Icol(Irank)
            Icol(Irank) = jj
         ENDIF
         kk = Irank + 1
         mm = Irank - M
         ll = ncol + mm
!
!     TEST FOR LAST ROW
!
         IF ( mm>=0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
!
!     TRANSFORM CURRENT SUBMATRIX AND SEARCH NEXT PIVOT
!
         jj = ll
         save = piv
         piv = 0.D0
         DO j = kk , M
            jj = jj + 1
            hold = A(jj)/save
            A(jj) = hold
            l = j - Irank
!
!     TEST FOR LAST COLUMN
!
            IF ( Irank<N ) THEN
               ii = jj
               DO i = kk , N
                  ii = ii + M
                  mm = ii - l
                  A(ii) = A(ii) - hold*A(mm)
                  IF ( dabs(A(ii))>dabs(piv) ) THEN
                     piv = A(ii)
                     ir = j
                     ic = i
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ENDDO SPAG_Loop_1_1
!
!     SET UP MATRIX EXPRESSING ROW DEPENDENCIES
!
      IF ( Irank>=1 ) THEN
         IF ( Irank/=1 ) THEN
            ir = ll
            DO j = 2 , Irank
               ii = j - 1
               ir = ir - M
               jj = ll
               DO i = kk , M
                  hold = 0.D0
                  jj = jj + 1
                  mm = jj
                  ic = ir
                  DO l = 1 , ii
                     hold = hold + A(mm)*A(ic)
                     ic = ic - 1
                     mm = mm - M
                  ENDDO
                  A(mm) = A(mm) - hold
               ENDDO
            ENDDO
         ENDIF
         CALL spag_block_2
         RETURN
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      RETURN
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!     TEST FOR COLUMN REGULARITY
!
      IF ( N<=Irank ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
!
!     SET UP MATRIX EXPRESSING BASIC VARIABLES IN TERMS OF FREE
!     PARAMETERS (HOMOGENEOUS SOLUTION).
!
      ir = ll
      kk = ll + M
      DO j = 1 , Irank
         DO i = kk , nm , M
            jj = ir
            ll = i
            hold = 0.D0
            ii = j
            SPAG_Loop_3_2: DO
               ii = ii - 1
               IF ( ii<=0 ) THEN
                  A(ll) = (hold-A(ll))/A(jj)
                  EXIT SPAG_Loop_3_2
               ELSE
                  hold = hold - A(jj)*A(ll)
                  jj = jj - M
                  ll = ll - 1
               ENDIF
            ENDDO SPAG_Loop_3_2
         ENDDO
         ir = ir - 1
      ENDDO
   END SUBROUTINE spag_block_2
END SUBROUTINE dmfgr
