
SUBROUTINE ema1s(J,Nsca,Scalas,Pivot,Dict,Cgv,Kgg,Cp,F)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   REAL F
   INTEGER J , Nsca
   INTEGER Cgv(1) , Dict(7) , Pivot(6) , Scalas(1)
   REAL Cp(1) , Kgg(1)
!
! Local variable declarations
!
   INTEGER i , icol0 , ii , ii0 , imat , k , l , m , ngrid
!
! End of declarations
!
!     SUBROUTINE EMA1D( J, NSCA, SCALAS, PIVOT, DICT, CGV, KGG, CP, F )
!*******
! EMA1S ADDS A COLUMN VECTOR IN REAL SINGLE PRECISION
! EMA1D ADDS A COLUMN VECTOR IN REAL DOUBLE PRECISION
!
!     J        INDEX IN SCALAS TO CURRENT RELATIVE COLUMN NBR
!     NSCA     NBR OF ROWS( TERMS ) PER GRID POINT IN COLUMN VECTOR
!     SCALAS   ARRAY OF RELATIVE ROW/COLUMN NUMBERS
!     PIVOT    6-WORD ARRAY AS FOLLOWS...
!          (1) INTERNAL INDEX OF PIVOT
!          (2) DOF OF PIVOT
!          (3) DOF OF EACH POINT CONNECTED TO PIVOT
!          (4) NBR OF CONNECTED POINTS
!          (5) INTERNAL INDEX OF  1ST CONNECTED POINT
!          (6) INTERNAL INDEX OF LAST CONNECTED POINT
!     DICT     DICTIONARY ENTRY FOR ELEMENT AS FOLLOWS...
!          (1) ELEMENT ID
!          (2) FORM( 1=RECT, 2=DIAG )
!          (3) NBR OF TERMS PER COLUMN
!          (4) COMPONENT CODE( DECODED IN SCALAS ARRAY )
!          (5) GE
!          (6) INTERNAL INDEX OF 1ST POINT
!          (7) GINO ADDR OF 1ST COLUMN PARTITION
!         ....
!     CGV      CONNECTED GRID POINT VECTOR
!     KGG      ADDR OF KGG COLUMNS FOR PIVOT
!     CP       ADDR OF COLUMN PARTITION
!     F        FACTOR( RSP ) TO BE APPLIED TO EACH TERM IN CP
!
!******
!
!     DOUBLE PRECISION KGG(1), CP(1)
!
! INITIALIZE
!
   icol0 = Scalas(J)*Pivot(3)*Pivot(4)
   ii0 = Pivot(5) - 1
   l = 1
   IF ( Dict(2)/=2 ) THEN
!
! PROCESS RECTANGULAR PARTITION
!
      ngrid = 4 + 2*Dict(3)/Nsca
      DO i = 6 , ngrid , 2
         k = Dict(i)
         IF ( k==0 ) RETURN
         imat = icol0 + Cgv(k-ii0)
         DO k = 1 , Nsca
            m = Scalas(k)
            Kgg(imat+m) = Kgg(imat+m) + F*Cp(l)
            l = l + 1
         ENDDO
      ENDDO
      GOTO 99999
   ENDIF
!
! PROCESS DIAGONAL PARTITION
!
   ii = Pivot(1)
   imat = icol0 + Cgv(ii-ii0) + Scalas(J)
   Kgg(imat) = Kgg(imat) + F*Cp(1)
   RETURN
99999 RETURN
END SUBROUTINE ema1s