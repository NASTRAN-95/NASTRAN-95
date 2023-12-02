!*==mpyl.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpyl(A,B,Ncola,Nrowa,Ncolb,C)
   IMPLICIT NONE
   INTEGER Ncola , Ncolb , Nrowa
   REAL A(Ncola,Nrowa) , B(Ncolb,Ncola) , C(Ncolb,Nrowa) , D(Nrowa,Ncola) , Vect(3) , X(3) , Y(3)
   INTEGER l , m , n
   REAL w
!
!     SINCE CDC FORTRAN 5 IMPOSES NO LONGER EXACT NO. OF DUMMY ARGUMENT
!     LIST FOR SUBROUTINE AND ENTRY POINTS, THIS ROUTINE IS NOW MACHINE
!     INDEPENDENT.
!
!
!     SIMPLE MATRIX MULTIPLICATION
!
   DO n = 1 , Ncolb
      DO l = 1 , Nrowa
         C(n,l) = 0.0
         DO m = 1 , Ncola
            C(n,l) = C(n,l) + B(n,m)*A(m,l)
         ENDDO
      ENDDO
   ENDDO
   RETURN
!
   ENTRY norm(X,Y)
!     ================
!
!     NORMALIZE X VECTOR
!
   Y(1) = X(1)*X(1) + X(2)*X(2) + X(3)*X(3)
   IF ( Y(1)/=0.0 ) THEN
      w = 1./sqrt(Y(1))
      X(1) = X(1)*w
      X(2) = X(2)*w
      X(3) = X(3)*w
   ENDIF
   RETURN
!
   ENTRY cross(X,Y,Vect)
!     ======================
!
!     CROSS PRODUCT
!
   Vect(1) = X(2)*Y(3) - X(3)*Y(2)
   Vect(2) = Y(1)*X(3) - X(1)*Y(3)
   Vect(3) = X(1)*Y(2) - Y(1)*X(2)
   RETURN
!
   ENTRY mpylt(D,B,Ncola,Nrowa,Ncolb,C)
!     =====================================
!
!     TRANSPOSE MULTIPLY
!
   DO n = 1 , Ncolb
      DO l = 1 , Nrowa
         C(n,l) = 0.0
         DO m = 1 , Ncola
            C(n,l) = C(n,l) + D(l,m)*B(n,m)
         ENDDO
      ENDDO
   ENDDO
END SUBROUTINE mpyl
