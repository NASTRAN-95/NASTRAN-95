
SUBROUTINE amatrx(D,V,C,Ca,Ca2,Va,Dm,Db,Yi)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   REAL C , Ca , Ca2 , Db , Dm , V , Va
   REAL D(10,10) , Yi(6,11)
!
! Local variable declarations
!
   INTEGER i , j
!
! End of declarations
!
!
!
! THIS ROUTINE COMPUTES THE STIFFNESS MATRIX IN FIELD COORDINATES FOR
! THE TOROIDAL RING
!
!
! NOTE THE DOUBLE SUBSCRIPTING USED IN AMATRIX SUBROUTINE IS
! COMPATIBLE WITH THE CALLING PROGRAM. THE DELINT ARRAY OF INTEGRALS
! IS A (11X6) SINGLY SUBSCRIPTED ARRAY (STORED ROWWISE) IN THE CALLING
! PROGRAM AND IT IS A (6X11) DOUBLY SUBSCRIPTED ARRAY (STORED
! COLUMNWISE) IN AMATRX ROUTINE.
!
!
!
!     ------------------------------------------------------------------
!
   D(1,1) = Dm*(Ca2*Yi(1,1)+2.*Va*Yi(2,1)+Yi(3,1))
   D(2,1) = Dm*(Ca2*Yi(1,2)+2.*Va*Yi(2,2)+Yi(3,2))
   D(3,1) = Dm*(Ca2*Yi(1,3)+2.*Va*Yi(2,3)+Yi(3,3))
   D(4,1) = Dm*(Ca2*Yi(1,4)+2.*Va*Yi(2,4)+Yi(3,4))
   D(5,1) = Dm*(Ca2*Yi(1,5)+2.*Va*Yi(2,5)+Yi(3,5))
   D(6,1) = Dm*(Ca2*Yi(1,6)+2.*Va*Yi(2,6)+Yi(3,6))
   D(7,1) = Dm*(Va*Yi(4,1)+Yi(5,1))
   D(8,1) = Dm*(Ca*Yi(1,1)+Va*Yi(4,2)+V*Yi(2,1)+Yi(5,2))
   D(9,1) = Dm*(2.*Ca*Yi(1,2)+Va*Yi(4,3)+2.*V*Yi(2,2)+Yi(5,3))
   D(10,1) = Dm*(3.*Ca*Yi(1,3)+Va*Yi(4,4)+3.*V*Yi(2,3)+Yi(5,4))
   D(2,2) = Db*Yi(6,1) + D(3,1)
   D(3,2) = Db*(2.*V*Yi(4,1)+2.*Yi(6,2)) + D(4,1)
   D(4,2) = Db*(6.*V*Yi(4,2)+3.*Yi(6,3)) + D(5,1)
   D(5,2) = Db*(12.*V*Yi(4,3)+4.*Yi(6,4)) + D(6,1)
   D(6,2) = Dm*(Ca2*Yi(1,7)+2.*Va*Yi(2,7)+Yi(3,7)) + Db*(20.*V*Yi(4,4)+5.*Yi(6,5))
   D(7,2) = Dm*(Va*Yi(4,2)+Yi(5,2))
   D(8,2) = Dm*(Ca*Yi(1,2)+Va*Yi(4,3)+V*Yi(2,2)+Yi(5,3))
   D(9,2) = Dm*(2.*Ca*Yi(1,3)+Va*Yi(4,4)+2.*V*Yi(2,3)+Yi(5,4))
   D(10,2) = Dm*(3.*Ca*Yi(1,4)+Va*Yi(4,5)+3.*V*Yi(2,4)+Yi(5,5))
   D(3,3) = Db*4.*(C*Yi(1,1)+2.*V*Yi(4,2)+Yi(6,3)) + D(5,1)
   D(4,3) = Db*6.*(2.*C*Yi(1,2)+3.*V*Yi(4,3)+Yi(6,4)) + D(6,1)
   D(5,3) = Dm*(Ca2*Yi(1,7)+2.*Va*Yi(2,7)+Yi(3,7)) + Db*2.*(12.*C*Yi(1,3)+16.*V*Yi(4,4)+4.*Yi(6,5))
   D(6,3) = Dm*(Ca2*Yi(1,8)+2.*Va*Yi(2,8)+Yi(3,8)) + Db*10.*(4.*C*Yi(1,4)+5.*V*Yi(4,5)+Yi(6,6))
   D(7,3) = Dm*(Va*Yi(4,3)+Yi(5,3))
   D(8,3) = Dm*(Ca*Yi(1,3)+Va*Yi(4,4)+V*Yi(2,3)+Yi(5,4))
   D(9,3) = Dm*(2.*Ca*Yi(1,4)+Va*Yi(4,5)+2.*V*Yi(2,4)+Yi(5,5))
   D(10,3) = Dm*(3.*Ca*Yi(1,5)+Va*Yi(4,6)+3.*V*Yi(2,5)+Yi(5,6))
   D(4,4) = Dm*(Ca2*Yi(1,7)+2.*Va*Yi(2,7)+Yi(3,7)) + Db*9.*(4.*C*Yi(1,3)+4.*V*Yi(4,4)+Yi(6,5))
   D(5,4) = Dm*(Ca2*Yi(1,8)+2.*Va*Yi(2,8)+Yi(3,8)) + Db*12.*(6.*C*Yi(1,4)+5.*V*Yi(4,5)+Yi(6,6))
   D(6,4) = Dm*(Ca2*Yi(1,9)+2.*Va*Yi(2,9)+Yi(3,9)) + Db*15.*(8.*C*Yi(1,5)+6.*V*Yi(4,6)+Yi(6,7))
   D(7,4) = Dm*(Va*Yi(4,4)+Yi(5,4))
   D(8,4) = Dm*(Ca*Yi(1,4)+Va*Yi(4,5)+V*Yi(2,4)+Yi(5,5))
   D(9,4) = Dm*(2.*Ca*Yi(1,5)+Va*Yi(4,6)+2.*V*Yi(2,5)+Yi(5,6))
   D(10,4) = Dm*(3.*Ca*Yi(1,6)+Va*Yi(4,7)+3.*V*Yi(2,6)+Yi(5,7))
   D(5,5) = Dm*(Ca2*Yi(1,9)+2.*Va*Yi(2,9)+Yi(3,9)) + Db*16.*(9.*C*Yi(1,5)+6.*V*Yi(4,6)+Yi(6,7))
   D(6,5) = Dm*(Ca2*Yi(1,10)+2.*Va*Yi(2,10)+Yi(3,10)) + Db*20.*(12.*C*Yi(1,6)+7.*V*Yi(4,7)+Yi(6,8))
   D(7,5) = Dm*(Va*Yi(4,5)+Yi(5,5))
   D(8,5) = Dm*(Ca*Yi(1,5)+Va*Yi(4,6)+V*Yi(2,5)+Yi(5,6))
   D(9,5) = Dm*(2.*Ca*Yi(1,6)+Va*Yi(4,7)+2.*V*Yi(2,6)+Yi(5,7))
   D(10,5) = Dm*(3.*Ca*Yi(1,7)+Va*Yi(4,8)+3.*V*Yi(2,7)+Yi(5,8))
   D(6,6) = Dm*(Ca2*Yi(1,11)+2.*Va*Yi(2,11)+Yi(3,11)) + Db*25.*(16.*C*Yi(1,7)+8.*V*Yi(4,8)+Yi(6,9))
   D(7,6) = Dm*(Va*Yi(4,6)+Yi(5,6))
   D(8,6) = Dm*(Ca*Yi(1,6)+Va*Yi(4,7)+V*Yi(2,6)+Yi(5,7))
   D(9,6) = Dm*(2.*Ca*Yi(1,7)+Va*Yi(4,8)+2.*V*Yi(2,7)+Yi(5,8))
   D(10,6) = Dm*(3.*Ca*Yi(1,8)+Va*Yi(4,9)+3.*V*Yi(2,8)+Yi(5,9))
   D(7,7) = Dm*Yi(6,1)
   D(8,7) = Dm*(V*Yi(4,1)+Yi(6,2))
   D(9,7) = Dm*(2.*V*Yi(4,2)+Yi(6,3))
   D(10,7) = Dm*(3.*V*Yi(4,3)+Yi(6,4))
   D(8,8) = Dm*(C*Yi(1,1)+2.*V*Yi(4,2)+Yi(6,3))
   D(9,8) = Dm*(2.*C*Yi(1,2)+3.*V*Yi(4,3)+Yi(6,4))
   D(10,8) = Dm*(3.*C*Yi(1,3)+4.*V*Yi(4,4)+Yi(6,5))
   D(9,9) = Dm*(4.*C*Yi(1,3)+4.*V*Yi(4,4)+Yi(6,5))
   D(10,9) = Dm*(6.*C*Yi(1,4)+5.*V*Yi(4,5)+Yi(6,6))
   D(10,10) = Dm*(9.*C*Yi(1,5)+6.*V*Yi(4,6)+Yi(6,7))
   DO i = 1 , 10
      DO j = 1 , i
         D(j,i) = D(i,j)
      ENDDO
   ENDDO
END SUBROUTINE amatrx
