
SUBROUTINE mce1a
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER A(7) , A11(7) , A12(7) , A21(7) , A22(7) , Gm , L , Mcb(7) , N , Nsub1 , Nsub2 , Nsub3 , Nz , Rg , Rm , Rn , Rule ,      &
         & Scr1 , Scr2 , Scr3 , Ug , Um , Un , Uset , Usetxx , Z(1)
   REAL U , Ua , Uf , Ul , Uo , Ur , Us , Usb , Usg
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug
   COMMON /blank / Uset , Rg , Gm , Scr1 , Scr2 , Scr3 , Rm , Rn , L , U , Mcb
   COMMON /parmeg/ A , A11 , A21 , A12 , A22 , N , Rule
   COMMON /patx  / Nz , Nsub1 , Nsub2 , Nsub3 , Usetxx
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER i , rect , square
   INTEGER korsz
!
! End of declarations
!
!
!     MCE1A PARTITIONS RG INTO RM AND RN
!
   DATA rect/2/ , square/1/ , i/1/
!
!     GENERATE ROW PARTITIONING VECTOR
!
   Nz = korsz(Z)
   Usetxx = Uset
   CALL calcv(Scr1,Ug,Un,Um,Z)
!
!     GENERATE NULL COLUMN PARTITIONING VECTOR
!
   Z(i) = 0
   Z(i+2) = Nsub2
   Z(i+7) = 1
   Z(i+8) = 2
   Z(i+9) = -16777215
!
!     INITIALIZE MATRIX CONTROL BLOCKS
!
   N = Nz
   Rule = 0
   A(1) = Rg
   CALL rdtrl(A)
   A11(1) = Rn
   A11(2) = Nsub1
   A11(3) = Nsub2
   A11(4) = rect
   A11(5) = A(5)
   A12(1) = Rm
   A12(2) = Nsub2
   A12(3) = Nsub2
   A12(4) = square
   A12(5) = A(5)
   Mcb(1) = Scr1
   CALL rdtrl(Mcb)
   A21(1) = 0
   A22(1) = 0
!
!     PARTITION RG INTO RM AND RN
!
   CALL partn(Mcb,Z,Z)
!
!     WRITE TRAILERS FOR RM AND RN
!
   CALL wrttrl(A12)
   CALL wrttrl(A11)
END SUBROUTINE mce1a
