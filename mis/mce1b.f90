
SUBROUTINE mce1b
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER A(7) , Gm , L , Lx(7) , Mcb(7) , Nz , Rg , Rm , Rn , Scr1 , Scr2 , Scr3 , Scrx1 , Scrx2 , Scrx3 , U , Uset , Ux(7)
   DOUBLE PRECISION Det , Mindia
   REAL Power , Z(1)
   COMMON /blank / Uset , Rg , Gm , Scr1 , Scr2 , Scr3 , Rm , Rn , L , U , Mcb
   COMMON /dcompx/ A , Lx , Ux , Scrx1 , Scrx2 , Scrx3 , Det , Power , Nz , Mindia
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER korsz
   INTEGER nam(2)
!
! End of declarations
!
!
!     MCE1B DECOMPOSES RM INTO LOWER AND UPPER TRIANGULAR FACTORS
!
   DATA nam/4HMCE1 , 4HB   /
!
!     INITIALIZE MATRIX CONTROL BLOCKS
!
   Nz = korsz(Z)
   A(1) = Rm
   CALL rdtrl(A)
   Lx(1) = L
   Lx(3) = A(3)
   Lx(4) = 4
   Lx(5) = A(5)
   Ux(1) = U
   Ux(3) = A(3)
   Ux(4) = 5
   Ux(5) = A(5)
   Scrx1 = Scr1
   Scrx2 = Scr2
   Scrx3 = Scr3
!
!     PERFORM DECOMPOSITION
!
   CALL decomp(*100,Z,Z,Z)
!
!     WRITE TRAILERS
!
   CALL wrttrl(Lx)
   CALL wrttrl(Ux)
   RETURN
!
!     FATAL ERROR MESSAGE FOR SINGULAR MATRIX
!
 100  CALL mesage(-5,Rm,nam)
END SUBROUTINE mce1b
