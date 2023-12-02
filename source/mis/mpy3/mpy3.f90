!*==mpy3.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpy3
   IMPLICIT NONE
   USE C_BLANK
   USE C_MPY3TL
   USE C_ZZZZZZ
   EXTERNAL korsz , makmcb , mpy3dr , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
!*****
!     PRIMARY DRIVER FOR MATRIX TRIPLE PRODUCT.
!
!     ASSOCIATED SUBROUTINES
!         MPY3DR - SECONDARY DRIVER.  SETS UP OPEN CORE AND DETERMINES
!                  SOLUTION METHOD.
!         MPY3IC - IN-CORE PRODUCT.
!         MPY3OC - OUT-OF-CORE PRODUCT.
!         MPY3A  - PREPARES B AND A(T).
!         MPY3B  - PROCESSES A AND PERFORMS FIRST PART OF PRODUCT.
!         MPY3P  - PERFORMS MULTIPLICATION AND SUMMATION.
!         MPY3NU - CALCULATES NEXT TIME USED FOR INDIVIDUAL COLUMNS OF B
!                  OR ENTRIES OF A.
!         MPY3C  - PERFORMS MULTIPLICATION AND SUMMATION FOR REMAINING
!                  TERMS IN COLUMN OF A.
!
!     DMAP CALLING SEQUENCE
!
!         MPY3     A,B,E / C / C,N,CODE/ C,N,PREC   $
!*****
!
!     DMAP PARAMETERS
!     FILES
!     OPEN CORE
!
!*****
!     ASSIGN GINO FILE NUMBERS.
!*****
   Filea(1) = 101
   Fileb(1) = 102
   Filee(1) = 103
   Scr1 = 301
   Scr2 = 302
   Scr3 = 303
   Code = Ibcc
   Prec = Ibcp
   Lcore = korsz(Z)
!*****
!     GET MATRIX TRAILERS
!*****
   CALL rdtrl(Filea)
   CALL rdtrl(Fileb)
   CALL rdtrl(Filee)
   IF ( Filee(1)<0 ) Filee(1) = 0
!
   CALL makmcb(Filec,201,Filea(2),1,Prec)
   IF ( Code/=0 ) THEN
      IF ( Code==2 ) Filec(3) = Fileb(3)
      IF ( Code==1 .AND. Filea(2)/=Fileb(2) ) Filec(4) = 2
      IF ( Code==2 .AND. Fileb(3)/=Filea(2) ) Filec(4) = 2
   ENDIF
!*****
!     PERFORM MULTIPLY
!*****
   CALL mpy3dr(Z)
   CALL wrttrl(Filec)
!
END SUBROUTINE mpy3
