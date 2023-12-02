!*==mpy3.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpy3
   USE c_blank
   USE c_mpy3tl
   USE c_zzzzzz
   IMPLICIT NONE
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
   filea(1) = 101
   fileb(1) = 102
   filee(1) = 103
   scr1 = 301
   scr2 = 302
   scr3 = 303
   code = ibcc
   prec = ibcp
   lcore = korsz(z)
!*****
!     GET MATRIX TRAILERS
!*****
   CALL rdtrl(filea)
   CALL rdtrl(fileb)
   CALL rdtrl(filee)
   IF ( filee(1)<0 ) filee(1) = 0
!
   CALL makmcb(filec,201,filea(2),1,prec)
   IF ( code/=0 ) THEN
      IF ( code==2 ) filec(3) = fileb(3)
      IF ( code==1 .AND. filea(2)/=fileb(2) ) filec(4) = 2
      IF ( code==2 .AND. fileb(3)/=filea(2) ) filec(4) = 2
   ENDIF
!*****
!     PERFORM MULTIPLY
!*****
   CALL mpy3dr(z)
   CALL wrttrl(filec)
!
END SUBROUTINE mpy3
