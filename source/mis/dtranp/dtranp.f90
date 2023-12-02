!*==dtranp.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dtranp
   USE c_blank
   USE c_system
   USE c_trnspx
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
   INTEGER , SAVE :: in1 , in2
   EXTERNAL korsz , rdtrl , trnsp , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     DRIVER OF MATRIX TRANSPOSE MODULE
!
!     TRNSP    IA/IAT/C,N,IXX  $
!
!     THE DIAGONALS OF THE LOWER OR UPPER TRIANGULAR MATRICES ARE
!     REPLACED BY UNITY (1.0) IF IXX IS ONE. (DEFAULT IS ZERO)
!
   DATA in1 , in2/101 , 201/
!
   ia(1) = in1
   CALL rdtrl(ia(1))
   IF ( ia(1)>0 ) THEN
      iat(1) = in2
      iat(2) = ia(3)
      iat(3) = ia(2)
      iat(4) = ia(4)
      iat(5) = ia(5)
      iat(6) = 0
      iat(7) = 0
      lcore = korsz(core)
      nscr = 8
      DO i = 1 , nscr
         iscr(i) = 300 + i
      ENDDO
      IF ( ixx==1 ) ixx = -123457890
      CALL trnsp(core(1))
      CALL wrttrl(iat(1))
   ELSE
      WRITE (nout,99001) uwm
99001 FORMAT (A25,' FROM TRNSP, MISSING INPUT DATA BLOCK FOR MATRIX ','TRANSPOSE')
   ENDIF
!
END SUBROUTINE dtranp
