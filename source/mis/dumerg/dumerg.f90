!*==dumerg.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dumerg
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , nogo
   INTEGER , DIMENSION(3) :: ib
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , SAVE :: phia , phif , phio , scr1 , uset
   EXTERNAL mesage , sdr1b
!
! End of declarations rewritten by SPAG
!
!
!     DRIVER FOR DMAP MODULE UMERGE
!
!     UMERGE   USET,PHIA,PHIO/PHIF/C,N,MAJOR/C,N,SUB0/C,N,SUB1 $
!
   DATA name/4HUMER , 4HGE  /
   DATA uset , phia , phio , phif , scr1/101 , 102 , 103 , 201 , 301/
!
   nogo = 0
!
!     DECIDE IF CHARACTERS ARE LEGAL BIT NUMBERS
!
   ib(1) = Major(1)
   ib(2) = Sub0(1)
   ib(3) = Sub1(1)
!
   SPAG_Loop_1_1: DO j = 1 , 3
      DO i = 1 , 32
         IF ( ib(j)==Iabit(i) ) THEN
            ib(j) = Ibit(i)
            CYCLE SPAG_Loop_1_1
         ENDIF
      ENDDO
!
!     INVALID
!
      CALL mesage(59,ib(j),name)
      nogo = 1
   ENDDO SPAG_Loop_1_1
!
   IF ( nogo==1 ) CALL mesage(-7,0,name)
   CALL sdr1b(scr1,phia,phio,phif,ib(1),ib(2),ib(3),uset,0,0)
!
END SUBROUTINE dumerg
