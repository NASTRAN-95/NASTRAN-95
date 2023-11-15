
SUBROUTINE dumerg
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Iabit(16) , Ibit(32) , Major(2) , Sub0(2) , Sub1(2)
   COMMON /bitpos/ Ibit , Iabit
   COMMON /blank / Major , Sub0 , Sub1
!
! Local variable declarations
!
   INTEGER i , ib(3) , j , name(2) , nogo , phia , phif , phio , scr1 , uset
!
! End of declarations
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
   DO j = 1 , 3
      DO i = 1 , 32
         IF ( ib(j)==Iabit(i) ) THEN
            ib(j) = Ibit(i)
            GOTO 100
         ENDIF
      ENDDO
!
!     INVALID
!
      CALL mesage(59,ib(j),name)
      nogo = 1
 100  ENDDO
!
   IF ( nogo==1 ) CALL mesage(-7,0,name)
   CALL sdr1b(scr1,phia,phio,phif,ib(1),ib(2),ib(3),uset,0,0)
!
END SUBROUTINE dumerg
