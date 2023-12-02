!*==elim.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE elim(In1,In2,In3,In4,Out1,Scr1,Scr2,Scr3)
   USE c_mpyadx
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: In1
   INTEGER :: In2
   INTEGER :: In3
   INTEGER :: In4
   INTEGER :: Out1
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , rdp
   INTEGER , SAVE :: plus
   EXTERNAL korsz , mpyad , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     ELIM EVALUATES THE MATRIX EQUATION -
!
!     OUT1 = IN1 + IN4(T)*IN2 + IN2(T)*IN4 + IN4(T)*IN3*IN4
!
!    1,               MCB(7)
   DATA plus/ + 1/
!
   rdp = iprec
!
!     PERFORM GENERAL INITIALIZATION
!
   nz = korsz(z)
   signab = plus
   signc = plus
   prec = rdp
   scrtch = Scr3
!
!     INITIALIZE MATRIX CONTROL BLOCKS FOR IN3,IN4,IN2 AND SCR1
!
   filea(1) = In3
   CALL rdtrl(filea)
   fileb(1) = In4
   CALL rdtrl(fileb)
   filec(1) = In2
   CALL rdtrl(filec)
   filed(1) = Scr1
   filed(3) = filec(3)
   filed(4) = filec(4)
   filed(5) = rdp
!
!     COMPUTE SCR1 = IN3*IN4 + IN2
!
   t = 0
   CALL mpyad(z,z,z)
!
!     SAVE MATRIX CONTROL BLOCK FOR SCR1
!
!     DO 41 I = 1,7
   CALL wrttrl(filed)
!
!     INITIALIZE MATRIX CONTROL BLOCKS FOR IN2, IN4, IN1 AND SCR2
!
   DO i = 1 , 7
      filea(i) = filec(i)
   ENDDO
   filec(1) = In1
   CALL rdtrl(filec)
   filed(1) = Scr2
   filed(3) = filec(3)
   filed(4) = filec(4)
!
!     COMPUTE SCR2 = IN2(T)*IN4 + IN1
!
   t = 1
   CALL mpyad(z,z,z)
   CALL wrttrl(filed)
!
!     INITIALIZE MATRIX CONTROL BLOCKS FOR IN4,SCR1,SCR2 AND OUT1
!
   filea(1) = fileb(1)
   fileb(1) = Scr1
   filec(1) = filed(1)
   CALL rdtrl(filea)
   CALL rdtrl(fileb)
   CALL rdtrl(filec)
   filed(1) = Out1
   filed(3) = filec(3)
   filed(4) = filec(4)
!
!     COMPUTE  OUT1= IN4(T)*SCR1 + SCR2
!
   t = 1
   CALL mpyad(z,z,z)
!
!     WRITE TRAILER FOR OUT1 AND RETURN
!
   CALL wrttrl(filed)
END SUBROUTINE elim
