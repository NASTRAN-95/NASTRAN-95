
SUBROUTINE elim(In1,In2,In3,In4,Out1,Scr1,Scr2,Scr3)
   IMPLICIT NONE
   INTEGER Filea(7) , Fileb(7) , Filec(7) , Filed(7) , Idum(54) , Iprec , Nz , Prec , Scrtch , Signab , Signc , T
   REAL Z(1)
   COMMON /mpyadx/ Filea , Fileb , Filec , Filed , Nz , T , Signab , Signc , Prec , Scrtch
   COMMON /system/ Idum , Iprec
   COMMON /zzzzzz/ Z
   INTEGER In1 , In2 , In3 , In4 , Out1 , Scr1 , Scr2 , Scr3
   INTEGER i , plus , rdp
   INTEGER korsz
!
!     ELIM EVALUATES THE MATRIX EQUATION -
!
!     OUT1 = IN1 + IN4(T)*IN2 + IN2(T)*IN4 + IN4(T)*IN3*IN4
!
!    1,               MCB(7)
   DATA plus/ + 1/
!
   rdp = Iprec
!
!     PERFORM GENERAL INITIALIZATION
!
   Nz = korsz(Z)
   Signab = plus
   Signc = plus
   Prec = rdp
   Scrtch = Scr3
!
!     INITIALIZE MATRIX CONTROL BLOCKS FOR IN3,IN4,IN2 AND SCR1
!
   Filea(1) = In3
   CALL rdtrl(Filea)
   Fileb(1) = In4
   CALL rdtrl(Fileb)
   Filec(1) = In2
   CALL rdtrl(Filec)
   Filed(1) = Scr1
   Filed(3) = Filec(3)
   Filed(4) = Filec(4)
   Filed(5) = rdp
!
!     COMPUTE SCR1 = IN3*IN4 + IN2
!
   T = 0
   CALL mpyad(Z,Z,Z)
!
!     SAVE MATRIX CONTROL BLOCK FOR SCR1
!
!     DO 41 I = 1,7
   CALL wrttrl(Filed)
!
!     INITIALIZE MATRIX CONTROL BLOCKS FOR IN2, IN4, IN1 AND SCR2
!
   DO i = 1 , 7
      Filea(i) = Filec(i)
   ENDDO
   Filec(1) = In1
   CALL rdtrl(Filec)
   Filed(1) = Scr2
   Filed(3) = Filec(3)
   Filed(4) = Filec(4)
!
!     COMPUTE SCR2 = IN2(T)*IN4 + IN1
!
   T = 1
   CALL mpyad(Z,Z,Z)
   CALL wrttrl(Filed)
!
!     INITIALIZE MATRIX CONTROL BLOCKS FOR IN4,SCR1,SCR2 AND OUT1
!
   Filea(1) = Fileb(1)
   Fileb(1) = Scr1
   Filec(1) = Filed(1)
   CALL rdtrl(Filea)
   CALL rdtrl(Fileb)
   CALL rdtrl(Filec)
   Filed(1) = Out1
   Filed(3) = Filec(3)
   Filed(4) = Filec(4)
!
!     COMPUTE  OUT1= IN4(T)*SCR1 + SCR2
!
   T = 1
   CALL mpyad(Z,Z,Z)
!
!     WRITE TRAILER FOR OUT1 AND RETURN
!
   CALL wrttrl(Filed)
END SUBROUTINE elim
