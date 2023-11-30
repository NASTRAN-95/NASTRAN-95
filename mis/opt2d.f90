
SUBROUTINE opt2d(Ipr,Pr)
   IMPLICIT NONE
   INTEGER Iz(1) , Next , Nprw , Nrd , Nrrew , Nwdsp , Nwrew , Nwrt , Optp1 , Optp2 , Zcor
   REAL Skp1(9) , Skp3(2) , Skp4(2) , Z(1)
   COMMON /blank / Skp1 , Nwdsp , Optp1 , Skp3 , Optp2 , Skp4 , Nprw
   COMMON /names / Nrd , Nrrew , Nwrt , Nwrew , Next
   COMMON /optpw2/ Zcor , Z
   INTEGER Ipr(1)
   REAL Pr(1)
   INTEGER eor , i , n
!-----
!   COPY OPTP1 TO OPTP2 DATA FILE.
!  CHANGE RECORD 3      WORD 1 = IABS (PID).
!                       WORD 4 = PLST
!                       WORD 5 = ALPH
!-----
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
!
!  . RECORD ZERO - COPY NAME AND 6 PARAMETERS...
!
   CALL fread(Optp1,Z(1),8,Next)
   CALL fname(Optp2,Z(1))
   CALL write(Optp2,Z(1),8,Next)
!
!  . RECORD ONE (POINTERS) AND TWO (ELEMENT DATA)...
!
   DO i = 1 , 2
      n = Zcor
 50   eor = Next
      CALL read(*100,*100,Optp1,Z,Zcor,0,n)
      eor = 0
 100  CALL write(Optp2,Z(1),n,eor)
      IF ( eor==0 ) GOTO 50
   ENDDO
!
!  . RECORD THREE - PROPERTY DATA...
!
   eor = 0
   DO i = 1 , Nprw , Nwdsp
      Ipr(i) = iabs(Ipr(i))
      Pr(i+4) = -1.0
      CALL write(Optp2,Ipr(i),Nwdsp,eor)
   ENDDO
   CALL write(Optp2,0,0,Next)
!
!  . RECORD FOUR - PLIMIT DATA...
!
   CALL fread(Optp1,0,0,Next)
   n = Zcor
 200  eor = Next
   CALL read(*300,*300,Optp1,Z,Zcor,0,n)
   eor = 0
 300  CALL write(Optp2,Z(1),n,eor)
   IF ( eor==0 ) GOTO 200
!
   CALL eof(Optp2)
   Iz(1) = Optp1
   CALL rdtrl(Iz(1))
   Iz(1) = Optp2
   CALL wrttrl(Iz(1))
END SUBROUTINE opt2d