
SUBROUTINE gkad1a(Usetd,Go,God,Scr1,Ue,Ua,Ud)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Core(1) , Ia(7) , Ia11(7) , Ia12(7) , Ib11(7) , Ib12(7) , Idum(54) , Iprec , Irule , Lc , N , N4 , No , Nz , Uset1
   COMMON /parmeg/ Ia , Ia11 , Ia12 , Ib11 , Ib12 , Nz , Irule
   COMMON /patx  / Lc , N , No , N4 , Uset1
   COMMON /system/ Idum , Iprec
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   INTEGER B1dd , Baa , Go , God , K1dd , K41dd , K4aa , Kaa , M1dd , Maa , Scr1 , Usetd
   REAL Ua , Ud , Ue
!
! Local variable declarations
!
   INTEGER i , ient , iout , ipv1(7)
   INTEGER korsz
!
! End of declarations
!
!
!     GKAD1A WILL EXPAND GO BY NULL MATRIX TO MAKE GOD, AND
!     AA-S TO D-S ADDING ZEROS FOR E-S
!
!
!
   ient = 0
!
!     COMPUTE CORE FOR CALCV AND MERGE
!
 100  Lc = korsz(Core)
!
!     BUILD PART VECTOR
!
   Uset1 = Usetd
   CALL calcv(Scr1,Ud,Ua,Ue,Core(1))
!
!     SET UP FOR MERGE
!
   Nz = Lc
   Irule = 0
   DO i = 1 , 7
      Ia11(i) = 0
      Ia(i) = 0
      Ia12(i) = 0
      Ib11(i) = 0
      Ib12(i) = 0
   ENDDO
   ipv1(1) = Scr1
   CALL rdtrl(ipv1)
   IF ( ient/=0 ) THEN
!
!     VECTOR MADE, SET UP MCB-S
!
      Ia(2) = N + No + N4
      Ia(3) = Ia(2)
      Ia(4) = 6
      Ia(5) = Iprec
      Ia11(1) = Kaa
      Ia(1) = K1dd
      iout = 1
      CALL rdtrl(Ia11)
      IF ( Ia11(1)>0 ) GOTO 300
      K1dd = 0
      GOTO 400
   ELSE
!
!     SET UP FOR 2 WAY MERGE
!
      Ia11(1) = Go
      CALL rdtrl(Ia11)
      Ia(1) = God
      Ia(2) = N + No + N4
      Ia(3) = Ia11(3)
      Ia(4) = Ia11(4)
      Ia(5) = Ia11(5)
!     BUILD NULL COLUMN IN CORE
      i = 0
      Core(1) = 0
      Core(i+2) = 1
      Core(i+3) = Ia(3)
      Core(i+4) = 2
      Core(i+5) = 1
      Core(i+6) = 0
      Core(i+7) = 0
      CALL merge(ipv1(1),Core(1),Core(1))
      CALL wrttrl(Ia)
   ENDIF
 200  RETURN
!
!
   ENTRY gkad1b(Usetd,Maa,Baa,K4aa,M1dd,B1dd,K41dd,Ua,Ue,Ud,Scr1)
!     ================================================================
!
   ient = 1
   GOTO 100
 300  CALL merge(ipv1(1),ipv1(1),Core(1))
   CALL wrttrl(Ia)
 400  DO
      IF ( iout==2 ) THEN
         iout = 3
         Ia(1) = M1dd
         Ia11(1) = Maa
         CALL rdtrl(Ia11)
         IF ( Ia11(1)>0 ) GOTO 300
         M1dd = 0
      ELSEIF ( iout==3 ) THEN
         iout = 4
         Ia(1) = K41dd
         Ia11(1) = K4aa
         CALL rdtrl(Ia11)
         IF ( Ia11(1)>0 ) GOTO 300
         K41dd = 0
      ELSEIF ( iout==4 ) THEN
         GOTO 200
      ELSE
         iout = 2
         Ia(1) = B1dd
         Ia11(1) = Baa
         CALL rdtrl(Ia11)
         IF ( Ia11(1)>0 ) GOTO 300
         B1dd = 0
      ENDIF
   ENDDO
END SUBROUTINE gkad1a
