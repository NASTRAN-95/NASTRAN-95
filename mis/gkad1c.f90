
SUBROUTINE gkad1c(Xmd,Xod,Xcr1,Xcr2,Xcr3,Xcr4,Xcr5,Xcr6,Xsetd)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL App(2) , G , Type(2) , Ua , Ud , Ue , Uf , Ufe , Ug , Ul , Um , Un , Une , Uo , Up , Ur , Us , Usb , Usg , W3 , W4
   INTEGER Ib2pp , Ik2pp , Im2pp , Modal(2) , Multi , Noue , Omit , Single
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug , Ue , Up , Une , Ufe , Ud
   COMMON /blank / Type , App , Modal , G , W3 , W4 , Ik2pp , Im2pp , Ib2pp , Multi , Single , Omit , Noue
!
! Dummy argument declarations
!
   INTEGER K2dd , K2pp , Xcr1 , Xcr2 , Xcr3 , Xcr4 , Xcr5 , Xcr6 , Xmd , Xod , Xsetd
!
! Local variable declarations
!
   INTEGER check , gmd , god , k2ff , k2nn , name(2) , scr1 , scr2 , scr3 , scr4 , scr5 , scr6 , usetd
!
! End of declarations
!
!
!     GKAD1C SETS UP TO REDUCE STRUCTURAL MODAL
!
!NV  3                MCB(7),T
   DATA name/4HGKAD , 4H1C  /
!
   gmd = Xmd
   god = Xod
   scr1 = Xcr1
   scr2 = Xcr2
   scr3 = Xcr3
   scr4 = Xcr4
   scr5 = Xcr5
   scr6 = Xcr6
   usetd = Xsetd
   check = 123456789
   RETURN
!
!
   ENTRY gkad1d(K2pp,K2dd)
!     ========================
!
   IF ( check/=123456789 ) CALL mesage(-37,0,name)
!
!     NAVY'S FIX (MARKED BY CNV) TO FORCE K2NN BE SYMMETRIC IF K2PP IS
!     SYMMETRIC. A PARAMETER OF -6 IS PASSED TO SSG2B TO FLAG THE FORM
!     OF THE MATRIX TO BE SYMMETRIC.
!     ALSO, IN SSG2B, ABOUT LINE 55, ADD FOLLOWING 2 LINES
!           IF (T1 .EQ. -6) T = 1
!           IF (T1 .EQ. -6) FILED(4) = SYMM
!
!     (THE FIX IS NOT ADOPTED HERE. A MORE GENERAL FIX IS ADDED IN SSG2B
!     WHICH SHOULD TAKE CARE OF THE PROBLEM HERE   G.C/UNISYS 3/93)
!
!NV   MCB(1) = K2PP
!NV   CALL RDTRL (MCB)
!NV   T = 1
!NV   IF (MCB(4) .EQ. 6) T = -6
!
   k2ff = K2dd
   IF ( Multi<0 ) THEN
      k2nn = K2pp
   ELSEIF ( Omit<0 .AND. Single<0 ) THEN
      k2nn = K2dd
   ELSE
      k2nn = scr4
      IF ( Single<0 ) k2nn = K2dd
   ENDIF
   IF ( Single<0 ) k2ff = k2nn
   IF ( Multi>=0 ) THEN
!
!     MULTI POINT CONSTRAINTS
!
      CALL upart(usetd,scr1,Up,Une,Um)
      CALL mpart(K2pp,scr2,scr3,scr5,scr4)
      CALL ssg2b(scr4,gmd,scr3,scr1,0,2,1,scr6)
      CALL ssg2b(scr5,gmd,scr2,scr3,0,2,1,scr6)
!
!NV   CALL SSG2B (GMD,SCR1,SCR3,K2NN,T,2,1,SCR6)
      CALL ssg2b(gmd,scr1,scr3,k2nn,1,2,1,scr6)
   ENDIF
!
   IF ( Single>=0 ) THEN
      CALL upart(usetd,scr1,Une,Ufe,Us)
      CALL mpart(k2nn,k2ff,0,0,0)
   ENDIF
   IF ( Omit>=0 ) THEN
      CALL upart(usetd,scr1,Ufe,Ud,Uo)
      CALL mpart(k2ff,scr2,scr3,scr5,scr4)
      CALL ssg2b(scr4,god,scr3,scr1,0,2,1,scr6)
      CALL ssg2b(scr5,god,scr2,scr3,0,2,1,scr6)
!
!NV   CALL SSG2B (GOD,SCR1,SCR3,K2DD,T,2,1,SCR6)
      CALL ssg2b(god,scr1,scr3,K2dd,1,2,1,scr6)
   ENDIF
!
END SUBROUTINE gkad1c
