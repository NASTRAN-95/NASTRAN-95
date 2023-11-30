
SUBROUTINE cfactr(A,Ll,Ul,Scr1,Scr2,Scr3,Iopt)
   IMPLICIT NONE
   REAL D(5) , D1(2) , Powr
   DOUBLE PRECISION Det(2) , Mind
   INTEGER Fa(7) , Fl(7) , Fu(7) , Ib , Ibbar , Ichol , Iz(1) , J1fil , J2fil , Jfa(7) , Jfc(7) , Jfl(7) , Jx , M1fil , M2fil ,     &
         & M3fil , Mfa(7) , Mfc(7) , Mfl(7) , Mxx , Nx , Sr1 , Sr2 , Sr3
   COMMON /cdcmpx/ Fa , Fl , Fu , Sr1 , Sr2 , Sr3 , Det , Powr , Nx , Mind , Ib , Ibbar
   COMMON /sdccsp/ Jfa , Jfl , Jfc , J1fil , J2fil , Jx
   COMMON /sfact / Mfa , Mfl , Mfc , M1fil , M2fil , Mxx , D , M3fil , D1 , Ichol
   COMMON /zzzzzz/ Iz
   INTEGER A , Iopt , Ll , Scr1 , Scr2 , Scr3 , Ul
   INTEGER i , mcb(7) , name(2) , nz
   INTEGER korsz
!
   DATA name/4HCFAC , 4HTR  /
!
!
   nz = korsz(Iz)
   mcb(1) = A
   CALL rdtrl(mcb)
   IF ( mcb(4)/=6 ) THEN
!
!     UNSYMMETRIC  COMPLEX
!
      DO i = 1 , 7
         Fa(i) = mcb(i)
         Fl(i) = mcb(i)
         Fu(i) = mcb(i)
      ENDDO
      Fl(1) = Ll
      Fu(1) = Ul
      Fl(4) = 4
      Fu(4) = 5
      Sr1 = Scr1
      Sr2 = Scr2
      Sr3 = Scr3
      Nx = nz
!     IB    = 0
!
!     IF IB IS SET TO ZERO HERE, T08021 PRINTS 27 MORE MESSAGES 3027
!     AND 3028 FROM GENVEC WHICH IS CALLED BY CFACTR, WHCIH IS CALLED BY
!     FRD2C, IN FRRD2 MODULE
!
!IBMI 6/93
      Ibbar = 0
      CALL cdcomp(*100,Iz,Iz,Iz)
      CALL wrttrl(Fu)
      CALL wrttrl(Fl)
      Iopt = 1
   ELSE
!
!     SYMMETRIC  COMPLEX
!
      DO i = 1 , 7
         Mfa(i) = mcb(i)
         Mfl(i) = mcb(i)
         Mfc(i) = mcb(i)
      ENDDO
      Mfl(1) = Ll
      Mfc(1) = Ul
      Mfl(4) = 4
      Mfc(4) = 5
      M1fil = Scr1
      M2fil = Scr2
      Mxx = nz
      M3fil = Scr3
      Ichol = 0
      CALL sdcomp(*100,Iz,Iz,Iz)
      CALL wrttrl(Mfl)
      Iopt = 2
   ENDIF
   RETURN
!
!     ERRORS
!
 100  CALL mesage(-5,A,name)
!
END SUBROUTINE cfactr
