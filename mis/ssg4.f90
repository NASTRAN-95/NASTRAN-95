
SUBROUTINE ssg4
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Iomt
   REAL Ua , Uf , Ug , Ul , Um , Un , Uo , Ur , Us , Usb , Usg
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug
   COMMON /blank / Iomt
!
! Local variable declarations
!
   INTEGER d , go , mll , mlr , moab , moob , mr , pl , pli , po , poi , qr , scr1 , scr2 , scr3 , scr4 , scr5 , uset
   REAL xxx
!
! End of declarations
!
!
!     DRIVER TO DO INERTIAL RELIEF PORTION OF SSG
!
!     DMAP SEQUENCE
!
!     SSG4  PL,QR,PO,MR,MLR,D,MLL,MOOB,MOAB,GO,USET/PLI,POI/V,N,IOMT $
!
   DATA pl , qr , po , mr , mlr , d , mll , moob , moab , pli , poi , scr1 , scr2 , scr3 , scr4 , scr5 , go , uset/101 , 102 , 103 ,&
      & 104 , 105 , 106 , 107 , 108 , 109 , 201 , 202 , 301 , 302 , 303 , 304 , 305 , 110 , 111/
!
!     COMPUTE  MR-1*QR=TEMP2
!
   CALL factor(mr,scr1,scr2,scr3,scr4,scr5)
   CALL ssg3a(mr,scr1,qr,scr3,scr4,scr5,-1,xxx)
!
!     COMPUTE  MLL*D+MLR=TEMP1
!
   CALL ssg2b(mll,d,mlr,scr4,0,2,1,scr1)
!
!     COMPUTE  TEMP1*TEMP2+PL=PLI
!
   CALL ssg2b(scr4,scr3,pl,pli,0,2,1,scr1)
   IF ( Iomt>0 ) THEN
!
!     COMPUTE  MOOB*GO+MOAB=SCR4
!
      CALL ssg2b(moob,go,moab,scr4,0,2,1,scr1)
!
!     COMPUTE DI*TEMP2  =SCR2
!
      CALL ssg2b(d,scr3,0,scr2,0,2,1,scr1)
      CALL sdr1b(scr5,scr2,scr3,scr1,Ua,Ul,Uo,uset,0,0)
!
!     COMPUTE  SCR4*SCR1+PO=POI
!
      CALL ssg2b(scr4,scr1,po,poi,0,2,1,scr3)
   ENDIF
END SUBROUTINE ssg4
