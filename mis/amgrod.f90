
SUBROUTINE amgrod(D,Beta)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ecore , Ia0 , Ia0p , Iarb , Iavr , Icg , Ics , Idelx , Iee , Ifla1 , Ifla2 , Inas , Inasb , Inb , Inbea1 , Inbea2 , Inc ,&
         & Infl , Ins , Insbea , Int121 , Int122 , Iria , Isg , Ith1a , Ith2a , Ix , Ixic , Ixij , Ixis1 , Ixis2 , Ixlam , Ixle ,   &
         & Ixte , Iyb , Iyin , Iys , Izb , Izin , Izs , Nb , Nby , Nbz , Next , Nj1 , Nk1 , Np , Nt0 , Ntp , Nty , Ntys , Ntz ,     &
         & Ntzs , Scr1 , Scr2 , Sysbuf
   REAL Scr3 , Scr4 , Scr5 , Z(1)
   COMMON /dlbdy / Nj1 , Nk1 , Np , Nb , Ntp , Nbz , Nby , Ntz , Nty , Nt0 , Ntzs , Ntys , Inc , Ins , Inb , Inas , Izin , Iyin ,   &
                 & Inbea1 , Inbea2 , Insbea , Izb , Iyb , Iavr , Iarb , Infl , Ixle , Ixte , Int121 , Int122 , Izs , Iys , Ics ,    &
                 & Iee , Isg , Icg , Ixij , Ix , Idelx , Ixic , Ixlam , Ia0 , Ixis1 , Ixis2 , Ia0p , Iria , Inasb , Ifla1 , Ifla2 , &
                 & Ith1a , Ith2a , Ecore , Next , Scr1 , Scr2 , Scr3 , Scr4 , Scr5
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   REAL Beta
   REAL D(1)
!
! Local variable declarations
!
   INTEGER ibuf1 , idzdy , iprnt , name(2) , nfyb , nfzb , nlyb , nlzb
!
! End of declarations
!
!  D IS REALLY A 2-D ARRAY D(2,NTZS)
   DATA name/4HAMGR , 4HOD  /
   CALL sswtch(30,iprnt)
   nfzb = 1
   nlzb = Nbz
   nfyb = Nb + 1 - Nby
   nlyb = Nb
   ibuf1 = Ecore - Sysbuf
!
!     CALCULATE DZ ON SCR1
!
   IF ( Ntzs/=0 ) THEN
      IF ( Next+2*Ntzs>ibuf1 ) CALL mesage(-8,0,name)
      CALL gopen(Scr1,Z(ibuf1),1)
      idzdy = 0
      CALL dzymat(D,nfzb,nlzb,Ntzs,idzdy,Scr1,Z(Ix),Beta,iprnt,Z(Inb),Z(Inc),Z(Iys),Z(Izs),Z(Isg),Z(Icg),Z(Iyb),Z(Izb),Z(Inbea1))
      CALL close(Scr1,1)
   ENDIF
   IF ( Ntys/=0 ) THEN
      IF ( Next+2*Ntys>ibuf1 ) CALL mesage(-8,0,name)
      CALL gopen(Scr2,Z(ibuf1),1)
      idzdy = 1
      CALL dzymat(D,nfyb,nlyb,Ntys,idzdy,Scr2,Z(Ix),Beta,iprnt,Z(Inb),Z(Inc),Z(Iys),Z(Izs),Z(Isg),Z(Icg),Z(Iyb),Z(Izb),Z(Inbea1))
      CALL close(Scr2,1)
   ENDIF
END SUBROUTINE amgrod
