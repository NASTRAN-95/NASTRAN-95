!*==amgrod.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgrod(D,Beta)
   IMPLICIT NONE
   USE C_DLBDY
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: D
   REAL :: Beta
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ibuf1 , idzdy , iprnt , nfyb , nfzb , nlyb , nlzb
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , dzymat , gopen , mesage , sswtch
!
! End of declarations rewritten by SPAG
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
