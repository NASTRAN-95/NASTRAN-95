!*==amgrod.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgrod(D,Beta)
   USE c_dlbdy
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
   nlzb = nbz
   nfyb = nb + 1 - nby
   nlyb = nb
   ibuf1 = ecore - sysbuf
!
!     CALCULATE DZ ON SCR1
!
   IF ( ntzs/=0 ) THEN
      IF ( next+2*ntzs>ibuf1 ) CALL mesage(-8,0,name)
      CALL gopen(scr1,z(ibuf1),1)
      idzdy = 0
      CALL dzymat(D,nfzb,nlzb,ntzs,idzdy,scr1,z(ix),Beta,iprnt,z(inb),z(inc),z(iys),z(izs),z(isg),z(icg),z(iyb),z(izb),z(inbea1))
      CALL close(scr1,1)
   ENDIF
   IF ( ntys/=0 ) THEN
      IF ( next+2*ntys>ibuf1 ) CALL mesage(-8,0,name)
      CALL gopen(scr2,z(ibuf1),1)
      idzdy = 1
      CALL dzymat(D,nfyb,nlyb,ntys,idzdy,scr2,z(ix),Beta,iprnt,z(inb),z(inc),z(iys),z(izs),z(isg),z(icg),z(iyb),z(izb),z(inbea1))
      CALL close(scr2,1)
   ENDIF
END SUBROUTINE amgrod
