!*==alg.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg
   USE c_algino
   USE c_blank
   USE c_contrl
   USE c_system
   USE c_ud3prt
   USE c_udstr2
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ibuf1 , ibuf2 , ibuf3 , ierr , ifile , iscr4 , nz
   INTEGER , SAVE :: iscr1 , iscr2
   INTEGER , DIMENSION(2) , SAVE :: name , wd
   INTEGER , DIMENSION(18) :: title1
   EXTERNAL algan , algar , algpo , algpr , close , fread , gopen , korsz , mesage , open
!
! End of declarations rewritten by SPAG
!
!
!     THIS IS THE DRIVER SUBROUTINE FOR THE ALG MODULE
!
   DATA name/4HALG  , 4H    /
   DATA wd/2HNO , 2HAN/
   DATA iscr1 , iscr2/301 , 302/
!
   iscr3 = 303
   iscr4 = 304
   istrml = strml
   ipgeom = pgeom
   IF ( ipgeom==3 ) ipgeom = 1
   iprtc = iprtk
   nz = korsz(iz)
   ibuf1 = nz - sysbuf + 1
   ibuf2 = ibuf1 - sysbuf
   ibuf3 = ibuf2 - sysbuf
   IF ( 3*sysbuf>nz ) CALL mesage(-8,0,name)
   CALL algpr(ierr)
   IF ( ierr>=0 ) THEN
      algdb = iscr1
      IF ( ierr==1 ) algdb = iscr2
      log1 = algdb
      log2 = nout
      log3 = 7
      log4 = algdb
      log5 = iscr4
      log6 = 9
      CALL gopen(log1,iz(ibuf1),0)
      CALL fread(log1,title1,18,1)
      CALL fread(log1,nanal,1,0)
      CALL fread(log1,naero,1,1)
      narbit = 0
      IF ( iprtc==1 ) WRITE (log2,99001) title1 , nanal , wd(naero+1)
99001 FORMAT (1H1,/40X,48HALG MODULE - COMPRESSOR DESIGN - CONTROL SECTION,/40X,48(1H*),//10X,8HTITLE = ,18A4,/10X,                 &
             &39HNUMBER OF ANALYTIC MEALINE BLADEROWS = ,I3,/10X,14HTHERE WILL BE ,A2,33H ENTRY TO THE AERODYNAMIC SECTION)
      IF ( iprtc==0 ) WRITE (log2,99002) uim
99002 FORMAT (A29,' - MODULE ALG ENTERED.')
!
      IF ( nanal/=0 ) THEN
         ifile = log5
         CALL open(*100,log5,iz(ibuf2),1)
         CALL algan
         CALL close(log5,1)
      ENDIF
      IF ( naero/=0 ) THEN
         ifile = log5
         CALL open(*100,log5,iz(ibuf2),0)
         ifile = iscr3
         CALL open(*100,iscr3,iz(ibuf3),1)
         CALL algar
         CALL close(iscr3,1)
         CALL close(log5,1)
      ENDIF
      CALL close(log1,1)
      CALL algpo(iscr3)
   ENDIF
   RETURN
 100  CALL mesage(-1,ifile,name)
!
END SUBROUTINE alg
