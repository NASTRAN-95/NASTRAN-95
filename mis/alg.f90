
SUBROUTINE alg
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Algdb , Apress , Atemp , Ifail , Ipgeom , Iprtc , Iprtk , Iscr3 , Istrml , Iz(1) , Log1 , Log2 , Log3 , Log4 , Log5 ,    &
         & Log6 , Naero , Nanal , Narbit , Nbldes , Nout , Pgeom , Strml , Sysbuf
   REAL Chordd(21) , Fxcoor , Fycoor , Fzcoor , Sign , Stag(21) , Zorign
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /algino/ Iscr3 , Algdb
   COMMON /blank / Apress , Atemp , Strml , Pgeom , Iprtk , Ifail , Sign , Zorign , Fxcoor , Fycoor , Fzcoor
   COMMON /contrl/ Nanal , Naero , Narbit , Log1 , Log2 , Log3 , Log4 , Log5 , Log6
   COMMON /system/ Sysbuf , Nout
   COMMON /ud3prt/ Iprtc , Istrml , Ipgeom
   COMMON /udstr2/ Nbldes , Stag , Chordd
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Iz
!
! Local variable declarations
!
   INTEGER ibuf1 , ibuf2 , ibuf3 , ierr , ifile , iscr1 , iscr2 , iscr4 , name(2) , nz , title1(18) , wd(2)
   INTEGER korsz
!
! End of declarations
!
!
!     THIS IS THE DRIVER SUBROUTINE FOR THE ALG MODULE
!
   DATA name/4HALG  , 4H    /
   DATA wd/2HNO , 2HAN/
   DATA iscr1 , iscr2/301 , 302/
!
   Iscr3 = 303
   iscr4 = 304
   Istrml = Strml
   Ipgeom = Pgeom
   IF ( Ipgeom==3 ) Ipgeom = 1
   Iprtc = Iprtk
   nz = korsz(Iz)
   ibuf1 = nz - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   IF ( 3*Sysbuf>nz ) CALL mesage(-8,0,name)
   CALL algpr(ierr)
   IF ( ierr>=0 ) THEN
      Algdb = iscr1
      IF ( ierr==1 ) Algdb = iscr2
      Log1 = Algdb
      Log2 = Nout
      Log3 = 7
      Log4 = Algdb
      Log5 = iscr4
      Log6 = 9
      CALL gopen(Log1,Iz(ibuf1),0)
      CALL fread(Log1,title1,18,1)
      CALL fread(Log1,Nanal,1,0)
      CALL fread(Log1,Naero,1,1)
      Narbit = 0
      IF ( Iprtc==1 ) WRITE (Log2,99001) title1 , Nanal , wd(Naero+1)
99001 FORMAT (1H1,/40X,48HALG MODULE - COMPRESSOR DESIGN - CONTROL SECTION,/40X,48(1H*),//10X,8HTITLE = ,18A4,/10X,                 &
             &39HNUMBER OF ANALYTIC MEALINE BLADEROWS = ,I3,/10X,14HTHERE WILL BE ,A2,33H ENTRY TO THE AERODYNAMIC SECTION)
      IF ( Iprtc==0 ) WRITE (Log2,99002) Uim
99002 FORMAT (A29,' - MODULE ALG ENTERED.')
!
      IF ( Nanal/=0 ) THEN
         ifile = Log5
         CALL open(*100,Log5,Iz(ibuf2),1)
         CALL algan
         CALL close(Log5,1)
      ENDIF
      IF ( Naero/=0 ) THEN
         ifile = Log5
         CALL open(*100,Log5,Iz(ibuf2),0)
         ifile = Iscr3
         CALL open(*100,Iscr3,Iz(ibuf3),1)
         CALL algar
         CALL close(Iscr3,1)
         CALL close(Log5,1)
      ENDIF
      CALL close(Log1,1)
      CALL algpo(Iscr3)
   ENDIF
   GOTO 99999
 100  CALL mesage(-1,ifile,name)
!
99999 RETURN
END SUBROUTINE alg
