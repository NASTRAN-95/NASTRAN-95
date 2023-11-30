
SUBROUTINE exio2
   IMPLICIT NONE
   REAL Datype(2) , Device(2) , Formt(2) , Sysbuf , X1(36)
   INTEGER Dry , Iadd , Lbuf , Mode(2) , Names(10) , Nbpc , Nbpw , Ncpw , Nout , Pos(2) , Uname(2) , Unit , Xblk
   CHARACTER*23 Ufm
   LOGICAL Univac
   CHARACTER*25 Uwm
   COMMON /blank / Dry , Xblk , Device , Uname , Formt , Mode , Pos , Datype , Names , Unit , Univac , Lbuf , Iadd
   COMMON /system/ Sysbuf , Nout , X1 , Nbpc , Nbpw , Ncpw
   COMMON /xmssg / Ufm , Uwm
   INTEGER eqf , fort , i , iomode , nogo , num(32) , rewi , sofin , sofout
!
!     EXIO2 COPIES SUBSTRUCTURE ITEMS BETWEEN THE SOF AND AN EXTERNAL
!     TAPE USING FORTRAN FORMATTED IO.  THE TAPE COULD HAVE BEEN CREATED
!     OR COULD BE READ ON A DIFFERENT BRAND OF COMPUTER.
!
!
   DATA fort , sofin , sofout , rewi , eqf/4HFORT , 4HSOFI , 4HSOFO , 4HREWI , 4HEOF /
   DATA num/2H1  , 2H2  , 2H3  , 2H4  , 2H5  , 2H6  , 2H7  , 2H8  , 2H9  , 2H10 , 2H11 , 2H12 , 2H13 , 2H14 , 2H15 , 2H16 , 2H17 ,  &
       &2H18 , 2H19 , 2H20 , 2H21 , 2H22 , 2H23 , 2H24 , 2H25 , 2H26 , 2H27 , 2H28 , 2H29 , 2H30 , 2H31 , 2H32/
!
!     INITIALIZE
!
   nogo = 0
!
!     DECODE FORTRAN UNIT
!
   IF ( Uname(1)==fort ) THEN
      DO i = 1 , 32
         Unit = i
         IF ( Uname(2)==num(Unit) ) GOTO 100
      ENDDO
   ENDIF
   nogo = 1
   CALL page2(-2)
   WRITE (Nout,99001) Uwm , Uname
99001 FORMAT (A25,' 6356, ',2A4,' IS AN INVALID UNIT FOR MODULE EXIO,',' EXTERNAL FORMAT')
!
!     DECODE MODE OF OPERATION
!
 100  iomode = 0
   IF ( Mode(1)==sofout ) iomode = 1
   IF ( Mode(1)==sofin ) iomode = 2
   IF ( iomode<=0 ) THEN
      nogo = 1
      CALL page2(-2)
      WRITE (Nout,99002) Uwm , Mode
!
!     MESSAGE TEXT
!
99002 FORMAT (A25,' 6338, ',2A4,' IS AN INVALID MODE PARAMETER FOR ','MODULE EXIO')
   ENDIF
!
!     IF ERRORS THEN QUIT
!
   IF ( nogo==0 ) THEN
!
!     SET POSITION AND UNIVAC FLAGS
!
      Univac = .TRUE.
      IF ( Xblk<=0 ) Xblk = 3960
      Xblk = Xblk - mod(Xblk,132)
      Lbuf = Xblk/Ncpw
      IF ( mod(Xblk,Ncpw)/=0 ) Lbuf = Lbuf + 1
      Iadd = 2
      IF ( Pos(1)==rewi ) Iadd = 1
      IF ( Pos(1)==eqf ) Iadd = 3
!
!     BRANCH ON MODE OF OPERATION
!
      IF ( iomode==2 ) THEN
!
!     SOFIN
!
         CALL exi2
      ELSE
!
!     SOFOUT
!
         CALL exo2
      ENDIF
   ELSE
      Dry = -2
   ENDIF
!
!     NORMAL MODULE COMPLETION
!
   RETURN
END SUBROUTINE exio2