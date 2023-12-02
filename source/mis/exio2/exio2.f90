!*==exio2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE exio2
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_XMSSG
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: eqf , fort , rewi , sofin , sofout
   INTEGER :: i , iomode , nogo
   INTEGER , DIMENSION(32) , SAVE :: num
   EXTERNAL exi2 , exo2 , page2
!
! End of declarations rewritten by SPAG
!
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
         IF ( Uname(2)==num(Unit) ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
      ENDDO
   ENDIF
   nogo = 1
   CALL page2(-2)
   WRITE (Nout,99001) Uwm , Uname
99001 FORMAT (A25,' 6356, ',2A4,' IS AN INVALID UNIT FOR MODULE EXIO,',' EXTERNAL FORMAT')
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     DECODE MODE OF OPERATION
!
      iomode = 0
      IF ( Mode(1)==sofout ) iomode = 1
      IF ( Mode(1)==sofin ) iomode = 2
      IF ( iomode<=0 ) THEN
         nogo = 1
         CALL page2(-2)
         WRITE (Nout,99002) Uwm , Mode
!
!     MESSAGE TEXT
!
99002    FORMAT (A25,' 6338, ',2A4,' IS AN INVALID MODE PARAMETER FOR ','MODULE EXIO')
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
   END SUBROUTINE spag_block_1
!
!     NORMAL MODULE COMPLETION
!
END SUBROUTINE exio2
