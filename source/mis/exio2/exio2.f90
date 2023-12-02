!*==exio2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE exio2
   USE c_blank
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
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
   IF ( uname(1)==fort ) THEN
      DO i = 1 , 32
         unit = i
         IF ( uname(2)==num(unit) ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
      ENDDO
   ENDIF
   nogo = 1
   CALL page2(-2)
   WRITE (nout,99001) uwm , uname
99001 FORMAT (A25,' 6356, ',2A4,' IS AN INVALID UNIT FOR MODULE EXIO,',' EXTERNAL FORMAT')
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     DECODE MODE OF OPERATION
!
      Iomode = 0
      IF ( mode(1)==Sofout ) Iomode = 1
      IF ( mode(1)==Sofin ) Iomode = 2
      IF ( Iomode<=0 ) THEN
         Nogo = 1
         CALL page2(-2)
         WRITE (Nout,99001) Uwm , mode
!
!     MESSAGE TEXT
!
99001    FORMAT (A25,' 6338, ',2A4,' IS AN INVALID MODE PARAMETER FOR ','MODULE EXIO')
      ENDIF
!
!     IF ERRORS THEN QUIT
!
      IF ( Nogo==0 ) THEN
!
!     SET POSITION AND UNIVAC FLAGS
!
         univac = .TRUE.
         IF ( xblk<=0 ) xblk = 3960
         xblk = xblk - mod(xblk,132)
         lbuf = xblk/ncpw
         IF ( mod(xblk,ncpw)/=0 ) lbuf = lbuf + 1
         iadd = 2
         IF ( pos(1)==Rewi ) iadd = 1
         IF ( pos(1)==Eqf ) iadd = 3
!
!     BRANCH ON MODE OF OPERATION
!
         IF ( Iomode==2 ) THEN
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
         dry = -2
      ENDIF
   END SUBROUTINE spag_block_1
!
!     NORMAL MODULE COMPLETION
!
END SUBROUTINE exio2
