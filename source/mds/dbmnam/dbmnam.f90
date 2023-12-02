!*==dbmnam.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dbmnam(Igname,Name,Ifilex)
   IMPLICIT NONE
   USE c_xfiat
   USE c_xfist
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Igname
   INTEGER , DIMENSION(2) :: Name
   INTEGER :: Ifilex
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank , pool
   INTEGER :: i , index , lim
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!********************************************************************
!  DBMNAM RETURNS THE DMAP NAME AND TRAILER FOR A GIVEN GINO FILE
!     ARGUMENTS
!       IGNAME  (INPUT )   GINO FILE NAME (E.G., 101,201,301)
!       FIST    (INPUT )   COMMON BLOCK /XFIST/
!       FIAT    (INPUT )   COMMON BLOCK /XFIAT/
!       NAME    (OUTPUT)   (2A4) DMAP FILE NAME
!********************************************************************
   DATA pool/4HPOOL/ , blank/4H    /
   IF ( Igname<=100 .OR. Igname>=400 ) THEN
      Name(1) = Igname
      Name(2) = blank
      IF ( Ifilex==22 ) Name(1) = pool
   ELSE
      lim = fist(2)*2 - 1
      DO i = 1 , lim , 2
         IF ( Igname==fist(2+i) ) THEN
            index = fist(3+i)
            Name(1) = fiat(index+2)
            Name(2) = fiat(index+3)
            GOTO 99999
         ENDIF
      ENDDO
      Name(1) = 0
      Name(2) = 0
   ENDIF
99999 END SUBROUTINE dbmnam
