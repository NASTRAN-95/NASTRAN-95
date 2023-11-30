
SUBROUTINE dbmnam(Igname,Name,Ifilex)
   IMPLICIT NONE
   INTEGER Fiat(100) , Fist(100)
   COMMON /xfiat / Fiat
   COMMON /xfist / Fist
   INTEGER Ifilex , Igname
   INTEGER Name(2)
   INTEGER blank , i , index , lim , pool
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
      lim = Fist(2)*2 - 1
      DO i = 1 , lim , 2
         IF ( Igname==Fist(2+i) ) THEN
            index = Fist(3+i)
            Name(1) = Fiat(index+2)
            Name(2) = Fiat(index+3)
            GOTO 99999
         ENDIF
      ENDDO
      Name(1) = 0
      Name(2) = 0
   ENDIF
99999 RETURN
END SUBROUTINE dbmnam
