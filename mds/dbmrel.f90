
SUBROUTINE dbmrel
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'ZZZZZZ.COM'
   INTEGER Isysbf , Iwr
   COMMON /system/ Isysbf , Iwr
   INTEGER index , isave
!********************************************************************
!  DBMREL  -   RELEASES IN-MEMORY BLOCKS THAT ARE CURRENTLY
!              ALLOCATED TO AN IN-MEMORY FILE
!********************************************************************
   IF ( Fcb(9,Ifilex)==0 .OR. Fcb(10,Ifilex)==0 ) THEN
      WRITE (Iwr,99001)
99001 FORMAT (///,' ERROR IN ATTEMPT TO FREE BLOCKS TO FREE CHAIN',/,' CONTENTS OF THE DIRECTORY ARE AS FOLLOWS')
      CALL dbmdmp
      CALL mesage(-61,0,0)
   ELSEIF ( Idbfre/=0 ) THEN
! SET FIRST OF BLOCKS TO BE FREED AT FIRST OF FREE CHAIN AND
! THEN CONNECT LAST OF BLOCKS TO BE FREED WITH FIRST OF EXISTING
! FREE CHAIN
      IF ( Fcb(9,Ifilex)==Fcb(10,Ifilex) ) THEN
! FILE HAD ONLY ONLY ONE BLOCK ALLOCATED TO IT
         isave = Idbfre
         Idbfre = Fcb(9,Ifilex)
         Mem(isave) = Idbfre
         Mem(Idbfre+1) = isave
      ELSE
         isave = Idbfre
         Idbfre = Fcb(9,Ifilex)
         Mem(isave) = Fcb(10,Ifilex)
         index = Fcb(10,Ifilex)
         Mem(index+1) = isave
      ENDIF
   ELSE
! FREE CHAIN IS EMPTY, THIS CHAIN BECOMES FREE CHAIN
      Idbfre = Fcb(9,Ifilex)
   ENDIF
   Fcb(9,Ifilex) = 0
   Fcb(10,Ifilex) = 0
   Fcb(11,Ifilex) = 0
END SUBROUTINE dbmrel
