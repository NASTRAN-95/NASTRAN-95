!*==dbmrel.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dbmrel
   IMPLICIT NONE
   USE I_DSIOF
   USE I_ZZZZZZ
   USE C_SYSTEM
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: index , isave
!
! End of declarations rewritten by SPAG
!
!********************************************************************
!  DBMREL  -   RELEASES IN-MEMORY BLOCKS THAT ARE CURRENTLY
!              ALLOCATED TO AN IN-MEMORY FILE
!********************************************************************
   IF ( fcb(9,ifilex)==0 .OR. fcb(10,ifilex)==0 ) THEN
      WRITE (Iwr,99001)
99001 FORMAT (///,' ERROR IN ATTEMPT TO FREE BLOCKS TO FREE CHAIN',/,' CONTENTS OF THE DIRECTORY ARE AS FOLLOWS')
      CALL dbmdmp
      CALL mesage(-61,0,0)
   ELSEIF ( idbfre/=0 ) THEN
! SET FIRST OF BLOCKS TO BE FREED AT FIRST OF FREE CHAIN AND
! THEN CONNECT LAST OF BLOCKS TO BE FREED WITH FIRST OF EXISTING
! FREE CHAIN
      IF ( fcb(9,ifilex)==fcb(10,ifilex) ) THEN
! FILE HAD ONLY ONLY ONE BLOCK ALLOCATED TO IT
         isave = idbfre
         idbfre = fcb(9,ifilex)
         mem(isave) = idbfre
         mem(idbfre+1) = isave
      ELSE
         isave = idbfre
         idbfre = fcb(9,ifilex)
         mem(isave) = fcb(10,ifilex)
         index = fcb(10,ifilex)
         mem(index+1) = isave
      ENDIF
   ELSE
! FREE CHAIN IS EMPTY, THIS CHAIN BECOMES FREE CHAIN
      idbfre = fcb(9,ifilex)
   ENDIF
   fcb(9,ifilex) = 0
   fcb(10,ifilex) = 0
   fcb(11,ifilex) = 0
END SUBROUTINE dbmrel
