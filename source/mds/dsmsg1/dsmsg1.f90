!*==dsmsg1.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsmsg1(Block)
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_zblpkx
   USE c_zntpkx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(15) :: Block
!
! End of declarations rewritten by SPAG
!
   WRITE (iwr,99001)
99001 FORMAT (' *** ERROR OCCURRED IN PAKUNPK I/O SUBSYSTEM ***')
   WRITE (iwr,99002)
99002 FORMAT (' CONTENTS OF THE STRING CONTROL BLOCK')
   WRITE (iwr,99007) Block
   WRITE (iwr,99003)
99003 FORMAT (' CONTENTS OF /ZBLPKX/')
   WRITE (iwr,99007) a1 , irow1
   WRITE (iwr,99004)
99004 FORMAT (' CONTENTS OF /ZNTPKX/')
   WRITE (iwr,99007) a2 , irow2 , ieol2 , ieor2
   WRITE (iwr,99005)
99005 FORMAT (' CONTENTS OF /PACKX/ ')
   WRITE (iwr,99007) itin3 , itout3 , irow3 , nrow3 , incr3
   WRITE (iwr,99006)
99006 FORMAT (' CONTENTS OF /UNPAKX/')
   WRITE (iwr,99007) itout4 , irow4 , nrow4 , incr4
99007 FORMAT (10(5(1X,Z8),/))
END SUBROUTINE dsmsg1
