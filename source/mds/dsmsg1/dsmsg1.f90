!*==dsmsg1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsmsg1(Block)
   IMPLICIT NONE
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZBLPKX
   USE C_ZNTPKX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(15) :: Block
!
! End of declarations rewritten by SPAG
!
   WRITE (Iwr,99001)
99001 FORMAT (' *** ERROR OCCURRED IN PAKUNPK I/O SUBSYSTEM ***')
   WRITE (Iwr,99002)
99002 FORMAT (' CONTENTS OF THE STRING CONTROL BLOCK')
   WRITE (Iwr,99007) Block
   WRITE (Iwr,99003)
99003 FORMAT (' CONTENTS OF /ZBLPKX/')
   WRITE (Iwr,99007) A1 , Irow1
   WRITE (Iwr,99004)
99004 FORMAT (' CONTENTS OF /ZNTPKX/')
   WRITE (Iwr,99007) A2 , Irow2 , Ieol2 , Ieor2
   WRITE (Iwr,99005)
99005 FORMAT (' CONTENTS OF /PACKX/ ')
   WRITE (Iwr,99007) Itin3 , Itout3 , Irow3 , Nrow3 , Incr3
   WRITE (Iwr,99006)
99006 FORMAT (' CONTENTS OF /UNPAKX/')
   WRITE (Iwr,99007) Itout4 , Irow4 , Nrow4 , Incr4
99007 FORMAT (10(5(1X,Z8),/))
END SUBROUTINE dsmsg1
