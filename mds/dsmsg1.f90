
SUBROUTINE dsmsg1(Block)
   IMPLICIT NONE
   REAL A1(4) , A2(4)
   INTEGER Ieol2 , Ieor2 , Incr3 , Incr4 , Irow1 , Irow2 , Irow3 , Irow4 , Itin3 , Itout3 , Itout4 , Iwr , None , Nrow3 , Nrow4
   COMMON /packx / Itin3 , Itout3 , Irow3 , Nrow3 , Incr3
   COMMON /system/ None , Iwr
   COMMON /unpakx/ Itout4 , Irow4 , Nrow4 , Incr4
   COMMON /zblpkx/ A1 , Irow1
   COMMON /zntpkx/ A2 , Irow2 , Ieol2 , Ieor2
   INTEGER Block(15)
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