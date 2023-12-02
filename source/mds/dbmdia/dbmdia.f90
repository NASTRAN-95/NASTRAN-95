!*==dbmdia.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dbmdia
   USE i_dsiof
   USE i_zzzzzz
   USE c_system
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iblksz , iextb , iintb , index , itoti , itotx
   INTEGER , DIMENSION(2) , SAVE :: scratch
!
! End of declarations rewritten by SPAG
!
!********************************************************************
!     DBMDIA - DUMPS THE IN MEMORY DATA BASE DIRECTORY
!********************************************************************
!DME  19 JAN 2016
!DME  D. Everhart
!DME  GFORTRAN doesn't allow character assignment to INTEGER data.
!DME  This should be fixed to remove the HOLLERITH, though!
   DATA scratch/4HSCRA , 4HTCHX/
!DME  DATA              SCRATCH / 4HSCRA , 4HTCHX /
   iblksz = isysbf - 4
   itoti = 0
   itotx = 0
   WRITE (iwr,99001)
99001 FORMAT (///,27X,' MEMORY DATA BASE DIRECTORY',//,'    UNIT    NAME   CURRENT  IN-MEM','   DISK ',/,                           &
             &'                    BLOCK   BLOCKS','  BLOCKS ',/)
   DO i = 1 , 80
      IF ( i/=7 ) THEN
         IF ( fcb(9,i)/=0 .OR. fcb(5,i)/=0 ) THEN
            index = fcb(10,i)
            iintb = 0
            iextb = 0
            IF ( fcb(9,i)/=0 ) iintb = mem(index+3)
            itoti = itoti + iintb
            IF ( fcb(5,i)/=0 ) iextb = fcb(6,i) - fcb(5,i) + 1
            IF ( iextb<fcb(7,ifilex) ) THEN
               itotx = itotx + iextb
               IF ( fcb(13,i)==0 ) THEN
                  fcb(13,i) = scratch(1)
                  fcb(14,i) = scratch(2)
               ENDIF
               WRITE (iwr,99002) i , fcb(13,i) , fcb(14,i) , fcb(4,i) , iintb , iextb
99002          FORMAT (I7,3X,2A4,2X,I6,2X,I6,2X,I6)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   WRITE (iwr,99003) itoti , itotx
99003 FORMAT (/,' CURRENT IN-MEMORY BLOCKS =',I16,/,' CURRENT DISK BLOCKS      =',I16)
!      WRITE ( IWR, 906 ) MAXBLK, MAXDSK, MAXALC, IBLKSZ
   RETURN
99004 FORMAT (/,' MAXIMUM IN-MEMORY BLOCKS USED                   =',I16,/,' MAXIMUM DISK BLOCKS WRITTEN                     =',I16,&
            & /,' BLOCKS INITIALLY ALLOCATED FOR THE IN-MEMORY DB =',I16,/,' BLOCK SIZE                                      =',I16)
END SUBROUTINE dbmdia
