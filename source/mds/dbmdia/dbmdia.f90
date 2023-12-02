!*==dbmdia.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dbmdia
   IMPLICIT NONE
   USE I_DSIOF
   USE I_ZZZZZZ
   USE C_SYSTEM
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
   iblksz = Isysbf - 4
   itoti = 0
   itotx = 0
   WRITE (Iwr,99001)
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
               WRITE (Iwr,99002) i , fcb(13,i) , fcb(14,i) , fcb(4,i) , iintb , iextb
99002          FORMAT (I7,3X,2A4,2X,I6,2X,I6,2X,I6)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   WRITE (Iwr,99003) itoti , itotx
99003 FORMAT (/,' CURRENT IN-MEMORY BLOCKS =',I16,/,' CURRENT DISK BLOCKS      =',I16)
!      WRITE ( IWR, 906 ) MAXBLK, MAXDSK, MAXALC, IBLKSZ
   RETURN
99004 FORMAT (/,' MAXIMUM IN-MEMORY BLOCKS USED                   =',I16,/,' MAXIMUM DISK BLOCKS WRITTEN                     =',I16,&
            & /,' BLOCKS INITIALLY ALLOCATED FOR THE IN-MEMORY DB =',I16,/,' BLOCK SIZE                                      =',I16)
END SUBROUTINE dbmdia
