
SUBROUTINE dbmdia
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'ZZZZZZ.COM'
   INTEGER Isysbf , Iwr
   COMMON /system/ Isysbf , Iwr
   INTEGER i , iblksz , iextb , iintb , index , itoti , itotx , scratch(2)
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
         IF ( Fcb(9,i)/=0 .OR. Fcb(5,i)/=0 ) THEN
            index = Fcb(10,i)
            iintb = 0
            iextb = 0
            IF ( Fcb(9,i)/=0 ) iintb = Mem(index+3)
            itoti = itoti + iintb
            IF ( Fcb(5,i)/=0 ) iextb = Fcb(6,i) - Fcb(5,i) + 1
            IF ( iextb<Fcb(7,Ifilex) ) THEN
               itotx = itotx + iextb
               IF ( Fcb(13,i)==0 ) THEN
                  Fcb(13,i) = scratch(1)
                  Fcb(14,i) = scratch(2)
               ENDIF
               WRITE (Iwr,99002) i , Fcb(13,i) , Fcb(14,i) , Fcb(4,i) , iintb , iextb
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