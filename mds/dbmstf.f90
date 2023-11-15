
SUBROUTINE dbmstf
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
!
! COMMON variable declarations
!
   INTEGER Isysbf , Iwr , Lout
   COMMON /logout/ Lout
   COMMON /system/ Isysbf , Iwr
!
! Local variable declarations
!
   INTEGER iblksz , imemnu , iperc1
   REAL perc1
!
! End of declarations
!
   iblksz = Isysbf - 4
   IF ( Maxblk/=0 ) perc1 = Maxblk*1.0/Maxalc
   iperc1 = perc1*100.
   imemnu = (Maxalc-Maxblk)*Lenalc
   WRITE (Lout,99001) Lenopc , Idblen , Maxblk , Maxalc , iperc1 , Maxdsk , iblksz , Numopn , Numcls , Numwri , Numrea
99001 FORMAT (1H1,5X,'STATISTICS ON IN-MEMORY DATA BASE AND DISK I/O USAGE',/,/,8X,                                                 &
             &' LENGTH (IN WORDS) OF OPEN CORE ALLOCATED          ',I16,/,8X,' LENGTH (IN WORDS) OF IN-MEMORY DATA BASE ALLOCATED', &
            & I16,/,8X,' NUMBER OF BLOCKS USED IN THE IN-MEMORY DATA BASE  ',I16,/,8X,                                              &
             &' NUMBER OF BLOCKS ALLOCATED FOR THE IN-MEMORY DATA ',I16,/,8X,' PERCENTAGE OF IN-MEMORY DATA USED                 ', &
            & I16,'%',/,8X,' TOTAL BLOCKS WRITTEN TO DISK                      ',I16,/,8X,                                          &
             &' BLOCK SIZE (IN WORDS)                             ',I16,/,8X,' NUMBER OF OPENS TO DISK FILES                     ', &
            & I16,/,8X,' NUMBER OF CLOSES TO DISK FILES                    ',I16,/,8X,                                              &
             &' NUMBER OF WRITES TO DISK FILES                    ',I16,/,8X,' NUMBER OF READS FROM DISK FILES                   ', &
            & I16)
   IF ( Idbdir/=0 ) WRITE (Lout,99002) imemnu
99002 FORMAT (8X,' MEMORY (IN WORDS) NOT USED BY IN-MEM. DATA BASE   ',I16)
END SUBROUTINE dbmstf
