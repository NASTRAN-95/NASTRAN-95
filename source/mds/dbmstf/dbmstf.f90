!*==dbmstf.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dbmstf
   IMPLICIT NONE
   USE I_DSIOF
   USE C_LOGOUT
   USE C_SYSTEM
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: iblksz , imemnu , iperc1
   REAL :: perc1
!
! End of declarations rewritten by SPAG
!
   iblksz = Isysbf - 4
   IF ( maxblk/=0 ) perc1 = maxblk*1.0/maxalc
   iperc1 = perc1*100.
   imemnu = (maxalc-maxblk)*lenalc
   WRITE (Lout,99001) lenopc , idblen , maxblk , maxalc , iperc1 , maxdsk , iblksz , numopn , numcls , numwri , numrea
99001 FORMAT (1H1,5X,'STATISTICS ON IN-MEMORY DATA BASE AND DISK I/O USAGE',/,/,8X,                                                 &
             &' LENGTH (IN WORDS) OF OPEN CORE ALLOCATED          ',I16,/,8X,' LENGTH (IN WORDS) OF IN-MEMORY DATA BASE ALLOCATED', &
            & I16,/,8X,' NUMBER OF BLOCKS USED IN THE IN-MEMORY DATA BASE  ',I16,/,8X,                                              &
             &' NUMBER OF BLOCKS ALLOCATED FOR THE IN-MEMORY DATA ',I16,/,8X,' PERCENTAGE OF IN-MEMORY DATA USED                 ', &
            & I16,'%',/,8X,' TOTAL BLOCKS WRITTEN TO DISK                      ',I16,/,8X,                                          &
             &' BLOCK SIZE (IN WORDS)                             ',I16,/,8X,' NUMBER OF OPENS TO DISK FILES                     ', &
            & I16,/,8X,' NUMBER OF CLOSES TO DISK FILES                    ',I16,/,8X,                                              &
             &' NUMBER OF WRITES TO DISK FILES                    ',I16,/,8X,' NUMBER OF READS FROM DISK FILES                   ', &
            & I16)
   IF ( idbdir/=0 ) WRITE (Lout,99002) imemnu
99002 FORMAT (8X,' MEMORY (IN WORDS) NOT USED BY IN-MEM. DATA BASE   ',I16)
END SUBROUTINE dbmstf
