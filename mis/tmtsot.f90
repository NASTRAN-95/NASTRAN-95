
SUBROUTINE tmtsot
   IMPLICIT NONE
   REAL Dummy(74) , Rbldpk , Rgetst , Rgino , Rintpk , Rpack , Rputst , Runpak , Tbldpk , Tgetsb , Tgetst , Tgino , Tintpk ,        &
      & Tllcdp , Tllcsp , Tllrdp , Tllrsp , Tpack , Tputst , Ttlcdp , Ttlcsp , Ttlrdp , Ttlrsp , Tunpak
   INTEGER Isy77 , Isysbf , Nitems , Nout
   COMMON /ntime / Nitems , Tgino , Tbldpk , Tintpk , Tpack , Tunpak , Tgetst , Tputst , Ttlrsp , Ttlrdp , Ttlcsp , Ttlcdp ,        &
                 & Tllrsp , Tllrdp , Tllcsp , Tllcdp , Tgetsb , Rgino , Rbldpk , Rintpk , Rpack , Runpak , Rgetst , Rputst
   COMMON /system/ Isysbf , Nout , Dummy , Isy77
!
!     THIS SUBROUTINE PRINTS THE CONTENTS OF COMMON /NTIME/
!
!
   WRITE (Nout,99001) Nitems
99001 FORMAT (1H1,23X,' DIAG 35 OUTPUT OF TIMING CONSTANTS IN COMMON /NTIME/'/24X,                                                  &
             &' ----------------------------------------------------'//' NUMBER OF TIMING CONSTANTS IN COMMON /NTIME/   ',          &
             &'                         --- ',I11/)
   WRITE (Nout,99002) Tgino
99002 FORMAT (' READ + WRITE + BACKWARD READ                   ',' (AVERAGE PER WORD     ) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99003) Tbldpk
99003 FORMAT (' BLDPK  - PACK   SUCCESSIVE ELEMENTS OF A COLUMN',' (AVERAGE PER WORD     ) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99004) Tintpk
99004 FORMAT (' INTPK  - UNPACK SUCCESSIVE ELEMENTS OF A COLUMN',' (AVERAGE PER WORD     ) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99005) Tpack
99005 FORMAT (' PACK   - PACK   AN ENTIRE COLUMN               ',' (AVERAGE PER WORD     ) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99006) Tunpak
99006 FORMAT (' UNPACK - UNPACK AN ENTIRE COLUMN               ',' (AVERAGE PER WORD     ) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99007) Tgetst
99007 FORMAT (' GETSTR - FORWARD READ  A STRING OF DATA        ',' (AVERAGE PER WORD     ) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99008) Tputst
99008 FORMAT (' PUTSTR - WRITE A STRING OF DATA                ',' (AVERAGE PER WORD     ) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99009) Ttlrsp
99009 FORMAT (' TIGHT-LOOP MULTIPLY - REAL    SINGLE PRECISION ',' (AVERAGE PER OPERATION) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99010) Ttlrdp
99010 FORMAT (' TIGHT-LOOP MULTIPLY - REAL    DOUBLE PRECISION ',' (AVERAGE PER OPERATION) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99011) Ttlcsp
99011 FORMAT (' TIGHT-LOOP MULTIPLY - COMPLEX SINGLE PRECISION ',' (AVERAGE PER OPERATION) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99012) Ttlcdp
99012 FORMAT (' TIGHT-LOOP MULTIPLY - COMPLEX DOUBLE PRECISION ',' (AVERAGE PER OPERATION) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99013) Tllrsp
99013 FORMAT (' LOOSE-LOOP MULTIPLY - REAL    SINGLE PRECISION ',' (AVERAGE PER OPERATION) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99014) Tllrdp
99014 FORMAT (' LOOSE-LOOP MULTIPLY - REAL    DOUBLE PRECISION ',' (AVERAGE PER OPERATION) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99015) Tllcsp
99015 FORMAT (' LOOSE-LOOP MULTIPLY - COMPLEX SINGLE PRECISION ',' (AVERAGE PER OPERATION) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99016) Tllcdp
99016 FORMAT (' LOOSE-LOOP MULTIPLY - COMPLEX DOUBLE PRECISION ',' (AVERAGE PER OPERATION) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99017) Tgetsb
99017 FORMAT (' GETSTB - BACKWARD READ  A STRING OF DATA       ',' (AVERAGE PER WORD     ) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99018) Rgino
99018 FORMAT (' READ + WRITE + BACKWARD READ                   ',' (AVERAGE PER RECORD   ) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99019) Rbldpk
99019 FORMAT (' BLDPK  - PACK   SUCCESSIVE ELEMENTS OF A COLUMN',' (AVERAGE PER RECORD   ) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99020) Rintpk
99020 FORMAT (' INTPK    UNPACK SUCCESSIVE ELEMENTS OF A COLUMN',' (AVERAGE PER RECORD   ) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99021) Rpack
99021 FORMAT (' PACK   - PACK   AN ENTIRE COLUMN               ',' (AVERAGE PER RECORD   ) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99022) Runpak
99022 FORMAT (' UNPACK - UNPACK AN ENTIRE COLUMN               ',' (AVERAGE PER RECORD   ) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99023) Rgetst
99023 FORMAT (' GETSTR - READ  A STRING OF DATA                ',' (AVERAGE PER RECORD   ) --- ',E11.4,' MICROSECONDS'/)
   WRITE (Nout,99024) Rputst
99024 FORMAT (' PUTSTR - WRITE A STRING OF DATA                ',' (AVERAGE PER RECORD   ) --- ',E11.4,' MICROSECONDS'/)
   IF ( Isy77/=-3 ) WRITE (Nout,99025)
99025 FORMAT ('0*** NASTRAN INFORMATION MESSAGE, TO INCORPORATE THESE ','TIMING CONSTANTS INTO NASTRAN PERMANENTLY',/5X,            &
             &'RE-RUN JOB WITH ''NASTRAN BULKDATA=-3'' FOR MORE ','INSTRUCTIONS',/)
   RETURN
END SUBROUTINE tmtsot