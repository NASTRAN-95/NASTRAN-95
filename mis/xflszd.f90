
SUBROUTINE xflszd(File,Iblock,Filnam)
!
!     XFLSZD (EXECUTIVE FILE SIZE DETERMINATOR) ACCUMULATES THE
!     NUMBER OF BLOCKS USED FOR A FILE (FILE LT 0) IN THE FIAT OR
!     FOR A FILE (FILE GT 0) IN THE DATA POOL FILE.
!     IF FILE GT 0 IT IS THE INDEX OF THE FILE ON THE DATA POOL FILE
!     IF FILE = 0 THE NUMBER OF WORDS PER BLOCK IS RETURNED IN IBLOCK
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Fiat(1) , Ifist(1) , Kystem , Lfist , Mach , Nfist , Pool(1)
   COMMON /machin/ Mach
   COMMON /system/ Kystem
   COMMON /xdpl  / Pool
   COMMON /xfiat / Fiat
   COMMON /xfist / Nfist , Lfist , Ifist
!
! Dummy argument declarations
!
   INTEGER File , Filnam , Iblock
!
! Local variable declarations
!
   INTEGER andf , rshift
   INTEGER i , indx , lim , mask
   EXTERNAL andf , rshift
!
! End of declarations
!
!
   DATA mask/32767/
!
   IF ( File<0 ) THEN
!
!     FILE IS IN THE FIAT
!
!     COMMENTS FROM G.CHAN/UNIVAC 8/90
!     VAX AND VAX-DERIVED MACHINES DO NOT SAVE ANY INFORMATION OF BLOCKS
!     USED IN FIAT 7TH AND 8TH WORDS. THEREFORE, IBLOCK IS ALWAYS ZERO.
!
!
      lim = 2*Lfist
      DO i = 1 , lim , 2
         IF ( Filnam==Ifist(i) ) THEN
            IF ( Ifist(i+1)<=0 ) EXIT
            indx = Ifist(i+1)
            Iblock = rshift(Fiat(indx+7),16) + andf(mask,Fiat(indx+8)) + rshift(Fiat(indx+8),16)
!            = BLOCK COUNT ON PRIMARY, SECONDARY AND TERTIARY FILES ??
!
            GOTO 99999
         ENDIF
      ENDDO
      Iblock = 0
   ELSEIF ( File==0 ) THEN
!
!     USER WANTS THE NUMBER OF WORDS PER BLOCK
!
      IF ( Mach==2 .OR. Mach>=5 ) Iblock = Kystem - 4
   ELSE
!
!     FILE IS ON THE DATA POOL FILE
!
      indx = File*3 + 3
      Iblock = rshift(Pool(indx),16)
   ENDIF
99999 RETURN
END SUBROUTINE xflszd
