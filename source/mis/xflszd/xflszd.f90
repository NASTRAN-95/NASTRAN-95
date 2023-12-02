!*==xflszd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xflszd(File,Iblock,Filnam)
!
!     XFLSZD (EXECUTIVE FILE SIZE DETERMINATOR) ACCUMULATES THE
!     NUMBER OF BLOCKS USED FOR A FILE (FILE LT 0) IN THE FIAT OR
!     FOR A FILE (FILE GT 0) IN THE DATA POOL FILE.
!     IF FILE GT 0 IT IS THE INDEX OF THE FILE ON THE DATA POOL FILE
!     IF FILE = 0 THE NUMBER OF WORDS PER BLOCK IS RETURNED IN IBLOCK
!
   USE c_machin
   USE c_system
   USE c_xdpl
   USE c_xfiat
   USE c_xfist
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   INTEGER :: Iblock
   INTEGER :: Filnam
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , indx , lim
   INTEGER , SAVE :: mask
   EXTERNAL andf , rshift
!
! End of declarations rewritten by SPAG
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
      lim = 2*lfist
      SPAG_Loop_1_1: DO i = 1 , lim , 2
         IF ( Filnam==ifist(i) ) THEN
            IF ( ifist(i+1)<=0 ) EXIT SPAG_Loop_1_1
            indx = ifist(i+1)
            Iblock = rshift(fiat(indx+7),16) + andf(mask,fiat(indx+8)) + rshift(fiat(indx+8),16)
!            = BLOCK COUNT ON PRIMARY, SECONDARY AND TERTIARY FILES ??
!
            RETURN
         ENDIF
      ENDDO SPAG_Loop_1_1
      Iblock = 0
   ELSEIF ( File==0 ) THEN
!
!     USER WANTS THE NUMBER OF WORDS PER BLOCK
!
      IF ( mach==2 .OR. mach>=5 ) Iblock = kystem - 4
   ELSE
!
!     FILE IS ON THE DATA POOL FILE
!
      indx = File*3 + 3
      Iblock = rshift(pool(indx),16)
   ENDIF
END SUBROUTINE xflszd
