
SUBROUTINE bislc2(*,Id,Aa,Nc,Nr,Loc)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Loc , Nc , Nr
   INTEGER Aa(Nr,Nc) , Id(2)
!
! Local variable declarations
!
   INTEGER k , khi , klo
!
! End of declarations
!
!-----
!     BINARY SEARCH ROUTINE - LOCATE ID POSTION IN AA
!     SEARCH BY FIRST 2 WORDS (ROWS) OF ENTRIES.
!
!     ID  = TARGET WORD SEARCH, 2 BCD-WORDS
!     AA  = A (NR X NC) TABLE TO SEARCH FOR ID.
!     NR  = SIZE   OF ENTRIES (ROW   ) IN THE AA.
!     NC  = NUMBER OF ENTRIES (COLUMN) IN THE AA.
!     LOC = POINTER RETURNED, OF NC LOCATION
!
!     NONSTANDARD RETURN IN THE EVENT OF NO MATCH.
!
!
   klo = 1
   khi = Nc
 100  k = (klo+khi+1)/2
 200  IF ( Id(1)<Aa(1,k) ) THEN
      khi = k
   ELSEIF ( Id(1)==Aa(1,k) ) THEN
      IF ( Id(2)<Aa(2,k) ) THEN
         khi = k
      ELSEIF ( Id(2)==Aa(2,k) ) THEN
         Loc = k
         RETURN
      ELSE
         klo = k
      ENDIF
   ELSE
      klo = k
   ENDIF
   IF ( khi-klo<1 ) THEN
   ELSEIF ( khi-klo==1 ) THEN
      IF ( k==klo ) THEN
         k = khi
      ELSE
         k = klo
      ENDIF
      klo = khi
      GOTO 200
   ELSE
      GOTO 100
   ENDIF
   RETURN 1
END SUBROUTINE bislc2
