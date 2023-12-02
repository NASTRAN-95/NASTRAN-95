!*==bislc2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bislc2(*,Id,Aa,Nc,Nr,Loc)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nc
   INTEGER :: Nr
   INTEGER , DIMENSION(2) :: Id
   INTEGER , DIMENSION(Nr,Nc) :: Aa
   INTEGER :: Loc
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: k , khi , klo , spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         spag_nextblock_1 = 2
      CASE (2)
         k = (klo+khi+1)/2
         DO
            IF ( Id(1)<Aa(1,k) ) THEN
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
               CYCLE
            ELSE
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            RETURN 1
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE bislc2
