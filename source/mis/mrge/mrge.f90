!*==mrge.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mrge(List,N,String,M)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: List
   INTEGER :: N
   INTEGER , DIMENSION(1) :: String
   INTEGER :: M
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: id , k , k1 , k2 , kk , kstart , kx , nm , spag_nextblock_1
   EXTERNAL bisloc
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
!*****
! MRGE IS A MERGE ROUTINE. GIVEN A SORTED LIST AND A SORTED STRING,
! MRGE ADDS THE ENTRIES IN THE STRING TO THE LIST IN THEIR APPROPRIATE
! POSITIONS.  DUPLICATES ARE DISCARDED.
!
!  ARGUMENTS
!
!     LIST   --- THE ARRAY CONTAINING THE SORTED LIST
!     N      --- THE NUMBER OF TERMS BEFORE AND AFTER THE MERGE
!     STRING --- THE ARRAY CONTAINING THE SORTED STRING
!     M      --- THE NUMBER OF TERMS IN THE STRING
!
!*****
!
! LOCATE THE POSITION IN THE LIST OF THE FIRST TERM IN THE STRING
!
         kk = 1
         id = String(kk)
         CALL bisloc(*20,id,List,1,N,k)
         kstart = min0(k+1,N)
         k2 = 2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      kstart = max0(1,k-1)
         k2 = 1
         spag_nextblock_1 = 2
      CASE (2)
!
! CREATE A HOLE IN THE LIST BY MOVING THE END OF THE LIST.
!
         k = N
         SPAG_Loop_1_1: DO
            List(k+M) = List(k)
            k = k - 1
            IF ( k<kstart ) THEN
               k1 = kstart + M
               nm = N + M
               k = kstart
!
! NOW ADD TO THE LIST BY MERGING FROM THE TWO STRINGS
!
               DO WHILE ( k1<=nm )
                  IF ( k2>M ) THEN
!
!    STRING EXHAUSTED -- COMPLETE LIST FROM OLD LIST
!
                     DO kx = k1 , nm
                        List(k) = List(kx)
                        k = k + 1
                     ENDDO
                     N = k - 1
                     EXIT SPAG_Loop_1_1
                  ELSE
                     IF ( List(k1)<String(k2) ) THEN
                     ELSEIF ( List(k1)==String(k2) ) THEN
!
!    DUPLICATES -- DISCARD TERM FROM STRING
!
                        k2 = k2 + 1
                     ELSE
!
!    CHOOSE TERM FROM STRING
!
                        List(k) = String(k2)
                        k2 = k2 + 1
                        k = k + 1
                        CYCLE
                     ENDIF
!
!    CHOOSE TERM FROM OLD LIST
!
                     List(k) = List(k1)
                     k1 = k1 + 1
                     k = k + 1
                  ENDIF
               ENDDO
!
!    OLD LIST EXHAUSTED -- COMPLETE LIST FROM STRING
!
               IF ( k2<=M ) THEN
                  DO kx = k2 , M
                     List(k) = String(kx)
                     k = k + 1
                  ENDDO
               ENDIF
!
! RETURN NEW NUMBER OF TERMS IN LIST.
!
               N = k - 1
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mrge
