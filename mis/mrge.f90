
SUBROUTINE mrge(List,N,String,M)
   IMPLICIT NONE
   INTEGER M , N
   INTEGER List(1) , String(1)
   INTEGER id , k , k1 , k2 , kk , kstart , kx , nm
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
   CALL bisloc(*100,id,List,1,N,k)
   kstart = min0(k+1,N)
   k2 = 2
   GOTO 200
 100  kstart = max0(1,k-1)
   k2 = 1
!
! CREATE A HOLE IN THE LIST BY MOVING THE END OF THE LIST.
!
 200  k = N
   DO
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
               GOTO 99999
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
         EXIT
      ENDIF
   ENDDO
99999 RETURN
END SUBROUTINE mrge
