!*==bisloc.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bisloc(Id,Arr,Len,Kn,Jloc) !HIDESTARS (*,Id,Arr,Len,Kn,Jloc)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Id
   INTEGER , DIMENSION(1) :: Arr
   INTEGER :: Len
   INTEGER :: Kn
   INTEGER :: Jloc
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: iswtch
   INTEGER :: j , jj , k , khi , klo
!
! End of declarations rewritten by SPAG
!
!-----
!     BINARY SEARCH - LOCATE KEY WORD 'ID' IN ARRAY 'ARR', 1ST ENTRY
!     IF FOUND, 'JLOC' IS THE MATCHED POSITION IN 'ARR'
!     IF NOT FOUND, NON-STANDARD RETURN
!                                                     I.E.
!     ID  = KEY WORD TO MATCH IN ARR.      MATCH AGAINST 1ST COL OF ARR
!     ARR = ARRAY TO SEARCH.                          ARR(ROW,COL)
!     LEN = LENGTH OF EACH ENTRY IN ARRAY.            LEN=ROW
!     KN  = NUMBER OF ENTRIES IN THE ARR.             KN =COL
!     JLOC= POINTER RETURNED - FIRST WORD OF ENTRY.   MATCHED ROW
!-----
!
   DATA iswtch/16/
!
   jj = Len - 1
   IF ( Kn<iswtch ) THEN
!
!     SEQUENTIAL SEARCH MORE EFFICIENT
!
      khi = Kn*Len - jj
      DO j = 1 , khi , Len
         IF ( Arr(j)<Id ) THEN
         ELSEIF ( Arr(j)==Id ) THEN
            CALL spag_block_1
            RETURN
         ELSE
            CALL spag_block_3
            RETURN
         ENDIF
      ENDDO
      Jloc = khi + Len
      CALL spag_block_2
      RETURN
   ELSE
      klo = 1
      khi = Kn
      k = (klo+khi+1)/2
      SPAG_Loop_1_1: DO
         j = k*Len - jj
         IF ( Id<Arr(j) ) THEN
            khi = k
         ELSEIF ( Id==Arr(j) ) THEN
            EXIT SPAG_Loop_1_1
         ELSE
            klo = k
         ENDIF
         IF ( khi-klo<1 ) THEN
            Jloc = khi*Len - jj
            j = Kn*Len - jj
            IF ( Id>Arr(j) ) Jloc = Jloc + Len
            CALL spag_block_2
            RETURN
         ELSEIF ( khi-klo==1 ) THEN
            IF ( k==klo ) THEN
               k = khi
            ELSE
               k = klo
            ENDIF
            klo = khi
         ELSE
            k = (klo+khi+1)/2
         ENDIF
      ENDDO SPAG_Loop_1_1
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      Jloc = j
      RETURN
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      RETURN 1
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
      Jloc = j
      CALL spag_block_2
      RETURN
   END SUBROUTINE spag_block_3
END SUBROUTINE bisloc
