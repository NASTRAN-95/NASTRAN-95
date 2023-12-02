!*==bishel.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bishel(List,Nent,Nterm,Array) !HIDESTARS (*,List,Nent,Nterm,Array)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: List
   INTEGER :: Nent
   INTEGER :: Nterm
   INTEGER , DIMENSION(1) :: Array
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , k , kid , l , m , n
   EXTERNAL bisloc
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!-----
!   BISHEL IS A MERGE/SORT/DUPLICATE ENTRY ELIMINATOR.  GIVEN A SORTED
! -ARRAY- AND A -LIST- TO MERGE, BISHEL ADDS THE -LIST- IN THE SORTED
! LOCATION.  SORT IS ONLY ON THE FIRST WORD OF LIST.
!
!   ARGUMENTS...
!
!     LIST  -- IN/OUT - LIST OF LENGTH NTERM TO MERGE INTO ARRAY.
!     NENT  -- IN/OUT - LENGTH OF LIST BEFORE/AFTER MERGE.
!     NTERM -- IN     - LENGTH OF ARRAY (AND LIST) ENTRIES.
!     ARRAY -- IN/OUT - ARRAY TO MERGE LIST INTO.
!     NONSTANDARD RETURN -- WHEN ARRAY(ITERM) IS A DUPLICATE.
!-----
!
         k = 1
         l = Nent + 1
         m = Nent - Nterm + 1
!
!  . LOCATE DUPLICATES...
!
         IF ( Nent<Nterm ) THEN
            Nent = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( List(1)<Array(m) ) THEN
               kid = List(1)
               CALL bisloc(*20,kid,Array,Nterm,Nent/Nterm,k)
            ELSEIF ( List(1)/=Array(m) ) THEN
               k = l
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            RETURN 1
         ENDIF
!
!  . CREATE A HOLE IN THE LIST BY MOVING THE END OF THE LIST...
!
 20      j = l - k
         n = Nent + Nterm
         DO i = 1 , j
            m = l - i
            Array(n) = Array(m)
            n = n - 1
!
!  . LOAD LIST INTO HOLE...
!
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
         DO i = 1 , Nterm
            Array(k) = List(i)
            k = k + 1
         ENDDO
         Nent = Nent + Nterm
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE bishel
