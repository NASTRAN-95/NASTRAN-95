
SUBROUTINE bishel(*,List,Nent,Nterm,Array)
   IMPLICIT NONE
   INTEGER Nent , Nterm
   INTEGER Array(1) , List(1)
   INTEGER i , j , k , kid , l , m , n
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
      GOTO 200
   ELSE
      IF ( List(1)<Array(m) ) THEN
         kid = List(1)
         CALL bisloc(*100,kid,Array,Nterm,Nent/Nterm,k)
      ELSEIF ( List(1)/=Array(m) ) THEN
         k = l
         GOTO 200
      ENDIF
      RETURN 1
   ENDIF
!
!  . CREATE A HOLE IN THE LIST BY MOVING THE END OF THE LIST...
!
 100  j = l - k
   n = Nent + Nterm
   DO i = 1 , j
      m = l - i
      Array(n) = Array(m)
      n = n - 1
!
!  . LOAD LIST INTO HOLE...
!
   ENDDO
 200  DO i = 1 , Nterm
      Array(k) = List(i)
      k = k + 1
   ENDDO
   Nent = Nent + Nterm
END SUBROUTINE bishel
