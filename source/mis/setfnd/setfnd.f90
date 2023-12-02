!*==setfnd.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE setfnd(Set,Lset,Id,Next) !HIDESTARS (*,Set,Lset,Id,Next)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Lset
   INTEGER , DIMENSION(Lset) :: Set
   INTEGER :: Id
   INTEGER :: Next
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: id1
!
! End of declarations rewritten by SPAG
!
   SPAG_Loop_1_2: DO
!*****
!  FINDS AN ID IN A SORTED SET LIST WHICH MAY HAVE THE NASTRAN THROUGH
!  NOTATION.  IE.  6,-18 IMPLIES 6 THRU 18.
!
!  -SET- IS THE LIST OF IDS.
!  -LSET- IS THE LENGTH OF THE LIST IN -SET-.
!  -ID- IS THE ID BEING LOOKED FOR IN THE LIST -SET-.
!  -NEXT- IS A RELATIVE INDEX INTO THE LIST -SET-.  IT SHOULD BE SET
!  TO 1 ON THE FIRST CALL TO THIS ROUTINE FOR A GIVEN LIST AND THEN
!  RETURNED ON FUTURE CALLS.
!
!  THIS ROUTINE MOVES FORWARD ONLY UNTIL AN ID IN THE LIST IS GREATER
!  THAN THE ID BEING ASKED FOR.    -NEXT-  IF NOT RESET TO 1 WILL ALLOW
!  THE ROUTINE TO SEARCH ONLY FROM WHERE LAST SEARCH LEFT OFF.
!
!  THE NON-STANDARD RETURN IS TAKEN IN THE EVENT THE ID IS NOT IN THE
!  LIST.  A NORMAL RETURN IS TAKEN IF THE ID IS IN THE LIST.
!
!  -NEXT- SHOULD BE SET TO 1 ON THE FIRST CALL TO THIS ROUTINE OR WHEN
!  AN ID TO BE LOOKED FOR IS SMALLER THAN AN ID PREVIOUSLY LOOKED FOR.
!
!  IF IDS TO BE LOOKED FOR ARE NOT IN SORT AND THE SET LIST IS IN SORT
!  WITHOUT THE NASTRAN THROUGH NOTATION, THEN THE NASTRAN BINARY SEARCH
!  ROUTINE -BISRCH- SHOULD BE USED.
!
!  IT IS OK TO CALL THIS ROUTINE WITH MORE THAN ONE OF THE SAME IDS
!  WITHOUT RESETTING -NEXT-.
!*****
!
      id1 = Set(Next)
      SPAG_Loop_2_1: DO
         IF ( Next<Lset ) THEN
!
!     STILL POSITIONED WITHIN THE SET LIST.
!
            IF ( Id<id1 ) EXIT SPAG_Loop_2_1
            IF ( Id==id1 ) RETURN
!
!     CHECK FOR THRU CASE
!
            id1 = Set(Next+1)
            IF ( id1<0 ) THEN
!
!     YES POSITIONED IN A THRU CASE
!
               IF ( Id+id1<=0 ) RETURN
!
!     ID BEING LOOKED FOR IS BEYOND THIS THRU CASE.
!
               Next = Next + 2
               CYCLE SPAG_Loop_1_2
            ELSE
!
!     NOT IN A THRU CASE
!
               Next = Next + 1
            ENDIF
         ELSEIF ( Next==Lset ) THEN
!
!     AT THE LAST ID IN THE LIST
!
            IF ( Id<id1 ) THEN
            ELSEIF ( Id==id1 ) THEN
               RETURN
            ELSE
               Next = Next + 1
            ENDIF
            EXIT SPAG_Loop_2_1
         ELSE
            EXIT SPAG_Loop_2_1
         ENDIF
      ENDDO SPAG_Loop_2_1
      EXIT SPAG_Loop_1_2
   ENDDO SPAG_Loop_1_2
   RETURN 1
END SUBROUTINE setfnd
