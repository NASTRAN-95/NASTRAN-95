!*==eqscod.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE eqscod(Loc,N,Z)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Loc
   INTEGER :: N
   INTEGER , DIMENSION(1) :: Z
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , icode , iloc , inew , ist , j , mend , ng
   EXTERNAL lshift , orf
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         i = Loc
         mend = Loc + N - 1
         spag_nextblock_1 = 2
      CASE (2)
         ist = i
         ng = 1
         SPAG_Loop_1_1: DO WHILE ( i<mend-2 )
            IF ( Z(i+3)/=Z(ist) ) EXIT SPAG_Loop_1_1
            ng = ng + 1
            i = i + 3
         ENDDO SPAG_Loop_1_1
         IF ( ng/=1 ) THEN
            DO j = 1 , ng
               iloc = ist + 3*(j-1)
               icode = 8*j + ng
               inew = lshift(icode,26)
               Z(iloc+2) = orf(Z(iloc+2),inew)
            ENDDO
            i = i + 3
            IF ( i<mend-2 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            i = i + 3
            IF ( i<mend-2 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE eqscod
