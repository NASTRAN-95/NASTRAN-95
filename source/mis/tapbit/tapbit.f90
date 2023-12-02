!*==tapbit.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION tapbit(File)
   USE c_system
   USE c_two
   USE c_xfiat
   USE c_xfist
   USE c_xpfist
   USE c_xxfiat
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   LOGICAL :: tapbit
   INTEGER :: File
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: j , npf1
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL andf , mesage
!
! End of declarations rewritten by SPAG
!
!
   DATA nam/4HTAPB , 4HIT  /
!
   tapbit = .TRUE.
   DO j = 1 , npfist
      IF ( fist(2*j-1)==File ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
   npf1 = npfist + 1
   DO j = npf1 , lfist
      IF ( fist(2*j-1)==File ) THEN
         CALL spag_block_2
         RETURN
      ENDIF
   ENDDO
   CALL mesage(-21,File,nam)
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
      J = -fist(2*J)
      IF ( andf(itwo(32-J),ib(45))/=0 ) RETURN
      IF ( andf(xfiat(J+1),32768)==0 ) tapbit = .FALSE.
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
      J = fist(2*J)
      IF ( andf(fiat(J+1),32768)==0 ) tapbit = .FALSE.
   END SUBROUTINE spag_block_2
END FUNCTION tapbit
