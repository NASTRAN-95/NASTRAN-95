!*==tapbit.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION tapbit(File)
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_TWO
   USE C_XFIAT
   USE C_XFIST
   USE C_XPFIST
   USE C_XXFIAT
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
   DO j = 1 , Npfist
      IF ( Fist(2*j-1)==File ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
   npf1 = Npfist + 1
   DO j = npf1 , Lfist
      IF ( Fist(2*j-1)==File ) THEN
         CALL spag_block_2
         RETURN
      ENDIF
   ENDDO
   CALL mesage(-21,File,nam)
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
      j = -Fist(2*j)
      IF ( andf(Itwo(32-j),Ib(45))/=0 ) RETURN
      IF ( andf(Xfiat(j+1),32768)==0 ) tapbit = .FALSE.
      RETURN
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
      j = Fist(2*j)
      IF ( andf(Fiat(j+1),32768)==0 ) tapbit = .FALSE.
   END SUBROUTINE spag_block_2
END FUNCTION tapbit
