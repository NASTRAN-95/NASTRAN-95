!*==skprec.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE skprec(Ifile,K)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ifile
   INTEGER :: K
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , m , spag_nextblock_1
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL bckrec , fwdrec , mesage
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
!
!
   DATA name/4HSKPR , 2HEC/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! ----------------------------------------------------------------------
!
         IF ( K<0 ) THEN
!
            m = iabs(K)
            DO i = 1 , m
               CALL bckrec(Ifile)
            ENDDO
         ELSEIF ( K/=0 ) THEN
!
            DO i = 1 , K
               CALL fwdrec(*20,Ifile)
            ENDDO
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
         RETURN
!
 20      CALL mesage(-2,Ifile,name)
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE skprec
