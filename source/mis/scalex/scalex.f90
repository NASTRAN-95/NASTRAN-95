!*==scalex.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE scalex(Ilval,Code,L)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ilval
   INTEGER :: Code
   INTEGER , DIMENSION(1) :: L
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(6) :: expnd
   INTEGER :: i , id , ii , inv , j , k
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
   DO i = 1 , 6
      L(i) = 0
   ENDDO
   IF ( Code<=0 ) THEN
      L(1) = Ilval
   ELSE
      id = Code
      DO i = 1 , 6
         inv = 7 - i
         expnd(inv) = mod(id,10)
         id = id/10
      ENDDO
      j = 0
      SPAG_Loop_1_1: DO i = 1 , 6
         IF ( expnd(i)/=0 ) THEN
            IF ( i>=2 ) THEN
               ii = i - 1
               DO k = 1 , ii
                  IF ( expnd(k)==expnd(i) ) CYCLE SPAG_Loop_1_1
               ENDDO
            ENDIF
            j = j + 1
            L(j) = expnd(i)
         ENDIF
      ENDDO SPAG_Loop_1_1
      i = 0
      SPAG_Loop_1_2: DO
         i = i + 1
         L(i) = Ilval + L(i) - 1
         IF ( i>=j ) EXIT SPAG_Loop_1_2
      ENDDO SPAG_Loop_1_2
   ENDIF
END SUBROUTINE scalex
