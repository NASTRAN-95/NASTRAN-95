!*==stack.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stack(Ideg,New,Ild,Iw)
   IMPLICIT NONE
   USE C_BANDD
   USE C_BANDS
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ideg
   INTEGER , DIMENSION(1) :: New
   INTEGER , DIMENSION(1) :: Ild
   INTEGER , DIMENSION(1) :: Iw
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , j1 , k , kflag , kt1 , l , nn1
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     STACK POINTS OF ZERO DEGREE AT END OF THE NUMBERING.
!     IW IS SCRATCH STORAGE.
!
!
   Kt = 0
   nn1 = Nn - 1
!
!     LIST POINTS OF ZERO DEGREE AND INCREMENT COUNTER KT.
!
   DO i = 1 , Nn
      IF ( Ideg(i)<=0 ) THEN
         Kt = Kt + 1
         Iw(Kt) = Ild(i)
      ENDIF
   ENDDO
   IF ( Kt>0 ) THEN
!
!     SORT LIST OF RENUMBERED NUMBERS TO BE STACKED.
!
      IF ( Kt>1 ) THEN
         kt1 = Kt - 1
         SPAG_Loop_1_1: DO i = 1 , kt1
            k = Kt - i
            kflag = 0
            DO j = 1 , k
               j1 = j + 1
               IF ( Iw(j)>Iw(j1) ) THEN
                  kflag = 1
                  l = Iw(j)
                  Iw(j) = Iw(j1)
                  Iw(j1) = l
               ENDIF
            ENDDO
            IF ( kflag==0 ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
      ENDIF
!
!     STACK POINTS OF ZERO DEGREE AT END OF NEW.
!
      DO l = 1 , Kt
         i = Iw(l) - l + 1
         k = New(i)
         IF ( i<Nn ) THEN
            DO j = i , nn1
               New(j) = New(j+1)
            ENDDO
         ENDIF
         New(Nn) = k
      ENDDO
   ENDIF
!
!     CORRECT ILD, THE INVERSE OF NEW.
!
   DO i = 1 , Nn
      k = New(i)
      Ild(k) = i
   ENDDO
END SUBROUTINE stack
