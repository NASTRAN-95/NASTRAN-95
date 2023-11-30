
SUBROUTINE stack(Ideg,New,Ild,Iw)
   IMPLICIT NONE
   REAL Dum(5)
   INTEGER Kt , Nn
   COMMON /bandd / Dum , Kt
   COMMON /bands / Nn
   INTEGER Ideg(1) , Ild(1) , Iw(1) , New(1)
   INTEGER i , j , j1 , k , kflag , kt1 , l , nn1
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
         DO i = 1 , kt1
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
            IF ( kflag==0 ) EXIT
         ENDDO
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
