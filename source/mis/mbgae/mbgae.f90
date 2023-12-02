!*==mbgae.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mbgae(Ajjl,In17,A,F,Df,F1,Df1,F2,Df2,Q,Q1,Q2,Mood)
   IMPLICIT NONE
   USE C_AMGMN
   USE C_MBOXC
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ajjl
   INTEGER :: In17
   COMPLEX , DIMENSION(1) :: A
   REAL , DIMENSION(1) :: F
   REAL , DIMENSION(1) :: Df
   REAL , DIMENSION(1) :: F1
   REAL , DIMENSION(1) :: Df1
   REAL , DIMENSION(1) :: F2
   REAL , DIMENSION(1) :: Df2
   COMPLEX , DIMENSION(1) :: Q
   COMPLEX , DIMENSION(1) :: Q1
   COMPLEX , DIMENSION(1) :: Q2
   INTEGER :: Mood
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL , SAVE :: debug
   REAL :: gck
   INTEGER :: i , j , jj , kcc , kcc1 , kcc2
   EXTERNAL bckrec , fread , pack
!
! End of declarations rewritten by SPAG
!
!
!     MULTIPLY SUM OBTAINED PREVIOUSLY BY SCRIPT A FACTOR
!
   DATA debug/.FALSE./
!
   gck = Gc*Boxw
   DO i = 1 , Njj
      A(i) = (0.0,0.0)
   ENDDO
   DO i = 1 , Npts0
      CALL fread(In17,F,Kct,0)
      CALL fread(In17,Df,Kct,0)
      DO j = 1 , Kc
         A(i) = A(i) + cmplx(Df(j),-Ek*F(j))*Q(j)
      ENDDO
      IF ( Kc/=Kct ) THEN
         kcc = Kc + 1
         DO j = kcc , Kct
            A(i) = A(i) + F(j)*Q(j)
         ENDDO
      ENDIF
   ENDDO
   IF ( Cntrl1 ) THEN
      jj = Npts0
      DO i = 1 , Npts1
         CALL fread(In17,F1,Kc1t,0)
         CALL fread(In17,Df1,Kc1t,0)
         DO j = 1 , Kc1
            A(i+jj) = A(i+jj) + cmplx(Df1(j),-Ek*F1(j))*Q1(j)
         ENDDO
         IF ( Kc1/=Kc1t ) THEN
            kcc1 = Kc1 + 1
            DO j = kcc1 , Kc1t
               A(i+jj) = A(i+jj) + F1(j)*Q1(j)
            ENDDO
         ENDIF
      ENDDO
   ENDIF
   IF ( Cntrl2 ) THEN
      jj = jj + Npts1
      DO i = 1 , Npts2
         CALL fread(In17,F2,Kc2t,0)
         CALL fread(In17,Df2,Kc2t,0)
         DO j = 1 , Kc2
            A(i+jj) = A(i+jj) + cmplx(Df2(j),-Ek*F2(j))*Q2(j)
         ENDDO
         IF ( Kc2/=Kc2t ) THEN
            kcc2 = Kc2 + 1
            DO j = kcc2 , Kc2t
               A(i+jj) = A(i+jj) + F2(j)*Q2(j)
            ENDDO
         ENDIF
      ENDDO
   ENDIF
   CALL bckrec(In17)
   DO i = 1 , Njj
      A(i) = A(i)*gck
   ENDDO
   CALL pack(A,Ajjl,Mcb)
!
!     PRINT OUT GENERALIZED AERODYNAMIC FORCE COEFFICIENTS
!
   IF ( .NOT.debug ) RETURN
   IF ( Mood<=1 ) THEN
      WRITE (N6,99001) Mach , Boxl , Ek , Boxw
99001 FORMAT (1H1,31X,30HGENERALIZED AERODYNAMIC FORCE ,12HCOEFFICIENTS/1H0,9X,11HMACH NUMBER,F9.3,40X,10HBOX LENGTH,F12.6/1H0,9X,  &
             &33HREDUCED FREQUENCY  ( ROOT CHORD ),F10.5,17X,9HBOX WIDTH,F13.6/1H0,42X,21H- -  A ( I , J )  - -/6H-  ROW,9X,4HREAL, &
            & 10X,4HIMAG,14X,4HREAL,10X,4HIMAG,14X,4HREAL,10X,4HIMAG)
   ENDIF
   WRITE (N6,99002) Mood , (A(j),j=1,Njj)
99002 FORMAT (1H0,I4,3(E18.4,E14.4)/(1H0,4X,3(E18.4,E14.4)))
END SUBROUTINE mbgae
