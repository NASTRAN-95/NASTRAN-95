!*==mbgae.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mbgae(Ajjl,In17,A,F,Df,F1,Df1,F2,Df2,Q,Q1,Q2,Mood)
   USE c_amgmn
   USE c_mboxc
   USE c_system
   IMPLICIT NONE
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
   gck = gc*boxw
   DO i = 1 , njj
      A(i) = (0.0,0.0)
   ENDDO
   DO i = 1 , npts0
      CALL fread(In17,F,kct,0)
      CALL fread(In17,Df,kct,0)
      DO j = 1 , kc
         A(i) = A(i) + cmplx(Df(j),-ek*F(j))*Q(j)
      ENDDO
      IF ( kc/=kct ) THEN
         kcc = kc + 1
         DO j = kcc , kct
            A(i) = A(i) + F(j)*Q(j)
         ENDDO
      ENDIF
   ENDDO
   IF ( cntrl1 ) THEN
      jj = npts0
      DO i = 1 , npts1
         CALL fread(In17,F1,kc1t,0)
         CALL fread(In17,Df1,kc1t,0)
         DO j = 1 , kc1
            A(i+jj) = A(i+jj) + cmplx(Df1(j),-ek*F1(j))*Q1(j)
         ENDDO
         IF ( kc1/=kc1t ) THEN
            kcc1 = kc1 + 1
            DO j = kcc1 , kc1t
               A(i+jj) = A(i+jj) + F1(j)*Q1(j)
            ENDDO
         ENDIF
      ENDDO
   ENDIF
   IF ( cntrl2 ) THEN
      jj = jj + npts1
      DO i = 1 , npts2
         CALL fread(In17,F2,kc2t,0)
         CALL fread(In17,Df2,kc2t,0)
         DO j = 1 , kc2
            A(i+jj) = A(i+jj) + cmplx(Df2(j),-ek*F2(j))*Q2(j)
         ENDDO
         IF ( kc2/=kc2t ) THEN
            kcc2 = kc2 + 1
            DO j = kcc2 , kc2t
               A(i+jj) = A(i+jj) + F2(j)*Q2(j)
            ENDDO
         ENDIF
      ENDDO
   ENDIF
   CALL bckrec(In17)
   DO i = 1 , njj
      A(i) = A(i)*gck
   ENDDO
   CALL pack(A,Ajjl,mcb)
!
!     PRINT OUT GENERALIZED AERODYNAMIC FORCE COEFFICIENTS
!
   IF ( .NOT.debug ) RETURN
   IF ( Mood<=1 ) THEN
      WRITE (n6,99001) mach , boxl , ek , boxw
99001 FORMAT (1H1,31X,30HGENERALIZED AERODYNAMIC FORCE ,12HCOEFFICIENTS/1H0,9X,11HMACH NUMBER,F9.3,40X,10HBOX LENGTH,F12.6/1H0,9X,  &
             &33HREDUCED FREQUENCY  ( ROOT CHORD ),F10.5,17X,9HBOX WIDTH,F13.6/1H0,42X,21H- -  A ( I , J )  - -/6H-  ROW,9X,4HREAL, &
            & 10X,4HIMAG,14X,4HREAL,10X,4HIMAG,14X,4HREAL,10X,4HIMAG)
   ENDIF
   WRITE (n6,99002) Mood , (A(j),j=1,njj)
99002 FORMAT (1H0,I4,3(E18.4,E14.4)/(1H0,4X,3(E18.4,E14.4)))
END SUBROUTINE mbgae
