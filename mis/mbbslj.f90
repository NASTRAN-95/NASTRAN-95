
SUBROUTINE mbbslj(Arg,N,Bsl)
   IMPLICIT NONE
   INTEGER N6
   CHARACTER*25 Sfm , Uwm
   REAL Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /system/ Sysbuf , N6
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   REAL Arg
   INTEGER N
   REAL Bsl(4)
   REAL asq , f , pf
   INTEGER i , j , m
!
!     SUBROUTINE TO COMPUTE EVEN ORDERED BESSEL FUNCTIONS OF FIRST KIND
!
!     UNDERFLOW MAY OCCUR IN THIS ROUTINE. THE RESULTS ARE NOT AFFECTED
!
!
   DO i = 1 , 20
      Bsl(i) = 0.0
   ENDDO
   asq = Arg**2
   IF ( asq<0.01 ) THEN
!
      Bsl(2) = 0.125*asq
      Bsl(1) = 1.0 - 2.0*Bsl(2)
      N = 2
      GOTO 99999
   ELSE
      N = amin1(17.0,(Arg+10.0))
      f = 2*N + 4
      Bsl(N+3) = 0.0
      pf = (4.0*f*(f-1.0)/asq-(f-1.0)/f)*0.3
      IF ( pf<=1.E-08 ) THEN
!
         CALL page2(3)
         WRITE (N6,99001) Sfm , Arg
99001    FORMAT (A25,' 2435, MBBSLJ SUBROUTINE FAILED BECAUSE THE ARGUMEN','T IS TOO LARGE FOR THE BSL ARRAY',/5X,'ARG =',1P,E13.5)
         CALL mesage(-61,0,0)
         GOTO 99999
      ELSE
         Bsl(N+2) = pf*1.E-30
         pf = 0.0
         j = N + 1
         DO i = 1 , j
            m = N - i + 2
            f = 2*m + 1
            Bsl(m) = ((4.*(f-1.)/asq-1./f-1./(f-2.))*Bsl(m+1)-Bsl(m+2)/f)*(f-2.0)
            pf = pf + 2.0*Bsl(m+1)
         ENDDO
         pf = pf + Bsl(1)
         f = 0.0
         IF ( abs(pf)>1.0 ) f = abs(pf)*1.E-10
      ENDIF
   ENDIF
   N = N + 2
   DO i = 1 , N
      IF ( f>=abs(Bsl(i)) ) Bsl(i) = 0.0
      Bsl(i) = Bsl(i)/pf
   ENDDO
   m = N
   DO i = 1 , m
      IF ( abs(Bsl(N))>1.0E-07 ) RETURN
      N = N - 1
   ENDDO
   RETURN
99999 RETURN
END SUBROUTINE mbbslj