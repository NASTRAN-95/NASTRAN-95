
SUBROUTINE cfeer1
   IMPLICIT NONE
   DOUBLE PRECISION Alpha(2) , Beta(2) , Dz(1) , Lambda(2)
   REAL Cdp , Dum(15) , Dumaa(91) , Dumm(10) , Dumxc(12) , Dumxc2(4) , Salpha(4) , Sbeta(4) , Scr(8) , Z(1)
   INTEGER Ib(7) , Ifila(7) , Ifilb(7) , Ifilc(7) , Ik(7) , Im(7) , Incr , Iprec , Irow , Itypal , Itypbt , Ksystm(65) , Mcblmb(7) ,&
         & Mcbs(67) , Nlast , Nomat , Nout , Nz , Scr1 , Scr11 , Scr2 , Sqr , Typout
   LOGICAL Nob , Qpr
   COMMON /feeraa/ Ik , Im , Ib , Dum , Scr1 , Scr2 , Scr , Scr11 , Dumaa , Mcblmb
   COMMON /feerxc/ Lambda , Dumxc , Nob , Dumxc2 , Qpr
   COMMON /names / Dumm , Cdp , Sqr
   COMMON /saddx / Nomat , Nz , Mcbs
   COMMON /system/ Ksystm
   COMMON /unpakx/ Typout , Irow , Nlast , Incr
   COMMON /zzzzzz/ Z
   INTEGER i , ibuf , itype , j , limit
   INTEGER korsz
!
!     CFEER1 INITIALIZES AND CALLS SUBROUTINE SADD FOR CFCNTL
!
!
   !>>>>EQUIVALENCE (Mcbs(1),Ifila(1)) , (Mcbs(8),Itypal) , (Mcbs(61),Ifilc(1)) , (Mcbs(13),Ifilb(1)) , (Mcbs(20),Itypbt) ,              &
!>>>>    & (Mcbs(21),Beta(1)) , (Mcbs(9),Alpha(1)) , (Iprec,Ksystm(55)) , (Alpha(1),Salpha(1)) , (Beta(1),Sbeta(1)) , (Z(1),Dz(1)) ,     &
!>>>>    & (Nout,Ksystm(2))
!
!     FORM   -(B + LAMBDA*M)  ON SCR2
!
   itype = Iprec + 2
   Nomat = 2
   DO i = 1 , 7
      Ifila(i) = Im(i)
      Ifilb(i) = Ib(i)
   ENDDO
   IF ( Iprec==2 ) THEN
      Alpha(1) = -Lambda(1)
      Alpha(2) = -Lambda(2)
      Beta(1) = -1.D0
      Beta(2) = 0.D0
   ELSE
      Salpha(1) = -sngl(Lambda(1))
      Salpha(2) = -sngl(Lambda(2))
      Salpha(3) = 0.
      Salpha(4) = 0.
      Sbeta(1) = -1.
      Sbeta(2) = 0.
      Sbeta(3) = 0.
      Sbeta(4) = 0.
   ENDIF
   Itypal = itype
   Itypbt = itype
   Nz = korsz(Z)
   Ifilc(1) = Scr2
   Ifilc(2) = Ik(2)
   Ifilc(3) = Ik(3)
   Ifilc(4) = 1
   Ifilc(5) = itype
   IF ( Nob ) THEN
!
!     DAMPING MATRIX ABSENT
!
      DO i = 1 , 7
         Ifilb(i) = Ik(i)
      ENDDO
      IF ( Iprec==2 ) THEN
         Alpha(1) = Lambda(1)**2 - Lambda(2)**2
         Alpha(2) = 2.D0*Lambda(1)*Lambda(2)
         Beta(1) = 1.D0
      ELSE
         Salpha(1) = sngl(Lambda(1)**2-Lambda(2)**2)
         Salpha(2) = 2.*sngl(Lambda(1)*Lambda(2))
         Sbeta(1) = 1.
      ENDIF
!
!----------- LOGIC FOR SPECIAL PRINT -------------------------
!
      IF ( Qpr ) THEN
         Typout = itype
         Irow = 1
         Nlast = Ik(2)
         limit = 2*Nlast
         Incr = 1
!-------------------------------------------------------------
!
         ibuf = Nz - Ksystm(1) - 2
      ENDIF
   ELSE
      CALL sadd(Z,Z)
!
!---------- SPECIAL PRINT ------------------------------
!
      IF ( Qpr ) THEN
         WRITE (Nout,99001)
99001    FORMAT (1H0,//7H CFEER1,//)
         Typout = itype
         Irow = 1
         Nlast = Ik(2)
         limit = 2*Nlast
         Incr = 1
         ibuf = Nz - Ksystm(1) - 2
         CALL gopen(Ifilc(1),Z(ibuf),0)
         DO i = 1 , Nlast
            WRITE (Nout,99003) i
            CALL unpack(*20,Ifilc(1),Z)
            IF ( Iprec==2 ) WRITE (Nout,99004) (Dz(j),j=1,limit)
            IF ( Iprec/=2 ) WRITE (Nout,99005) (Z(j),j=1,limit)
 20      ENDDO
         CALL close(Ifilc(1),1)
      ENDIF
!
!
!     FORM  (LAMBDA**2*M + LAMBDA*B + K)  ON SCR1
!
      DO i = 1 , 7
         Ifila(i) = Ik(i)
      ENDDO
      Ifilb(1) = Ifilc(1)
      Ifilb(2) = Ik(2)
      Ifilb(3) = Ik(3)
      Ifilb(4) = Sqr
      Ifilb(5) = itype
      IF ( Iprec==2 ) THEN
         Alpha(1) = 1.D0
         Alpha(2) = 0.D0
         Beta(1) = -Lambda(1)
         Beta(2) = -Lambda(2)
      ELSE
         Salpha(1) = 1.
         Salpha(2) = 0.
         Salpha(3) = 0.
         Salpha(4) = 0.
         Sbeta(1) = -sngl(Lambda(1))
         Sbeta(2) = -sngl(Lambda(2))
         Sbeta(3) = 0.
         Sbeta(4) = 0.
      ENDIF
   ENDIF
   Ifilc(1) = Scr1
   CALL sadd(Z,Z)
!
!---------- SPECIAL PRINT ------------------------------
!
   IF ( Qpr ) THEN
      WRITE (Nout,99002)
99002 FORMAT (1H ,13(10H----------),//,19H THE DYNAMIC MATRIX,//)
      CALL gopen(Ifilc(1),Z(ibuf),0)
      DO i = 1 , Nlast
         WRITE (Nout,99003) i
         CALL unpack(*50,Ifilc(1),Z)
         IF ( Iprec==2 ) WRITE (Nout,99004) (Dz(j),j=1,limit)
         IF ( Iprec/=2 ) WRITE (Nout,99005) (Z(j),j=1,limit)
 50   ENDDO
      CALL close(Ifilc(1),1)
   ENDIF
!
!-------------------------------------------------------
!     MCBLMB NOT USED WHEN DAMPING MATRIX ABSENT
!
   DO i = 1 , 7
      Mcblmb(i) = Ifilb(i)
   ENDDO
99003 FORMAT (7H COLUMN,I4)
99004 FORMAT (1H ,13(10H----------)/(1H ,4D25.16))
99005 FORMAT (1H ,13(10H----------)/(1H ,4E25.16))
!
END SUBROUTINE cfeer1