!*==cfeer1.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cfeer1
   USE c_feeraa
   USE c_feerxc
   USE c_names
   USE c_saddx
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: alpha , beta
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: i , ibuf , iprec , itypal , itypbt , itype , j , limit , nout
   INTEGER , DIMENSION(7) :: ifila , ifilb , ifilc
   REAL , DIMENSION(4) :: salpha , sbeta
   EXTERNAL close , gopen , korsz , sadd , unpack
!
! End of declarations rewritten by SPAG
!
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
   itype = iprec + 2
   nomat = 2
   DO i = 1 , 7
      ifila(i) = im(i)
      ifilb(i) = ib(i)
   ENDDO
   IF ( iprec==2 ) THEN
      alpha(1) = -lambda(1)
      alpha(2) = -lambda(2)
      beta(1) = -1.D0
      beta(2) = 0.D0
   ELSE
      salpha(1) = -sngl(lambda(1))
      salpha(2) = -sngl(lambda(2))
      salpha(3) = 0.
      salpha(4) = 0.
      sbeta(1) = -1.
      sbeta(2) = 0.
      sbeta(3) = 0.
      sbeta(4) = 0.
   ENDIF
   itypal = itype
   itypbt = itype
   nz = korsz(z)
   ifilc(1) = scr2
   ifilc(2) = ik(2)
   ifilc(3) = ik(3)
   ifilc(4) = 1
   ifilc(5) = itype
   IF ( nob ) THEN
!
!     DAMPING MATRIX ABSENT
!
      DO i = 1 , 7
         ifilb(i) = ik(i)
      ENDDO
      IF ( iprec==2 ) THEN
         alpha(1) = lambda(1)**2 - lambda(2)**2
         alpha(2) = 2.D0*lambda(1)*lambda(2)
         beta(1) = 1.D0
      ELSE
         salpha(1) = sngl(lambda(1)**2-lambda(2)**2)
         salpha(2) = 2.*sngl(lambda(1)*lambda(2))
         sbeta(1) = 1.
      ENDIF
!
!----------- LOGIC FOR SPECIAL PRINT -------------------------
!
      IF ( qpr ) THEN
         typout = itype
         irow = 1
         nlast = ik(2)
         limit = 2*nlast
         incr = 1
!-------------------------------------------------------------
!
         ibuf = nz - ksystm(1) - 2
      ENDIF
   ELSE
      CALL sadd(z,z)
!
!---------- SPECIAL PRINT ------------------------------
!
      IF ( qpr ) THEN
         WRITE (nout,99001)
99001    FORMAT (1H0,//7H CFEER1,//)
         typout = itype
         irow = 1
         nlast = ik(2)
         limit = 2*nlast
         incr = 1
         ibuf = nz - ksystm(1) - 2
         CALL gopen(ifilc(1),z(ibuf),0)
         DO i = 1 , nlast
            WRITE (nout,99003) i
            CALL unpack(*20,ifilc(1),z)
            IF ( iprec==2 ) WRITE (nout,99004) (dz(j),j=1,limit)
            IF ( iprec/=2 ) WRITE (nout,99005) (z(j),j=1,limit)
 20      ENDDO
         CALL close(ifilc(1),1)
      ENDIF
!
!
!     FORM  (LAMBDA**2*M + LAMBDA*B + K)  ON SCR1
!
      DO i = 1 , 7
         ifila(i) = ik(i)
      ENDDO
      ifilb(1) = ifilc(1)
      ifilb(2) = ik(2)
      ifilb(3) = ik(3)
      ifilb(4) = sqr
      ifilb(5) = itype
      IF ( iprec==2 ) THEN
         alpha(1) = 1.D0
         alpha(2) = 0.D0
         beta(1) = -lambda(1)
         beta(2) = -lambda(2)
      ELSE
         salpha(1) = 1.
         salpha(2) = 0.
         salpha(3) = 0.
         salpha(4) = 0.
         sbeta(1) = -sngl(lambda(1))
         sbeta(2) = -sngl(lambda(2))
         sbeta(3) = 0.
         sbeta(4) = 0.
      ENDIF
   ENDIF
   ifilc(1) = scr1
   CALL sadd(z,z)
!
!---------- SPECIAL PRINT ------------------------------
!
   IF ( qpr ) THEN
      WRITE (nout,99002)
99002 FORMAT (1H ,13(10H----------),//,19H THE DYNAMIC MATRIX,//)
      CALL gopen(ifilc(1),z(ibuf),0)
      DO i = 1 , nlast
         WRITE (nout,99003) i
         CALL unpack(*50,ifilc(1),z)
         IF ( iprec==2 ) WRITE (nout,99004) (dz(j),j=1,limit)
         IF ( iprec/=2 ) WRITE (nout,99005) (z(j),j=1,limit)
 50   ENDDO
      CALL close(ifilc(1),1)
   ENDIF
!
!-------------------------------------------------------
!     MCBLMB NOT USED WHEN DAMPING MATRIX ABSENT
!
   DO i = 1 , 7
      mcblmb(i) = ifilb(i)
   ENDDO
99003 FORMAT (7H COLUMN,I4)
99004 FORMAT (1H ,13(10H----------)/(1H ,4D25.16))
99005 FORMAT (1H ,13(10H----------)/(1H ,4E25.16))
!
END SUBROUTINE cfeer1
