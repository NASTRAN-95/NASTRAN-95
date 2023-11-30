
SUBROUTINE timts2
   IMPLICIT NONE
   REAL A(1) , B(1) , C(1) , D(1)
   COMPLEX Ac(1) , Bc(1) , Cc(1) , Dc(1)
   DOUBLE PRECISION Ad(1) , Bd(1) , Cd(1) , Dd(1)
   INTEGER Ksystm(65) , M , N , Opt1 , Opt2 , Output , Sysbuf , Type
   COMMON /blank / N , M , Type , Opt1 , Opt2
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ A
   COMPLEX adnc
   DOUBLE PRECISION adnd
   REAL adno , asq , t1 , t2 , time , tperop
   INTEGER buf1 , buf2 , end , end2 , end4 , i , iret , isubr(2) , itot , j , l , los(16) , m8 , med(16) , name(4) , p , tig(16)
   INTEGER korsz
!
!     TIMTS2 TIME TESTS CPU TIMES FOR VARIOUS TYPES OF LOOPS
!
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Output) , (A(1),Ac(1),Ad(1),B(1),Bc(1),Bd(1),C(1),Cc(1),Cd(1),D(1),Dc(1),Dd(1))
   DATA tig/1H  , 4HTIGH , 4HT( R , 4HSP ) , 1H  , 4HTIGH , 4HT( R , 4HDP ) , 1H  , 4HTIGH , 4HT( C , 4HSP ) , 1H  , 4HTIGH ,       &
      & 4HT( C , 4HDP )/
   DATA med/1H  , 4HMEDI , 4HUM(R , 4HSP ) , 1H  , 4HMEDI , 4HUM(R , 4HDP ) , 1H  , 4HMEDI , 4HUM(C , 4HSP ) , 1H  , 4HMEDI ,       &
      & 4HUM(C , 4HDP )/
   DATA los/1H  , 4HLOOS , 4HE (R , 4HSP ) , 1H  , 4HLOOS , 4HE (R , 4HDP ) , 1H  , 4HLOOS , 4HE (C , 4HSP ) , 1H  , 4HLOOS ,       &
      & 4HE (C , 4HDP )/
   DATA isubr/4HTIMT , 4HS2  / , m8/ - 8/
!
!     INITIALIZE
!
   CALL page1
   WRITE (Output,99001) N , M , Type , Opt1
99001 FORMAT (1H ,20X,25HNASTRAN TIME TEST D   N =,I4,5H, M =,I4,8H, TYPE =,I4,8H, OPT1 =,I4)
   buf1 = korsz(A) - Sysbuf
   buf2 = buf1 - Sysbuf
   end = N*M
   IF ( end>=buf1-1 ) CALL mesage(m8,0,isubr)
!
!     CPU TIME TESTS
!
   p = 4*(Type-1) + 1
   asq = M + N
   adno = 1/(asq*asq)
   adnd = adno
   adnc = cmplx(adno,adno)
   end2 = end/2
   end4 = end/4
   IF ( Type==2 ) THEN
!
!     DOUBLE PRECISION TESTS
!
!
      IF ( M>end2 .OR. N>end2 ) CALL mesage(m8,0,isubr)
      DO i = 1 , end2
         Ad(i) = adnd
      ENDDO
      CALL cputim(t1,t1,1)
      DO i = 1 , N
         DO j = 1 , M
            Dd(j) = Ad(j)*Bd(j) + Cd(j)
         ENDDO
      ENDDO
      CALL cputim(t2,t2,1)
      iret = 4
      name(1) = tig(p)
      name(2) = tig(p+1)
      name(3) = tig(p+2)
      name(4) = tig(p+3)
   ELSEIF ( Type==3 ) THEN
!
!     COMPLEX SINGLE PRECISION TESTS
!
!
      IF ( M>end2 .OR. N>end2 ) CALL mesage(m8,0,isubr)
      DO i = 1 , end2
         Ac(i) = adnc
      ENDDO
      CALL cputim(t1,t1,1)
      DO i = 1 , N
         DO j = 1 , M
            Dc(j) = Ac(j)*Bc(j) + Cc(j)
         ENDDO
      ENDDO
      CALL cputim(t2,t2,1)
      iret = 7
      name(1) = tig(p)
      name(2) = tig(p+1)
      name(3) = tig(p+2)
      name(4) = tig(p+3)
   ELSEIF ( Type==4 ) THEN
!
!     DOUBLE PRECISION COMPLEX TESTS
!
!
      IF ( M>end4 .OR. N>end4 ) CALL mesage(m8,0,isubr)
      DO i = 1 , end2
         Ad(i) = adnd
      ENDDO
      CALL cputim(t1,t1,1)
      DO i = 1 , N
         DO j = 1 , M
!
!     D(J) AND D(J+1) CALCULATIONS WERE REVERSED
!     IN ORDER TO COUNTERACT THE ITERATIVE BUILD UP
!
            Dd(j+1) = Ad(j)*Bd(j) - Ad(j+1)*Bd(j+1) + Cd(j)
            Dd(j) = Ad(j)*Bd(j+1) + Ad(j+1)*Bd(j) + Cd(j+1)
         ENDDO
      ENDDO
      CALL cputim(t2,t2,1)
      iret = 10
      name(1) = tig(p)
      name(2) = tig(p+1)
      name(3) = tig(p+2)
      name(4) = tig(p+3)
   ELSE
!
!     REAL CPU TIME TESTS
!
!
      IF ( M>end .OR. N>end ) CALL mesage(m8,0,isubr)
      DO i = 1 , end
         A(i) = adno
      ENDDO
      CALL cputim(t1,t1,1)
      DO i = 1 , N
         DO j = 1 , M
            D(j) = A(j)*B(j) + C(j)
         ENDDO
      ENDDO
      CALL cputim(t2,t2,1)
      iret = 1
      name(1) = tig(p)
      name(2) = tig(p+1)
      name(3) = tig(p+2)
      name(4) = tig(p+3)
   ENDIF
   DO
!
!
!     INTERNAL ROUTINE TO WRITE OUTPUT ONTO THE OUTPUT FILE
!
      time = t2 - t1
      itot = M*N
      tperop = 1.0E6*time/itot
      IF ( iret==2 .OR. iret==5 .OR. iret==8 .OR. iret==11 ) WRITE (Output,99002) name , itot , time , tperop
!
99002 FORMAT (1H0,4A4,' CPU TIME FOR ',I9,' OPERATIONS = ',E12.5,' SECONDS'/1X,16X,' CPU TIME FOR ','      ONE',' OPERATION  = ',   &
            & E12.5,' MICROSECONDS')
!
      IF ( iret/=2 .AND. iret/=5 .AND. iret/=8 .AND. iret/=11 ) WRITE (Output,99003) name , itot , time , tperop
!
99003 FORMAT (1H0,4A4,' CPU TIME FOR ',I9,' OPERATIONS = ',E12.5,' SECONDS'/1X,16X,' CPU TIME FOR ','      ONE',' OPERATION  = ',   &
            & E12.5,' MICROSECONDS','  ---  DATA FOR USE IN COMMON /NTIME/')
!
      IF ( iret==1 ) THEN
!
         DO i = 1 , end
            A(i) = adno
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , N
            DO j = 1 , M
               D(j) = A(i)*B(j) + C(j)
            ENDDO
         ENDDO
         CALL cputim(t2,t2,1)
         iret = 2
         name(1) = med(p)
         name(2) = med(p+1)
         name(3) = med(p+2)
         name(4) = med(p+3)
      ELSEIF ( iret==2 ) THEN
!
         DO i = 1 , end
            A(i) = adno
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , N
            DO j = 1 , M
               l = i + j - 1
               D(j) = A(i)*B(l) + C(j)
            ENDDO
         ENDDO
         CALL cputim(t2,t2,1)
         iret = 3
         name(1) = los(p)
         name(2) = los(p+1)
         name(3) = los(p+2)
         name(4) = los(p+3)
      ELSEIF ( iret==3 .OR. iret==6 .OR. iret==9 .OR. iret==12 ) THEN
         RETURN
      ELSEIF ( iret==4 ) THEN
!
         DO i = 1 , end2
            Ad(i) = adnd
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , N
            DO j = 1 , M
               Dd(j) = Ad(i)*Bd(j) + Cd(j)
            ENDDO
         ENDDO
         CALL cputim(t2,t2,1)
         iret = 5
         name(1) = med(p)
         name(2) = med(p+1)
         name(3) = med(p+2)
         name(4) = med(p+3)
      ELSEIF ( iret==5 ) THEN
!
         DO i = 1 , end2
            Ad(i) = adnd
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , N
            DO j = 1 , M
               l = i + j - 1
               Dd(j) = Ad(i)*Bd(l) + Cd(j)
            ENDDO
         ENDDO
         CALL cputim(t2,t2,1)
         iret = 6
         name(1) = los(p)
         name(2) = los(p+1)
         name(3) = los(p+2)
         name(4) = los(p+3)
      ELSEIF ( iret==7 ) THEN
!
         DO i = 1 , end2
            Ac(i) = adnc
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , N
            DO j = 1 , M
               Dc(j) = Ac(i)*Bc(j) + Cc(j)
            ENDDO
         ENDDO
         CALL cputim(t2,t2,1)
         iret = 8
         name(1) = med(p)
         name(2) = med(p+1)
         name(3) = med(p+2)
         name(4) = med(p+3)
      ELSEIF ( iret==8 ) THEN
!
         DO i = 1 , end2
            Ac(i) = adnc
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , N
            DO j = 1 , M
               l = i + j - 1
               Dc(j) = Ac(i)*Bc(l) + Cc(j)
            ENDDO
         ENDDO
         CALL cputim(t2,t2,1)
         iret = 9
         name(1) = los(p)
         name(2) = los(p+1)
         name(3) = los(p+2)
         name(4) = los(p+3)
      ELSEIF ( iret==10 ) THEN
!
         DO i = 1 , end2
            Ad(i) = adnd
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , N
            DO j = 1 , M
               Dd(j) = Ad(i)*Bd(j) - Ad(i+1)*Bd(j+1) + Cd(j)
               Dd(j+1) = Ad(i)*Bd(j+1) + Ad(i+1)*Bd(j) + Cd(j+1)
            ENDDO
         ENDDO
         CALL cputim(t2,t2,1)
         iret = 11
         name(1) = med(p)
         name(2) = med(p+1)
         name(3) = med(p+2)
         name(4) = med(p+3)
      ELSEIF ( iret==11 ) THEN
!
         DO i = 1 , end2
            Ad(i) = adnd
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , N
            DO j = 1 , M
               l = i + j - 1
               Dd(j) = Ad(i)*Bd(l) - Ad(i+1)*Bd(l+1) + Cd(j)
               Dd(j+1) = Ad(i)*Bd(l+1) + Ad(i+1)*Bd(l) + Cd(j+1)
            ENDDO
         ENDDO
         CALL cputim(t2,t2,1)
         iret = 12
         name(1) = los(p)
         name(2) = los(p+1)
         name(3) = los(p+2)
         name(4) = los(p+3)
      ELSE
         EXIT
      ENDIF
   ENDDO
END SUBROUTINE timts2