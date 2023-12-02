!*==timts2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE timts2
   IMPLICIT NONE
   USE c_blank
   USE c_system
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   COMPLEX , DIMENSION(1) :: ac , bc , cc , dc
   REAL*8 , DIMENSION(1) :: ad , bd , cd , dd
   COMPLEX :: adnc
   REAL*8 :: adnd
   REAL :: adno , asq , t1 , t2 , time , tperop
   REAL , DIMENSION(1) :: b , c , d
   INTEGER :: buf1 , buf2 , end , end2 , end4 , i , iret , itot , j , l , output , p , sysbuf
   INTEGER , DIMENSION(2) , SAVE :: isubr
   INTEGER , DIMENSION(16) , SAVE :: los , med , tig
   INTEGER , SAVE :: m8
   INTEGER , DIMENSION(4) :: name
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
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
   WRITE (output,99001) n , m , type , opt1
99001 FORMAT (1H ,20X,25HNASTRAN TIME TEST D   N =,I4,5H, M =,I4,8H, TYPE =,I4,8H, OPT1 =,I4)
   buf1 = korsz(a) - sysbuf
   buf2 = buf1 - sysbuf
   end = n*m
   IF ( end>=buf1-1 ) CALL mesage(m8,0,isubr)
!
!     CPU TIME TESTS
!
   p = 4*(type-1) + 1
   asq = m + n
   adno = 1/(asq*asq)
   adnd = adno
   adnc = cmplx(adno,adno)
   end2 = end/2
   end4 = end/4
   IF ( type==2 ) THEN
!
!     DOUBLE PRECISION TESTS
!
!
      IF ( m>end2 .OR. n>end2 ) CALL mesage(m8,0,isubr)
      DO i = 1 , end2
         ad(i) = adnd
      ENDDO
      CALL cputim(t1,t1,1)
      DO i = 1 , n
         DO j = 1 , m
            dd(j) = ad(j)*bd(j) + cd(j)
         ENDDO
      ENDDO
      CALL cputim(t2,t2,1)
      iret = 4
      name(1) = tig(p)
      name(2) = tig(p+1)
      name(3) = tig(p+2)
      name(4) = tig(p+3)
   ELSEIF ( type==3 ) THEN
!
!     COMPLEX SINGLE PRECISION TESTS
!
!
      IF ( m>end2 .OR. n>end2 ) CALL mesage(m8,0,isubr)
      DO i = 1 , end2
         ac(i) = adnc
      ENDDO
      CALL cputim(t1,t1,1)
      DO i = 1 , n
         DO j = 1 , m
            dc(j) = ac(j)*bc(j) + cc(j)
         ENDDO
      ENDDO
      CALL cputim(t2,t2,1)
      iret = 7
      name(1) = tig(p)
      name(2) = tig(p+1)
      name(3) = tig(p+2)
      name(4) = tig(p+3)
   ELSEIF ( type==4 ) THEN
!
!     DOUBLE PRECISION COMPLEX TESTS
!
!
      IF ( m>end4 .OR. n>end4 ) CALL mesage(m8,0,isubr)
      DO i = 1 , end2
         ad(i) = adnd
      ENDDO
      CALL cputim(t1,t1,1)
      DO i = 1 , n
         DO j = 1 , m
!
!     D(J) AND D(J+1) CALCULATIONS WERE REVERSED
!     IN ORDER TO COUNTERACT THE ITERATIVE BUILD UP
!
            dd(j+1) = ad(j)*bd(j) - ad(j+1)*bd(j+1) + cd(j)
            dd(j) = ad(j)*bd(j+1) + ad(j+1)*bd(j) + cd(j+1)
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
      IF ( m>end .OR. n>end ) CALL mesage(m8,0,isubr)
      DO i = 1 , end
         a(i) = adno
      ENDDO
      CALL cputim(t1,t1,1)
      DO i = 1 , n
         DO j = 1 , m
            d(j) = a(j)*b(j) + c(j)
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
      itot = m*n
      tperop = 1.0E6*time/itot
      IF ( iret==2 .OR. iret==5 .OR. iret==8 .OR. iret==11 ) WRITE (output,99002) name , itot , time , tperop
!
99002 FORMAT (1H0,4A4,' CPU TIME FOR ',I9,' OPERATIONS = ',E12.5,' SECONDS'/1X,16X,' CPU TIME FOR ','      ONE',' OPERATION  = ',   &
            & E12.5,' MICROSECONDS')
!
      IF ( iret/=2 .AND. iret/=5 .AND. iret/=8 .AND. iret/=11 ) WRITE (output,99003) name , itot , time , tperop
!
99003 FORMAT (1H0,4A4,' CPU TIME FOR ',I9,' OPERATIONS = ',E12.5,' SECONDS'/1X,16X,' CPU TIME FOR ','      ONE',' OPERATION  = ',   &
            & E12.5,' MICROSECONDS','  ---  DATA FOR USE IN COMMON /NTIME/')
!
      IF ( iret==1 ) THEN
!
         DO i = 1 , end
            a(i) = adno
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , n
            DO j = 1 , m
               d(j) = a(i)*b(j) + c(j)
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
            a(i) = adno
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , n
            DO j = 1 , m
               l = i + j - 1
               d(j) = a(i)*b(l) + c(j)
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
            ad(i) = adnd
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , n
            DO j = 1 , m
               dd(j) = ad(i)*bd(j) + cd(j)
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
            ad(i) = adnd
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , n
            DO j = 1 , m
               l = i + j - 1
               dd(j) = ad(i)*bd(l) + cd(j)
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
            ac(i) = adnc
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , n
            DO j = 1 , m
               dc(j) = ac(i)*bc(j) + cc(j)
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
            ac(i) = adnc
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , n
            DO j = 1 , m
               l = i + j - 1
               dc(j) = ac(i)*bc(l) + cc(j)
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
            ad(i) = adnd
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , n
            DO j = 1 , m
               dd(j) = ad(i)*bd(j) - ad(i+1)*bd(j+1) + cd(j)
               dd(j+1) = ad(i)*bd(j+1) + ad(i+1)*bd(j) + cd(j+1)
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
            ad(i) = adnd
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , n
            DO j = 1 , m
               l = i + j - 1
               dd(j) = ad(i)*bd(l) - ad(i+1)*bd(l+1) + cd(j)
               dd(j+1) = ad(i)*bd(l+1) + ad(i+1)*bd(l) + cd(j+1)
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
