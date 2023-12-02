!*==tmtslp.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE tmtslp
   IMPLICIT NONE
   USE C_MACHIN
   USE C_NTIME
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   COMPLEX , DIMENSION(1) :: ac , bc , cc , dc
   REAL*8 , DIMENSION(1) :: ad , bd , cd , dd
   COMPLEX :: adnc
   REAL*8 :: adnd
   REAL :: adno , asq , t1 , t2 , time , tperop
   REAL , DIMENSION(1) :: b , c , d
   INTEGER :: buf1 , buf2 , end , end2 , end4 , i , iret , itot , j , l , m , n , type
   REAL , DIMENSION(16) :: e
   INTEGER , DIMENSION(2) , SAVE :: isubr
!
! End of declarations rewritten by SPAG
!
!
!     TMTSLP TIME TESTS CPU TIMES FOR VARIOUS TYPES OF LOOPS
!
!     COMMENT FROM G.CHAN/UNISYS   5/91
!     BASICALLY THIS ROUTINE IS SAME AS TIMTS2
!
!     IF ALL TIMING CONSTANTS ARE ZEROS (OR 0.001) SYSTEM HAS A WRONG
!     CPUTIM.MDS SUBROUTINE. MOST LIKELY THE CPUTIM.MIS IS BEING USED.
!
   !>>>>EQUIVALENCE (A(1),Ac(1),Ad(1),B(1),Bc(1),Bd(1),C(1),Cc(1),Cd(1),D(1),Dc(1),Dd(1)) , (E(1),Tgino)
   DATA isubr/4HTMTS , 4HLP  /
!
!     INITIALIZE
!     DOUBLE N SIZE SINCE VAX (AND UNIX) CLOCK MAY NOT TICK FAST ENOUGH
!
   n = 50
   IF ( Mach>=5 ) n = 100
   m = n
!
   buf1 = korsz(A) - Sysbuf
   buf2 = buf1 - Sysbuf
   end = n*m
   IF ( end>=buf1-1 ) CALL mesage(-8,0,isubr)
!
!     CPU TIME TESTS
!
   asq = m + n
   adno = 1/(asq*asq)
   adnd = adno
   adnc = cmplx(adno,adno)
   end2 = end/2
   end4 = end/4
   DO type = 1 , 4
      IF ( type==2 ) THEN
!
!     DOUBLE PRECISION TESTS
!
!
         IF ( m>end2 .OR. n>end2 ) CALL mesage(-8,0,isubr)
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
         ASSIGN 200 TO iret
      ELSEIF ( type==3 ) THEN
!
!     COMPLEX SINGLE PRECISION TESTS
!
!
         IF ( m>end2 .OR. n>end2 ) CALL mesage(-8,0,isubr)
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
         ASSIGN 300 TO iret
      ELSEIF ( type==4 ) THEN
!
!     DOUBLE PRECISION COMPLEX TESTS
!
!
         IF ( m>end4 .OR. n>end4 ) CALL mesage(-8,0,isubr)
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
         ASSIGN 400 TO iret
      ELSE
!
!     REAL CPU TIME TESTS
!
!
         IF ( m>end .OR. n>end ) CALL mesage(-8,0,isubr)
         DO i = 1 , end
            A(i) = adno
         ENDDO
         CALL cputim(t1,t1,1)
         DO i = 1 , n
            DO j = 1 , m
               d(j) = A(j)*b(j) + c(j)
            ENDDO
         ENDDO
         CALL cputim(t2,t2,1)
         ASSIGN 100 TO iret
      ENDIF
!
!
!     INTERNAL ROUTINE TO STORE TIMING DATA IN /NTIME/ COMMON BLOCK
!
 50   time = t2 - t1
      itot = m*n
      tperop = 1.0E6*time/itot
      GOTO iret
 100  Ttlrsp = tperop
!
      DO i = 1 , end
         A(i) = adno
      ENDDO
      CALL cputim(t1,t1,1)
      DO i = 1 , n
         DO j = 1 , m
            l = i + j - 1
            d(j) = A(i)*b(l) + c(j)
         ENDDO
      ENDDO
      CALL cputim(t2,t2,1)
      ASSIGN 150 TO iret
      GOTO 50
 150  Tllrsp = tperop
      CYCLE
 200  Ttlrdp = tperop
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
      ASSIGN 250 TO iret
      GOTO 50
 250  Tllrdp = tperop
      CYCLE
 300  Ttlcsp = tperop
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
      ASSIGN 350 TO iret
      GOTO 50
 350  Tllcsp = tperop
      CYCLE
 400  Ttlcdp = tperop
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
      ASSIGN 450 TO iret
      GOTO 50
 450  Tllcdp = tperop
   ENDDO
!
!     MAKE SURE ALL TIME CONTSTANTS ARE OK
!
   DO i = 1 , Nitems
      IF ( Isy77==-3 .AND. e(i)<0.001 ) e(i) = 0.001
      IF ( Isy77/=-3 .AND. e(i)<1.E-7 ) e(i) = 1.E-7
   ENDDO
   IF ( Isy77==-3 ) THEN
      WRITE (Nout,99001) Nitems , Nitems , e
99001 FORMAT ('0*** NASTRAN SYSTEM MESSAGE. IF THESE',I4,' NEW TIMING',' CONSTANTS ARE HARD-CODED INTO THE LABEL COMMON /NTIME/ OF',&
            & /5X,'SUBROUTINE SEMDBD, COMPILE, AND RE-LINKE LINK 1, THE ','COMPUTATIONS OF THESE CONSTANTS IN ALL NASTRAN JOBS WILL'&
            & ,/5X,'BE ELIMINATED.',/5X,'OR TO ACCOMPLISH THE SAME RESULT, ',                                                       &
             &'EDIT THE TIM-LINE IN THE NASINFO FILE TO INCLUDE THESE',I4,' NEW',/5X,'TIMING CONSTANTS',//5X,9F8.3,/5X,7F8.3,//)
      CALL pexit
   ENDIF
   CALL sswtch(35,j)
   IF ( j/=0 ) CALL tmtsot
!
END SUBROUTINE tmtslp
