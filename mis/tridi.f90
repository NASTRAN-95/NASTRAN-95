
SUBROUTINE tridi(D,O,C,A,B,Aa)
   IMPLICIT NONE
   INTEGER Entry , Idummy(52) , Ii , Iii , Incr , Incr1 , Iprec , It1 , It2 , It3 , Jj , Jjj , M1 , M2 , M3 , M4 , Md , Mo , Mr1 ,  &
         & N , Nout , Row , Rstrt , Savemr , Sysbuf , Xentry
   REAL T10 , T12(5) , T19 , Title(1) , Vvcom(150)
   CHARACTER*23 Ufm
   COMMON /givn  / Title , Mo , Md , Mr1 , M1 , M2 , M3 , M4 , Savemr , T10 , Entry , T12 , Rstrt , Row , T19 , Xentry
   COMMON /packx / It1 , It2 , Ii , Jj , Incr
   COMMON /system/ Sysbuf , Nout , Idummy , Iprec
   COMMON /unpakx/ It3 , Iii , Jjj , Incr1
   COMMON /xmssg / Ufm
   REAL A(2)
   DOUBLE PRECISION Aa(1) , B(1) , C(1) , D(1) , O(1)
   INTEGER count , i , ibuf1 , ibuf2 , im1 , j , k , max , mcb(7) , midin , midout , mr , ms , ms1 , ms2 , ms3 , ms4 , mss , na ,   &
         & nm1 , nm2 , nz , nzsq , nzz , rot , row1 , row2 , rowp1 , rowp2
   INTEGER filcor , korsz
!
!     MODIFIED GIVENS REAL SYMMETRIC TRIDIAGONALIZATION
!     THIS ROUTINE IS CALLED ONLY BY VALVEC
!
   !>>>>EQUIVALENCE (Vvcom(1),Title(1)) , (N,Vvcom(101))
   DATA count , max , mcb/0 , 10 , 7*0/
!
!
!     DEFINITION OF VARIABLES
!
!     D       = LOCATION OF DIAGIONAL
!     O       = LOCATION OF OFF DIAGONAL
!     C       = LOCATION OF COSINES
!     A       = REST OF OPEN CORE
!     B       = O**2
!     SAVEMR
!     RSTRT
!     ROW
!     XENTRY
!     FILCOR
!     ROT
!     ROW1
!     ROW2
!     MO      = RESTART DATA - SINES AND COSINES
!     MD      = INPUT  MATRIX
!     MR1     = RESTART TAPE
!     M1      = SCRATCH TAPE
!     M2
!     M3
!     M4
!     MR2
!     MIDIN
!     COUNT   = NUMBER OF ROWS ROTATED
!     MAX     = NUMBER OF ROWS TO ROTATE BEFORE CHECKPOINTING
!
!
!     INITIALIZATION
!
   nz = korsz(A)
   ibuf1 = nz - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   nz = nz - 2*Sysbuf
   nzz = nz/Iprec
   nzsq = sqrt(float((nzz-1)*2))
   im1 = 1
   nm1 = N - 1
   nm2 = N - 2
   M3 = 305
   mss = Mr1
   ms1 = M1
   ms2 = M2
   ms3 = M3
   ms4 = M4
!
!     INITIALIZE  TRANSFORMATION ROUTINES
!
!     SICOX AND ROTAX ARE NOT USED ANY MORE. SEE SINC0S AND ROTATE
!
!     CALL SICOX (D,O,C)
!     CALL ROTAX (O,D,C)
!
   midin = N
   mr = Mr1
!
!     START AT THE BEGINNING
!
   Row = 0
!
!     OPEN MD
!
   CALL gopen(Md,A(ibuf1),0)
   CALL gopen(mr,A(ibuf2),1)
!
!     SET UP FOR UNPACK
!
   It3 = 2
   Iii = 1
   Jjj = N
   Incr1 = 1
   CALL unpack(*300,Md,D)
!
!     COPY REST OF MD ONTO MR
!
 100  It1 = 2
   It2 = 2
   Incr = 1
   k = N - 1
   DO i = 1 , k
      Iii = 0
      CALL unpack(*200,Md,A)
      Ii = Iii
      Jj = Jjj
 150  CALL pack(A,mr,mcb)
      CYCLE
 200  Ii = 1
      Jj = 1
      A(1) = 0.0
      A(2) = 0.0
      GOTO 150
   ENDDO
   Iii = 1
   Jjj = N
   Ii = 1
   Jj = N
!
!     END OF MATRIX MD
!
   CALL write(mr,Row,1,1)
!
!     ATTACH DIAGONALS
!
   CALL pack(D,mr,mcb)
   CALL close(Md,1)
   CALL close(mr,1)
   ms = mr
   CALL gopen(ms,A(ibuf1),0)
   GOTO 400
 300  DO i = 1 , N
      D(i) = 0.0D0
   ENDDO
   GOTO 100
!
!     TRIDIAGONALIZATION PROCEDURE UNTIL THE MATRIX FITS IN CORE
!
 400  Row = Row + 1
   rowp1 = Row + 1
   rowp2 = Row + 2
   It3 = 2
   Iii = rowp1
   CALL unpack(*500,ms,O(rowp1))
   GOTO 600
 500  DO i = rowp1 , N
      O(i) = 0.0D0
   ENDDO
!
!     FIND SINES AND COSINES
!
 600  CALL sinc0s(Row,rot,D,O,C)
   CALL gopen(Mo,A(ibuf2),im1)
   im1 = 3
   Ii = rowp2
   It1 = 2
   It2 = 2
   CALL pack(D(rowp2),Mo,mcb)
   CALL close(Mo,2)
!
!     WILL THE REST OF MATRIX FIT IN CORE
!
   IF ( (N-rowp1)*(N-rowp1+1)/2+1<=nzz ) THEN
!
!     TRIDIAGONALIZATION PROCEDURE WHEN MATRIX FITS IN CORE
!
!
!     FILL CORE WITH THE REST OF THE MATRIX
!
      row2 = filcor(mss,ms2,Iprec,rowp2,midin,N,A,nz,A(ibuf1))
      na = 1
      CALL gopen(Mo,A(ibuf2),3)
   ELSE
!
!         (N-ROWP1)*(N-ROWP1  )     < (NZZ-1)*2
!                   (N-ROWP1  )     < SQRT((NZZ-1)*2) (=NZSQ)
!                    N              < NZSQ + ROWP1
!                    N-NZSQ         < ROWP1
!                    N-NZSQ         = NUMBER OF ROTAIONS NEEDED
!
!     NO-- MUST REST OF MATRIX BE ROTATED
!
      IF ( rot/=0 ) THEN
         count = count + 1
         IF ( count==max ) count = 0
!
!     ROTATE THE REST OF THE MATRIX
!
         midout = rowp1 + (N-rowp1+3)/4
         row1 = rowp2
         CALL gopen(ms3,A(ibuf2),1)
!
!     HERE THRU 217 WILL BE VERY TIME COMSUMING. THE ROTATION IS ONE
!     ROW AT A TIME. COMPUTE HOW MANY ROTATIONS NEEDED. IF TOO MANY,
!     ISSUE A USER FATAL MESSAGE AND GET OUT
!
         i = N - nzsq
         IF ( i>25 ) THEN
            j = (N*N-nzsq*nzsq)*Iprec
            WRITE (Nout,99001) Ufm , N , N , i , j
99001       FORMAT (A23,' FROM GIVENS EIGENSOLVER - EXCESSIVE CPU TIME IS ','NEEDED FOR TRIDIAGONALIZE THE DYNAMIC',/5X,            &
                   &'MATRIX, WHICH IS',I6,' BY',I6,15X,1H(,I6,' LOOPS)',/5X,'RERUN JOB WITH',I8,                                    &
                   &' ADDITIONAL CORE WORDS, OR USE FEER,',' OR OTHER METHOD')
            CALL mesage(-61,0,0)
         ENDIF
         DO
!
!     FILL CORE WITH AS MUCH OF MATRIX AS POSSIBLE--UP TO ROW -ROW2-
!
            row2 = filcor(mss,ms2,Iprec,row1,midin,N,A,nz,A(ibuf1))
!
!     ROTATE ROWS ROW1 TO ROW2
!
            CALL rotate(Aa,Row,row1,row2,O,D,C)
!
!     EMPTY THE ROTATED ROWS ONTO MS3 AND MS4
!
            CALL empcor(ms3,ms4,Iprec,Iprec,row1,midout,row2,N,A,A(ibuf2))
            row1 = row2 + 1
            IF ( row2>=N ) THEN
!
!     SWITCH TAPES
!
               ms = ms1
               ms1 = ms3
               ms3 = ms
               ms = ms2
               ms2 = ms4
               ms4 = ms
               mss = ms1
               midin = midout
               EXIT
            ENDIF
         ENDDO
      ENDIF
      DO i = rowp1 , N
         D(i) = O(i)
      ENDDO
      ms = mss
      IF ( Row>midin ) THEN
         ms = ms2
         CALL gopen(ms,A(ibuf1),0)
      ELSEIF ( rot/=0 ) THEN
         CALL gopen(ms,A(ibuf1),0)
      ENDIF
      GOTO 400
   ENDIF
   DO
      IF ( rot/=0 ) THEN
         row1 = rowp2
         CALL rotate(Aa(na),Row,row1,row2,O,D,C)
      ENDIF
      DO i = rowp1 , N
         D(i) = O(i)
      ENDDO
      IF ( Row/=nm2 ) THEN
         Row = Row + 1
         rowp1 = Row + 1
         rowp2 = Row + 2
         DO i = rowp1 , N
            O(i) = Aa(na)
            na = na + 1
         ENDDO
         CALL sinc0s(Row,rot,D,O,C)
!
!     WRITE SINES ON MO
!
         Ii = rowp2
         It1 = 2
         It2 = 2
         CALL pack(D(rowp2),Mo,mcb)
      ELSE
!
!     ALL DONE.
!
         D(N) = Aa(na)
         O(N-1) = O(N)
         O(N) = 0.0D0
         CALL close(Mo,3)
         DO i = 1 , N
            C(i) = D(i)
            B(i) = O(i)**2
         ENDDO
         Xentry = -Entry
         Rstrt = 0
         Savemr = 0
         EXIT
      ENDIF
   ENDDO
END SUBROUTINE tridi