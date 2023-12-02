!*==tridi1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tridi1(D,O,C,A,B,Aa)
   IMPLICIT NONE
   USE C_GIVN
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: D
   REAL , DIMENSION(1) :: O
   REAL , DIMENSION(1) :: C
   REAL , DIMENSION(2) :: A
   REAL , DIMENSION(1) :: B
   REAL , DIMENSION(1) :: Aa
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: count , max
   INTEGER :: i , ibuf1 , ibuf2 , im1 , j , k , midin , midout , mr , ms , ms1 , ms2 , ms3 , ms4 , mss , n , na , nm1 , nm2 , nz ,  &
            & nzsq , nzz , rot , row1 , row2 , rowp1 , rowp2
   INTEGER , DIMENSION(7) , SAVE :: mcb
   EXTERNAL close , empcor , filcor , gopen , korsz , mesage , pack , rotate1 , sinc0s1 , unpack , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     MODIFIED GIVENS REAL SYMMETRIC TRIDIAGONALIZATION
!     THIS ROUTINE IS CALLED ONLY BY VALVEC
!
   !>>>>EQUIVALENCE (Vvcom(1),Title(1)) , (N,Vvcom(101))
   DATA count , max , mcb/0 , 10 , 7*0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         nm1 = n - 1
         nm2 = n - 2
         M3 = 305
         mss = Mr1
         ms1 = M1
         ms2 = M2
         ms3 = M3
         ms4 = M4
!
!     INITIALIZE  TRANSFORMATION ROUTINES
!
         midin = n
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
         It3 = 1
         Iii = 1
         Jjj = n
         Incr1 = 1
         CALL unpack(*20,Md,D)
         spag_nextblock_1 = 2
      CASE (2)
!
!     COPY REST OF MD ONTO MR
!
         It1 = 1
         It2 = 1
         Incr = 1
         k = n - 1
         DO i = 1 , k
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  Iii = 0
                  CALL unpack(*2,Md,A)
                  Ii = Iii
                  Jj = Jjj
                  spag_nextblock_2 = 2
               CASE (2)
                  CALL pack(A,mr,mcb)
                  CYCLE
 2                Ii = 1
                  Jj = 1
                  A(1) = 0.0
                  A(2) = 0.0
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         Iii = 1
         Jjj = n
         Ii = 1
         Jj = n
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
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 20      DO i = 1 , n
            D(i) = 0.0
         ENDDO
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
!     TRIDIAGONALIZATION PROCEDURE UNTIL THE MATRIX FITS IN CORE
!
         Row = Row + 1
         rowp1 = Row + 1
         rowp2 = Row + 2
         It3 = 1
         Iii = rowp1
         CALL unpack(*40,ms,O(rowp1))
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 40      DO i = rowp1 , n
            O(i) = 0.0
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
!
!     FIND SINES AND COSINES
!
         CALL sinc0s1(Row,rot,D,O,C)
         CALL gopen(Mo,A(ibuf2),im1)
         im1 = 3
         Ii = rowp2
         It1 = 1
         It2 = 1
         CALL pack(D(rowp2),Mo,mcb)
         CALL close(Mo,2)
!
!     WILL THE REST OF MATRIX FIT IN CORE
!
         IF ( (n-rowp1)*(n-rowp1+1)/2+1<=nzz ) THEN
!
!     TRIDIAGONALIZATION PROCEDURE WHEN MATRIX FITS IN CORE
!
!
!     FILL CORE WITH THE REST OF THE MATRIX
!
            row2 = filcor(mss,ms2,Iprec,rowp2,midin,n,A,nz,A(ibuf1))
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
               midout = rowp1 + (n-rowp1+3)/4
               row1 = rowp2
               CALL gopen(ms3,A(ibuf2),1)
!
!     HERE THRU 217 WILL BE VERY TIME COMSUMING. THE ROTATION IS ONE
!     ROW AT A TIME. COMPUTE HOW MANY ROTATIONS NEEDED. IF TOO MANY,
!     ISSUE A USER FATAL MESSAGE AND GET OUT
!
               i = n - nzsq
               IF ( i>25 ) THEN
                  j = (n*n-nzsq*nzsq)*Iprec
                  WRITE (Nout,99001) Ufm , n , n , i , j
99001             FORMAT (A23,' FROM GIVENS EIGENSOLVER - EXCESSIVE CPU TIME IS ','NEEDED FOR TRIDIAGONALIZE THE DYNAMIC',/5X,      &
                         &'MATRIX, WHICH IS',I6,' BY',I6,15X,1H(,I6,' LOOPS)',/5X,'RERUN JOB WITH',I8,                              &
                         &' ADDITIONAL CORE WORDS, OR USE FEER,',' OR OTHER METHOD')
                  CALL mesage(-61,0,0)
               ENDIF
               SPAG_Loop_1_1: DO
!
!     FILL CORE WITH AS MUCH OF MATRIX AS POSSIBLE--UP TO ROW -ROW2-
!
                  row2 = filcor(mss,ms2,Iprec,row1,midin,n,A,nz,A(ibuf1))
!
!     ROTATE ROWS ROW1 TO ROW2
!
                  CALL rotate1(Aa,Row,row1,row2,O,D,C)
!
!     EMPTY THE ROTATED ROWS ONTO MS3 AND MS4
!
                  CALL empcor(ms3,ms4,Iprec,Iprec,row1,midout,row2,n,A,A(ibuf2))
                  row1 = row2 + 1
                  IF ( row2>=n ) THEN
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
                     EXIT SPAG_Loop_1_1
                  ENDIF
               ENDDO SPAG_Loop_1_1
            ENDIF
            DO i = rowp1 , n
               D(i) = O(i)
            ENDDO
            ms = mss
            IF ( Row>midin ) THEN
               ms = ms2
               CALL gopen(ms,A(ibuf1),0)
            ELSEIF ( rot/=0 ) THEN
               CALL gopen(ms,A(ibuf1),0)
            ENDIF
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_2: DO
            IF ( rot/=0 ) THEN
               row1 = rowp2
               CALL rotate1(Aa(na),Row,row1,row2,O,D,C)
            ENDIF
            DO i = rowp1 , n
               D(i) = O(i)
            ENDDO
            IF ( Row/=nm2 ) THEN
               Row = Row + 1
               rowp1 = Row + 1
               rowp2 = Row + 2
               DO i = rowp1 , n
                  O(i) = Aa(na)
                  na = na + 1
               ENDDO
               CALL sinc0s1(Row,rot,D,O,C)
!
!     WRITE SINES ON MO
!
               Ii = rowp2
               It1 = 1
               It2 = 1
               CALL pack(D(rowp2),Mo,mcb)
            ELSE
!
!     ALL DONE.
!
               D(n) = Aa(na)
               O(n-1) = O(n)
               O(n) = 0.0
               CALL close(Mo,3)
               DO i = 1 , n
                  C(i) = D(i)
                  B(i) = O(i)**2
               ENDDO
               Xentry = -Entry
               Rstrt = 0
               Savemr = 0
               EXIT SPAG_Loop_1_2
            ENDIF
         ENDDO SPAG_Loop_1_2
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE tridi1
