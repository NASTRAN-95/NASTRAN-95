!*==tridi.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tridi(D,O,C,A,B,Aa)
   USE c_givn
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: D
   REAL(REAL64) , DIMENSION(1) :: O
   REAL(REAL64) , DIMENSION(1) :: C
   REAL , DIMENSION(2) :: A
   REAL(REAL64) , DIMENSION(1) :: B
   REAL(REAL64) , DIMENSION(1) :: Aa
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: count , max
   INTEGER :: i , ibuf1 , ibuf2 , im1 , j , k , midin , midout , mr , ms , ms1 , ms2 , ms3 , ms4 , mss , n , na , nm1 , nm2 , nz ,  &
            & nzsq , nzz , rot , row1 , row2 , rowp1 , rowp2
   INTEGER , DIMENSION(7) , SAVE :: mcb
   EXTERNAL close , empcor , filcor , gopen , korsz , mesage , pack , rotate , sinc0s , unpack , write
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
         ibuf1 = nz - sysbuf + 1
         ibuf2 = ibuf1 - sysbuf
         nz = nz - 2*sysbuf
         nzz = nz/iprec
         nzsq = sqrt(float((nzz-1)*2))
         im1 = 1
         nm1 = n - 1
         nm2 = n - 2
         m3 = 305
         mss = mr1
         ms1 = m1
         ms2 = m2
         ms3 = m3
         ms4 = m4
!
!     INITIALIZE  TRANSFORMATION ROUTINES
!
!     SICOX AND ROTAX ARE NOT USED ANY MORE. SEE SINC0S AND ROTATE
!
!     CALL SICOX (D,O,C)
!     CALL ROTAX (O,D,C)
!
         midin = n
         mr = mr1
!
!     START AT THE BEGINNING
!
         row = 0
!
!     OPEN MD
!
         CALL gopen(md,A(ibuf1),0)
         CALL gopen(mr,A(ibuf2),1)
!
!     SET UP FOR UNPACK
!
         it3 = 2
         iii = 1
         jjj = n
         incr1 = 1
         CALL unpack(*20,md,D)
         spag_nextblock_1 = 2
      CASE (2)
!
!     COPY REST OF MD ONTO MR
!
         it1 = 2
         it2 = 2
         incr = 1
         k = n - 1
         DO i = 1 , k
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  iii = 0
                  CALL unpack(*2,md,A)
                  ii = iii
                  jj = jjj
                  spag_nextblock_2 = 2
               CASE (2)
                  CALL pack(A,mr,mcb)
                  CYCLE
 2                ii = 1
                  jj = 1
                  A(1) = 0.0
                  A(2) = 0.0
                  spag_nextblock_2 = 2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         iii = 1
         jjj = n
         ii = 1
         jj = n
!
!     END OF MATRIX MD
!
         CALL write(mr,row,1,1)
!
!     ATTACH DIAGONALS
!
         CALL pack(D,mr,mcb)
         CALL close(md,1)
         CALL close(mr,1)
         ms = mr
         CALL gopen(ms,A(ibuf1),0)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 20      DO i = 1 , n
            D(i) = 0.0D0
         ENDDO
         spag_nextblock_1 = 2
      CASE (3)
!
!     TRIDIAGONALIZATION PROCEDURE UNTIL THE MATRIX FITS IN CORE
!
         row = row + 1
         rowp1 = row + 1
         rowp2 = row + 2
         it3 = 2
         iii = rowp1
         CALL unpack(*40,ms,O(rowp1))
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 40      DO i = rowp1 , n
            O(i) = 0.0D0
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
!
!     FIND SINES AND COSINES
!
         CALL sinc0s(row,rot,D,O,C)
         CALL gopen(mo,A(ibuf2),im1)
         im1 = 3
         ii = rowp2
         it1 = 2
         it2 = 2
         CALL pack(D(rowp2),mo,mcb)
         CALL close(mo,2)
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
            row2 = filcor(mss,ms2,iprec,rowp2,midin,n,A,nz,A(ibuf1))
            na = 1
            CALL gopen(mo,A(ibuf2),3)
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
                  j = (n*n-nzsq*nzsq)*iprec
                  WRITE (nout,99001) ufm , n , n , i , j
99001             FORMAT (A23,' FROM GIVENS EIGENSOLVER - EXCESSIVE CPU TIME IS ','NEEDED FOR TRIDIAGONALIZE THE DYNAMIC',/5X,      &
                         &'MATRIX, WHICH IS',I6,' BY',I6,15X,1H(,I6,' LOOPS)',/5X,'RERUN JOB WITH',I8,                              &
                         &' ADDITIONAL CORE WORDS, OR USE FEER,',' OR OTHER METHOD')
                  CALL mesage(-61,0,0)
               ENDIF
               SPAG_Loop_1_1: DO
!
!     FILL CORE WITH AS MUCH OF MATRIX AS POSSIBLE--UP TO ROW -ROW2-
!
                  row2 = filcor(mss,ms2,iprec,row1,midin,n,A,nz,A(ibuf1))
!
!     ROTATE ROWS ROW1 TO ROW2
!
                  CALL rotate(Aa,row,row1,row2,O,D,C)
!
!     EMPTY THE ROTATED ROWS ONTO MS3 AND MS4
!
                  CALL empcor(ms3,ms4,iprec,iprec,row1,midout,row2,n,A,A(ibuf2))
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
            IF ( row>midin ) THEN
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
               CALL rotate(Aa(na),row,row1,row2,O,D,C)
            ENDIF
            DO i = rowp1 , n
               D(i) = O(i)
            ENDDO
            IF ( row/=nm2 ) THEN
               row = row + 1
               rowp1 = row + 1
               rowp2 = row + 2
               DO i = rowp1 , n
                  O(i) = Aa(na)
                  na = na + 1
               ENDDO
               CALL sinc0s(row,rot,D,O,C)
!
!     WRITE SINES ON MO
!
               ii = rowp2
               it1 = 2
               it2 = 2
               CALL pack(D(rowp2),mo,mcb)
            ELSE
!
!     ALL DONE.
!
               D(n) = Aa(na)
               O(n-1) = O(n)
               O(n) = 0.0D0
               CALL close(mo,3)
               DO i = 1 , n
                  C(i) = D(i)
                  B(i) = O(i)**2
               ENDDO
               xentry = -entry
               rstrt = 0
               savemr = 0
               EXIT SPAG_Loop_1_2
            ENDIF
         ENDDO SPAG_Loop_1_2
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE tridi
