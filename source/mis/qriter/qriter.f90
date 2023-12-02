!*==qriter.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE qriter(Val,O,Loc,Qr)
USE C_GIVN
USE C_MGIVXX
USE C_REIGKR
USE C_SYSTEM
USE C_XMSSG
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: Val
   REAL(REAL64) , DIMENSION(1) :: O
   INTEGER , DIMENSION(1) :: Loc
   INTEGER :: Qr
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(5) , SAVE :: above , below
   CHARACTER(5) :: belabv
   REAL(REAL64) , SAVE :: epsi , one , zero
   REAL(REAL64) :: g , ones , r , s , shift , t , u
   INTEGER :: i , ib , ibuf1 , iter , j , k , m , m1 , m1p1 , m2 , m2m1 , mm , mp1 , nn
   INTEGER , SAVE :: mgiv
   INTEGER , DIMENSION(10) , SAVE :: msg
   REAL :: valx
   EXTERNAL close , gopen , korsz , mesage , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     ORTEGA-KAISER QR ITERATION FOR A LARGE TRIDIAGONAL MATRIX
!
   DATA epsi , zero , one , msg/1.0D-10 , 0.0D+0 , 1.0D+0 , 53 , 9*0/
   DATA mgiv , below , above/4HMGIV , 'BELOW' , 'ABOVE'/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     VAL    = DIAGONAL TERMS OF THE TRIDIAGONAL.
!              REORDERED EIGENVALUES UPON RETURN.
!     O      = SQUARE OF THE OFF-DIAGIONAL TERMS OF THE TRIDIAGONAL.
!     LOC    = ORIGINAL LOCATIONS OF THE REORDERED EIGENVALUES.
!     QR     = 1 MEANS  VAL= EIGENVALUES--JUST REORDER THEM
!     N      = ORDER OF THE PROBLEM = ALSO NO. OF FREQ. EXTRACTED
!     MAX    = MAXIMUM NUMBER OF ITERATIONS
!     SHIFT  = SHIFT FACTOR (SMALLEST DIAGONAL TERM
!     LFREQ  , HFREQ = FREQ. RANGE OF INTEREST IF NV IS ZERO
!     NV     = NUMBER OF EIGENVECTORS TO BE COMPUTED, SAVED AND OUTPUT.
!              IF NV IS ZERO (INPUT), AND LFREQ-HFREQ ARE PRESENT, NV IS
!              SET TO BE NO. OF MODES WITHIN THE FREQ. RANGE (OUTPUT)
!     NE     = NO. OF EIGENVALUES (INCLUDING RIGID MODES) TO BE PRINTED.
!              ALL, IF NE IS NOT SPECIFIED.
!              IF NE .LT. NV, NE IS SET EQUAL TO NV
!
         Max = 100*N
         IF ( Nv>N ) Nv = N
         IF ( Ne==0 ) Ne = N
         IF ( Ne<Nv ) Ne = Nv
!
!     IS THIS AN ORDERING ONLY CALL
!
         Never = 0
         IF ( Qr/=0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SEARCH FOR A DECOUPLED SUBMATRIX.
!
         m2 = N
         spag_nextblock_1 = 2
      CASE (2)
         m2m1 = m2 - 1
         DO k = 1 , m2m1
            m1 = m2 - k
            IF ( O(m1)/=zero ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     ALL OFF-DIAGONAL TERMS ARE ZEROS, JOB DONE. GO TO 150
!     THE DIAGONALS CONTAIN THE EIGENVALUES.
!
         ENDDO
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
!     DECOUPLED SUBMATRIX
!
         m2m1 = m1
         m2 = m1 + 1
         IF ( m2m1/=1 ) THEN
            DO k = 2 , m2m1
               m1 = m2 - k
               IF ( O(m1)==zero ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         m1 = m1 + 1
         spag_nextblock_1 = 5
      CASE (5)
         mm = m1
         spag_nextblock_1 = 6
      CASE (6)
!
!     Q-R ITERATION FOR THE DECOUPLED SUBMATRIX
!
         DO iter = 1 , Max
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  IF ( dabs(Val(m2))+O(m2m1)==dabs(Val(m2)) ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  DO k = m1 , m2m1
                     IF ( Val(k)/=Val(k+1) ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  shift = zero
                  spag_nextblock_2 = 3
                  CYCLE SPAG_DispatchLoop_2
               CASE (2)
!
!     FIND THE SMALLEST DIAGONAL TERM = SHIFT
!
                  shift = Val(m2)
                  DO i = m1 , m2m1
                     IF ( dabs(Val(i))<dabs(shift) ) shift = Val(i)
                  ENDDO
!
!     REDUCE ALL TERMS BY SHIFT
!
                  DO i = m1 , m2
                     Val(i) = Val(i) - shift
                  ENDDO
                  spag_nextblock_2 = 3
               CASE (3)
!
!     Q-R ITERATION
!
                  r = Val(m1)**2
                  s = O(m1)/(r+O(m1))
                  t = zero
                  u = s*(Val(m1)+Val(m1+1))
                  Val(m1) = Val(m1) + u
                  IF ( m1/=m2m1 ) THEN
                     m1p1 = m1 + 1
                     DO i = m1p1 , m2m1
                        g = Val(i) - u
                        r = (one-t)*O(i-1)
                        ones = one - s
                        IF ( dabs(ones)>epsi ) r = g*g/ones
                        r = r + O(i)
                        O(i-1) = s*r
                        IF ( O(i-1)==zero ) mm = i
                        t = s
!
!     IBM MAY FLAG AN EXPONENT UNDERFLOW ON NEXT LINE.
!     IT IS PERFECTLY OK SINCE O(I) SHOULD BE APPROACHING ZERO.
!
                        s = O(i)/r
                        u = s*(g+Val(i+1))
                        Val(i) = u + g
                     ENDDO
                  ENDIF
!
                  Val(m2) = Val(m2) - u
                  r = (one-t)*O(m2m1)
                  ones = one - s
                  IF ( dabs(ones)>epsi ) r = Val(m2)**2/ones
                  O(m2m1) = s*r
!
!     SHIFT BACK
!
                  IF ( shift/=zero ) THEN
                     DO i = m1 , m2
                        Val(i) = Val(i) + shift
                     ENDDO
                  ENDIF
                  m1 = mm
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
!
!     TOO MANY ITERATIONS
!
!
!     THE ACCURACY OF EIGENVALUE  XXXXX  IS IN DOUBT--QRITER FAILED TO
!     CONVERGE IN  XX  ITERATIONS
!
         Never = Never + 1
         CALL mesage(msg(1),Val(m2),Max)
         spag_nextblock_1 = 7
      CASE (7)
!
!     CONVERGENCE ACHIEVED
!
         IF ( m1/=m2m1 ) THEN
            m2 = m2m1
            m2m1 = m2 - 1
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( m1>2 ) THEN
            m2 = m1 - 1
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         IF ( N/=1 ) THEN
!
!     REORDER EIGENVALUES ALGEBRAICALLY IN ASCENDING ORDER
!
            IF ( Ioptn==mgiv ) THEN
!
!     FOR MGIV METHOD, RECOMPUTE LAMBDA
!
               DO k = 1 , N
                  Val(k) = (1.0D0/Val(k)) - Dlmdas
               ENDDO
            ENDIF
            DO k = 1 , N
               SPAG_Loop_2_1: DO m = 1 , N
                  IF ( Val(m)/=-10000.0D0 ) EXIT SPAG_Loop_2_1
               ENDDO SPAG_Loop_2_1
               IF ( m/=N ) THEN
                  mp1 = m + 1
                  DO i = mp1 , N
                     IF ( Val(i)/=-10000.0D0 ) THEN
                        IF ( Val(m)>Val(i) ) m = i
                     ENDIF
                  ENDDO
               ENDIF
               O(k) = Val(m)
               Val(m) = -10000.0D0
               Loc(k) = m
            ENDDO
            DO i = 1 , N
               Val(i) = O(i)
            ENDDO
!
!     IF RIGID MODES WERE FOUND BEFORE, REPLACE RIGID FREQ. BY ZERO
!
            IF ( Nfound/=0 ) THEN
               DO i = 1 , Nfound
                  Val(i) = zero
               ENDDO
            ENDIF
         ENDIF
!
!     OUTPUT OPTION CHECK - BY FREQ. RANGE OR BY NO. OF FREQ.
!     REQUESTED
!
         ib = 1
         IF ( Nv==0 ) THEN
            IF ( Lfreq>0.0 ) THEN
!
!     LOCATE PONTER THAT POINTS TO EIGENVALUE ABOVE OR EQUAL THE
!     LOWEST LFREQ. AS REQUESTED.
!
               DO i = 1 , N
                  IF ( Val(i)>=Lfreq ) GOTO 5
               ENDDO
               i = 0
 5             ib = i
            ENDIF
         ENDIF
!
!     OPEN LAMA FOR OUTPUT
!     PUT EIGENVALUES ON LAMA FOLLOWED BY ORDER FOUND
!
         ibuf1 = (korsz(O)-Sysbuf+1)/2
         CALL gopen(Lama,O(ibuf1),1)
         nn = 0
         IF ( ib/=0 ) THEN
            SPAG_Loop_1_2: DO i = ib , N
               valx = Val(i)
               IF ( Nv/=0 .AND. i>Ne ) EXIT SPAG_Loop_1_2
               IF ( Nv==0 .AND. valx>Hfreq ) EXIT SPAG_Loop_1_2
               CALL write(Lama,valx,1,0)
               nn = nn + 1
            ENDDO SPAG_Loop_1_2
         ENDIF
!
!
!     IF FREQ. RANGE IS REQUESTED, AND ALL FREQ. FOUND ARE OUTSIDE THE
!     RANGE, OUTPUT AT LEAST ONE FREQ.
!
         IF ( nn<=0 ) THEN
            IF ( ib==0 ) belabv = below
            IF ( ib/=0 ) belabv = above
            WRITE (Nout,99001) Uim , belabv
99001       FORMAT (A29,', ALL ROOTS FOUND WERE ',A5,' FREQ. RANGE SPECIFIED',/5X,                                                  &
                   &'HOWEVER, ONE EIGENVALUE OUTSIDE THIS FREQ. RANGE WAS',' SAVED AND PRINTED')
            nn = 1
            IF ( ib/=0 ) ib = N
            IF ( ib==0 ) ib = 1
            CALL write(Lama,Val(ib),1,0)
         ENDIF
         CALL write(Lama,0,0,1)
         CALL write(Lama,Loc(ib),nn,1)
         CALL close(Lama,1)
         msg(2) = Lama
         msg(3) = nn
         CALL wrttrl(msg(2))
!
!     IF FREQ. DOES NOT START FROM FIRST FUNDAMENTAL MODE, ADJUST VAL
!     AND LOC TABLES SO THAT WILVEC WILL PICK UP FREQUENCIES CORRECTLY
!
         IF ( ib>1 ) THEN
            j = 1
            DO i = ib , N
               Val(j) = Val(i)
               Loc(j) = Loc(i)
               j = j + 1
            ENDDO
         ENDIF
!
         IF ( Nv==0 .AND. ib>1 .AND. nn<Nfound .AND. Val(1)<=zero ) Nfound = 0
         IF ( Nv==0 ) Nv = nn
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE qriter
