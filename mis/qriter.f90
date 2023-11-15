
SUBROUTINE qriter(Val,O,Loc,Qr)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION Dlmdas
   REAL Hfreq , Lfreq
   INTEGER Idum0(100) , Idum11 , Idum12 , Idum13 , Idum3 , Idum4 , Idum9 , Ioptn , Lama , Max , N , Ne , Never , Nfound , Nout ,    &
         & Nv , Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /givn  / Idum0 , N , Lfreq , Idum3 , Idum4 , Hfreq , Lama , Nv , Ne , Idum9 , Nfound , Idum11 , Idum12 , Idum13 , Never , &
                 & Max
   COMMON /mgivxx/ Dlmdas
   COMMON /reigkr/ Ioptn
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm , Uim
!
! Dummy argument declarations
!
   INTEGER Qr
   INTEGER Loc(1)
   DOUBLE PRECISION O(1) , Val(1)
!
! Local variable declarations
!
   CHARACTER*5 above , belabv , below
   DOUBLE PRECISION epsi , g , one , ones , r , s , shift , t , u , zero
   INTEGER i , ib , ibuf1 , iter , j , k , m , m1 , m1p1 , m2 , m2m1 , mgiv , mm , mp1 , msg(10) , nn
   INTEGER korsz
   REAL valx
!
! End of declarations
!
!
!     ORTEGA-KAISER QR ITERATION FOR A LARGE TRIDIAGONAL MATRIX
!
   DATA epsi , zero , one , msg/1.0D-10 , 0.0D+0 , 1.0D+0 , 53 , 9*0/
   DATA mgiv , below , above/4HMGIV , 'BELOW' , 'ABOVE'/
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
   IF ( Qr/=0 ) GOTO 800
!
!     SEARCH FOR A DECOUPLED SUBMATRIX.
!
   m2 = N
 100  m2m1 = m2 - 1
   DO k = 1 , m2m1
      m1 = m2 - k
      IF ( O(m1)/=zero ) GOTO 200
   ENDDO
!
!     ALL OFF-DIAGONAL TERMS ARE ZEROS, JOB DONE. GO TO 150
!     THE DIAGONALS CONTAIN THE EIGENVALUES.
!
   GOTO 800
!
!     DECOUPLED SUBMATRIX
!
 200  m2m1 = m1
   m2 = m1 + 1
   IF ( m2m1/=1 ) THEN
      DO k = 2 , m2m1
         m1 = m2 - k
         IF ( O(m1)==zero ) GOTO 300
      ENDDO
   ENDIF
   GOTO 400
 300  m1 = m1 + 1
 400  mm = m1
!
!     Q-R ITERATION FOR THE DECOUPLED SUBMATRIX
!
 500  DO iter = 1 , Max
      IF ( dabs(Val(m2))+O(m2m1)==dabs(Val(m2)) ) GOTO 700
      DO k = m1 , m2m1
         IF ( Val(k)/=Val(k+1) ) GOTO 550
      ENDDO
      shift = zero
      GOTO 600
!
!     FIND THE SMALLEST DIAGONAL TERM = SHIFT
!
 550  shift = Val(m2)
      DO i = m1 , m2m1
         IF ( dabs(Val(i))<dabs(shift) ) shift = Val(i)
      ENDDO
!
!     REDUCE ALL TERMS BY SHIFT
!
      DO i = m1 , m2
         Val(i) = Val(i) - shift
      ENDDO
!
!     Q-R ITERATION
!
 600  r = Val(m1)**2
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
!
!     CONVERGENCE ACHIEVED
!
 700  IF ( m1/=m2m1 ) THEN
      m2 = m2m1
      m2m1 = m2 - 1
      GOTO 500
   ELSEIF ( m1>2 ) THEN
      m2 = m1 - 1
      GOTO 100
   ENDIF
 800  IF ( N/=1 ) THEN
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
         DO m = 1 , N
            IF ( Val(m)/=-10000.0D0 ) EXIT
         ENDDO
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
            IF ( Val(i)>=Lfreq ) GOTO 820
         ENDDO
         i = 0
 820     ib = i
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
      DO i = ib , N
         valx = Val(i)
         IF ( Nv/=0 .AND. i>Ne ) EXIT
         IF ( Nv==0 .AND. valx>Hfreq ) EXIT
         CALL write(Lama,valx,1,0)
         nn = nn + 1
      ENDDO
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
99001 FORMAT (A29,', ALL ROOTS FOUND WERE ',A5,' FREQ. RANGE SPECIFIED',/5X,'HOWEVER, ONE EIGENVALUE OUTSIDE THIS FREQ. RANGE WAS', &
             &' SAVED AND PRINTED')
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
END SUBROUTINE qriter
