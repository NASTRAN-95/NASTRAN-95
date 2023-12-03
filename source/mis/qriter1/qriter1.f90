!*==qriter1.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE qriter1(Val,O,Loc,Qr)
   IMPLICIT NONE
   USE c_givn
   USE c_mgivxx
   USE c_reigkr
   USE c_system
   USE c_xmssg
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Val
   REAL , DIMENSION(1) :: O
   INTEGER , DIMENSION(1) :: Loc
   INTEGER :: Qr
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(5) , SAVE :: above , below
   CHARACTER(5) :: belabv
   REAL , SAVE :: epsi , one , zero
   REAL :: g , ones , r , s , shift , t , u , valx
   INTEGER :: i , ib , ibuf1 , iter , j , k , m , m1 , m1p1 , m2 , m2m1 , mm , mp1 , nn
   INTEGER , SAVE :: mgiv
   INTEGER , DIMENSION(10) , SAVE :: msg
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     ORTEGA-KAISER QR ITERATION FOR A LARGE TRIDIAGONAL MATRIX
!
!WKBR 2/94 SPR93027      COMMON /SYSTEM/  SYSBUF,NOUT
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
   max = 100*n
   IF ( nv>n ) nv = n
   IF ( ne==0 ) ne = n
   IF ( ne<nv ) ne = nv
!
!     IS THIS AN ORDERING ONLY CALL
!
   never = 0
   IF ( Qr/=0 ) GOTO 800
!
!     SEARCH FOR A DECOUPLED SUBMATRIX.
!
   m2 = n
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
 500  DO iter = 1 , max
      IF ( abs(Val(m2))+O(m2m1)==abs(Val(m2)) ) GOTO 700
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
         IF ( abs(Val(i))<abs(shift) ) shift = Val(i)
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
            IF ( abs(ones)>epsi ) r = g*g/ones
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
      IF ( abs(ones)>epsi ) r = Val(m2)**2/ones
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
   never = never + 1
   CALL mesage(msg(1),Val(m2),max)
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
 800  IF ( n/=1 ) THEN
!
!     REORDER EIGENVALUES ALGEBRAICALLY IN ASCENDING ORDER
!
      IF ( ioptn==mgiv ) THEN
!
!     FOR MGIV METHOD, RECOMPUTE LAMBDA
!
         DO k = 1 , n
            Val(k) = (1.0/Val(k)) - dlmdas
         ENDDO
      ENDIF
      DO k = 1 , n
         DO m = 1 , n
            IF ( Val(m)/=-10000.0 ) EXIT
         ENDDO
         IF ( m/=n ) THEN
            mp1 = m + 1
            DO i = mp1 , n
               IF ( Val(i)/=-10000.0 ) THEN
                  IF ( Val(m)>Val(i) ) m = i
               ENDIF
            ENDDO
         ENDIF
         O(k) = Val(m)
         Val(m) = -10000.0
         Loc(k) = m
      ENDDO
      DO i = 1 , n
         Val(i) = O(i)
      ENDDO
!
!     IF RIGID MODES WERE FOUND BEFORE, REPLACE RIGID FREQ. BY ZERO
!
      IF ( nfound/=0 ) THEN
         DO i = 1 , nfound
            Val(i) = zero
         ENDDO
      ENDIF
   ENDIF
!
!     OUTPUT OPTION CHECK - BY FREQ. RANGE OR BY NO. OF FREQ.
!     REQUESTED
!
   ib = 1
   IF ( nv==0 ) THEN
      IF ( lfreq>0.0 ) THEN
!
!     LOCATE PONTER THAT POINTS TO EIGENVALUE ABOVE OR EQUAL THE
!     LOWEST LFREQ. AS REQUESTED.
!
         DO i = 1 , n
            IF ( Val(i)>=lfreq ) GOTO 820
         ENDDO
         i = 0
 820     ib = i
      ENDIF
   ENDIF
!
!     OPEN LAMA FOR OUTPUT
!     PUT EIGENVALUES ON LAMA FOLLOWED BY ORDER FOUND
!
!WKBR 2/94 SPR93027  225 IBUF1 = (KORSZ(O)-SYSBUF+1)/2
   ibuf1 = (korsz(O)-sysbuf+1)/iprec
   CALL gopen(lama,O(ibuf1),1)
   nn = 0
   IF ( ib/=0 ) THEN
      DO i = ib , n
         valx = Val(i)
         IF ( nv/=0 .AND. i>ne ) EXIT
         IF ( nv==0 .AND. valx>hfreq ) EXIT
         CALL write(lama,valx,1,0)
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
      WRITE (nout,99001) uim , belabv
99001 FORMAT (A29,', ALL ROOTS FOUND WERE ',A5,' FREQ. RANGE SPECIFIED',/5X,'HOWEVER, ONE EIGENVALUE OUTSIDE THIS FREQ. RANGE WAS', &
             &' SAVED AND PRINTED')
      nn = 1
      IF ( ib/=0 ) ib = n
      IF ( ib==0 ) ib = 1
      CALL write(lama,Val(ib),1,0)
   ENDIF
   CALL write(lama,0,0,1)
   CALL write(lama,Loc(ib),nn,1)
   CALL close(lama,1)
   msg(2) = lama
   msg(3) = nn
   CALL wrttrl(msg(2))
!
!     IF FREQ. DOES NOT START FROM FIRST FUNDAMENTAL MODE, ADJUST VAL
!     AND LOC TABLES SO THAT WILVEC WILL PICK UP FREQUENCIES CORRECTLY
!
   IF ( ib>1 ) THEN
      j = 1
      DO i = ib , n
         Val(j) = Val(i)
         Loc(j) = Loc(i)
         j = j + 1
      ENDDO
   ENDIF
!
   IF ( nv==0 .AND. ib>1 .AND. nn<nfound .AND. Val(1)<=zero ) nfound = 0
   IF ( nv==0 ) nv = nn
END SUBROUTINE qriter1