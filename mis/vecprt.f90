
SUBROUTINE vecprt(*,*,Px,Nx,A,Ox)
   IMPLICIT NONE
   INTEGER Count , Maxlin , Mo
   REAL Skp1 , Skp2(6) , Skp3(2)
   COMMON /system/ Skp1 , Mo , Skp2 , Maxlin , Skp3 , Count
   INTEGER Nx , Ox , Px
   REAL A(Nx)
   INTEGER cdp , csp , i , k , k1 , k2 , k6 , kk , kn , knkk , l , m , n , o , p , pm , rdp , rsp , tra
   INTEGER eject
!
   DATA rsp , rdp , csp , cdp/1 , 2 , 3 , 4/
!
!     PX = VECTOR TYPE + PRECISION.
!     NX = VECTOR LENGTH.
!     A  = VECTOR LOCATION.
!
!     THE VECTOR COMPONENTS WILL BE PRINTED 6 PER LINE IF REAL OR
!                IMAGINARY, AND 3 PER LINE IF COMPLEX.
!          O = 0 IF ALL THE VECTOR COMPONENTS ARE TO BE PRINTED, AND IF
!                THEY ARE TO BE PRINTED STARTING ON A NEW PAGE IF THEY
!                WILL NOT FIT ON THE CURRENT PAGE.
!          O = 1 IF ONLY THOSE LINES WHICH HAVE AT LEAST ONE NON-ZERO
!                COMPONENT ARE TO BE PRINTED, AND IF THE VECTOR IS TO BE
!                PRINTED STARTING ON A NEW PAGE IF IT WILL NOT FIT ON
!                THE CURRENT PAGE.
!          O =-1 IF ONLY THOSE LINES WHICH HAVE AT LEAST ONE NON-ZERO
!                COMPONENT ARE TO BE PRINTED, AND IF THE VECTOR IS TO BE
!                PRINTED ON THE CURRENT PAGE UNLESS TWO LINES WILL NOT
!                FIT.
!
!     RETURN 1 - PRINT SUBTITLE + VECTOR IDENTIFICATION.
!     RETURN 2 - PRINT VECTOR IDENTIFICATION ONLY.
!                PRTVEC = RETURN ENTRY POINT.
!
   p = Px
   n = Nx
   o = Ox
!
   pm = p
   IF ( p==rdp ) pm = rsp
   IF ( p==cdp ) pm = csp
   kk = 1
   IF ( pm==csp ) kk = 2
   IF ( p==rdp .OR. p==cdp ) kk = 2*kk
   kn = kk*n
   IF ( pm==csp ) kk = kk/2
   k6 = kk*6
   IF ( o==0 ) THEN
      m = (n+5)/6 + 1
      IF ( pm==csp ) m = (n+2)/3 + 2
   ELSE
!
      m = 1
      DO k = 1 , kn , k6
         l = k + k6 - kk
         IF ( l>kn ) l = kn
         DO i = k , l , kk
            IF ( A(i)/=0. ) GOTO 20
         ENDDO
         CYCLE
 20      m = m + 1
      ENDDO
      IF ( m==1 ) GOTO 700
!
      IF ( o<0 ) m = 2
   ENDIF
   ASSIGN 100 TO tra
   IF ( eject(m)/=0 ) GOTO 800
   RETURN 2
 100  Count = Count - m
   knkk = kn/kk
   IF ( knkk>6 ) THEN
!
      ASSIGN 400 TO tra
      k = 1
   ELSE
      CALL format(A,1,kn,kk,-1,n)
      Count = Count + 1
      GOTO 600
   ENDIF
 200  l = k + k6 - kk
   IF ( l>kn ) l = kn
   IF ( o/=0 ) THEN
      DO i = k , l , kk
         IF ( A(i)/=0. ) GOTO 300
      ENDDO
      GOTO 500
   ENDIF
 300  IF ( eject(1)/=0 ) GOTO 800
 400  k1 = (k+kk-1)/kk
   k2 = (l+kk-1)/kk
   IF ( pm==csp ) THEN
      k1 = (k1+1)/2
      k2 = k2/2
   ENDIF
   CALL format(A,k,l,kk,k1,k2)
 500  k = k + k6
   IF ( k<=kn ) GOTO 200
!
 600  WRITE (Mo,99001)
99001 FORMAT (1X)
   Count = Count + 1
 700  RETURN
!
 800  RETURN 1
!
!
   ENTRY prtvec(*,*)
!     ==================
!
   Count = Count + 1
   IF ( pm==csp ) THEN
      Count = Count + 1
      IF ( knkk<4 ) THEN
         WRITE (Mo,99002)
99002    FORMAT (51X,4HREAL,11X,9HIMAGINARY)
      ELSEIF ( knkk==4 ) THEN
         WRITE (Mo,99003)
99003    FORMAT (21X,2(12X,4HREAL,11X,9HIMAGINARY))
      ELSE
         WRITE (Mo,99004)
99004    FORMAT (3X,3(12X,4HREAL,11X,9HIMAGINARY))
      ENDIF
   ENDIF
   GOTO tra
END SUBROUTINE vecprt
