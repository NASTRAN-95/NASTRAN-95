!*==vecprt.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE vecprt(*,*,Px,Nx,A,Ox)
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nx
   INTEGER :: Px
   REAL , DIMENSION(Nx) :: A
   INTEGER :: Ox
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: cdp , csp , rdp , rsp
   INTEGER :: i , k , k1 , k2 , k6 , kk , kn , knkk , l , m , n , o , p , pm , tra
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
! End of declarations rewritten by SPAG
!
!
   DATA rsp , rdp , csp , cdp/1 , 2 , 3 , 4/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     l = k + k6 - kk
                     IF ( l>kn ) l = kn
                     DO i = k , l , kk
                        IF ( A(i)/=0. ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                     CYCLE
                  CASE (2)
                     m = m + 1
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
            IF ( m==1 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
            IF ( o<0 ) m = 2
         ENDIF
         ASSIGN 20 TO tra
         IF ( eject(m)/=0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         RETURN 2
 20      count = count - m
         knkk = kn/kk
         IF ( knkk>6 ) THEN
!
            ASSIGN 40 TO tra
            k = 1
         ELSE
            CALL format(A,1,kn,kk,-1,n)
            count = count + 1
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         l = k + k6 - kk
         IF ( l>kn ) l = kn
         IF ( o/=0 ) THEN
            DO i = k , l , kk
               IF ( A(i)/=0. ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         IF ( eject(1)/=0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      k1 = (k+kk-1)/kk
         k2 = (l+kk-1)/kk
         IF ( pm==csp ) THEN
            k1 = (k1+1)/2
            k2 = k2/2
         ENDIF
         CALL format(A,k,l,kk,k1,k2)
         spag_nextblock_1 = 4
      CASE (4)
         k = k + k6
         IF ( k<=kn ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
         WRITE (mo,99001)
99001    FORMAT (1X)
         count = count + 1
         spag_nextblock_1 = 6
      CASE (6)
         RETURN
      CASE (7)
!
         RETURN 1
!
!
         ENTRY prtvec(*,*)
!     ==================
!
         count = count + 1
         IF ( pm==csp ) THEN
            count = count + 1
            IF ( knkk<4 ) THEN
               WRITE (mo,99002)
99002          FORMAT (51X,4HREAL,11X,9HIMAGINARY)
            ELSEIF ( knkk==4 ) THEN
               WRITE (mo,99003)
99003          FORMAT (21X,2(12X,4HREAL,11X,9HIMAGINARY))
            ELSE
               WRITE (mo,99004)
99004          FORMAT (3X,3(12X,4HREAL,11X,9HIMAGINARY))
            ENDIF
         ENDIF
         GOTO tra
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE vecprt
