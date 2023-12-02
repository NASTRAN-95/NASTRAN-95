!*==alg01.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg01(Xdata,Ydata,Ndata,Xin,Yout,Slope,Nxy,Ntype,Nwot)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: Xdata
   REAL , DIMENSION(2) :: Ydata
   INTEGER :: Ndata
   REAL , DIMENSION(1) :: Xin
   REAL , DIMENSION(1) :: Yout
   REAL , DIMENSION(1) :: Slope
   INTEGER :: Nxy
   INTEGER :: Ntype
   INTEGER :: Nwot
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(21) :: a , b , d , m
   REAL :: dx , yprime
   INTEGER :: i , ii , islope , iy , j , jp , kp , n
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!
!
   IF ( Ntype/=1 .AND. Ndata>=3 ) THEN
      a(1) = 1.0
      b(1) = 0.0
      d(1) = 0.0
      n = Ndata - 1
      DO i = 2 , n
         a(i) = (Xdata(i+1)-Xdata(i-1))/3.0 - (Xdata(i)-Xdata(i-1))*b(i-1)/(6.0*a(i-1))
         b(i) = (Xdata(i+1)-Xdata(i))/6.0
         d(i) = (Ydata(i+1)-Ydata(i))/(Xdata(i+1)-Xdata(i)) - (Ydata(i)-Ydata(i-1))/(Xdata(i)-Xdata(i-1)) - (Xdata(i)-Xdata(i-1))   &
              & *d(i-1)/(6.0*a(i-1))
      ENDDO
      a(Ndata) = 0.0
      b(Ndata) = 1.0
      d(Ndata) = 0.0
      m(Ndata) = a(Ndata)*d(n)/(a(Ndata)*b(n)-a(n)*b(Ndata))
      DO ii = 2 , Ndata
         i = Ndata + 1 - ii
         m(i) = (d(i)-b(i)*m(i+1))/a(i)
      ENDDO
      ASSIGN 5 TO iy
      IF ( Nwot==1 ) ASSIGN 10 TO iy
      ASSIGN 10 TO islope
      IF ( Nwot==0 ) ASSIGN 50 TO islope
      j = 2
      DO i = 1 , Nxy
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
               IF ( Xin(i)<Xdata(1) ) THEN
                  jp = 1
                  kp = 2
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( Xin(i)>Xdata(Ndata) ) THEN
                  jp = Ndata
                  kp = n
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  DO WHILE ( Xin(i)>Xdata(j) )
                     j = j + 1
                  ENDDO
                  dx = Xdata(j) - Xdata(j-1)
                  GOTO iy
               ENDIF
 5             Yout(i) = m(j-1)/(6.0*dx)*(Xdata(j)-Xin(i))**3 + m(j)/(6.0*dx)*(Xin(i)-Xdata(j-1))**3 + (Xdata(j)-Xin(i))            &
                       & *(Ydata(j-1)/dx-m(j-1)/6.0*dx) + (Xin(i)-Xdata(j-1))*(Ydata(j)/dx-m(j)/6.0*dx)
               GOTO islope
 10            Slope(i) = (-m(j-1)*(Xdata(j)-Xin(i))**2/2.0+m(j)*(Xin(i)-Xdata(j-1))**2/2.0+Ydata(j)-Ydata(j-1))/dx - (m(j)-m(j-1)) &
                        & /6.0*dx
               CYCLE
            CASE (2)
               yprime = (Ydata(kp)-Ydata(jp))/(Xdata(kp)-Xdata(jp)) - m(kp)/6.0*(Xdata(kp)-Xdata(jp))
               IF ( Nwot/=1 ) Yout(i) = Ydata(jp) + (Xin(i)-Xdata(jp))*yprime
               IF ( Nwot/=0 ) Slope(i) = yprime
               EXIT SPAG_DispatchLoop_1
            END SELECT
         ENDDO SPAG_DispatchLoop_1
 50   ENDDO
      RETURN
   ELSEIF ( Ndata/=1 ) THEN
      IF ( Nwot/=1 ) THEN
         j = 2
         DO i = 1 , Nxy
            DO WHILE ( Xin(i)>Xdata(j) .AND. j/=Ndata )
               j = j + 1
            ENDDO
            Yout(i) = Ydata(j-1) + (Ydata(j)-Ydata(j-1))/(Xdata(j)-Xdata(j-1))*(Xin(i)-Xdata(j-1))
         ENDDO
         IF ( Nwot/=2 ) RETURN
      ENDIF
      yprime = (Ydata(2)-Ydata(1))/(Xdata(2)-Xdata(1))
      DO i = 1 , Nxy
         Slope(i) = yprime
      ENDDO
   ELSE
      DO i = 1 , Nxy
         Yout(i) = Ydata(1)
      ENDDO
      RETURN
   ENDIF
END SUBROUTINE alg01
