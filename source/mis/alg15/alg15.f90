!*==alg15.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg15(Xdata,Ydata,Ndata,Xin,Yout,Nxy,Ntype)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: Xdata
   REAL , DIMENSION(2) :: Ydata
   INTEGER :: Ndata
   REAL , DIMENSION(1) :: Xin
   REAL , DIMENSION(1) :: Yout
   INTEGER :: Nxy
   INTEGER :: Ntype
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(21) :: a , b , d , m
   REAL :: dx , yprime
   INTEGER :: i , ii , j , jp , kp , n
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!
         IF ( Ndata<=1 ) THEN
            DO i = 1 , Nxy
               Yout(i) = Ydata(1)
            ENDDO
            RETURN
         ELSE
            IF ( Ndata>2 ) THEN
               IF ( Ntype<=0 ) THEN
                  a(1) = 1.0
                  b(1) = 0.0
                  d(1) = 0.0
                  n = Ndata - 1
                  DO i = 2 , n
                     a(i) = (Xdata(i+1)-Xdata(i-1))/3.0 - (Xdata(i)-Xdata(i-1))*b(i-1)/(6.0*a(i-1))
                     b(i) = (Xdata(i+1)-Xdata(i))/6.0
                     d(i) = (Ydata(i+1)-Ydata(i))/(Xdata(i+1)-Xdata(i)) - (Ydata(i)-Ydata(i-1))/(Xdata(i)-Xdata(i-1))               &
                          & - (Xdata(i)-Xdata(i-1))*d(i-1)/6.0/a(i-1)
                  ENDDO
                  m(Ndata) = 0.0
                  DO ii = 2 , n
                     i = Ndata + 1 - ii
                     m(i) = (d(i)-b(i)*m(i+1))/a(i)
                  ENDDO
                  m(1) = 0.0
                  j = 1
                  i = 1
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            j = 1
            i = 1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         IF ( Xin(i)<=Xdata(2) ) THEN
            Yout(i) = Ydata(1) + (Ydata(2)-Ydata(1))/(Xdata(2)-Xdata(1))*(Xin(i)-Xdata(1))
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Xin(i)>=Xdata(Ndata-1) ) THEN
            Yout(i) = Ydata(Ndata-1) + (Ydata(Ndata)-Ydata(Ndata-1))/(Xdata(Ndata)-Xdata(Ndata-1))*(Xin(i)-Xdata(Ndata-1))
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_1: DO
            IF ( Xin(i)<Xdata(j) ) THEN
            ELSEIF ( Xin(i)==Xdata(j) ) THEN
               Yout(i) = Ydata(j) + (Ydata(j+1)-Ydata(j))/(Xdata(j+1)-Xdata(j))*(Xin(i)-Xdata(j))
               EXIT SPAG_Loop_1_1
            ELSEIF ( Xin(i)<=Xdata(j+1) ) THEN
               Yout(i) = Ydata(j) + (Ydata(j+1)-Ydata(j))/(Xdata(j+1)-Xdata(j))*(Xin(i)-Xdata(j))
               EXIT SPAG_Loop_1_1
            ENDIF
            j = j + 1
            IF ( j>=Ndata ) j = 1
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 3
      CASE (3)
         IF ( i<Nxy ) THEN
            i = i + 1
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            RETURN
         ENDIF
      CASE (4)
         IF ( Xin(i)<Xdata(1) ) THEN
            jp = 1
            kp = 2
         ELSEIF ( Xin(i)==Xdata(1) ) THEN
            Yout(i) = Ydata(1)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( Xin(i)<Xdata(Ndata) ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Xin(i)==Xdata(Ndata) ) THEN
               Yout(i) = Ydata(Ndata)
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
               jp = Ndata
               kp = Ndata - 1
            ENDIF
         ENDIF
         yprime = (Ydata(kp)-Ydata(jp))/(Xdata(kp)-Xdata(jp)) - m(kp)/6.0*(Xdata(kp)-Xdata(jp))
         Yout(i) = Ydata(jp) + (Xin(i)-Xdata(jp))*yprime
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
         IF ( Xin(i)>=Xdata(j) ) THEN
            IF ( Xin(i)==Xdata(j) ) THEN
               Yout(i) = Ydata(j)
            ELSEIF ( Xin(i)<Xdata(j+1) ) THEN
               dx = Xdata(j+1) - Xdata(j)
               Yout(i) = m(j)/(6.0*dx)*(Xdata(j+1)-Xin(i))**3 + m(j+1)/(6.0*dx)*(Xin(i)-Xdata(j))**3 + (Xdata(j+1)-Xin(i))          &
                       & *(Ydata(j)/dx-m(j)/6.0*dx) + (Xin(i)-Xdata(j))*(Ydata(j+1)/dx-m(j+1)/6.0*dx)
            ELSEIF ( Xin(i)==Xdata(j+1) ) THEN
               Yout(i) = Ydata(j+1)
            ELSE
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         j = j + 1
         IF ( j>=Ndata ) j = 1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
         IF ( i<Nxy ) THEN
            i = i + 1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE alg15
