
SUBROUTINE alg15(Xdata,Ydata,Ndata,Xin,Yout,Nxy,Ntype)
   IMPLICIT NONE
   INTEGER Ndata , Ntype , Nxy
   REAL Xdata(2) , Xin(1) , Ydata(2) , Yout(1)
   REAL a(21) , b(21) , d(21) , dx , m(21) , yprime
   INTEGER i , ii , j , jp , kp , n
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
               d(i) = (Ydata(i+1)-Ydata(i))/(Xdata(i+1)-Xdata(i)) - (Ydata(i)-Ydata(i-1))/(Xdata(i)-Xdata(i-1))                     &
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
            GOTO 400
         ENDIF
      ENDIF
      j = 1
      i = 1
   ENDIF
 100  IF ( Xin(i)<=Xdata(2) ) THEN
      Yout(i) = Ydata(1) + (Ydata(2)-Ydata(1))/(Xdata(2)-Xdata(1))*(Xin(i)-Xdata(1))
      GOTO 300
   ELSEIF ( Xin(i)>=Xdata(Ndata-1) ) THEN
      Yout(i) = Ydata(Ndata-1) + (Ydata(Ndata)-Ydata(Ndata-1))/(Xdata(Ndata)-Xdata(Ndata-1))*(Xin(i)-Xdata(Ndata-1))
      GOTO 300
   ENDIF
 200  IF ( Xin(i)<Xdata(j) ) THEN
   ELSEIF ( Xin(i)==Xdata(j) ) THEN
      Yout(i) = Ydata(j) + (Ydata(j+1)-Ydata(j))/(Xdata(j+1)-Xdata(j))*(Xin(i)-Xdata(j))
      GOTO 300
   ELSEIF ( Xin(i)<=Xdata(j+1) ) THEN
      Yout(i) = Ydata(j) + (Ydata(j+1)-Ydata(j))/(Xdata(j+1)-Xdata(j))*(Xin(i)-Xdata(j))
      GOTO 300
   ENDIF
   j = j + 1
   IF ( j>=Ndata ) j = 1
   GOTO 200
 300  IF ( i<Nxy ) THEN
      i = i + 1
      GOTO 100
   ELSE
      RETURN
   ENDIF
 400  IF ( Xin(i)<Xdata(1) ) THEN
      jp = 1
      kp = 2
   ELSEIF ( Xin(i)==Xdata(1) ) THEN
      Yout(i) = Ydata(1)
      GOTO 700
   ELSE
      IF ( Xin(i)<Xdata(Ndata) ) GOTO 500
      IF ( Xin(i)==Xdata(Ndata) ) THEN
         Yout(i) = Ydata(Ndata)
         GOTO 700
      ELSE
         jp = Ndata
         kp = Ndata - 1
      ENDIF
   ENDIF
   yprime = (Ydata(kp)-Ydata(jp))/(Xdata(kp)-Xdata(jp)) - m(kp)/6.0*(Xdata(kp)-Xdata(jp))
   Yout(i) = Ydata(jp) + (Xin(i)-Xdata(jp))*yprime
   GOTO 700
 500  IF ( Xin(i)<Xdata(j) ) GOTO 600
   IF ( Xin(i)==Xdata(j) ) THEN
      Yout(i) = Ydata(j)
   ELSEIF ( Xin(i)<Xdata(j+1) ) THEN
      dx = Xdata(j+1) - Xdata(j)
      Yout(i) = m(j)/(6.0*dx)*(Xdata(j+1)-Xin(i))**3 + m(j+1)/(6.0*dx)*(Xin(i)-Xdata(j))**3 + (Xdata(j+1)-Xin(i))                   &
              & *(Ydata(j)/dx-m(j)/6.0*dx) + (Xin(i)-Xdata(j))*(Ydata(j+1)/dx-m(j+1)/6.0*dx)
   ELSEIF ( Xin(i)==Xdata(j+1) ) THEN
      Yout(i) = Ydata(j+1)
   ELSE
      GOTO 600
   ENDIF
   GOTO 700
 600  j = j + 1
   IF ( j>=Ndata ) j = 1
   GOTO 500
 700  IF ( i<Nxy ) THEN
      i = i + 1
      GOTO 400
   ENDIF
END SUBROUTINE alg15
