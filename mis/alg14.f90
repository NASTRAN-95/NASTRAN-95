
SUBROUTINE alg14(Xdata,Ydata,Ndata,Xin,Yout,Yprime,Nxy,Nwot)
   IMPLICIT NONE
   INTEGER Ndata , Nwot , Nxy
   REAL Xdata(2) , Xin(1) , Ydata(2) , Yout(1) , Yprime(1)
   REAL a(65) , b(65) , d(65) , dx , e1 , e2 , m(65) , ydash
   INTEGER i , ii , j , n
!
!     THIS SPLINE ROUTINE DETERMINES Y AND/OR YPRIME  LINEAR EXTRAPOLATI
!     XDATA AND XIN MUST BE IN ASCENDING ORDER  E1 AND E2 ARE D2YDX2 LAS
!     D2YDX2 LAST-BUT-ONE AT ENDS OF SPECIFIED REGION OF BEAM
!
!
!
   IF ( Ndata<2 ) THEN
   ELSEIF ( Ndata==2 ) THEN
      IF ( Nwot/=1 ) THEN
         DO i = 1 , Nxy
            Yout(i) = ((Ydata(2)-Ydata(1))/(Xdata(2)-Xdata(1)))*(Xin(i)-Xdata(1)) + Ydata(1)
         ENDDO
      ENDIF
      IF ( Nwot>0 ) THEN
         DO i = 1 , Nxy
            Yprime(i) = (Ydata(2)-Ydata(1))/(Xdata(2)-Xdata(1))
         ENDDO
      ENDIF
   ELSE
      e1 = 1.0
      e2 = 1.0
      a(1) = 1.0
      b(1) = -e1
      d(1) = 0.0
      n = Ndata - 1
      DO i = 2 , n
         a(i) = (Xdata(i+1)-Xdata(i-1))/3.0 - (Xdata(i)-Xdata(i-1))*b(i-1)/(6.0*a(i-1))
         b(i) = (Xdata(i+1)-Xdata(i))/6.0
         d(i) = (Ydata(i+1)-Ydata(i))/(Xdata(i+1)-Xdata(i)) - (Ydata(i)-Ydata(i-1))/(Xdata(i)-Xdata(i-1)) - (Xdata(i)-Xdata(i-1))   &
              & *d(i-1)/6.0/a(i-1)
      ENDDO
      a(Ndata) = -e2
      b(Ndata) = 1.0
      d(Ndata) = 0.0
      m(Ndata) = a(Ndata)*d(n)/(a(Ndata)*b(n)-a(n)*b(Ndata))
      DO ii = 2 , Ndata
         i = Ndata + 1 - ii
         m(i) = (d(i)-b(i)*m(i+1))/a(i)
      ENDDO
      j = 1
      i = 1
      GOTO 100
   ENDIF
   GOTO 99999
 100  DO
      IF ( Xin(i)<=Xdata(1) ) THEN
         ydash = (Ydata(2)-Ydata(1))/(Xdata(2)-Xdata(1)) - (m(1)/3.0+m(2)/6.0)*(Xdata(2)-Xdata(1))
         IF ( Nwot/=1 ) THEN
            Yout(i) = Ydata(1) - ydash*(Xdata(1)-Xin(i))
            IF ( Nwot/=0 ) Yprime(i) = ydash
         ELSE
            Yprime(i) = ydash
         ENDIF
      ELSE
         DO WHILE ( Xin(i)>Xdata(j+1) )
            IF ( j+1>=Ndata ) EXIT
            j = j + 1
         ENDDO
         IF ( Xin(i)<Xdata(Ndata) ) THEN
            dx = Xdata(j+1) - Xdata(j)
            IF ( Nwot/=1 ) THEN
               Yout(i) = m(j)/(6.0*dx)*(Xdata(j+1)-Xin(i))**3 + m(j+1)/(6.0*dx)*(Xin(i)-Xdata(j))**3 + (Xdata(j+1)-Xin(i))          &
                       & *(Ydata(j)/dx-m(j)/6.0*dx) + (Xin(i)-Xdata(j))*(Ydata(j+1)/dx-m(j+1)/6.0*dx)
               IF ( Nwot/=0 ) Yprime(i) = (-m(j)*(Xdata(j+1)-Xin(i))**2/2.0+m(j+1)*(Xin(i)-Xdata(j))**2/2.0+Ydata(j+1)-Ydata(j))    &
                  & /dx - (m(j+1)-m(j))/6.0*dx
            ELSE
               Yprime(i) = (-m(j)*(Xdata(j+1)-Xin(i))**2/2.0+m(j+1)*(Xin(i)-Xdata(j))**2/2.0+Ydata(j+1)-Ydata(j))/dx - (m(j+1)-m(j))&
                         & /6.0*dx
            ENDIF
         ELSE
            ydash = (Ydata(Ndata)-Ydata(n))/(Xdata(Ndata)-Xdata(n)) + (m(Ndata)/3.0+m(n)/6.0)*(Xdata(Ndata)-Xdata(n))
            IF ( Nwot/=1 ) THEN
               Yout(i) = Ydata(Ndata) + ydash*(Xin(i)-Xdata(Ndata))
               IF ( Nwot/=0 ) Yprime(i) = ydash
            ELSE
               Yprime(i) = ydash
            ENDIF
         ENDIF
      ENDIF
      i = i + 1
      IF ( i>Nxy ) EXIT
   ENDDO
99999 RETURN
END SUBROUTINE alg14