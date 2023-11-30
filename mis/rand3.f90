
SUBROUTINE rand3(F,S,Q,N)
   IMPLICIT NONE
   REAL Degra , Phi , Radeg , S4pisq , Twopi
   COMMON /condas/ Phi , Twopi , Radeg , Degra , S4pisq
   INTEGER N
   REAL R , Tau
   REAL F(1) , Q(2) , S(1)
   REAL a , alp , b , bta , df , fi , fi1 , fii1 , q1 , sum , sum1
   INTEGER check , i , name(2) , nn
!
!     COMPUTES  MEAN RESPONSE  Q
!
   DATA name/4HRAND , 4H3   /
!
!     F IS ARRAY OF FREQUENCIES
!     S IS ARRAY OF POWER SPECTRAL DENSITY FUNCTIONS
!     Q IS MEAN RESPONSE
!     N IS NUMBER OF FREQUENCIES
!
   sum1 = 0.0
   nn = N - 1
   sum = 0.0
   DO i = 1 , nn
      df = F(i+1) - F(i)
      sum = sum + (S(i)+S(i+1))*df
      fi = F(i)*F(i)
      fi1 = F(i+1)*F(i+1)
      fii1 = 2.*F(i)*F(i+1)
      alp = (3.*fi+fii1+fi1)/6.
      bta = (fi+fii1+3.*fi1)/6.
      sum1 = sum1 + (alp*S(i)+bta*S(i+1))*df
   ENDDO
   sum = sqrt(sum*0.5)
   sum1 = sqrt(sum1*.5)
   Q(1) = sum
   Q(2) = 0.0
   q1 = Q(1)
   IF ( q1/=0.0 ) Q(2) = sum1/q1
   check = 123456789
   RETURN
!
!     AUTOCORRALATION FUNCTION
!
   ENTRY rand4(F,S,Tau,R,N)
!     =========================
!
!     COMPUTES  AUTOCORRALATION FUNCTION R  AT TIME TAU
!     WHERE F,S AS ABOVE. IF TAU = 0.0  R = Q*Q
!
   IF ( check/=123456789 ) CALL mesage(-37,0,name)
   IF ( Tau==0.0 ) THEN
      R = q1*q1
   ELSE
      nn = N - 1
      a = 2.0*Phi*Tau
      b = 1.0/a
      sum = 0.0
      DO i = 1 , nn
         sum = sum + b*(S(i+1)-S(i))/(F(i+1)-F(i))*(cos(a*F(i+1))-cos(a*F(i))) + S(i+1)*sin(a*F(i+1)) - S(i)*sin(a*F(i))
      ENDDO
      sum = sum*b
      R = sum
   ENDIF
END SUBROUTINE rand3