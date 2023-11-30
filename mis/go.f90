
FUNCTION go(R,Etar,Etal,Ekm)
   IMPLICIT NONE
   REAL Ekm , Etal , Etar , R
   REAL go
   REAL arg , as(2) , bsl(23) , c(2) , dbslj , f , fi , s(2) , s0(2) , s4
   INTEGER i , j , n
!
!
   dbslj = 1.0E-10
   s(1) = Etar
   s(2) = Etal
   DO i = 1 , 2
      IF ( abs(s(i))>=R ) THEN
!
         as(i) = sign(1.570796,s(i))
         s(i) = 0.0
      ELSE
         s(i) = s(i)/R
         c(i) = sqrt(1.0-s(i)**2)
         as(i) = 2.0*atan(s(i)/(1.0+c(i)))
         s(i) = 2.0*s(i)*c(i)
         c(i) = 2.0*c(i)**2 - 1.0
      ENDIF
!
      s0(i) = 0.0
   ENDDO
!
   go = as(1) - as(2)
   IF ( abs(go)<=dbslj ) THEN
!
      go = 0.0
      GOTO 99999
   ENDIF
!
   arg = Ekm*R
   IF ( arg==0.0 ) RETURN
   CALL mbbslj(arg,n,bsl)
!
   go = bsl(1)*go
   f = 1.0
   fi = 1.0
   DO j = 2 , n
      go = bsl(j)*(s(1)-s(2))/fi - go
!
      DO i = 1 , 2
         s4 = 2.0*s(i)*c(i) - s0(i)
         s0(i) = s(i)
         s(i) = s4
      ENDDO
!
      f = -f
      fi = fi + 1.0
   ENDDO
!
   IF ( f<0.0 ) go = -go
   RETURN
99999 RETURN
END FUNCTION go
