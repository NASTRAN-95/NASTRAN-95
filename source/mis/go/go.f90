!*==go.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION go(R,Etar,Etal,Ekm)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: go
   REAL :: R
   REAL :: Etar
   REAL :: Etal
   REAL :: Ekm
!
! Local variable declarations rewritten by SPAG
!
   REAL :: arg , dbslj , f , fi , s4
   REAL , DIMENSION(2) :: as , c , s , s0
   REAL , DIMENSION(23) :: bsl
   INTEGER :: i , j , n
   EXTERNAL mbbslj
!
! End of declarations rewritten by SPAG
!
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
      RETURN
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
END FUNCTION go
