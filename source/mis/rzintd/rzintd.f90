!*==rzintd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION rzintd(Ip,Iq,R,Z,Ngrids)
USE iso_fortran_env
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) :: rzintd
   INTEGER :: Ip
   INTEGER :: Iq
   REAL(REAL64) , DIMENSION(4) :: R
   REAL(REAL64) , DIMENSION(4) :: Z
   INTEGER :: Ngrids
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: detj , drdeta , drdxi , dzdeta , dzdxi , rr , rrp , xint , zz , zzq
   REAL(REAL64) , DIMENSION(3) :: h , pt
   INTEGER :: iii , jjj , npt
!
! End of declarations rewritten by SPAG
!
!
! Function and Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
   IF ( Ngrids/=3 ) THEN
      npt = 3
      pt(1) = -.7745966692D0
      pt(2) = 0.D0
      pt(3) = -pt(1)
      h(1) = 5.D0/9.D0
      h(2) = 8.D0/9.D0
      h(3) = h(1)
      xint = 0.D0
      DO iii = 1 , npt
         DO jjj = 1 , npt
            rr = .25D0*((1.D0-pt(iii))*(1.D0-pt(jjj))*R(1)+(1.D0+pt(iii))*(1.D0-pt(jjj))*R(2)+(1.D0+pt(iii))*(1.D0+pt(jjj))*R(3)    &
               & +(1.D0-pt(iii))*(1.D0+pt(jjj))*R(4))
            zz = .25D0*((1.D0-pt(iii))*(1.D0-pt(jjj))*Z(1)+(1.D0+pt(iii))*(1.D0-pt(jjj))*Z(2)+(1.D0+pt(iii))*(1.D0+pt(jjj))*Z(3)    &
               & +(1.D0-pt(iii))*(1.D0+pt(jjj))*Z(4))
            rrp = rr**Ip
            zzq = zz**Iq
            drdxi = .25D0*((1.D0-pt(jjj))*(R(2)-R(1))+(1.D0+pt(jjj))*(R(3)-R(4)))
            dzdxi = .25D0*((1.D0-pt(jjj))*(Z(2)-Z(1))+(1.D0+pt(jjj))*(Z(3)-Z(4)))
            drdeta = .25D0*((1.D0-pt(iii))*(R(4)-R(1))+(1.D0+pt(iii))*(R(3)-R(2)))
            dzdeta = .25D0*((1.D0-pt(iii))*(Z(4)-Z(1))+(1.D0+pt(iii))*(Z(3)-Z(2)))
            detj = drdxi*dzdeta - dzdxi*drdeta
            detj = dabs(detj)
            xint = xint + rrp*zzq*h(iii)*h(jjj)*detj
         ENDDO
      ENDDO
      rzintd = xint
   ENDIF
END FUNCTION rzintd
