!*==rzints.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION rzints(Ip,Iq,R,Z,Ngrids)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: rzints
   INTEGER :: Ip
   INTEGER :: Iq
   REAL , DIMENSION(4) :: R
   REAL , DIMENSION(4) :: Z
   INTEGER :: Ngrids
!
! Local variable declarations rewritten by SPAG
!
   REAL :: detj , drdeta , drdxi , dzdeta , dzdxi , rr , rrp , xint , zz , zzq
   REAL , DIMENSION(3) :: h , pt
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
      pt(1) = -.7745966692
      pt(2) = 0.
      pt(3) = -pt(1)
      h(1) = 5./9.
      h(2) = 8./9.
      h(3) = h(1)
      xint = 0.
      DO iii = 1 , npt
         DO jjj = 1 , npt
            rr = .25*((1.-pt(iii))*(1.-pt(jjj))*R(1)+(1.+pt(iii))*(1.-pt(jjj))*R(2)+(1.+pt(iii))*(1.+pt(jjj))*R(3)+(1.-pt(iii))     &
               & *(1.+pt(jjj))*R(4))
            zz = .25*((1.-pt(iii))*(1.-pt(jjj))*Z(1)+(1.+pt(iii))*(1.-pt(jjj))*Z(2)+(1.+pt(iii))*(1.+pt(jjj))*Z(3)+(1.-pt(iii))     &
               & *(1.+pt(jjj))*Z(4))
            rrp = rr**Ip
            zzq = zz**Iq
            drdxi = .25*((1.-pt(jjj))*(R(2)-R(1))+(1.+pt(jjj))*(R(3)-R(4)))
            dzdxi = .25*((1.-pt(jjj))*(Z(2)-Z(1))+(1.+pt(jjj))*(Z(3)-Z(4)))
            drdeta = .25*((1.-pt(iii))*(R(4)-R(1))+(1.+pt(iii))*(R(3)-R(2)))
            dzdeta = .25*((1.-pt(iii))*(Z(4)-Z(1))+(1.+pt(iii))*(Z(3)-Z(2)))
            detj = drdxi*dzdeta - dzdxi*drdeta
            detj = abs(detj)
            xint = xint + rrp*zzq*h(iii)*h(jjj)*detj
         ENDDO
      ENDDO
      rzints = xint
   ENDIF
END FUNCTION rzints