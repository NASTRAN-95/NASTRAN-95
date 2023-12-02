!*==dki.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION dki(I,J,K,L,M,N,Ip,Iq,R,Z)
USE iso_fortran_env
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) :: dki
   INTEGER :: I
   INTEGER :: J
   INTEGER :: K
   INTEGER :: L
   INTEGER :: M
   INTEGER :: N
   INTEGER :: Ip
   INTEGER :: Iq
   REAL(REAL64) , DIMENSION(1) :: R
   REAL(REAL64) , DIMENSION(1) :: Z
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: abs1 , ai , akkl , akmn , amkl , amm , ammn , arr , rd , xx
   INTEGER :: irr , iss , mm , nn
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
   IF ( R(I)==R(J) ) THEN
      ai = 0.0D0
      dki = ai
   ELSE
      rd = R(J)
      IF ( R(J)==0.0D0 ) rd = R(I)
      abs1 = dabs((R(I)-R(J))/rd)
      IF ( abs1<=0.1D-3 ) THEN
         ai = 0.0D0
         dki = ai
      ELSE
         amkl = (R(L)*Z(K)-R(K)*Z(L))/(R(L)-R(K))
         akkl = (Z(L)-Z(K))/(R(L)-R(K))
         ammn = (R(N)*Z(M)-R(M)*Z(N))/(R(N)-R(M))
         akmn = (Z(N)-Z(M))/(R(N)-R(M))
         IF ( akmn/=akkl .OR. ammn/=amkl ) THEN
            iss = iabs(Ip)
            irr = iabs(Iq)
            IF ( Iq+1<0 ) THEN
               IF ( Ip<0 ) THEN
                  mm = iss
                  nn = irr - 1
                  ai = dk100(I,amkl,akkl,mm,nn,R) - dk100(I,ammn,akmn,mm,nn,R) - dk100(J,amkl,akkl,mm,nn,R)                         &
                     & + dk100(J,ammn,akmn,mm,nn,R)
                  arr = irr
                  ai = (1.0D0/(1.0D0-arr))*ai
               ELSE
                  mm = Ip
                  nn = irr - 1
                  ai = dk89(I,amkl,akkl,mm,nn,R) - dk89(I,ammn,akmn,mm,nn,R) - dk89(J,amkl,akkl,mm,nn,R) + dk89(J,ammn,akmn,mm,nn,R)
                  arr = irr
                  ai = (1.0D0/(1.0D0-arr))*ai
               ENDIF
            ELSEIF ( Iq+1==0 ) THEN
               IF ( Ip+1<0 ) THEN
                  mm = iss - 1
                  amm = mm
                  xx = amm*R(I)**mm
                  ai = (-dlog(dabs(amkl+akkl*R(I)))/xx+akkl/amm*dk100(I,amkl,akkl,mm,1,R)+dlog(dabs(ammn+akmn*R(I)))                &
                     & /xx-akmn/amm*dk100(I,ammn,akmn,mm,1,R))
                  xx = amm*R(J)**M
                  ai = (+dlog(dabs(amkl+akkl*R(J)))/xx-akkl/amm*dk100(J,amkl,akkl,mm,1,R)-dlog(dabs(ammn+akmn*R(J)))                &
                     & /xx+akmn/amm*dk100(J,ammn,akmn,mm,1,R)) + ai
               ELSEIF ( Ip+1==0 ) THEN
                  ai = dk211(I,amkl,akkl,R) - dk211(I,ammn,akmn,R) - dk211(J,amkl,akkl,R) + dk211(J,ammn,akmn,R)
               ELSE
                  mm = Ip + 1
                  amm = mm
                  xx = R(I)**mm/amm
                  ai = (+xx*dlog(dabs(amkl+akkl*R(I)))-akkl/amm*dk89(I,amkl,akkl,mm,1,R)-xx*dlog(dabs(ammn+akmn*R(I)))              &
                     & +akmn/amm*dk89(I,ammn,akmn,mm,1,R))
                  xx = R(J)**mm/amm
                  ai = (-xx*dlog(dabs(amkl+akkl*R(J)))+akkl/amm*dk89(J,amkl,akkl,mm,1,R)+xx*dlog(dabs(ammn+akmn*R(J)))              &
                     & -akmn/amm*dk89(J,ammn,akmn,mm,1,R)) + ai
               ENDIF
            ELSE
               mm = Ip
               nn = Iq + 1
               ai = dkint(I,J,ammn,akmn,mm,nn,R,Z) - dkint(I,J,amkl,akkl,mm,nn,R,Z)
            ENDIF
            dki = ai
         ELSE
            ai = 0.0D0
            dki = ai
         ENDIF
      ENDIF
   ENDIF
END FUNCTION dki
