
SUBROUTINE mbcap(Nphi,Capphi)
   IMPLICIT NONE
   REAL Asym , Beta , Boxa , Boxl , Boxw , Cntrl1 , Cntrl2 , Cr , Crank1 , Crank2 , Ek , Ekbar , Ekm , Gc , Kbar , Km , Mach
   INTEGER Kc , Kc1 , Kc1t , Kc2 , Kc2t , Kct , Nbox , Ncb , Njj , Npts0 , Npts1 , Npts2 , Nsb , Nsbd , Ntote
   COMMON /mboxc / Njj , Crank1 , Crank2 , Cntrl1 , Cntrl2 , Nbox , Npts0 , Npts1 , Npts2 , Asym , Gc , Cr , Mach , Beta , Ek ,     &
                 & Ekbar , Ekm , Boxl , Boxw , Boxa , Ncb , Nsb , Nsbd , Ntote , Kc , Kc1 , Kc2 , Kct , Kc1t , Kc2t
   INTEGER Nphi
   COMPLEX Capphi(1)
   REAL arg , arg1 , p(10) , w(10) , x , xb , xl , xr , xu
   REAL go , zj
   INTEGER i , j , l
!
   !>>>>EQUIVALENCE (Km,Ekm) , (Kbar,Ekbar)
   DATA w/0.0506143 , 0.1111905 , 0.1568533 , 0.1813419 , 0.1813419 , 0.1568533 , 0.1111905 , 0.0506143 , 0.0 , 0.0/ , p/0.0198551 ,&
      & 0.1016667 , 0.2372338 , 0.4082826 , 0.5917174 , 0.7627662 , 0.8983333 , 0.9801449 , 0.0 , 0.0/
!
   DO i = 1 , Nphi
      Capphi(i) = (0.0,0.0)
   ENDDO
!
!     COMPUTE CAPPHI FOR RECEIVING BOX
!
   IF ( Kbar<=0.0 ) THEN
!
      Capphi(1) = (-0.5,0.0)
   ELSE
      DO i = 1 , 8
         j = 9 - i
         arg = Kbar*p(j)/2.0
         arg1 = w(i)*zj(arg/Mach)/2.0
         Capphi(1) = Capphi(1) + cmplx(-cos(arg)*arg1,sin(arg)*arg1)
      ENDDO
   ENDIF
!
!     COMPUTE REMAINING CAPPHI
!
   Nphi = 1
   xb = 0.5
   xu = xb + 1.0
   DO i = 2 , Ncb
      xl = -0.5
      xr = xl + 1.0
      DO j = 1 , i
         Nphi = Nphi + 1
         DO l = 1 , 8
            x = xb + p(l)
            arg = Kbar*x
            arg1 = w(l)*go(x,xr,xl,Km)/3.14159265
            Capphi(Nphi) = Capphi(Nphi) - cmplx(cos(arg)*arg1,-sin(arg)*arg1)
         ENDDO
         xl = xr
         xr = xr + 1.0
      ENDDO
!
      xb = xu
      xu = xb + 1.0
   ENDDO
!
   DO i = 1 , Nphi
      Capphi(i) = Boxw*Capphi(i)
   ENDDO
END SUBROUTINE mbcap