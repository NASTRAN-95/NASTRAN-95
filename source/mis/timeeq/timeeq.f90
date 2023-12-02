!*==timeeq.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE timeeq(B,Bbar,C,Cbar,R,Ientry,Ncol,Tim)
   USE c_ntime
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: B
   REAL :: Bbar
   REAL :: C
   REAL :: Cbar
   REAL :: R
   INTEGER :: Ientry
   INTEGER :: Ncol
   REAL :: Tim
!
! Local variable declarations rewritten by SPAG
!
   REAL :: aaio , aapak , ab , abbar , ac , acbar , aio , amb , amc , ancol , apak , ar , k1 , k2 , k3 , k4 , k5 , timex
   INTEGER :: ib , ibbar , ic , icbar , ientr , incol , iprec , ir , iret , jentry , kentry , nbpw , nx , sysbuf
   REAL , DIMENSION(1) :: mb , mc
!
! End of declarations rewritten by SPAG
!
!
!     TIMEEQ SOLVES THE TIME AND CORE FUNCTIONS FOR DECOMP AND CDCOMP
!
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(40),Nbpw) , (Ksystm(55),Iprec) , (Tcons(1),Aaio) , (Tcons(2),Aapak) , (Tcons(8),Mb(1)) ,&
!>>>>    & (Tcons(12),Mc(1))
!
!
   ientr = Ientry
   amb = mb(iprec)
   amc = mc(iprec)
   IF ( nbpw>=60 ) THEN
      amb = 3.0*amb
      amc = 3.0*amc
   ENDIF
   aio = aaio
   apak = aapak
   IF ( ientr/=1 ) THEN
      amb = 5.*amb
      amc = 5.*amc
      aio = aio + aio
      apak = 1.1*apak
   ENDIF
   Tim = float(Ncol)*(amb*Bbar*R+amc*(Bbar*C+Bbar*Cbar+B*Cbar+2.0*C*Cbar)+aio*Bbar*(B+Bbar-R-1.0))*1.E-06
END SUBROUTINE timeeq
!*==tfin.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!
!
SUBROUTINE tfin(Ab,Abbar,Ac,Acbar,Ar,Jentry,Ancol,Timex)
   USE c_ntime
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Ab
   REAL :: Abbar
   REAL :: Ac
   REAL :: Acbar
   REAL :: Ar
   INTEGER :: Jentry
   REAL :: Ancol
   REAL :: Timex
!
! Local variable declarations rewritten by SPAG
!
   REAL :: aaio , aapak , aio , amb , amc , apak , b , bbar , c , cbar , k1 , k2 , k3 , k4 , k5 , r , tim
   INTEGER :: ib , ibbar , ic , icbar , ientr , ientry , incol , iprec , ir , iret , kentry , nbpw , ncol , nx , sysbuf
   REAL , DIMENSION(1) :: mb , mc
!
! End of declarations rewritten by SPAG
!
!
!     TIMEEQ SOLVES THE TIME AND CORE FUNCTIONS FOR DECOMP AND CDCOMP
!
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(40),Nbpw) , (Ksystm(55),Iprec) , (Tcons(1),Aaio) , (Tcons(2),Aapak) , (Tcons(8),Mb(1)) ,&
!>>>>    & (Tcons(12),Mc(1))
!
!
   ientr = Jentry
   amb = mb(iprec)
   amc = mc(iprec)
   IF ( nbpw>=60 ) THEN
      amb = 3.0*amb
      amc = 3.0*amc
   ENDIF
   aio = aaio
   apak = aapak
   IF ( ientr/=1 ) THEN
      amb = 5.*amb
      amc = 5.*amc
      aio = aio + aio
      apak = 1.1*apak
   ENDIF
   Timex = 0.
   k1 = Ancol - Ab - Abbar - Abbar
   IF ( k1>0. ) Timex = k1*(amb*Abbar*Ar+aio*Abbar*(Ab+Abbar-Ar)+apak*(Ab+Abbar*2.))
   k2 = Ab + Abbar
   k3 = k2
   IF ( Ancol<Ab+Abbar+Abbar ) THEN
      k2 = Ancol - Abbar
      k3 = Ab + Abbar
      IF ( Ancol<Ab+Abbar ) k3 = Ancol
   ENDIF
   Timex = Timex + .5*k2*(Abbar*k2*amb+(k3-Ar)*(aio-amb)*Abbar+2.*apak*Abbar+apak*k2)
   IF ( Ancol<Ab+Abbar+Abbar ) THEN
      k4 = Ancol - Ar
      k5 = Ancol
      IF ( Ancol-Ar>Abbar ) k4 = Abbar
   ELSE
      k4 = Ab + Abbar - Ar
      k5 = Ab + 1.5*Abbar
      IF ( Ab>Ar ) k4 = Abbar
   ENDIF
   Timex = Timex + Abbar**3/3.*amb + k4**3*.5*aio + apak*Abbar*k5
   Timex = (Timex+(Ancol-Abbar)*(amc*(Abbar*Ac+Ab*Acbar+Abbar*Acbar+Ac*Acbar)+apak*(Ac+Acbar)))*1.E-06
END SUBROUTINE tfin
!*==rcore.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!
!
SUBROUTINE rcore(Ib,Ibbar,Ic,Icbar,Incol,Kentry,Nx,Ir)
   USE c_ntime
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ib
   INTEGER :: Ibbar
   INTEGER :: Ic
   INTEGER :: Icbar
   INTEGER :: Incol
   INTEGER :: Kentry
   INTEGER :: Nx
   INTEGER :: Ir
!
! Local variable declarations rewritten by SPAG
!
   REAL :: aaio , aapak , ab , abbar , ac , acbar , aio , amb , amc , ancol , apak , ar , b , bbar , c , cbar , k1 , k2 , k3 , k4 , &
         & k5 , r , tim , timex
   INTEGER :: ientr , ientry , iprec , iret , jentry , nbpw , ncol , sysbuf
   REAL , DIMENSION(1) :: mb , mc
!
! End of declarations rewritten by SPAG
!
!
!     ENTRY FOR THE CORE FUNCTION
!
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(40),Nbpw) , (Ksystm(55),Iprec) , (Tcons(1),Aaio) , (Tcons(2),Aapak) , (Tcons(8),Mb(1)) ,&
!>>>>    & (Tcons(12),Mc(1))
!
!
   Ir = (Nx-((Ib+Ibbar+1)+2*Kentry*min0(Incol,Ib+Ibbar+Ibbar)+2*Kentry*Ic*(Ibbar+2)+2*Icbar*Kentry*(min0(Ib+Ibbar,Incol)+1)         &
      & +2*Kentry*Ic*Icbar+Ic+Icbar*Kentry+Icbar)-6*sysbuf)/(2*Kentry*Ibbar)
END SUBROUTINE rcore
