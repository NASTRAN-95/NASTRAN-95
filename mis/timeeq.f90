
SUBROUTINE timeeq(B,Bbar,C,Cbar,R,Ientry,Ncol,Tim)
   IMPLICIT NONE
   REAL Aaio , Aapak , Mb(1) , Mc(1) , Tcons(15)
   INTEGER Iprec , Ksystm(65) , Lntime , Nbpw , Sysbuf
   COMMON /ntime / Lntime , Tcons
   COMMON /system/ Ksystm
   REAL Ab , Abbar , Ac , Acbar , Ancol , Ar , B , Bbar , C , Cbar , R , Tim , Timex
   INTEGER Ib , Ibbar , Ic , Icbar , Ientry , Incol , Ir , Jentry , Kentry , Ncol , Nx
   REAL aio , amb , amc , apak , k1 , k2 , k3 , k4 , k5
   INTEGER ientr , iret
!
!     TIMEEQ SOLVES THE TIME AND CORE FUNCTIONS FOR DECOMP AND CDCOMP
!
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(40),Nbpw) , (Ksystm(55),Iprec) , (Tcons(1),Aaio) , (Tcons(2),Aapak) , (Tcons(8),Mb(1)) ,&
!>>>>    & (Tcons(12),Mc(1))
!
!
   ientr = Ientry
   amb = Mb(Iprec)
   amc = Mc(Iprec)
   IF ( Nbpw>=60 ) THEN
      amb = 3.0*amb
      amc = 3.0*amc
   ENDIF
   aio = Aaio
   apak = Aapak
   IF ( ientr/=1 ) THEN
      amb = 5.*amb
      amc = 5.*amc
      aio = aio + aio
      apak = 1.1*apak
   ENDIF
   Tim = float(Ncol)*(amb*Bbar*R+amc*(Bbar*C+Bbar*Cbar+B*Cbar+2.0*C*Cbar)+aio*Bbar*(B+Bbar-R-1.0))*1.E-06
   RETURN
END SUBROUTINE timeeq
!
!
SUBROUTINE tfin(Ab,Abbar,Ac,Acbar,Ar,Jentry,Ancol,Timex)
   IMPLICIT NONE
   REAL Aaio , Aapak , Mb(1) , Mc(1) , Tcons(15)
   INTEGER Iprec , Ksystm(65) , Lntime , Nbpw , Sysbuf
   COMMON /ntime / Lntime , Tcons
   COMMON /system/ Ksystm
   REAL Ab , Abbar , Ac , Acbar , Ancol , Ar , B , Bbar , C , Cbar , R , Tim , Timex
   INTEGER Ib , Ibbar , Ic , Icbar , Ientry , Incol , Ir , Jentry , Kentry , Ncol , Nx
   REAL aio , amb , amc , apak , k1 , k2 , k3 , k4 , k5
   INTEGER ientr , iret
!
!     TIMEEQ SOLVES THE TIME AND CORE FUNCTIONS FOR DECOMP AND CDCOMP
!
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(40),Nbpw) , (Ksystm(55),Iprec) , (Tcons(1),Aaio) , (Tcons(2),Aapak) , (Tcons(8),Mb(1)) ,&
!>>>>    & (Tcons(12),Mc(1))
!
!
   ientr = Jentry
   amb = Mb(Iprec)
   amc = Mc(Iprec)
   IF ( Nbpw>=60 ) THEN
      amb = 3.0*amb
      amc = 3.0*amc
   ENDIF
   aio = Aaio
   apak = Aapak
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
      RETURN
END SUBROUTINE tfin
!
!
SUBROUTINE rcore(Ib,Ibbar,Ic,Icbar,Incol,Kentry,Nx,Ir)
   IMPLICIT NONE
   REAL Aaio , Aapak , Mb(1) , Mc(1) , Tcons(15)
   INTEGER Iprec , Ksystm(65) , Lntime , Nbpw , Sysbuf
   COMMON /ntime / Lntime , Tcons
   COMMON /system/ Ksystm
   REAL Ab , Abbar , Ac , Acbar , Ancol , Ar , B , Bbar , C , Cbar , R , Tim , Timex
   INTEGER Ib , Ibbar , Ic , Icbar , Ientry , Incol , Ir , Jentry , Kentry , Ncol , Nx
   REAL aio , amb , amc , apak , k1 , k2 , k3 , k4 , k5
   INTEGER ientr , iret
!
!     ENTRY FOR THE CORE FUNCTION
!
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(40),Nbpw) , (Ksystm(55),Iprec) , (Tcons(1),Aaio) , (Tcons(2),Aapak) , (Tcons(8),Mb(1)) ,&
!>>>>    & (Tcons(12),Mc(1))
!
!
   Ir = (Nx-((Ib+Ibbar+1)+2*Kentry*min0(Incol,Ib+Ibbar+Ibbar)+2*Kentry*Ic*(Ibbar+2)+2*Icbar*Kentry*(min0(Ib+Ibbar,Incol)+1)         &
      & +2*Kentry*Ic*Icbar+Ic+Icbar*Kentry+Icbar)-6*Sysbuf)/(2*Kentry*Ibbar)
END SUBROUTINE rcore
