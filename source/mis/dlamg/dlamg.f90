!*==dlamg.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dlamg(Input,Matout,Skj)
   IMPLICIT NONE
   USE C_AMGMN
   USE C_BLANK
   USE C_DLCOM
   USE C_PACKX
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   INTEGER :: Matout
   INTEGER :: Skj
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: a
   COMPLEX , DIMENSION(1) :: dt
   INTEGER :: i , k , ks , n , nbxr , nread
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL gend , korsz , mesage , pack , read
!
! End of declarations rewritten by SPAG
!
!
!     DRIVER FOR THE DOUBLET LATTICE METHOD
!     COMPUTATIONS ARE FOR THE AJJL MATRIX
!
   !>>>>EQUIVALENCE (Work(1),Iz(1),Dt(1))
   DATA name/4HDLAM , 4HG   /
!
   Njj = Nj
!
!     READ IN NP,NSIZE,NTP,F
!
   CALL read(*100,*100,Input,Np,4,0,n)
!
!     COMPUTE POINTERS AND SEE IF THERE IS ENOUGH CORE
!
   Ecore = korsz(iz)
   Ecore = Ecore - 4*Sysbuf
   Inc = 1
   Inb = Inc + Np
   Iys = Inb + Np
   Izs = Iys + Nstrip
   Iee = Izs + Nstrip
   Isg = Iee + Nstrip
   Icg = Isg + Nstrip
   Ixic = Icg + Nstrip
   Idelx = Ixic + Ntp
   Ixlam = Idelx + Ntp
   nread = Ixlam + Ntp
!     IDT IS A COMPLEX POINTER
!     THE MATRIX PACKED OUT IS NJ LONG STARTING AT DT
   Idt = (nread+2)/2
   Next = Idt*2 + 2*Nj + 1
!
!     FILL IN DATA
!
   IF ( Next>Ecore ) THEN
!
!     ERROR MESSAGES
!
!     NOT ENOUGH CORE
      CALL mesage(-8,0,name)
   ELSE
      nread = nread - 1
      CALL read(*100,*100,Input,Work,nread,1,n)
!
!     CHECK FOR ENOUGH SCRATCH STORAGE
!
      n = Inc + Np - 1
      Length = 1
!
!     PUT OUT SKJ
!
      Iti = 1
      Ito = 3
      Ii = Isk
      Nsk = Nsk + 2
      Nn = Nsk
      k = 0
      ks = 0
      nbxr = iz(Inc+k)
      DO i = 1 , Ntp
         a(1) = 2.0*Work(Iee+ks)*Work(Idelx+i-1)
         a(2) = (Work(Iee+ks)*Work(Idelx+i-1)**2)/2.0
         CALL pack(a,Skj,Tskj)
         Ii = Ii + 2
         IF ( i/=Ntp ) THEN
            Nn = Nn + 2
            IF ( i==iz(Inb+k) ) k = k + 1
            IF ( i==nbxr ) THEN
               ks = ks + 1
               nbxr = nbxr + iz(Inc+k)
            ENDIF
         ENDIF
      ENDDO
      Isk = Ii
      Nsk = Nn
      Iti = 3
      Ito = 3
      Ii = 1
      Nn = Nj
      CALL gend(Work(Inc),Work(Inb),Work(Iys),Work(Izs),Work(Isg),Work(Icg),dt(Idt),Work(1),Matout)
      Nrow = Nrow + Ntp
      RETURN
   ENDIF
!     INPUT NOT POSITIONED PROPERLY OR INCORRECTLY WRITTEN
 100  CALL mesage(-7,0,name)
END SUBROUTINE dlamg
