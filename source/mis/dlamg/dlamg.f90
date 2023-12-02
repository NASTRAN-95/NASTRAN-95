!*==dlamg.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dlamg(Input,Matout,Skj)
   USE c_amgmn
   USE c_blank
   USE c_dlcom
   USE c_packx
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
   njj = nj
!
!     READ IN NP,NSIZE,NTP,F
!
   CALL read(*100,*100,Input,np,4,0,n)
!
!     COMPUTE POINTERS AND SEE IF THERE IS ENOUGH CORE
!
   ecore = korsz(iz)
   ecore = ecore - 4*sysbuf
   inc = 1
   inb = inc + np
   iys = inb + np
   izs = iys + nstrip
   iee = izs + nstrip
   isg = iee + nstrip
   icg = isg + nstrip
   ixic = icg + nstrip
   idelx = ixic + ntp
   ixlam = idelx + ntp
   nread = ixlam + ntp
!     IDT IS A COMPLEX POINTER
!     THE MATRIX PACKED OUT IS NJ LONG STARTING AT DT
   idt = (nread+2)/2
   next = idt*2 + 2*nj + 1
!
!     FILL IN DATA
!
   IF ( next>ecore ) THEN
!
!     ERROR MESSAGES
!
!     NOT ENOUGH CORE
      CALL mesage(-8,0,name)
   ELSE
      nread = nread - 1
      CALL read(*100,*100,Input,work,nread,1,n)
!
!     CHECK FOR ENOUGH SCRATCH STORAGE
!
      n = inc + np - 1
      length = 1
!
!     PUT OUT SKJ
!
      iti = 1
      ito = 3
      ii = isk
      nsk = nsk + 2
      nn = nsk
      k = 0
      ks = 0
      nbxr = iz(inc+k)
      DO i = 1 , ntp
         a(1) = 2.0*work(iee+ks)*work(idelx+i-1)
         a(2) = (work(iee+ks)*work(idelx+i-1)**2)/2.0
         CALL pack(a,Skj,tskj)
         ii = ii + 2
         IF ( i/=ntp ) THEN
            nn = nn + 2
            IF ( i==iz(inb+k) ) k = k + 1
            IF ( i==nbxr ) THEN
               ks = ks + 1
               nbxr = nbxr + iz(inc+k)
            ENDIF
         ENDIF
      ENDDO
      isk = ii
      nsk = nn
      iti = 3
      ito = 3
      ii = 1
      nn = nj
      CALL gend(work(inc),work(inb),work(iys),work(izs),work(isg),work(icg),dt(idt),work(1),Matout)
      nrow = nrow + ntp
      RETURN
   ENDIF
!     INPUT NOT POSITIONED PROPERLY OR INCORRECTLY WRITTEN
 100  CALL mesage(-7,0,name)
END SUBROUTINE dlamg
