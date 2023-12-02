!*==dlamby.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dlamby(Input,Matout,Skj)
   IMPLICIT NONE
   USE C_AMGMN
   USE C_BLANK
   USE C_DLBDY
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
   REAL :: beta
   INTEGER :: i , k , lnas , lnb , lnfl , lns , lnsb , lt1 , lt2 , n , n1
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , SAVE :: nhaero , nhcore , nhpoin
   EXTERNAL amgbfs , amgrod , amgsba , bug , fread , gendsb , korsz , mesage
!
! End of declarations rewritten by SPAG
!
!
!     DRIVER FOR DOUBLET LATTICE WITH BODIES
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
   DATA name/4HDLAM , 4HBY  /
   DATA nhaero , nhpoin , nhcore/4HAERO , 4HPOIN , 4HCORE/
!
   Scr1 = 301
   Scr2 = 302
   Scr3 = 303
   Scr4 = 304
   Scr5 = 305
!
!     GET CORE THEN SET POINTERS TO ACPT TABLE ARRAYS
!
   Ecore = korsz(iz) - 4*Sysbuf
!
!     READ LENGTHS OF ARRAYS
!
   CALL fread(Input,Nj1,13,0)
!
!     COMPUTE POINTERS TO OPEN CORE
!
   lns = Inc
   Inc = 1
   Ins = Inc
   Inb = Ins + Np
   Inas = Inb + Np
   Izin = Inas
   Iyin = Izin
   Inbea1 = Iyin + Np
   Inbea2 = Inbea1 + Nb
   Insbea = Inbea2 + Nb
   Izb = Insbea + Nb
   Iyb = Izb + Nb
   Iavr = Iyb + Nb
   Iarb = Iavr + Nb
   Infl = Iarb + Nb
   Ixle = Infl + Nb
   Ixte = Ixle + Nb
   Int121 = Ixte + Nb
   Int122 = Int121 + Nb
   Izs = Int122 + Nb
   n = 3*Np + 12*Nb
!
!     READ FIXED ARRAYS
!
   IF ( n>Ecore ) THEN
!
!     ERROR MESSAGES
!
      CALL mesage(-8,0,name)
   ELSE
      CALL fread(Input,iz,n,0)
!
!     GET LENGTHS OF VARIABLE ARRAYS, PANELS THEN BODIES
!
      lnas = 0
      IF ( Np/=0 ) THEN
         DO i = 1 , Np
            lnas = lnas + iz(Inas+i-1)
         ENDDO
      ENDIF
      lnb = 0
      lnsb = 0
      lnfl = 0
      lt1 = 0
      lt2 = 0
      DO i = 1 , Nb
         k = i - 1
         lnb = lnb + iz(Inbea1+k)
         lnsb = lnsb + iz(Insbea+k)
         lnfl = lnfl + iz(Infl+k)
         lt1 = lt1 + iz(Int121+k)
         lt2 = lt2 + iz(Int122+k)
      ENDDO
      Ntbe = Ntp + lnb
!
!     READ VARIABLE  ARRAYS AND SET POINTERS TO CORE
!
      Next = n + 1
      n = 2*Nb + 5*lns + 4*Ntp + 3*lnb + 4*lnsb + lnas + 2*lnfl + lt1 + lt2
      IF ( Next+n+4*Nj>=Ecore ) THEN
         CALL mesage(-8,0,name)
      ELSE
         CALL fread(Input,iz(Next),n,1)
         Next = Next + n + 1
         Iys = Izs + Nb + lns
         Ics = Iys
         Iee = Ics + Nb + lns
         Isg = Iee + lns
         Icg = Isg + lns
         Ixij = Icg
         Ix = Ixij + lns
         Idelx = Ix + Ntp + lnb
         Ixic = Idelx + Ntp + lnb
         Ixlam = Ixic + Ntp
         Ia0 = Ixlam + Ntp
         Ixis1 = Ia0 + lnsb
         Ixis2 = Ixis1 + lnsb
         Ia0p = Ixis2 + lnsb
         Iria = Ia0p + lnsb
         Inasb = Iria + lnb
         Ifla1 = Inasb + lnas
         Ifla2 = Ifla1 + lnfl
         Ith1a = Ifla2 + lnfl
         Ith2a = Ith1a + lt1
!
!     BUILD A MATRIX
!
         CALL bug(nhaero,100,Nd,5)
         CALL bug(nhpoin,100,Nj1,59)
         CALL bug(nhcore,100,Z,Next)
         n1 = Next
         n = Next + 2*Ntbe
         Next = Next + 4*Ntbe
         IF ( Nt0/=0 ) CALL gendsb(Z(Inc),Z(Inb),Z(Isg),Z(Icg),Z(Infl),Z(Inbea1),Z(Inbea2),Z(Ifla1),Z(Ifla2),Z(n1),Z(n1),Z(n))
         n = Ntzs + Ntys
         Next = n1
         beta = sqrt(1.0-Fmach**2)
         IF ( Nt0/=0 .AND. n/=0 ) CALL amgrod(Z(n1),beta)
         CALL amgsba(Matout,Z(Ia0),Z(Iarb),Z(Insbea),Z(n1),Z(Iyb),Z(Izb))
         Nrow = Nrow + Nj1
!
!     BUILD SKJ MATRIX BE SURE TO BUMP ISK NSK
!
         CALL amgbfs(Skj,Z(Iee),Z(Idelx),Z(Inc),Z(Inb),Z(Ixis2),Z(Ixis1),Z(Ia0),Z(Ia0p),Z(Insbea))
      ENDIF
   ENDIF
END SUBROUTINE dlamby
