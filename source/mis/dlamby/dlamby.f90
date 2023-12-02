!*==dlamby.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dlamby(Input,Matout,Skj)
   USE c_amgmn
   USE c_blank
   USE c_dlbdy
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
   scr1 = 301
   scr2 = 302
   scr3 = 303
   scr4 = 304
   scr5 = 305
!
!     GET CORE THEN SET POINTERS TO ACPT TABLE ARRAYS
!
   ecore = korsz(iz) - 4*sysbuf
!
!     READ LENGTHS OF ARRAYS
!
   CALL fread(Input,nj1,13,0)
!
!     COMPUTE POINTERS TO OPEN CORE
!
   lns = inc
   inc = 1
   ins = inc
   inb = ins + np
   inas = inb + np
   izin = inas
   iyin = izin
   inbea1 = iyin + np
   inbea2 = inbea1 + nb
   insbea = inbea2 + nb
   izb = insbea + nb
   iyb = izb + nb
   iavr = iyb + nb
   iarb = iavr + nb
   infl = iarb + nb
   ixle = infl + nb
   ixte = ixle + nb
   int121 = ixte + nb
   int122 = int121 + nb
   izs = int122 + nb
   n = 3*np + 12*nb
!
!     READ FIXED ARRAYS
!
   IF ( n>ecore ) THEN
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
      IF ( np/=0 ) THEN
         DO i = 1 , np
            lnas = lnas + iz(inas+i-1)
         ENDDO
      ENDIF
      lnb = 0
      lnsb = 0
      lnfl = 0
      lt1 = 0
      lt2 = 0
      DO i = 1 , nb
         k = i - 1
         lnb = lnb + iz(inbea1+k)
         lnsb = lnsb + iz(insbea+k)
         lnfl = lnfl + iz(infl+k)
         lt1 = lt1 + iz(int121+k)
         lt2 = lt2 + iz(int122+k)
      ENDDO
      ntbe = ntp + lnb
!
!     READ VARIABLE  ARRAYS AND SET POINTERS TO CORE
!
      next = n + 1
      n = 2*nb + 5*lns + 4*ntp + 3*lnb + 4*lnsb + lnas + 2*lnfl + lt1 + lt2
      IF ( next+n+4*nj>=ecore ) THEN
         CALL mesage(-8,0,name)
      ELSE
         CALL fread(Input,iz(next),n,1)
         next = next + n + 1
         iys = izs + nb + lns
         ics = iys
         iee = ics + nb + lns
         isg = iee + lns
         icg = isg + lns
         ixij = icg
         ix = ixij + lns
         idelx = ix + ntp + lnb
         ixic = idelx + ntp + lnb
         ixlam = ixic + ntp
         ia0 = ixlam + ntp
         ixis1 = ia0 + lnsb
         ixis2 = ixis1 + lnsb
         ia0p = ixis2 + lnsb
         iria = ia0p + lnsb
         inasb = iria + lnb
         ifla1 = inasb + lnas
         ifla2 = ifla1 + lnfl
         ith1a = ifla2 + lnfl
         ith2a = ith1a + lt1
!
!     BUILD A MATRIX
!
         CALL bug(nhaero,100,nd,5)
         CALL bug(nhpoin,100,nj1,59)
         CALL bug(nhcore,100,z,next)
         n1 = next
         n = next + 2*ntbe
         next = next + 4*ntbe
         IF ( nt0/=0 ) CALL gendsb(z(inc),z(inb),z(isg),z(icg),z(infl),z(inbea1),z(inbea2),z(ifla1),z(ifla2),z(n1),z(n1),z(n))
         n = ntzs + ntys
         next = n1
         beta = sqrt(1.0-fmach**2)
         IF ( nt0/=0 .AND. n/=0 ) CALL amgrod(z(n1),beta)
         CALL amgsba(Matout,z(ia0),z(iarb),z(insbea),z(n1),z(iyb),z(izb))
         nrow = nrow + nj1
!
!     BUILD SKJ MATRIX BE SURE TO BUMP ISK NSK
!
         CALL amgbfs(Skj,z(iee),z(idelx),z(inc),z(inb),z(ixis2),z(ixis1),z(ia0),z(ia0p),z(insbea))
      ENDIF
   ENDIF
END SUBROUTINE dlamby
