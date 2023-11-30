
SUBROUTINE dlamby(Input,Matout,Skj)
   IMPLICIT NONE
   INTEGER Ecore , Ia0 , Ia0p , Iarb , Iavr , Icg , Ics , Idelx , Iee , Ifla1 , Ifla2 , Inas , Inasb , Inb , Inbea1 , Inbea2 , Inc ,&
         & Infl , Ins , Insbea , Int121 , Int122 , Iria , Isg , Isk , Ith1a , Ith2a , Ix , Ixic , Ixij , Ixis1 , Ixis2 , Ixlam ,    &
         & Ixle , Ixte , Iyb , Iyin , Iys , Iz(1) , Izb , Izin , Izs , Mcb(7) , Nb , Nby , Nbz , Nd , Ne , Next , Nj , Nj1 , Nk ,   &
         & Nk1 , Np , Nrow , Nsk , Nt0 , Ntbe , Ntp , Nty , Ntys , Ntz , Ntzs , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Sysbuf , Tskj(7)
   REAL Fmach , Refc , Rfk , Z(1)
   COMMON /amgmn / Mcb , Nrow , Nd , Ne , Refc , Fmach , Rfk , Tskj , Isk , Nsk
   COMMON /blank / Nk , Nj
   COMMON /dlbdy / Nj1 , Nk1 , Np , Nb , Ntp , Nbz , Nby , Ntz , Nty , Nt0 , Ntzs , Ntys , Inc , Ins , Inb , Inas , Izin , Iyin ,   &
                 & Inbea1 , Inbea2 , Insbea , Izb , Iyb , Iavr , Iarb , Infl , Ixle , Ixte , Int121 , Int122 , Izs , Iys , Ics ,    &
                 & Iee , Isg , Icg , Ixij , Ix , Idelx , Ixic , Ixlam , Ia0 , Ixis1 , Ixis2 , Ia0p , Iria , Inasb , Ifla1 , Ifla2 , &
                 & Ith1a , Ith2a , Ecore , Next , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Ntbe
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
   INTEGER Input , Matout , Skj
   REAL beta
   INTEGER i , k , lnas , lnb , lnfl , lns , lnsb , lt1 , lt2 , n , n1 , name(2) , nhaero , nhcore , nhpoin
   INTEGER korsz
!
!     DRIVER FOR DOUBLET LATTICE WITH BODIES
!
   EQUIVALENCE (Iz(1),Z(1))
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
   Ecore = korsz(Iz) - 4*Sysbuf
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
      CALL fread(Input,Iz,n,0)
!
!     GET LENGTHS OF VARIABLE ARRAYS, PANELS THEN BODIES
!
      lnas = 0
      IF ( Np/=0 ) THEN
         DO i = 1 , Np
            lnas = lnas + Iz(Inas+i-1)
         ENDDO
      ENDIF
      lnb = 0
      lnsb = 0
      lnfl = 0
      lt1 = 0
      lt2 = 0
      DO i = 1 , Nb
         k = i - 1
         lnb = lnb + Iz(Inbea1+k)
         lnsb = lnsb + Iz(Insbea+k)
         lnfl = lnfl + Iz(Infl+k)
         lt1 = lt1 + Iz(Int121+k)
         lt2 = lt2 + Iz(Int122+k)
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
         CALL fread(Input,Iz(Next),n,1)
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
   RETURN
END SUBROUTINE dlamby
