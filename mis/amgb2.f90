
SUBROUTINE amgb2(Input,D1jk,D2jk)
   IMPLICIT NONE
   REAL Amach , Amachr , Blspc , Bspace , Chord , Degra , Den , Dum(2) , Flowa , Mach , Maxmac , Minmac , Pi , Radeg , Radius ,     &
      & Redf , Refc , Refcrd , Refden , Refflo , Refmac , Refstg , Refvel , Rfreq , S4pisq , Sigma , Stager , Twopi , Vel , Work(1) &
      & , Xsign
   LOGICAL Debug , Tsonic
   INTEGER Ii , Incr , Iout , Iref , Iti , Ito , Iz(1) , Mcb(7) , Nj , Nk , Nlines , Nn , Nrow , Nstns , Nstnsx , Sln , Sysbuf ,    &
         & Td1jk(7) , Td2jk(7)
   COMMON /amgbug/ Debug
   COMMON /amgmn / Mcb , Nrow , Dum , Refc , Sigma , Rfreq
   COMMON /amgp2 / Td1jk , Td2jk
   COMMON /bamg2l/ Iref , Minmac , Maxmac , Nlines , Nstns , Refstg , Refcrd , Refmac , Refden , Refvel , Refflo , Sln , Nstnsx ,   &
                 & Stager , Chord , Radius , Bspace , Mach , Den , Vel , Flowa , Amach , Redf , Blspc , Amachr , Tsonic , Xsign
   COMMON /blank / Nk , Nj
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /system/ Sysbuf , Iout
   COMMON /zzzzzz/ Work
   INTEGER D1jk , D2jk , Input
   INTEGER ecore , i , icol , ip1 , ip2 , ip3 , iptr , irsln , j , k , name(2) , ndata , next , nline , nsns , nwar
   INTEGER korsz
   REAL radii
!
!     DRIVER FOR COMPRESSOR BLADE THEORY
!     COMPUTATIONS ARE FOR D1JK AND D2JK MATRICES
!     FOR COMPRESSOR BLADES K-SET = J-SET = NLINES*NSTNS
!     D1JK = F(INVERSE)TRANSPOSE
!     NOTE - AMP MODULE TAKES D1JK(TRANSPOSE) SO OUTPUT
!            F(INVERSE)TRANSPOSE TO GET EFFECT OF F(INVERSE) IN AMP.
!
!     D2JK = NULL
!
   EQUIVALENCE (Work(1),Iz(1))
   DATA name/4HAMGB , 4H2   /
!
!     READ PARAMETERS IREF,MINMAC,MAXMAC,NLINES AND NSTNS
!
   CALL fread(Input,Iref,5,0)
   IF ( Debug ) CALL bug1('ACPT-REF  ',5,Iref,5)
!
!     READ REST OF ACPT RECORD INTO OPEN CORE AND LOCATE REFERENCE
!     PARAMETERS REFSTG,REFCRD,REFMAC,REFDEN,REFVEL AND REFFLO
!
   ecore = korsz(Iz) - 3*Sysbuf
   CALL read(*100,*100,Input,Iz,ecore,1,nwar)
!
!     ERROR MESSAGES
!
!     NOT ENOUGH CORE
!
   CALL mesage(-8,0,name)
   GOTO 99999
 100  ndata = 3*Nstns + 10
   IF ( Debug ) CALL bug1('ACPT-REST ',10,Iz,nwar)
   irsln = 0
   nline = 0
   DO i = 1 , nwar , ndata
      IF ( Iref==Iz(i) ) irsln = i
      nline = nline + 1
   ENDDO
!
!     DETERMINE DIRECTION OF BLADE ROTATION VIA Y-COORDINATES AT TIP
!     STREAMLINE. USE COORDINATES OF FIRST 2 NODES ON STREAMLINE.
!
   iptr = ndata*(Nlines-1)
   Xsign = 1.0
   IF ( Work(iptr+15)<Work(iptr+12) ) Xsign = -1.0
!
   IF ( Debug ) CALL bug1('RADII     ',20,radii,Nlines)
!
!     DID IREF MATCH AN SLN OR IS THE DEFAULT TO BE TAKEN (BLADE TIP)
!
   IF ( irsln==0 ) irsln = (Nlines-1)*ndata + 1
   Refstg = Work(irsln+2)
   Refcrd = Work(irsln+3)
   Refmac = Work(irsln+6)
   Refden = Work(irsln+7)
   Refvel = Work(irsln+8)
   Refflo = Work(irsln+9)
!
!     REPOSITION ACPT TO BEGINNING OF COMPRESSOR BLADE DATA
!
   CALL bckrec(Input)
   CALL fread(Input,0,-6,0)
!
   IF ( Debug ) CALL bug1('BAMG2L    ',22,Iref,27)
!
!     COMPUTE POINTERS AND SEE IF THERE IS ENOUGH CORE
!
   nsns = Nstns*Nstns
   ip1 = 1
   ip2 = ip1 + nsns
   next = ip2 + 3*Nstns
   IF ( next>ecore ) THEN
      CALL mesage(-8,0,name)
   ELSE
!
!     COMPUTE F(INVERSE) FOR EACH STREAMLINE
!
      Nn = Ii + Nstns - 1
      DO nline = 1 , Nlines
         CALL amgb2a(Input,Work(ip1),Work(ip2),Work(ip2))
!
!     OUTPUT D1JK (=F(INVERSE)TRANSPOSE) FOR THIS STREAMLINE.
!     NOTE - AMP MODULE TAKES D1JK(TRANSPOSE) SO OUTPUT
!            F(INVERSE)TRANSPOSE TO GET EFFECT OF F(INVERSE) IN AMP.
!
         ip3 = ip2 + Nstns - 1
         DO i = 1 , Nstns
            k = i
            DO j = ip2 , ip3
               Work(j) = Work(k)
               k = k + Nstns
            ENDDO
            CALL pack(Work(ip2),D1jk,Td1jk)
            IF ( Debug ) CALL bug1('D1JK      ',31,Work(ip2),Nstns)
         ENDDO
         Ii = Ii + Nstns
         IF ( nline/=Nlines ) Nn = Nn + Nstns
      ENDDO
!
!     OUTPUT D2JK = NULL
!
      DO icol = 1 , Nk
         CALL bldpk(Iti,Ito,D2jk,0,0)
         CALL bldpkn(D2jk,0,Td2jk)
      ENDDO
      RETURN
   ENDIF
99999 RETURN
END SUBROUTINE amgb2
