!*==amgb2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgb2(Input,D1jk,D2jk)
   IMPLICIT NONE
   USE C_AMGBUG
   USE C_AMGMN
   USE C_AMGP2
   USE C_BAMG2L
   USE C_BLANK
   USE C_CONDAS
   USE C_PACKX
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   INTEGER :: D1jk
   INTEGER :: D2jk
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ecore , i , icol , ip1 , ip2 , ip3 , iptr , irsln , j , k , ndata , next , nline , nsns , nwar
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: radii
   EXTERNAL amgb2a , bckrec , bldpk , bldpkn , bug1 , fread , korsz , mesage , pack , read
!
! End of declarations rewritten by SPAG
!
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
   !>>>>EQUIVALENCE (Work(1),Iz(1))
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
   ecore = korsz(iz) - 3*Sysbuf
   CALL read(*100,*100,Input,iz,ecore,1,nwar)
!
!     ERROR MESSAGES
!
!     NOT ENOUGH CORE
!
   CALL mesage(-8,0,name)
   RETURN
 100  ndata = 3*Nstns + 10
   IF ( Debug ) CALL bug1('ACPT-REST ',10,iz,nwar)
   irsln = 0
   nline = 0
   DO i = 1 , nwar , ndata
      IF ( Iref==iz(i) ) irsln = i
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
END SUBROUTINE amgb2
