!*==amgt2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgt2(Input,D1jk,D2jk)
   USE c_amgbug
   USE c_amgmn
   USE c_amgp2
   USE c_blank
   USE c_condas
   USE c_packx
   USE c_system
   USE c_tamg2l
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   INTEGER :: D1jk
   INTEGER :: D2jk
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ecore , i , icol , ip1 , ip2 , ip3 , iptr , irsln , j , k , ndata , next , nline , nsns , nstns2 , nwar
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL amgt2a , bckrec , bldpk , bldpkn , bug1 , fread , korsz , mesage , pack , read
!
! End of declarations rewritten by SPAG
!
!
!     DRIVER FOR SWEPT TURBOPROP BLADES (AEROELASTIC THEORY 7).
!
!     COMPUTATIONS ARE FOR D1JK AND D2JK MATRICES.
!     FOR SWEPT TURBOPROPS K-SET = J-SET = 2*NSTNS*NLINES.
!
!     D1JK = F(INVERSE)TRANSPOSE
!
!     D2JK = NULL
!
   !>>>>EQUIVALENCE (Work(1),Iz(1))
   DATA name/4HAMGT , 4H2   /
!
!     READ PARAMETERS IREF,MINMAC,MAXMAC,NLINES AND NSTNS
!
   CALL fread(Input,iref,5,0)
   IF ( debug ) CALL bug1('ACPT-REF  ',5,iref,5)
!
!     READ REST OF ACPT RECORD INTO OPEN CORE AND LOCATE REFERENCE
!     PARAMETERS REFSTG,REFCRD,REFMAC,REFDEN,REFVEL AND REFSWP
!
   ecore = korsz(iz) - 3*sysbuf
   CALL read(*100,*100,Input,iz,ecore,1,nwar)
!
!     ERROR MESSAGES
!
!     NOT ENOUGH CORE
!
   CALL mesage(-8,0,name)
   RETURN
 100  ndata = 3*nstns + 10
   IF ( debug ) CALL bug1('ACPT-REST ',10,iz,nwar)
   irsln = 0
   nline = 0
   DO i = 1 , nwar , ndata
      IF ( iref==iz(i) ) irsln = i
      nline = nline + 1
   ENDDO
!
!     DETERMINE DIRECTION OF BLADE ROTATION VIA Y-COORDINATES AT TIP
!     STREAMLINE. USE COORDINATES OF FIRST 2 NODES ON STREAMLINE.
!
   iptr = ndata*(nlines-1)
   xsign = 1.0
   IF ( work(iptr+15)<work(iptr+12) ) xsign = -1.0
!
!     DID IREF MATCH AN SLN OR IS THE DEFAULT TO BE TAKEN (BLADE TIP)
!
   IF ( irsln==0 ) irsln = (nlines-1)*ndata + 1
   refstg = work(irsln+2)
   refcrd = work(irsln+3)
   refmac = work(irsln+6)
   refden = work(irsln+7)
   refvel = work(irsln+8)
   refswp = work(irsln+9)
!
!     REPOSITION ACPT TO BEGINNING OF BLADE DATA.
!
   CALL bckrec(Input)
   CALL fread(Input,0,-6,0)
!
   IF ( debug ) CALL bug1('TAMG2L    ',22,iref,27)
!
!     COMPUTE POINTERS AND SEE IF THERE IS ENOUGH CORE
!
   nstns2 = 2*nstns
   nsns = nstns*nstns
   ip1 = 1
   ip2 = ip1 + nsns
   next = ip2 + 3*nstns
   IF ( next>ecore ) THEN
      CALL mesage(-8,0,name)
   ELSE
!
!     COMPUTE F(INVERSE) FOR EACH STREAMLINE
!
      nn = ii + nstns - 1
      DO nline = 1 , nlines
         CALL amgt2a(Input,work(ip1),work(ip2),work(ip2))
!
!     OUTPUT D1JK (=F(INVERSE)TRANSPOSE) FOR THIS STREAMLINE.
!
         ip3 = ip2 + nstns - 1
         DO i = 1 , nstns
            k = i
            DO j = ip2 , ip3
               work(j) = work(k)
               k = k + nstns
            ENDDO
            CALL pack(work(ip2),D1jk,td1jk)
            IF ( debug ) CALL bug1('D1JK      ',40,work(ip2),nstns)
         ENDDO
         ii = ii + nstns
         nn = nn + nstns
         DO i = 1 , nstns
            k = i
            DO j = ip2 , ip3
               work(j) = work(k)
               k = k + nstns
            ENDDO
            CALL pack(work(ip2),D1jk,td1jk)
            IF ( debug ) CALL bug1('D1JK      ',70,work(ip2),nstns)
         ENDDO
         ii = ii + nstns
         IF ( nline/=nlines ) nn = nn + nstns
      ENDDO
!
!     OUTPUT D2JK = NULL
!
      DO icol = 1 , nk
         CALL bldpk(iti,ito,D2jk,0,0)
         CALL bldpkn(D2jk,0,td2jk)
      ENDDO
      RETURN
   ENDIF
END SUBROUTINE amgt2
