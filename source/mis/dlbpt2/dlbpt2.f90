!*==dlbpt2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dlbpt2(Input,W1jk,W2jk)
   USE c_amgmn
   USE c_amgp2
   USE c_packx
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   INTEGER :: W1jk
   INTEGER :: W2jk
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(4) :: a
   INTEGER :: ecore , i , iarb , iavr , ibt , icg , ics , idelx , iee , ify , iii , inas , inb , inbea1 , inbea2 , inc , infl ,     &
            & ins , insbea , int121 , int122 , isg , ix , ixij , ixle , ixte , iyb , iyin , iys , izb , izin , izs , j , k , lnas , &
            & lnb , lnfl , lns , lnsb , lt1 , lt2 , n , nbe , nbtd , next , ntzy , nw
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL fread , korsz , mesage , pack , read
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
   DATA name/4HDLBP , 4HT2  /
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
   IF ( ntp==0 ) CALL fread(Input,0,0,1)
   IF ( ntp/=0 ) THEN
      lns = iz(1)
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
         GOTO 100
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
!
!     READ VARIABLE  ARRAYS AND SET POINTERS TO CORE
!
         next = n + 1
         n = 2*nb + 5*lns + 4*ntp + 3*lnb + 4*lnsb + lnas + 2*lnfl + lt1 + lt2
         IF ( next+n>=ecore ) THEN
            CALL mesage(-8,0,name)
            GOTO 100
         ELSE
            CALL read(*100,*100,Input,iz(next),n,1,nw)
            next = next + n + 1
            iys = izs + nb + lns
            ics = iys
            iee = ics + nb + lns
            isg = iee + lns
            icg = isg + lns
            ixij = icg
            ix = ixij + lns
            idelx = ix + ntp + lnb
!
!     COMPUTE TERMS AND PACK
!
            nn = ii + 1
            DO i = 1 , ntp
               a(1) = 0.0
               a(2) = 1.0
               CALL pack(a,W1jk,tw1jk)
               a(1) = -(2.0/refc)
               a(2) = z(idelx+i-1)/(2.0*refc)
               CALL pack(a,W2jk,tw2jk)
!
!     BUMP PACK INDEXES
!
               ii = ii + 2
               IF ( i/=ntp ) nn = nn + 2
            ENDDO
         ENDIF
      ENDIF
   ENDIF
   ntzy = ntz + nty
   IF ( ntzy/=0 ) THEN
      nn = ii + 1
      a(1) = 0.0
      a(2) = 0.0
      DO i = 1 , ntzy
         CALL pack(a,W1jk,tw1jk)
         CALL pack(a,W2jk,tw2jk)
      ENDDO
   ENDIF
   ntzy = ntzs + ntys
   IF ( ntzy/=0 ) THEN
!
!     ANOTHER HARDER SHUFFLE
!
      iii = ii
      inbea2 = inbea2 - 1
      insbea = insbea - 1
      ify = ii
      IF ( nbz/=0 ) THEN
         DO i = 1 , nbz
            ibt = iz(inbea2+i)
            nbe = iz(insbea+i)
            IF ( ibt==2 ) THEN
               a(1) = 0.0
               a(4) = 0.0
               DO j = 1 , nbe
                  nn = ii + 3
                  a(2) = 0.0
                  a(3) = 1.0
                  CALL pack(a,W1jk,tw1jk)
                  a(2) = -2.0/refc
                  a(3) = 0.0
                  CALL pack(a,W2jk,tw2jk)
                  ii = ii + 4
               ENDDO
            ELSE
               a(1) = 0.0
               a(2) = 1.0
               a(3) = -2.0/refc
               a(4) = 0.0
               DO j = 1 , nbe
                  nn = ii + 1
                  CALL pack(a,W1jk,tw1jk)
                  CALL pack(a(3),W2jk,tw2jk)
                  ii = ii + 2
                  ify = ii
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      IF ( nby/=0 ) THEN
         ii = ify
         nbtd = nb - nby + 1
         DO i = nbtd , nb
            ibt = iz(inbea2+i)
            nbe = iz(insbea+i)
            IF ( ibt==3 ) THEN
               a(1) = 0.0
               a(2) = -1.0
               a(3) = -2.0/refc
               a(4) = 0.0
               DO j = 1 , nbe
                  nn = ii + 1
                  CALL pack(a,W1jk,tw1jk)
                  CALL pack(a(3),W2jk,tw2jk)
                  ii = ii + 2
               ENDDO
            ELSE
               a(2) = 0.0
               a(3) = 0.0
               DO j = 1 , nbe
                  nn = ii + 3
                  a(1) = 0.0
                  a(4) = -1.0
                  CALL pack(a,W1jk,tw1jk)
                  a(1) = -2.0/refc
                  a(4) = 0.0
                  CALL pack(a,W2jk,tw2jk)
                  ii = ii + 4
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      ii = iii + ntzy*2
      nn = ii - 1
   ENDIF
   RETURN
 100  CALL mesage(-7,0,name)
END SUBROUTINE dlbpt2
