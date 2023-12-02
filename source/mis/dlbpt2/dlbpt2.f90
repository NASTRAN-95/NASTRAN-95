!*==dlbpt2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dlbpt2(Input,W1jk,W2jk)
   IMPLICIT NONE
   USE C_AMGMN
   USE C_AMGP2
   USE C_PACKX
   USE C_SYSTEM
   USE C_ZZZZZZ
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
   ecore = korsz(iz) - 4*Sysbuf
!
!     READ LENGTHS OF ARRAYS
!
   CALL fread(Input,Nj1,13,0)
!
!     COMPUTE POINTERS TO OPEN CORE
!
   IF ( Ntp==0 ) CALL fread(Input,0,0,1)
   IF ( Ntp/=0 ) THEN
      lns = iz(1)
      inc = 1
      ins = inc
      inb = ins + Np
      inas = inb + Np
      izin = inas
      iyin = izin
      inbea1 = iyin + Np
      inbea2 = inbea1 + Nb
      insbea = inbea2 + Nb
      izb = insbea + Nb
      iyb = izb + Nb
      iavr = iyb + Nb
      iarb = iavr + Nb
      infl = iarb + Nb
      ixle = infl + Nb
      ixte = ixle + Nb
      int121 = ixte + Nb
      int122 = int121 + Nb
      izs = int122 + Nb
      n = 3*Np + 12*Nb
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
         IF ( Np/=0 ) THEN
            DO i = 1 , Np
               lnas = lnas + iz(inas+i-1)
            ENDDO
         ENDIF
         lnb = 0
         lnsb = 0
         lnfl = 0
         lt1 = 0
         lt2 = 0
         DO i = 1 , Nb
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
         n = 2*Nb + 5*lns + 4*Ntp + 3*lnb + 4*lnsb + lnas + 2*lnfl + lt1 + lt2
         IF ( next+n>=ecore ) THEN
            CALL mesage(-8,0,name)
            GOTO 100
         ELSE
            CALL read(*100,*100,Input,iz(next),n,1,nw)
            next = next + n + 1
            iys = izs + Nb + lns
            ics = iys
            iee = ics + Nb + lns
            isg = iee + lns
            icg = isg + lns
            ixij = icg
            ix = ixij + lns
            idelx = ix + Ntp + lnb
!
!     COMPUTE TERMS AND PACK
!
            Nn = Ii + 1
            DO i = 1 , Ntp
               a(1) = 0.0
               a(2) = 1.0
               CALL pack(a,W1jk,Tw1jk)
               a(1) = -(2.0/Refc)
               a(2) = Z(idelx+i-1)/(2.0*Refc)
               CALL pack(a,W2jk,Tw2jk)
!
!     BUMP PACK INDEXES
!
               Ii = Ii + 2
               IF ( i/=Ntp ) Nn = Nn + 2
            ENDDO
         ENDIF
      ENDIF
   ENDIF
   ntzy = Ntz + Nty
   IF ( ntzy/=0 ) THEN
      Nn = Ii + 1
      a(1) = 0.0
      a(2) = 0.0
      DO i = 1 , ntzy
         CALL pack(a,W1jk,Tw1jk)
         CALL pack(a,W2jk,Tw2jk)
      ENDDO
   ENDIF
   ntzy = Ntzs + Ntys
   IF ( ntzy/=0 ) THEN
!
!     ANOTHER HARDER SHUFFLE
!
      iii = Ii
      inbea2 = inbea2 - 1
      insbea = insbea - 1
      ify = Ii
      IF ( Nbz/=0 ) THEN
         DO i = 1 , Nbz
            ibt = iz(inbea2+i)
            nbe = iz(insbea+i)
            IF ( ibt==2 ) THEN
               a(1) = 0.0
               a(4) = 0.0
               DO j = 1 , nbe
                  Nn = Ii + 3
                  a(2) = 0.0
                  a(3) = 1.0
                  CALL pack(a,W1jk,Tw1jk)
                  a(2) = -2.0/Refc
                  a(3) = 0.0
                  CALL pack(a,W2jk,Tw2jk)
                  Ii = Ii + 4
               ENDDO
            ELSE
               a(1) = 0.0
               a(2) = 1.0
               a(3) = -2.0/Refc
               a(4) = 0.0
               DO j = 1 , nbe
                  Nn = Ii + 1
                  CALL pack(a,W1jk,Tw1jk)
                  CALL pack(a(3),W2jk,Tw2jk)
                  Ii = Ii + 2
                  ify = Ii
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      IF ( Nby/=0 ) THEN
         Ii = ify
         nbtd = Nb - Nby + 1
         DO i = nbtd , Nb
            ibt = iz(inbea2+i)
            nbe = iz(insbea+i)
            IF ( ibt==3 ) THEN
               a(1) = 0.0
               a(2) = -1.0
               a(3) = -2.0/Refc
               a(4) = 0.0
               DO j = 1 , nbe
                  Nn = Ii + 1
                  CALL pack(a,W1jk,Tw1jk)
                  CALL pack(a(3),W2jk,Tw2jk)
                  Ii = Ii + 2
               ENDDO
            ELSE
               a(2) = 0.0
               a(3) = 0.0
               DO j = 1 , nbe
                  Nn = Ii + 3
                  a(1) = 0.0
                  a(4) = -1.0
                  CALL pack(a,W1jk,Tw1jk)
                  a(1) = -2.0/Refc
                  a(4) = 0.0
                  CALL pack(a,W2jk,Tw2jk)
                  Ii = Ii + 4
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      Ii = iii + ntzy*2
      Nn = Ii - 1
   ENDIF
   RETURN
 100  CALL mesage(-7,0,name)
END SUBROUTINE dlbpt2
