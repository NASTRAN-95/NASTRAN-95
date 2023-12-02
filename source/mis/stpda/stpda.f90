!*==stpda.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stpda(Input,Ajjl,Skj)
   IMPLICIT NONE
   USE C_AMGMN
   USE C_BLANK
   USE C_CONDAS
   USE C_PACKX
   USE C_STRIPC
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   INTEGER :: Ajjl
   INTEGER :: Skj
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: claf , i , i8 , ibloc , ibm , ica , icla , icore , id , idy , igap , igm , insize , ioc , ipm , lcirc , lclaf , n ,   &
            & nmach , nnj , nw
   INTEGER , DIMENSION(8) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: rm
   EXTERNAL fread , korsz , mesage , pack , stpaic , stpbg , stpphi
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     DRIVER FOR STRIP THEORY
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
   DATA name/4HSTPD , 4HA   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         icore = korsz(iz) - 4*Sysbuf
!
!     BRING IN DATA AND ALLOCATE CORE
!
         CALL fread(Input,Z,8,0)
         nnj = iz(1)
         claf = iz(2)
         lclaf = iz(3)
         Ncirc = iz(4)
         lcirc = iz(5)
         Nncirc = Ncirc + 1
         nmach = iz(6)
         Ns = iz(7)
         i8 = 8
         Clam = Z(i8)
         Fm = 1.0
         Bref = Refc/2.0
         Ekr(1) = Rfk
         idy = 1
         ibloc = idy + Ns
         id = ibloc + Ns
         ica = id + Ns
         igap = ica + Ns
         insize = igap + Ns
         icla = insize + Ns
         ibm = icla + Ns
         igm = ibm + 16*Ns
         ipm = igm + 12*Ns
         ioc = ipm + 37*Ns
         IF ( ioc>icore ) CALL mesage(-8,0,name)
!
!     READ IN ARRAYS WHICH ARE FIXED
!
         nw = 6*Ns
         CALL fread(Input,Z,nw,0)
!
!     SET CLA ARRAY OR BB AND BETA
!
         IF ( claf==0 ) THEN
            DO i = 1 , Ns
               Z(icla+i-1) = Twopi
            ENDDO
            IF ( Ncirc==0 ) THEN
               CALL fread(Input,Z,0,1)
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
               DO i = 1 , nmach
                  CALL fread(Input,rm,1,0)
                  IF ( rm==Fmach ) GOTO 10
                  CALL fread(Input,Z,-(2*Ncirc+1),0)
               ENDDO
!
!     ERROR MESSAGES
!
               n = lcirc
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
 10         CALL fread(Input,Bb(1),1,0)
            DO i = 2 , Nncirc
               CALL fread(Input,Bb(i),1,0)
               CALL fread(Input,Beta(i),1,0)
            ENDDO
            CALL fread(Input,Z,0,1)
         ELSE
            IF ( claf<0 ) THEN
               CALL fread(Input,rm,1,0)
               CALL fread(Input,Z(icla),Ns,1)
               DO i = 1 , Ns
                  Z(icla+i-1) = Z(icla+i-1)*sqrt((1.0-(rm*rm*Clam*Clam))/(1.0-(Fmach*Fmach*Clam*Clam)))
               ENDDO
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     FIND MACH NUMBER FOR CLA
!
               DO i = 1 , nmach
                  CALL fread(Input,rm,1,0)
                  IF ( rm==Fmach ) GOTO 20
                  CALL fread(Input,Z,-Ns,0)
               ENDDO
               n = lclaf
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     MACH NUMBER NOT INPUT ON AEFACT CARD CLCAF
!
 20         CALL fread(Input,Z(icla),Ns,1)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     OUTPUT SKJ
!
         Iti = 1
         It0 = 3
         Ii = Isk
         Nsk = Nsk + 1
         Nn = Nsk
         rm = 1.0
         DO i = 1 , nnj
            CALL pack(rm,Skj,Tskj)
            Ii = Ii + 1
            IF ( i/=nnj ) Nn = Nn + 1
         ENDDO
         Isk = Ii
         Nsk = Nn
         Iti = 3
         It0 = 3
         CALL stpbg(Z(ibm),Z(igm),Ns,Z(ibloc),Z(id),Z(ica),Z(insize))
         CALL stpphi(Z(ica),Z(ibloc),Z(ipm),Ns)
         CALL stpaic(Z(ibloc),Z(idy),Z(insize),Z(igap),Z(ibm),Z(igm),Z(ipm),Ns,Z(icla),Ajjl)
         Nrow = Nrow + nnj
         RETURN
      CASE (3)
         WRITE (Nout,99001) Ufm , Fmach , n
99001    FORMAT (A23,' 2426, MACH NUMBER ',F10.5,' WAS NOT FOUND ON ','AEFACT CARD',I9)
         CALL mesage(-61,0,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE stpda
