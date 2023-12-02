!*==stpda.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stpda(Input,Ajjl,Skj)
   USE c_amgmn
   USE c_blank
   USE c_condas
   USE c_packx
   USE c_stripc
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
         icore = korsz(iz) - 4*sysbuf
!
!     BRING IN DATA AND ALLOCATE CORE
!
         CALL fread(Input,z,8,0)
         nnj = iz(1)
         claf = iz(2)
         lclaf = iz(3)
         ncirc = iz(4)
         lcirc = iz(5)
         nncirc = ncirc + 1
         nmach = iz(6)
         ns = iz(7)
         i8 = 8
         clam = z(i8)
         fm = 1.0
         bref = refc/2.0
         ekr(1) = rfk
         idy = 1
         ibloc = idy + ns
         id = ibloc + ns
         ica = id + ns
         igap = ica + ns
         insize = igap + ns
         icla = insize + ns
         ibm = icla + ns
         igm = ibm + 16*ns
         ipm = igm + 12*ns
         ioc = ipm + 37*ns
         IF ( ioc>icore ) CALL mesage(-8,0,name)
!
!     READ IN ARRAYS WHICH ARE FIXED
!
         nw = 6*ns
         CALL fread(Input,z,nw,0)
!
!     SET CLA ARRAY OR BB AND BETA
!
         IF ( claf==0 ) THEN
            DO i = 1 , ns
               z(icla+i-1) = twopi
            ENDDO
            IF ( ncirc==0 ) THEN
               CALL fread(Input,z,0,1)
               spag_nextblock_1 = 2
            ELSE
               DO i = 1 , nmach
                  CALL fread(Input,rm,1,0)
                  IF ( rm==fmach ) GOTO 10
                  CALL fread(Input,z,-(2*ncirc+1),0)
               ENDDO
!
!     ERROR MESSAGES
!
               n = lcirc
               spag_nextblock_1 = 3
            ENDIF
            CYCLE
 10         CALL fread(Input,bb(1),1,0)
            DO i = 2 , nncirc
               CALL fread(Input,bb(i),1,0)
               CALL fread(Input,beta(i),1,0)
            ENDDO
            CALL fread(Input,z,0,1)
         ELSE
            IF ( claf<0 ) THEN
               CALL fread(Input,rm,1,0)
               CALL fread(Input,z(icla),ns,1)
               DO i = 1 , ns
                  z(icla+i-1) = z(icla+i-1)*sqrt((1.0-(rm*rm*clam*clam))/(1.0-(fmach*fmach*clam*clam)))
               ENDDO
               spag_nextblock_1 = 2
            ELSE
!
!     FIND MACH NUMBER FOR CLA
!
               DO i = 1 , nmach
                  CALL fread(Input,rm,1,0)
                  IF ( rm==fmach ) GOTO 20
                  CALL fread(Input,z,-ns,0)
               ENDDO
               n = lclaf
               spag_nextblock_1 = 3
            ENDIF
            CYCLE
!
!     MACH NUMBER NOT INPUT ON AEFACT CARD CLCAF
!
 20         CALL fread(Input,z(icla),ns,1)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     OUTPUT SKJ
!
         iti = 1
         it0 = 3
         ii = isk
         nsk = nsk + 1
         nn = nsk
         rm = 1.0
         DO i = 1 , nnj
            CALL pack(rm,Skj,tskj)
            ii = ii + 1
            IF ( i/=nnj ) nn = nn + 1
         ENDDO
         isk = ii
         nsk = nn
         iti = 3
         it0 = 3
         CALL stpbg(z(ibm),z(igm),ns,z(ibloc),z(id),z(ica),z(insize))
         CALL stpphi(z(ica),z(ibloc),z(ipm),ns)
         CALL stpaic(z(ibloc),z(idy),z(insize),z(igap),z(ibm),z(igm),z(ipm),ns,z(icla),Ajjl)
         nrow = nrow + nnj
         RETURN
      CASE (3)
         WRITE (nout,99001) ufm , fmach , n
99001    FORMAT (A23,' 2426, MACH NUMBER ',F10.5,' WAS NOT FOUND ON ','AEFACT CARD',I9)
         CALL mesage(-61,0,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE stpda
