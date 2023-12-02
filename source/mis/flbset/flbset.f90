!*==flbset.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE flbset
   IMPLICIT NONE
   USE C_BITPOS
   USE C_FLBFIL
   USE C_FLBPTR
   USE C_MACHIN
   USE C_TWO
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , icstm , ifree , igrd , iuset , j , jloc , jsil , juset , k , luset , mask , maska , n , nfl , nfr , nfree ,&
            & ngroup , nngrid , nnsil , nstr , nuset , nz , total
   INTEGER , DIMENSION(3) :: group
   INTEGER , DIMENSION(7) , SAVE :: mcb
   INTEGER , DIMENSION(2) , SAVE :: nam , name
   EXTERNAL andf , bisloc , close , complf , eof , flbprt , gopen , mesage , open , orf , read , rshift , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     CONSTRUCTS THE HYDROELASTIC USET VECTOR AND WRITES THE CONECT
!     FILE FOR USE IN CORE ALLOCATION DURING MATRIX ASSEMBLY
!
!
!
!
!
!     MACHINE AND HALF WORD
!
!
!     GINO FILES
!
!
!     CORE POINTERS
!
!
!     OPEN CORE
!
!
!     POWERS OF TWO
!
!
!     USET PIT POSITIONS
!
!
   DATA name/4HFLBS , 4HET  /
   DATA nam/4HCONE , 4HCT  /
   DATA mcb/7*0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!***********************************************************************
!
!     READ SIL INTO CORE
!
         file = Sil
         Isil = Icore
         nz = Igrid - Isil - 1
         CALL gopen(Sil,Z(Ibuf1),0)
         CALL read(*80,*20,Sil,Z(Isil),nz,0,Nsil)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      CALL close(Sil,1)
!
!     WRITE OUT CONECT FILE
!
!     FILE 1 - FOR USE IN ASSEMBLING AF MATRIX, CONTAINS SILS WHICH
!              CONNECT FLUID POINTS TO STRUCTURE POINTS ALONG THE
!              BOUNDARY AND SILS WHICH CONNECT FLUID POINTS ALONG THE
!              FREE SURFACE
!     FILE 2 - FOR USE IN ASSEMBLING THE DKGG MATRIX, CONTAINS SILS
!              WHICH CONNECT STRUCTURE POINTS ALONG THE BOUNDARY AND
!              SILS WHICH CONNECT FLUID POINTS ALONG THE FREE SURFACE
!
!     EACH FILE IS COMPOSED OF A 3 WORD RECORD FOR EACH SIL
!
!              WORD      DESCRIPTION
!
!               1        SIL NUMBER
!               2        MAXIMUN GRID POINTS CONNECTED
!               3        MAXIMUM SILS CONNECTED
!
         file = Conect
         CALL open(*60,Conect,Z(Ibuf1),1)
!
!     FILE 1
!
         CALL write(Conect,nam,2,1)
         DO i = 1 , Ngrid
            j = Igrid + i - 1
            IF ( Z(j)>0 ) THEN
               nfr = Z(j)/1000000
               nfl = Z(j) - nfr*1000000
               group(1) = Z(Isil+i-1)
               group(2) = nfr + nfl
               group(3) = nfr + 3*nfl
               CALL write(Conect,group,3,1)
            ENDIF
         ENDDO
         CALL eof(Conect)
!
!     FILE 2
!
         CALL write(Conect,nam,2,1)
         DO i = 1 , Ngrid
            j = Igrid + i - 1
            IF ( Z(j)<0 .OR. Z(j)>=1000000 ) THEN
               IF ( Z(j)>0 ) THEN
                  ngroup = 1
                  nngrid = Z(j)/1000000
                  nnsil = nngrid
               ELSE
                  ngroup = 3
                  nngrid = iabs(Z(j))
                  nnsil = nngrid*3
               ENDIF
               jsil = Z(Isil+i-1)
               DO j = 1 , ngroup
                  group(1) = jsil
                  group(2) = nngrid
                  group(3) = nnsil
                  CALL write(Conect,group,3,1)
                  jsil = jsil + 1
               ENDDO
            ENDIF
         ENDDO
!
         CALL close(Conect,1)
         mcb(1) = Conect
         mcb(2) = Ngrid
         CALL wrttrl(mcb)
!
!     READ USET TABLE INTO CORE
!
         file = Uset
         iuset = Isil + Nsil + 1
         nz = Igrid - iuset - 1
         CALL gopen(Uset,Z(Ibuf1),0)
         CALL read(*80,*40,Uset,Z(iuset),nz,0,nuset)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      CALL close(Uset,1)
!
!     CONSTRUCT A LIST OF FREE SURFACE GRID POINTS BY PASSING THROUGH
!     THE GRID POINT CONNECTIVITY TABLE.
!
         Icore = iuset + nuset
         ifree = Icore
         DO i = 1 , Ngrid
            IF ( Z(Igrid+i-1)>=1000000 ) THEN
               Z(Icore) = i
               Icore = Icore + 1
               IF ( Icore>=Igrid ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
         nfree = Icore - ifree
!
!     PASS THROUGH SIL AND PROCESS EACH GRID POINT TO SET THE
!     APPROPRIATE BIT POSITIONS IN THE NEW USET
!
!     *** NOTE.
!     THE UW BIT IS NO LONGER USED.  INSTEAD THE UA BIT WILL REFLECT
!     THE SOLUTION SET  (UAB + UFR)
!
         Z(Isil+Nsil) = nuset + 1
         nstr = 0
         total = 0
         maska = complf(Two(Ua))
         juset = iuset
         DO igrd = 1 , Nsil
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  k = Ibgpdt + 4*(igrd-1)
                  icstm = Z(k)
                  IF ( icstm==-1 ) THEN
                     nnsil = 1
                  ELSEIF ( Z(Isil+igrd)==Z(Isil+igrd-1)+1 ) THEN
!
!     FLUID POINT - SET Y BIT.
!
                     Z(juset) = orf(Z(juset),Two(Uy))
                     CALL bisloc(*42,igrd,Z(ifree),1,nfree,jloc)
!
!     FREE SURFACE FLUID POINT - SET UFR, UA AND UZ BITS
!
                     Z(juset) = orf(Z(juset),Two(Ufr))
                     Z(juset) = orf(Z(juset),Two(Ua))
                     Z(juset) = orf(Z(juset),Two(Uz))
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
!
!     STURCTURE POINT - SET UX AND UZ. ALSO SET UAB IF UA IS SET
!
                     nnsil = 6
                  ENDIF
                  nstr = nstr + nnsil
                  DO j = 1 , nnsil
                     Z(juset) = orf(Z(juset),Two(Ux))
                     Z(juset) = orf(Z(juset),Two(Uz))
                     IF ( andf(Z(juset),Two(Ua))/=0 ) Z(juset) = orf(Z(juset),Two(Uab))
                     total = orf(total,Z(juset))
                     juset = juset + 1
                  ENDDO
                  CYCLE
!
!     INTERIOR FLUID POINT - SET UI BIT AND TURN OF UA BIT
!
 42               Z(juset) = orf(Z(juset),Two(Ui))
                  Z(juset) = andf(Z(juset),maska)
                  spag_nextblock_2 = 2
               CASE (2)
!
                  total = orf(total,Z(juset))
                  juset = juset + 1
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
!
!     WRITE OUT NEW USETF VECTOR
!
         CALL gopen(Usetf,Z(Ibuf1),1)
         CALL write(Usetf,Z(iuset),nuset,1)
         CALL close(Usetf,1)
         mcb(1) = Usetf
         mcb(2) = 0
         mcb(3) = nuset
         mcb(4) = rshift(total,Ihalf)
         mcb(5) = andf(total,Jhalf)
         CALL wrttrl(mcb)
!
!     WRITE OUT NEW USETS VECTOR
!
         CALL gopen(Usets,Z(Ibuf1),1)
         luset = iuset + nuset - 1
         DO i = iuset , luset
            IF ( andf(Z(i),Two(Ux))/=0 ) CALL write(Usets,Z(i),1,0)
         ENDDO
         CALL close(Usets,1)
         mask = complf(orf(Two(Uy),Two(Ufr)))
         total = andf(total,mask)
         mcb(1) = Usets
         mcb(3) = nstr
         mcb(4) = rshift(total,Ihalf)
         mcb(5) = andf(total,Jhalf)
         CALL wrttrl(mcb)
!
!     PRINT NEW USET VECTOR IF USER REQUESTS
!
         Icore = iuset + nuset
         CALL flbprt(iuset,Icore,Ibuf1)
!
!     USET PROCESSING COMPLETED
!
         Icore = iuset
         RETURN
!
!     ERROR CONDITIONS
!
 60      n = -1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 80      n = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         n = -8
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(n,file,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE flbset
