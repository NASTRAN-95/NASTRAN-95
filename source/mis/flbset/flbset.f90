!*==flbset.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE flbset
   USE c_bitpos
   USE c_flbfil
   USE c_flbptr
   USE c_machin
   USE c_two
   USE c_zzzzzz
   IMPLICIT NONE
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
         file = sil
         isil = icore
         nz = igrid - isil - 1
         CALL gopen(sil,z(ibuf1),0)
         CALL read(*80,*20,sil,z(isil),nz,0,nsil)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      CALL close(sil,1)
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
         file = conect
         CALL open(*60,conect,z(ibuf1),1)
!
!     FILE 1
!
         CALL write(conect,nam,2,1)
         DO i = 1 , ngrid
            j = igrid + i - 1
            IF ( z(j)>0 ) THEN
               nfr = z(j)/1000000
               nfl = z(j) - nfr*1000000
               group(1) = z(isil+i-1)
               group(2) = nfr + nfl
               group(3) = nfr + 3*nfl
               CALL write(conect,group,3,1)
            ENDIF
         ENDDO
         CALL eof(conect)
!
!     FILE 2
!
         CALL write(conect,nam,2,1)
         DO i = 1 , ngrid
            j = igrid + i - 1
            IF ( z(j)<0 .OR. z(j)>=1000000 ) THEN
               IF ( z(j)>0 ) THEN
                  ngroup = 1
                  nngrid = z(j)/1000000
                  nnsil = nngrid
               ELSE
                  ngroup = 3
                  nngrid = iabs(z(j))
                  nnsil = nngrid*3
               ENDIF
               jsil = z(isil+i-1)
               DO j = 1 , ngroup
                  group(1) = jsil
                  group(2) = nngrid
                  group(3) = nnsil
                  CALL write(conect,group,3,1)
                  jsil = jsil + 1
               ENDDO
            ENDIF
         ENDDO
!
         CALL close(conect,1)
         mcb(1) = conect
         mcb(2) = ngrid
         CALL wrttrl(mcb)
!
!     READ USET TABLE INTO CORE
!
         file = uset
         iuset = isil + nsil + 1
         nz = igrid - iuset - 1
         CALL gopen(uset,z(ibuf1),0)
         CALL read(*80,*40,uset,z(iuset),nz,0,nuset)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      CALL close(uset,1)
!
!     CONSTRUCT A LIST OF FREE SURFACE GRID POINTS BY PASSING THROUGH
!     THE GRID POINT CONNECTIVITY TABLE.
!
         icore = iuset + nuset
         ifree = icore
         DO i = 1 , ngrid
            IF ( z(igrid+i-1)>=1000000 ) THEN
               z(icore) = i
               icore = icore + 1
               IF ( icore>=igrid ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
         nfree = icore - ifree
!
!     PASS THROUGH SIL AND PROCESS EACH GRID POINT TO SET THE
!     APPROPRIATE BIT POSITIONS IN THE NEW USET
!
!     *** NOTE.
!     THE UW BIT IS NO LONGER USED.  INSTEAD THE UA BIT WILL REFLECT
!     THE SOLUTION SET  (UAB + UFR)
!
         z(isil+nsil) = nuset + 1
         nstr = 0
         total = 0
         maska = complf(two(ua))
         juset = iuset
         DO igrd = 1 , nsil
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  k = ibgpdt + 4*(igrd-1)
                  icstm = z(k)
                  IF ( icstm==-1 ) THEN
                     nnsil = 1
                  ELSEIF ( z(isil+igrd)==z(isil+igrd-1)+1 ) THEN
!
!     FLUID POINT - SET Y BIT.
!
                     z(juset) = orf(z(juset),two(uy))
                     CALL bisloc(*42,igrd,z(ifree),1,nfree,jloc)
!
!     FREE SURFACE FLUID POINT - SET UFR, UA AND UZ BITS
!
                     z(juset) = orf(z(juset),two(ufr))
                     z(juset) = orf(z(juset),two(ua))
                     z(juset) = orf(z(juset),two(uz))
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
                     z(juset) = orf(z(juset),two(ux))
                     z(juset) = orf(z(juset),two(uz))
                     IF ( andf(z(juset),two(ua))/=0 ) z(juset) = orf(z(juset),two(uab))
                     total = orf(total,z(juset))
                     juset = juset + 1
                  ENDDO
                  CYCLE
!
!     INTERIOR FLUID POINT - SET UI BIT AND TURN OF UA BIT
!
 42               z(juset) = orf(z(juset),two(ui))
                  z(juset) = andf(z(juset),maska)
                  spag_nextblock_2 = 2
               CASE (2)
!
                  total = orf(total,z(juset))
                  juset = juset + 1
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
!
!     WRITE OUT NEW USETF VECTOR
!
         CALL gopen(usetf,z(ibuf1),1)
         CALL write(usetf,z(iuset),nuset,1)
         CALL close(usetf,1)
         mcb(1) = usetf
         mcb(2) = 0
         mcb(3) = nuset
         mcb(4) = rshift(total,ihalf)
         mcb(5) = andf(total,jhalf)
         CALL wrttrl(mcb)
!
!     WRITE OUT NEW USETS VECTOR
!
         CALL gopen(usets,z(ibuf1),1)
         luset = iuset + nuset - 1
         DO i = iuset , luset
            IF ( andf(z(i),two(ux))/=0 ) CALL write(usets,z(i),1,0)
         ENDDO
         CALL close(usets,1)
         mask = complf(orf(two(uy),two(ufr)))
         total = andf(total,mask)
         mcb(1) = usets
         mcb(3) = nstr
         mcb(4) = rshift(total,ihalf)
         mcb(5) = andf(total,jhalf)
         CALL wrttrl(mcb)
!
!     PRINT NEW USET VECTOR IF USER REQUESTS
!
         icore = iuset + nuset
         CALL flbprt(iuset,icore,ibuf1)
!
!     USET PROCESSING COMPLETED
!
         icore = iuset
         RETURN
!
!     ERROR CONDITIONS
!
 60      n = -1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 80      n = -2
         spag_nextblock_1 = 3
      CASE (2)
         n = -8
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(n,file,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE flbset
