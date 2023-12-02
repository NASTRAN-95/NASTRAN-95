!*==flbema.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE flbema(Type)
   USE c_flbfil
   USE c_flbptr
   USE c_names
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Type
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(3) :: alloc
   INTEGER , DIMENSION(12) :: colsil
   INTEGER , DIMENSION(2) :: dict
   INTEGER :: file , i , icode , icol , icpos , ifcol , ilcol , iloc , j , jcore , kcol , kcore , luset , n , ncol , ncore , nloc , &
            & nrow , ntpers , nwds , optc , optw , outmat , typin , typout , xdict , xmat
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(4) :: rowsil
   LOGICAL :: skip
   INTEGER , DIMENSION(288) :: terms
   EXTERNAL bldpk , bldpkn , close , filpos , fwdrec , gopen , mesage , open , pakcol , rdtrl , read , skpfil , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     ASSEMBLES THE AF OR DKGG MATRIX UTITLIZING THE ELEMENT
!     MATRICES GENERATED IN FLBEMG
!
!     TYPE = 1  AFF MATRIX
!     TYPE = 2  DKGG MATRIX
!
   DATA name/4HFLBE , 4HMA  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     ASSIGN FILES DEPENDING ON TYPE
!
         IF ( Type==2 ) THEN
!
!     DKGG MATRIX
!
            outmat = dkgg
            xmat = kgmat
            xdict = kgdict
         ELSE
!
!     AF MATRIX
!
            outmat = af
            xmat = afmat
            xdict = afdict
         ENDIF
!
!     ALLOCATE COLUMN POINTER VECTOR IN TOP OF CORE
!
         mcb(1) = uset
         CALL rdtrl(mcb)
         luset = mcb(3)
         icol = 1
         ncol = luset
         DO i = 1 , ncol
            z(i) = 0
         ENDDO
!
!     INITILIZE OPEN AND CLOSE OPTIONS
!
         optw = wrtrew
         optc = norew
!
!     POSITION CONNECT FILE TO PROPER RECORD
!
         file = conect
         CALL open(*80,conect,z(ibuf1),rdrew)
         IF ( Type==2 ) CALL skpfil(conect,1)
         CALL fwdrec(*100,conect)
         CALL close(conect,norew)
!
!     INITIALIZE PACK - UNPACK DATA
!
         typin = 2
         typout = 2
         mcb(1) = outmat
         mcb(2) = 0
         mcb(3) = luset
         mcb(4) = 3 - Type
         mcb(5) = typout
         mcb(6) = 0
         mcb(7) = 0
!
!     SET UP CORE POINTERS
!
         icore = ncol + 1
         lcore = ibuf2 - 1
         ncore = lcore - icore
         IF ( ncore<200 ) GOTO 140
!
         skip = .FALSE.
         ilcol = 0
         spag_nextblock_1 = 2
      CASE (2)
!
!
!     ALLOCATE ALL AVALABLE CORE FOR THIS PASS BY USE OF CONECT FILE
!
         ifcol = ilcol + 1
         jcore = icore
         file = conect
!
         CALL gopen(conect,z(ibuf1),rd)
!
         IF ( .NOT.(skip) ) CALL read(*20,*140,conect,alloc,3,1,n)
         DO
!
            isil = alloc(1)
            z(isil) = jcore
            z(jcore) = jcore + 1
            jcore = jcore + 1 + alloc(2) + 2*alloc(3)
            IF ( jcore>lcore ) THEN
!
!     INSUFFICIENT CORE FOR NEXT COLUMN - SET FLAG TO SAVE CURRENT
!     CONECT ALLOCATION RECORD
!
               skip = .TRUE.
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSE
               ilcol = isil
               CALL read(*20,*140,conect,alloc,3,1,n)
            ENDIF
         ENDDO
!
!     END OF RECORD ON CONECT - ALL COLUMNS ALLOCATED
!
 20      ilcol = luset
         optc = rew
         spag_nextblock_1 = 3
      CASE (3)
!
         CALL close(conect,optc)
!
!     OPEN DICTIONARY AND MATRIX FILES AND PREPARE TO MAKE PASS
!
         CALL gopen(xdict,z(ibuf1),rdrew)
         CALL gopen(xmat,z(ibuf2),rdrew)
         icpos = 0
         spag_nextblock_1 = 4
      CASE (4)
         DO
!
!     READ XDICT ENTRY AND DETERMINE IF COLUMN IS IN CORE FOR THIS
!     PASS
!
            file = xdict
            CALL read(*100,*60,xdict,dict,2,0,n)
            isil = dict(1)
            IF ( isil>=ifcol .AND. isil<=ilcol ) THEN
!
!     THE COLUMN IS IN CORE - OBTAIN MATRIX DATA FROM XMAT FILE IF
!     WE DO NOT ALREADY HAVE IT
!
               IF ( dict(2)==icpos ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               icpos = dict(2)
               file = xmat
               CALL filpos(xmat,icpos)
               CALL read(*100,*120,xmat,rowsil,4,0,n)
               CALL read(*100,*120,xmat,colsil,4,0,n)
               nrow = 4
               IF ( rowsil(4)<0 ) nrow = 3
               ncol = 4
               IF ( colsil(4)<0 ) ncol = 3
               CALL read(*100,*40,xmat,terms,289,0,nwds)
               icode = 1
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     EXPAND COLSIL TO INCLUDE ALL SILS
!
 40      IF ( nwds>=162 ) THEN
            DO i = 1 , 4
               j = 4 - i
               colsil(3*j+1) = colsil(j+1)
               colsil(3*j+2) = colsil(j+1) + 1
               colsil(3*j+3) = colsil(j+1) + 2
            ENDDO
            ncol = ncol*3
         ENDIF
         ntpers = 2
         IF ( nwds>=54 ) ntpers = 6
         spag_nextblock_1 = 5
      CASE (5)
!
!     LOCATE POSITION OF MATRIX TERMS FOR DESIRED SIL
!
         DO kcol = 1 , ncol
            IF ( colsil(kcol)==isil ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         icode = 2
         spag_nextblock_1 = 7
      CASE (6)
!
         iloc = (kcol-1)*nrow*ntpers + 1
!
!     EXTRACT MATRIX TERMS AND STORE THEM IN CORE
!
         icode = 3
         jcore = z(isil)
         IF ( jcore==0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         kcore = z(jcore)
         DO i = 1 , nrow
            z(kcore) = rowsil(i)
            IF ( ntpers==2 ) z(kcore) = -rowsil(i)
            kcore = kcore + 1
            DO j = 1 , ntpers
               z(kcore) = terms(iloc)
               iloc = iloc + 1
               kcore = kcore + 1
            ENDDO
         ENDDO
!
         z(jcore) = kcore
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!     END OF FILE ON XDICT - PREPARE TO PACK OUT COLUMNS IN CORE
!
 60      CALL close(xdict,optc)
         CALL close(xmat,optc)
         CALL gopen(outmat,z(ibuf1),optw)
!
!     PACK OUT COLUMNS
!
         DO i = ifcol , ilcol
            CALL bldpk(typin,typout,outmat,0,0)
            IF ( z(i)/=0 ) THEN
!
               iloc = z(i) + 1
               nloc = z(iloc-1) - iloc
               CALL pakcol(z(iloc),nloc)
            ENDIF
!
            CALL bldpkn(outmat,0,mcb)
         ENDDO
!
         CALL close(outmat,optc)
!
!     RETURN FOR ADDITIONAL PASS IF MORE NONZERO COLUMNS REMAIN
!
         optw = wrt
         IF ( ilcol<luset ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ALL COLUMNS PROCESSED - WRITE TRAILER AND RETURN
!
         CALL wrttrl(mcb)
         RETURN
!
!     ERROR CONDITIONS
!
 80      n = -1
!
         CALL mesage(n,file,name)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 100     n = -2
         CALL mesage(n,file,name)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 120     n = -3
         CALL mesage(n,file,name)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 140     n = -8
         CALL mesage(n,file,name)
         spag_nextblock_1 = 7
      CASE (7)
         DO
!
            WRITE (nout,99001) sfm , icode
99001       FORMAT (A25,' 8010, LOGIC ERROR IN SUBROUTINE FLBEMA - CODE',I3/)
            n = -61
            CALL mesage(n,file,name)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE flbema
