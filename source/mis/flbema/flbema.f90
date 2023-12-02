!*==flbema.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE flbema(Type)
   IMPLICIT NONE
   USE C_FLBFIL
   USE C_FLBPTR
   USE C_NAMES
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
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
            outmat = Dkgg
            xmat = Kgmat
            xdict = Kgdict
         ELSE
!
!     AF MATRIX
!
            outmat = Af
            xmat = Afmat
            xdict = Afdict
         ENDIF
!
!     ALLOCATE COLUMN POINTER VECTOR IN TOP OF CORE
!
         mcb(1) = Uset
         CALL rdtrl(mcb)
         luset = mcb(3)
         icol = 1
         ncol = luset
         DO i = 1 , ncol
            Z(i) = 0
         ENDDO
!
!     INITILIZE OPEN AND CLOSE OPTIONS
!
         optw = Wrtrew
         optc = Norew
!
!     POSITION CONNECT FILE TO PROPER RECORD
!
         file = Conect
         CALL open(*80,Conect,Z(Ibuf1),Rdrew)
         IF ( Type==2 ) CALL skpfil(Conect,1)
         CALL fwdrec(*100,Conect)
         CALL close(Conect,Norew)
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
         Icore = ncol + 1
         Lcore = Ibuf2 - 1
         ncore = Lcore - Icore
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
         jcore = Icore
         file = Conect
!
         CALL gopen(Conect,Z(Ibuf1),Rd)
!
         IF ( .NOT.(skip) ) CALL read(*20,*140,Conect,alloc,3,1,n)
         DO
!
            Isil = alloc(1)
            Z(Isil) = jcore
            Z(jcore) = jcore + 1
            jcore = jcore + 1 + alloc(2) + 2*alloc(3)
            IF ( jcore>Lcore ) THEN
!
!     INSUFFICIENT CORE FOR NEXT COLUMN - SET FLAG TO SAVE CURRENT
!     CONECT ALLOCATION RECORD
!
               skip = .TRUE.
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSE
               ilcol = Isil
               CALL read(*20,*140,Conect,alloc,3,1,n)
            ENDIF
         ENDDO
!
!     END OF RECORD ON CONECT - ALL COLUMNS ALLOCATED
!
 20      ilcol = luset
         optc = Rew
         spag_nextblock_1 = 3
      CASE (3)
!
         CALL close(Conect,optc)
!
!     OPEN DICTIONARY AND MATRIX FILES AND PREPARE TO MAKE PASS
!
         CALL gopen(xdict,Z(Ibuf1),Rdrew)
         CALL gopen(xmat,Z(Ibuf2),Rdrew)
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
            Isil = dict(1)
            IF ( Isil>=ifcol .AND. Isil<=ilcol ) THEN
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
            IF ( colsil(kcol)==Isil ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         icode = 2
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
!
         iloc = (kcol-1)*nrow*ntpers + 1
!
!     EXTRACT MATRIX TERMS AND STORE THEM IN CORE
!
         icode = 3
         jcore = Z(Isil)
         IF ( jcore==0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         kcore = Z(jcore)
         DO i = 1 , nrow
            Z(kcore) = rowsil(i)
            IF ( ntpers==2 ) Z(kcore) = -rowsil(i)
            kcore = kcore + 1
            DO j = 1 , ntpers
               Z(kcore) = terms(iloc)
               iloc = iloc + 1
               kcore = kcore + 1
            ENDDO
         ENDDO
!
         Z(jcore) = kcore
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!     END OF FILE ON XDICT - PREPARE TO PACK OUT COLUMNS IN CORE
!
 60      CALL close(xdict,optc)
         CALL close(xmat,optc)
         CALL gopen(outmat,Z(Ibuf1),optw)
!
!     PACK OUT COLUMNS
!
         DO i = ifcol , ilcol
            CALL bldpk(typin,typout,outmat,0,0)
            IF ( Z(i)/=0 ) THEN
!
               iloc = Z(i) + 1
               nloc = Z(iloc-1) - iloc
               CALL pakcol(Z(iloc),nloc)
            ENDIF
!
            CALL bldpkn(outmat,0,mcb)
         ENDDO
!
         CALL close(outmat,optc)
!
!     RETURN FOR ADDITIONAL PASS IF MORE NONZERO COLUMNS REMAIN
!
         optw = Wrt
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
            WRITE (Nout,99001) Sfm , icode
99001       FORMAT (A25,' 8010, LOGIC ERROR IN SUBROUTINE FLBEMA - CODE',I3/)
            n = -61
            CALL mesage(n,file,name)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE flbema
