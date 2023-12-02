!*==sgenm.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sgenm(Ntype,Ifile,Sfile,Ofile,Icode,Ocode,Ctypes,Ctypeo)
   IMPLICIT NONE
   USE C_BLANK
   USE C_SGENCM
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ntype
   INTEGER :: Ifile
   INTEGER :: Sfile
   INTEGER :: Ofile
   INTEGER , DIMENSION(4,1) :: Icode
   INTEGER , DIMENSION(4,1) :: Ocode
   INTEGER , DIMENSION(2,8) :: Ctypes
   INTEGER , DIMENSION(2,8) :: Ctypeo
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(3) :: card
   REAL :: file
   INTEGER :: i , i1 , idx , j , k , m , nwds
   INTEGER , DIMENSION(3) , SAVE :: nlimit
   INTEGER , DIMENSION(2) , SAVE :: subnam
   EXTERNAL close , fread , fwdrec , gopen , mesage , read , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE MERGES CONVERTED SUBSTRUCTURING DATA WITH EXISTING
!     NASTRAN DATA
!
!     INPUTS
!     NTYPE  - NUMBER OF DIFFERENT SUBSTRUCTURING CARDS
!     IFILE  - INPUT FILE NAME
!     SFILE  - SCRATCH FILE NAME
!     OFILE  - OUTPUT FILE NAME
!     ICODE  - LOCATE CODES FOR INPUT CARD TYPES
!     OCODE  - LOCATE CODES FOR OUTPUT CARD TYPES
!     CTYPES - BCD NAMES OF SUBSTRUCTURING CARDS
!     CTYPEO - BCD NAMES OF CORRESPONDING NASTRAN CARDS
!
   DATA nlimit/3*2147483647/
   DATA subnam/4HSGEN , 4HM   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     OPEN FILES
!
         CALL gopen(Ifile,Z(Buf1),0)
         CALL gopen(Sfile,Z(Buf2),0)
         CALL gopen(Ofile,Z(Buf3),1)
!
!     READ HEADER FROM IFILE - DETERMINE IF SUBSTRUCTURING OR NASTRAN
!     CARD
!
         file = Ifile
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            CALL read(*80,*100,Ifile,card,3,0,idx)
            IF ( card(1)==nlimit(1) ) GOTO 40
            DO i = 1 , Ntype
               IF ( Icode(1,i)==card(1) .AND. Icode(2,i)==card(2) ) THEN
!
!     SKIP RECORD IF SUBSTRUCTURING CARD
!
                  CALL fwdrec(*40,Ifile)
                  CYCLE SPAG_Loop_1_1
               ENDIF
            ENDDO
            SPAG_Loop_2_2: DO i = 1 , Ntype
               IF ( Ocode(1,i)==card(1) .AND. Ocode(2,i)==card(2) ) THEN
!
!     FATAL ERROR IF BOTH SUBSTRUCTURING AND NASTRAN CARDS
!
                  IF ( Icode(4,i)==0 ) EXIT SPAG_Loop_2_2
                  Nono = 1
                  j = Ocode(4,i)
                  WRITE (Outt,99001) Ufm , Name , (Ctypes(k,j),k=1,2) , (Ctypeo(k,j),k=1,2)
!
99001             FORMAT (A23,' 6330, SOLUTION SUBSTRUCTURE ',2A4,3H - ,2A4,' AND ',2A4,' CARDS CANNOT BE USED TOGETHER.',/30X,     &
                         &'USE EITHER ONE, BUT NOT BOTH.')
                  CALL fwdrec(*40,Ifile)
                  CYCLE SPAG_Loop_1_1
               ENDIF
            ENDDO SPAG_Loop_2_2
!
!     COPY RECORD FROM IFILE TO OUTPUT
!
            CALL write(Ofile,card,3,0)
            DO
               CALL read(*80,*20,Ifile,Z,Nz,0,nwds)
               CALL write(Ofile,Z,Nz,0)
            ENDDO
         ENDDO SPAG_Loop_1_1
 20      CALL write(Ofile,Z,nwds,1)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     COPY RECORD FROM SFILE TO OUTPUT
!
 40      i1 = 1
         spag_nextblock_1 = 3
      CASE (3)
         DO i = i1 , Ntype
            IF ( Icode(4,i)==1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     CLOSE FILES
!
         CALL write(Ofile,nlimit,3,1)
         CALL close(Ifile,1)
         CALL close(Sfile,1)
         CALL close(Ofile,1)
         RETURN
      CASE (4)
         CALL fread(Sfile,card,3,0)
         CALL write(Ofile,Ocode(1,i),3,0)
         file = Sfile
         DO
            CALL read(*80,*60,Sfile,Z,Nz,0,nwds)
            CALL write(Ofile,Z,Nz,0)
         ENDDO
 60      CALL write(Ofile,Z,nwds,1)
         i1 = i + 1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     ERRORS
!
 80      m = -2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 100     m = -3
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(m,file,subnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sgenm
