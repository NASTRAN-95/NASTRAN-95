!*==xyfind.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xyfind(Majid,Idz) !HIDESTARS (*,*,*,Majid,Idz)
   IMPLICIT NONE
   USE C_XYWORK
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(11) :: Majid
   INTEGER :: Idz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: eor
   INTEGER :: flag , isav , itemp , k
   LOGICAL :: retry
   EXTERNAL fwdrec , read , rewind
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   DATA eor/1/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     THIS SUBROUTINE LOCATES THE ID RECORD FOR A PARTICULAR ELEMENT OR
!     POINT ID AND IF THIS IS A RANDOM PLOT IT CONSIDERS THE COMPONENT
!
         k = 1
         retry = .FALSE.
         itemp = Idz
         IF ( Subc(File)>=0 ) THEN
            IF ( Knt<0 ) THEN
               isav = Idin(4)
               DO
                  CALL read(*20,*60,Ifile,Idin(1),146,1,flag)
                  IF ( isav==Idin(4) ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL fwdrec(*40,Ifile)
               ENDDO
            ELSEIF ( Knt/=0 ) THEN
               isav = Idin(4)
               DO
                  CALL read(*20,*60,Ifile,Idin(1),146,1,flag)
                  IF ( Idin(4)/=isav ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL fwdrec(*40,Ifile)
               ENDDO
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL rewind(Ifile)
         CALL fwdrec(*40,Ifile)
         Vecid(File) = 0
         spag_nextblock_1 = 3
      CASE (3)
         CALL read(*20,*60,Ifile,Idin(1),146,eor,flag)
         spag_nextblock_1 = 4
      CASE (4)
         IF ( Major==Idin(2) ) THEN
            IF ( Subc(File)==0 ) THEN
!
!     MATCH ON MAJOR ID MADE
!
               Vecid(File) = Vector
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Subc(File)==Idin(4) ) THEN
               Vecid(File) = Vector
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         CALL fwdrec(*40,Ifile)
         k = k + 1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
         SPAG_Loop_1_1: DO
            IF ( Idin(5)/10/=Z(Idz) ) THEN
               itemp = -1
!
!     IF RANDOM CHECK COMPONENT FOR MATCH
!
            ELSEIF ( .NOT.(Z(Idz+1)/=Idin(6) .AND. Random) ) THEN
               IF ( Subc(File)==0 ) RETURN
               IF ( Subc(File)==Idin(4) ) RETURN
            ENDIF
            CALL fwdrec(*40,Ifile)
            CALL read(*20,*60,Ifile,Idin(1),146,eor,flag)
            IF ( Major/=Idin(2) ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
!
!     ELEMENT DATA ARE NOT IN ASCENDING SORT LIKE GRID DATA, BUT ARE
!     SORTED BY ELEMENT NAME, THEN BY ELEMENT NUMBER.
!     SINCE IT IS POSSIBLE FOR THE DESIRED ELEMENT TO BE AHEAD OF THE
!     CURRENT POSITION OF FILE, REWIND AND TRY AGAIN TO FIND MISSING
!     ELEMENT DATA FOR FORCES AND STRESSES.
!
 20      IF ( .NOT.(Knt==0 .OR. retry .OR. Subc(File)==0) ) THEN
            retry = .TRUE.
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Subc(File)/=0 ) THEN
!
            Vecid(File) = 0
            Idz = itemp
            CALL rewind(Ifile)
            CALL fwdrec(*40,Ifile)
            RETURN 3
         ELSE
            Subc(File) = -1
            RETURN
         ENDIF
!
!     EOF HIT WHEN AN EOF SHOULD NOT HAVE BEEN HIT
!
 40      RETURN 1
!
!     EOR HIT WHEN AN EOR SHOULD NOT HAVE BEEN HIT
!
 60      RETURN 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE xyfind
