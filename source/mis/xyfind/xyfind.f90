!*==xyfind.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xyfind(*,*,*,Majid,Idz)
   USE c_xywork
   USE c_zzzzzz
   IMPLICIT NONE
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
         IF ( subc(file)>=0 ) THEN
            IF ( knt<0 ) THEN
               isav = idin(4)
               DO
                  CALL read(*20,*60,ifile,idin(1),146,1,flag)
                  IF ( isav==idin(4) ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL fwdrec(*40,ifile)
               ENDDO
            ELSEIF ( knt/=0 ) THEN
               isav = idin(4)
               DO
                  CALL read(*20,*60,ifile,idin(1),146,1,flag)
                  IF ( idin(4)/=isav ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL fwdrec(*40,ifile)
               ENDDO
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL rewind(ifile)
         CALL fwdrec(*40,ifile)
         vecid(file) = 0
         spag_nextblock_1 = 3
      CASE (3)
         CALL read(*20,*60,ifile,idin(1),146,eor,flag)
         spag_nextblock_1 = 4
      CASE (4)
         IF ( major==idin(2) ) THEN
            IF ( subc(file)==0 ) THEN
!
!     MATCH ON MAJOR ID MADE
!
               vecid(file) = vector
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( subc(file)==idin(4) ) THEN
               vecid(file) = vector
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         CALL fwdrec(*40,ifile)
         k = k + 1
         spag_nextblock_1 = 3
      CASE (5)
         SPAG_Loop_1_1: DO
            IF ( idin(5)/10/=z(Idz) ) THEN
               itemp = -1
!
!     IF RANDOM CHECK COMPONENT FOR MATCH
!
            ELSEIF ( .NOT.(z(Idz+1)/=idin(6) .AND. random) ) THEN
               IF ( subc(file)==0 ) RETURN
               IF ( subc(file)==idin(4) ) RETURN
            ENDIF
            CALL fwdrec(*40,ifile)
            CALL read(*20,*60,ifile,idin(1),146,eor,flag)
            IF ( major/=idin(2) ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
!
!     ELEMENT DATA ARE NOT IN ASCENDING SORT LIKE GRID DATA, BUT ARE
!     SORTED BY ELEMENT NAME, THEN BY ELEMENT NUMBER.
!     SINCE IT IS POSSIBLE FOR THE DESIRED ELEMENT TO BE AHEAD OF THE
!     CURRENT POSITION OF FILE, REWIND AND TRY AGAIN TO FIND MISSING
!     ELEMENT DATA FOR FORCES AND STRESSES.
!
 20      IF ( .NOT.(knt==0 .OR. retry .OR. subc(file)==0) ) THEN
            retry = .TRUE.
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( subc(file)/=0 ) THEN
!
            vecid(file) = 0
            Idz = itemp
            CALL rewind(ifile)
            CALL fwdrec(*40,ifile)
            RETURN 3
         ELSE
            subc(file) = -1
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
