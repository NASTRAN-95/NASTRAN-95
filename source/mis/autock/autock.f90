!*==autock.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE autock(Iadd)
   USE c_autocm
   USE c_autohd
   USE c_xgpi4
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iadd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: casecc , casei , casess , xchk
   INTEGER :: i , ib , ifin , incr , iop , ist , j , mi , n2 , ndb , nlist , nopf , nwd , osbot , ospnt , osprc
   INTEGER , SAVE :: iblank
   INTEGER , DIMENSION(100) :: list
   INTEGER , DIMENSION(1) :: oscar
   EXTERNAL lshift , rshift , xlnkhd
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE GENERATES A CHKPT OSCAR RECORD WHEN THE PRECHK
!     OPTION IS BEING USED, THE ADDRESS IADD IS THE STARTING
!     LOCATION OF THE OUTPUT FILE NAMES TO BE TESTED
!
   !>>>>EQUIVALENCE (Loscar,Core(1),Os(1)) , (Osprc,Os(2)) , (Osbot,Os(3)) , (Ospnt,Os(4)) , (Oscar(1),Os(5))
   DATA casess/4HCASE , 4HSS  /
   DATA casecc/4HCASE , 4HCC  /
   DATA casei/4HCASE , 4HI   /
   DATA xchk/4HXCHK , 4H    /
   DATA iblank/0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ihead = 0
         iop = 0
         IF ( preflg<=0 ) THEN
!
!     PURGE OR EQUIV DATA BLOCK LIST MUST BE CHECKED
!
            nwd = oscar(ospnt)
            mi = rshift(oscar(ospnt+2),16)
            ib = ospnt + 6
            preflg = iabs(preflg)
            ndb = oscar(ib)
            iop = 1
            IF ( mi==9 ) ist = ib + 1
            IF ( mi==10 ) ist = ib + 4
            IF ( mi==9 ) ifin = ist + 2*ndb - 1
            IF ( mi==10 ) ifin = ist + 2*ndb - 3
            nwd = nwd - 6
            incr = 2
            nlist = 0
            IF ( preflg==1 ) THEN
            ELSEIF ( preflg==2 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( preflg==3 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSE
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            preflg = iabs(preflg)
            nopf = oscar(Iadd)
            nwd = 3*nopf
            ist = Iadd + 1
            ifin = ist + nwd - 1
            nlist = 0
            incr = 3
!
            IF ( preflg==2 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( preflg==3 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     CHECK OUTPUT FILE AGAINST LIST
!
         n2 = 2*nnames
         DO i = ist , ifin , incr
            DO j = 1 , n2 , 2
               IF ( prenam(j)/=casess(1) .OR. prenam(j+1)/=casess(2) ) THEN
                  IF ( prenam(j)/=casecc(1) .OR. prenam(j+1)/=casecc(2) ) THEN
                     IF ( prenam(j)/=casei(1) .OR. prenam(j+1)/=casei(2) ) THEN
                        IF ( prenam(j)==oscar(i) .AND. prenam(j+1)==oscar(i+1) ) THEN
                           nlist = nlist + 1
                           list(2*nlist-1) = oscar(i)
                           list(2*nlist) = oscar(i+1)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         IF ( iop==1 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nlist==0 ) RETURN
         spag_nextblock_1 = 6
      CASE (3)
!
!     PREFLG=ALL OPTION, CHECKPOINT ALL OUTPUT DATA BLOCKS
!
         DO i = ist , ifin , incr
            IF ( oscar(i)/=iblank .OR. oscar(i+1)/=iblank ) THEN
               IF ( oscar(i)/=casess(1) .OR. oscar(i+1)/=casess(2) ) THEN
                  IF ( oscar(i)/=casecc(1) .OR. oscar(i+1)/=casecc(2) ) THEN
                     IF ( oscar(i)/=casei(1) .OR. oscar(i+1)/=casei(2) ) THEN
                        nlist = nlist + 1
                        list(2*nlist-1) = oscar(i)
                        list(2*nlist) = oscar(i+1)
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         IF ( iop==1 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (4)
!
!     CHECK OUTPUT FILES EXCEPT THOSE IN LIST
!
         n2 = 2*nnames
         SPAG_Loop_1_1: DO i = ist , ifin , incr
            DO j = 1 , n2 , 2
               IF ( prenam(j)==oscar(i) .AND. prenam(j+1)==oscar(i+1) ) CYCLE SPAG_Loop_1_1
            ENDDO
            IF ( oscar(i)/=iblank .OR. oscar(i+1)/=iblank ) THEN
               IF ( oscar(i)/=casess(1) .OR. oscar(i+1)/=casess(2) ) THEN
                  IF ( oscar(i)/=casecc(1) .OR. oscar(i+1)/=casecc(2) ) THEN
                     IF ( oscar(i)/=casei(1) .OR. oscar(i+1)/=casei(2) ) THEN
                        nlist = nlist + 1
                        list(2*nlist-1) = oscar(i)
                        list(2*nlist) = oscar(i+1)
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
         IF ( iop/=1 ) THEN
            IF ( nlist==0 ) RETURN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         nwd = nwd - 2*ndb - 2
         IF ( mi==10 ) nwd = nwd - 1
         IF ( nwd>0 .OR. nlist==0 ) THEN
            IF ( nwd<=0 .AND. nlist==0 ) THEN
               ihead = 0
               RETURN
            ELSE
               ndb = oscar(ifin+2)
               IF ( mi==9 ) ist = ifin + 3
               IF ( mi==10 ) ist = ifin + 6
               ifin = ist + 2*ndb - 1
               IF ( mi==10 ) ifin = ifin - 2
               IF ( preflg==1 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( preflg==2 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( preflg==3 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     UPDATE OSCAR PARAMETERS
!
         ihead = 1
         osprc = osbot
         osbot = oscar(osbot) + osbot
         ospnt = osbot
         iseqn = oscar(osprc+1) + 1
!
!     LOAD HEADER
!
         oscar(ospnt) = 6
         oscar(ospnt+1) = iseqn
         oscar(ospnt+2) = 4 + lshift(3,16)
         oscar(ospnt+3) = xchk(1)
         oscar(ospnt+4) = xchk(2)
         oscar(ospnt+5) = dmpcnt
         IF ( iop==1 ) oscar(ospnt+5) = oscar(ospnt+5) - 1
         oscar(ospnt+6) = nlist
         CALL xlnkhd
         IF ( nlist/=0 ) THEN
!
!     LOAD CHKPNT INFORMATION
!
            nlist = 2*nlist
            DO i = 1 , nlist , 2
               oscar(ospnt+6+i) = list(i)
               oscar(ospnt+7+i) = list(i+1)
            ENDDO
         ENDIF
         oscar(ospnt) = oscar(ospnt) + nlist + 1
         ihead = 0
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE autock
