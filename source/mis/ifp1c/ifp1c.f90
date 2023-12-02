!*==ifp1c.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp1c(I81,Nz)
   IMPLICIT NONE
   USE C_IFP1A
   USE C_SYSTEM
   USE C_XIFP1
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: I81
   INTEGER :: Nz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: core
   INTEGER , SAVE :: exce , thru
   REAL :: flag
   INTEGER :: i , i81o , iacip , ial1 , ibk , ibk1 , iexcpt , ifwd , ifwd1 , il1 , ilset , iput , ireal , iret , iset , ithru ,     &
            & jexcpt
   INTEGER , DIMENSION(2) , SAVE :: nifp1c
   EXTERNAL ifp1d , ifp1s , mesage , mvbits , page , read , xrcard , xread
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (Corex(1),Corey(1)) , (Core(1),Corey(401))
   DATA thru/4HTHRU/ , exce/4HEXCE/
   DATA nifp1c/4H IFP , 4H1C  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         i81o = I81
         core(I81+2) = Isub
         IF ( core(I81+3)/=-1 ) THEN
!
!     NO NAME FOR SET
!
            CALL ifp1d(-615)
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSE
            core(I81) = core(I81+4)
            ilset = I81 + 1
            core(ilset) = 0
!
!     FIND BEGINNING OF SET LIST
!
            I81 = I81 + 5
            IF ( core(I81)==Ieor ) THEN
!
!     UNEXPECTED END OF RECORD
!
               CALL ifp1d(-623)
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSE
               ireal = 0
               IF ( core(I81)>1 ) THEN
!
!     FOULED UP SET
!
                  CALL ifp1d(-614)
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  I81 = I81 + 3
                  IF ( core(I81)==Ieor ) THEN
                     CALL ifp1d(-623)
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     iput = ilset + 2
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
 20      ithru = 0
         iexcpt = 0
         spag_nextblock_1 = 2
      CASE (2)
         ASSIGN 20 TO iret
         IF ( core(I81)<0 ) THEN
            ithru = 0
            iexcpt = 0
         ELSEIF ( core(I81)==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         IF ( iabs(core(I81))/=1 ) ireal = 1
         core(iput) = core(I81+1)
         ibk1 = iabs(core(I81+1))
         I81 = I81 + 2
         iput = iput + 1
         core(ilset) = core(ilset) + 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!     CONTINUATION CARD
!
! ... ALLOW ON-LINE READ IF INTRA IS .GT. ZERO, SET BY ONLINS
!
         IF ( Intra<=0 ) THEN
            CALL read(*60,*60,Scr1,core(1),Nwpc,0,flag)
            WRITE (Otpe,99001) Icc , (core(i),i=1,Nwpc)
99001       FORMAT (11X,I8,6X,20A4)
            Icc = Icc + 1
            Line = Line + 1
            IF ( Line>=Nlpp ) CALL page
         ELSE
            CALL xread(*60,core(1))
            Icc = Icc + 1
         ENDIF
         I81 = iput
         Nz = Nz - core(ilset)
         CALL xrcard(core(I81),Nz,core(1))
         GOTO iret
      CASE (5)
!
!     THRU AND EXCEPT
!
         IF ( core(I81)==Ieor ) THEN
!
!     END OF RECORD
!
            I81 = iput
            IF ( core(ilset)<1 ) THEN
               CALL ifp1d(-614)
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( core(ilset)/=1 ) THEN
               IF ( ireal/=1 ) THEN
!
!     SORT LIST
!
                  iset = core(ilset)
                  CALL ifp1s(core(ilset+2),core(I81),core(ilset))
!
!     CORRECT FOR DELETIONS
!
                  I81 = I81 + core(ilset) - iset
               ENDIF
            ENDIF
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( ireal==1 ) CALL ifp1d(-622)
            IF ( Bit64 ) CALL mvbits(Blank,0,32,core(I81+1),0)
            IF ( core(I81+1)/=thru ) THEN
!
!     EXCEPT
!
               IF ( core(I81+1)/=exce ) THEN
                  CALL ifp1d(-614)
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( ithru==1 ) THEN
!
!     PROCESS EXCEPT CANDIDATES
!
                  I81 = I81 + 3
                  IF ( core(I81)==Ieor ) THEN
                     CALL ifp1d(-623)
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( iexcpt==1 ) THEN
!
!     EXCEPT FOLLOWED BY THRU
!
                     CALL ifp1d(-616)
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     iexcpt = 1
                     jexcpt = 0
                  ENDIF
               ELSE
!
!     EXCEPT WITHOUT THRU
!
                  CALL ifp1d(-613)
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( core(ilset)==0 ) THEN
               CALL ifp1d(-614)
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( core(iput-1)<0 ) THEN
               CALL ifp1d(-616)
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSE
               I81 = I81 + 3
               IF ( core(I81)==Ieor ) THEN
                  CALL ifp1d(-623)
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  ibk = ibk1
                  ifwd = core(I81+1)
                  ifwd1 = ifwd
                  IF ( ibk>=ifwd ) THEN
                     CALL ifp1d(-614)
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     ithru = 1
!     TEST FOR DEGENERATE THRU INTERVAL
                     IF ( ifwd-ibk/=1 ) core(I81+1) = -core(I81+1)
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
 40      SPAG_Loop_1_1: DO
            ASSIGN 40 TO iret
            IF ( core(I81)<0 ) THEN
               IF ( core(I81+1)>ifwd1 ) GOTO 20
               IF ( core(I81+1)<ibk ) THEN
                  CALL ifp1d(-614)
                  EXIT SPAG_Loop_1_1
               ELSEIF ( core(I81+1)<=core(I81-1) .AND. jexcpt==1 .AND. (core(I81+2)<=0 .OR. core(I81+2)==Ieor) ) THEN
                  CALL ifp1d(-626)
                  I81 = I81 + 2
               ELSE
                  jexcpt = 1
                  IF ( core(I81+1)==ibk ) THEN
!
!     EXCEPTING BEGINNING OF INTERVAL
!
                     ibk = ibk + 1
                     core(iput-2) = ibk
                     I81 = I81 + 2
                     IF ( ifwd-ibk==1 ) core(iput-1) = ifwd
                     IF ( ibk==ifwd ) THEN
                        iput = iput - 1
                        core(ilset) = core(ilset) - 1
                        ibk = 0
                        ifwd = 0
                     ENDIF
                  ELSEIF ( core(I81+1)==ifwd ) THEN
!
!     EXCEPT END OF INTERVAL
!
                     ifwd = ifwd - 1
                     core(iput-1) = -ifwd
                     I81 = I81 + 2
                     IF ( ifwd-ibk==1 ) core(iput-1) = ifwd
                     IF ( ibk==ifwd ) THEN
                        iput = iput - 1
                        core(ilset) = core(ilset) - 1
                     ENDIF
                     GOTO 20
                  ELSEIF ( core(I81+1)==ifwd1 ) THEN
!
!     EXCEPT PAST OLD END OF INTERVAL
!
                     I81 = I81 + 2
                     iput = iput - 1
                     core(ilset) = core(ilset) - 1
                     GOTO 20
                  ELSE
                     IF ( core(I81+1)-1==ibk ) THEN
!     EXCEPT ADJACENT TO BOTTOM OF INTERVAL
                        il1 = core(iput-1)
                        ibk = ibk + 2
                        core(iput-1) = ibk
                        ial1 = iabs(il1)
                        IF ( ial1-ibk==1 ) il1 = ial1
                        core(iput) = il1
                        IF ( ibk==ial1 ) THEN
                           ibk = 0
                           ifwd = 0
                           I81 = I81 + 2
                           CYCLE
                        ENDIF
                     ELSEIF ( core(I81+1)+1==ifwd ) THEN
!     EXCEPT ADJACENT TO TOP OF INTERVAL
                        core(iput) = iabs(core(iput-1))
                        ifwd = ifwd - 2
                        core(iput-1) = -ifwd
                        IF ( ifwd-ibk==1 ) core(iput-1) = ifwd
                     ELSE
!     EXCEPT IN MIDDLE OF INTERVAL
                        core(iput-1) = -core(I81+1) + 1
                        iacip = iabs(core(iput-1))
                        IF ( iacip-ibk==1 ) core(iput-1) = iacip
                        core(iput) = core(I81+1) + 1
                        core(iput+1) = -ifwd
                        IF ( ifwd-core(iput)==1 ) core(iput+1) = ifwd
                        ibk = core(iput)
                        I81 = I81 + 2
                        iput = iput + 2
                        core(ilset) = core(ilset) + 2
                        CYCLE
                     ENDIF
                     iput = iput + 1
                     I81 = I81 + 2
                     core(ilset) = core(ilset) + 1
                  ENDIF
               ENDIF
            ELSEIF ( core(I81)==0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSE
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 6
      CASE (6)
         I81 = i81o
         Nset = Nset - 1
         spag_nextblock_1 = 7
      CASE (7)
         RETURN
 60      DO
            CALL mesage(-1,Scr1,nifp1c)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ifp1c
