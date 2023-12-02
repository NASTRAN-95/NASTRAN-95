!*==ssg1a.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssg1a(N1,Ilist,Nedt,Ntemp,Ncent,Casecc,Iharm)
   USE c_blank
   USE c_loadx
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: N1
   INTEGER , DIMENSION(1) :: Ilist
   INTEGER :: Nedt
   INTEGER :: Ntemp
   INTEGER :: Ncent
   INTEGER :: Casecc
   INTEGER :: Iharm
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(138) :: core
   REAL :: flag
   INTEGER :: i , iflag , ifound , ifrst , ione , ip1 , ip2 , islt , ispcn , j , k , lc1 , llist , mpcn , nogo
   INTEGER , DIMENSION(1080) :: icomb , idefml , itempl
   INTEGER , DIMENSION(2) , SAVE :: name , name1
   EXTERNAL close , fwdrec , mesage , open , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     ROUTINE ANALIZES CASECC AND SLT TO BUILD LISTS OF SELECTED
!     LOADS
!
   !>>>>EQUIVALENCE (Icore(1),Core(1))
   DATA name/4HSSG1 , 4HA   /
   DATA name1/4HSLT  , 4HSSG1/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     INITIALIZE.
!
         Nedt = 0
         Ntemp = 0
         Ncent = 0
         ifound = 0
         N1 = 0
         lc1 = lc - system
         islt = 0
         CALL open(*40,slt,core(lc1+1),0)
         islt = 1
         CALL read(*100,*20,slt,Ilist(1),-2,0,N1)
         CALL read(*100,*20,slt,Ilist(1),lc1,1,N1)
!
!     ALLOW FOR 360 LOADS
!
 20      IF ( N1>360 ) THEN
            name(2) = N1
            CALL mesage(-30,137,name)
         ENDIF
         lc1 = lc1 - system
         llist = N1
 40      CALL open(*120,Casecc,core(lc1+1),0)
         ione = 0
         DO i = 1 , loadnn
            CALL fwdrec(*120,Casecc)
         ENDDO
         ifrst = 0
         SPAG_Loop_1_1: DO
            CALL read(*60,*120,Casecc,core(1),166,1,flag)
            IF ( ifrst==0 ) THEN
               ifrst = 1
               ispcn = core(3)
               mpcn = core(2)
            ENDIF
!
!     TEST FOR SYMMETRY BUCKLING, OR DIFFERENTIAL STIFFNESS
!
            IF ( core(16)==0 .AND. core(5)==0 .AND. core(138)==0 ) THEN
               IF ( core(2)/=mpcn .OR. core(3)/=ispcn ) EXIT SPAG_Loop_1_1
               Iharm = core(136)
               ione = 1
               IF ( core(6)/=0 ) THEN
!
!     SEE IF EL DEFORM LOAD ALREADY APPLIED
!
                  IF ( Nedt/=0 ) THEN
                     DO i = 1 , Nedt
                        IF ( idefml(i)==core(6) ) GOTO 45
                     ENDDO
                  ENDIF
!
!     ADD TO LIST
!
                  Nedt = Nedt + 1
                  idefml(Nedt) = core(6)
               ENDIF
 45            IF ( core(7)/=0 ) THEN
!
!     SEE IF TEMP LOAD ALREADY APPLIED
!
                  IF ( itherm==0 ) THEN
                     IF ( Ntemp/=0 ) THEN
                        DO i = 1 , Ntemp
                           IF ( itempl(i)==core(7) ) GOTO 50
                        ENDDO
                     ENDIF
                     Ntemp = Ntemp + 1
                     itempl(Ntemp) = core(7)
                  ENDIF
               ENDIF
 50            IF ( core(4)/=0 ) THEN
                  IF ( islt==0 ) CALL mesage(-31,core(4),name1)
                  IF ( N1/=0 ) THEN
                     DO i = 1 , N1
                        IF ( core(4)==iabs(Ilist(i)) ) GOTO 55
                     ENDDO
                  ENDIF
!
!     MUST LOOK AT LOAD CARDS
!
                  ifound = ifound + 1
                  icomb(ifound) = core(4)
               ENDIF
               CYCLE
 55            Ilist(i) = -iabs(Ilist(i))
            ENDIF
         ENDDO SPAG_Loop_1_1
 60      CALL close(Casecc,1)
         IF ( ione==0 ) THEN
            WRITE (nout,99001)
99001       FORMAT ('0*** MISSING LOAD CARD IN CASE CONTROL')
            CALL mesage(-7,0,name)
            GOTO 140
         ELSE
            IF ( Ntemp/=0 ) THEN
               DO i = 1 , Ntemp
                  j = N1 + i
                  Ilist(j) = itempl(i)
               ENDDO
            ENDIF
            IF ( Nedt/=0 ) THEN
               DO i = 1 , Nedt
                  j = N1 + Ntemp + i
                  Ilist(j) = idefml(i)
               ENDDO
            ENDIF
            IF ( ifound==0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     LOOK AT LOAD CARDS
!
            DO i = 1 , N1
               CALL fwdrec(*100,slt)
            ENDDO
            i = 1
            nogo = 0
            CALL read(*140,*80,slt,core(1),lc1,1,iflag)
         ENDIF
 80      llist = N1 + Nedt + Ntemp
         IF ( llist==0 ) GOTO 140
         DO i = 1 , ifound
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  j = 1
                  SPAG_Loop_2_2: DO WHILE ( icomb(i)/=core(j) )
                     j = j + 6
                     DO WHILE ( j-1<=iflag )
                        IF ( core(j-1)==-1 ) CYCLE SPAG_Loop_2_2
                        j = j + 2
                     ENDDO
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDDO SPAG_Loop_2_2
                  j = j + 3
                  SPAG_Loop_2_3: DO WHILE ( core(j)/=-1 )
                     DO k = 1 , llist
                        IF ( core(j)==iabs(Ilist(k)) ) THEN
                           Ilist(k) = -iabs(Ilist(k))
                           j = j + 2
                           CYCLE SPAG_Loop_2_3
                        ENDIF
                     ENDDO
                     spag_nextblock_2 = 2
                     EXIT SPAG_Loop_2_3
                  ENDDO SPAG_Loop_2_3
               CASE (2)
                  CALL mesage(31,icomb(i),name1)
                  nogo = 1
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         IF ( nogo/=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         IF ( islt/=0 ) CALL close(slt,1)
         IF ( N1/=0 ) THEN
            DO i = 1 , N1
               IF ( Ilist(i)<0 ) THEN
                  Ilist(i) = -Ilist(i)
               ELSEIF ( Ilist(i)/=0 ) THEN
                  Ilist(i) = 0
               ENDIF
            ENDDO
         ENDIF
         RETURN
!
!     ERROR MESSAGES.
!
 100     ip1 = slt
         spag_nextblock_1 = 3
      CASE (3)
         ip2 = -1
         CALL mesage(ip2,ip1,name)
 120     ip1 = Casecc
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 140     ip2 = 31
         DO i = 1 , ifound
            ip1 = icomb(i)
            CALL mesage(ip2,ip1,name1)
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(-61,0,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE ssg1a
