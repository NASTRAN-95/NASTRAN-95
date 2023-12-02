!*==ssg1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssg1
   USE c_blank
   USE c_loads
   USE c_loadx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , flag
   REAL , DIMENSION(1) :: ary
   INTEGER :: casecc , i , icr2 , icr3 , iflag , ifrst , iharm , inull , ip1 , ip2 , ipont , ipont1 , islt , ispcn , j , k , lc1 ,  &
            & mpcn , n1 , n1a , ncent , nedt , newslt , nex , ngrav , nllst , ntemp , nwds
   INTEGER , DIMENSION(166) :: core
   REAL , DIMENSION(2) :: defml
   REAL , DIMENSION(1080) :: gvect
   INTEGER , DIMENSION(1) :: iary
   INTEGER , DIMENSION(2) :: idefml
   INTEGER , DIMENSION(360) :: ilist
   INTEGER , DIMENSION(4) , SAVE :: iword
   INTEGER , DIMENSION(7) :: mcb , pg
   INTEGER , DIMENSION(2) , SAVE :: subnam
   EXTERNAL bckrec , close , combin , edtl , extern , fwdrec , gravl1 , gravl2 , korsz , mesage , open , rdtrl , read , ssg1a ,     &
          & ssg2b , ssgslt , templ , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (Core(1),Icore(1),Iary(1),Ary(1)) , (defml(1),idefml(1))
   DATA iword/4 , 6 , 7 , 162/
   DATA subnam/4HSSG1 , 4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     MODIFY OPEN CORE POINTER IPTR FOR MAGNETICS PROBLEM
!
         iptr = max0(nrowsp,166)
         mcb(1) = 105
         CALL rdtrl(mcb(1))
         IF ( mcb(1)>0 ) iptr = max0(3*nrowsp,3*mcb(2),166)
!
!     INITIALIZE.
!
         lc = korsz(icore(1))
         nllst = lc - 2*sysbuf
         slt = 101
         bgpdt = 102
         cstm = 103
         sil = 104
         ecpt = 105
         mpt = 106
         gptt = 107
         edt = 108
         mass = 109
         casecc = 110
         idit = 111
         lodc = 201
!             205 = NEWSLT (THERMAL)
         pg(1) = 301
         icr2 = 302
         icr3 = 303
         DO i = 2 , 7
            pg(i) = 0
         ENDDO
         pg(3) = nrowsp
         pg(4) = 2
         pg(5) = 1
!
!     AVOID CALCULATING UNUSED LOADS
!
!     NEDT  = NUMBER OF ELEMENT  DEFORMATIONS
!     NTEMP = NUMBER OF THERMAL LOADS
!     NCENT = NUMBER OF CENTRIFUGAL LOADS
!
         CALL ssg1a(n1,ilist(1),nedt,ntemp,ncent,casecc,iharm)
         n1a = n1 + 1
         lc = lc - sysbuf
         CALL open(*80,pg(1),icore(lc+1),1)
         CALL write(pg(1),pg(1),2,1)
         ngrav = 0
         nex = n1 + ntemp + nedt + ncent
         IF ( n1/=0 ) THEN
!
!     MODIFY SLT -QVOL-, -QBDY1-, -QBDY2-, AND -QVECT- CARDS.
!
            newslt = icr3
            IF ( itherm/=0 ) newslt = 205
            islt = slt
            CALL ssgslt(slt,newslt,ecpt)
            slt = newslt
            CALL extern(nex,ngrav,gvect(1),ilist(1),pg(1),n1,iharm)
!
!     RESET -SLT- TO ORIGINAL SLT DATA BLOCK
!
            slt = islt
            n1 = n1 - ngrav
         ENDIF
         IF ( ntemp/=0 ) THEN
            CALL templ(ntemp,ilist(n1+1),pg(1))
            n1 = n1 + ntemp
         ENDIF
         IF ( nedt/=0 ) THEN
            CALL edtl(nedt,ilist(n1+1),pg(1))
            n1 = n1 + nedt
         ENDIF
         CALL close(pg,1)
         CALL wrttrl(pg(1))
         IF ( ngrav/=0 ) THEN
!
!     CHECK TO SEE IF THE MASS MATRIX IS PURGED
!
            mcb(1) = mass
            CALL rdtrl(mcb(1))
            IF ( mcb(1)<=0 ) CALL mesage(-56,0,iword)
            CALL gravl1(ngrav,gvect(1),icr2,iharm)
!
!     USE LOAD FILE AS SCRATCH NOTHING ON IT NOW
!
            CALL ssg2b(mass,icr2,0,icr3,0,1,1,lodc)
            CALL gravl2(ngrav,icr3,pg(1))
            n1 = n1 + ngrav
         ENDIF
         ipont1 = iptr + 2
         ipont = iptr + 1
         nload = 0
         DO i = 1 , nllst
            iary(i) = 0
         ENDDO
         CALL open(*100,casecc,icore(lc+1),0)
         lc1 = lc - sysbuf
         islt = 0
         CALL open(*20,slt,icore(lc1+1),0)
         islt = 1
         DO i = 1 , n1a
            CALL fwdrec(*60,slt)
         ENDDO
 20      DO i = 1 , loadnn
            CALL fwdrec(*100,casecc)
         ENDDO
         ifrst = 0
         SPAG_Loop_1_1: DO
            CALL read(*40,*40,casecc,core(1),166,1,flag)
            IF ( ifrst==0 ) THEN
               ifrst = 1
               ispcn = core(3)
               mpcn = core(2)
            ENDIF
!
!     TEST FOR SYMMETRY, BUCKLING OR DIFFERENTIAL STIFFNESS.
!
            IF ( core(16)==0 .AND. core(5)==0 .AND. core(138)==0 ) THEN
               IF ( core(3)/=ispcn .OR. core(2)/=mpcn ) EXIT SPAG_Loop_1_1
               inull = 0
               SPAG_Loop_2_2: DO k = 1 , 4
                  i = iword(k)
                  IF ( itherm==0 .OR. i/=7 ) THEN
                     IF ( core(i)==0 ) CYCLE
                     DO j = 1 , n1
                        IF ( core(i)==ilist(j) ) GOTO 22
                     ENDDO
!
!     COMBINATION CARD
!
                     inull = 1
                     DO
                        CALL read(*60,*120,slt,idefml(1),2,0,iflag)
                        IF ( core(i)==idefml(1) ) THEN
                           a = defml(2)
                           DO
                              CALL read(*60,*120,slt,idefml(1),2,0,iflag)
                              IF ( idefml(2)==-1 ) THEN
                                 CALL bckrec(slt)
                                 CYCLE SPAG_Loop_2_2
                              ELSE
                                 IF ( ipont+1>nllst ) THEN
                                    spag_nextblock_1 = 4
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                                 iary(ipont) = iary(ipont) + 1
                                 iary(ipont1) = idefml(2)
                                 ary(ipont1+1) = a*defml(1)
                                 ipont1 = ipont1 + 2
                              ENDIF
                           ENDDO
                        ELSE
                           SPAG_Loop_4_3: DO
                              CALL read(*60,*120,slt,idefml(1),2,0,iflag)
                              IF ( idefml(2)==-1 ) EXIT SPAG_Loop_4_3
                           ENDDO SPAG_Loop_4_3
                        ENDIF
                     ENDDO
 22                  iary(ipont) = iary(ipont) + 1
                     IF ( ipont+1>nllst ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     iary(ipont1) = core(i)
                     ary(ipont1+1) = 1.0
                     ipont1 = ipont1 + 2
                     inull = 1
                  ENDIF
               ENDDO SPAG_Loop_2_2
               IF ( inull==0 ) THEN
!
                  iary(ipont) = 1
                  IF ( ipont+1>nllst ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  iary(ipont1) = -1
                  ary(ipont1+1) = 1.0
                  ipont1 = ipont1 + 2
               ENDIF
               ipont = ipont + iary(ipont)*2 + 1
               nload = nload + 1
               ipont1 = ipont1 + 1
            ENDIF
         ENDDO SPAG_Loop_1_1
 40      CALL close(casecc,1)
         IF ( islt==1 ) CALL close(slt,1)
         CALL combin(pg(1),ilist(1),n1)
         RETURN
!
 60      ip1 = slt
         spag_nextblock_1 = 2
      CASE (2)
         ip2 = -1
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(ip2,ip1,subnam)
         ip1 = casecc
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 80      ip1 = pg(1)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 100     ip1 = casecc
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 120     ip2 = -2
         ip1 = slt
         spag_nextblock_1 = 3
      CASE (4)
!
         i = icore(i)
         nwds = 0
         DO
            CALL read(*120,*140,slt,core(1),lc,0,iflag)
            nwds = nwds + lc
         ENDDO
 140     nwds = nwds + iflag
         WRITE (iotpe,99001) ufm , i , nllst , nwds
99001    FORMAT (A23,' 3176, INSUFFICIENT OPEN CORE AVAILABLE TO PROCESS ','ALL LOAD CARD COMBINATIONS IN MODULE SSG1.',/32X,       &
                &'CURRENT LOAD ID BEING PROCESSED IS',I9,1H.,/32X,'OPEN CORE AVAILABLE IS',I9,' WORDS.',/32X,                       &
                &'ADDITIONAL OPEN CORE REQUIRED IS',I9,' WORDS.')
         ip1 = 0
         ip2 = -61
         spag_nextblock_1 = 3
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ssg1
