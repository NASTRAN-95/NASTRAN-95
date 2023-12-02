!*==extern.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE extern(Nex,Ngrav,Gvect,Ilist,Pg,N1,Iharm)
!
!     GENERATES EXTERNAL LOADS
!
   USE c_blank
   USE c_gpta1
   USE c_hmatdd
   USE c_loadx
   USE c_packx
   USE c_pindex
   USE c_system
   USE c_tranx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nex
   INTEGER :: Ngrav
   REAL , DIMENSION(1) :: Gvect
   INTEGER , DIMENSION(1) :: Ilist
   INTEGER , DIMENSION(1) :: Pg
   INTEGER :: N1
   INTEGER :: Iharm
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: all , file , flag , i , ib , ibuf1 , ibuf2 , ibuf3 , ibuf4 , ibuf5 , ido , iii , ij , ilsym , ip1 , ipre , iset ,     &
            & isetno , j , jopen , kcc , kkkk , kset , lcore , mcore , mset , n , nbdys , ncc , ncore , nelout , nextz , ngrold ,   &
            & nloop , nograv , nset , ret , setno
   INTEGER , SAVE :: casecc , hccens , hcflds , permbd , remfls , scr6
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , direct , eandm , fpont , fread , fwdrec , gopen , grav , mesage , open , pack , plbar1 , pload , pload3 ,       &
          & pload4 , ploadx , prehma , premat , presax , qhbdy , qloadl , qvol , read , rewind , rforce , skprec , sload , tpont
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   !>>>>EQUIVALENCE (Core(1),Iz(1))
   DATA casecc , permbd , hcflds , remfls , scr6 , hccens , name/110 , 112 , 304 , 305 , 306 , 307 , 4HEXTE , 4HRN  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         iest(1) = -1
         idum(1) = 0
         jopen = 0
         ipre = 0
         incur = 1
         ii = 1
         jj = nrowsp
         Ngrav = 0
         old = 0
         icm = 1
         itya = 1
         ityb = 1
         ibuf1 = lcare - sysbuf + 1
         ibuf2 = ibuf1 - sysbuf
         ibuf3 = ibuf2 - sysbuf
         ibuf4 = ibuf3 - sysbuf
         ibuf5 = ibuf4 - sysbuf
         lcore = ibuf5 - sysbuf
         CALL gopen(slt,core(ibuf1),0)
         CALL gopen(bgpdt,core(ibuf2),0)
         file = cstm
         CALL open(*20,cstm,core(ibuf3),0)
         icm = 0
         CALL skprec(cstm,1)
 20      CALL gopen(sil,core(ibuf4),0)
         file = slt
         isil = 0
         IF ( lcore<nrowsp ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         iii = 1
         DO nloop = 1 , N1
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
!
                  ilid = Ilist(iii)
                  IF ( ilid/=0 ) THEN
                     DO i = 1 , nrowsp
                        core(i) = 0.0
                     ENDDO
                     nograv = 0
                     ngrold = Ngrav
                  ELSE
                     CALL skprec(slt,1)
                     spag_nextblock_2 = 9
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
                  SPAG_Loop_2_1: DO
                     spag_nextblock_3 = 1
                     SPAG_DispatchLoop_3: DO
                        SELECT CASE (spag_nextblock_3)
                        CASE (1)
                           CALL read(*40,*38,slt,nobld,1,0,flag)
                           CALL fread(slt,ido,1,0)
                           IF ( nograv==1 ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( nobld==-20 ) EXIT SPAG_Loop_2_1
                           IF ( nobld==3 .OR. nobld==4 ) THEN
                              DO j = 1 , ido
                                 CALL tpont
                              ENDDO
                              CYCLE
                           ELSEIF ( nobld==5 .OR. nobld==6 ) THEN
                              DO j = 1 , ido
                                 CALL fpont
                              ENDDO
                              CYCLE
                           ELSEIF ( nobld==7 ) THEN
                              DO j = 1 , ido
                                 CALL sload
                              ENDDO
                              CYCLE
                           ELSEIF ( nobld==8 ) THEN
                              IF ( nograv==2 ) THEN
                                 spag_nextblock_1 = 2
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              DO j = 1 , ido
                                 CALL grav(Ngrav,Gvect(1),Nex,Ilist(1),nloop)
                              ENDDO
                              nograv = 1
                              CYCLE
                           ELSEIF ( nobld==9 ) THEN
                              DO j = 1 , ido
                                 CALL pload
                              ENDDO
                              CYCLE
                           ELSEIF ( nobld==10 ) THEN
!
!     RFORCE CARDS
!
                              DO j = 1 , ido
                                 CALL rforce(lcore)
                              ENDDO
                              CYCLE
                           ELSEIF ( nobld==11 ) THEN
!
!     PRESAX CARDS
!
                              DO j = 1 , ido
                                 CALL presax(Iharm)
                              ENDDO
                              CYCLE
                           ELSEIF ( nobld==12 ) THEN
!
!     QHBDY CARDS
!
                              DO j = 1 , ido
                                 CALL qhbdy
                              ENDDO
                              CYCLE
                           ELSEIF ( nobld==13 ) THEN
!
!     QVOL CARDS (MODIFIED USER ENTRYS)
!
                              DO j = 1 , ido
                                 CALL qvol
                              ENDDO
                              CYCLE
                           ELSEIF ( nobld==14 ) THEN
!
!     QBDY1 CARDS (MODIFIED USER ENTRYS)
!
                              kkkk = 1
                           ELSEIF ( nobld==15 ) THEN
!
!     QBDY2 CARDS (MODIFIED USER ENTRYS)
!
                              kkkk = 2
                           ELSEIF ( nobld==16 ) THEN
!
!     QVECT CARDS (MODIFIED USER ENTRYS)
!
                              kkkk = 3
                           ELSEIF ( nobld==17 ) THEN
!
!     PLOAD3 CARDS
!
                              DO j = 1 , ido
                                 CALL pload3
                              ENDDO
                              CYCLE
                           ELSEIF ( nobld==18 ) THEN
!
!     PLOAD1 CARDS
!
                              IF ( ipre==1 ) THEN
                                 spag_nextblock_3 = 2
                                 CYCLE SPAG_DispatchLoop_3
                              ENDIF
                              ipre = 1
                              lcore = lcore - sysbuf - 1
                              mcore = lcore - nrowsp - 1
                              IF ( lcore<nrowsp ) THEN
                                 spag_nextblock_1 = 3
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              CALL premat(core(nrowsp+1),core(nrowsp+1),core(lcore),mcore,ncore,mpt,idit)
                              spag_nextblock_3 = 2
                              CYCLE SPAG_DispatchLoop_3
                           ELSEIF ( nobld==19 ) THEN
!
!     PLOADX CARDS
!
                              DO j = 1 , ido
                                 CALL ploadx
                              ENDDO
                              CYCLE
                           ELSEIF ( nobld==20 .OR. nobld==21 .OR. nobld==22 .OR. nobld==23 .OR. nobld==24 ) THEN
                              EXIT SPAG_Loop_2_1
                           ELSEIF ( nobld==25 ) THEN
!
!     PLOAD4 CARDS
!
                              CALL pload4(ibuf5,ido,jopen)
                              CYCLE
                           ELSE
                              DO j = 1 , ido
                                 CALL direct
                              ENDDO
                              CYCLE
                           ENDIF
                           DO j = 1 , ido
                              CALL qloadl(kkkk)
                           ENDDO
                        CASE (2)
                           DO j = 1 , ido
                              CALL plbar1(ido,lcore)
                           ENDDO
                           EXIT SPAG_DispatchLoop_3
                        END SELECT
                     ENDDO SPAG_DispatchLoop_3
                  ENDDO SPAG_Loop_2_1
!
!     CEMLOOP, SPCFLD, GEMLOOP, MDIPOLE, AND REMFLUX CARDS
!
!     BRING HEAT MATERIALS INTO CORE
!
                  IF ( ipre==1 ) THEN
!
!     NO DO LOOP ON IDO. IN EANDM WE WILL READ ALL CARDS
!
                     CALL eandm(nobld,ido,nextz,lcore,nbdys,all,nelout)
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
                     ipre = 1
!
!     1ST AND LAST AVAILABLE LOCATIONS IN OPEN CORE
!
                     iihmat = nrowsp
                     nnhmat = lcore
                     mptfil = mpt
                     iditfl = idit
                     CALL prehma(core)
!
!     NOW NNHMAT CONTAINS LAST LOCATION OF MATERIAL INFO
!
                     nextz = nnhmat + 1
!
!     OPEN HCFLDS TO CONTAIN APPLIED MAGNETIC FIELD LOAD
!
                     lcore = lcore - sysbuf
                     IF ( lcore>nextz ) THEN
!
!     STORE SILS  ON PERMBDY, IF ANY, INTO OPEN CORE
!
                        nbdys = 0
                        file = permbd
                        CALL open(*24,permbd,core(lcore+1),0)
                        CALL fwdrec(*40,permbd)
                        CALL read(*40,*22,permbd,core(nextz),lcore-nextz+1,0,nbdys)
                     ENDIF
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
 22               CALL close(permbd,1)
 24               nextz = nextz + nbdys
!
!     NOW CHECK FOR FORCE REQUESTS ON CASECC(MAGNETIC FIELD REQUESTS)
!     MAKE A UNIQUE LIST OF ELEMENT ID-S CORRESPONDING TO ALL SUBCASES.
!     IF A SUBCASE REQUESTS ALL, NO LIST IS NECESSARY.
!
                  all = 0
                  nelout = 0
                  ij = 0
!
!     1ST GET MAXIMUM LENGTH OF CASE CONTROL IN ORDER TO STORE ELEMENT
!     ID-S
!
                  ncc = 0
                  CALL gopen(casecc,core(lcore+1),0)
                  spag_nextblock_2 = 3
               CASE (3)
                  CALL read(*28,*26,casecc,core(nextz),lcore-nextz+1,0,kcc)
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
 26               ncc = max0(ncc,kcc)
                  spag_nextblock_2 = 3
                  CYCLE SPAG_DispatchLoop_2
 28               CALL rewind(casecc)
                  CALL fwdrec(*40,casecc)
                  kset = nextz + ncc
                  spag_nextblock_2 = 4
               CASE (4)
!
                  CALL read(*36,*30,casecc,core(nextz),lcore-nextz+1,0,ncc)
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
 30               setno = iz(nextz+25)
                  IF ( setno==0 ) THEN
                     spag_nextblock_2 = 4
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  IF ( setno>0 ) THEN
!
!     CREATE UNIQUE LIST OF ELEMENT ID-S
!
                     ilsym = iz(nextz+165)
                     isetno = ilsym + iz(ilsym+nextz-1) + nextz
                     SPAG_Loop_2_2: DO
                        iset = isetno + 2
                        nset = iz(isetno+1) + iset - 1
                        IF ( iz(isetno)==setno ) THEN
!
!     PICK UP ELEMENT ID-S. STORE IN UNIQUE LIST
!
                           i = iset
                           spag_nextblock_2 = 6
                           CYCLE SPAG_DispatchLoop_2
                        ELSE
                           isetno = nset + 1
!
!     IF SET CANNOT BE FOUND, SET TO ALL. BUT SHOULD NOT HAPPEN
!
                           IF ( isetno>=ncc+nextz-1 ) EXIT SPAG_Loop_2_2
                        ENDIF
                     ENDDO SPAG_Loop_2_2
                  ENDIF
                  spag_nextblock_2 = 5
               CASE (5)
!
!     ALL
!
                  all = 1
                  nelout = 0
                  GOTO 36
               CASE (6)
                  IF ( i==nset ) THEN
                     spag_nextblock_2 = 7
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  IF ( iz(i+1)>0 ) THEN
                     spag_nextblock_2 = 7
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  ib = iz(i)
                  n = -iz(i+1)
                  i = i + 1
                  ASSIGN 32 TO ret
                  spag_nextblock_2 = 8
                  CYCLE SPAG_DispatchLoop_2
 32               ib = ib + 1
                  IF ( ib<=n ) THEN
                     spag_nextblock_2 = 8
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  GOTO 34
               CASE (7)
                  ib = iz(i)
                  ASSIGN 34 TO ret
                  spag_nextblock_2 = 8
                  CYCLE SPAG_DispatchLoop_2
 34               i = i + 1
!
!     DONE WITH THIS SET. GO BACK FOR ANOTHER
!
                  IF ( i<=nset ) THEN
                     spag_nextblock_2 = 6
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 4
               CASE (8)
!
!     SEARCH LIST OF ELEMENT ID-S. ADD ID TO LIST IF NOT A DUPLICATE
!
                  IF ( ij/=0 ) THEN
                     DO j = mset , ij
                        IF ( iz(j)==ib ) GOTO ret
                     ENDDO
                     ij = ij + 1
                     IF ( ij>=lcore ) THEN
                        spag_nextblock_2 = 5
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     iz(ij) = ib
                     nelout = nelout + 1
                     GOTO ret
                  ELSE
                     mset = kset
                     iz(mset) = ib
                     nelout = 1
                     ij = mset
                     GOTO ret
                  ENDIF
!
!     DONE WITH ALL CASES. IF ALL.NE.1, MOVE THE ID-S UP IN CORE
!
 36               CALL close(casecc,1)
                  IF ( all/=1 ) THEN
!
                     DO j = 1 , nelout
                        iz(nextz+j-1) = iz(mset+j-1)
                     ENDDO
                     nextz = nextz + nelout
                  ENDIF
!
                  CALL gopen(hcflds,core(lcore+1),1)
                  i = lcore - sysbuf
                  j = i - sysbuf
                  lcore = j - sysbuf
                  IF ( lcore<=nextz ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL gopen(remfls,core(i+1),1)
                  CALL gopen(hccens,core(j+1),1)
                  CALL gopen(scr6,core(lcore+1),1)
                  CALL eandm(nobld,ido,nextz,lcore,nbdys,all,nelout)
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
!
!
 38               IF ( ngrold/=Ngrav ) CYCLE
                  CALL pack(core,Pg,Pg(1))
                  spag_nextblock_2 = 9
               CASE (9)
                  iii = iii + 1
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO
!
         CALL close(bgpdt,1)
         IF ( icm==0 ) CALL close(cstm,1)
         CALL close(slt,1)
         CALL close(sil,1)
         IF ( ipre==1 ) THEN
            CALL close(hcflds,1)
            CALL close(remfls,1)
            CALL close(hccens,1)
            CALL close(scr6,1)
         ENDIF
         RETURN
!
!     FILE ERRORS
!
 40      ip1 = -2
         spag_nextblock_1 = 4
      CASE (2)
         ip1 = -7
         spag_nextblock_1 = 4
      CASE (3)
         ip1 = -8
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(ip1,file,name(1))
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE extern
