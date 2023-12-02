!*==extern.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE extern(Nex,Ngrav,Gvect,Ilist,Pg,N1,Iharm)
!
!     GENERATES EXTERNAL LOADS
!
   IMPLICIT NONE
   USE C_BLANK
   USE C_GPTA1
   USE C_HMATDD
   USE C_LOADX
   USE C_PACKX
   USE C_PINDEX
   USE C_SYSTEM
   USE C_TRANX
   USE C_ZZZZZZ
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
         Iest(1) = -1
         Idum(1) = 0
         jopen = 0
         ipre = 0
         Incur = 1
         Ii = 1
         Jj = Nrowsp
         Ngrav = 0
         Old = 0
         Icm = 1
         Itya = 1
         Ityb = 1
         ibuf1 = Lcare - Sysbuf + 1
         ibuf2 = ibuf1 - Sysbuf
         ibuf3 = ibuf2 - Sysbuf
         ibuf4 = ibuf3 - Sysbuf
         ibuf5 = ibuf4 - Sysbuf
         lcore = ibuf5 - Sysbuf
         CALL gopen(Slt,Core(ibuf1),0)
         CALL gopen(Bgpdt,Core(ibuf2),0)
         file = Cstm
         CALL open(*20,Cstm,Core(ibuf3),0)
         Icm = 0
         CALL skprec(Cstm,1)
 20      CALL gopen(Sil,Core(ibuf4),0)
         file = Slt
         Isil = 0
         IF ( lcore<Nrowsp ) THEN
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
                  Ilid = Ilist(iii)
                  IF ( Ilid/=0 ) THEN
                     DO i = 1 , Nrowsp
                        Core(i) = 0.0
                     ENDDO
                     nograv = 0
                     ngrold = Ngrav
                  ELSE
                     CALL skprec(Slt,1)
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
                           CALL read(*40,*38,Slt,Nobld,1,0,flag)
                           CALL fread(Slt,ido,1,0)
                           IF ( nograv==1 ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( Nobld==-20 ) EXIT SPAG_Loop_2_1
                           IF ( Nobld==3 .OR. Nobld==4 ) THEN
                              DO j = 1 , ido
                                 CALL tpont
                              ENDDO
                              CYCLE
                           ELSEIF ( Nobld==5 .OR. Nobld==6 ) THEN
                              DO j = 1 , ido
                                 CALL fpont
                              ENDDO
                              CYCLE
                           ELSEIF ( Nobld==7 ) THEN
                              DO j = 1 , ido
                                 CALL sload
                              ENDDO
                              CYCLE
                           ELSEIF ( Nobld==8 ) THEN
                              IF ( nograv==2 ) THEN
                                 spag_nextblock_1 = 2
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              DO j = 1 , ido
                                 CALL grav(Ngrav,Gvect(1),Nex,Ilist(1),nloop)
                              ENDDO
                              nograv = 1
                              CYCLE
                           ELSEIF ( Nobld==9 ) THEN
                              DO j = 1 , ido
                                 CALL pload
                              ENDDO
                              CYCLE
                           ELSEIF ( Nobld==10 ) THEN
!
!     RFORCE CARDS
!
                              DO j = 1 , ido
                                 CALL rforce(lcore)
                              ENDDO
                              CYCLE
                           ELSEIF ( Nobld==11 ) THEN
!
!     PRESAX CARDS
!
                              DO j = 1 , ido
                                 CALL presax(Iharm)
                              ENDDO
                              CYCLE
                           ELSEIF ( Nobld==12 ) THEN
!
!     QHBDY CARDS
!
                              DO j = 1 , ido
                                 CALL qhbdy
                              ENDDO
                              CYCLE
                           ELSEIF ( Nobld==13 ) THEN
!
!     QVOL CARDS (MODIFIED USER ENTRYS)
!
                              DO j = 1 , ido
                                 CALL qvol
                              ENDDO
                              CYCLE
                           ELSEIF ( Nobld==14 ) THEN
!
!     QBDY1 CARDS (MODIFIED USER ENTRYS)
!
                              kkkk = 1
                           ELSEIF ( Nobld==15 ) THEN
!
!     QBDY2 CARDS (MODIFIED USER ENTRYS)
!
                              kkkk = 2
                           ELSEIF ( Nobld==16 ) THEN
!
!     QVECT CARDS (MODIFIED USER ENTRYS)
!
                              kkkk = 3
                           ELSEIF ( Nobld==17 ) THEN
!
!     PLOAD3 CARDS
!
                              DO j = 1 , ido
                                 CALL pload3
                              ENDDO
                              CYCLE
                           ELSEIF ( Nobld==18 ) THEN
!
!     PLOAD1 CARDS
!
                              IF ( ipre==1 ) THEN
                                 spag_nextblock_3 = 2
                                 CYCLE SPAG_DispatchLoop_3
                              ENDIF
                              ipre = 1
                              lcore = lcore - Sysbuf - 1
                              mcore = lcore - Nrowsp - 1
                              IF ( lcore<Nrowsp ) THEN
                                 spag_nextblock_1 = 3
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              CALL premat(Core(Nrowsp+1),Core(Nrowsp+1),Core(lcore),mcore,ncore,Mpt,Idit)
                              spag_nextblock_3 = 2
                              CYCLE SPAG_DispatchLoop_3
                           ELSEIF ( Nobld==19 ) THEN
!
!     PLOADX CARDS
!
                              DO j = 1 , ido
                                 CALL ploadx
                              ENDDO
                              CYCLE
                           ELSEIF ( Nobld==20 .OR. Nobld==21 .OR. Nobld==22 .OR. Nobld==23 .OR. Nobld==24 ) THEN
                              EXIT SPAG_Loop_2_1
                           ELSEIF ( Nobld==25 ) THEN
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
                           CYCLE
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
                     CALL eandm(Nobld,ido,nextz,lcore,nbdys,all,nelout)
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
                     ipre = 1
!
!     1ST AND LAST AVAILABLE LOCATIONS IN OPEN CORE
!
                     Iihmat = Nrowsp
                     Nnhmat = lcore
                     Mptfil = Mpt
                     Iditfl = Idit
                     CALL prehma(Core)
!
!     NOW NNHMAT CONTAINS LAST LOCATION OF MATERIAL INFO
!
                     nextz = Nnhmat + 1
!
!     OPEN HCFLDS TO CONTAIN APPLIED MAGNETIC FIELD LOAD
!
                     lcore = lcore - Sysbuf
                     IF ( lcore>nextz ) THEN
!
!     STORE SILS  ON PERMBDY, IF ANY, INTO OPEN CORE
!
                        nbdys = 0
                        file = permbd
                        CALL open(*24,permbd,Core(lcore+1),0)
                        CALL fwdrec(*40,permbd)
                        CALL read(*40,*22,permbd,Core(nextz),lcore-nextz+1,0,nbdys)
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
                  CALL gopen(casecc,Core(lcore+1),0)
                  spag_nextblock_2 = 3
               CASE (3)
                  CALL read(*28,*26,casecc,Core(nextz),lcore-nextz+1,0,kcc)
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
                  CALL read(*36,*30,casecc,Core(nextz),lcore-nextz+1,0,ncc)
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
                  CYCLE SPAG_DispatchLoop_2
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
                  CALL gopen(hcflds,Core(lcore+1),1)
                  i = lcore - Sysbuf
                  j = i - Sysbuf
                  lcore = j - Sysbuf
                  IF ( lcore<=nextz ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL gopen(remfls,Core(i+1),1)
                  CALL gopen(hccens,Core(j+1),1)
                  CALL gopen(scr6,Core(lcore+1),1)
                  CALL eandm(Nobld,ido,nextz,lcore,nbdys,all,nelout)
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
!
!
 38               IF ( ngrold/=Ngrav ) CYCLE
                  CALL pack(Core,Pg,Pg(1))
                  spag_nextblock_2 = 9
               CASE (9)
                  iii = iii + 1
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO
!
         CALL close(Bgpdt,1)
         IF ( Icm==0 ) CALL close(Cstm,1)
         CALL close(Slt,1)
         CALL close(Sil,1)
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
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         ip1 = -7
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
         ip1 = -8
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(ip1,file,name(1))
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE extern
