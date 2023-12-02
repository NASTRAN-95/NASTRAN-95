!*==cmsofo.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmsofo
   USE c_blank
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_cmb004
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa
   INTEGER , DIMENSION(9) :: ce
   REAL , DIMENSION(4) :: ecpt
   INTEGER , DIMENSION(5) :: ent
   INTEGER , SAVE :: eog , loap , lods , nhbgss , nhcstm , nheqss , nhplts , papp
   INTEGER :: getip , i , iadd , idir , ifile , ii , imsg , ipn , isil , ist , istnm , istrn , isub , itest , itran , itype , j ,   &
            & jdh , jj , k , kdh , kid , kk , kkk , lbgss , lcc , litm , llco , ncomp , ncs , ndir , ndof , ngrp , nlv , nnames ,   &
            & nnn , nnsub , noipn , nout , np2 , npp , npsp1 , npwd , nsub , nt , nw , nwd , sbgss
   INTEGER , DIMENSION(32) :: list
   INTEGER , DIMENSION(14) :: namold
   REAL , DIMENSION(3) :: rent , sav1
   REAL , DIMENSION(1) :: rz
   REAL , DIMENSION(9) :: sav2 , tmat
   EXTERNAL andf , bisloc , close , cmiwrt , decode , eqsout , fwdrec , gmmats , mesage , open , pretrs , read , rewind , rshift ,  &
          & setlvl , sfetch , sjump , skpfil , sofcls , sort , suread , suwrt , transs , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     THIS ROUTINE GENERATES THE NEW SOF DATA FOR A COMBINATION.
!
   !>>>>EQUIVALENCE (Rz(1),Z(1)) , (itran,ecpt(1))
   DATA aaa/4HCMSO , 4HFO  / , eog/4H$EOG/
   DATA papp , lods , loap/4HPAPP , 4HLODS , 4HLOAP/
   DATA nheqss , nhbgss , nhcstm , nhplts/4HEQSS , 4HBGSS , 4HCSTM , 4HPLTS/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     GET NAMES OF BASIC COMPONENTS FROM THE TABLE OF CONTENTS
!
         ifile = sctoc
         IF ( .NOT.tocopn ) CALL open(*200,sctoc,z(buf2),0)
         CALL rewind(sctoc)
         k = 0
         j = 0
         spag_nextblock_1 = 2
      CASE (2)
         j = j + 1
!
         CALL read(*40,*220,sctoc,0,-3,0,nnn)
         CALL read(*40,*20,sctoc,z(score+k),lcore,1,nnn)
 20      k = k + nnn
         IF ( j<npsub ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      IF ( .NOT.tocopn ) CALL close(sctoc,1)
         istnm = score
         score = score + k
         nnames = k
!
         nsub = 0
         DO j = 1 , npsub
            nsub = nsub + combo(j,5)
         ENDDO
!
!     WRITE THE FIRST GROUP OF THE EQSS
!
         IF ( lonly ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         np2 = 2*npsub
         DO i = 1 , np2 , 2
            j = i/2 + 1
            namold(i) = combo(j,1)
            namold(i+1) = combo(j,2)
         ENDDO
         npp = npsub
         CALL setlvl(cnam,npp,namold,itest,29)
         IF ( itest==8 ) THEN
!
            WRITE (outt,99001) ufm
99001       FORMAT (A23,' 6518, ONE OF THE COMPONENT SUBSTRUCTURES HAS BEEN ','USED IN A PREVIOUS COMBINE OR REDUCE.')
            imsg = -37
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSE
            itest = 3
            CALL sfetch(cnam,nheqss,2,itest)
            itest = 1
            CALL suwrt(cnam,2,itest)
            CALL suwrt(nsub,1,itest)
            CALL suwrt(nipnew,1,itest)
            itest = 2
            CALL suwrt(z(istnm),nnames,itest)
!
            ifile = scconn
            CALL open(*200,scconn,z(buf1),0)
            ifile = scsfil
            CALL open(*200,scsfil,z(buf2),0)
            ifile = scr1
            CALL open(*200,scr1,z(buf3),1)
            DO i = 1 , npsub
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     ipn = 0
                     kk = 0
                     spag_nextblock_2 = 2
                  CASE (2)
                     ipn = ipn + 1
                     CALL read(*44,*42,scconn,ce,10,1,nnn)
 42                  IF ( ce(i+2)/=0 ) THEN
                        z(score+kk) = ce(1)
                        z(score+kk+1) = ce(i+2)
                        z(score+kk+2) = ipn
                        kk = kk + 3
                     ENDIF
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
 44                  noipn = (kk)/3
                     CALL sort(0,0,3,2,z(score),kk)
!
!     READ BGSS FROM SUBFIL
!
                     ifile = scsfil
                     npsp1 = combo(i,5) + 1
                     DO j = 1 , npsp1
                        CALL fwdrec(*220,scsfil)
                     ENDDO
                     sbgss = score + kk
                     lcore = lcore - kk
                     CALL read(*220,*46,scsfil,z(sbgss),lcore,1,lbgss)
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
 46                  DO j = 1 , kk , 3
                        jj = j - 1
                        getip = z(score+jj+1)
                        ent(1) = z(sbgss+4*getip-4)
                        ent(2) = z(sbgss+4*getip-4+1)
                        ent(3) = z(sbgss+4*getip-4+2)
                        ent(4) = z(sbgss+4*getip-4+3)
                        ent(5) = z(score+jj+2)
                        CALL write(scr1,ent,5,0)
                     ENDDO
                     CALL write(scr1,ent,0,1)
                     IF ( i==1 ) CALL rewind(scsfil)
                     IF ( i/=1 ) CALL skpfil(scsfil,-1)
                     IF ( i/=1 ) CALL skpfil(scsfil,1)
                     ncomp = combo(i,5)
                     DO j = 1 , ncomp
                        CALL read(*200,*48,scsfil,z(sbgss),lcore,1,nnn)
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
 48                     IF ( nnn/=0 ) THEN
                           DO jj = 1 , nnn , 3
                              kid = z(sbgss+jj)
                              CALL bisloc(*50,kid,z(score+1),3,noipn,nwd)
                              SPAG_Loop_4_1: DO WHILE ( z(score+nwd)==z(score+nwd-3) )
                                 IF ( nwd<=1 ) EXIT SPAG_Loop_4_1
                                 nwd = nwd - 3
                              ENDDO SPAG_Loop_4_1
                              SPAG_Loop_4_2: DO
                                 ent(1) = z(sbgss+jj-1)
                                 ent(2) = z(score+nwd+1)
                                 ent(3) = z(score+nwd-1)
                                 CALL write(scr1,ent,3,0)
                                 IF ( z(score+nwd)/=z(score+nwd+3) ) EXIT SPAG_Loop_4_2
                                 IF ( nwd+3>=noipn*3 ) EXIT SPAG_Loop_4_2
                                 nwd = nwd + 3
                              ENDDO SPAG_Loop_4_2
 50                        ENDDO
                        ENDIF
                        CALL write(scr1,0,0,1)
                     ENDDO
                     CALL skpfil(scsfil,1)
                     CALL rewind(scconn)
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
            CALL close(scsfil,1)
            CALL close(scr1,1)
!
!     WRITE OUT EQSS ONTO SOF
!
            ifile = scr1
            CALL open(*200,scr1,z(buf3),0)
            DO i = 1 , npsub
               ncomp = combo(i,5)
               CALL fwdrec(*220,scr1)
               DO j = 1 , ncomp
                  CALL read(*220,*52,scr1,z(sbgss),lcore,1,nnn)
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
 52               itest = 2
                  CALL suwrt(z(sbgss),nnn,itest)
               ENDDO
            ENDDO
!
!     WRITE OUT MASTER SIL,C LIST FOR NEW STRUCTURE
!
            CALL rewind(scconn)
            kk = 0
            isil = 1
            CALL read(*80,*60,scconn,ce,10,1,nnn)
         ENDIF
 60      DO
            CALL decode(ce(1),z(buf2),ndof)
            z(sbgss+kk) = isil
            z(sbgss+kk+1) = ce(1)
            isil = isil + ndof
            kk = kk + 2
            CALL read(*80,*60,scconn,ce,10,1,nnn)
         ENDDO
 80      itest = 2
         CALL suwrt(z(sbgss),kk,itest)
         itest = 3
         CALL suwrt(ent,0,itest)
!
!     WRITE BGSS ONTO SOF
!
         lcc = lcore
         kk = 0
         CALL rewind(scr1)
         DO i = 1 , npsub
            ncomp = combo(i,5)
            CALL read(*220,*90,scr1,z(sbgss+kk),lcc,1,nw)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
 90         kk = kk + nw
            lcc = lcc - nw
            DO j = 1 , ncomp
               CALL fwdrec(*100,scr1)
            ENDDO
         ENDDO
 100     CALL sort(0,0,5,5,z(sbgss),kk)
         itest = 3
         CALL sfetch(cnam,nhbgss,2,itest)
         itest = 1
         CALL suwrt(cnam,2,itest)
         itest = 2
         CALL suwrt(nipnew,1,itest)
         kkk = 0
         DO
            isub = sbgss + kkk
            ent(1) = z(isub)
            ent(2) = z(isub+1)
            ent(3) = z(isub+2)
            ent(4) = z(isub+3)
            IF ( kkk<=0 .OR. z(isub+4)/=z(isub-1) ) THEN
               itest = 1
               CALL suwrt(ent,4,itest)
            ENDIF
            kkk = kkk + 5
            IF ( kkk>=kk ) THEN
               itest = 2
               CALL suwrt(ent,0,itest)
               itest = 3
               CALL suwrt(ent,0,itest)
               CALL close(scr1,1)
               CALL close(scconn,1)
!
!     PROCESS CSTM ITEM
!
               CALL open(*200,sccstm,z(buf3),0)
               CALL read(*120,*120,sccstm,z(score),lcore,1,nnn)
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
 120     IF ( nnn/=0 ) THEN
            itest = 3
            CALL sfetch(cnam,nhcstm,2,itest)
            itest = 2
            CALL suwrt(cnam,2,itest)
            itest = 2
            CALL suwrt(z(score),nnn,itest)
            itest = 3
            CALL suwrt(0,0,itest)
         ENDIF
         CALL close(sccstm,1)
         spag_nextblock_1 = 3
      CASE (3)
!
!     PROCESS LODS ITEM
!
         nlv = 0
         ncs = nnames/2
         j = 0
         litm = lods
         IF ( pora==papp ) litm = loap
         DO i = 1 , npsub
            namold(1) = combo(i,1)
            namold(2) = combo(i,2)
            CALL sfetch(namold,litm,1,itest)
            IF ( itest==3 ) THEN
               z(score+j) = 0
               z(score+j+1) = eog
               j = j + 2
            ELSE
               CALL suread(ce,4,nout,itest)
               nlv = nlv + ce(3)
               jdh = 1
               CALL sjump(jdh)
               CALL suread(z(score+j),-2,nout,itest)
               j = j + nout
            ENDIF
         ENDDO
         itest = 3
         CALL sfetch(cnam,litm,2,itest)
         itest = 1
         CALL suwrt(cnam,2,itest)
         CALL suwrt(nlv,1,itest)
         CALL suwrt(ncs,1,itest)
         itest = 2
         CALL suwrt(z(istnm),nnames,itest)
         itest = 3
         CALL suwrt(z(score),j,itest)
         IF ( lonly ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     PROCESS PLTS ITEM
!
!
!     FIND OLD PLTS TRANSFORMATIONS
!
         nout = 0
         j = 0
         nnsub = 0
         DO i = 1 , npsub
            namold(1) = combo(i,1)
            namold(2) = combo(i,2)
            CALL sfetch(namold,nhplts,1,itest)
            IF ( itest/=3 ) THEN
               CALL suread(z(score+j),3,nout,itest)
               nnsub = nnsub + z(score+j+2)
               CALL suread(z(score+j),-1,nout,itest)
               j = j + nout
            ENDIF
         ENDDO
         npwd = j
         istrn = score + npwd
         llco = lcore - npwd
         itest = 3
         CALL sfetch(cnam,nhplts,2,itest)
         itest = 1
         CALL suwrt(cnam,2,itest)
         CALL suwrt(nnsub,1,itest)
         nt = 0
         IF ( .NOT.tdat(3) ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL open(*200,scbdat,z(buf1),0)
         CALL skpfil(scbdat,2)
         CALL read(*220,*140,scbdat,z(istrn),lcore,1,nt)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 140     CALL pretrs(z(istrn),nt)
         spag_nextblock_1 = 4
      CASE (4)
         IF ( .NOT.tocopn ) CALL open(*200,sctoc,z(buf2),1)
         CALL rewind(sctoc)
         ist = istrn + nt
         llco = llco - nt
         j = 0
         spag_nextblock_1 = 5
      CASE (5)
         j = j + 1
         itran = combo(j,3)
         DO i = 1 , 9
            tmat(i) = 0.0
         ENDDO
         CALL read(*180,*160,sctoc,z(ist),llco,1,nnn)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 160     IF ( itran==0 ) THEN
            tmat(1) = 1.0
            tmat(5) = 1.0
            tmat(9) = 1.0
         ELSE
            DO i = 2 , 4
               ecpt(i) = 0.0
            ENDDO
            CALL transs(ecpt,tmat)
         ENDIF
!
!     DETERMINE SYMMETRY
!
         IF ( combo(j,4)/=0 ) THEN
            CALL decode(combo(j,4),list,ndir)
            DO i = 1 , ndir
               idir = list(i) + 1
               idir = 4 - idir
               tmat(idir) = -tmat(idir)
               tmat(idir+3) = -tmat(idir+3)
               tmat(idir+6) = -tmat(idir+6)
            ENDDO
         ENDIF
         DO i = 1 , 3
            rent(i) = origin(j,i)
         ENDDO
         nnn = nnn - 1
         DO i = 3 , nnn , 2
            spag_nextblock_3 = 1
            SPAG_DispatchLoop_3: DO
               SELECT CASE (spag_nextblock_3)
               CASE (1)
!
!     PROCESS OLD TRANSFORMATIONS
!
                  SPAG_Loop_4_1: DO kdh = 1 , npwd , 14
                     IF ( z(ist+i)==z(score+kdh-1) .AND. z(ist+i+1)==z(score+kdh) ) THEN
                        spag_nextblock_3 = 2
                        EXIT SPAG_Loop_4_1
                     ENDIF
                  ENDDO SPAG_Loop_4_1
               CASE (2)
                  CALL gmmats(tmat,3,3,0,rz(score+kdh+1),3,1,0,sav1)
                  DO ii = 1 , 3
                     sav1(ii) = sav1(ii) + rent(ii)
                  ENDDO
                  CALL gmmats(tmat,3,3,0,rz(score+kdh+4),3,3,0,sav2)
                  itest = 1
                  CALL suwrt(z(ist+i),2,itest)
                  CALL suwrt(sav1(1),3,itest)
                  CALL suwrt(sav2(1),9,itest)
                  EXIT SPAG_DispatchLoop_3
               END SELECT
            ENDDO SPAG_DispatchLoop_3
         ENDDO
         IF ( j<npsub ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 180     IF ( .NOT.tocopn ) CALL close(sctoc,1)
         itest = 2
         CALL suwrt(0,0,itest)
         itest = 3
         CALL suwrt(0,0,itest)
         CALL close(scbdat,1)
         CALL eqsout
!
!     PROCESS OUTPUT REQUESTS
!
         IF ( andf(rshift(iprint,12),1)==1 ) THEN
!
!     WRITE EQSS FOR NEW STRUCTURE
!
            CALL sfetch(cnam,nheqss,1,itest)
            CALL suread(z(score),4,nout,itest)
            CALL suread(z(score),-1,nout,itest)
            ist = score + nout
            DO i = 1 , nsub
               CALL suread(z(ist),-1,nout,itest)
               iadd = score + 2*(i-1)
               CALL cmiwrt(1,cnam,z(iadd),ist,nout,z,z)
            ENDDO
            CALL suread(z(ist),-1,nout,itest)
            CALL cmiwrt(8,cnam,0,ist,nout,z,z)
         ENDIF
         IF ( andf(rshift(iprint,13),1)==1 ) THEN
!
!     WRITE BGSS FOR NEW STRUCTURE
!
            CALL sfetch(cnam,nhbgss,1,itest)
            ngrp = 1
            CALL sjump(ngrp)
            ist = score
            CALL suread(z(ist),-1,nout,itest)
            CALL cmiwrt(2,cnam,cnam,ist,nout,z,z)
         ENDIF
         IF ( andf(rshift(iprint,14),1)==1 ) THEN
!
!     WRITE CSTM ITEM
!
            CALL sfetch(cnam,nhcstm,1,itest)
            IF ( itest/=3 ) THEN
               ngrp = 1
               CALL sjump(ngrp)
               ist = score
               CALL suread(z(ist),-1,nout,itest)
               CALL cmiwrt(3,cnam,cnam,ist,nout,z,z)
            ENDIF
         ENDIF
         IF ( andf(rshift(iprint,15),1)==1 ) THEN
!
!     WRITE PLTS ITEM
!
            CALL sfetch(cnam,nhplts,1,itest)
            ist = score
            CALL suread(z(ist),3,nout,itest)
            CALL suread(z(ist),-1,nout,itest)
            CALL cmiwrt(4,cnam,cnam,ist,nout,z,z)
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         IF ( andf(rshift(iprint,16),1)==1 ) THEN
!
!     WRITE LODS ITEM
!
            CALL sfetch(cnam,lods,1,itest)
            IF ( itest/=3 ) THEN
               CALL suread(z(score),4,nout,itest)
               CALL suread(z(score),-1,nout,itest)
               ist = score + nout
               itype = 5
               IF ( litm==loap ) itype = 7
               DO i = 1 , nsub
                  iadd = score + 2*(i-1)
                  CALL suread(z(ist),-1,nout,itest)
                  CALL cmiwrt(itype,cnam,z(iadd),ist,nout,z,z)
                  itype = 6
               ENDDO
            ENDIF
         ENDIF
         RETURN
 200     imsg = -1
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 220     imsg = -2
         spag_nextblock_1 = 8
      CASE (7)
         imsg = -8
         spag_nextblock_1 = 8
      CASE (8)
         CALL sofcls
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cmsofo
