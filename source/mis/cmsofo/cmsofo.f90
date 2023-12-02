!*==cmsofo.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmsofo
   IMPLICIT NONE
   USE C_BLANK
   USE C_CMB001
   USE C_CMB002
   USE C_CMB003
   USE C_CMB004
   USE C_XMSSG
   USE C_ZZZZZZ
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
         ifile = Sctoc
         IF ( .NOT.Tocopn ) CALL open(*200,Sctoc,Z(Buf2),0)
         CALL rewind(Sctoc)
         k = 0
         j = 0
         spag_nextblock_1 = 2
      CASE (2)
         j = j + 1
!
         CALL read(*40,*220,Sctoc,0,-3,0,nnn)
         CALL read(*40,*20,Sctoc,Z(Score+k),Lcore,1,nnn)
 20      k = k + nnn
         IF ( j<Npsub ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      IF ( .NOT.Tocopn ) CALL close(Sctoc,1)
         istnm = Score
         Score = Score + k
         nnames = k
!
         nsub = 0
         DO j = 1 , Npsub
            nsub = nsub + Combo(j,5)
         ENDDO
!
!     WRITE THE FIRST GROUP OF THE EQSS
!
         IF ( Lonly ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         np2 = 2*Npsub
         DO i = 1 , np2 , 2
            j = i/2 + 1
            namold(i) = Combo(j,1)
            namold(i+1) = Combo(j,2)
         ENDDO
         npp = Npsub
         CALL setlvl(Cnam,npp,namold,itest,29)
         IF ( itest==8 ) THEN
!
            WRITE (Outt,99001) Ufm
99001       FORMAT (A23,' 6518, ONE OF THE COMPONENT SUBSTRUCTURES HAS BEEN ','USED IN A PREVIOUS COMBINE OR REDUCE.')
            imsg = -37
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSE
            itest = 3
            CALL sfetch(Cnam,nheqss,2,itest)
            itest = 1
            CALL suwrt(Cnam,2,itest)
            CALL suwrt(nsub,1,itest)
            CALL suwrt(Nipnew,1,itest)
            itest = 2
            CALL suwrt(Z(istnm),nnames,itest)
!
            ifile = Scconn
            CALL open(*200,Scconn,Z(Buf1),0)
            ifile = Scsfil
            CALL open(*200,Scsfil,Z(Buf2),0)
            ifile = Scr1
            CALL open(*200,Scr1,Z(Buf3),1)
            DO i = 1 , Npsub
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     ipn = 0
                     kk = 0
                     spag_nextblock_2 = 2
                  CASE (2)
                     ipn = ipn + 1
                     CALL read(*44,*42,Scconn,ce,10,1,nnn)
 42                  IF ( ce(i+2)/=0 ) THEN
                        Z(Score+kk) = ce(1)
                        Z(Score+kk+1) = ce(i+2)
                        Z(Score+kk+2) = ipn
                        kk = kk + 3
                     ENDIF
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
 44                  noipn = (kk)/3
                     CALL sort(0,0,3,2,Z(Score),kk)
!
!     READ BGSS FROM SUBFIL
!
                     ifile = Scsfil
                     npsp1 = Combo(i,5) + 1
                     DO j = 1 , npsp1
                        CALL fwdrec(*220,Scsfil)
                     ENDDO
                     sbgss = Score + kk
                     Lcore = Lcore - kk
                     CALL read(*220,*46,Scsfil,Z(sbgss),Lcore,1,lbgss)
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
 46                  DO j = 1 , kk , 3
                        jj = j - 1
                        getip = Z(Score+jj+1)
                        ent(1) = Z(sbgss+4*getip-4)
                        ent(2) = Z(sbgss+4*getip-4+1)
                        ent(3) = Z(sbgss+4*getip-4+2)
                        ent(4) = Z(sbgss+4*getip-4+3)
                        ent(5) = Z(Score+jj+2)
                        CALL write(Scr1,ent,5,0)
                     ENDDO
                     CALL write(Scr1,ent,0,1)
                     IF ( i==1 ) CALL rewind(Scsfil)
                     IF ( i/=1 ) CALL skpfil(Scsfil,-1)
                     IF ( i/=1 ) CALL skpfil(Scsfil,1)
                     ncomp = Combo(i,5)
                     DO j = 1 , ncomp
                        CALL read(*200,*48,Scsfil,Z(sbgss),Lcore,1,nnn)
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
 48                     IF ( nnn/=0 ) THEN
                           DO jj = 1 , nnn , 3
                              kid = Z(sbgss+jj)
                              CALL bisloc(*50,kid,Z(Score+1),3,noipn,nwd)
                              SPAG_Loop_4_1: DO WHILE ( Z(Score+nwd)==Z(Score+nwd-3) )
                                 IF ( nwd<=1 ) EXIT SPAG_Loop_4_1
                                 nwd = nwd - 3
                              ENDDO SPAG_Loop_4_1
                              SPAG_Loop_4_2: DO
                                 ent(1) = Z(sbgss+jj-1)
                                 ent(2) = Z(Score+nwd+1)
                                 ent(3) = Z(Score+nwd-1)
                                 CALL write(Scr1,ent,3,0)
                                 IF ( Z(Score+nwd)/=Z(Score+nwd+3) ) EXIT SPAG_Loop_4_2
                                 IF ( nwd+3>=noipn*3 ) EXIT SPAG_Loop_4_2
                                 nwd = nwd + 3
                              ENDDO SPAG_Loop_4_2
 50                        ENDDO
                        ENDIF
                        CALL write(Scr1,0,0,1)
                     ENDDO
                     CALL skpfil(Scsfil,1)
                     CALL rewind(Scconn)
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
            CALL close(Scsfil,1)
            CALL close(Scr1,1)
!
!     WRITE OUT EQSS ONTO SOF
!
            ifile = Scr1
            CALL open(*200,Scr1,Z(Buf3),0)
            DO i = 1 , Npsub
               ncomp = Combo(i,5)
               CALL fwdrec(*220,Scr1)
               DO j = 1 , ncomp
                  CALL read(*220,*52,Scr1,Z(sbgss),Lcore,1,nnn)
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
 52               itest = 2
                  CALL suwrt(Z(sbgss),nnn,itest)
               ENDDO
            ENDDO
!
!     WRITE OUT MASTER SIL,C LIST FOR NEW STRUCTURE
!
            CALL rewind(Scconn)
            kk = 0
            isil = 1
            CALL read(*80,*60,Scconn,ce,10,1,nnn)
         ENDIF
 60      DO
            CALL decode(ce(1),Z(Buf2),ndof)
            Z(sbgss+kk) = isil
            Z(sbgss+kk+1) = ce(1)
            isil = isil + ndof
            kk = kk + 2
            CALL read(*80,*60,Scconn,ce,10,1,nnn)
         ENDDO
 80      itest = 2
         CALL suwrt(Z(sbgss),kk,itest)
         itest = 3
         CALL suwrt(ent,0,itest)
!
!     WRITE BGSS ONTO SOF
!
         lcc = Lcore
         kk = 0
         CALL rewind(Scr1)
         DO i = 1 , Npsub
            ncomp = Combo(i,5)
            CALL read(*220,*90,Scr1,Z(sbgss+kk),lcc,1,nw)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
 90         kk = kk + nw
            lcc = lcc - nw
            DO j = 1 , ncomp
               CALL fwdrec(*100,Scr1)
            ENDDO
         ENDDO
 100     CALL sort(0,0,5,5,Z(sbgss),kk)
         itest = 3
         CALL sfetch(Cnam,nhbgss,2,itest)
         itest = 1
         CALL suwrt(Cnam,2,itest)
         itest = 2
         CALL suwrt(Nipnew,1,itest)
         kkk = 0
         DO
            isub = sbgss + kkk
            ent(1) = Z(isub)
            ent(2) = Z(isub+1)
            ent(3) = Z(isub+2)
            ent(4) = Z(isub+3)
            IF ( kkk<=0 .OR. Z(isub+4)/=Z(isub-1) ) THEN
               itest = 1
               CALL suwrt(ent,4,itest)
            ENDIF
            kkk = kkk + 5
            IF ( kkk>=kk ) THEN
               itest = 2
               CALL suwrt(ent,0,itest)
               itest = 3
               CALL suwrt(ent,0,itest)
               CALL close(Scr1,1)
               CALL close(Scconn,1)
!
!     PROCESS CSTM ITEM
!
               CALL open(*200,Sccstm,Z(Buf3),0)
               CALL read(*120,*120,Sccstm,Z(Score),Lcore,1,nnn)
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
 120     IF ( nnn/=0 ) THEN
            itest = 3
            CALL sfetch(Cnam,nhcstm,2,itest)
            itest = 2
            CALL suwrt(Cnam,2,itest)
            itest = 2
            CALL suwrt(Z(Score),nnn,itest)
            itest = 3
            CALL suwrt(0,0,itest)
         ENDIF
         CALL close(Sccstm,1)
         spag_nextblock_1 = 3
      CASE (3)
!
!     PROCESS LODS ITEM
!
         nlv = 0
         ncs = nnames/2
         j = 0
         litm = lods
         IF ( Pora==papp ) litm = loap
         DO i = 1 , Npsub
            namold(1) = Combo(i,1)
            namold(2) = Combo(i,2)
            CALL sfetch(namold,litm,1,itest)
            IF ( itest==3 ) THEN
               Z(Score+j) = 0
               Z(Score+j+1) = eog
               j = j + 2
            ELSE
               CALL suread(ce,4,nout,itest)
               nlv = nlv + ce(3)
               jdh = 1
               CALL sjump(jdh)
               CALL suread(Z(Score+j),-2,nout,itest)
               j = j + nout
            ENDIF
         ENDDO
         itest = 3
         CALL sfetch(Cnam,litm,2,itest)
         itest = 1
         CALL suwrt(Cnam,2,itest)
         CALL suwrt(nlv,1,itest)
         CALL suwrt(ncs,1,itest)
         itest = 2
         CALL suwrt(Z(istnm),nnames,itest)
         itest = 3
         CALL suwrt(Z(Score),j,itest)
         IF ( Lonly ) THEN
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
         DO i = 1 , Npsub
            namold(1) = Combo(i,1)
            namold(2) = Combo(i,2)
            CALL sfetch(namold,nhplts,1,itest)
            IF ( itest/=3 ) THEN
               CALL suread(Z(Score+j),3,nout,itest)
               nnsub = nnsub + Z(Score+j+2)
               CALL suread(Z(Score+j),-1,nout,itest)
               j = j + nout
            ENDIF
         ENDDO
         npwd = j
         istrn = Score + npwd
         llco = Lcore - npwd
         itest = 3
         CALL sfetch(Cnam,nhplts,2,itest)
         itest = 1
         CALL suwrt(Cnam,2,itest)
         CALL suwrt(nnsub,1,itest)
         nt = 0
         IF ( .NOT.Tdat(3) ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL open(*200,Scbdat,Z(Buf1),0)
         CALL skpfil(Scbdat,2)
         CALL read(*220,*140,Scbdat,Z(istrn),Lcore,1,nt)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 140     CALL pretrs(Z(istrn),nt)
         spag_nextblock_1 = 4
      CASE (4)
         IF ( .NOT.Tocopn ) CALL open(*200,Sctoc,Z(Buf2),1)
         CALL rewind(Sctoc)
         ist = istrn + nt
         llco = llco - nt
         j = 0
         spag_nextblock_1 = 5
      CASE (5)
         j = j + 1
         itran = Combo(j,3)
         DO i = 1 , 9
            tmat(i) = 0.0
         ENDDO
         CALL read(*180,*160,Sctoc,Z(ist),llco,1,nnn)
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
         IF ( Combo(j,4)/=0 ) THEN
            CALL decode(Combo(j,4),list,ndir)
            DO i = 1 , ndir
               idir = list(i) + 1
               idir = 4 - idir
               tmat(idir) = -tmat(idir)
               tmat(idir+3) = -tmat(idir+3)
               tmat(idir+6) = -tmat(idir+6)
            ENDDO
         ENDIF
         DO i = 1 , 3
            rent(i) = Origin(j,i)
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
                  DO kdh = 1 , npwd , 14
                     IF ( Z(ist+i)==Z(Score+kdh-1) .AND. Z(ist+i+1)==Z(Score+kdh) ) THEN
                        spag_nextblock_3 = 2
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                  ENDDO
                  CYCLE
               CASE (2)
                  CALL gmmats(tmat,3,3,0,rz(Score+kdh+1),3,1,0,sav1)
                  DO ii = 1 , 3
                     sav1(ii) = sav1(ii) + rent(ii)
                  ENDDO
                  CALL gmmats(tmat,3,3,0,rz(Score+kdh+4),3,3,0,sav2)
                  itest = 1
                  CALL suwrt(Z(ist+i),2,itest)
                  CALL suwrt(sav1(1),3,itest)
                  CALL suwrt(sav2(1),9,itest)
                  EXIT SPAG_DispatchLoop_3
               END SELECT
            ENDDO SPAG_DispatchLoop_3
         ENDDO
         IF ( j<Npsub ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 180     IF ( .NOT.Tocopn ) CALL close(Sctoc,1)
         itest = 2
         CALL suwrt(0,0,itest)
         itest = 3
         CALL suwrt(0,0,itest)
         CALL close(Scbdat,1)
         CALL eqsout
!
!     PROCESS OUTPUT REQUESTS
!
         IF ( andf(rshift(Iprint,12),1)==1 ) THEN
!
!     WRITE EQSS FOR NEW STRUCTURE
!
            CALL sfetch(Cnam,nheqss,1,itest)
            CALL suread(Z(Score),4,nout,itest)
            CALL suread(Z(Score),-1,nout,itest)
            ist = Score + nout
            DO i = 1 , nsub
               CALL suread(Z(ist),-1,nout,itest)
               iadd = Score + 2*(i-1)
               CALL cmiwrt(1,Cnam,Z(iadd),ist,nout,Z,Z)
            ENDDO
            CALL suread(Z(ist),-1,nout,itest)
            CALL cmiwrt(8,Cnam,0,ist,nout,Z,Z)
         ENDIF
         IF ( andf(rshift(Iprint,13),1)==1 ) THEN
!
!     WRITE BGSS FOR NEW STRUCTURE
!
            CALL sfetch(Cnam,nhbgss,1,itest)
            ngrp = 1
            CALL sjump(ngrp)
            ist = Score
            CALL suread(Z(ist),-1,nout,itest)
            CALL cmiwrt(2,Cnam,Cnam,ist,nout,Z,Z)
         ENDIF
         IF ( andf(rshift(Iprint,14),1)==1 ) THEN
!
!     WRITE CSTM ITEM
!
            CALL sfetch(Cnam,nhcstm,1,itest)
            IF ( itest/=3 ) THEN
               ngrp = 1
               CALL sjump(ngrp)
               ist = Score
               CALL suread(Z(ist),-1,nout,itest)
               CALL cmiwrt(3,Cnam,Cnam,ist,nout,Z,Z)
            ENDIF
         ENDIF
         IF ( andf(rshift(Iprint,15),1)==1 ) THEN
!
!     WRITE PLTS ITEM
!
            CALL sfetch(Cnam,nhplts,1,itest)
            ist = Score
            CALL suread(Z(ist),3,nout,itest)
            CALL suread(Z(ist),-1,nout,itest)
            CALL cmiwrt(4,Cnam,Cnam,ist,nout,Z,Z)
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         IF ( andf(rshift(Iprint,16),1)==1 ) THEN
!
!     WRITE LODS ITEM
!
            CALL sfetch(Cnam,lods,1,itest)
            IF ( itest/=3 ) THEN
               CALL suread(Z(Score),4,nout,itest)
               CALL suread(Z(Score),-1,nout,itest)
               ist = Score + nout
               itype = 5
               IF ( litm==loap ) itype = 7
               DO i = 1 , nsub
                  iadd = Score + 2*(i-1)
                  CALL suread(Z(ist),-1,nout,itest)
                  CALL cmiwrt(itype,Cnam,Z(iadd),ist,nout,Z,Z)
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
         CYCLE SPAG_DispatchLoop_1
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
