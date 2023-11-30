
SUBROUTINE cmsofo
   IMPLICIT NONE
   INTEGER Buf1 , Buf2 , Buf3 , Cnam(2) , Combo(7,5) , Iauto , Inpt , Iprint , Isort , Lcore , Mcon , Nipnew , Npsub , Outt , Pora ,&
         & Scbdat , Scconn , Sccstm , Score , Scr1 , Scsfil , Sctoc , Z(1)
   REAL Buf4 , Buf5 , Casecc , Conect , Conset , Dry , Geom4 , Origin(7,3) , Restct(7,7) , Rz(1) , Scmcon , Scr2 , Step , Toler ,   &
      & Tran
   LOGICAL Lonly , Tdat(6) , Tocopn
   CHARACTER*23 Ufm
   COMMON /blank / Step , Dry , Pora
   COMMON /cmb001/ Scr1 , Scr2 , Scbdat , Scsfil , Scconn , Scmcon , Sctoc , Geom4 , Casecc , Sccstm
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Score , Lcore , Inpt , Outt
   COMMON /cmb003/ Combo , Conset , Iauto , Toler , Npsub , Conect , Tran , Mcon , Restct , Isort , Origin , Iprint , Tocopn
   COMMON /cmb004/ Tdat , Nipnew , Cnam , Lonly
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER aaa(2) , ce(9) , ent(5) , eog , getip , i , iadd , idir , ifile , ii , imsg , ipn , isil , ist , istnm , istrn , isub ,  &
         & itest , itran , itype , j , jdh , jj , k , kdh , kid , kk , kkk , lbgss , lcc , list(32) , litm , llco , loap , lods ,   &
         & namold(14) , ncomp , ncs , ndir , ndof , ngrp , nhbgss , nhcstm , nheqss , nhplts , nlv , nnames , nnn , nnsub , noipn , &
         & nout , np2 , npp , npsp1 , npwd , nsub , nt , nw , nwd , papp , sbgss
   INTEGER andf , rshift
   REAL ecpt(4) , rent(3) , sav1(3) , sav2(9) , tmat(9)
   EXTERNAL andf , rshift
!
!     THIS ROUTINE GENERATES THE NEW SOF DATA FOR A COMBINATION.
!
   !>>>>EQUIVALENCE (Rz(1),Z(1)) , (itran,ecpt(1))
   DATA aaa/4HCMSO , 4HFO  / , eog/4H$EOG/
   DATA papp , lods , loap/4HPAPP , 4HLODS , 4HLOAP/
   DATA nheqss , nhbgss , nhcstm , nhplts/4HEQSS , 4HBGSS , 4HCSTM , 4HPLTS/
!
!     GET NAMES OF BASIC COMPONENTS FROM THE TABLE OF CONTENTS
!
   ifile = Sctoc
   IF ( .NOT.Tocopn ) CALL open(*1600,Sctoc,Z(Buf2),0)
   CALL rewind(Sctoc)
   k = 0
   j = 0
 100  j = j + 1
!
   CALL read(*300,*1700,Sctoc,0,-3,0,nnn)
   CALL read(*300,*200,Sctoc,Z(Score+k),Lcore,1,nnn)
 200  k = k + nnn
   IF ( j<Npsub ) GOTO 100
 300  IF ( .NOT.Tocopn ) CALL close(Sctoc,1)
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
   IF ( Lonly ) GOTO 900
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
99001 FORMAT (A23,' 6518, ONE OF THE COMPONENT SUBSTRUCTURES HAS BEEN ','USED IN A PREVIOUS COMBINE OR REDUCE.')
      imsg = -37
      GOTO 1900
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
      CALL open(*1600,Scconn,Z(Buf1),0)
      ifile = Scsfil
      CALL open(*1600,Scsfil,Z(Buf2),0)
      ifile = Scr1
      CALL open(*1600,Scr1,Z(Buf3),1)
      DO i = 1 , Npsub
         ipn = 0
         kk = 0
 320     ipn = ipn + 1
         CALL read(*360,*340,Scconn,ce,10,1,nnn)
 340     IF ( ce(i+2)/=0 ) THEN
            Z(Score+kk) = ce(1)
            Z(Score+kk+1) = ce(i+2)
            Z(Score+kk+2) = ipn
            kk = kk + 3
         ENDIF
         GOTO 320
 360     noipn = (kk)/3
         CALL sort(0,0,3,2,Z(Score),kk)
!
!     READ BGSS FROM SUBFIL
!
         ifile = Scsfil
         npsp1 = Combo(i,5) + 1
         DO j = 1 , npsp1
            CALL fwdrec(*1700,Scsfil)
         ENDDO
         sbgss = Score + kk
         Lcore = Lcore - kk
         CALL read(*1700,*380,Scsfil,Z(sbgss),Lcore,1,lbgss)
         GOTO 1800
 380     DO j = 1 , kk , 3
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
            CALL read(*1600,*390,Scsfil,Z(sbgss),Lcore,1,nnn)
            GOTO 1800
 390        IF ( nnn/=0 ) THEN
               DO jj = 1 , nnn , 3
                  kid = Z(sbgss+jj)
                  CALL bisloc(*395,kid,Z(Score+1),3,noipn,nwd)
                  DO WHILE ( Z(Score+nwd)==Z(Score+nwd-3) )
                     IF ( nwd<=1 ) EXIT
                     nwd = nwd - 3
                  ENDDO
                  DO
                     ent(1) = Z(sbgss+jj-1)
                     ent(2) = Z(Score+nwd+1)
                     ent(3) = Z(Score+nwd-1)
                     CALL write(Scr1,ent,3,0)
                     IF ( Z(Score+nwd)/=Z(Score+nwd+3) ) EXIT
                     IF ( nwd+3>=noipn*3 ) EXIT
                     nwd = nwd + 3
                  ENDDO
 395           ENDDO
            ENDIF
            CALL write(Scr1,0,0,1)
         ENDDO
         CALL skpfil(Scsfil,1)
         CALL rewind(Scconn)
      ENDDO
      CALL close(Scsfil,1)
      CALL close(Scr1,1)
!
!     WRITE OUT EQSS ONTO SOF
!
      ifile = Scr1
      CALL open(*1600,Scr1,Z(Buf3),0)
      DO i = 1 , Npsub
         ncomp = Combo(i,5)
         CALL fwdrec(*1700,Scr1)
         DO j = 1 , ncomp
            CALL read(*1700,*400,Scr1,Z(sbgss),Lcore,1,nnn)
            GOTO 1800
 400        itest = 2
            CALL suwrt(Z(sbgss),nnn,itest)
         ENDDO
      ENDDO
!
!     WRITE OUT MASTER SIL,C LIST FOR NEW STRUCTURE
!
      CALL rewind(Scconn)
      kk = 0
      isil = 1
      CALL read(*600,*500,Scconn,ce,10,1,nnn)
   ENDIF
 500  DO
      CALL decode(ce(1),Z(Buf2),ndof)
      Z(sbgss+kk) = isil
      Z(sbgss+kk+1) = ce(1)
      isil = isil + ndof
      kk = kk + 2
      CALL read(*600,*500,Scconn,ce,10,1,nnn)
   ENDDO
 600  itest = 2
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
      CALL read(*1700,*650,Scr1,Z(sbgss+kk),lcc,1,nw)
      GOTO 1800
 650  kk = kk + nw
      lcc = lcc - nw
      DO j = 1 , ncomp
         CALL fwdrec(*700,Scr1)
      ENDDO
   ENDDO
 700  CALL sort(0,0,5,5,Z(sbgss),kk)
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
         CALL open(*1600,Sccstm,Z(Buf3),0)
         CALL read(*800,*800,Sccstm,Z(Score),Lcore,1,nnn)
         GOTO 1800
      ENDIF
   ENDDO
 800  IF ( nnn/=0 ) THEN
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
!
!     PROCESS LODS ITEM
!
 900  nlv = 0
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
   IF ( Lonly ) GOTO 1500
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
   IF ( .NOT.Tdat(3) ) GOTO 1100
   CALL open(*1600,Scbdat,Z(Buf1),0)
   CALL skpfil(Scbdat,2)
   CALL read(*1700,*1000,Scbdat,Z(istrn),Lcore,1,nt)
   GOTO 1800
 1000 CALL pretrs(Z(istrn),nt)
 1100 IF ( .NOT.Tocopn ) CALL open(*1600,Sctoc,Z(Buf2),1)
   CALL rewind(Sctoc)
   ist = istrn + nt
   llco = llco - nt
   j = 0
 1200 j = j + 1
   itran = Combo(j,3)
   DO i = 1 , 9
      tmat(i) = 0.0
   ENDDO
   CALL read(*1400,*1300,Sctoc,Z(ist),llco,1,nnn)
   GOTO 1800
 1300 IF ( itran==0 ) THEN
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
!
!     PROCESS OLD TRANSFORMATIONS
!
      DO kdh = 1 , npwd , 14
         IF ( Z(ist+i)==Z(Score+kdh-1) .AND. Z(ist+i+1)==Z(Score+kdh) ) GOTO 1350
      ENDDO
      CYCLE
 1350 CALL gmmats(tmat,3,3,0,Rz(Score+kdh+1),3,1,0,sav1)
      DO ii = 1 , 3
         sav1(ii) = sav1(ii) + rent(ii)
      ENDDO
      CALL gmmats(tmat,3,3,0,Rz(Score+kdh+4),3,3,0,sav2)
      itest = 1
      CALL suwrt(Z(ist+i),2,itest)
      CALL suwrt(sav1(1),3,itest)
      CALL suwrt(sav2(1),9,itest)
   ENDDO
   IF ( j<Npsub ) GOTO 1200
 1400 IF ( .NOT.Tocopn ) CALL close(Sctoc,1)
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
 1500 IF ( andf(rshift(Iprint,16),1)==1 ) THEN
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
 1600 imsg = -1
   GOTO 1900
 1700 imsg = -2
   GOTO 1900
 1800 imsg = -8
 1900 CALL sofcls
   CALL mesage(imsg,ifile,aaa)
END SUBROUTINE cmsofo