!*==cmsfil.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmsfil
   IMPLICIT NONE
   USE C_CMB001
   USE C_CMB002
   USE C_CMB003
   USE C_CMB004
   USE C_GTMATX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa
   INTEGER :: bufex , cgid , cstmid , ecpt1 , i , i1 , i2 , i6 , iadd , iadd1 , iadd2 , icode , icomp , ifile , igtran , iip ,      &
            & ikind , ikkind , imsg , ioefil , ip , itest , itis , j , j6 , jdh , jj , k , kfbgss , kfcstm , kfeqss , kfgtrn ,      &
            & kfhptr , kfncid , kftran , kkc , klbgss , klcstm , kleqss , klgtrn , klhptr , klncid , kneqss , ksbgss , kscstm ,     &
            & ksej , kseqss , ksgtrn , kshptr , ksncid , kstran , l , llco , llcold , loc , loc2 , look4 , ncomp , ndof , ngrp ,    &
            & nhmat , nip , nnn , sym , twojm1
   REAL , DIMENSION(6) :: dofn
   REAL , DIMENSION(4) :: ecpt
   INTEGER , DIMENSION(32) :: list
   INTEGER , DIMENSION(2) :: nam
   INTEGER , SAVE :: nhbgss , nhcstm , nheqss
   REAL , DIMENSION(1) :: rz
   REAL , DIMENSION(3,3) :: tc , tg , tt
   REAL , DIMENSION(6,6) :: tg6 , tmat , tsave
   LOGICAL :: xcstm , xtran
   REAL , DIMENSION(3) :: xx
   EXTERNAL andf , close , cmiwrt , decode , eof , eqscod , gmmats , gtmat1 , gtmat2 , gtmat3 , mesage , open , page1 , page2 ,     &
          & read , rshift , sfetch , sjump , skpfil , sort , suread , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS SUBROUTINE GENERATES THE WORKING SUBSTRUCTURE FILE AND
!     APPLIES ALL TRANSFORMATIONS
!
!WKBI ALPHA-OSF 9/94
   !>>>>EQUIVALENCE (ecpt1,ecpt(1)) , (Rz(1),Z(1))
   DATA aaa/4H CMS , 4HFIL /
   DATA nhbgss , nhcstm , nheqss/4HBGSS , 4HCSTM , 4HEQSS/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         bufex = Lcore - Buf1 + Buf2
         Lcore = bufex - 1
         IF ( Lcore<0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         llco = Lcore
         ioefil = 310
         ifile = Scbdat
         CALL open(*60,Scbdat,Z(Buf4),0)
!
!     READ GTRAN DATA INTO OPEN CORE
!
         IF ( Tdat(3) .OR. Tdat(6) ) CALL skpfil(Scbdat,1)
         IF ( .NOT.Tdat(6) ) THEN
!
!     READ TRANS DATA INTO OPEN CORE
!
            kstran = Score
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            ksgtrn = Score
            CALL read(*80,*20,Scbdat,Z(ksgtrn),llco,1,klgtrn)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      llco = llco - klgtrn
         kfgtrn = ksgtrn + klgtrn - 1
         kstran = kfgtrn + 1
         spag_nextblock_1 = 2
      CASE (2)
         IF ( Tdat(3) .OR. Tdat(6) ) CALL skpfil(Scbdat,1)
         IF ( .NOT.Tdat(3) ) THEN
            Loc1 = 0
            xtran = .FALSE.
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL read(*80,*40,Scbdat,Z(kstran),llco,1,Kltran)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      Loc1 = kstran
         llco = llco - Kltran
         xtran = .TRUE.
         kftran = kstran + Kltran - 1
         spag_nextblock_1 = 3
      CASE (3)
         CALL close(Scbdat,1)
         ifile = Scsfil
         CALL open(*60,Scsfil,Z(Buf4),1)
         ifile = Sccstm
         CALL open(*60,Sccstm,Z(Buf3),1)
         kkc = 0
!
!     LOOP ON EACH PSEUDOSTRUCTURE
!
         ifile = Scr3
         CALL open(*60,Scr3,Z(Buf1),1)
         ifile = ioefil
         CALL open(*60,ioefil,Z(bufex),1)
         llcold = llco
!
         DO i = 1 , Npsub
            llco = llcold
            nam(1) = Combo(i,1)
            nam(2) = Combo(i,2)
            Trn = Combo(i,3)
            sym = Combo(i,4)
            ncomp = Combo(i,5)
!
!     READ BGSS FOR I-TH PSEUDOSTRUCTURE
!
            ksbgss = kstran
            IF ( xtran ) ksbgss = kstran + Kltran
            CALL sfetch(nam,nhbgss,1,itest)
            ngrp = 1
            CALL sjump(ngrp)
            CALL suread(Z(ksbgss),llco,klbgss,itest)
            IF ( klbgss/=llco .OR. itest==2 ) THEN
               llco = llco - klbgss
               kfbgss = ksbgss + klbgss - 1
               nip = klbgss/4
!
!     READ CSTM FOR THIS PSEUDOSTRUCTURE
!
               CALL sfetch(nam,nhcstm,1,itest)
               xcstm = .FALSE.
               loc2 = 0
               IF ( itest/=3 ) THEN
                  kscstm = kfbgss + 1
                  ngrp = 1
                  CALL sjump(ngrp)
                  CALL suread(Z(kscstm),llco,klcstm,itest)
                  IF ( klcstm==llco .AND. itest/=2 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  llco = llco - klcstm
                  loc2 = kscstm
                  xcstm = .TRUE.
                  kfcstm = kscstm + klcstm - 1
               ENDIF
!
!     DEFINE OPEN CORE ARRAYS FOR NEW CID AND HPTR
!
               ksncid = kfbgss + 1
               IF ( loc2/=0 ) ksncid = kfcstm + 1
               klncid = nip
               llco = llco - klncid
               kfncid = ksncid + klncid - 1
!
               kshptr = kfncid + 1
               klhptr = nip
               llco = llco - klhptr
               kfhptr = kshptr + klhptr - 1
!
!     SET ARRAYS TO ZERO
!
               DO j = ksncid , kfncid
                  Z(j) = 0
               ENDDO
               DO j = kshptr , kfhptr
                  Z(j) = 0
               ENDDO
!
!     GET THE TRANS AND SYMT MATRIX FOR THIS PSEUDOSTRUCTURE
!
               CALL gtmat1(sym,tt)
!
!     TRANSFORM THE COORDINATES IN THE BGSS, NOTE THAT THE ORIGINS
!     FOR TRANSLATION ARE STORED IN ARRAY ORIGIN.
!
               IF ( Trn+sym/=0 ) THEN
                  DO j = ksbgss , kfbgss , 4
                     IF ( Z(j)/=-1 ) THEN
                        CALL gmmats(tt,3,3,0,rz(j+1),3,1,0,xx)
                        DO jj = 1 , 3
                           rz(j+jj) = xx(jj) + Origin(i,jj)
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
!
!     TRANSFORM DEGREES OF FREEDOM FOR EACH EQSS CONTAINED
!     IN THE PSEUDOSTRUCTURE.
!
               CALL write(Scr3,Tt6,36,1)
               nhmat = 1
               CALL sfetch(nam,nheqss,1,itest)
               kneqss = kfhptr + 1
               CALL suread(Z(kneqss),4,kleqss,itest)
               CALL suread(Z(kneqss),-1,kleqss,itest)
               llco = llco - 2*ncomp
               ifile = Scmcon
               CALL open(*60,Scmcon,Z(Buf2),1)
               DO j = 1 , ncomp
                  kseqss = kfhptr + 2*ncomp
                  CALL suread(Z(kseqss),llco,kleqss,itest)
                  IF ( kleqss/=0 ) THEN
                     IF ( kleqss==llco .AND. itest/=2 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     kfeqss = kseqss + kleqss - 1
!
!     LOOP ON EACH IP IN THE EQSS AND GENERATE TRANSFORMATION MATRIX
!
                     DO jj = kseqss , kfeqss , 3
                        spag_nextblock_2 = 1
                        SPAG_DispatchLoop_2: DO
                           SELECT CASE (spag_nextblock_2)
                           CASE (1)
                              ip = Z(jj+1)
                              icomp = Z(jj+2)
!
!     GET CSTM FOR THIS IP
!
                              cstmid = Z(ksbgss+4*ip-4)
                              ecpt1 = cstmid
                              IF ( cstmid<0 ) ecpt1 = 0
                              DO jdh = 1 , 3
                                 ecpt(jdh+1) = rz(ksbgss+4*ip-4+jdh)
                              ENDDO
                              CALL gtmat2(loc2,klcstm,ecpt,tc)
!
!     TEST FOR POSSIBLE GTRAN
!
                              igtran = 0
                              IF ( Tdat(6) ) THEN
                                 cgid = 1000000*j + Z(jj)
                                 DO k = ksgtrn , kfgtrn , 5
                                    IF ( Z(k+3)==cgid .AND. Z(k)==i .AND. Z(k+1)==j ) THEN
                                       spag_nextblock_2 = 2
                                       CYCLE SPAG_DispatchLoop_2
                                    ENDIF
!
!     NO GTRAN
!
                                 ENDDO
                              ENDIF
                              CALL gtmat3(-1,tg,tg6,ikind)
                              spag_nextblock_2 = 3
                              CYCLE SPAG_DispatchLoop_2
                           CASE (2)
                              CALL gtmat3(Z(k+4),tg,tg6,ikind)
                              igtran = 1
                              spag_nextblock_2 = 3
                           CASE (3)
!
!     ALL TRANSFORMATIONS HAVE BEEN FOUND, COMPUTE THE FINAL MATRIX TMAT
!
                              CALL gmmats(tg6,6,6,1,Tt6,6,6,0,tsave)
                              CALL gmmats(tsave,6,6,0,Tc6,6,6,0,tmat)
!
!     DECODE DEGREES OF FREEDOM AND FORM VECTOR
!
                              CALL decode(icomp,list,ndof)
!
!     FIND NEW DEGREES OF FREEDOM AND UPDATE EQSS
!
                              IF ( cstmid/=0 .AND. igtran==0 ) THEN
                                 icode = icomp
                              ELSE
                                 DO i1 = 1 , 6
                                    dofn(i1) = 0.0
                                    SPAG_Loop_5_1: DO i2 = 1 , ndof
                                       l = list(i2) + 1
                                       IF ( abs(tmat(l,i1))>=1.0E-4 ) THEN
                                         dofn(i1) = 1.0
                                         EXIT SPAG_Loop_5_1
                                       ENDIF
                                    ENDDO SPAG_Loop_5_1
                                 ENDDO
                                 icode = 0
                                 DO i1 = 1 , 6
                                    icode = icode + dofn(i1)*2**(i1-1)
                                 ENDDO
                              ENDIF
                              Z(jj+2) = icode
!
!     WRITE IP,C ON SCRATCH TO COMPUTE NEW SIL,C
!
                              CALL write(Scmcon,ip,1,0)
                              CALL write(Scmcon,icode,1,0)
!
!     UPDATE CID NUMBERS
!
                              iadd = ksbgss + 4*ip - 4
                              iadd1 = ksncid + ip - 1
                              ikkind = ikind + 1
                              IF ( ikkind==3 .OR. ikkind==4 ) THEN
!
!     COMMENTS FROM G.CHAN/UNISYS  9/92
!     250 AND 240 ARE IDENTICAL HERE. IS IT POSSIBLY AN ERROR HERE?
!
                                 Z(iadd1) = Z(iadd)
                                 IF ( Z(iadd)==-1 ) Z(iadd1) = -100000000
                                 IF ( Z(iadd)==-2 ) Z(iadd1) = -200000000
                              ELSEIF ( ikkind==7 .OR. ikkind==8 ) THEN
                                 Z(iadd1) = 0
                              ELSEIF ( ikkind==9 .OR. ikkind==10 .OR. ikkind==11 .OR. ikkind==12 .OR. ikkind==17 .OR.               &
                                     & ikkind==18 .OR. ikkind==19 .OR. ikkind==20 .OR. ikkind==21 .OR. ikkind==22 .OR.              &
                                     & ikkind==23 .OR. ikkind==24 .OR. ikkind==25 .OR. ikkind==26 .OR. ikkind==27 .OR. ikkind==28 ) &
                                     & THEN
                              ELSEIF ( ikkind==13 .OR. ikkind==14 ) THEN
                                 Z(iadd1) = -Trn
                              ELSEIF ( ikkind==15 .OR. ikkind==16 .OR. ikkind==29 .OR. ikkind==30 .OR. ikkind==31 .OR.              &
                                     & ikkind==32 .OR. ikkind==33 .OR. ikkind==34 .OR. ikkind==35 ) THEN
                                 Z(iadd1) = -Z(k+4)
                              ELSE
                                 Z(iadd1) = Z(iadd)
                                 IF ( Z(iadd)==-1 ) Z(iadd1) = -100000000
                                 IF ( Z(iadd)==-2 ) Z(iadd1) = -200000000
                              ENDIF
!
!     SET POINTERS FOR H MATRIX
!
                              itis = 0
                              iadd2 = kshptr + ip - 1
                              IF ( cstmid>=0 ) THEN
                                 IF ( Z(iadd2)<=2 ) THEN
                                    IF ( ikkind==3 .OR. ikkind==4 .OR. ikkind==13 .OR. ikkind==14 ) itis = 1
                                    IF ( ikkind==1 .OR. ikkind==2 .OR. ikkind==5 .OR. ikkind==6 ) itis = 2
                                    IF ( itis<1 ) THEN
                                       nhmat = nhmat + 1
                                       Z(iadd2) = nhmat
                                       CALL write(Scr3,tmat,36,1)
                                    ELSEIF ( itis==1 ) THEN
                                       spag_nextblock_2 = 4
                                       CYCLE SPAG_DispatchLoop_2
                                    ELSE
                                       Z(iadd2) = 1
                                    ENDIF
                                 ENDIF
                                 CYCLE
                              ENDIF
                              spag_nextblock_2 = 4
                           CASE (4)
                              Z(iadd2) = 0
                              EXIT SPAG_DispatchLoop_2
                           END SELECT
                        ENDDO SPAG_DispatchLoop_2
                     ENDDO
!
!     INSERT MULTIPLE IP CODE
!
                     IF ( ncomp/=1 ) CALL eqscod(kseqss,kleqss,Z(1))
                  ENDIF
!
!     WRITE EQSS ON FILE SCSFIL
!
                  CALL write(Scsfil,Z(kseqss),kleqss,1)
                  twojm1 = 2*(j-1)
                  IF ( andf(rshift(Iprint,19),1)==1 ) CALL cmiwrt(1,nam,Z(kneqss+twojm1),kseqss,kleqss,Z,Z)
               ENDDO
               CALL eof(Scr3)
               CALL close(Scmcon,1)
!
!     GENERATE NEW SIL,C LIST
!
               ifile = Scmcon
               CALL open(*60,Scmcon,Z(Buf2),0)
               CALL read(*50,*50,Scmcon,Z(kseqss),llco,1,nnn)
            ENDIF
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
 50         CALL sort(0,0,2,1,Z(kseqss),nnn)
            ksej = kseqss + nnn - 1
            i1 = kseqss
            i2 = kseqss + 2
            DO WHILE ( i2-kseqss<nnn )
               IF ( Z(i1)==Z(i2) ) THEN
                  DO j = i2 , ksej
                     Z(j-2) = Z(j)
                  ENDDO
                  ksej = ksej - 2
                  nnn = nnn - 2
               ELSE
                  i1 = i1 + 2
                  i2 = i2 + 2
               ENDIF
            ENDDO
            Z(kseqss) = 1
            DO j = 3 , nnn , 2
               jj = j - 1
               icode = Z(kseqss+jj-1)
               CALL decode(icode,list,ndof)
               Z(kseqss+jj) = Z(kseqss+jj-2) + ndof
            ENDDO
            CALL write(Scsfil,Z(kseqss),nnn,1)
            CALL suread(Z(kseqss),llco,kleqss,itest)
            CALL write(ioefil,Z(kseqss),kleqss,1)
            CALL close(Scmcon,1)
!
!     PRINT EQSS SIL LIST IF REQUESTED
!
            IF ( andf(rshift(Iprint,19),1)==1 ) CALL cmiwrt(8,nam,0,kseqss,kleqss,Z,Z)
!
!     UPDATE CSTM NUMBERING SYSTEM
!     KKC IS TRANSFORMED SYSTEM COORD. ID
!
            ip = 0
            DO i6 = ksncid , kfncid
               ip = ip + 1
               loc = ksbgss + 4*(ip-1)
               IF ( Z(i6)/=100000000 ) THEN
                  IF ( Z(i6)<0 ) THEN
                     IF ( Z(i6)==-100000000 ) THEN
                        Z(loc) = -1
                        IF ( Z(i6)==-200000000 ) Z(loc) = -2
                        CYCLE
                     ELSE
                        i1 = kstran
                        i2 = kftran
                     ENDIF
                  ELSEIF ( Z(i6)==0 ) THEN
                     Z(loc) = 0
                     CYCLE
                  ELSE
                     i1 = kscstm
                     i2 = kfcstm
                  ENDIF
                  kkc = kkc + 1
                  IF ( .NOT.(Iauto) ) THEN
                     IF ( kkc<=1 ) THEN
                        CALL page1
                        CALL page2(5)
                        WRITE (Outt,99001)
99001                   FORMAT (//45X,'SUMMARY OF OVERALL SYSTEM COORDINATES',//36X,                                                &
                               &'PSEUDO STRUCTURE ID.   SYSTEM COORD.ID    USER COORD.ID',/)
                     ENDIF
                     CALL page2(1)
                     WRITE (Outt,99002) i , kkc , Z(loc)
99002                FORMAT (43X,I6,14X,I6,11X,I6)
                  ENDIF
                  look4 = Z(i6)
                  DO j6 = i1 , i2 , 14
                     IF ( iabs(Z(i6))==Z(j6) ) THEN
                        IF ( Z(i6)<=0 ) THEN
                           CALL write(Sccstm,kkc,1,0)
                           CALL write(Sccstm,Z(j6+1),13,0)
                        ELSE
                           CALL gmmats(tt,3,3,0,Z(j6+5),3,3,0,tc)
                           CALL write(Sccstm,kkc,1,0)
                           CALL write(Sccstm,Z(j6+1),4,0)
                           CALL write(Sccstm,tc,9,0)
                        ENDIF
                     ENDIF
                  ENDDO
!
!     FIND OTHER CIDS THAT ARE THE SAME
!
                  iip = 0
                  DO j6 = ksncid , kfncid
                     iip = iip + 1
                     IF ( Z(j6)==look4 ) THEN
                        loc = ksbgss + 4*(iip-1)
                        Z(loc) = kkc
                        Z(j6) = 100000000
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
!
!     WRITE PROCESSED BGSS
!
            CALL write(Scsfil,Z(ksbgss),klbgss,1)
            IF ( andf(rshift(Iprint,18),1)==1 ) CALL cmiwrt(2,nam,nam,ksbgss,klbgss,Z(1),Z(1))
!
!     WRITE ARRAY OF H POINTERS
!
            CALL write(Scsfil,Z(kshptr),klhptr,1)
            CALL eof(Scsfil)
         ENDDO
!
         CALL close(Scr3,1)
         CALL close(Scsfil,1)
         CALL write(Sccstm,tmat,0,1)
         CALL close(Sccstm,1)
         CALL close(ioefil,1)
         Lcore = bufex + Buf1 - Buf2
         RETURN
!
 60      imsg = -1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 80      imsg = -2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         imsg = -8
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cmsfil
