!*==cmsfil.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmsfil
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_cmb004
   USE c_gtmatx
   USE c_zzzzzz
   IMPLICIT NONE
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
         bufex = lcore - buf1 + buf2
         lcore = bufex - 1
         IF ( lcore<0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         llco = lcore
         ioefil = 310
         ifile = scbdat
         CALL open(*60,scbdat,z(buf4),0)
!
!     READ GTRAN DATA INTO OPEN CORE
!
         IF ( tdat(3) .OR. tdat(6) ) CALL skpfil(scbdat,1)
         IF ( .NOT.tdat(6) ) THEN
!
!     READ TRANS DATA INTO OPEN CORE
!
            kstran = score
            spag_nextblock_1 = 2
         ELSE
            ksgtrn = score
            CALL read(*80,*20,scbdat,z(ksgtrn),llco,1,klgtrn)
            spag_nextblock_1 = 4
         ENDIF
         CYCLE
 20      llco = llco - klgtrn
         kfgtrn = ksgtrn + klgtrn - 1
         kstran = kfgtrn + 1
         spag_nextblock_1 = 2
      CASE (2)
         IF ( tdat(3) .OR. tdat(6) ) CALL skpfil(scbdat,1)
         IF ( .NOT.tdat(3) ) THEN
            loc1 = 0
            xtran = .FALSE.
            spag_nextblock_1 = 3
         ELSE
            CALL read(*80,*40,scbdat,z(kstran),llco,1,kltran)
            spag_nextblock_1 = 4
         ENDIF
         CYCLE
 40      loc1 = kstran
         llco = llco - kltran
         xtran = .TRUE.
         kftran = kstran + kltran - 1
         spag_nextblock_1 = 3
      CASE (3)
         CALL close(scbdat,1)
         ifile = scsfil
         CALL open(*60,scsfil,z(buf4),1)
         ifile = sccstm
         CALL open(*60,sccstm,z(buf3),1)
         kkc = 0
!
!     LOOP ON EACH PSEUDOSTRUCTURE
!
         ifile = scr3
         CALL open(*60,scr3,z(buf1),1)
         ifile = ioefil
         CALL open(*60,ioefil,z(bufex),1)
         llcold = llco
!
         DO i = 1 , npsub
            llco = llcold
            nam(1) = combo(i,1)
            nam(2) = combo(i,2)
            trn = combo(i,3)
            sym = combo(i,4)
            ncomp = combo(i,5)
!
!     READ BGSS FOR I-TH PSEUDOSTRUCTURE
!
            ksbgss = kstran
            IF ( xtran ) ksbgss = kstran + kltran
            CALL sfetch(nam,nhbgss,1,itest)
            ngrp = 1
            CALL sjump(ngrp)
            CALL suread(z(ksbgss),llco,klbgss,itest)
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
                  CALL suread(z(kscstm),llco,klcstm,itest)
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
                  z(j) = 0
               ENDDO
               DO j = kshptr , kfhptr
                  z(j) = 0
               ENDDO
!
!     GET THE TRANS AND SYMT MATRIX FOR THIS PSEUDOSTRUCTURE
!
               CALL gtmat1(sym,tt)
!
!     TRANSFORM THE COORDINATES IN THE BGSS, NOTE THAT THE ORIGINS
!     FOR TRANSLATION ARE STORED IN ARRAY ORIGIN.
!
               IF ( trn+sym/=0 ) THEN
                  DO j = ksbgss , kfbgss , 4
                     IF ( z(j)/=-1 ) THEN
                        CALL gmmats(tt,3,3,0,rz(j+1),3,1,0,xx)
                        DO jj = 1 , 3
                           rz(j+jj) = xx(jj) + origin(i,jj)
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
!
!     TRANSFORM DEGREES OF FREEDOM FOR EACH EQSS CONTAINED
!     IN THE PSEUDOSTRUCTURE.
!
               CALL write(scr3,tt6,36,1)
               nhmat = 1
               CALL sfetch(nam,nheqss,1,itest)
               kneqss = kfhptr + 1
               CALL suread(z(kneqss),4,kleqss,itest)
               CALL suread(z(kneqss),-1,kleqss,itest)
               llco = llco - 2*ncomp
               ifile = scmcon
               CALL open(*60,scmcon,z(buf2),1)
               DO j = 1 , ncomp
                  kseqss = kfhptr + 2*ncomp
                  CALL suread(z(kseqss),llco,kleqss,itest)
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
                              ip = z(jj+1)
                              icomp = z(jj+2)
!
!     GET CSTM FOR THIS IP
!
                              cstmid = z(ksbgss+4*ip-4)
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
                              IF ( tdat(6) ) THEN
                                 cgid = 1000000*j + z(jj)
                                 DO k = ksgtrn , kfgtrn , 5
                                    IF ( z(k+3)==cgid .AND. z(k)==i .AND. z(k+1)==j ) THEN
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
                           CASE (2)
                              CALL gtmat3(z(k+4),tg,tg6,ikind)
                              igtran = 1
                              spag_nextblock_2 = 3
                           CASE (3)
!
!     ALL TRANSFORMATIONS HAVE BEEN FOUND, COMPUTE THE FINAL MATRIX TMAT
!
                              CALL gmmats(tg6,6,6,1,tt6,6,6,0,tsave)
                              CALL gmmats(tsave,6,6,0,tc6,6,6,0,tmat)
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
                              z(jj+2) = icode
!
!     WRITE IP,C ON SCRATCH TO COMPUTE NEW SIL,C
!
                              CALL write(scmcon,ip,1,0)
                              CALL write(scmcon,icode,1,0)
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
                                 z(iadd1) = z(iadd)
                                 IF ( z(iadd)==-1 ) z(iadd1) = -100000000
                                 IF ( z(iadd)==-2 ) z(iadd1) = -200000000
                              ELSEIF ( ikkind==7 .OR. ikkind==8 ) THEN
                                 z(iadd1) = 0
                              ELSEIF ( ikkind==9 .OR. ikkind==10 .OR. ikkind==11 .OR. ikkind==12 .OR. ikkind==17 .OR.               &
                                     & ikkind==18 .OR. ikkind==19 .OR. ikkind==20 .OR. ikkind==21 .OR. ikkind==22 .OR.              &
                                     & ikkind==23 .OR. ikkind==24 .OR. ikkind==25 .OR. ikkind==26 .OR. ikkind==27 .OR. ikkind==28 ) &
                                     & THEN
                              ELSEIF ( ikkind==13 .OR. ikkind==14 ) THEN
                                 z(iadd1) = -trn
                              ELSEIF ( ikkind==15 .OR. ikkind==16 .OR. ikkind==29 .OR. ikkind==30 .OR. ikkind==31 .OR.              &
                                     & ikkind==32 .OR. ikkind==33 .OR. ikkind==34 .OR. ikkind==35 ) THEN
                                 z(iadd1) = -z(k+4)
                              ELSE
                                 z(iadd1) = z(iadd)
                                 IF ( z(iadd)==-1 ) z(iadd1) = -100000000
                                 IF ( z(iadd)==-2 ) z(iadd1) = -200000000
                              ENDIF
!
!     SET POINTERS FOR H MATRIX
!
                              itis = 0
                              iadd2 = kshptr + ip - 1
                              IF ( cstmid>=0 ) THEN
                                 IF ( z(iadd2)<=2 ) THEN
                                    IF ( ikkind==3 .OR. ikkind==4 .OR. ikkind==13 .OR. ikkind==14 ) itis = 1
                                    IF ( ikkind==1 .OR. ikkind==2 .OR. ikkind==5 .OR. ikkind==6 ) itis = 2
                                    IF ( itis<1 ) THEN
                                       nhmat = nhmat + 1
                                       z(iadd2) = nhmat
                                       CALL write(scr3,tmat,36,1)
                                    ELSEIF ( itis==1 ) THEN
                                       spag_nextblock_2 = 4
                                    ELSE
                                       z(iadd2) = 1
                                    ENDIF
                                 ENDIF
                                 CYCLE
                              ENDIF
                              spag_nextblock_2 = 4
                           CASE (4)
                              z(iadd2) = 0
                              EXIT SPAG_DispatchLoop_2
                           END SELECT
                        ENDDO SPAG_DispatchLoop_2
                     ENDDO
!
!     INSERT MULTIPLE IP CODE
!
                     IF ( ncomp/=1 ) CALL eqscod(kseqss,kleqss,z(1))
                  ENDIF
!
!     WRITE EQSS ON FILE SCSFIL
!
                  CALL write(scsfil,z(kseqss),kleqss,1)
                  twojm1 = 2*(j-1)
                  IF ( andf(rshift(iprint,19),1)==1 ) CALL cmiwrt(1,nam,z(kneqss+twojm1),kseqss,kleqss,z,z)
               ENDDO
               CALL eof(scr3)
               CALL close(scmcon,1)
!
!     GENERATE NEW SIL,C LIST
!
               ifile = scmcon
               CALL open(*60,scmcon,z(buf2),0)
               CALL read(*50,*50,scmcon,z(kseqss),llco,1,nnn)
            ENDIF
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
 50         CALL sort(0,0,2,1,z(kseqss),nnn)
            ksej = kseqss + nnn - 1
            i1 = kseqss
            i2 = kseqss + 2
            DO WHILE ( i2-kseqss<nnn )
               IF ( z(i1)==z(i2) ) THEN
                  DO j = i2 , ksej
                     z(j-2) = z(j)
                  ENDDO
                  ksej = ksej - 2
                  nnn = nnn - 2
               ELSE
                  i1 = i1 + 2
                  i2 = i2 + 2
               ENDIF
            ENDDO
            z(kseqss) = 1
            DO j = 3 , nnn , 2
               jj = j - 1
               icode = z(kseqss+jj-1)
               CALL decode(icode,list,ndof)
               z(kseqss+jj) = z(kseqss+jj-2) + ndof
            ENDDO
            CALL write(scsfil,z(kseqss),nnn,1)
            CALL suread(z(kseqss),llco,kleqss,itest)
            CALL write(ioefil,z(kseqss),kleqss,1)
            CALL close(scmcon,1)
!
!     PRINT EQSS SIL LIST IF REQUESTED
!
            IF ( andf(rshift(iprint,19),1)==1 ) CALL cmiwrt(8,nam,0,kseqss,kleqss,z,z)
!
!     UPDATE CSTM NUMBERING SYSTEM
!     KKC IS TRANSFORMED SYSTEM COORD. ID
!
            ip = 0
            DO i6 = ksncid , kfncid
               ip = ip + 1
               loc = ksbgss + 4*(ip-1)
               IF ( z(i6)/=100000000 ) THEN
                  IF ( z(i6)<0 ) THEN
                     IF ( z(i6)==-100000000 ) THEN
                        z(loc) = -1
                        IF ( z(i6)==-200000000 ) z(loc) = -2
                        CYCLE
                     ELSE
                        i1 = kstran
                        i2 = kftran
                     ENDIF
                  ELSEIF ( z(i6)==0 ) THEN
                     z(loc) = 0
                     CYCLE
                  ELSE
                     i1 = kscstm
                     i2 = kfcstm
                  ENDIF
                  kkc = kkc + 1
                  IF ( .NOT.(iauto) ) THEN
                     IF ( kkc<=1 ) THEN
                        CALL page1
                        CALL page2(5)
                        WRITE (outt,99001)
99001                   FORMAT (//45X,'SUMMARY OF OVERALL SYSTEM COORDINATES',//36X,                                                &
                               &'PSEUDO STRUCTURE ID.   SYSTEM COORD.ID    USER COORD.ID',/)
                     ENDIF
                     CALL page2(1)
                     WRITE (outt,99002) i , kkc , z(loc)
99002                FORMAT (43X,I6,14X,I6,11X,I6)
                  ENDIF
                  look4 = z(i6)
                  DO j6 = i1 , i2 , 14
                     IF ( iabs(z(i6))==z(j6) ) THEN
                        IF ( z(i6)<=0 ) THEN
                           CALL write(sccstm,kkc,1,0)
                           CALL write(sccstm,z(j6+1),13,0)
                        ELSE
                           CALL gmmats(tt,3,3,0,z(j6+5),3,3,0,tc)
                           CALL write(sccstm,kkc,1,0)
                           CALL write(sccstm,z(j6+1),4,0)
                           CALL write(sccstm,tc,9,0)
                        ENDIF
                     ENDIF
                  ENDDO
!
!     FIND OTHER CIDS THAT ARE THE SAME
!
                  iip = 0
                  DO j6 = ksncid , kfncid
                     iip = iip + 1
                     IF ( z(j6)==look4 ) THEN
                        loc = ksbgss + 4*(iip-1)
                        z(loc) = kkc
                        z(j6) = 100000000
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
!
!     WRITE PROCESSED BGSS
!
            CALL write(scsfil,z(ksbgss),klbgss,1)
            IF ( andf(rshift(iprint,18),1)==1 ) CALL cmiwrt(2,nam,nam,ksbgss,klbgss,z(1),z(1))
!
!     WRITE ARRAY OF H POINTERS
!
            CALL write(scsfil,z(kshptr),klhptr,1)
            CALL eof(scsfil)
         ENDDO
!
         CALL close(scr3,1)
         CALL close(scsfil,1)
         CALL write(sccstm,tmat,0,1)
         CALL close(sccstm,1)
         CALL close(ioefil,1)
         lcore = bufex + buf1 - buf2
         RETURN
!
 60      imsg = -1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 80      imsg = -2
         spag_nextblock_1 = 5
      CASE (4)
         imsg = -8
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cmsfil
