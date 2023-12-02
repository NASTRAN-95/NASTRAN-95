!*==mpyado.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
SUBROUTINE mpyado(Zz,Z,Zd)
   USE c_logout
   USE c_machin
   USE c_mpyadx
   USE c_mpyadz
   USE c_mpyqt4
   USE c_names
   USE c_ntime
   USE c_packx
   USE c_system
   USE c_type
   USE c_unpakx
   USE c_zblpkx
   USE c_zntpkx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(6) :: Zz
   REAL , DIMENSION(1) :: Z
   REAL(REAL64) , DIMENSION(1) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: ad , bd , dd
   REAL :: arith , aterm , bterm , core , cterm , cterm2 , dterm , dterm2 , fm , fn , fnt , fp , fp1 , fr , rhoa , rhob , rhoc ,    &
         & rhod , time3
   INTEGER :: arow , arow1 , arown , buf1 , buf2 , buf3 , buf4 , bufi , cfile , densc , dfile , efile , file , flag , form , icrq , &
            & ielems , ierr , ii , inc , iprec , irowb , irowc , it , itime , itimgo , itypea , itypeb , itypec , itypsg , j ,      &
            & jcol , jelems , jj , jmax1 , jz , jzb , jzdb , k , k2 , kr1 , krn , ksys58 , l , l19 , l41 , m , mm , mout , mpass1 , &
            & mpass2 , mpass3 , mt2 , mt4 , n , nbd , nbpw , nbrrow , nbrstr , ncore , noab , nogo , nout , nwda1 , nwdb1 , nzz ,   &
            & op , op2 , opa , opb , opbc , opc , p , p1 , point , prca , q , r , row , rowa
   REAL , DIMENSION(4) :: b
   INTEGER , DIMENSION(2) :: bcd , namea , nameb , namec , named
   INTEGER , DIMENSION(15) :: blk
   INTEGER , SAVE :: jbegn , jend
   LOGICAL :: last , null
   INTEGER , DIMENSION(6) , SAVE :: method
   INTEGER , DIMENSION(3) :: mpy
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(4) , SAVE :: prntyp
   INTEGER :: sysbuf , typd , type , typea , typeb , typec
   REAL , SAVE :: time1 , time2
   REAL , DIMENSION(1) :: xns
   REAL , DIMENSION(4) , SAVE :: zero
   EXTERNAL andf , bckrec , bldpk , bldpki , bldpkn , close , conmsg , cpystr , endget , filswi , fname , fwdrec , getstr , gopen , &
          & intpk , lshift , mesage , mpy1v , mpy2nv , mpy2tv , mpy3t , mpy4t , mpydri , mpyq , open , orf , pack , sswtch ,        &
          & tmtogo , unpack , write , zblpki , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   INTEGER :: spag_nextblock_4
!
!     MPYAD PERFORMS THE MATRIX OPERATION
!       (+/-)A    * B (+/-)C = D   OR
!       (+/-)A(T) * B (+/-)C = D
!
!     LAST REVISED  1/92 BY G.CHAN/UNISYS
!     . NEW METHOD 4T WAS ADDED WHICH IS FASTER THAN METHOD 2T UNDER
!       CERTAIN CONDITIONS.
!     . NEW ROUTINE FOR DIAGONAL, IDENTITY, AND ROW VECTOR MATRICES
!     . USER CAN REVERT TO ORIGINAL MPYAD ALL METHODS, BY DIAG 41
!
!
!     LEGEND:
!
!     +---+ + +  IS A MATRIX       +-            \    AN ELEMENT OF
!     |   | | |  BY COLUMNS        |  IS A        \   A, B, OR C IN
!     |   | | |  IN MULTIPLE       |  COLUMN       \     AND
!     |   | | |  PASSES            |             OR   /  AN ELEMENT
!     +---+ + +                    +-                /   OF D OUT
!
!     +-------+  IS A MATRIX
!     |       |  BY ROWS           => OR   INDICATES MATRICES C AND D
!     +-------+  IN MULTIPLE       <=      ARE USING SAME CORE SPACE
!     +-------+  PASSES
!     +-------+
!
!     UPPER CASE LETTER INDICATES UNPACKED MATRIX OR COLUMN
!     LOWER CASE LETTER INDICATES MATRIX OR COLUMN IN STRINGS FORM
!
!
!     METHOD 1NT AND 1T       METHOD 2NT               METHOD 2T
!                                        B                        +-
!         +----+ + +                    /                         |
!         |    | | |                   /                          |B
!     a   | B  | | |          + + +---+ +-                        |
!      \  |    | | |          | | |   | |            +----------+ +-
!       \ +----+ + +          | | | a | |C           |    a     |
!         +----+              | | |   | |            +----------+
!         |    |              + + +---+ +- +-        +----------+ \
!         | D  | <= C                      |         +----------+  \
!         |    |                        => |D                     \ C
!         |    |                           |                       \
!         +----+                           +-                       D
!
!      METHOD 3T     +-                  METHOD 4T     +-
!                    |                                 |
!                    |b                                |b(BANDED)
!                    |                                 |
!                    +-                                +-
!         + + +----+ +-    +-              +---------+ +-
!         | | |    | |     |               |         | |
!         | | | A  | |D  + |C              |    a    | |C
!         | | |    | |     |               |         | |
!         + + +----+ +-    +-              +---------+ +-  +-
!                        ADD ON            +---------+     |
!                        LAST              +---------+     |D(FULL)
!                        PASS                           => |
!                                                          +-
!
!WKBI 9/93
!NVXNB
!NVXNE
!WKBI 10/93
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Mout) , (Ksystm(58),Ksys58) , (Ksystm(40),Nbpw) , (Ksystm(55),Iprec)
   !>>>>EQUIVALENCE (A(1),Ad(1)) , (b(1),bd(1)) , (D(1),Dd(1)) , (Filea(2),M) , (Filea(3),N,Rowa) , (Filea(5),Typea) , (Fileb(2),Q) ,    &
!>>>>    & (Fileb(3),R) , (Fileb(5),Typeb) , (Filec(5),Typec) , (Filed(5),Typd) , (nzz,buf1) , (Acoln,Arown) , (Filec(7),Densc)
   !>>>>EQUIVALENCE (Block(2),Type) , (Block(3),Form) , (Block(4),Row) , (Block(5),Point) , (Block(6),Nbrstr) , (Block(8),Flag) ,        &
!>>>>    & (Xnd(1),Xns(1)) , (Acol1,Arow1) , (Acol,Arow) , (mpy(1),name(1))
!
   DATA name/4HMPYA , 4HD   / , jbegn/4HBEGN/ , jend/3HEND/time1/0./ , time2/0./ , zero/4*0/ , method/4H1 NT , 4H1 T  , 4H2 NT ,    &
       &4H2 T  , 4H3 T  , 3H4 T/
!WKBI 9/93
   DATA prntyp/2HRS , 2HRD , 2HCS , 2HCD/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!NVXNB
         IF ( typea==0 ) typea = iprec
         IF ( typeb==0 ) typeb = iprec
         IF ( typec==0 ) typec = iprec
!NVXNE
!WKBNB 7/94 SPR94008
         itypea = typea
         itypeb = typeb
         itypec = typec
!WKBNE 7/94 SPR94008
!
!     CHECK TO SEE IF THE INPUT MATRICES ARE CONFORMABLE
!
         CALL sswtch(19,l19)
         CALL sswtch(41,l41)
         nogo = 0
         file = 0
         noab = 0
         IF ( filea(6)==0 .OR. fileb(6)==0 ) noab = 1
         irowb = filea(2)
         irowc = filea(3)
         IF ( t/=0 ) t = 1
         IF ( t/=0 ) THEN
            irowb = filea(3)
            irowc = filea(2)
         ENDIF
         IF ( noab/=1 ) THEN
            IF ( fileb(3)/=irowb ) nogo = 1
            IF ( filec(1)>0 ) THEN
               IF ( filec(2)/=fileb(2) .OR. filec(3)/=irowc ) nogo = 1
            ENDIF
            IF ( nogo==1 ) THEN
               CALL fname(filea,Zz(1))
               CALL fname(fileb,Zz(3))
               CALL fname(filec,Zz(5))
               IF ( filec(2)/=fileb(2) .OR. filec(3)/=irowc ) nogo = 1
               WRITE (mout,99001) Zz(1) , Zz(2) , filea(2) , filea(3) , Zz(3) , Zz(4) , fileb(2) , fileb(3) , Zz(5) , Zz(6) ,       &
                                & filec(2) , irowc
99001          FORMAT (3(4X,2A4,2I7))
               mm = -55
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     PERFORM GENERAL INITIALIZATION
!
         mpy(3) = jbegn
         IF ( filed(1)>0 ) CALL conmsg(mpy,3,0)
         nout = lout
!
!  -- USE SINGLE PRECISION ON MACHINES WITH 60 OR 64 BITS PER WORD
!
         IF ( nbpw>=60 ) prec1 = 1
         opb = rdrew
         opc = rdrew
         op2 = wrtrew
         op = cls
         cfile = filec(1)
         IF ( cfile==0 ) typec = 1
         b(2) = 0.
         b(3) = 0.
         b(4) = 0.
         typd1 = typd
         one1 = 1
         one2 = 1
         p = n
         IF ( t/=0 ) p = m
         pp1 = p
         incr1 = 1
         IF ( cfile==0 .OR. filec(6)==0 ) cfile = 0
         IF ( fileb(6)==0 .AND. cfile==0 ) pp1 = 1
         incr2 = 1
         filed(2) = 0
         filed(6) = 0
         filed(7) = 0
         mpass3 = 0
         time3 = 1.0E+10
         prec = prec1
         IF ( prec/=2 ) prec = 1
         IF ( prec1==0 .AND. (prc(typea)==2 .OR. prc(typeb)==2 .OR. prc(typec)==2) ) prec = 2
!
!     ELIMINATE METHOD THREE FROM SELECTION FOR THIS BAD CASE
!     (I.E. TRANSPOSE AND MIXED MATRIX PRECISION)
!
         it = t
         IF ( it/=0 .AND. prec==1 .AND. prc(typeb)==2 ) it = 0
         IF ( it/=t .AND. l19/=0 ) WRITE (nout,99002) typea , typeb , typec
99002    FORMAT ('0METHOD 3T IS ELIMINATED FROM SELECTION/MPYAD@60',/1X,'MATRIX TYPES A,B,C =',3I3)
!
!     COMPUTE TYPE AND PRECISION OF D MATRIX
!     RCD    = 1 FOR REAL,   2 FOR COMPLEX
!     PREC   = 1 FOR SINGLE, 2 FOR DOUBLE
!     TYPED  = 1 FOR RSP, 2 FOR RDP, 3 FOR CSP, AND 4 FOR CDP
!     PRC(1) = 1 FOR S.P.   PRC(2) = 2 FOR D.P.
!
         rcd = 0
         IF ( prec/=2 ) THEN
            IF ( andf(typea,1)==0 ) typea = typea - 1
            IF ( andf(typeb,1)==0 ) typeb = typeb - 1
            IF ( andf(typec,1)==0 ) typec = typec - 1
         ENDIF
         IF ( typea>2 .OR. typeb>2 .OR. typec>2 ) rcd = 2
         typed = rcd + prec
         IF ( rcd==0 ) rcd = 1
!
!     RCA/B/D   = 1 IF A/B/D IS REAL, = 2 IF A/B/D IS COMPLEX
!     NWDA/B/D  = NUMBER OF WORDS PER ELEMENT OF A/B/D
!     NBX/DX    = NUMBER OF ELEMENTS PER COLUMN OF B/C ORD
!     NB/D      = NUMBER OF WORDS PER COLUMN OF B/C OR D
!     NZZ       = BUF1 = POINTER TO FIRST GINO BUFFER
!     BUF2/3    = POINTER TO SECOND AND THIRD GINO BUFFERS
!     JJ        = MAX. NO. OF COLNS OF B AND D THAT MAY BE HELD IN CORE
!     MPASS1/2/3 = NUMBER OF PASSES REQUIRED FOR METHOD ONE/TWO/THREE
!     JZB/JZDB  = POINTER TO FIRST ELEMENT OF B FOR SP/DP REFERENCE
!     JB        = POINTER TO FIRST ELEMENT OF B FOR PRECISION OF PROBLEM
!     ACORE     = POINTER TO FIRST WORD FOR STORAGE OF PACKED COLUMNS
!                 OF A MATRIX FOR METHOD TWO
!     KSYS58    = SYSTEM(58), METHOD REQUESTED BY USER IF IT IS NON-ZERO
!
!
!     TURN TRANSPOSE FLAG OFF IF INPUT MATRIX A IS SYMMETRIC, AND SURELY
!     THAT COLUMNS EQUEL ROWS, AND DIAG 41 IS OFF.
!
!     IF INPUT A OR B IS DIAGONAL, ROW VECTOR, OR IDENTITY MATRICES,
!     MATRICES ARE NOT IN MIXED PRECISTION TYPES, AND DIAG 41 FLAG IS
!     OFF AND SYSTEM(94) IS NOT 1, BRANCH OFF TO SPECIAL SUBROUTINE
!     MPY-D-R-I
!
         k = filea(4)
         IF ( k==6 .AND. m==n .AND. l41==0 ) t = 0
!        SYMMETRIC    COLN=ROW     DIAG41 OFF
         IF ( l41/=1 .AND. mod(ksystm(94),10)/=1 ) THEN
            j = fileb(4)
            IF ( k==3 .OR. k==7 .OR. k==8 .OR. j==3 .OR. j==7 .OR. j==8 ) THEN
!         DIAGONAL     ROW VCTR     IDENTITY
!
               IF ( typea==typeb .AND. typea==typd ) THEN
                  k = max0(m,n,q,r)
                  j = k*2 + 1
                  k = k + 1
                  CALL mpydri(Z,Z,Z(k),Z(k),Z(j),Z(j))
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
!
         rcb = rc(typeb)
         nbx = r*rcb
         nwdb = nwds(typeb)
         nwdb1 = nwdb + 1
         nb = r*nwdb
         ndx = p*rcd
         nd = p*nwds(typed)
         nzz = iabs(nz) - sysbuf + 1
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf
         buf4 = buf3 - sysbuf
         jj = (nzz-1)/(nb+nd)
         icrq = nb + nd - nzz + 1
         IF ( icrq>0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         mpass1 = (q-1)/jj + 1
         jzb = jj*nd + 1
         jzdb = jj*ndx + 1
         jb = jzb
         IF ( prc(typeb)==2 ) jb = jzdb
         nwda = nwds(typea)
         prca = prc(typea)
         na = nwda*n
         nwda1 = nwda + 1
         nwdd = nwds(typed)
         acore = nd + 1
         IF ( t/=0 ) acore = nb + 1
         acore = ((acore+1)/2)*2 + 1
         IF ( signab==0 .AND. prec==1 .AND. (prc(typea)==2 .OR. prc(typeb)==2) ) typed = rcd + 1
         IF ( noab==1 .OR. signab==0 ) THEN
!
!     A MATRIX OR B MATRIX IS NULL - COPY C MATRIX TO D MATRIX
!
            time = 0.0
            IF ( filed(1)<0 ) RETURN
            IF ( q<=0 ) q = filec(2)
            filed(2) = 0
            filed(6) = 0
            filed(7) = 0
            WRITE (nout,99003)
99003       FORMAT ('             MPYAD - NULL MATRIX PRODUCT')
            CALL gopen(filed,Z(buf1),wrtrew)
            IF ( cfile==0 ) THEN
!
!     PACK NULL COLUMNS BECAUSE C MATRIX IS NULL
!
               pp1 = 1
               DO acol = 1 , q
                  CALL pack(zero,filed,filed)
               ENDDO
            ELSEIF ( typec==signc*typd ) THEN
!
!     USE CPYSTR TO COPY C TO D
!
               block(1) = cfile
               blk(1) = filed(1)
               CALL gopen(cfile,Z(buf2),rdrew)
               DO ii = 1 , q
                  CALL cpystr(block,blk,0,0)
               ENDDO
               CALL close(cfile,clsrew)
               filed(2) = q
               filed(5) = filec(5)
               filed(6) = filec(6)
               filed(7) = filec(7)
            ELSE
!
!     USE INTPK/BLDPK TO COPY C TO D BECAUSE TYPES CONFLICT
!
               CALL gopen(cfile,Z(buf2),rdrew)
               DO ii = 1 , q
                  CALL bldpk(typd,typd,filed,block,1)
                  itypsg = signc*typd
                  CALL intpk(*2,filec,0,itypsg,0)
                  SPAG_Loop_2_1: DO
                     CALL zntpki
                     CALL bldpki(a,ip,filed,block)
                     IF ( eol/=0 ) EXIT SPAG_Loop_2_1
                  ENDDO SPAG_Loop_2_1
 2                CALL bldpkn(filed,block,filed)
               ENDDO
               CALL close(cfile,clsrew)
            ENDIF
            IF ( filec(1)>0 ) filed(4) = filec(4)
            CALL close(filed,clsrew)
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( signab==1 .OR. signab==-1 ) THEN
            CALL mpyq(Z)
!
!     CALCULATE ESTIMATED EXECUTION TIMES AND SELECT METHOD.
!
            ncore = buf3 - acore
            icrq = -ncore
            IF ( icrq>0 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            core = float(ncore/nwda)
            fn = filea(2)
            fm = filea(3)
            fp = fileb(2)
            rhoa = amin1(1.E-4*float(filea(7)),1.0)
            rhob = amin1(1.E-4*float(fileb(7)),1.0)
            rhoc = amin1(1.E-4*float(filec(7)),1.0)
            rhod = amax1(rhoa,rhob)
            arith = fm*fn*(tmt(typed)+(1.0-rhoa)*tml(typed))
            aterm = (fm*rhoa+5.0)*fn*tmipak
            bterm = float(r)*fp*0.5*(1.0+rhob)*tmupak
            dterm = fm*fp*0.5*(1.0+rhod)*tmpak
            cterm = 0
            IF ( cfile/=0 ) cterm = fm*fp*0.5*(1.0+rhoc)*tmupak
            time1 = (fm*fn*fp*rhoa*tmt(typed)+float(mpass1)*aterm+bterm+dterm+cterm)*1.0E-6
!
            mpass2 = (2.0-rhoa)*fm*fn*rhoa/core + 1.0
            fr = mpass2
            IF ( t/=0 ) THEN
!
               fnt = fn*fm*rhob
               p1 = amin1((fnt/float(fileb(6))+fp)/2.0,fnt,fp)
               fp1 = p1
               cterm2 = 0.
               IF ( cfile/=0 ) cterm2 = (fn*rhoc+5.0)*fp*tmipak
               bterm = fm*fp*0.5*(1.0+rhob)*tmupak
               dterm2 = (fn*rhod+5.0)*fp
               time2 = (fp1*rhoa*arith+(fm*rhoa+5.0)*fn*tmipak+fr*bterm+(fr+1.0)/2.0*dterm2*tmbpak+(fr-1.0)/2.0*dterm2*tmipak+      &
                     & cterm2)*1.0E-6
!
               bufi = buf4
               IF ( filec(1)==0 ) bufi = buf3
               nbrrow = min0((bufi-orf(nd+1,1))/na,m)
               mpass3 = (m-1)/nbrrow + 1
               fr = mpass3
               time3 = (fm*fn*fp*rhob*tmt(typed)+fm*fn*0.5*(1.0+rhoa)*tmupak+fr*fp*(fn*rhob+5.0)*tmipak+(fr+1.0)                    &
                     & /4.0*fn*fp*(1.0+rhod)*tmpak+(fr-1.0)/4.0*fn*fp*(1.0+rhod)*tmupak+cterm2)*1.E-6
            ELSE
               time2 = (fp*rhoa*rhob*arith+aterm+(fr+1.0)/2.0*(fn*rhob+10.0)*fp*tmipak+fr*dterm+(fr-1.0)*0.5*fm*fp*(1.0+rhod)       &
                     & *tmupak+cterm)*1.0E-6
            ENDIF
            CALL tmtogo(itimgo)
            IF ( core<=0.0 ) time2 = amax1(time1,time3) + 1.0
            time = amin1(time1,time2,time3)
            itime = time + 1
            IF ( itimgo<=itime .AND. filed(1)>0 ) THEN
               mm = -50
               file = itime
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     PRINT TIMING MESSAGE AND IF OUTPUT FILE IS PURGED RETURN
!
               ielems = fn*fm*rhoa + 0.5
               jelems = float(r)*fp*rhob
!WKBNB 9/93
               IF ( l19/=0 ) THEN
                  CALL fname(filea,namea)
                  CALL fname(fileb,nameb)
                  CALL fname(filec,namec)
                  CALL fname(filed,named)
!WKBR 7/94/SPR 94008 *         NAMEA, N, M, IELEMS, RHOA, PRNTYP( TYPEA )
!KWBR 7/94 SPR 94008 *,        NAMEB, R, Q, JELEMS, RHOB, PRNTYP( TYPEB )
                  WRITE (nout,99004,IOSTAT=ierr) namea , n , m , ielems , rhoa , prntyp(itypea) , nameb , r , q , jelems , rhob ,   &
                       & prntyp(itypeb)
99004             FORMAT ('  /-----------------------------------------------------------/',/,                                      &
                         &'  /     MATRIX      ROWS   COLS     TERMS  DENS    TYPE       /',/,                                      &
                         &'  /-----------------------------------------------------------/',/,'  /  A- ',2A4,I8,I7,I10,F7.4,5X,A2,/,&
                         &'  /  B- ',2A4,I8,I7,I10,F7.4,5X,A2)
                  ielems = fn*fm*rhoc + .5
                  IF ( cfile/=0 ) THEN
                     WRITE (nout,99005,IOSTAT=ierr) namec , filec(3) , filec(2) , ielems , rhoc , prntyp(itypec)
99005                FORMAT ('  /  C- ',2A4,I8,I7,I10,F7.4,5X,A2)
                  ENDIF
                  WRITE (nout,99006) named , prntyp(typed)
99006             FORMAT ('  /  D- ',2A4,8X,7X,10X,7X,5X,A2)
                  WRITE (nout,99007) signab , signc , t , core , mpass1 , mpass2 , mpass3 , time1 , time2 , time3
99007             FORMAT ('  /  SIGNAB =',I4,'  SIGNC =',I4,'  TIME EST=',I9,' MEMORY =',F8.0,/,'  /  MPASS1 =',I4,'  MPASS2=',I4,  &
                         &'  MPASS3=',I4,/,'  /  TIME1  =',E9.2,' TIME2=',E9.2,' TIME3=',E9.2,/,                                    &
                         &'  /-----------------------------------------------------------/')
               ENDIF
!WKBNE 9/93
!
               IF ( filed(1)<0 ) RETURN
!
               j = ksys58
               IF ( j<0 .OR. j>3 .OR. (j==3 .AND. it==0) ) j = 0
               IF ( j/=0 ) THEN
                  IF ( j==1 ) GOTO 5
                  IF ( j==2 ) THEN
                     spag_nextblock_1 = 9
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( j==3 ) THEN
                     spag_nextblock_1 = 20
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
               IF ( it/=0 ) THEN
!WKBD 2/95 NCL93004 190 IF (TIME1.LT.TIME2 .AND. TIME1.LT.TIME3) GO TO 200
!WKBNB 2/95 NCL93004
                  IF ( .NOT.(mpass1<mpass2 .AND. (time1<time3 .OR. mpass1<mpass3)) ) THEN
                     IF ( .NOT.(mpass2<mpass1 .AND. (time2<time3 .OR. mpass2<mpass3)) ) THEN
                        IF ( time1>=time2 .OR. time1>=time3 ) THEN
!WKBNE 2/95 NCL93004
                           IF ( time2<time3 ) THEN
                              spag_nextblock_1 = 9
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           spag_nextblock_1 = 20
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDIF
                  ENDIF
!WKBNB 2/95 NCL93004
               ELSEIF ( mpass1>=mpass2 ) THEN
                  IF ( mpass2<mpass1 ) THEN
                     spag_nextblock_1 = 9
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
!WKBNE 2/95 NCL93004
                  IF ( time1>=time2 ) THEN
                     spag_nextblock_1 = 9
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
!
!               *********************
!               *                   *
!               *    METHOD  ONE    *
!               *    MPY1NT $ 1T    *
!               *                   *
!               *********************
!
!     BUILD MATRIX PRODUCT JMAX COLUMNS PER PASS OF A MATRIX
!     WHERE JMAX=JJ EXCEPT ON FINAL PASS
!
 5             jcol = 1
               WRITE (nout,99009) method(t+1) , mpass1 , time1
            ENDIF
         ELSE
            WRITE (mout,99008)
99008       FORMAT ('0*** USER FATAL MESSAGE 2398, MPYAD REQUIRES SIGN OF ','A*B TO BE -1, 0, OR +1')
            mm = -37
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         jmax = min0(jcol+jj-1,q)
         IF ( jmax==q ) op = clsrew
         jmax1 = jmax - jcol
         jmax = jmax1 + 1
         jmax1x = jmax1*ndx
         IF ( fileb(6)/=0 ) THEN
!
!     READ AND UNPACK JMAX COLUMNS OF THE B MATRIX
!
            file = fileb(1)
            jz = jzb
            typebd = typeb*signab
            nbd = nb
            opbc = opb
            pp2 = r
            ASSIGN 20 TO mm
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     READ AND UNPACK JMAX COLUMNS OF THE C MATRIX
!
 20      file = filec(1)
         jz = 1
         typebd = typed*signc
         nbd = nd
         opbc = opc
         pp2 = p
         ASSIGN 40 TO mm
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
!     OPEN AND POSITION A MATRIX TO FIRST COLUMN
!
 40      IF ( fileb(6)==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = filea(1)
         CALL open(*80,filea,Z(nzz),rdrew)
         CALL fwdrec(*100,filea)
!
!     SET POINTERS
!     L   = COLUMN NUMBER
!     LL  = POINTER TO LTH ROW OF B MATRIX
!     LLL = POINTER TO LTH ROW OF D MATRIX
!
         l = 1
         ll = jb
         lll = 1
         spag_nextblock_1 = 3
      CASE (3)
!
!     CALL INTPK TO INITIATE READING THE LTH COLUMN OF THE A MATRIX
!     IF COLUMN IS NULL, BYPASS ARITHMETIC
!
         CALL intpk(*60,filea,0,typed,0)
!
!     FORM EITHER  A(I,L)*B(L,J) + D(I,J)
!              OR  A(L,I)*B(I,J) + D(L,J)
!           WHERE  J RUNS ACROSS COLUMNS OF B AND D NOW IN CORE
!
         CALL mpy1v(Zz,Z,Zd)
!
!     POSITION POINTERS FOR NEXT COLUMN OF A
!
 60      ll = ll + rcb
         lll = lll + rcd
         l = l + 1
         IF ( l<=m ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CLOSE AND REWIND FILE CONTAINING A MATRIX
!
         CALL close(filea,clsrew)
         spag_nextblock_1 = 4
      CASE (4)
!
!     OPEN FILE CONTAINING D MATRIX TO WRITE
!
         file = filed(1)
         CALL open(*80,filed,Z(nzz),op2)
!
!     IF FIRST COLUMNS OF D, WRITE HEADER
!
         IF ( op2/=wrt ) THEN
            CALL fname(filed,bcd)
            CALL write(filed,bcd,2,1)
         ENDIF
!
!     PACK AND WRITE JMAX COLUMNS OF THE D MATRIX
!
         jz = 1
         DO j = 1 , jmax
            CALL pack(Z(jz),filed,filed)
            jz = jz + nd
         ENDDO
!
!     TEST FOR END OF MULTIPLICATION
!     CLOSE FILE CONTAINING D MATRIX
!
         CALL close(filed,op)
!
!     SET OP FLAGS FOR OPEN CALLS FOR NEXT PASS
!
         opb = rd
         opc = rd
         op2 = wrt
!
         jcol = jcol + jj
         IF ( jcol<=q ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         mpy(3) = jend
         CALL conmsg(mpy,3,0)
         RETURN
      CASE (6)
!
!     INTERNAL SUBROUTINE TO READ JMAX COLUMNS OF THE B OR C MATRICES
!     ELEMENTS ARE SET TO ZERO IF COLUMN IS NULL OR MATRIX ABSENT
!
!     OPEN AND POSITION FILE IF MATRIX IS PRESENT
!
         IF ( file/=0 ) THEN
            CALL open(*80,file,Z(nzz),opbc)
            IF ( jcol==1 ) CALL fwdrec(*100,file)
         ENDIF
!
!     LOOP THROUGH JMAX COLUMNS OF MATRIX
!
         DO j = 1 , jmax
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
!
!     UNPACK THE JTH COLUMN IF MATRIX IS PRESENT
!
                  IF ( file/=0 ) THEN
                     CALL unpack(*62,file,Z(jz))
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
!
!     ZERO COLUMN
!
 62               k2 = jz + nbd - 1
                  DO k = jz , k2
                     Z(k) = 0.
                  ENDDO
                  spag_nextblock_2 = 2
               CASE (2)
!
!     POSITION POINTERS TO NEXT COLUMN OF MATRIX
!
                  jz = jz + nbd
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
!
!     CLOSE FILE IF MATRIX IS PRESENT
!
         IF ( file/=0 ) CALL close(file,op)
!
!     RETURN
!
         GOTO mm
!
!
!     ERROR CONDITIONS
!
 80      mm = -1
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 100     mm = -2
         spag_nextblock_1 = 7
      CASE (7)
         CALL mesage(mm,file,name)
         RETURN
      CASE (8)
!
         mm = -8
         file = icrq
         spag_nextblock_1 = 7
      CASE (9)
!
!
!               *********************
!               *                   *
!               *    METHOD  TWO    *
!               *    MPY2NT $ 2T    *
!               *    AND   MPY4T    *
!               *                   *
!               *********************
!
!
!     INITIALIZE FOR METHODS 2NT, 2T AND 4T.
!     METHOD 4T DOES NOT HANDLE COMPLEX MATRIX-D FROM REAL MATRICES A
!     AND B (LL4 = 6).
!
         mt4 = 0
         mt2 = t
         IF ( mod(ksystm(94),100)/10/=1 ) THEN
            IF ( t/=0 .AND. l41/=1 .AND. ll4/=6 ) THEN
               IF ( mpass2>2 .AND. cfile/=0 .AND. densc>=700 ) THEN
                  mt2 = 0
                  mt4 = 2
                  acore = nb + nd + 1
                  acore = ((acore+1)/2)*2 + 1
                  jzb = nd + 1
                  jb = nd/prec1 + 1
               ENDIF
            ENDIF
         ENDIF
         dfile = filed(1)
         efile = scrtch
         block(1) = filea(1)
         cfile = filec(1)
         opa = rdrew
         typec = typed*signc
         firstl = buf3 - 1
         WRITE (nout,99009) method(t+3+mt4) , mpass2 , time2
!
!     BEGIN PASS
!
!     OPEN DFILE TO WRITE.
!     READ AS MANY COLUMNS (OR ROWS) OF A AS CAN BE HELD
!     IN CORE IN PACKED FORM ON THIS PASS.
!
         acol1 = 1
         spag_nextblock_1 = 10
      CASE (10)
         file = dfile
         CALL open(*80,dfile,Z(buf3),wrtrew)
         CALL fname(filed(1),bcd)
         CALL write(dfile,bcd,2,1)
         filed(2) = 0
         filed(6) = 0
         filed(7) = 0
         file = filea(1)
         CALL gopen(filea,Z(buf2),opa)
         apoint = acore
         l = firstl
         acol = acol1
         spag_nextblock_1 = 11
      CASE (11)
!WKBR 9/94   660 IF ( (APOINT+NA+2) .GE. L-2) GO TO 530
! ABOVE CHECK WAS OVER-ZEALOUS IN CHECKING FOR AVAILABLE MEMORY
! BECAUSE OF THE CHECK TWO LINES AFTER STATEMENT 670
         IF ( (apoint+2)>=l-2 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Zz(l) = 0
         Zz(l-1) = 0
         block(8) = -1
         CALL getstr(*120,block)
         incra = 1
         IF ( prc(type)==2 .AND. prc(typea)==1 ) incra = 2
         Zz(l) = apoint
         DO
            kr1 = apoint + 2
            krn = kr1 + nbrstr*nwda - 1
            IF ( krn>=l-2 ) THEN
!
!     ALL COLUMNS OF A WILL NOT FIT ON THIS PASS.
!
               CALL bckrec(filea(1))
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     MOVE STRING FROM BUFFER TO CORE AND COMPLETE STRING DEFINITION
!     WORDS
!
               IF ( prc(type)/=2 .OR. prc(typea)/=1 ) THEN
                  IF ( prc(type)==2 ) point = point*2 - 1
                  DO ii = kr1 , krn
                     Z(ii) = xns(point)
                     point = point + incra
                  ENDDO
               ELSE
!
!  -- THIS CODE NECESSARY FOR UNIVAC DOUBLE PRECISION TO SINGLE PRC.
!
                  inc = 1
                  incra = 1
                  IF ( type==4 ) inc = 2
                  krn = kr1 + nbrstr*inc - 1
                  DO ii = kr1 , krn
                     Z(ii) = xnd(point)
                     point = point + incra
                  ENDDO
               ENDIF
               Zz(apoint) = row
               Zz(apoint+1) = nbrstr
               Zz(l-1) = Zz(l-1) + 1
               apoint = krn + 1
!
!     GET NEXT STRING DEFINITION
!
               CALL endget(block)
               CALL getstr(*120,block)
            ENDIF
         ENDDO
!
!     END-OF-COLUMN -
!     SAVE LAST NON-ZERO TERM POSTION FOR MTHOD 4T, THEN
!     TEST FOR ALL COLUMNS
!
!     SINCE GINO IS MDS, MAKE SURE THAT THE LAST VALUES IN NBRSTR AND
!     ROW HERE ARE STILL VALID. OTHERWISE THEY MUST BE SAVED FIRST (AT
!     710) AND USED ON NEXT LINE.
!
 120     IF ( mt4==2 ) Zz(l-1) = orf(Zz(l-1),lshift(row+nbrstr-1,ihalf))
!                                    NBR + LAST NON-ZERO TERM COLUMN NO.
!
         l = l - 2
         acol = acol + 1
         IF ( acol<=m ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ALL COLUMNS OF A ARE IN - THIS IS THE LAST PASS
!
         acoln = m
         CALL close(filea(1),clsrew)
         spag_nextblock_1 = 13
      CASE (12)
         CALL close(filea(1),cls)
         acoln = acol - 1
         spag_nextblock_1 = 13
      CASE (13)
!
!     IF CFILE IS PRESENT, OPEN IT.
!     IF THIS IS THE FIRST PASS, SKIP HEADER RECORD.
!     OPEN BFILE AND SKIP HEADER RECORD.
!     INITIALIZE COLUMN (OR ORW) COUNTER, BCOL, TO 1, AND BRANCH ON T.
!
         IF ( cfile/=0 ) THEN
            file = cfile
            CALL open(*80,cfile,Z(buf1),rdrew)
            CALL fwdrec(*100,cfile)
         ENDIF
         file = fileb(1)
         CALL open(*80,fileb(1),Z(buf2),rdrew)
         CALL fwdrec(*100,fileb(1))
         bcol = 1
         spag_nextblock_1 = 14
      CASE (14)
         IF ( mt2==1 ) THEN
!
!     TRANSPOSE CASE, METHOD 2T
!     =========================
!
!     INITIATE BUILDING OF A PACKED COLUMN OF D.
!     UNPACK A COLUMN OF B IN CORE. IF NULL, COPY COLUMN FROM C TO D.
!     INITIATE INTERPRETATION OF A COLUMN OF C.
!
            CALL bldpk(typed,typd,dfile,0,0)
            typebd = typeb*signab
            pp2 = r
            CALL unpack(*180,fileb(1),Z)
            eol = 1
            crow = 16777215
!            16777215 = 2**24 - 1
!
            IF ( cfile/=0 ) THEN
               CALL intpk(*200,cfile,0,typec,0)
               crow = 0
            ENDIF
            GOTO 200
!
!     UNPACK A COLUMN OF C.
!
         ELSEIF ( cfile/=0 ) THEN
            typebd = typec
            IF ( mt4/=0 ) one2 = 1
            pp2 = p
            CALL unpack(*140,cfile,Z)
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 140     DO ii = 1 , nd
            Z(ii) = 0.
         ENDDO
         spag_nextblock_1 = 15
      CASE (15)
         IF ( mt4/=0 ) THEN
!
!     TRNASPOSE CASE, METHOD 4T
!     =========================
!
!     UNPACK A BANDED COLUMN OF MATRIX B, RANGING FROM ONE2 THRU PP2.
!     FOR THE RANGE  MAX0(ONE2,ACOL1) THRU MIN0(PP2,ACOLN), FORM ALL
!     PRODUCTS
!     D(I,K) = A(I,J)*B(J,K) + C(I,K)
!
            typebd = typeb*signab
            one2 = 0
            CALL unpack(*160,fileb,Z(jzb))
!
!     WE HAVE HERE -
!     ACLO1, ACOLN = COLUMNS OF MATRIX A IN CORE
!     BCOL = CURRENTLY WE ARE WORKING ON THE BCOL COLUMN OF MATRIX B,
!            WHICH IS ALSO THE WORKING COLUMNS OF MATRIX D AND MATRIX C
!     Z(JZB) THRU Z(ACORE-1) CONTAIN THE BCOL COLUMN OF MATRIX B
!
            CALL mpy4t(Z,Z,Z)
         ELSE
!
!     NON-TRANSPOSE CASE, METHOD 2NT
!     ==============================
!
!     INITIATE INTERPRETATION OF A COLUMN OF B.
!
!     ITYPSG = TYPED*SIGNAB
            itypsg = typeb*signab
            CALL intpk(*160,fileb(1),0,itypsg,0)
!
!     FOR EACH NON-ZERO ELEMENT B(I) IN THE CURRENT COLMN OF B SUCH
!     THAT FOR I.GE.ACOL1 .AND I.LE.ACOLN, FORM ALL PRODUCTS OF
!     D(K,I) = A(K,I)*B(I) + C(K,I)
!
            CALL mpy2nv(Zz,Z,Zd)
         ENDIF
!
!     PACK CURRENT COLUMN ONTO DFILE FOR BOTH 2NT AND 4T METHOD, AND
!     GO TO TEST FOR END OF PASS.
!
 160     CALL pack(Z,dfile,filed)
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
 180     IF ( cfile==0 ) GOTO 220
         CALL intpk(*220,cfile,0,typec,0)
         spag_nextblock_1 = 16
      CASE (16)
         CALL zntpki
         crow = ip
         spag_nextblock_1 = 17
      CASE (17)
         DO ii = 1 , nwdd
            d(ii) = a(ii)
         ENDDO
         drow = crow
         CALL zblpki
         spag_nextblock_1 = 18
      CASE (18)
         IF ( eol==0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO 220
!
!     FOR ALL NON-NULL ROWS OF A IN CORE, FORM A(I,J)*B(J) + C(I)
!
 200     CALL mpy2tv(Zz,Z,Zd)
         IF ( arown/=m .AND. crow/=16777215 ) THEN
            IF ( crow>arown ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     TERMINATE CURRENT COLUMN OF D.
!
 220     CALL bldpkn(dfile,0,filed)
         spag_nextblock_1 = 19
      CASE (19)
!
!     BOTH TRANSPOSE (2T AND 4T) AND NON-TRANSPOSE (2NT) CASES
!
!     TEST FOR COMPLETION OF PASS. IF COMPLETE, TEST ALL PASSES.
!
         bcol = bcol + 1
         IF ( bcol<=q ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL close(fileb,clsrew)
         IF ( cfile/=0 ) CALL close(cfile,clsrew)
         CALL close(dfile,clsrew)
         IF ( acoln==m ) THEN
!
!     LAST PASS -
!     MAKE SURE D MATRIX IS ON PROPER FILE.
!     IF NOT, SWITCH FIST AND FIAT UNIT NBRS IN /XFIAT/
!
            IF ( dfile/=filed(1) ) CALL filswi(dfile,filed)
            spag_nextblock_1 = 5
         ELSE
!
!     NOT LAST PASS - SWITCH C AND D FILES AND CONTINUE
!
            opa = rd
            typec = typed
            IF ( acol1==1 ) THEN
               cfile = dfile
               dfile = efile
            ELSE
               k = cfile
               cfile = dfile
               dfile = k
            ENDIF
            acol1 = acoln + 1
            spag_nextblock_1 = 10
         ENDIF
      CASE (20)
!
!
!               *********************
!               *                   *
!               *    METHOD THREE   *
!               *       MPY3T       *
!               *                   *
!               *********************
!
!     TRANSPOSE CASE ONLY, METHOD 3T
!     ==============================
!
         WRITE (nout,99009) method(5) , mpass3 , time3
         block(1) = fileb(1)
         acore = orf(nd+1,1)
         cfile = scrtch
         dfile = filed(1)
         IF ( mod(mpass3,2)==0 ) THEN
            cfile = filed(1)
            dfile = scrtch
         ENDIF
         arow1 = 1
         last = .FALSE.
         opa = rdrew
         DO
!
!     BEGIN PASS BY FILLING CORE WITH UNPACKED COLUMNS OF A
!
            arown = min0(arow1+nbrrow-1,m)
            IF ( arown==m ) last = .TRUE.
            CALL gopen(filea,Z(buf1),opa)
            typebd = typea*signab
            pp2 = n
            apoint = acore
            DO arow = arow1 , arown
               spag_nextblock_3 = 1
               SPAG_DispatchLoop_3: DO
                  SELECT CASE (spag_nextblock_3)
                  CASE (1)
                     CALL unpack(*222,filea,Z(apoint))
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
 222                 k2 = apoint + na - 1
                     DO ii = apoint , k2
                        Z(ii) = 0.
                     ENDDO
                     spag_nextblock_3 = 2
                  CASE (2)
                     apoint = apoint + na
                     EXIT SPAG_DispatchLoop_3
                  END SELECT
               ENDDO SPAG_DispatchLoop_3
            ENDDO
            ii = cls
            IF ( last ) ii = clsrew
            CALL close(filea,ii)
            incra = (arown-arow1)*na
!
!     PREPARE TO PASS B MATRIX AND C MATRIX FROM LAST PASS
!
            IF ( arow1/=1 ) CALL gopen(cfile,Z(buf2),rdrew)
            CALL gopen(dfile,Z(buf3),wrtrew)
            CALL gopen(fileb,Z(buf1),rdrew)
            IF ( last .AND. filec(1)/=0 ) CALL gopen(filec,Z(buf4),rdrew)
            filed(2) = 0
            filed(6) = 0
            filed(7) = 0
            typebd = typed
            pp2 = arown
            k2 = arown*nwdd
!
            DO bcol = 1 , q
               spag_nextblock_4 = 1
               SPAG_DispatchLoop_4: DO
                  SELECT CASE (spag_nextblock_4)
                  CASE (1)
                     IF ( arow1/=1 ) THEN
!
!     INTERMEDIATE PASS OR LAST PASS - UNPACK COLUMN FROM PREVIOUS PASS
!
                        CALL unpack(*224,cfile,Z)
                        null = .FALSE.
                        IF ( .NOT.(last) ) GOTO 226
                        spag_nextblock_4 = 2
                        CYCLE SPAG_DispatchLoop_4
                     ENDIF
!
!     FIRST PASS OR NULL COLUMN ON CFILE - SET COLUMN OF D TO ZERO
!
 224                 DO ii = 1 , k2
                        Z(ii) = 0.
                     ENDDO
                     null = .TRUE.
                     IF ( .NOT.(last) ) GOTO 226
                     spag_nextblock_4 = 2
                  CASE (2)
!
!     LAST PASS - ADD COLUMN FROM C MATRIX (IF PRESENT)
!
                     IF ( filec(1)/=0 ) THEN
                        itypsg = typed*signc
                        CALL intpk(*226,filec,0,itypsg,0)
                        null = .FALSE.
                        SPAG_Loop_3_2: DO
                           CALL zntpki
                           IF ( typed==2 ) THEN
                              Zd(ip) = Zd(ip) + ad(1)
                           ELSEIF ( typed==3 ) THEN
                              Z(2*ip-1) = Z(2*ip-1) + a(1)
                              Z(2*ip) = Z(2*ip) + a(2)
                           ELSEIF ( typed==4 ) THEN
                              Zd(2*ip-1) = Zd(2*ip-1) + ad(1)
                              Zd(2*ip) = Zd(2*ip) + ad(2)
                           ELSE
                              Z(ip) = Z(ip) + a(1)
                           ENDIF
                           IF ( eol/=0 ) EXIT SPAG_Loop_3_2
                        ENDDO SPAG_Loop_3_2
                     ENDIF
!
!     FOR EACH NON-ZERO TERM B(J) IN THE CURRENT COLUMN OF B FORM
!     D(I,K) = D(I,K) + A(I,J)*B(J,K)
!
 226                 CALL mpy3t(*228,Z(acore),Z(acore),Z(1),Z(1))
                     spag_nextblock_4 = 3
                     CYCLE SPAG_DispatchLoop_4
!
!     PACK NULL COLUMN
!
 228                 IF ( null ) THEN
                        pp1 = 1
                        CALL pack(zero,dfile,filed)
                        CYCLE
                     ENDIF
                     spag_nextblock_4 = 3
                  CASE (3)
!
!     PACK NON-NULL COLUMN
!
                     pp1 = arown
                     CALL pack(Z,dfile,filed)
                     EXIT SPAG_DispatchLoop_4
                  END SELECT
               ENDDO SPAG_DispatchLoop_4
!
!     TEST FOR END OF CURRENT PASS
!
            ENDDO
!
            IF ( arow1/=1 ) CALL close(cfile,clsrew)
            CALL close(dfile,clsrew)
            CALL close(fileb,clsrew)
            IF ( last ) THEN
!
!     LAST PASS - SIGNAL END AND RETURN
!
               IF ( filec(1)/=0 ) CALL close(filec,clsrew)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     NOT LAST PASS - SWITCH FILES AND CONTINUE
!
               ii = cfile
               cfile = dfile
               dfile = ii
               arow1 = arown + 1
               opa = rd
            ENDIF
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99009 FORMAT ('    METHOD TO BE USED:',A4,', NBR PASSES =',I4,',  EST. TIME =',F9.1)
!
END SUBROUTINE mpyado
