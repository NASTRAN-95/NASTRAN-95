
 
SUBROUTINE mpyado(Zz,Z,Zd)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(4) , D(4) , Qt(2) , Time , Tmbpak , Tmgstr , Tmio , Tmipak , Tml(4) , Tmpak , Tmpstr , Tmt(4) , Tmupak , Xns(1)
   INTEGER Acol , Acol1 , Acoln , Acore , Apoint , Arow , Arow1 , Arown , Bcol , Block(20) , Cls , Clsrew , Crow , Densc , Drow ,   &
         & Eol , Eor , Filea(7) , Fileb(7) , Filec(7) , Filed(7) , Firstl , Flag , Form , Ihalf , Incr1 , Incr2 , Incra , Ip ,      &
         & Iprec , Jb , Jhalf , Jmax , Jmax1x , Jmp(2) , Ksys58 , Ksystm(152) , Ll , Ll4 , Lll , Lout , M , Mach , Mout , N , Na ,  &
         & Nb , Nbpw , Nbrstr , Nbx , Nd , Ndx , Nitems , Nwda , Nwdb , Nwdd , Nwds(4) , Nz , One1 , One2 , Point , Pp1 , Pp2 ,     &
         & Prc(2) , Prec , Prec1 , Q , R , Rc(4) , Rcb , Rcd , Rd , Rdrew , Row , Rowa , Scrtch , Signab , Signc , Sysbuf , T
   DOUBLE PRECISION Ad(2) , Dd(2) , Xnd(8500)
   INTEGER Typd , Typd1 , Type , Typea , Typeb , Typebd , Typec , Typed , Wrt , Wrtrew
   COMMON /logout/ Lout
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /mpyadx/ Filea , Fileb , Filec , Filed , Nz , T , Signab , Signc , Prec1 , Scrtch , Time
   COMMON /mpyadz/ Rcb , Rcd , Ll , Lll , Jb , Nbx , Ndx , Jmax1x , Acol , Acol1 , Acoln , Acore , Apoint , Bcol , Crow , Firstl ,  &
                 & Na , Nb , Nd , Nwda , Nwdb , Nwdd , Prec , Jmax , Incra , Block
   COMMON /mpyqt4/ Qt , Ll4 , Jmp
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /ntime / Nitems , Tmio , Tmbpak , Tmipak , Tmpak , Tmupak , Tmgstr , Tmpstr , Tmt , Tml
   COMMON /packx / Typed , Typd1 , One1 , Pp1 , Incr1
   COMMON /system/ Ksystm
   COMMON /type  / Prc , Nwds , Rc
   COMMON /unpakx/ Typebd , One2 , Pp2 , Incr2
   COMMON /zblpkx/ D , Drow
   COMMON /zntpkx/ A , Ip , Eol , Eor
   COMMON /zzzzzz/ Xnd
!
! Dummy argument declarations
!
   REAL Z(1)
   DOUBLE PRECISION Zd(1)
   INTEGER Zz(6)
!
! Local variable declarations
!
   INTEGER andf , lshift , orf
   REAL arith , aterm , b(4) , bterm , core , cterm , cterm2 , dterm , dterm2 , fm , fn , fnt , fp , fp1 , fr , rhoa , rhob , rhoc ,&
      & rhod , time1 , time2 , time3 , zero(4)
   INTEGER bcd(2) , blk(15) , buf1 , buf2 , buf3 , buf4 , bufi , cfile , dfile , efile , file , icrq , ielems , ierr , ii , inc ,   &
         & irowb , irowc , it , itime , itimgo , itypea , itypeb , itypec , itypsg , j , jbegn , jcol , jelems , jend , jj , jmax1 ,&
         & jz , jzb , jzdb , k , k2 , kr1 , krn , l , l19 , l41 , method(6) , mm , mpass1 , mpass2 , mpass3 , mpy(3) , mt2 , mt4 ,  &
         & name(2) , namea(2) , nameb(2) , namec(2) , named(2) , nbd , nbrrow , ncore , noab , nogo , nout , nwda1 , nwdb1 , nzz ,  &
         & op , op2 , opa , opb , opbc , opc , p , p1 , prca , prntyp(4)
   DOUBLE PRECISION bd(2)
   LOGICAL last , null
   EXTERNAL andf , lshift , orf
!
! End of declarations
!
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
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Mout) , (Ksystm(58),Ksys58) , (Ksystm(40),Nbpw) , (Ksystm(55),Iprec)
   EQUIVALENCE (A(1),Ad(1)) , (b(1),bd(1)) , (D(1),Dd(1)) , (Filea(2),M) , (Filea(3),N,Rowa) , (Filea(5),Typea) , (Fileb(2),Q) ,    &
    & (Fileb(3),R) , (Fileb(5),Typeb) , (Filec(5),Typec) , (Filed(5),Typd) , (nzz,buf1) , (Acoln,Arown) , (Filec(7),Densc)
   EQUIVALENCE (Block(2),Type) , (Block(3),Form) , (Block(4),Row) , (Block(5),Point) , (Block(6),Nbrstr) , (Block(8),Flag) ,        &
    & (Xnd(1),Xns(1)) , (Acol1,Arow1) , (Acol,Arow) , (mpy(1),name(1))
!
   DATA name/4HMPYA , 4HD   / , jbegn/4HBEGN/ , jend/3HEND/time1/0./ , time2/0./ , zero/4*0/ , method/4H1 NT , 4H1 T  , 4H2 NT ,    &
       &4H2 T  , 4H3 T  , 3H4 T/
!WKBI 9/93
   DATA prntyp/2HRS , 2HRD , 2HCS , 2HCD/
!NVXNB
   IF ( Typea==0 ) Typea = Iprec
   IF ( Typeb==0 ) Typeb = Iprec
   IF ( Typec==0 ) Typec = Iprec
!NVXNE
!WKBNB 7/94 SPR94008
   itypea = Typea
   itypeb = Typeb
   itypec = Typec
!WKBNE 7/94 SPR94008
!
!     CHECK TO SEE IF THE INPUT MATRICES ARE CONFORMABLE
!
   CALL sswtch(19,l19)
   CALL sswtch(41,l41)
   nogo = 0
   file = 0
   noab = 0
   IF ( Filea(6)==0 .OR. Fileb(6)==0 ) noab = 1
   irowb = Filea(2)
   irowc = Filea(3)
   IF ( T/=0 ) T = 1
   IF ( T/=0 ) THEN
      irowb = Filea(3)
      irowc = Filea(2)
   ENDIF
   IF ( noab/=1 ) THEN
      IF ( Fileb(3)/=irowb ) nogo = 1
      IF ( Filec(1)>0 ) THEN
         IF ( Filec(2)/=Fileb(2) .OR. Filec(3)/=irowc ) nogo = 1
      ENDIF
      IF ( nogo==1 ) THEN
         CALL fname(Filea,Zz(1))
         CALL fname(Fileb,Zz(3))
         CALL fname(Filec,Zz(5))
         IF ( Filec(2)/=Fileb(2) .OR. Filec(3)/=irowc ) nogo = 1
         WRITE (Mout,99001) Zz(1) , Zz(2) , Filea(2) , Filea(3) , Zz(3) , Zz(4) , Fileb(2) , Fileb(3) , Zz(5) , Zz(6) , Filec(2) ,  &
                          & irowc
99001    FORMAT (3(4X,2A4,2I7))
         mm = -55
         GOTO 1200
      ENDIF
   ENDIF
!
!     PERFORM GENERAL INITIALIZATION
!
   mpy(3) = jbegn
   IF ( Filed(1)>0 ) CALL conmsg(mpy,3,0)
   nout = Lout
!
!  -- USE SINGLE PRECISION ON MACHINES WITH 60 OR 64 BITS PER WORD
!
   IF ( Nbpw>=60 ) Prec1 = 1
   opb = Rdrew
   opc = Rdrew
   op2 = Wrtrew
   op = Cls
   cfile = Filec(1)
   IF ( cfile==0 ) Typec = 1
   b(2) = 0.
   b(3) = 0.
   b(4) = 0.
   Typd1 = Typd
   One1 = 1
   One2 = 1
   p = N
   IF ( T/=0 ) p = M
   Pp1 = p
   Incr1 = 1
   IF ( cfile==0 .OR. Filec(6)==0 ) cfile = 0
   IF ( Fileb(6)==0 .AND. cfile==0 ) Pp1 = 1
   Incr2 = 1
   Filed(2) = 0
   Filed(6) = 0
   Filed(7) = 0
   mpass3 = 0
   time3 = 1.0E+10
   Prec = Prec1
   IF ( Prec/=2 ) Prec = 1
   IF ( Prec1==0 .AND. (Prc(Typea)==2 .OR. Prc(Typeb)==2 .OR. Prc(Typec)==2) ) Prec = 2
!
!     ELIMINATE METHOD THREE FROM SELECTION FOR THIS BAD CASE
!     (I.E. TRANSPOSE AND MIXED MATRIX PRECISION)
!
   it = T
   IF ( it/=0 .AND. Prec==1 .AND. Prc(Typeb)==2 ) it = 0
   IF ( it/=T .AND. l19/=0 ) WRITE (nout,99002) Typea , Typeb , Typec
99002 FORMAT ('0METHOD 3T IS ELIMINATED FROM SELECTION/MPYAD@60',/1X,'MATRIX TYPES A,B,C =',3I3)
!
!     COMPUTE TYPE AND PRECISION OF D MATRIX
!     RCD    = 1 FOR REAL,   2 FOR COMPLEX
!     PREC   = 1 FOR SINGLE, 2 FOR DOUBLE
!     TYPED  = 1 FOR RSP, 2 FOR RDP, 3 FOR CSP, AND 4 FOR CDP
!     PRC(1) = 1 FOR S.P.   PRC(2) = 2 FOR D.P.
!
   Rcd = 0
   IF ( Prec/=2 ) THEN
      IF ( andf(Typea,1)==0 ) Typea = Typea - 1
      IF ( andf(Typeb,1)==0 ) Typeb = Typeb - 1
      IF ( andf(Typec,1)==0 ) Typec = Typec - 1
   ENDIF
   IF ( Typea>2 .OR. Typeb>2 .OR. Typec>2 ) Rcd = 2
   Typed = Rcd + Prec
   IF ( Rcd==0 ) Rcd = 1
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
   k = Filea(4)
   IF ( k==6 .AND. M==N .AND. l41==0 ) T = 0
!        SYMMETRIC    COLN=ROW     DIAG41 OFF
   IF ( l41/=1 .AND. mod(Ksystm(94),10)/=1 ) THEN
      j = Fileb(4)
      IF ( k==3 .OR. k==7 .OR. k==8 .OR. j==3 .OR. j==7 .OR. j==8 ) THEN
!         DIAGONAL     ROW VCTR     IDENTITY
!
         IF ( Typea==Typeb .AND. Typea==Typd ) THEN
            k = max0(M,N,Q,R)
            j = k*2 + 1
            k = k + 1
            CALL mpydri(Z,Z,Z(k),Z(k),Z(j),Z(j))
            GOTO 700
         ENDIF
      ENDIF
   ENDIF
!
   Rcb = Rc(Typeb)
   Nbx = R*Rcb
   Nwdb = Nwds(Typeb)
   nwdb1 = Nwdb + 1
   Nb = R*Nwdb
   Ndx = p*Rcd
   Nd = p*Nwds(Typed)
   nzz = iabs(Nz) - Sysbuf + 1
   buf2 = buf1 - Sysbuf
   buf3 = buf2 - Sysbuf
   buf4 = buf3 - Sysbuf
   jj = (nzz-1)/(Nb+Nd)
   icrq = Nb + Nd - nzz + 1
   IF ( icrq>0 ) GOTO 1300
   mpass1 = (Q-1)/jj + 1
   jzb = jj*Nd + 1
   jzdb = jj*Ndx + 1
   Jb = jzb
   IF ( Prc(Typeb)==2 ) Jb = jzdb
   Nwda = Nwds(Typea)
   prca = Prc(Typea)
   Na = Nwda*N
   nwda1 = Nwda + 1
   Nwdd = Nwds(Typed)
   Acore = Nd + 1
   IF ( T/=0 ) Acore = Nb + 1
   Acore = ((Acore+1)/2)*2 + 1
   IF ( Signab==0 .AND. Prec==1 .AND. (Prc(Typea)==2 .OR. Prc(Typeb)==2) ) Typed = Rcd + 1
   IF ( noab==1 .OR. Signab==0 ) THEN
!
!     A MATRIX OR B MATRIX IS NULL - COPY C MATRIX TO D MATRIX
!
      Time = 0.0
      IF ( Filed(1)<0 ) GOTO 99999
      IF ( Q<=0 ) Q = Filec(2)
      Filed(2) = 0
      Filed(6) = 0
      Filed(7) = 0
      WRITE (nout,99003)
99003 FORMAT ('             MPYAD - NULL MATRIX PRODUCT')
      CALL gopen(Filed,Z(buf1),Wrtrew)
      IF ( cfile==0 ) THEN
!
!     PACK NULL COLUMNS BECAUSE C MATRIX IS NULL
!
         Pp1 = 1
         DO Acol = 1 , Q
            CALL pack(zero,Filed,Filed)
         ENDDO
      ELSEIF ( Typec==Signc*Typd ) THEN
!
!     USE CPYSTR TO COPY C TO D
!
         Block(1) = cfile
         blk(1) = Filed(1)
         CALL gopen(cfile,Z(buf2),Rdrew)
         DO ii = 1 , Q
            CALL cpystr(Block,blk,0,0)
         ENDDO
         CALL close(cfile,Clsrew)
         Filed(2) = Q
         Filed(5) = Filec(5)
         Filed(6) = Filec(6)
         Filed(7) = Filec(7)
      ELSE
!
!     USE INTPK/BLDPK TO COPY C TO D BECAUSE TYPES CONFLICT
!
         CALL gopen(cfile,Z(buf2),Rdrew)
         DO ii = 1 , Q
            CALL bldpk(Typd,Typd,Filed,Block,1)
            itypsg = Signc*Typd
            CALL intpk(*10,Filec,0,itypsg,0)
            DO
               CALL zntpki
               CALL bldpki(A,Ip,Filed,Block)
               IF ( Eol/=0 ) EXIT
            ENDDO
 10         CALL bldpkn(Filed,Block,Filed)
         ENDDO
         CALL close(cfile,Clsrew)
      ENDIF
      IF ( Filec(1)>0 ) Filed(4) = Filec(4)
      CALL close(Filed,Clsrew)
      GOTO 700
   ELSEIF ( Signab==1 .OR. Signab==-1 ) THEN
      CALL mpyq(Z)
!
!     CALCULATE ESTIMATED EXECUTION TIMES AND SELECT METHOD.
!
      ncore = buf3 - Acore
      icrq = -ncore
      IF ( icrq>0 ) GOTO 1300
      core = float(ncore/Nwda)
      fn = Filea(2)
      fm = Filea(3)
      fp = Fileb(2)
      rhoa = amin1(1.E-4*float(Filea(7)),1.0)
      rhob = amin1(1.E-4*float(Fileb(7)),1.0)
      rhoc = amin1(1.E-4*float(Filec(7)),1.0)
      rhod = amax1(rhoa,rhob)
      arith = fm*fn*(Tmt(Typed)+(1.0-rhoa)*Tml(Typed))
      aterm = (fm*rhoa+5.0)*fn*Tmipak
      bterm = float(R)*fp*0.5*(1.0+rhob)*Tmupak
      dterm = fm*fp*0.5*(1.0+rhod)*Tmpak
      cterm = 0
      IF ( cfile/=0 ) cterm = fm*fp*0.5*(1.0+rhoc)*Tmupak
      time1 = (fm*fn*fp*rhoa*Tmt(Typed)+float(mpass1)*aterm+bterm+dterm+cterm)*1.0E-6
!
      mpass2 = (2.0-rhoa)*fm*fn*rhoa/core + 1.0
      fr = mpass2
      IF ( T/=0 ) THEN
!
         fnt = fn*fm*rhob
         p1 = amin1((fnt/float(Fileb(6))+fp)/2.0,fnt,fp)
         fp1 = p1
         cterm2 = 0.
         IF ( cfile/=0 ) cterm2 = (fn*rhoc+5.0)*fp*Tmipak
         bterm = fm*fp*0.5*(1.0+rhob)*Tmupak
         dterm2 = (fn*rhod+5.0)*fp
         time2 = (fp1*rhoa*arith+(fm*rhoa+5.0)*fn*Tmipak+fr*bterm+(fr+1.0)/2.0*dterm2*Tmbpak+(fr-1.0)/2.0*dterm2*Tmipak+cterm2)     &
               & *1.0E-6
!
         bufi = buf4
         IF ( Filec(1)==0 ) bufi = buf3
         nbrrow = min0((bufi-orf(Nd+1,1))/Na,M)
         mpass3 = (M-1)/nbrrow + 1
         fr = mpass3
         time3 = (fm*fn*fp*rhob*Tmt(Typed)+fm*fn*0.5*(1.0+rhoa)*Tmupak+fr*fp*(fn*rhob+5.0)*Tmipak+(fr+1.0)/4.0*fn*fp*(1.0+rhod)     &
               & *Tmpak+(fr-1.0)/4.0*fn*fp*(1.0+rhod)*Tmupak+cterm2)*1.E-6
      ELSE
         time2 = (fp*rhoa*rhob*arith+aterm+(fr+1.0)/2.0*(fn*rhob+10.0)*fp*Tmipak+fr*dterm+(fr-1.0)*0.5*fm*fp*(1.0+rhod)             &
               & *Tmupak+cterm)*1.0E-6
      ENDIF
      CALL tmtogo(itimgo)
      IF ( core<=0.0 ) time2 = amax1(time1,time3) + 1.0
      Time = amin1(time1,time2,time3)
      itime = Time + 1
      IF ( itimgo<=itime .AND. Filed(1)>0 ) THEN
         mm = -50
         file = itime
         GOTO 1200
      ELSE
!
!     PRINT TIMING MESSAGE AND IF OUTPUT FILE IS PURGED RETURN
!
         ielems = fn*fm*rhoa + 0.5
         jelems = float(R)*fp*rhob
!WKBNB 9/93
         IF ( l19/=0 ) THEN
            CALL fname(Filea,namea)
            CALL fname(Fileb,nameb)
            CALL fname(Filec,namec)
            CALL fname(Filed,named)
!WKBR 7/94/SPR 94008 *         NAMEA, N, M, IELEMS, RHOA, PRNTYP( TYPEA )
!KWBR 7/94 SPR 94008 *,        NAMEB, R, Q, JELEMS, RHOB, PRNTYP( TYPEB )
            WRITE (nout,99004,IOSTAT=ierr) namea , N , M , ielems , rhoa , prntyp(itypea) , nameb , R , Q , jelems , rhob ,         &
                 & prntyp(itypeb)
99004       FORMAT ('  /-----------------------------------------------------------/',/,                                            &
                   &'  /     MATRIX      ROWS   COLS     TERMS  DENS    TYPE       /',/,                                            &
                   &'  /-----------------------------------------------------------/',/,'  /  A- ',2A4,I8,I7,I10,F7.4,5X,A2,/,      &
                   &'  /  B- ',2A4,I8,I7,I10,F7.4,5X,A2)
            ielems = fn*fm*rhoc + .5
            IF ( cfile/=0 ) THEN
               WRITE (nout,99005,IOSTAT=ierr) namec , Filec(3) , Filec(2) , ielems , rhoc , prntyp(itypec)
99005          FORMAT ('  /  C- ',2A4,I8,I7,I10,F7.4,5X,A2)
            ENDIF
            WRITE (nout,99006) named , prntyp(Typed)
99006       FORMAT ('  /  D- ',2A4,8X,7X,10X,7X,5X,A2)
            WRITE (nout,99007) Signab , Signc , T , core , mpass1 , mpass2 , mpass3 , time1 , time2 , time3
99007       FORMAT ('  /  SIGNAB =',I4,'  SIGNC =',I4,'  TIME EST=',I9,' MEMORY =',F8.0,/,'  /  MPASS1 =',I4,'  MPASS2=',I4,        &
                   &'  MPASS3=',I4,/,'  /  TIME1  =',E9.2,' TIME2=',E9.2,' TIME3=',E9.2,/,                                          &
                   &'  /-----------------------------------------------------------/')
         ENDIF
!WKBNE 9/93
!
         IF ( Filed(1)<0 ) GOTO 99999
!
         j = Ksys58
         IF ( j<0 .OR. j>3 .OR. (j==3 .AND. it==0) ) j = 0
         IF ( j/=0 ) THEN
            IF ( j==1 ) GOTO 20
            IF ( j==2 ) GOTO 1400
            IF ( j==3 ) GOTO 3100
         ENDIF
         IF ( it/=0 ) THEN
!WKBD 2/95 NCL93004 190 IF (TIME1.LT.TIME2 .AND. TIME1.LT.TIME3) GO TO 200
!WKBNB 2/95 NCL93004
            IF ( .NOT.(mpass1<mpass2 .AND. (time1<time3 .OR. mpass1<mpass3)) ) THEN
               IF ( .NOT.(mpass2<mpass1 .AND. (time2<time3 .OR. mpass2<mpass3)) ) THEN
                  IF ( time1>=time2 .OR. time1>=time3 ) THEN
!WKBNE 2/95 NCL93004
                     IF ( time2>=time3 ) GOTO 3100
                     GOTO 1400
                  ENDIF
               ENDIF
            ENDIF
!WKBNB 2/95 NCL93004
         ELSEIF ( mpass1>=mpass2 ) THEN
            IF ( mpass2<mpass1 ) GOTO 1400
!WKBNE 2/95 NCL93004
            IF ( time1>=time2 ) GOTO 1400
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
 20      jcol = 1
         WRITE (nout,99009) method(T+1) , mpass1 , time1
      ENDIF
   ELSE
      WRITE (Mout,99008)
99008 FORMAT ('0*** USER FATAL MESSAGE 2398, MPYAD REQUIRES SIGN OF ','A*B TO BE -1, 0, OR +1')
      mm = -37
      GOTO 1200
   ENDIF
 100  Jmax = min0(jcol+jj-1,Q)
   IF ( Jmax==Q ) op = Clsrew
   jmax1 = Jmax - jcol
   Jmax = jmax1 + 1
   Jmax1x = jmax1*Ndx
   IF ( Fileb(6)/=0 ) THEN
!
!     READ AND UNPACK JMAX COLUMNS OF THE B MATRIX
!
      file = Fileb(1)
      jz = jzb
      Typebd = Typeb*Signab
      nbd = Nb
      opbc = opb
      Pp2 = R
      ASSIGN 200 TO mm
      GOTO 800
   ENDIF
!
!     READ AND UNPACK JMAX COLUMNS OF THE C MATRIX
!
 200  file = Filec(1)
   jz = 1
   Typebd = Typed*Signc
   nbd = Nd
   opbc = opc
   Pp2 = p
   ASSIGN 300 TO mm
   GOTO 800
!
!     OPEN AND POSITION A MATRIX TO FIRST COLUMN
!
 300  IF ( Fileb(6)==0 ) GOTO 600
   file = Filea(1)
   CALL open(*1000,Filea,Z(nzz),Rdrew)
   CALL fwdrec(*1100,Filea)
!
!     SET POINTERS
!     L   = COLUMN NUMBER
!     LL  = POINTER TO LTH ROW OF B MATRIX
!     LLL = POINTER TO LTH ROW OF D MATRIX
!
   l = 1
   Ll = Jb
   Lll = 1
!
!     CALL INTPK TO INITIATE READING THE LTH COLUMN OF THE A MATRIX
!     IF COLUMN IS NULL, BYPASS ARITHMETIC
!
 400  CALL intpk(*500,Filea,0,Typed,0)
!
!     FORM EITHER  A(I,L)*B(L,J) + D(I,J)
!              OR  A(L,I)*B(I,J) + D(L,J)
!           WHERE  J RUNS ACROSS COLUMNS OF B AND D NOW IN CORE
!
   CALL mpy1v(Zz,Z,Zd)
!
!     POSITION POINTERS FOR NEXT COLUMN OF A
!
 500  Ll = Ll + Rcb
   Lll = Lll + Rcd
   l = l + 1
   IF ( l<=M ) GOTO 400
!
!     CLOSE AND REWIND FILE CONTAINING A MATRIX
!
   CALL close(Filea,Clsrew)
!
!     OPEN FILE CONTAINING D MATRIX TO WRITE
!
 600  file = Filed(1)
   CALL open(*1000,Filed,Z(nzz),op2)
!
!     IF FIRST COLUMNS OF D, WRITE HEADER
!
   IF ( op2/=Wrt ) THEN
      CALL fname(Filed,bcd)
      CALL write(Filed,bcd,2,1)
   ENDIF
!
!     PACK AND WRITE JMAX COLUMNS OF THE D MATRIX
!
   jz = 1
   DO j = 1 , Jmax
      CALL pack(Z(jz),Filed,Filed)
      jz = jz + Nd
   ENDDO
!
!     TEST FOR END OF MULTIPLICATION
!     CLOSE FILE CONTAINING D MATRIX
!
   CALL close(Filed,op)
!
!     SET OP FLAGS FOR OPEN CALLS FOR NEXT PASS
!
   opb = Rd
   opc = Rd
   op2 = Wrt
!
   jcol = jcol + jj
   IF ( jcol<=Q ) GOTO 100
 700  mpy(3) = jend
   CALL conmsg(mpy,3,0)
   GOTO 99999
!
!     INTERNAL SUBROUTINE TO READ JMAX COLUMNS OF THE B OR C MATRICES
!     ELEMENTS ARE SET TO ZERO IF COLUMN IS NULL OR MATRIX ABSENT
!
!     OPEN AND POSITION FILE IF MATRIX IS PRESENT
!
 800  IF ( file/=0 ) THEN
      CALL open(*1000,file,Z(nzz),opbc)
      IF ( jcol==1 ) CALL fwdrec(*1100,file)
   ENDIF
!
!     LOOP THROUGH JMAX COLUMNS OF MATRIX
!
   DO j = 1 , Jmax
!
!     UNPACK THE JTH COLUMN IF MATRIX IS PRESENT
!
      IF ( file/=0 ) THEN
         CALL unpack(*850,file,Z(jz))
         GOTO 900
      ENDIF
!
!     ZERO COLUMN
!
 850  k2 = jz + nbd - 1
      DO k = jz , k2
         Z(k) = 0.
      ENDDO
!
!     POSITION POINTERS TO NEXT COLUMN OF MATRIX
!
 900  jz = jz + nbd
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
 1000 mm = -1
   GOTO 1200
 1100 mm = -2
 1200 CALL mesage(mm,file,name)
   GOTO 99999
!
 1300 mm = -8
   file = icrq
   GOTO 1200
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
 1400 mt4 = 0
   mt2 = T
   IF ( mod(Ksystm(94),100)/10/=1 ) THEN
      IF ( T/=0 .AND. l41/=1 .AND. Ll4/=6 ) THEN
         IF ( mpass2>2 .AND. cfile/=0 .AND. Densc>=700 ) THEN
            mt2 = 0
            mt4 = 2
            Acore = Nb + Nd + 1
            Acore = ((Acore+1)/2)*2 + 1
            jzb = Nd + 1
            Jb = Nd/Prec1 + 1
         ENDIF
      ENDIF
   ENDIF
   dfile = Filed(1)
   efile = Scrtch
   Block(1) = Filea(1)
   cfile = Filec(1)
   opa = Rdrew
   Typec = Typed*Signc
   Firstl = buf3 - 1
   WRITE (nout,99009) method(T+3+mt4) , mpass2 , time2
!
!     BEGIN PASS
!
!     OPEN DFILE TO WRITE.
!     READ AS MANY COLUMNS (OR ROWS) OF A AS CAN BE HELD
!     IN CORE IN PACKED FORM ON THIS PASS.
!
   Acol1 = 1
 1500 file = dfile
   CALL open(*1000,dfile,Z(buf3),Wrtrew)
   CALL fname(Filed(1),bcd)
   CALL write(dfile,bcd,2,1)
   Filed(2) = 0
   Filed(6) = 0
   Filed(7) = 0
   file = Filea(1)
   CALL gopen(Filea,Z(buf2),opa)
   Apoint = Acore
   l = Firstl
   Acol = Acol1
!WKBR 9/94   660 IF ( (APOINT+NA+2) .GE. L-2) GO TO 530
! ABOVE CHECK WAS OVER-ZEALOUS IN CHECKING FOR AVAILABLE MEMORY
! BECAUSE OF THE CHECK TWO LINES AFTER STATEMENT 670
 1600 IF ( (Apoint+2)>=l-2 ) GOTO 1800
   Zz(l) = 0
   Zz(l-1) = 0
   Block(8) = -1
   CALL getstr(*1700,Block)
   Incra = 1
   IF ( Prc(Type)==2 .AND. Prc(Typea)==1 ) Incra = 2
   Zz(l) = Apoint
   DO
      kr1 = Apoint + 2
      krn = kr1 + Nbrstr*Nwda - 1
      IF ( krn>=l-2 ) THEN
!
!     ALL COLUMNS OF A WILL NOT FIT ON THIS PASS.
!
         CALL bckrec(Filea(1))
         GOTO 1800
      ELSE
!
!     MOVE STRING FROM BUFFER TO CORE AND COMPLETE STRING DEFINITION
!     WORDS
!
         IF ( Prc(Type)/=2 .OR. Prc(Typea)/=1 ) THEN
            IF ( Prc(Type)==2 ) Point = Point*2 - 1
            DO ii = kr1 , krn
               Z(ii) = Xns(Point)
               Point = Point + Incra
            ENDDO
         ELSE
!
!  -- THIS CODE NECESSARY FOR UNIVAC DOUBLE PRECISION TO SINGLE PRC.
!
            inc = 1
            Incra = 1
            IF ( Type==4 ) inc = 2
            krn = kr1 + Nbrstr*inc - 1
            DO ii = kr1 , krn
               Z(ii) = Xnd(Point)
               Point = Point + Incra
            ENDDO
         ENDIF
         Zz(Apoint) = Row
         Zz(Apoint+1) = Nbrstr
         Zz(l-1) = Zz(l-1) + 1
         Apoint = krn + 1
!
!     GET NEXT STRING DEFINITION
!
         CALL endget(Block)
         CALL getstr(*1700,Block)
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
 1700 IF ( mt4==2 ) Zz(l-1) = orf(Zz(l-1),lshift(Row+Nbrstr-1,Ihalf))
!                                    NBR + LAST NON-ZERO TERM COLUMN NO.
!
   l = l - 2
   Acol = Acol + 1
   IF ( Acol<=M ) GOTO 1600
!
!     ALL COLUMNS OF A ARE IN - THIS IS THE LAST PASS
!
   Acoln = M
   CALL close(Filea(1),Clsrew)
   GOTO 1900
 1800 CALL close(Filea(1),Cls)
   Acoln = Acol - 1
!
!     IF CFILE IS PRESENT, OPEN IT.
!     IF THIS IS THE FIRST PASS, SKIP HEADER RECORD.
!     OPEN BFILE AND SKIP HEADER RECORD.
!     INITIALIZE COLUMN (OR ORW) COUNTER, BCOL, TO 1, AND BRANCH ON T.
!
 1900 IF ( cfile/=0 ) THEN
      file = cfile
      CALL open(*1000,cfile,Z(buf1),Rdrew)
      CALL fwdrec(*1100,cfile)
   ENDIF
   file = Fileb(1)
   CALL open(*1000,Fileb(1),Z(buf2),Rdrew)
   CALL fwdrec(*1100,Fileb(1))
   Bcol = 1
 2000 IF ( mt2==1 ) THEN
!
!     TRANSPOSE CASE, METHOD 2T
!     =========================
!
!     INITIATE BUILDING OF A PACKED COLUMN OF D.
!     UNPACK A COLUMN OF B IN CORE. IF NULL, COPY COLUMN FROM C TO D.
!     INITIATE INTERPRETATION OF A COLUMN OF C.
!
      CALL bldpk(Typed,Typd,dfile,0,0)
      Typebd = Typeb*Signab
      Pp2 = R
      CALL unpack(*2400,Fileb(1),Z)
      Eol = 1
      Crow = 16777215
!            16777215 = 2**24 - 1
!
      IF ( cfile/=0 ) THEN
         CALL intpk(*2800,cfile,0,Typec,0)
         Crow = 0
      ENDIF
      GOTO 2800
!
!     UNPACK A COLUMN OF C.
!
   ELSEIF ( cfile/=0 ) THEN
      Typebd = Typec
      IF ( mt4/=0 ) One2 = 1
      Pp2 = p
      CALL unpack(*2100,cfile,Z)
      GOTO 2200
   ENDIF
 2100 DO ii = 1 , Nd
      Z(ii) = 0.
   ENDDO
 2200 IF ( mt4/=0 ) THEN
!
!     TRNASPOSE CASE, METHOD 4T
!     =========================
!
!     UNPACK A BANDED COLUMN OF MATRIX B, RANGING FROM ONE2 THRU PP2.
!     FOR THE RANGE  MAX0(ONE2,ACOL1) THRU MIN0(PP2,ACOLN), FORM ALL
!     PRODUCTS
!     D(I,K) = A(I,J)*B(J,K) + C(I,K)
!
      Typebd = Typeb*Signab
      One2 = 0
      CALL unpack(*2300,Fileb,Z(jzb))
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
      itypsg = Typeb*Signab
      CALL intpk(*2300,Fileb(1),0,itypsg,0)
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
 2300 CALL pack(Z,dfile,Filed)
   GOTO 3000
 2400 IF ( cfile==0 ) GOTO 2900
   CALL intpk(*2900,cfile,0,Typec,0)
 2500 CALL zntpki
   Crow = Ip
 2600 DO ii = 1 , Nwdd
      D(ii) = A(ii)
   ENDDO
   Drow = Crow
   CALL zblpki
 2700 IF ( Eol/=0 ) GOTO 2900
   GOTO 2500
!
!     FOR ALL NON-NULL ROWS OF A IN CORE, FORM A(I,J)*B(J) + C(I)
!
 2800 CALL mpy2tv(Zz,Z,Zd)
   IF ( Arown/=M .AND. Crow/=16777215 ) THEN
      IF ( Crow<=Arown ) GOTO 2700
      GOTO 2600
   ENDIF
!
!     TERMINATE CURRENT COLUMN OF D.
!
 2900 CALL bldpkn(dfile,0,Filed)
!
!     BOTH TRANSPOSE (2T AND 4T) AND NON-TRANSPOSE (2NT) CASES
!
!     TEST FOR COMPLETION OF PASS. IF COMPLETE, TEST ALL PASSES.
!
 3000 Bcol = Bcol + 1
   IF ( Bcol<=Q ) GOTO 2000
   CALL close(Fileb,Clsrew)
   IF ( cfile/=0 ) CALL close(cfile,Clsrew)
   CALL close(dfile,Clsrew)
   IF ( Acoln==M ) THEN
!
!     LAST PASS -
!     MAKE SURE D MATRIX IS ON PROPER FILE.
!     IF NOT, SWITCH FIST AND FIAT UNIT NBRS IN /XFIAT/
!
      IF ( dfile/=Filed(1) ) CALL filswi(dfile,Filed)
      GOTO 700
   ELSE
!
!     NOT LAST PASS - SWITCH C AND D FILES AND CONTINUE
!
      opa = Rd
      Typec = Typed
      IF ( Acol1==1 ) THEN
         cfile = dfile
         dfile = efile
      ELSE
         k = cfile
         cfile = dfile
         dfile = k
      ENDIF
      Acol1 = Acoln + 1
      GOTO 1500
   ENDIF
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
 3100 WRITE (nout,99009) method(5) , mpass3 , time3
   Block(1) = Fileb(1)
   Acore = orf(Nd+1,1)
   cfile = Scrtch
   dfile = Filed(1)
   IF ( mod(mpass3,2)==0 ) THEN
      cfile = Filed(1)
      dfile = Scrtch
   ENDIF
   Arow1 = 1
   last = .FALSE.
   opa = Rdrew
   DO
!
!     BEGIN PASS BY FILLING CORE WITH UNPACKED COLUMNS OF A
!
      Arown = min0(Arow1+nbrrow-1,M)
      IF ( Arown==M ) last = .TRUE.
      CALL gopen(Filea,Z(buf1),opa)
      Typebd = Typea*Signab
      Pp2 = N
      Apoint = Acore
      DO Arow = Arow1 , Arown
         CALL unpack(*3120,Filea,Z(Apoint))
         GOTO 3140
 3120    k2 = Apoint + Na - 1
         DO ii = Apoint , k2
            Z(ii) = 0.
         ENDDO
 3140    Apoint = Apoint + Na
      ENDDO
      ii = Cls
      IF ( last ) ii = Clsrew
      CALL close(Filea,ii)
      Incra = (Arown-Arow1)*Na
!
!     PREPARE TO PASS B MATRIX AND C MATRIX FROM LAST PASS
!
      IF ( Arow1/=1 ) CALL gopen(cfile,Z(buf2),Rdrew)
      CALL gopen(dfile,Z(buf3),Wrtrew)
      CALL gopen(Fileb,Z(buf1),Rdrew)
      IF ( last .AND. Filec(1)/=0 ) CALL gopen(Filec,Z(buf4),Rdrew)
      Filed(2) = 0
      Filed(6) = 0
      Filed(7) = 0
      Typebd = Typed
      Pp2 = Arown
      k2 = Arown*Nwdd
!
      DO Bcol = 1 , Q
         IF ( Arow1/=1 ) THEN
!
!     INTERMEDIATE PASS OR LAST PASS - UNPACK COLUMN FROM PREVIOUS PASS
!
            CALL unpack(*3160,cfile,Z)
            null = .FALSE.
            IF ( last ) GOTO 3180
            GOTO 3200
         ENDIF
!
!     FIRST PASS OR NULL COLUMN ON CFILE - SET COLUMN OF D TO ZERO
!
 3160    DO ii = 1 , k2
            Z(ii) = 0.
         ENDDO
         null = .TRUE.
         IF ( .NOT.(last) ) GOTO 3200
!
!     LAST PASS - ADD COLUMN FROM C MATRIX (IF PRESENT)
!
 3180    IF ( Filec(1)==0 ) GOTO 3200
         itypsg = Typed*Signc
         CALL intpk(*3200,Filec,0,itypsg,0)
         null = .FALSE.
         DO
            CALL zntpki
            IF ( Typed==2 ) THEN
               Zd(Ip) = Zd(Ip) + Ad(1)
            ELSEIF ( Typed==3 ) THEN
               Z(2*Ip-1) = Z(2*Ip-1) + A(1)
               Z(2*Ip) = Z(2*Ip) + A(2)
            ELSEIF ( Typed==4 ) THEN
               Zd(2*Ip-1) = Zd(2*Ip-1) + Ad(1)
               Zd(2*Ip) = Zd(2*Ip) + Ad(2)
            ELSE
               Z(Ip) = Z(Ip) + A(1)
            ENDIF
            IF ( Eol/=0 ) EXIT
         ENDDO
!
!     FOR EACH NON-ZERO TERM B(J) IN THE CURRENT COLUMN OF B FORM
!     D(I,K) = D(I,K) + A(I,J)*B(J,K)
!
 3200    CALL mpy3t(*3220,Z(Acore),Z(Acore),Z(1),Z(1))
         GOTO 3240
!
!     PACK NULL COLUMN
!
 3220    IF ( null ) THEN
            Pp1 = 1
            CALL pack(zero,dfile,Filed)
            CYCLE
         ENDIF
!
!     PACK NON-NULL COLUMN
!
 3240    Pp1 = Arown
         CALL pack(Z,dfile,Filed)
!
!     TEST FOR END OF CURRENT PASS
!
      ENDDO
!
      IF ( Arow1/=1 ) CALL close(cfile,Clsrew)
      CALL close(dfile,Clsrew)
      CALL close(Fileb,Clsrew)
      IF ( last ) THEN
!
!     LAST PASS - SIGNAL END AND RETURN
!
         IF ( Filec(1)/=0 ) CALL close(Filec,Clsrew)
         GOTO 700
      ELSE
!
!     NOT LAST PASS - SWITCH FILES AND CONTINUE
!
         ii = cfile
         cfile = dfile
         dfile = ii
         Arow1 = Arown + 1
         opa = Rd
      ENDIF
   ENDDO
99009 FORMAT ('    METHOD TO BE USED:',A4,', NBR PASSES =',I4,',  EST. TIME =',F9.1)
!
99999 RETURN
END SUBROUTINE mpyado
