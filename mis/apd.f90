
SUBROUTINE apd
   IMPLICIT NONE
   INTEGER Acpt , Acsid , Bgpa , Buf10 , Buf11 , Buf12 , Ca2e , Ca2s , Ca3e , Ca3s , Ca4e , Ca4s , Ca5e , Ca5s , Cidbx , Cp ,       &
         & Cstma , Ecta , Eid , Gpla , Iacs , Ibit(64) , Igid , Isiln , Itwo(32) , Iz(1) , Lchord , Left , Lspan , Luseta , Mcstm , &
         & Naef1 , Naef2 , Nca1 , Nca2 , Ncam , Nchord , Ncrd , Ncst1 , Ncst2 , Next , Nj , Nk , Not , Npa1 , Npa2 , Nspan , Pa2e , &
         & Pa2s , Pa3e , Pa3s , Pa4e , Pa4s , Pa5e , Pa5s , Pid , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Sila , Silb , Sysbuf , Useta
   REAL Alzo , Bov , X1 , X12 , X1p , X4 , X43 , Xop , Y1 , Y4 , Z(1) , Z1 , Z4
   COMMON /apd1c / Eid , Pid , Cp , Nspan , Nchord , Lspan , Lchord , Igid , X1 , Y1 , Z1 , X12 , X4 , Y4 , Z4 , X43 , Xop , X1p ,  &
                 & Alzo , Mcstm , Ncst1 , Ncst2 , Cidbx , Acsid , Iacs , Silb , Ncrd , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Ecta ,    &
                 & Bgpa , Gpla , Useta , Sila , Cstma , Acpt , Buf10 , Buf11 , Buf12 , Next , Left , Isiln , Ncam , Naef1 , Naef2 , &
                 & Nca1 , Nca2 , Ca2s , Ca2e , Ca3s , Ca3e , Ca4s , Ca4e , Npa1 , Npa2 , Pa2s , Pa2e , Pa3s , Pa3e , Pa4s , Pa4e ,  &
                 & Ca5s , Ca5e , Pa5s , Pa5e
   COMMON /bitpos/ Ibit
   COMMON /blank / Nk , Nj , Luseta , Bov
   COMMON /system/ Sysbuf , Not
   COMMON /two   / Itwo
   COMMON /zzzzzz/ Z
   INTEGER aefact(3) , aero(3) , aeror , aerx(3) , bgpdt , buf(7) , buf1 , buf2 , buf3 , buf4 , buf5 , buf6 , buf7 , buf8 , buf9 ,  &
         & caero1(3) , caero2(3) , caero3(3) , caero4(3) , caero5(3) , clsrew , cstm , ect , edt , eqaero , eqdyn , file , flag ,   &
         & flfact(3) , flist , fluttr(3) , gpld , i , i17 , i20 , ip1 , iret , isp , izx , j , j1 , jp , k , kid , ko , kspl(3) ,   &
         & last , lca , lcae , lcas , mask , mkaer1(3) , mkaer2(3) , msg(7) , msg1(9) , msg2(5) , msg3(6) , msg4(10) , n1 , n2 ,    &
         & n3 , n4 , nam(2) , nbca(3) , ncrdo , ncsa , nextra , nls , nogo , nsplie , nsplis , nx , ny , paero1(3) , paero2(3) ,    &
         & paero3(3) , paero4(3) , paero5(3) , pspa , rdrew
   INTEGER andf , corwds , korsz , orf
   REAL bref , vsound
   LOGICAL lmkaer , lset , lskip , lsplin
   INTEGER set1(3) , set2(3) , sild , silgp , splin1(3) , splin2(3) , splin3(3) , spline , symxy , symxz , usetd , wtrew
   EXTERNAL andf , orf
!
   EQUIVALENCE (Z(1),Iz(1))
   EQUIVALENCE (aerx(1),symxz) , (aerx(2),symxy) , (aerx(3),bref)
   DATA rdrew , clsrew , wtrew/0 , 1 , 1/
   DATA msg1/4HSETI , 4H AND , 4H/OR  , 4HSPLI , 4HNEI  , 4HCARD , 4HS RE , 4HQUIR , 4HED  /
   DATA msg2/4HNO A , 4HERO  , 4HCARD , 4H FOU , 4HND  /
   DATA msg3/4HNO C , 4HAERO , 4H  CA , 4HARDS , 4HFOUN , 4HD   /
   DATA msg4/4HNEIT , 4HHER  , 4HMKAE , 4HRO1  , 4HOR   , 4HMKAE , 4HRO2  , 4HCARD , 4HS FO , 4HUND /
   DATA caero2/4301 , 43 , 0/ , caero3/4401 , 44 , 0/
   DATA caero4/4501 , 45 , 0/ , paero2/4601 , 46 , 0/
   DATA paero3/4701 , 47 , 0/ , paero4/4801 , 48 , 0/
   DATA caero5/5001 , 50 , 0/ , paero5/5101 , 51 , 0/
   DATA splin3/4901 , 49 , 0/
   DATA kspl/200 , 2 , 0/
   DATA caero1/3002 , 30 , 16/ , paero1/3102 , 31 , 0/ , aero/3202 , 32 , 0/ , splin1/3302 , 33 , 0/ , splin2/3402 , 34 , 0/ ,      &
      & set1/3502 , 35 , 0/ , set2/3602 , 36 , 0/ , mkaer2/3702 , 37 , 0/ , mkaer1/3802 , 38 , 0/ , fluttr/3902 , 39 , 0/ ,         &
      & aefact/4002 , 40 , 0/ , flfact/4102 , 41 , 0/ , nbca/3002 , 46 , 0/
   DATA edt , eqdyn , ect , bgpdt , sild , usetd , cstm , gpld/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108/
   DATA eqaero , spline , aeror , flist/201 , 206 , 207 , 209/
   DATA msg/7*0/ , nam/4HAPD  , 4H    /
!
   lca = caero1(3)
   nogo = 0
   buf1 = korsz(Iz) - Sysbuf
   buf2 = buf1 - Sysbuf
   buf3 = buf2 - Sysbuf
   buf4 = buf3 - Sysbuf
   buf5 = buf4 - Sysbuf
   buf6 = buf5 - Sysbuf
   buf7 = buf6 - Sysbuf
   buf8 = buf7 - Sysbuf
   buf9 = buf8 - Sysbuf
   Buf10 = buf9 - Sysbuf
   Buf11 = Buf10 - Sysbuf
   Buf12 = Buf11 - Sysbuf
   Scr1 = 301
   Scr2 = 302
   Scr3 = 303
   Scr4 = 304
   Scr5 = 305
   Ecta = 202
   Bgpa = 203
   Sila = 204
   Useta = 205
   Acpt = 208
   Cstma = 210
   Gpla = 211
   silgp = 212
   last = Buf12 - 1
   IF ( last<=0 ) GOTO 5600
   Nj = 0
   Nk = 0
   i17 = Ibit(17)
   i20 = Ibit(20)
   pspa = orf(Itwo(i17),Itwo(i20))
!
!     READ AERO CARDS
!
   Left = last
   file = edt
   CALL preloc(*5400,Z(buf1),edt)
   CALL locate(*4900,Z(buf1),aero,flag)
   CALL read(*5700,*5800,edt,Z(1),6,1,flag)
   Acsid = Iz(1)
   izx = 2
   vsound = Z(izx)
   izx = 3
   bref = Z(izx)
   Bov = 0.0
   IF ( vsound/=0.0 ) Bov = bref/(2.0*vsound)
   izx = 5
   symxz = Iz(izx)
   izx = 6
   symxy = Iz(izx)
!
!     READ AEFACT CARDS
!
   Naef2 = 0
   CALL apdr(edt,Z,Left,Naef1,Naef2,flag,buf1,aefact)
!
!     READ CSTM TABLE
!
   file = cstm
   Ncst2 = Naef2
   Ncst1 = 0
   Mcstm = 0
   buf(1) = cstm
   CALL rdtrl(buf)
   IF ( buf(1)/=cstm ) GOTO 200
   CALL gopen(cstm,Z(buf2),rdrew)
   Ncst1 = Ncst2 + 1
   CALL read(*5700,*100,cstm,Z(Ncst1),Left,0,Ncst2)
   GOTO 5800
 100  CALL close(cstm,clsrew)
   Left = Left - Ncst2
   Ncst2 = Ncst1 + Ncst2 - 1
!
!     FIND LARGEST CID OF CSTM
!
   DO j = Ncst1 , Ncst2 , 14
      IF ( Iz(j)>=Mcstm ) Mcstm = Iz(j)
   ENDDO
!
!     FIND AC TRANS
!
 200  IF ( Acsid/=0 ) THEN
      IF ( Ncst1/=0 ) THEN
         DO Iacs = Ncst1 , Ncst2 , 14
            IF ( Iz(Iacs)==Acsid ) GOTO 300
         ENDDO
      ENDIF
      CALL mesage(-30,25,Acsid)
      GOTO 5400
   ENDIF
!
!     WRITE CSTM TO CSTMA
!
 300  CALL gopen(Cstma,Z(buf2),wtrew)
   IF ( Mcstm/=0 ) CALL write(Cstma,Z(Ncst1),Ncst2-Ncst1+1,0)
   ncsa = Mcstm
!
!     READ EQDYN INTO CORE
!
   Next = Ncst2 + 1
   file = eqdyn
   CALL gopen(eqdyn,Z(buf3),rdrew)
   CALL skprec(eqdyn,1)
   CALL read(*5700,*400,eqdyn,Z(Next),Left,0,nx)
   GOTO 5800
 400  CALL close(eqdyn,clsrew)
   buf(1) = eqdyn
   CALL rdtrl(buf)
   nextra = buf(3)
!
!     CIDBX = LARGEST ID
!     NCRD  = NUMBER OF GRID AND SCALAR POINTS
!
   Ncrd = buf(2) - nextra
   ncrdo = Ncrd
   Cidbx = 1000000
!
!     WRITE SECOND RECORD OF EQDYN ONTO SCR1
!
   CALL gopen(Scr1,Z(buf3),wtrew)
   CALL write(Scr1,Z(Next),nx,0)
!
!     READ BGPDT
!
   file = bgpdt
   CALL gopen(bgpdt,Z(buf4),rdrew)
   CALL read(*5700,*500,bgpdt,Z(Next),Left,0,nx)
   GOTO 5800
 500  CALL close(bgpdt,clsrew)
!
!     WRITE BGPDT TO BGPA
!
   CALL gopen(Bgpa,Z(buf4),wtrew)
   CALL write(Bgpa,Z(Next),nx,0)
!
!     READ USETD
!
   file = usetd
   CALL gopen(usetd,Z(buf5),rdrew)
   CALL read(*5700,*600,usetd,Z(Next),Left,0,nx)
   GOTO 5800
 600  CALL close(usetd,clsrew)
!
!     MASK IN PS AND PA BITS
!
   n2 = Next + nx - 1
   DO i = Next , n2
      Iz(i) = orf(Iz(i),pspa)
   ENDDO
!
!     WRITE USETD TO USETA
!
   file = Useta
   CALL gopen(Useta,Z(buf5),wtrew)
   CALL write(Useta,Z(Next),nx,0)
!
!     READ ECT
!
   file = ect
   buf(1) = ect
   CALL gopen(Ecta,Z(buf6),wtrew)
   CALL rdtrl(buf)
   IF ( buf(1)/=ect ) GOTO 900
   CALL gopen(ect,Z(buf7),rdrew)
   CALL read(*800,*700,ect,Z(Next),Left,0,nx)
   GOTO 5800
 700  CALL write(Ecta,Z(Next),nx,1)
   CALL read(*800,*700,ect,Z(Next),Left,0,nx)
   GOTO 5800
 800  CALL close(ect,clsrew)
 900  CALL write(Ecta,nbca,3,0)
!
!     READ FIRST RECORD OF SILD INTO CORE
!
   file = sild
   CALL gopen(sild,Z(buf8),rdrew)
   CALL read(*5700,*1000,sild,Z(Next),Left,0,nx)
   GOTO 5800
!
!     WRITE FIRST RECORD OF SILD ONTO SILA
!
 1000 buf(1) = sild
   CALL rdtrl(buf)
!
!     SILB  + 6 = NEXT DOF IN PROBLEM
!     ISILN + 6 = NEXT DOF WITHOUT EXTRA POINTS
!
   Silb = buf(2) - 5
   Isiln = Silb - nextra
   CALL gopen(Sila,Z(buf7),wtrew)
   CALL write(Sila,Z(Next),nx,0)
!
!     READ SECOND RECORD OF SILD INTO CORE
!
   CALL read(*5700,*1100,sild,Z(Next),Left,0,nx)
   GOTO 5800
 1100 CALL close(sild,clsrew)
!
!     WRITE SECOND RECORD OF SILD ONTO SCR2
!
   CALL gopen(Scr2,Z(buf8),wtrew)
   CALL write(Scr2,Z(Next),nx,0)
!
!     COPY GPLD TO GPLA
!
   file = gpld
   CALL gopen(gpld,Z(buf9),rdrew)
   CALL read(*5700,*1200,gpld,Z(Next),Left,0,nx)
   GOTO 5800
 1200 CALL close(gpld,clsrew)
   CALL gopen(Gpla,Z(buf9),wtrew)
   CALL write(Gpla,Z(Next),nx,0)
!
!     READ CAERO CARDS INTO CORE
!
   Nca2 = Ncst2
   lcas = Nca2 + 1
   CALL apdr(edt,Z,Left,Nca1,Nca2,flag,buf1,caero1)
   Ca2e = Nca2
   CALL apdr(edt,Z,Left,Ca2s,Ca2e,flag,buf1,caero2)
   Ca3e = Ca2e
   CALL apdr(edt,Z,Left,Ca3s,Ca3e,flag,buf1,caero3)
   Ca4e = Ca3e
   CALL apdr(edt,Z,Left,Ca4s,Ca4e,flag,buf1,caero4)
   Ca5e = Ca4e
   CALL apdr(edt,Z,Left,Ca5s,Ca5e,flag,buf1,caero5)
   lcae = max0(Nca2,Ca2e,Ca3e,Ca4e,Ca5e)
!
!     READ PAERO CARDS INTO CORE
!
   Npa2 = Ca5e
   CALL apdr(edt,Z,Left,Npa1,Npa2,flag,buf1,paero1)
   Pa2e = Npa2
   CALL apdr(edt,Z,Left,Pa2s,Pa2e,flag,buf1,paero2)
   Pa3e = Pa2e
   CALL apdr(edt,Z,Left,Pa3s,Pa3e,flag,buf1,paero3)
   Pa4e = Pa3e
   CALL apdr(edt,Z,Left,Pa4s,Pa4e,flag,buf1,paero4)
   Pa5e = Pa4e
   CALL apdr(edt,Z,Left,Pa5s,Pa5e,flag,buf1,paero5)
   Next = Pa5e + 1
   CALL close(edt,clsrew)
   IF ( Nca1==0 .AND. Ca2s==0 .AND. Ca3s==0 .AND. Ca4s==0 .AND. Ca5s==0 ) GOTO 5200
!
!     OPEN ACPT
!
   CALL gopen(Acpt,Z(buf1),wtrew)
!
!     CALL CAERO TYPE
!
   IF ( Nca1/=0 .OR. Ca2s/=0 ) CALL apd12
   IF ( Ca3s/=0 ) CALL apd3
   IF ( Ca4s/=0 ) CALL apd4
   IF ( Ca5s/=0 ) CALL apd5
   Luseta = Luseta + 5
   CALL write(Cstma,0,0,1)
   CALL close(Cstma,clsrew)
   CALL close(Acpt,clsrew)
   CALL write(Ecta,0,0,1)
   CALL close(Ecta,clsrew)
   CALL write(Bgpa,0,0,1)
   CALL close(Bgpa,clsrew)
   CALL write(Gpla,0,0,1)
   CALL close(Gpla,clsrew)
   CALL write(Useta,0,0,1)
   CALL close(Useta,clsrew)
   CALL write(Sila,0,0,1)
   CALL write(Scr1,0,0,1)
   CALL close(Scr1,clsrew)
   CALL write(Scr2,0,0,1)
   CALL close(Scr2,clsrew)
!
!     READ SECOND RECORD OF EQAERO TABLE OFF SCR1
!
   file = Scr1
   CALL gopen(Scr1,Z(buf3),rdrew)
   i = Next
   DO WHILE ( i+2<=Next+Left )
      CALL read(*5700,*1300,Scr1,Z(i),2,0,nx)
      Iz(i+2) = 0
      i = i + 3
   ENDDO
   GOTO 5800
 1300 CALL close(Scr1,clsrew)
   nx = i - Next
!
!     SORT TABLE ON SILD VALUE
!
   CALL sort(0,0,3,2,Z(Next),nx)
   ny = Next + nx - 1
!
!     REPLACE THIRD ENTRIES WITH INTERNAL GRID ID WITH OUT EXTRA
!
   k = 0
   DO i = Next , ny , 3
      IF ( Iz(i+1)-(Iz(i+1)/10)*10/=3 ) THEN
         k = k + 1
         Iz(i+2) = k
      ENDIF
   ENDDO
!
!     SORT EQAERO TABLE
!
   CALL sort(0,0,3,1,Z(Next),nx)
!
!     CHECK FOR DUPLICATE EXT ID
!
   n1 = Next + 3
   DO i = n1 , ny , 3
      IF ( Iz(i-3)==Iz(i) ) THEN
         CALL emsg(0,2329,1,2,0)
         nogo = 1
         WRITE (Not,99001) Iz(i)
99001    FORMAT (10X,26HDUPLICATE EXTERNAL ID NO. ,I8,11H GENERATED.)
      ENDIF
   ENDDO
!
!     WRITE FIRST RECORD OF EQAERO TABLE
!
   CALL gopen(eqaero,Z(buf3),wtrew)
   DO i = Next , ny , 3
      buf(1) = Iz(i)
      buf(2) = Iz(i+2)
      CALL write(eqaero,buf,2,0)
   ENDDO
   CALL write(eqaero,0,0,1)
!
!     WRITE SECOND RECORD OF EQAERO TABLE
!
   DO i = Next , ny , 3
      CALL write(eqaero,Iz(i),2,0)
   ENDDO
   CALL write(eqaero,0,0,1)
   CALL close(eqaero,clsrew)
!
!     PUT ON SPLINE A RECORD OF K POINTS WITH
!     EXTERNAL ID , BGPA POINTERS, AND K COLUMN NUMBER
!
   file = Useta
   n1 = Next + nx
   CALL gopen(Useta,Z(buf3),rdrew)
   CALL read(*5700,*1400,Useta,Z(n1),Left-nx,0,n2)
   GOTO 5800
 1400 CALL close(Useta,clsrew)
   CALL gopen(spline,Z(buf3),wtrew)
   CALL write(spline,kspl,3,0)
   mask = Ibit(19)
   mask = Itwo(mask)
   ko = 1
   n3 = (ncrdo+nextra)*3 + Next
   DO i = Next , ny , 3
      IF ( mod(Iz(i+1),10)==1 ) THEN
         k = 0
         n4 = Iz(i+1)/10 - 2
         DO j = 1 , 6
            IF ( andf(Iz(n1+n4+j),mask)/=0 ) k = k + 1
         ENDDO
         IF ( k/=0 ) THEN
            buf(1) = Iz(i)
            buf(2) = Iz(i+2)
            buf(3) = ko
            CALL write(spline,buf,3,0)
            ko = ko + k
         ENDIF
      ENDIF
   ENDDO
   CALL write(spline,0,0,1)
   CALL close(spline,2)
!
!     READ SECOND RECORD OF SILA TABLE
!
   CALL gopen(Scr2,Z(buf8),rdrew)
   CALL read(*5700,*1500,Scr2,Z(Next),Left,0,nx)
   GOTO 5800
 1500 CALL close(Scr2,clsrew)
   CALL write(Sila,Z(Next),nx,1)
   CALL close(Sila,clsrew)
!
!     BUILD SILGP TABLE
!
   CALL gopen(silgp,Z(buf8),wtrew)
   ny = Next + nx - 1
   k = 0
   DO i = Next , ny , 2
      Iz(Next+k) = Iz(i)
      k = k + 1
   ENDDO
   CALL write(silgp,Iz(Next),k,1)
   CALL close(silgp,clsrew)
!
!     WRITE RECORD
!
   CALL gopen(aeror,Z(buf2),wtrew)
   CALL write(aeror,aerx,3,1)
!
!     READ IN MKAERO1 CARDS
!
   file = edt
   CALL preloc(*5400,Z(buf1),edt)
   lmkaer = .FALSE.
   CALL locate(*1800,Z(buf1),mkaer1,flag)
   CALL read(*5700,*1600,edt,Z(Next),Left,0,nx)
   GOTO 5800
 1600 n1 = Next
   lmkaer = .TRUE.
 1700 n2 = n1 + 7
   DO i = n1 , n2
      IF ( Iz(i)==-1 ) EXIT
      buf(1) = Iz(i)
      n3 = n2 + 1
      n4 = n3 + 7
      DO j = n3 , n4
         IF ( Iz(j)==-1 ) EXIT
         buf(2) = Iz(j)
         CALL write(aeror,buf,2,0)
      ENDDO
   ENDDO
   IF ( n4-Next+1<nx ) THEN
      n1 = n1 + 16
      GOTO 1700
   ENDIF
!
!     READ IN MKAER2 CARDS
!
 1800 CALL locate(*2000,Z(buf1),mkaer2,flag)
   CALL read(*5700,*1900,edt,Z(Next),Left,0,nx)
   GOTO 5800
 1900 CALL write(aeror,Z(Next),nx,0)
   lmkaer = .TRUE.
 2000 CALL write(aeror,0,0,1)
   CALL close(aeror,clsrew)
   IF ( lmkaer ) THEN
!
!     PROCESS SET1 CARDS
!
      CALL open(*5400,spline,Z(buf2),3)
      lset = .FALSE.
      CALL locate(*2300,Z(buf1),set1,flag)
      lset = .TRUE.
      CALL read(*5700,*2100,edt,Z(Next),Left,0,nx)
      GOTO 5800
   ELSE
      CALL emsg(38,-2322,1,2,msg4)
      CALL mesage(-30,25,Acsid)
      GOTO 5400
   ENDIF
 2100 n3 = Next + nx
   CALL gopen(eqaero,Z(buf3),rdrew)
   Left = corwds(Iz(n3),Iz(last))
   file = eqaero
   CALL read(*5700,*2200,eqaero,Z(n3),Left,0,n4)
   GOTO 5800
 2200 n1 = Next
   file = edt
   n2 = n1 + nx - 1
   CALL close(eqaero,clsrew)
   Left = corwds(Iz(Next),Iz(last))
!
!     CONVERT SET1 TO INTERNAL COOR NO
!
   lskip = .TRUE.
   DO i = n1 , n2
      IF ( Iz(i)==-1 ) THEN
         lskip = .TRUE.
         CYCLE
      ELSEIF ( lskip ) THEN
         lskip = .FALSE.
         CYCLE
      ELSE
         kid = Iz(i)
         CALL bisloc(*2250,kid,Iz(n3),2,n4/2,jp)
         k = n3 + jp
         IF ( Iz(k)<=ncrdo ) THEN
            Iz(i) = Iz(k)
            CYCLE
         ENDIF
      ENDIF
 2250 CALL emsg(0,2330,1,2,0)
      nogo = 1
      WRITE (Not,99005) Iz(n1) , Iz(i)
   ENDDO
!
!     WRITE OUT SET1 CARD ON SPLINE
!
   CALL write(spline,set1,3,0)
   CALL write(spline,Z(Next),nx,1)
!
!     PROCESS SET2 CARDS
!
 2300 CALL locate(*2700,Z(buf1),set2,flag)
   lset = .TRUE.
   CALL write(spline,set2,3,0)
 2400 CALL read(*5700,*2600,edt,Z(Next),8,0,nx)
   CALL write(spline,Z(Next),10,0)
   nx = Iz(Next+1)
   DO i = lcas , lcae , lca
      IF ( Iz(i)==nx ) GOTO 2500
   ENDDO
   GOTO 5300
 2500 CALL write(spline,Z(i),lca,0)
   GOTO 2400
 2600 CALL write(spline,0,0,1)
!
!     PROCESS SPLINE1 CARDS
!
 2700 lsplin = .FALSE.
   CALL locate(*3400,Z(buf1),splin1,flag)
   lsplin = .TRUE.
   CALL write(spline,splin1,3,0)
   ASSIGN 2800 TO iret
 2800 CALL read(*5700,*3300,edt,Z(Next),6,0,nx)
!
!     INTERNAL ROUTINE TO ATTACH CAERO DATA TO SPLINE
!
 2900 CALL write(spline,Z(Next),10,0)
   nx = Iz(Next+1)
   DO i = lcas , lcae , lca
      IF ( Iz(i)==nx ) GOTO 3000
   ENDDO
   GOTO 5000
 3000 CALL write(spline,Z(i),lca,0)
   IF ( Iz(Next+2)<=Iz(Next+3) ) THEN
      j1 = Iz(i+4)*Iz(i+3) + Iz(i) - 1
      IF ( Iz(Next+2)>=Iz(i) .AND. Iz(Next+3)<=j1 ) GOTO 3200
   ENDIF
 3100 nogo = 1
   CALL emsg(0,2331,1,2,0)
   WRITE (Not,99002) Iz(Next) , Iz(i)
99002 FORMAT (10X,30HBOX PICKED ON SPLINE CARD NO. ,I8,32HNOT GENERATED BY CAERO CARD NO. ,I8,1H.)
 3200 GOTO iret
 3300 CALL write(spline,0,0,1)
!
!     PROCESS SPLINE2 CARDS
!
 3400 CALL locate(*3700,Z(buf1),splin2,flag)
   lsplin = .TRUE.
   CALL write(spline,splin2,3,0)
   ASSIGN 3500 TO iret
 3500 CALL read(*5700,*3600,edt,Z(Next),10,0,nx)
   GOTO 2900
 3600 CALL write(spline,0,0,1)
!
!     PROCESS  SPLINE3 CARDS
!
 3700 nsplie = Next - 1
   CALL apdr(edt,Z,Left,nsplis,nsplie,flag,buf1,splin3)
   IF ( nsplis==0 ) GOTO 4300
   file = eqaero
   n3 = nsplie + 1
   CALL gopen(eqaero,Z(buf3),rdrew)
   CALL read(*5700,*3800,eqaero,Z(n3),Left,0,n4)
   GOTO 5800
 3800 file = edt
   CALL close(eqaero,clsrew)
   n4 = n4/2
   lset = .TRUE.
   Left = Left + flag
   lsplin = .TRUE.
   ASSIGN 4100 TO iret
   CALL write(spline,splin3,3,0)
   isp = nsplis
   nls = 0
!
!     PICK UP NEXT SPLIN3 AND ATTACHED CAERO
!
 3900 isp = isp + nls
   IF ( isp>=nsplie ) THEN
      CALL write(spline,0,0,1)
      GOTO 4300
   ELSE
      CALL apdoe(Iz(isp),Z,isp,nsplie,flag,nls)
      nls = nls + 1
      nx = Iz(isp+1)
      DO i = lcas , lcae , lca
         j = i
         IF ( Iz(i)==nx ) GOTO 4000
      ENDDO
      GOTO 5000
   ENDIF
 4000 j1 = Iz(i+3)*Iz(i+4) + nx - 1
   Iz(Next) = Iz(isp)
   IF ( Iz(isp+2)<Iz(isp+1) ) GOTO 3100
   IF ( Iz(isp+2)>j1 ) GOTO 3100
!
!     CONVERT TO INTERNAL ID
!
   n2 = nls - 4
   DO i = 1 , n2 , 3
      n1 = Iz(isp+i+3)
      CALL bisloc(*4200,n1,Iz(n3),2,n4,jp)
      IF ( Iz(n3+jp)>ncrdo ) GOTO 4200
      Iz(isp+i+3) = Iz(n3+jp)
   ENDDO
 4100 CALL write(spline,nls+lca,1,0)
   CALL write(spline,Iz(isp),nls,0)
   nls = nls + 1
   CALL write(spline,Iz(j),lca,0)
   GOTO 3900
 4200 nogo = 1
   CALL emsg(0,2330,1,2,0)
   WRITE (Not,99005) Iz(isp) , n1
   GOTO 4100
 4300 CALL close(spline,clsrew)
!
!     CREATE FLIST TABLE
!
   CALL gopen(flist,Z(buf2),wtrew)
   CALL locate(*4900,Z(buf1),aero,flag)
   CALL read(*5700,*4400,edt,Z(Next),Left,0,nx)
   GOTO 5800
 4400 CALL write(flist,aero,3,0)
   CALL write(flist,Z(Next),nx,1)
   CALL locate(*4600,Z(buf1),flfact,flag)
   CALL read(*5700,*4500,edt,Z(Next),Left,1,nx)
   GOTO 5800
 4500 CALL write(flist,flfact,3,0)
   CALL write(flist,Z(Next),nx,1)
 4600 CALL locate(*4800,Z(buf1),fluttr,flag)
   CALL read(*5700,*4700,edt,Z(Next),Left,0,nx)
   GOTO 5800
 4700 CALL write(flist,fluttr,3,0)
   CALL write(flist,Z(Next),nx,1)
 4800 CALL close(flist,clsrew)
   CALL close(edt,clsrew)
   msg(1) = aeror
   msg(2) = 1
   CALL wrttrl(msg)
   msg(1) = eqdyn
   CALL rdtrl(msg)
   msg(1) = eqaero
   msg(2) = Ncrd + nextra
   CALL wrttrl(msg)
   msg(1) = bgpdt
   CALL rdtrl(msg(1))
   msg(3) = Ncrd - msg(2)
   msg(1) = Bgpa
   msg(2) = Ncrd
   CALL wrttrl(msg)
   msg(1) = Sila
   msg(2) = Luseta
   msg(3) = nextra
   CALL wrttrl(msg)
   msg(1) = Acpt
   msg(2) = 1
   CALL wrttrl(msg)
   msg(1) = Gpla
   msg(2) = Ncrd + nextra
   CALL wrttrl(msg)
   msg(1) = cstm
   CALL rdtrl(msg)
   IF ( msg(1)<0 ) msg(3) = 0
   msg(1) = Cstma
   msg(3) = msg(3) + Mcstm - ncsa
   CALL wrttrl(msg)
   msg(1) = Useta
   msg(2) = Luseta
   msg(3) = nextra
   msg(4) = pspa
   CALL wrttrl(msg)
   msg(1) = edt
   CALL rdtrl(msg)
   msg(1) = flist
   CALL wrttrl(msg)
   msg(1) = edt
   CALL rdtrl(msg)
   msg(1) = spline
   msg(2) = orf(msg(2),Itwo(18))
   CALL wrttrl(msg)
   msg(1) = ect
   CALL rdtrl(msg)
   n1 = (nbca(2)-1)/16 + 2
   n2 = nbca(2) - (n1-2)*16 + 16
   msg(n1) = orf(msg(n1),Itwo(n2))
   msg(1) = Ecta
   CALL wrttrl(msg)
!
!     PUT OUT SILGP TRAILER
!
   msg(1) = silgp
   msg(2) = Ncrd
   msg(3) = Luseta - nextra
   msg(4) = 0
   msg(5) = 0
   msg(6) = 0
   msg(7) = 0
   CALL wrttrl(msg)
   IF ( nogo==1 ) CALL mesage(-37,0,nam)
   IF ( lset .AND. lsplin ) RETURN
!
!     ERROR MESSAGES
!
   CALL emsg(35,-2328,1,2,msg1)
 4900 CALL emsg(18,-2318,1,3,msg2)
 5000 CALL emsg(0,-2324,1,2,0)
   WRITE (Not,99003) nx
99003 FORMAT (10X,19HCAERO  ELEMENT NO. ,I8,45H REFERENCED ON A SPLINEI CARD DOES NOT EXIST.)
 5100 CALL mesage(-61,0,nam)
 5200 CALL emsg(21,-2319,1,2,msg3)
 5300 CALL emsg(0,-2325,1,2,0)
   WRITE (Not,99004) nx
99004 FORMAT (10X,19HCAERO  ELEMENT NO. ,I8,42H REFERENCED ON A SET2 CARD DOES NOT EXIST.)
   GOTO 5100
 5400 ip1 = -1
 5500 CALL mesage(ip1,file,nam)
 5600 ip1 = -8
   GOTO 5500
 5700 ip1 = -2
   GOTO 5500
 5800 ip1 = 3
   GOTO 5500
99005 FORMAT (10X,24HSET1 OR SPLIN3 CARD NO. ,I8,28H REFERENCES EXTERNAL ID NO. ,I8,22H WHICH DOES NOT EXIST.)
END SUBROUTINE apd
