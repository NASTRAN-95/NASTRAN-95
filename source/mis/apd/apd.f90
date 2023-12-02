!*==apd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE apd
   USE c_apd1c
   USE c_bitpos
   USE c_blank
   USE c_system
   USE c_two
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(3) , SAVE :: aefact , aero , caero1 , caero2 , caero3 , caero4 , caero5 , flfact , fluttr , kspl , mkaer1 ,  &
                                  & mkaer2 , nbca , paero1 , paero2 , paero3 , paero4 , paero5 , set1 , set2 , splin1 , splin2 ,    &
                                  & splin3
   INTEGER , SAVE :: aeror , bgpdt , clsrew , cstm , ect , edt , eqaero , eqdyn , flist , gpld , rdrew , sild , spline , usetd ,    &
                   & wtrew
   INTEGER , DIMENSION(3) :: aerx
   REAL :: bref , vsound
   INTEGER , DIMENSION(7) :: buf
   INTEGER :: buf1 , buf2 , buf3 , buf4 , buf5 , buf6 , buf7 , buf8 , buf9 , file , flag , i , i17 , i20 , ip1 , iret , isp , izx , &
            & j , j1 , jp , k , kid , ko , last , lca , lcae , lcas , mask , n1 , n2 , n3 , n4 , ncrdo , ncsa , nextra , nls ,      &
            & nogo , nsplie , nsplis , nx , ny , pspa , silgp , symxy , symxz
   INTEGER , DIMENSION(1) :: iz
   LOGICAL :: lmkaer , lset , lskip , lsplin
   INTEGER , DIMENSION(7) , SAVE :: msg
   INTEGER , DIMENSION(9) , SAVE :: msg1
   INTEGER , DIMENSION(5) , SAVE :: msg2
   INTEGER , DIMENSION(6) , SAVE :: msg3
   INTEGER , DIMENSION(10) , SAVE :: msg4
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL andf , apd12 , apd3 , apd4 , apd5 , apdoe , apdr , bisloc , close , corwds , emsg , gopen , korsz , locate , mesage ,   &
          & open , orf , preloc , rdtrl , read , skprec , sort , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   !>>>>EQUIVALENCE (aerx(1),symxz) , (aerx(2),symxy) , (aerx(3),bref)
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         lca = caero1(3)
         nogo = 0
         buf1 = korsz(iz) - sysbuf
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf
         buf4 = buf3 - sysbuf
         buf5 = buf4 - sysbuf
         buf6 = buf5 - sysbuf
         buf7 = buf6 - sysbuf
         buf8 = buf7 - sysbuf
         buf9 = buf8 - sysbuf
         buf10 = buf9 - sysbuf
         buf11 = buf10 - sysbuf
         buf12 = buf11 - sysbuf
         scr1 = 301
         scr2 = 302
         scr3 = 303
         scr4 = 304
         scr5 = 305
         ecta = 202
         bgpa = 203
         sila = 204
         useta = 205
         acpt = 208
         cstma = 210
         gpla = 211
         silgp = 212
         last = buf12 - 1
         IF ( last<=0 ) THEN
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nj = 0
         nk = 0
         i17 = ibit(17)
         i20 = ibit(20)
         pspa = orf(itwo(i17),itwo(i20))
!
!     READ AERO CARDS
!
         left = last
         file = edt
         CALL preloc(*740,z(buf1),edt)
         CALL locate(*720,z(buf1),aero,flag)
         CALL read(*760,*780,edt,z(1),6,1,flag)
         acsid = iz(1)
         izx = 2
         vsound = z(izx)
         izx = 3
         bref = z(izx)
         bov = 0.0
         IF ( vsound/=0.0 ) bov = bref/(2.0*vsound)
         izx = 5
         symxz = iz(izx)
         izx = 6
         symxy = iz(izx)
!
!     READ AEFACT CARDS
!
         naef2 = 0
         CALL apdr(edt,z,left,naef1,naef2,flag,buf1,aefact)
!
!     READ CSTM TABLE
!
         file = cstm
         ncst2 = naef2
         ncst1 = 0
         mcstm = 0
         buf(1) = cstm
         CALL rdtrl(buf)
         IF ( buf(1)/=cstm ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(cstm,z(buf2),rdrew)
         ncst1 = ncst2 + 1
         CALL read(*760,*20,cstm,z(ncst1),left,0,ncst2)
         GOTO 780
 20      CALL close(cstm,clsrew)
         left = left - ncst2
         ncst2 = ncst1 + ncst2 - 1
!
!     FIND LARGEST CID OF CSTM
!
         DO j = ncst1 , ncst2 , 14
            IF ( iz(j)>=mcstm ) mcstm = iz(j)
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
!
!     FIND AC TRANS
!
         IF ( acsid/=0 ) THEN
            IF ( ncst1/=0 ) THEN
               DO iacs = ncst1 , ncst2 , 14
                  IF ( iz(iacs)==acsid ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ENDIF
            CALL mesage(-30,25,acsid)
            GOTO 740
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     WRITE CSTM TO CSTMA
!
         CALL gopen(cstma,z(buf2),wtrew)
         IF ( mcstm/=0 ) CALL write(cstma,z(ncst1),ncst2-ncst1+1,0)
         ncsa = mcstm
!
!     READ EQDYN INTO CORE
!
         next = ncst2 + 1
         file = eqdyn
         CALL gopen(eqdyn,z(buf3),rdrew)
         CALL skprec(eqdyn,1)
         CALL read(*760,*40,eqdyn,z(next),left,0,nx)
         GOTO 780
 40      CALL close(eqdyn,clsrew)
         buf(1) = eqdyn
         CALL rdtrl(buf)
         nextra = buf(3)
!
!     CIDBX = LARGEST ID
!     NCRD  = NUMBER OF GRID AND SCALAR POINTS
!
         ncrd = buf(2) - nextra
         ncrdo = ncrd
         cidbx = 1000000
!
!     WRITE SECOND RECORD OF EQDYN ONTO SCR1
!
         CALL gopen(scr1,z(buf3),wtrew)
         CALL write(scr1,z(next),nx,0)
!
!     READ BGPDT
!
         file = bgpdt
         CALL gopen(bgpdt,z(buf4),rdrew)
         CALL read(*760,*60,bgpdt,z(next),left,0,nx)
         GOTO 780
 60      CALL close(bgpdt,clsrew)
!
!     WRITE BGPDT TO BGPA
!
         CALL gopen(bgpa,z(buf4),wtrew)
         CALL write(bgpa,z(next),nx,0)
!
!     READ USETD
!
         file = usetd
         CALL gopen(usetd,z(buf5),rdrew)
         CALL read(*760,*80,usetd,z(next),left,0,nx)
         GOTO 780
 80      CALL close(usetd,clsrew)
!
!     MASK IN PS AND PA BITS
!
         n2 = next + nx - 1
         DO i = next , n2
            iz(i) = orf(iz(i),pspa)
         ENDDO
!
!     WRITE USETD TO USETA
!
         file = useta
         CALL gopen(useta,z(buf5),wtrew)
         CALL write(useta,z(next),nx,0)
!
!     READ ECT
!
         file = ect
         buf(1) = ect
         CALL gopen(ecta,z(buf6),wtrew)
         CALL rdtrl(buf)
         IF ( buf(1)/=ect ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(ect,z(buf7),rdrew)
         CALL read(*120,*100,ect,z(next),left,0,nx)
         GOTO 780
 100     CALL write(ecta,z(next),nx,1)
         CALL read(*120,*100,ect,z(next),left,0,nx)
         GOTO 780
 120     CALL close(ect,clsrew)
         spag_nextblock_1 = 4
      CASE (4)
         CALL write(ecta,nbca,3,0)
!
!     READ FIRST RECORD OF SILD INTO CORE
!
         file = sild
         CALL gopen(sild,z(buf8),rdrew)
         CALL read(*760,*140,sild,z(next),left,0,nx)
         GOTO 780
!
!     WRITE FIRST RECORD OF SILD ONTO SILA
!
 140     buf(1) = sild
         CALL rdtrl(buf)
!
!     SILB  + 6 = NEXT DOF IN PROBLEM
!     ISILN + 6 = NEXT DOF WITHOUT EXTRA POINTS
!
         silb = buf(2) - 5
         isiln = silb - nextra
         CALL gopen(sila,z(buf7),wtrew)
         CALL write(sila,z(next),nx,0)
!
!     READ SECOND RECORD OF SILD INTO CORE
!
         CALL read(*760,*160,sild,z(next),left,0,nx)
         GOTO 780
 160     CALL close(sild,clsrew)
!
!     WRITE SECOND RECORD OF SILD ONTO SCR2
!
         CALL gopen(scr2,z(buf8),wtrew)
         CALL write(scr2,z(next),nx,0)
!
!     COPY GPLD TO GPLA
!
         file = gpld
         CALL gopen(gpld,z(buf9),rdrew)
         CALL read(*760,*180,gpld,z(next),left,0,nx)
         GOTO 780
 180     CALL close(gpld,clsrew)
         CALL gopen(gpla,z(buf9),wtrew)
         CALL write(gpla,z(next),nx,0)
!
!     READ CAERO CARDS INTO CORE
!
         nca2 = ncst2
         lcas = nca2 + 1
         CALL apdr(edt,z,left,nca1,nca2,flag,buf1,caero1)
         ca2e = nca2
         CALL apdr(edt,z,left,ca2s,ca2e,flag,buf1,caero2)
         ca3e = ca2e
         CALL apdr(edt,z,left,ca3s,ca3e,flag,buf1,caero3)
         ca4e = ca3e
         CALL apdr(edt,z,left,ca4s,ca4e,flag,buf1,caero4)
         ca5e = ca4e
         CALL apdr(edt,z,left,ca5s,ca5e,flag,buf1,caero5)
         lcae = max0(nca2,ca2e,ca3e,ca4e,ca5e)
!
!     READ PAERO CARDS INTO CORE
!
         npa2 = ca5e
         CALL apdr(edt,z,left,npa1,npa2,flag,buf1,paero1)
         pa2e = npa2
         CALL apdr(edt,z,left,pa2s,pa2e,flag,buf1,paero2)
         pa3e = pa2e
         CALL apdr(edt,z,left,pa3s,pa3e,flag,buf1,paero3)
         pa4e = pa3e
         CALL apdr(edt,z,left,pa4s,pa4e,flag,buf1,paero4)
         pa5e = pa4e
         CALL apdr(edt,z,left,pa5s,pa5e,flag,buf1,paero5)
         next = pa5e + 1
         CALL close(edt,clsrew)
         IF ( nca1==0 .AND. ca2s==0 .AND. ca3s==0 .AND. ca4s==0 .AND. ca5s==0 ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     OPEN ACPT
!
         CALL gopen(acpt,z(buf1),wtrew)
!
!     CALL CAERO TYPE
!
         IF ( nca1/=0 .OR. ca2s/=0 ) CALL apd12
         IF ( ca3s/=0 ) CALL apd3
         IF ( ca4s/=0 ) CALL apd4
         IF ( ca5s/=0 ) CALL apd5
         luseta = luseta + 5
         CALL write(cstma,0,0,1)
         CALL close(cstma,clsrew)
         CALL close(acpt,clsrew)
         CALL write(ecta,0,0,1)
         CALL close(ecta,clsrew)
         CALL write(bgpa,0,0,1)
         CALL close(bgpa,clsrew)
         CALL write(gpla,0,0,1)
         CALL close(gpla,clsrew)
         CALL write(useta,0,0,1)
         CALL close(useta,clsrew)
         CALL write(sila,0,0,1)
         CALL write(scr1,0,0,1)
         CALL close(scr1,clsrew)
         CALL write(scr2,0,0,1)
         CALL close(scr2,clsrew)
!
!     READ SECOND RECORD OF EQAERO TABLE OFF SCR1
!
         file = scr1
         CALL gopen(scr1,z(buf3),rdrew)
         i = next
         DO WHILE ( i+2<=next+left )
            CALL read(*760,*200,scr1,z(i),2,0,nx)
            iz(i+2) = 0
            i = i + 3
         ENDDO
         GOTO 780
 200     CALL close(scr1,clsrew)
         nx = i - next
!
!     SORT TABLE ON SILD VALUE
!
         CALL sort(0,0,3,2,z(next),nx)
         ny = next + nx - 1
!
!     REPLACE THIRD ENTRIES WITH INTERNAL GRID ID WITH OUT EXTRA
!
         k = 0
         DO i = next , ny , 3
            IF ( iz(i+1)-(iz(i+1)/10)*10/=3 ) THEN
               k = k + 1
               iz(i+2) = k
            ENDIF
         ENDDO
!
!     SORT EQAERO TABLE
!
         CALL sort(0,0,3,1,z(next),nx)
!
!     CHECK FOR DUPLICATE EXT ID
!
         n1 = next + 3
         DO i = n1 , ny , 3
            IF ( iz(i-3)==iz(i) ) THEN
               CALL emsg(0,2329,1,2,0)
               nogo = 1
               WRITE (not,99001) iz(i)
99001          FORMAT (10X,26HDUPLICATE EXTERNAL ID NO. ,I8,11H GENERATED.)
            ENDIF
         ENDDO
!
!     WRITE FIRST RECORD OF EQAERO TABLE
!
         CALL gopen(eqaero,z(buf3),wtrew)
         DO i = next , ny , 3
            buf(1) = iz(i)
            buf(2) = iz(i+2)
            CALL write(eqaero,buf,2,0)
         ENDDO
         CALL write(eqaero,0,0,1)
!
!     WRITE SECOND RECORD OF EQAERO TABLE
!
         DO i = next , ny , 3
            CALL write(eqaero,iz(i),2,0)
         ENDDO
         CALL write(eqaero,0,0,1)
         CALL close(eqaero,clsrew)
!
!     PUT ON SPLINE A RECORD OF K POINTS WITH
!     EXTERNAL ID , BGPA POINTERS, AND K COLUMN NUMBER
!
         file = useta
         n1 = next + nx
         CALL gopen(useta,z(buf3),rdrew)
         CALL read(*760,*220,useta,z(n1),left-nx,0,n2)
         GOTO 780
 220     CALL close(useta,clsrew)
         CALL gopen(spline,z(buf3),wtrew)
         CALL write(spline,kspl,3,0)
         mask = ibit(19)
         mask = itwo(mask)
         ko = 1
         n3 = (ncrdo+nextra)*3 + next
         DO i = next , ny , 3
            IF ( mod(iz(i+1),10)==1 ) THEN
               k = 0
               n4 = iz(i+1)/10 - 2
               DO j = 1 , 6
                  IF ( andf(iz(n1+n4+j),mask)/=0 ) k = k + 1
               ENDDO
               IF ( k/=0 ) THEN
                  buf(1) = iz(i)
                  buf(2) = iz(i+2)
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
         CALL gopen(scr2,z(buf8),rdrew)
         CALL read(*760,*240,scr2,z(next),left,0,nx)
         GOTO 780
 240     CALL close(scr2,clsrew)
         CALL write(sila,z(next),nx,1)
         CALL close(sila,clsrew)
!
!     BUILD SILGP TABLE
!
         CALL gopen(silgp,z(buf8),wtrew)
         ny = next + nx - 1
         k = 0
         DO i = next , ny , 2
            iz(next+k) = iz(i)
            k = k + 1
         ENDDO
         CALL write(silgp,iz(next),k,1)
         CALL close(silgp,clsrew)
!
!     WRITE RECORD
!
         CALL gopen(aeror,z(buf2),wtrew)
         CALL write(aeror,aerx,3,1)
!
!     READ IN MKAERO1 CARDS
!
         file = edt
         CALL preloc(*740,z(buf1),edt)
         lmkaer = .FALSE.
         CALL locate(*280,z(buf1),mkaer1,flag)
         CALL read(*760,*260,edt,z(next),left,0,nx)
         GOTO 780
 260     n1 = next
         lmkaer = .TRUE.
         spag_nextblock_1 = 5
      CASE (5)
         n2 = n1 + 7
         SPAG_Loop_1_1: DO i = n1 , n2
            IF ( iz(i)==-1 ) EXIT SPAG_Loop_1_1
            buf(1) = iz(i)
            n3 = n2 + 1
            n4 = n3 + 7
            SPAG_Loop_2_2: DO j = n3 , n4
               IF ( iz(j)==-1 ) EXIT SPAG_Loop_2_2
               buf(2) = iz(j)
               CALL write(aeror,buf,2,0)
            ENDDO SPAG_Loop_2_2
         ENDDO SPAG_Loop_1_1
         IF ( n4-next+1<nx ) THEN
            n1 = n1 + 16
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     READ IN MKAER2 CARDS
!
 280     CALL locate(*320,z(buf1),mkaer2,flag)
         CALL read(*760,*300,edt,z(next),left,0,nx)
         GOTO 780
 300     CALL write(aeror,z(next),nx,0)
         lmkaer = .TRUE.
 320     CALL write(aeror,0,0,1)
         CALL close(aeror,clsrew)
         IF ( lmkaer ) THEN
!
!     PROCESS SET1 CARDS
!
            CALL open(*740,spline,z(buf2),3)
            lset = .FALSE.
            CALL locate(*380,z(buf1),set1,flag)
            lset = .TRUE.
            CALL read(*760,*340,edt,z(next),left,0,nx)
            GOTO 780
         ELSE
            CALL emsg(38,-2322,1,2,msg4)
            CALL mesage(-30,25,acsid)
            GOTO 740
         ENDIF
 340     n3 = next + nx
         CALL gopen(eqaero,z(buf3),rdrew)
         left = corwds(iz(n3),iz(last))
         file = eqaero
         CALL read(*760,*360,eqaero,z(n3),left,0,n4)
         GOTO 780
 360     n1 = next
         file = edt
         n2 = n1 + nx - 1
         CALL close(eqaero,clsrew)
         left = corwds(iz(next),iz(last))
!
!     CONVERT SET1 TO INTERNAL COOR NO
!
         lskip = .TRUE.
         DO i = n1 , n2
            IF ( iz(i)==-1 ) THEN
               lskip = .TRUE.
               CYCLE
            ELSEIF ( lskip ) THEN
               lskip = .FALSE.
               CYCLE
            ELSE
               kid = iz(i)
               CALL bisloc(*370,kid,iz(n3),2,n4/2,jp)
               k = n3 + jp
               IF ( iz(k)<=ncrdo ) THEN
                  iz(i) = iz(k)
                  CYCLE
               ENDIF
            ENDIF
 370        CALL emsg(0,2330,1,2,0)
            nogo = 1
            WRITE (not,99005) iz(n1) , iz(i)
         ENDDO
!
!     WRITE OUT SET1 CARD ON SPLINE
!
         CALL write(spline,set1,3,0)
         CALL write(spline,z(next),nx,1)
!
!     PROCESS SET2 CARDS
!
 380     CALL locate(*420,z(buf1),set2,flag)
         lset = .TRUE.
         CALL write(spline,set2,3,0)
         spag_nextblock_1 = 6
      CASE (6)
         CALL read(*760,*400,edt,z(next),8,0,nx)
         CALL write(spline,z(next),10,0)
         nx = iz(next+1)
         DO i = lcas , lcae , lca
            IF ( iz(i)==nx ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 18
      CASE (7)
         CALL write(spline,z(i),lca,0)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 400     CALL write(spline,0,0,1)
!
!     PROCESS SPLINE1 CARDS
!
 420     lsplin = .FALSE.
         CALL locate(*480,z(buf1),splin1,flag)
         lsplin = .TRUE.
         CALL write(spline,splin1,3,0)
         ASSIGN 440 TO iret
 440     CALL read(*760,*460,edt,z(next),6,0,nx)
         spag_nextblock_1 = 8
      CASE (8)
!
!     INTERNAL ROUTINE TO ATTACH CAERO DATA TO SPLINE
!
         CALL write(spline,z(next),10,0)
         nx = iz(next+1)
         DO i = lcas , lcae , lca
            IF ( iz(i)==nx ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 15
      CASE (9)
         CALL write(spline,z(i),lca,0)
         IF ( iz(next+2)<=iz(next+3) ) THEN
            j1 = iz(i+4)*iz(i+3) + iz(i) - 1
            IF ( iz(next+2)>=iz(i) .AND. iz(next+3)<=j1 ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
         nogo = 1
         CALL emsg(0,2331,1,2,0)
         WRITE (not,99002) iz(next) , iz(i)
99002    FORMAT (10X,30HBOX PICKED ON SPLINE CARD NO. ,I8,32HNOT GENERATED BY CAERO CARD NO. ,I8,1H.)
         spag_nextblock_1 = 11
      CASE (11)
         GOTO iret
 460     CALL write(spline,0,0,1)
!
!     PROCESS SPLINE2 CARDS
!
 480     CALL locate(*540,z(buf1),splin2,flag)
         lsplin = .TRUE.
         CALL write(spline,splin2,3,0)
         ASSIGN 500 TO iret
 500     CALL read(*760,*520,edt,z(next),10,0,nx)
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 520     CALL write(spline,0,0,1)
!
!     PROCESS  SPLINE3 CARDS
!
 540     nsplie = next - 1
         CALL apdr(edt,z,left,nsplis,nsplie,flag,buf1,splin3)
         IF ( nsplis==0 ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = eqaero
         n3 = nsplie + 1
         CALL gopen(eqaero,z(buf3),rdrew)
         CALL read(*760,*560,eqaero,z(n3),left,0,n4)
         GOTO 780
 560     file = edt
         CALL close(eqaero,clsrew)
         n4 = n4/2
         lset = .TRUE.
         left = left + flag
         lsplin = .TRUE.
         ASSIGN 580 TO iret
         CALL write(spline,splin3,3,0)
         isp = nsplis
         nls = 0
         spag_nextblock_1 = 12
      CASE (12)
!
!     PICK UP NEXT SPLIN3 AND ATTACHED CAERO
!
         isp = isp + nls
         IF ( isp>=nsplie ) THEN
            CALL write(spline,0,0,1)
            spag_nextblock_1 = 14
         ELSE
            CALL apdoe(iz(isp),z,isp,nsplie,flag,nls)
            nls = nls + 1
            nx = iz(isp+1)
            DO i = lcas , lcae , lca
               j = i
               IF ( iz(i)==nx ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            spag_nextblock_1 = 15
         ENDIF
      CASE (13)
         j1 = iz(i+3)*iz(i+4) + nx - 1
         iz(next) = iz(isp)
         IF ( iz(isp+2)<iz(isp+1) ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iz(isp+2)>j1 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CONVERT TO INTERNAL ID
!
         n2 = nls - 4
         DO i = 1 , n2 , 3
            n1 = iz(isp+i+3)
            CALL bisloc(*600,n1,iz(n3),2,n4,jp)
            IF ( iz(n3+jp)>ncrdo ) GOTO 600
            iz(isp+i+3) = iz(n3+jp)
         ENDDO
 580     CALL write(spline,nls+lca,1,0)
         CALL write(spline,iz(isp),nls,0)
         nls = nls + 1
         CALL write(spline,iz(j),lca,0)
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 600     nogo = 1
         CALL emsg(0,2330,1,2,0)
         WRITE (not,99005) iz(isp) , n1
         GOTO 580
      CASE (14)
         CALL close(spline,clsrew)
!
!     CREATE FLIST TABLE
!
         CALL gopen(flist,z(buf2),wtrew)
         CALL locate(*720,z(buf1),aero,flag)
         CALL read(*760,*620,edt,z(next),left,0,nx)
         GOTO 780
 620     CALL write(flist,aero,3,0)
         CALL write(flist,z(next),nx,1)
         CALL locate(*660,z(buf1),flfact,flag)
         CALL read(*760,*640,edt,z(next),left,1,nx)
         GOTO 780
 640     CALL write(flist,flfact,3,0)
         CALL write(flist,z(next),nx,1)
 660     CALL locate(*700,z(buf1),fluttr,flag)
         CALL read(*760,*680,edt,z(next),left,0,nx)
         GOTO 780
 680     CALL write(flist,fluttr,3,0)
         CALL write(flist,z(next),nx,1)
 700     CALL close(flist,clsrew)
         CALL close(edt,clsrew)
         msg(1) = aeror
         msg(2) = 1
         CALL wrttrl(msg)
         msg(1) = eqdyn
         CALL rdtrl(msg)
         msg(1) = eqaero
         msg(2) = ncrd + nextra
         CALL wrttrl(msg)
         msg(1) = bgpdt
         CALL rdtrl(msg(1))
         msg(3) = ncrd - msg(2)
         msg(1) = bgpa
         msg(2) = ncrd
         CALL wrttrl(msg)
         msg(1) = sila
         msg(2) = luseta
         msg(3) = nextra
         CALL wrttrl(msg)
         msg(1) = acpt
         msg(2) = 1
         CALL wrttrl(msg)
         msg(1) = gpla
         msg(2) = ncrd + nextra
         CALL wrttrl(msg)
         msg(1) = cstm
         CALL rdtrl(msg)
         IF ( msg(1)<0 ) msg(3) = 0
         msg(1) = cstma
         msg(3) = msg(3) + mcstm - ncsa
         CALL wrttrl(msg)
         msg(1) = useta
         msg(2) = luseta
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
         msg(2) = orf(msg(2),itwo(18))
         CALL wrttrl(msg)
         msg(1) = ect
         CALL rdtrl(msg)
         n1 = (nbca(2)-1)/16 + 2
         n2 = nbca(2) - (n1-2)*16 + 16
         msg(n1) = orf(msg(n1),itwo(n2))
         msg(1) = ecta
         CALL wrttrl(msg)
!
!     PUT OUT SILGP TRAILER
!
         msg(1) = silgp
         msg(2) = ncrd
         msg(3) = luseta - nextra
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
 720     CALL emsg(18,-2318,1,3,msg2)
         spag_nextblock_1 = 15
      CASE (15)
         CALL emsg(0,-2324,1,2,0)
         WRITE (not,99003) nx
99003    FORMAT (10X,19HCAERO  ELEMENT NO. ,I8,45H REFERENCED ON A SPLINEI CARD DOES NOT EXIST.)
         spag_nextblock_1 = 16
      CASE (16)
         CALL mesage(-61,0,nam)
         spag_nextblock_1 = 17
      CASE (17)
         CALL emsg(21,-2319,1,2,msg3)
         spag_nextblock_1 = 18
      CASE (18)
         CALL emsg(0,-2325,1,2,0)
         WRITE (not,99004) nx
99004    FORMAT (10X,19HCAERO  ELEMENT NO. ,I8,42H REFERENCED ON A SET2 CARD DOES NOT EXIST.)
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 740     ip1 = -1
         spag_nextblock_1 = 19
      CASE (19)
         CALL mesage(ip1,file,nam)
         spag_nextblock_1 = 20
      CASE (20)
         ip1 = -8
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
 760     ip1 = -2
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
 780     ip1 = 3
         spag_nextblock_1 = 19
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99005 FORMAT (10X,24HSET1 OR SPLIN3 CARD NO. ,I8,28H REFERENCES EXTERNAL ID NO. ,I8,22H WHICH DOES NOT EXIST.)
END SUBROUTINE apd
