
SUBROUTINE cyct2
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Cdir(2) , Cycseq , Ii , Iii , Iik , Incr , Incr1 , Iprec , Ita , Itb , Itc , Iz(1) , Jj , Jjk , Kindex , Kseg ,          &
         & Ksystm(65) , Nload , Nogo , Nseg , Sysbuf
   DOUBLE PRECISION Constd(5) , Dz(2) , Pi
   REAL Z(1)
   COMMON /blank / Cdir , Nseg , Kseg , Cycseq , Nload , Nogo
   COMMON /condad/ Constd
   COMMON /packx / Ita , Itb , Ii , Jj , Incr
   COMMON /system/ Ksystm
   COMMON /unpakx/ Itc , Iik , Jjk , Incr1
   COMMON /zblpkx/ Dz , Iii
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   DOUBLE PRECISION arg , cos , sin
   INTEGER cycd , file , fore , i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , ibuf5 , ics , idir , iflag , igc , iks , ilama , ip , ip1 ,     &
         & ipass , ityp , j , kaa , kxx , l , lama , lamx , lua , maa , mcb(14) , mcb1(7) , mcb2(7) , mm , mxx , name(2) , ncopy ,  &
         & nlps , nskip , nx , nz , scr1 , scr2 , scr3 , scr4 , scr5 , scr6 , v1i , v1o , v2i , v2o
   INTEGER korsz
!
! End of declarations
!
!
!     CYCT2 TRANSFORMS CYCLIC PROBLEMS BETWEEN SOLUTION VARIABLES AND
!     THE CYCLIC COMPONENTS
!
!     INPUT DATA BLOCKS - CYCD    CYCLIC COMPONENT CONSTRAINT DATA
!     INPUT DATA BLOCKS - KAA     MATRIX - STIFFNESS    MAY BE PURGED
!     INPUT DATA BLOCKS - MAA     MATRIX - MASS         MAY BE PURGED
!     INPUT DATA BLOCKS - V1I     MATRIX - LOAD OR DISP MAY BE PURGED
!     INPUT DATA BLOCKS - V2I     MATRIX - EIGENVECTORS MAY BE PURGED
!     INPUT DATA BLOCKS - LAMX    TABLE  - EIGENVALUES MUST EXIS IF V2I
!
!     OUTPUT DATA BLOCKS- KXX,MXX,V1O,V2O,LAMA
!
!     PARAMETERS - CDIR           INPUT,  BCD, (FORE OR BACK)
!     PARAMETERS - NSEG           INPUT,  INTEGER,NUMBER OF SEGS
!     PARAMETERS - KSEG           INPUT,  INTEGER,CYCLIC INDEX
!     PARAMETERS - CYCSEQ         INPUT,  INTEGER,ALTERNATE=-1
!     PARAMETERS - NLOAD          INPUT,  INTEGER,NUMBER OF LOAD COND
!     PARAMETERS - NOGO           OUTPUT, INTEGER,-1 = ERROR
!
!     SCRATCH FILES (6)
!
!     DEFINITION OF VARIABLES
!     LUA       LENGT OF A SET
!     ITYP      TYPE (0=ROT, 1=DIH)
!     IDIR      DIRECTION (0=FORE, 1=BACK)
!     IFLAG     1 IMPLIES KSEG = 0 OR 2*KSEG = NSEG
!     IPASS     1 IMPLIES SECOND PASS TRROUGH CYCD
!     IGC       1 IMPLIES FIRST MATRIX TYPE (GC FOR ROT)
!     ICS       1 IMPLIES FIRST COLUMN TYPE (COSINE FOR ROT)
!
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(55),Iprec) , (Constd(1),Pi) , (Kseg,Kindex) , (Z(1),Iz(1)) , (mcb(1),mcb1(1)) ,         &
    & (mcb(8),mcb2(1))
   DATA cycd , kaa , maa , v1i , v2i , lamx , kxx , mxx , v1o , v2o , lama/101 , 102 , 103 , 104 , 105 , 106 , 201 , 202 , 203 ,    &
      & 204 , 205/
   DATA scr1 , scr2 , scr3 , scr4 , scr5 , scr6/301 , 302 , 303 , 304 , 305 , 306/
   DATA name , fore/4HCYCT , 4H2    , 4HFORE/
!
!
!IBMNB 6/93
   cycd = 101
   kaa = 102
   maa = 103
   v1i = 104
   v2i = 105
   lamx = 106
   kxx = 201
   mxx = 202
   v1o = 203
   v2o = 204
   lama = 205
   scr1 = 301
   scr2 = 302
   scr3 = 303
   scr4 = 304
   scr5 = 305
   scr6 = 306
!IBMNE
   nz = korsz(Iz)
   Nogo = 1
   v1i = 104
   v1o = 203
   scr3 = 303
   mcb(1) = cycd
   CALL rdtrl(mcb)
   lua = mcb(3)
   ityp = mcb(2) - 1
   idir = 1
   IF ( Cdir(1)==fore ) idir = 0
   nx = nz
   ibuf1 = nz - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   nz = ibuf3 - 1
   IF ( 2*Kseg>Nseg .OR. Kseg<0 .OR. Nseg<=0 ) GOTO 1900
   j = 2
   IF ( mcb(5)==4 ) j = 4
   IF ( nz<j*lua ) CALL mesage(-8,0,name)
!
!     PRODUCE GC AND GS MATRICES (ON SCR1 AND SCR2)
!
   arg = float(Kseg)/float(Nseg)
   arg = arg*Pi
   IF ( ityp==0 ) arg = 2.0D0*arg
!
!     BRING IN CYCD
!
   CALL gopen(cycd,Iz(ibuf1),0)
   CALL fread(cycd,Iz(1),lua,1)
   CALL close(cycd,1)
   CALL gopen(scr1,Iz(ibuf1),1)
!
!     COMPUTE COS AND SIN
!
   IF ( ityp==0 ) THEN
      IF ( Kseg/=0 ) THEN
         IF ( 2*Kseg/=Nseg ) GOTO 100
         cos = -1.0
         sin = 0.0
         GOTO 200
      ENDIF
   ELSEIF ( Kseg/=0 ) THEN
      IF ( 2*Kseg/=Nseg ) GOTO 100
      cos = 0.0
      sin = 1.0
      GOTO 200
   ENDIF
   cos = 1.0
   sin = 0.0
   GOTO 200
 100  cos = dcos(arg)
   sin = dsin(arg)
 200  iflag = 0
   IF ( Kseg==0 .OR. 2*Kseg==Nseg ) iflag = 1
   IF ( ityp/=0 .OR. iflag==0 ) CALL gopen(scr2,Iz(ibuf2),1)
   Ita = 2
   Itb = 1
   Incr = 1
   Ii = 1
   Jj = lua
   CALL makmcb(mcb1,scr1,lua,2,Iprec)
   CALL makmcb(mcb2,scr2,lua,2,Iprec)
   CALL wrttrl(mcb1)
   CALL wrttrl(mcb2)
   ipass = 0
   IF ( ityp/=0 ) THEN
!
!     BUILD DIHEDRAL MATRICES
!
      ipass = 0
      l = 1
      GOTO 700
   ELSE
!
!     BUILD ROTATIONAL MATRICES
!
      l = 1
   ENDIF
 300  IF ( Iz(l)<0 ) GOTO 600
   mm = Iz(l)
   ip = 1
!
!     FIRST BUILD GC
!
   igc = 1
   file = scr1
!
!     FIRST DO COSINE
!
   ics = 1
   IF ( ipass/=0 ) ics = 0
!
!     BUILD COLUMN
!
 400  CALL bldpk(2,Iprec,file,0,0)
   IF ( mm<0 ) GOTO 600
   IF ( mm/=0 ) THEN
!
!     SIDE 1 POINTS
!
      IF ( ics/=0 ) THEN
!
!     COSINE COLUMN
!
         IF ( igc==0 ) THEN
!
!     MATRIX IS GS
!
            Iii = mm
            Dz(1) = -sin
            CALL zblpki
!
!     MATRIX IS GC
!
            GOTO 500
         ENDIF
!
!     SINE COLUMN
!
      ELSEIF ( igc/=0 ) THEN
!
!     MATRIX IS GC
!
         Iii = mm
         Dz(1) = sin
         CALL zblpki
         GOTO 500
      ENDIF
!
!     MATRIX IS GS
!
      IF ( l>mm ) THEN
         Iii = mm
         Dz(1) = cos
         CALL zblpki
         Iii = l
         Dz(1) = 1.0
         CALL zblpki
      ELSE
         Dz(1) = 1.0
         Iii = l
         CALL zblpki
         Dz(1) = cos
         Iii = mm
         CALL zblpki
      ENDIF
!
!     INTERIOR POINT
!
   ELSEIF ( .NOT.((ics==0 .AND. igc==1) .OR. (ics==1 .AND. igc==0)) ) THEN
      Iii = l
      Dz(1) = 1.0
      CALL zblpki
   ENDIF
 500  CALL bldpkn(file,0,mcb(ip))
   IF ( Cycseq/=1 ) THEN
!
!     NOW DO SINE COLUMN
!
      IF ( ics/=0 ) THEN
         IF ( iflag==1 ) GOTO 600
         ics = 0
         GOTO 400
      ENDIF
   ENDIF
!
!     NOW DO GS
!
   IF ( iflag/=1 .AND. ip/=8 ) THEN
      ip = 8
      igc = 0
      ics = 1
      file = scr2
      GOTO 400
   ENDIF
!
!     CONSIDER NEXT CYCD VALUE
!
 600  l = l + 1
   IF ( l<=lua ) GOTO 300
!
!     GONE THRU CYCD ONCE. DONE IF CYCSEQ = -1
!
   IF ( Cycseq==-1 ) GOTO 1100
!
!     MUST NOW DO SINE COLUMNS UNLESS IFLAG = 1
!
   IF ( ipass==1 ) GOTO 1100
   IF ( iflag==1 ) GOTO 1100
   ipass = 1
   l = 1
   GOTO 300
 700  ip = 1
   igc = 1
   file = scr1
!
!     FIRST DO S COLUMN
!
   ics = 1
   IF ( ipass/=0 ) ics = 0
   mm = Iz(l)
   IF ( mm>0 .AND. ipass==1 ) GOTO 1000
 800  CALL bldpk(2,Iprec,file,0,0)
   IF ( mm>0 ) THEN
!
!     SIDE POINT
!
      IF ( igc==0 ) THEN
!
!     MATRIX IS GA
!
         Iii = l
         IF ( mm==2 ) THEN
            Dz(1) = cos
            CALL zblpki
         ELSEIF ( mm==3 ) THEN
         ELSEIF ( mm==4 ) THEN
            GOTO 850
         ELSE
            Dz(1) = sin
            CALL zblpki
         ENDIF
!
!     MATRIX IS GS
!
      ELSEIF ( mm==2 ) THEN
         Iii = l
         Dz(1) = -sin
         CALL zblpki
      ELSEIF ( mm==3 ) THEN
         Iii = l
         GOTO 850
      ELSEIF ( mm/=4 ) THEN
         Iii = l
         Dz(1) = cos
         CALL zblpki
      ENDIF
      GOTO 900
 850  Dz(1) = 1.0
      CALL zblpki
   ELSE
!
!     INTERIOR POINT
!
      IF ( ics/=0 ) THEN
!
!     SCOLUMN
!
!
!     MATRIX IS GA - S COLUMN
!
!
!     MATRIX IS GS - COLUMN IS S
!
         IF ( igc==0 ) GOTO 900
!
!     A COLUMN
!
      ELSEIF ( igc/=0 ) THEN
         GOTO 900
      ENDIF
!
!     MATRIX IS GA  - COLUMN IS A
!
      Dz(1) = 1.0
      Iii = l
!
!     MATRIX IS GS - COLUMN IS A
!
      CALL zblpki
   ENDIF
 900  CALL bldpkn(file,0,mcb(ip))
   IF ( Cycseq/=1 .AND. mm<=0 ) THEN
!
!     NOW DO A COLUMN
!
      IF ( ics/=0 ) THEN
         ics = 0
         GOTO 800
      ENDIF
   ENDIF
!
!     NOW DO GA
!
   IF ( ip/=8 ) THEN
      ip = 8
      igc = 0
      file = scr2
      ics = 1
      GOTO 800
   ENDIF
!
!     CONSIDER NEXT CYCD VALUE
!
 1000 l = l + 1
   IF ( l<=lua ) GOTO 700
!
!     GONE THRU CYCD ONCE - DONE IF CYCSEQ = -1
!
   IF ( Cycseq/=-1 ) THEN
!
!     NOW DO A COLUMNS
!
      IF ( ipass/=1 ) THEN
         ipass = 1
         l = 1
         GOTO 700
      ENDIF
   ENDIF
!
!     CLOSE UP SHOP
!
 1100 CALL close(scr1,1)
   CALL close(scr2,1)
   CALL wrttrl(mcb1)
   IF ( iflag==0 .OR. ityp/=0 ) CALL wrttrl(mcb2)
   Itc = 1
   Iik = 1
   Jjk = lua
   Incr1 = 1
   IF ( idir/=0 ) THEN
!
!     DIRECTION IS BACK
!
      mcb(1) = v1i
      CALL rdtrl(mcb)
      IF ( mcb(1)<=0 ) GOTO 1600
      iks = mcb(3)
      Itc = mcb(5)
      IF ( Itc==4 .AND. nz<4*lua ) CALL mesage(-8,0,name)
!
!     POSITION V1O
!
      mcb(1) = v1o
      IF ( Kindex==0 ) GOTO 1400
      CALL rdtrl(mcb)
      IF ( mcb(2)<=0 ) GOTO 1400
      CALL gopen(v1o,Iz(ibuf1),0)
      CALL skpfil(v1o,+1)
      CALL skpfil(v1o,-1)
      CALL close(v1o,2)
      GOTO 1500
   ELSE
!
!     FORWARD TRANSFORMATIONS
!
!
!     TRANSFORM MATRICES
!
      CALL cyct2a(kaa,kxx,scr1,scr2,scr3,scr4,scr5)
      CALL cyct2a(maa,mxx,scr1,scr2,scr3,scr4,scr5)
!
      mcb1(1) = kaa
      mcb2(1) = maa
      CALL rdtrl(mcb1(1))
      CALL rdtrl(mcb2(1))
      IF ( mcb1(5)<=2 .AND. mcb2(5)<=2 ) THEN
         IF ( mcb1(4)==6 .OR. mcb2(4)==6 ) THEN
            mcb1(1) = kxx
            mcb2(1) = mxx
            CALL rdtrl(mcb1(1))
            CALL rdtrl(mcb2(1))
            mcb1(4) = 6
            mcb2(4) = 6
            IF ( mcb1(1)>0 ) CALL wrttrl(mcb1(1))
            IF ( mcb2(1)>0 ) CALL wrttrl(mcb2(1))
         ENDIF
      ENDIF
!
!     TRANSFORM LOADS
!
      mcb(1) = v1i
      CALL rdtrl(mcb(1))
      IF ( mcb(1)<=0 ) GOTO 1300
      Itc = mcb(5)
      IF ( Itc==4 .AND. nz<4*lua ) CALL mesage(-8,0,name)
      CALL gopen(v1i,Iz(ibuf1),0)
      CALL gopen(scr3,Iz(ibuf2),1)
      CALL gopen(scr4,Iz(ibuf3),1)
!
!     COMPUTE NUMBER OF RECORDS TO SKIP
!
      CALL makmcb(mcb1,scr3,lua,2,mcb(5))
      CALL makmcb(mcb2,scr4,lua,2,mcb(5))
      IF ( Kseg/=0 ) THEN
         nskip = Nload*Kseg*(ityp+1)*2 - Nload*(ityp+1)
         file = v1i
         DO i = 1 , nskip
            CALL fwdrec(*1700,v1i)
         ENDDO
      ENDIF
      CALL cyct2b(v1i,scr3,Nload,Iz,mcb1)
      IF ( ityp/=0 ) THEN
         IF ( iflag==0 ) THEN
!
!     COPY - PCA
!
            DO j = 1 , Nload
               CALL fwdrec(*1700,v1i)
            ENDDO
            CALL cyct2b(v1i,scr3,Nload,Iz,mcb1)
         ENDIF
      ENDIF
!
!     NOW COPY ONTO PS
!
      IF ( ityp/=0 .OR. iflag==0 ) THEN
         CALL cyct2b(v1i,scr4,Nload,Iz,mcb2)
         IF ( iflag==0 ) THEN
            IF ( ityp/=0 ) THEN
               CALL rewind(v1i)
               CALL fwdrec(*1700,v1i)
               nlps = nskip + Nload
               DO j = 1 , nlps
                  CALL fwdrec(*1700,v1i)
               ENDDO
               Itc = -mcb(5)
               CALL cyct2b(v1i,scr4,Nload,Iz,mcb2)
               Itc = mcb(5)
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
!     DONE WITH COPY
!
 1200 CALL close(v1i,1)
   CALL close(scr3,1)
   CALL close(scr4,1)
   CALL wrttrl(mcb1)
   CALL wrttrl(mcb2)
   IF ( iflag/=0 .AND. ityp==0 ) THEN
!
!     NO GS
!
      CALL ssg2b(scr1,scr3,0,v1o,1,Iprec,1,scr5)
   ELSE
      CALL ssg2b(scr1,scr3,0,scr5,1,Iprec,1,v1o)
      CALL ssg2b(scr2,scr4,scr5,v1o,1,Iprec,1,scr3)
   ENDIF
 1300 DO
!
!     TRANSFORM EIGENVECTORS FORWARD
!
      IF ( v1o==v2o ) RETURN
      mcb(1) = v2i
      CALL rdtrl(mcb)
      IF ( mcb(1)<=0 ) RETURN
      Itc = mcb(5)
      IF ( Itc==4 .AND. nz<4*lua ) CALL mesage(-8,0,name)
      IF ( mod(mcb(2),2)/=2 ) CALL mesage(-7,0,name)
      IF ( iflag/=1 .OR. ityp/=0 ) THEN
         CALL gopen(v2i,Iz(ibuf1),0)
         CALL gopen(scr3,Iz(ibuf2),1)
         CALL gopen(scr4,Iz(ibuf3),1)
         ncopy = mcb(2)
         CALL makmcb(mcb1,scr3,lua,2,mcb(5))
         CALL makmcb(mcb2,scr4,lua,2,mcb(5))
         DO i = 1 , ncopy
            file = scr3
            ip = 1
            IF ( mod(i,2)==0 ) ip = 8
            IF ( mod(i,2)==0 ) file = scr4
            CALL cyct2b(v2i,file,1,Iz,mcb(ip))
         ENDDO
         v1o = v2o
         v1i = v2i
         GOTO 1200
      ELSE
!
!     IN = OUT
!
         v1o = v2o
         scr3 = v1i
         CALL ssg2b(scr1,scr3,0,v1o,1,Iprec,1,scr5)
      ENDIF
   ENDDO
 1400 CALL gopen(v1o,Iz(ibuf1),1)
   CALL close(v1o,2)
   CALL makmcb(mcb,v1o,lua,2,mcb(5))
   CALL wrttrl(mcb)
 1500 IF ( ityp==0 ) THEN
!
!     DO ROTATIONAL OR SPECIAL CASE DIH
!
      scr3 = v1i
!
!     DISTRIBUTE UX1 AND UX2 FOR MULTIPLYS
!
   ELSEIF ( iflag==1 ) THEN
      scr3 = v1i
   ELSE
      CALL makmcb(mcb1,scr3,iks,2,mcb(5))
      CALL makmcb(mcb2,scr4,iks,2,mcb(5))
      CALL gopen(v1i,Iz(ibuf1),0)
      CALL gopen(scr3,Iz(ibuf2),1)
      CALL gopen(scr4,Iz(ibuf3),1)
      CALL cyct2b(v1i,scr3,Nload,Iz(1),mcb1)
      CALL cyct2b(v1i,scr4,Nload,Iz(1),mcb2)
      CALL close(scr3,1)
      CALL wrttrl(mcb1)
      CALL close(scr4,1)
      CALL wrttrl(mcb2)
      CALL close(v1i,1)
   ENDIF
!
!     COMPUTE UCS
!
   CALL ssg2b(scr1,scr3,0,scr5,0,Iprec,1,scr6)
   CALL gopen(v1o,Iz(ibuf1),3)
   CALL gopen(scr5,Iz(ibuf2),0)
   mcb(1) = v1o
   CALL rdtrl(mcb(1))
   CALL cyct2b(scr5,v1o,Nload,Iz(1),mcb)
   IF ( ityp/=0 .OR. iflag==0 ) THEN
      CALL close(v1o,2)
      CALL close(scr5,1)
      IF ( ityp/=0 .AND. iflag==0 ) THEN
!
!     COMPUTE UCA
!
         CALL ssg2b(scr2,scr4,0,scr5,0,Iprec,0,scr6)
         CALL gopen(v1o,Iz(ibuf1),3)
         CALL gopen(scr5,Iz(ibuf2),0)
         CALL cyct2b(scr5,v1o,Nload,Iz(1),mcb)
         CALL close(v1o,2)
         CALL close(scr5,1)
!
!     COMPUTE USS
!
         CALL ssg2b(scr1,scr4,0,scr5,0,Iprec,1,scr6)
         CALL gopen(v1o,Iz(ibuf1),3)
         CALL gopen(scr5,Iz(ibuf2),0)
         CALL cyct2b(scr5,v1o,Nload,Iz(1),mcb)
         CALL close(scr5,1)
         CALL close(v1o,2)
      ENDIF
!
!     COMPUTE USA
!
      CALL ssg2b(scr2,scr3,0,scr5,0,Iprec,1,scr6)
      CALL gopen(v1o,Iz(ibuf1),3)
      CALL gopen(scr5,Iz(ibuf2),0)
      CALL cyct2b(scr5,v1o,Nload,Iz(1),mcb)
   ENDIF
   CALL close(scr5,1)
   CALL close(v1o,1)
   CALL wrttrl(mcb)
!
!     SEE IF DONE
!
 1600 mcb(1) = v2i
   CALL rdtrl(mcb)
   IF ( mcb(1)<=0 ) RETURN
   scr3 = 303
   Itc = mcb(5)
   IF ( Itc==4 .AND. nz<4*lua ) CALL mesage(-8,0,name)
!
!     NOW DO EIGENVECTORS
!
!
!     COMPUTE NEW VECTORS
!
   CALL ssg2b(scr1,v2i,0,scr3,0,Iprec,1,scr5)
   IF ( ityp/=0 .OR. iflag/=1 ) CALL ssg2b(scr2,v2i,0,scr4,0,Iprec,1,scr5)
!
!     POSITION FILES
!
!
!      SET LAMA FLAG
!
   mcb(1) = lamx
   CALL rdtrl(mcb)
   ilama = 0
   IF ( mcb(1)<=0 ) ilama = 1
   CALL gopen(v2o,Iz(ibuf1),1)
   IF ( ilama==0 ) THEN
      CALL gopen(lama,Iz(ibuf2),1)
      file = lamx
      CALL gopen(lamx,Iz(ibuf3),0)
      CALL read(*1700,*1800,lamx,Iz(1),146,1,iflag)
      CALL write(lama,Iz(1),146,1)
   ENDIF
   mcb(1) = v2i
   CALL rdtrl(mcb)
   Nload = mcb(2)
   CALL makmcb(mcb,v2o,lua,2,mcb(5))
   ibuf4 = ibuf3 - Sysbuf
   CALL gopen(scr3,Iz(ibuf4),0)
   IF ( ityp/=0 .OR. iflag/=1 ) THEN
      ibuf5 = ibuf4 - Sysbuf
      CALL gopen(scr4,Iz(ibuf5),0)
   ENDIF
   DO i = 1 , Nload
      CALL cyct2b(scr3,v2o,1,Iz(1),mcb)
      IF ( ilama==0 ) THEN
         CALL read(*1700,*1800,lamx,Iz(1),7,0,iflag)
         CALL write(lama,Iz(1),7,0)
      ENDIF
      IF ( ityp/=0 .OR. iflag/=1 ) THEN
         IF ( ilama==0 ) CALL write(lama,Iz(1),7,0)
         CALL cyct2b(scr4,v2o,1,Iz(1),mcb)
      ENDIF
   ENDDO
   CALL wrttrl(mcb)
   CALL close(v2o,1)
   CALL close(scr3,1)
   CALL close(scr4,1)
   IF ( ilama==0 ) THEN
      CALL close(lama,1)
      CALL close(lamx,1)
      mcb(1) = lama
      CALL wrttrl(mcb)
   ENDIF
!
!     DONE
!
   RETURN
 1700 ip1 = -2
!
!     ERROR MESSAGES
!
! 600 IP1 = -1
   CALL mesage(ip1,file,name)
   GOTO 1900
 1800 ip1 = -3
   CALL mesage(ip1,file,name)
 1900 CALL mesage(7,0,name)
   Nogo = -1
END SUBROUTINE cyct2
