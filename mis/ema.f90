
SUBROUTINE ema
   IMPLICIT NONE
   INTEGER Buf(100) , Buf1 , Buf2 , Buf3 , Cls , Clsrew , Col , Col1 , Coln , Gpewds , High , I , Icol , Icolx , Idict , Ielem ,    &
         & Igpx , Ihalf , Ihq(180) , Ii , Ilist , Imat , Imatn , Ipiez , Ipvt , Iq , Irow , Irowp , Irowx , Ivpt , J , Jhalf , Jj , &
         & Jnext , K , K4flag , Kelem , Kk , Kshift , Ksystm(100) , L , L1 , L2 , Lcore , Ldict , Lhpw(5) , Low , M , Mach , Maxii ,&
         & Maxipv , Maxn , Maxnpr , N , Nbrcol , Nbrrow , Nbrwds , Ncol , Ngps , Ngrids , Nhdr , Nlist , Nlocs , Nmat , Nok4 ,      &
         & Npvt , Nread , Nrec , Nrow , Nrowp , Nsca , Nwds , Oldcod , Op , Output , Prec , Q(4) , Rd , Rdrew , Scalas(32)
   DOUBLE PRECISION D(18) , Zd(1)
   INTEGER Scrin , Scrout , Sysbuf , Union , Wrt , Wrtrew , Z(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   REAL Wtmass , Y(1)
   COMMON /blank / Nok4 , Wtmass
   COMMON /lhpwx / Lhpw , Kshift
   COMMON /ma1xx / Buf , Buf1 , Buf2 , Buf3 , Col , Coln , Col1 , Gpewds , High , I , Icol , Icolx , Idict , Ielem , Igpx , Ilist , &
                 & Imat , Imatn , Ipvt , Irow , Irowp , Irowx , Jj , K , Kelem , Kk , Ii , K4flag , L , J , Lcore , Ldict , Low ,   &
                 & L1 , L2 , M , Maxii , Maxipv , Maxn , Maxnpr , Jnext , N , Nbrcol , Nbrrow , Nbrwds , Ncol , Ngps , Ngrids ,     &
                 & Nhdr , Nlist , Nlocs , Nmat , Npvt , Nread , Nrec , Nrow , Nrowp , Nsca , Nwds , Oldcod , Op , Prec , Scrin ,    &
                 & Scrout , Union , Scalas
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zblpkx/ Q , Iq
   COMMON /zzzzzz/ Z
   INTEGER andf , korsz , lshift , orf , rshift
   REAL eps , factor , xs(2) , yj
   LOGICAL first , last , piez
   INTEGER gpect , gpectx , hdr(6) , icode , id , indx , is(2) , itab , jdict , jjlast , kfact , kons , ksft , lbuf , ma1h(2) ,     &
         & mask , maxblk , maxdof , maxel , maxgpe , mcb(7) , mdict , msg(4) , nsil , ntab , nxtcol , openr , openw , ppoint ,      &
         & scr1 , scr2 , xblock , xemd , xgg
   DOUBLE PRECISION xd(1) , xdd
   EXTERNAL andf , lshift , orf , rshift
!
!     DMAP SEQUENCE
!
!     EMA    GPECT,XEMD,XBLOCK/XGG/C,N,NOK4/C,N,WTMASS $
!
!            WHERE NOK4 .NE. -1 TO BUILD K4GG (USE DAMPING FACTOR),
!                       .EQ. -1 TO IGNORE DAMPING FACTOR
!
!     EMA USES TWO SCRATCH FILES
!
!WKBI 1/95
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Output) , (Ksystm(78),Ipiez)
   EQUIVALENCE (Z(1),Zd(1)) , (xs(1),xd(1),is(1)) , (Z(1),Y(1)) , (Buf(1),D(1)) , (Ipvt,Ivpt) , (Buf(1),Ihq(1))
   DATA lbuf/100/ , mcb/7*0/ , ma1h/4HEMA  , 2H  / , kons/14/ , mdict/6/ , hdr/6*0/
   DATA gpect , xemd , xblock/101 , 102 , 103/ , xgg/201/ , scr1 , scr2/301 , 302/
!
!
!     RE-SET KONS IF HALF WORD IS LARGER THAN THAN 16 BITS
!
   IF ( Ihalf>=18 ) kons = 16
   IF ( Ihalf>=30 ) kons = 24
!
!     ALLOCATE BUFFERS. OPEN GPECT AND ALLOCATE A TABLE OF 4 WORDS
!     PER ELEMENT TYPE. OPEN SCRATCH FILE FOR GPECTX. OPEN XEMD.
!
   mcb(1) = 0
   mcb(2) = 0
   mcb(3) = 0
   mcb(4) = 0
   mcb(5) = 0
   mcb(6) = 0
   mcb(7) = 0
   Maxii = 2**kons - 1
   maxblk = 0
   ksft = Kshift
   mask = lshift(Jhalf,Ihalf)
   Buf1 = korsz(Z) - Sysbuf
   Buf2 = Buf1 - Sysbuf
   Buf3 = Buf2 - Sysbuf
   Lcore = Buf1 + Sysbuf - 1
   K4flag = 1
   IF ( Nok4==-1 ) K4flag = 0
!
!     SET LOGICAL VARIABLE TRUE IF THIS IS A PIEZOELECRRIC COUPLED
!     PROBLEM AND STRUCTURAL DAMPING FLAG IS ON
!
   piez = .FALSE.
   IF ( Ipiez==1 .AND. K4flag/=0 ) piez = .TRUE.
   Buf(1) = xblock
   CALL rdtrl(Buf)
   IF ( Buf(1)<0 ) THEN
      msg(3) = xblock
      msg(4) = 1001
      GOTO 2900
   ELSE
      Prec = Buf(2)
      Buf(1) = gpect
      CALL rdtrl(Buf)
      IF ( Buf(1)<0 ) THEN
         msg(3) = gpect
         msg(4) = 1002
         GOTO 2900
      ELSE
         Idict = 4*Buf(2) + 1
         nsil = Buf(3)
         maxel = Buf(4)
         maxdof = Buf(5)
         CALL gopen(gpect,Z(Buf1),Rdrew)
         L = (2**(Ihalf+Ihalf-2)-1)*2 + 1
         Maxipv = rshift(L,kons)
         Scrin = gpect
         Scrout = scr1
         CALL gopen(xemd,Z(Buf3),Rdrew)
         CALL open(*3200,Scrout,Z(Buf2),Wrtrew)
         CALL write(Scrout,Buf,3,1)
!
!     SET SWITCHES FOR MULTIPLICATION BY DAMPING
!     OR WEIGHT MASS FACTOR (OR BOTH)
!
         eps = abs(Wtmass-1.0)
         IF ( eps<1.E-6 .AND. K4flag==0 ) ASSIGN 1914 TO kfact
         IF ( eps<1.E-6 .AND. K4flag/=0 ) ASSIGN 1906 TO kfact
         IF ( eps>1.E-6 .AND. K4flag==0 ) ASSIGN 1908 TO kfact
         IF ( eps>1.E-6 .AND. K4flag/=0 ) ASSIGN 1910 TO kfact
!
!     FILL CORE WITH ELEMENT MATRIX DICTIONARIES. FOR EACH ELEMENT TYPE
!     STORE POINTER TO 1ST DICT AND THE NBR OF DICTS IN TABLE AT TOP OF
!     CORE ALSO STORE LENGTH OF EACH DICTIONARY AND FORMAT CODE.
!
         L = Idict
         DO I = 1 , Idict
            Z(I) = 0
         ENDDO
         Maxn = 0
         last = .TRUE.
      ENDIF
   ENDIF
 100  CALL read(*400,*3300,xemd,hdr,3,0,Nread)
   Ielem = 4*hdr(1) - 3
   Ldict = hdr(2)
   Z(Ielem) = L
   Z(Ielem+2) = Ldict
   Z(Ielem+3) = hdr(3)
 200  DO WHILE ( L+Ldict<Buf3 )
      jdict = Ldict
      CALL read(*3400,*300,xemd,Z(L),Ldict,0,Nread)
      L = L + Ldict
   ENDDO
   last = .FALSE.
   Z(Ielem+1) = (L-Z(Ielem))/Ldict
   High = Z(L-jdict)
   IF ( Z(Ielem+1)==0 ) Z(Ielem) = 0
   GOTO 500
 300  IF ( Nread/=0 ) THEN
!
!     FATAL ERROR MESSAGES
!
      msg(1) = 1016
      GOTO 2700
   ELSE
      High = Z(L-Ldict)
      Z(Ielem+1) = (L-Z(Ielem))/Ldict
      GOTO 100
   ENDIF
 400  last = .TRUE.
   CALL close(xemd,Clsrew)
!
!     PASS GPECT (OR PARTIALLY COMPLETED GPECTX) ENTRY BY ENTRY.
!     IF ENTRY HAS BEEN COMPLETED, COPY IT OUT.  OTHERWISE, LOCATE
!     DICTIONARY FOR ELEMENT (IF IN CORE) AND ATTACH IT.
!     DETERMINE LENGTH OF LONGEST RECORD IN GPECTX.
!     DETERMINE THE MAXIMUM LENGTH OF ONE COLUMN OF AN ELEMENT MATRIX.
!
 500  Nhdr = 2
   IF ( last ) Nhdr = 5
   maxgpe = 0
   Low = Z(Idict)
   Ngps = 0
!
!     READ AND WRITE HEADER FOR RECORD (SIL, DOF, ETC.)
!
 600  CALL read(*1100,*3500,Scrin,hdr,2,0,Nread)
   CALL write(Scrout,hdr,Nhdr,0)
   Gpewds = Nhdr
   Ngps = Ngps + 1
 700  DO
!
!     READ FIRST WORD  OF ENTRY ON GPECT. TEST FOR DICT ALREADY ATTACHED
!
      CALL read(*3600,*1000,Scrin,Buf,1,0,Nread)
      IF ( iabs(Buf(1))>lbuf ) THEN
         msg(1) = 1035
         GOTO 2700
      ELSE
         IF ( Buf(1)<0 ) THEN
!
!     DICTIONARY NOT ATTACHED---TRY TO LOCATE DICT IN CORE
!
            M = -Buf(1)
            CALL read(*3900,*4000,Scrin,Buf(2),M,0,Nread)
            IF ( Buf(2)<Low ) CYCLE
            IF ( Buf(2)>High ) THEN
               IF ( last ) CYCLE
               N = M + 1
            ELSE
               Kelem = 4*Buf(3) - 3
               IF ( Z(Kelem)==0 ) CYCLE
               L = Z(Kelem)
               N = Z(Kelem+1)
               Ldict = Z(Kelem+2)
               Nlocs = Z(Kelem+3)
               CALL bisloc(*700,Buf(2),Z(L),Ldict,N,K)
               K = K + L - 1
               IF ( K4flag/=0 .AND. Z(K+4)==0 ) CYCLE
!
!     DICTIONARY LOCATED---WRITE OUT COMPLETED ENTRY ON GPECTX
!         0             NO. OF WORDS IN ENTRY (NOT INCL THIS WORD)
!       1 - 5           ELEM ID, F, N, C, GE
!         6             LOC OF ELEMENT MATRIX COLUMNS FOR CURRENT PIVOT
!       7 - 6+NGRIDS    SIL-S OF CONNECTED GRID POINTS
!
               Ngrids = M - 2
               indx = K + mdict - 1
               IF ( Nlocs==1 ) GOTO 900
               IF ( Ngrids>Nlocs ) THEN
                  msg(1) = 1052
                  GOTO 2700
               ELSE
                  Kk = 1
                  DO I = 1 , Ngrids
                     IF ( Kk/=1 ) THEN
!
!     CHECK FOR DUPLICATE SILS - E.G. HBDY ELEMENT WITH AMBIENT PTS
!
                        DO WHILE ( Buf(Kk+3)==Buf(Kk+2) )
                           Kk = Kk + 1
                        ENDDO
                     ENDIF
                     IF ( Buf(Kk+3)==hdr(1) ) THEN
!
!     SIL THAT MATCHES THE PIVOT FOUND.  NOW INSURE THAT THIS SIL
!     HAS NOT BEEN ALREADY CONNECTED DUE TO A PREVIOUS ENTRY IN THIS
!     GPECT RECORD.  (CAUSED BY DUPLICATE IDS I.E. CELAS2)
!
!     GINO-LOC WILL NOW BE ZERO IF THAT IS TRUE
!
                        indx = K + mdict + I - 2
                        IF ( Z(indx)/=0 ) GOTO 800
                     ENDIF
                     Kk = Kk + 1
                  ENDDO
                  CYCLE
               ENDIF
            ENDIF
         ELSE
!
!     DICTIONARY ALREADY ATTACHED---READ REMAINDER OF ENTRY AND
!     COPY ENTRY TO GPECTX.
!
            CALL read(*3700,*3800,Scrin,Buf(2),Buf(1),0,Nread)
            N = Buf(1) + 1
         ENDIF
         CALL write(Scrout,Buf,N,0)
         Gpewds = Gpewds + N
      ENDIF
   ENDDO
 800  Z(K+mdict-1) = Z(indx)
   IF ( Z(K+1)==2 ) THEN
      Buf(4) = Buf(I+3)
      Ngrids = 1
   ENDIF
 900  IF ( Ldict-Nlocs+1/=mdict ) THEN
      msg(1) = 1056
      GOTO 2700
   ELSE
      N = mdict + Ngrids
      CALL write(Scrout,N,1,0)
      CALL write(Scrout,Z(K),mdict,0)
      maxblk = max0(Z(indx)/ksft,maxblk)
!
!     ZERO GINO-LOC AS HAVING BEEN USED NOW.
!
      Z(indx) = 0
      CALL write(Scrout,Buf(4),Ngrids,0)
      Maxn = max0(Maxn,Z(K+2))
      Gpewds = Gpewds + N + 1
      GOTO 700
   ENDIF
!
!     HERE ON END-OF-RECORD ON GPECT
!
 1000 CALL write(Scrout,0,0,1)
   maxgpe = max0(maxgpe,Gpewds)
   GOTO 600
!
!     HERE ON END-OF-FILE ON GPECT---TEST FOR COMPLETION OF GPECTX
!
 1100 CALL close(Scrin,Clsrew)
   CALL close(Scrout,Clsrew)
   IF ( Ngps/=nsil ) THEN
      msg(1) = 1110
      GOTO 2700
   ELSEIF ( last ) THEN
!
!     HERE WE GO NOW FOR THE ASSEMBLY PHASE---PREPARE BY ALLOCATING
!     STORAGE FOR ONE ELEMENT MATRIX COLUMN AND ITS ROW POSITIONS
!
      Irowp = Prec*Maxn + 1
      Igpx = Irowp + Maxn
      first = .TRUE.
      gpectx = Scrout
      mcb(1) = xgg
      mcb(4) = 6
      mcb(5) = Prec
      mcb(6) = 0
      mcb(7) = 0
      last = .FALSE.
      Nrec = 0
      Maxnpr = Maxn*Prec
      openr = Rdrew
      openw = Wrtrew
      Oldcod = 0
      itab = Buf1 - maxblk
      ntab = Buf1 - 1
      IF ( itab<Igpx ) THEN
         msg(1) = 1202
         GOTO 2700
      ENDIF
   ELSE
!
!     GPECTX NOT COMPLETE---SWITCH FILES AND MAKE ANOTHER PASS
!
      IF ( Scrin==gpect ) Scrin = scr2
      K = Scrin
      Scrin = Scrout
      Scrout = K
      CALL gopen(Scrin,Z(Buf1),Rdrew)
      CALL gopen(Scrout,Z(Buf2),Wrtrew)
      L = Idict
      Ldict = Z(Ielem+2)
      Nlocs = Z(Ielem+3)
      DO I = 1 , Idict
         Z(I) = 0
      ENDDO
      last = .TRUE.
      Z(Ielem) = Idict
      Z(Ielem+2) = Ldict
      Z(Ielem+3) = Nlocs
      GOTO 200
   ENDIF
!
!     BEGIN A PASS - OPEN GPECTX
!
 1200 Ipvt = Igpx
   Jj = itab - 3
   DO indx = itab , ntab
      Z(indx) = 0
   ENDDO
   CALL gopen(gpectx,Z(Buf1),openr)
!
!     READ A RECORD FROM GPECTX INTO CORE
!
 1300 IF ( Ivpt+maxgpe>=Jj .OR. Ipvt>Maxipv ) GOTO 1800
   CALL read(*4100,*1400,gpectx,Z(Ivpt),maxgpe+1,1,Nread)
   msg(1) = 1220
   GOTO 2700
 1400 Icol = Ipvt + Nread
   Nrec = Nrec + 1
!
!     MAKE A PASS THROUGH EACH ELEMENT CONNECTED TO THE PIVOT---FORM THE
!     UNION OF ALL CODE WORDS AND STORE ELEMENT POINTERS IN LIST AT THE
!     END OF OPEN CORE
!
   ppoint = lshift(Ipvt,kons)
   Ii = Ipvt + 5
   Union = 0
   Z(Ipvt+2) = 0
   Z(Ipvt+3) = 0
   Z(Ipvt+4) = Ipvt
   DO WHILE ( Ii<Icol )
      IF ( Z(Ii)<0 ) THEN
         msg(1) = 1231
         GOTO 2700
      ELSE
         Kk = Ii - Ipvt
         IF ( Kk>Maxii ) THEN
            msg(1) = 1232
            GOTO 2700
         ELSE
            IF ( Jj<=Icol ) GOTO 1700
            Z(Jj) = Z(Ii+mdict)
            Z(Jj+1) = orf(ppoint,Kk)
            Z(Jj+2) = 0
            indx = itab + Z(Jj)/ksft - 1
            IF ( indx>ntab ) THEN
               msg(1) = 1238
               GOTO 2700
            ELSE
               IF ( Z(indx)/=0 ) THEN
                  jjlast = itab - andf(Z(indx),Jhalf)
                  Z(jjlast+2) = Jj
                  Z(indx) = orf(andf(Z(indx),mask),itab-Jj)
               ELSE
                  Z(indx) = orf(lshift(itab-Jj,Ihalf),itab-Jj)
               ENDIF
               Jj = Jj - 3
               Union = orf(Union,Z(Ii+4))
               Ii = Ii + Z(Ii) + 1
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   IF ( Ii/=Icol ) THEN
      msg(1) = 1235
      GOTO 2700
   ELSE
!
!     FORM THE LIST OF NON-NULL COLUMNS TO BE BUILT FOR THIS PIVOT
!
      IF ( Union==0 ) GOTO 1600
      IF ( Union/=Oldcod ) THEN
         CALL decode(Union,Scalas,Nsca)
         Oldcod = Union
      ENDIF
      Z(Ipvt+2) = Icol
      IF ( Icol+Nsca>=Jj ) GOTO 1700
      Ii = Icol
      DO L = 1 , Nsca
         Z(Ii) = Z(Ipvt) + Scalas(L)
         Ii = Ii + 1
      ENDDO
      Irow = Ii
!
!     NOW MAKE A PASS AGAIN THROUGH EACH ELEMENT CONNECTED TO CURRENT
!     PIVOT AND FORM A LIST OF UNIQUE ROW INDICES.
!
      Ii = Ipvt + 5
   ENDIF
 1500 L1 = Ii + mdict + 1
   L2 = Ii + Z(Ii)
   IF ( Oldcod/=Z(Ii+4) ) THEN
      icode = Z(Ii+4)
      CALL decode(icode,Scalas,Nsca)
      Oldcod = Z(Ii+4)
   ENDIF
   Kk = Irow
   IF ( Ii/=Ipvt+5 ) THEN
      J = Irowp
      DO L = L1 , L2
!
!     IGNORE DUPLICATE IDS AS IN SOME CELAS2 ELEMENTS ETC.
!
         IF ( L<=L1 .OR. Z(L)/=Z(L-1) ) THEN
            DO I = 1 , Nsca
               Z(J) = Z(L) + Scalas(I)
               J = J + 1
            ENDDO
         ENDIF
      ENDDO
      IF ( J>Igpx ) THEN
         msg(1) = 1264
         GOTO 2700
      ELSE
         M = J - Irowp
         IF ( Irow+Nbrwds+M>=Jj ) GOTO 1700
         CALL mrge(Z(Irow),Nbrwds,Z(Irowp),M)
         Nrow = Irow + Nbrwds - 1
         IF ( Nrow>=Jj ) GOTO 1700
      ENDIF
   ELSE
      DO L = L1 , L2
!
!     IGNORE DUPLICATE IDS AS IN SOME CELAS2 ELEMENTS ETC.
!
         IF ( L<=L1 .OR. Z(L)/=Z(L-1) ) THEN
            DO I = 1 , Nsca
               Z(Kk) = Z(L) + Scalas(I)
               Kk = Kk + 1
               IF ( Kk>=Jj ) GOTO 1700
            ENDDO
         ENDIF
      ENDDO
      Nrow = Kk - 1
      Nbrwds = Kk - Irow
   ENDIF
   Ii = L2 + 1
   IF ( Ii<Icol ) GOTO 1500
   Z(Ipvt+3) = Irow
!
!     NOW ALLOCATE STORAGE FOR COLUMNS OF XGG ASSOCIATED WITH THIS PIVOT
!
   Imat = Nrow + 1
   Nbrcol = Irow - Icol
   Nbrrow = Imat - Irow
   Nbrwds = Prec*Nbrcol*Nbrrow
   Nmat = Imat + Nbrwds - 1
   IF ( Nmat>=Jj ) GOTO 1700
   DO I = Imat , Nmat
      Z(I) = 0
   ENDDO
   Z(Ipvt+4) = Imat
   Ii = Nmat + 1
!
!     ADVANCE POINTER AND TRY TO GET ANOTHER PIVOT ALLOCATED
!
 1600 Ilist = Jj + 3
   Npvt = Ipvt
   IF ( Nrec==Ngps ) THEN
!
!     HERE WHEN LAST PIVOT POINT HAS BEEN READ AND ALLOCATED
!
      last = .TRUE.
      Op = Clsrew
      GOTO 1900
   ELSE
      Ipvt = Ii
      GOTO 1300
   ENDIF
!
!     HERE WHEN STORAGE EXCEEDED DURING PROCESSING OF A PIVOT.
!     IF FIRST PIVOT ON PASS, INSUFFICIENT CORE FOR MODULE.
!     OTHERWISE, BACKSPACE GPECTX AND PREPARE TO PROCESS ALL
!     PIVOTS IN CORE WHICH HAVE BEEN COMPLETELY ALLOCATED.
!
 1700 IF ( Ipvt==Igpx ) CALL mesage(-8,0,ma1h)
   CALL bckrec(gpectx)
   Nrec = Nrec - 1
 1800 Op = Cls
   IF ( Ipvt==Igpx ) CALL mesage(-8,0,ma1h)
!
!     CLOSE GPECTX. OPEN XBLOCK.
!
 1900 CALL close(gpectx,Op)
   Nwds = Buf1 - Ilist
   IF ( Nwds>0 ) THEN
      CALL gopen(xblock,Z(Buf1),Rdrew)
      Oldcod = 0
!
!     PASS THE LIST OF ELEMENT MATRIX POINTERS. EACH ENTRY POINTS TO THE
!     PIVOT POINT AND ELEMENT DICTIONARY IN CORE AND TO THE POSITION IN
!     THE XBLOCK FILE CONTAINING THE ASSOCIATED ELEMENT MATRIX COLUMNS.
!     WHEN PROCESSING OF ALL ENTRIES IS COMPLETE, COLUMNS OF XGG NOW IN
!     CORE ARE COMPLETE.
!
      DO indx = itab , ntab
         IF ( Z(indx)/=0 ) THEN
            Jj = itab - rshift(Z(indx),Ihalf)
            IF ( Jj>=Ilist ) THEN
               DO
                  CALL filpos(xblock,Z(Jj))
                  Ipvt = rshift(Z(Jj+1),kons)
                  IF ( Ipvt>Npvt ) GOTO 2500
                  Ielem = Ipvt + andf(Z(Jj+1),Maxii)
                  Icol = Z(Ipvt+2)
                  Irow = Z(Ipvt+3)
                  Imat = Z(Ipvt+4)
!
!     DECODE CODE WORD FOR ELEMENT. FORM LIST OF ROW INDICES DESCRIBING
!     TERMS IN THE ELEMENT MATRIX COLUMN. THEN CONVERT THESE INDICES TO
!     RELATIVE ADDRESSES IN XGG COLUMN IN CORE (USE LIST OF ROW INDICES
!     FOR XGG COLUMN TO DO THIS).
!
                  IF ( Z(Ielem+4)/=Oldcod ) THEN
                     icode = Z(Ielem+4)
                     CALL decode(icode,Scalas,Nsca)
                     Oldcod = Z(Ielem+4)
                  ENDIF
                  L1 = Ielem + mdict + 1
                  L2 = Ielem + Z(Ielem)
                  K = Irowp
                  DO L = L1 , L2
!
!     IGNORE DUPLICATE IDS AS IN SOME CELAS2 ELEMENTS ETC.
!
                     IF ( L<=L1 .OR. Z(L)/=Z(L-1) ) THEN
                        DO I = 1 , Nsca
                           Z(K) = Z(L) + Scalas(I)
                           K = K + 1
                        ENDDO
                     ENDIF
                  ENDDO
                  Nrowp = K - 1
                  IF ( Nrowp>=Igpx ) GOTO 2100
                  Nrow = Imat - 1
                  id = Z(Irowp)
                  CALL bisloc(*2600,id,Z(Irow),1,(Imat-Irow),Irowx)
                  Irowx = Irow + Irowx - 1
                  DO K = Irowp , Nrowp
                     DO I = Irowx , Nrow
                        IF ( Z(K)==Z(I) ) GOTO 1902
                     ENDDO
                     GOTO 2200
 1902                Z(K) = (I-Irow)*Prec
                     Irowx = I + 1
                  ENDDO
                  Nbrrow = Nrowp - Irowp + 1
!
!     PREPARE TO READ EACH COLUMN OF ELEMENT MATRIX
!
                  Ncol = Irow - 1
                  Icolx = Icol
                  Nbrwds = Z(Ielem+3)*Prec
                  IF ( Z(Ielem+2)==2 ) Nbrwds = Prec
                  IF ( Nbrwds>Maxnpr ) GOTO 2300
                  DO I = 1 , Nsca
!
!     READ A COLUMN OF THE ELEMENT MATRIX AND DETERMINE ADDRESS
!     OF FIRST WORD OF ASSOCIATED COLUMN OF XGG IN CORE.
!
                     CALL read(*4200,*4300,xblock,Z,Nbrwds,0,Nread)
                     Col = Z(Ipvt) + Scalas(I)
                     DO K = Icolx , Ncol
                        IF ( Col==Z(K) ) GOTO 1904
                     ENDDO
                     GOTO 2400
 1904                Imatn = Imat + (Imat-Irow)*(K-Icol)*Prec
                     Icolx = K + 1
                     IF ( Z(Ielem+2)==2 ) THEN
!
!     ELEMENT MATRIX IS DIAGONAL
!
                        Nbrrow = 1
                        Z(Irowp) = Z(Irowp+I-1)
                     ENDIF
!
!     IF DAMPING OR WEIGHT MASS FACTOR (OR BOTH) PRESENT, MULTIPLY
!     EACH TERM IN THE ELEMENT MATRIX COLUMN BY THE FACTOR.
!
                     GOTO kfact
 1906                factor = Y(Ielem+5)
                     GOTO 1912
 1908                factor = Wtmass
                     GOTO 1912
 1910                factor = Y(Ielem+5)*Wtmass
 1912                IF ( Prec==2 ) THEN
                        M = Nbrwds/2
                        DO K = 1 , M
                           IF ( piez .AND. (I==Nsca .OR. mod(K,4)==0) ) Zd(K) = 0.D0
                           Zd(K) = factor*Zd(K)
                        ENDDO
                        GOTO 1918
                     ELSE
                        DO K = 1 , Nbrwds
!
!     FOR PIEZOELECTRIC COUPLED PROBLEMS, ANY STRUCTURAL DAMPING COEFF.
!     SHOULD MULTIPLY ONLY THE UNCOUPLED STRUCTURAL TERMS. SO, SKIP
!     EVERY 4TH TERM IN A COLUMN AND SKIP EVERY 4TH COLUMN
!
                           IF ( piez .AND. (I==Nsca .OR. mod(K,4)==0) ) Y(K) = 0.
                           Y(K) = factor*Y(K)
                        ENDDO
                        GOTO 1916
                     ENDIF
!
!     NOW ADD TERMS OF THE ELEMENT MATRIX INTO XGG
!
 1914                IF ( Prec==2 ) GOTO 1918
!
!     DO ARITHMETIC IN SINGLE PRECISION
!
 1916                DO K = 1 , Nbrrow
                        J = Imatn + Z(Irowp+K-1)
!WKBI 1/95
                        yj = Y(J)
                        Y(J) = Y(J) + Y(K)
!WKBNB 1/95  FOLLOWING CODE WILL CAUSE A TRUE ZERO WHEN SUBTRACTING SAME NO.
                        IF ( Y(K)/=0.0 ) THEN
                           yj = yj/Y(K)
                           IF ( yj<=-.999999999998 .AND. yj>=-1.000000000001 ) Y(J) = 0.0
                        ENDIF
!WKBNE 1/95
                     ENDDO
                     CYCLE
!
!     DO ARITHMETIC IN DOUBLE PRECISION
!
 1918                DO K = 1 , Nbrrow
                        J = Imatn + Z(Irowp+K-1)
                        is(1) = Z(J)
                        is(2) = Z(J+1)
!WKBI 1/95
                        xdd = xd(1)
                        xd(1) = xd(1) + Zd(K)
!WKBNB 1/95 FOLLOWING CODE WILL CAUSE A TRUE ZERO WHEN SUBTRACTING SAME NO.
!  WITHOUT THIS CODE, A SYMMETRIC MATRIX MIGHT HAVE UNSYMMETRIC TERMS ON
!  THE HP AND ULTRIX (SEE DEMO D01011A, MATRIX KAA, COLUMN 84 ROW 70)
                        IF ( Zd(K)/=0.0D0 ) THEN
                           xdd = xdd/Zd(K)
                           IF ( xdd<=-.999999999998 .AND. xdd>=-1.000000000001 ) xd(1) = 0.0D0
                        ENDIF
!WKBNE 1/95
                        Z(J) = is(1)
                        Z(J+1) = is(2)
!
                     ENDDO
!
!     END OF DO LOOPS
!
                  ENDDO
                  Jj = Z(Jj+2)
                  IF ( Jj<Ilist ) EXIT
               ENDDO
            ENDIF
         ENDIF
      ENDDO
!
!     ALL COLUMNS OF XGG IN CORE ARE NOW COMPLETE - SEND THEM
!     OUT TO THE XGG DATA BLOCK VIA THE BLDPK ROUTINE.
!
      CALL close(xblock,Clsrew)
   ENDIF
   CALL gopen(xgg,Z(Buf1),openw)
   Ipvt = Igpx
!
!     PREPARE TO PACK ALL COLUMNS FOR CURRENT PIVOT
!
 2000 Col1 = Z(Ipvt)
   Coln = Col1 + Z(Ipvt+1) - 1
   Icol = Z(Ipvt+2)
   Irow = Z(Ipvt+3)
   Imat = Z(Ipvt+4)
   Ncol = Irow - 1
   Nrow = Imat - 1
   Jnext = Icol
   nxtcol = Z(Jnext)
   Ii = Imat
   DO Col = Col1 , Coln
!
!     INITIATE PACKING BY CALLING BLDPK. TEST FOR NULL COL.
!
      CALL bldpk(Prec,Prec,xgg,0,0)
      IF ( Icol/=0 ) THEN
         IF ( Col>=nxtcol ) THEN
            Jnext = Jnext + 1
            nxtcol = Z(Jnext)
            IF ( Jnext>Ncol ) nxtcol = Coln + 1
            IF ( Prec==2 ) THEN
!
!     DOUBLE PRECISION
!
               DO K = Irow , Nrow
                  Iq = Z(K)
                  Q(1) = Z(Ii)
                  Q(2) = Z(Ii+1)
                  CALL zblpki
                  Ii = Ii + 2
               ENDDO
            ELSE
!
!     NON-NULL COLUMN - SEND THE TERMS OUT VIA ZBLPKI
!
!     SINGLE PRECISION
!
               DO K = Irow , Nrow
                  Iq = Z(K)
                  Q(1) = Z(Ii)
                  CALL zblpki
                  Ii = Ii + 1
               ENDDO
            ENDIF
         ENDIF
      ENDIF
!
!     TERMINATE COLUMN BY CALLING BLDPKN
!
      CALL bldpkn(xgg,0,mcb)
   ENDDO
!
!     LOGIC TEST TO MAKE SURE POINTERS ENDED CORRECTLY
!
   Nbrwds = 5
   IF ( Icol/=0 ) THEN
      Nbrwds = (Imat-Irow)*(Irow-Icol)*Prec
      IF ( Ii-Imat/=Nbrwds .AND. Z(Ipvt+1)/=1 ) THEN
         msg(1) = 1432
         GOTO 2700
      ENDIF
   ENDIF
!
!     TEST FOR LAST PIVOT
!
   IF ( Ipvt>=Npvt ) THEN
!
!     CLOSE XGG
!
      CALL close(xgg,Op)
!
!     TEST FOR LAST PASS
!
      IF ( last ) THEN
!
!     XGG NOW COMPLETE - WRITE ITS TRAILER.
!
         mcb(3) = mcb(2)
         IF ( mcb(2)/=Z(Npvt)+Z(Npvt+1)-1 ) THEN
            msg(1) = 1490
            GOTO 2700
         ELSE
            CALL wrttrl(mcb)
            RETURN
         ENDIF
      ELSE
         first = .FALSE.
         openr = Rd
         openw = Wrt
         GOTO 1200
      ENDIF
   ELSE
      Ipvt = Imat + Nbrwds
      GOTO 2000
   ENDIF
 2100 msg(1) = 1344
   GOTO 2700
 2200 msg(1) = 1346
   GOTO 2700
 2300 msg(1) = 1352
   GOTO 2700
 2400 msg(1) = 1362
   GOTO 2700
 2500 msg(1) = 1332
   GOTO 2700
 2600 msg(1) = 1345
 2700 WRITE (Output,99001) Sfm , msg(1)
99001 FORMAT (A25,' 3102, LOGIC ERROR EMA - ',I4)
 2800 WRITE (Output,99002)
99002 FORMAT (/,' *** CONTENTS OF /MA1XX/')
   WRITE (Output,99007) Ihq
   WRITE (Output,99003)
99003 FORMAT (/,' FIRST 250 WORDS OF OPEN CORE')
   J = 250
   WRITE (Output,99007) (Z(I),I=1,J)
   CALL mesage(-61,0,0)
 2900 CALL fname(msg(3),msg(1))
   WRITE (Output,99004) Sfm , (msg(I),I=1,4)
99004 FORMAT (A25,' 3001, ATTEMPT TO OPEN DATA SET ',2A4,', FILE (',I4,') IN SUBROUTINE EMA (',I4,                                  &
            & ') WHICH WAS NOT DEFINED IN FIST.')
   GOTO 2800
 3000 CALL fname(msg(3),msg(1))
   WRITE (Output,99005) Sfm , (msg(I),I=1,4)
99005 FORMAT (A25,' 3002, EOF ENCOUNTERED WHILE READING DATA SET ',2A4,', (FILE',I5,') IN SUBROUTINE EMA (',I4,1H))
   GOTO 2800
 3100 CALL fname(msg(3),msg(1))
   WRITE (Output,99006) Sfm , (msg(I),I=1,4)
99006 FORMAT (A25,' 3003, ATTEMPT TO READ PAST END OF LOGICAL RECORD IN',' DATA SET ',2A4,' (FILE',I5,') IN SUBROUTINE EMA (',I4,   &
            & 1H))
   GOTO 2800
 3200 msg(3) = Scrout
   msg(4) = 1005
   GOTO 2900
 3300 msg(3) = xemd
   msg(4) = 1014
   GOTO 3100
 3400 msg(3) = xemd
   msg(4) = 1017
   GOTO 3000
 3500 msg(3) = Scrin
   msg(4) = 1032
   GOTO 3100
 3600 msg(3) = Scrin
   msg(4) = 1035
   GOTO 3000
 3700 msg(3) = Scrin
   msg(4) = 1036
   GOTO 3000
 3800 msg(3) = Scrin
   msg(4) = 1036
   GOTO 3100
 3900 msg(3) = Scrin
   msg(4) = 1040
   GOTO 3000
 4000 msg(3) = Scrin
   msg(4) = 1040
   GOTO 3100
 4100 msg(3) = gpectx
   msg(4) = 1221
   GOTO 3000
 4200 msg(3) = xblock
   msg(4) = 1360
   GOTO 3000
 4300 msg(3) = xblock
   msg(4) = 1360
   GOTO 3100
99007 FORMAT (5X,10I10)
END SUBROUTINE ema
