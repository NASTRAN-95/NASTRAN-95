!*==ema.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ema
USE C_BLANK
USE C_LHPWX
USE C_MA1XX
USE C_MACHIN
USE C_NAMES
USE C_SYSTEM
USE C_XMSSG
USE C_ZBLPKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(18) :: d
   REAL :: eps , factor , yj
   LOGICAL :: first , last , piez
   INTEGER , SAVE :: gpect , kons , lbuf , mdict , scr1 , scr2 , xblock , xemd , xgg
   INTEGER :: gpectx , icode , id , indx , ipiez , itab , ivpt , jdict , jjlast , kfact , ksft , mask , maxblk , maxdof , maxel ,   &
            & maxgpe , nsil , ntab , nxtcol , openr , openw , output , ppoint , sysbuf
   INTEGER , DIMENSION(6) , SAVE :: hdr
   INTEGER , DIMENSION(180) :: ihq
   INTEGER , DIMENSION(2) :: is
   INTEGER , DIMENSION(2) , SAVE :: ma1h
   INTEGER , DIMENSION(7) , SAVE :: mcb
   INTEGER , DIMENSION(4) :: msg
   REAL(REAL64) , DIMENSION(1) :: xd , zd
   REAL(REAL64) :: xdd
   REAL , DIMENSION(2) :: xs
   REAL , DIMENSION(1) :: y
   EXTERNAL andf , bckrec , bisloc , bldpk , bldpkn , close , decode , filpos , fname , gopen , korsz , lshift , mesage , mrge ,    &
          & open , orf , rdtrl , read , rshift , write , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
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
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Output) , (Ksystm(78),Ipiez)
   !>>>>EQUIVALENCE (Z(1),Zd(1)) , (xs(1),xd(1),is(1)) , (Z(1),Y(1)) , (Buf(1),D(1)) , (Ipvt,Ivpt) , (Buf(1),Ihq(1))
   DATA lbuf/100/ , mcb/7*0/ , ma1h/4HEMA  , 2H  / , kons/14/ , mdict/6/ , hdr/6*0/
   DATA gpect , xemd , xblock/101 , 102 , 103/ , xgg/201/ , scr1 , scr2/301 , 302/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         Buf1 = korsz(Z) - sysbuf
         Buf2 = Buf1 - sysbuf
         Buf3 = Buf2 - sysbuf
         Lcore = Buf1 + sysbuf - 1
         K4flag = 1
         IF ( Nok4==-1 ) K4flag = 0
!
!     SET LOGICAL VARIABLE TRUE IF THIS IS A PIEZOELECRRIC COUPLED
!     PROBLEM AND STRUCTURAL DAMPING FLAG IS ON
!
         piez = .FALSE.
         IF ( ipiez==1 .AND. K4flag/=0 ) piez = .TRUE.
         Buf(1) = xblock
         CALL rdtrl(Buf)
         IF ( Buf(1)<0 ) THEN
            msg(3) = xblock
            msg(4) = 1001
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ELSE
            Prec = Buf(2)
            Buf(1) = gpect
            CALL rdtrl(Buf)
            IF ( Buf(1)<0 ) THEN
               msg(3) = gpect
               msg(4) = 1002
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
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
               CALL open(*160,Scrout,Z(Buf2),Wrtrew)
               CALL write(Scrout,Buf,3,1)
!
!     SET SWITCHES FOR MULTIPLICATION BY DAMPING
!     OR WEIGHT MASS FACTOR (OR BOTH)
!
               eps = abs(Wtmass-1.0)
               IF ( eps<1.E-6 .AND. K4flag==0 ) ASSIGN 128 TO kfact
               IF ( eps<1.E-6 .AND. K4flag/=0 ) ASSIGN 122 TO kfact
               IF ( eps>1.E-6 .AND. K4flag==0 ) ASSIGN 124 TO kfact
               IF ( eps>1.E-6 .AND. K4flag/=0 ) ASSIGN 126 TO kfact
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
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*40,*180,xemd,hdr,3,0,Nread)
         Ielem = 4*hdr(1) - 3
         Ldict = hdr(2)
         Z(Ielem) = L
         Z(Ielem+2) = Ldict
         Z(Ielem+3) = hdr(3)
         spag_nextblock_1 = 3
      CASE (3)
         DO WHILE ( L+Ldict<Buf3 )
            jdict = Ldict
            CALL read(*200,*20,xemd,Z(L),Ldict,0,Nread)
            L = L + Ldict
         ENDDO
         last = .FALSE.
         Z(Ielem+1) = (L-Z(Ielem))/Ldict
         High = Z(L-jdict)
         IF ( Z(Ielem+1)==0 ) Z(Ielem) = 0
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 20      IF ( Nread/=0 ) THEN
!
!     FATAL ERROR MESSAGES
!
            msg(1) = 1016
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ELSE
            High = Z(L-Ldict)
            Z(Ielem+1) = (L-Z(Ielem))/Ldict
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      last = .TRUE.
         CALL close(xemd,Clsrew)
         spag_nextblock_1 = 4
      CASE (4)
!
!     PASS GPECT (OR PARTIALLY COMPLETED GPECTX) ENTRY BY ENTRY.
!     IF ENTRY HAS BEEN COMPLETED, COPY IT OUT.  OTHERWISE, LOCATE
!     DICTIONARY FOR ELEMENT (IF IN CORE) AND ATTACH IT.
!     DETERMINE LENGTH OF LONGEST RECORD IN GPECTX.
!     DETERMINE THE MAXIMUM LENGTH OF ONE COLUMN OF AN ELEMENT MATRIX.
!
         Nhdr = 2
         IF ( last ) Nhdr = 5
         maxgpe = 0
         Low = Z(Idict)
         Ngps = 0
         spag_nextblock_1 = 5
      CASE (5)
!
!     READ AND WRITE HEADER FOR RECORD (SIL, DOF, ETC.)
!
         CALL read(*100,*220,Scrin,hdr,2,0,Nread)
         CALL write(Scrout,hdr,Nhdr,0)
         Gpewds = Nhdr
         Ngps = Ngps + 1
 60      SPAG_Loop_1_1: DO
!
!     READ FIRST WORD  OF ENTRY ON GPECT. TEST FOR DICT ALREADY ATTACHED
!
            CALL read(*240,*80,Scrin,Buf,1,0,Nread)
            IF ( iabs(Buf(1))>lbuf ) THEN
               msg(1) = 1035
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( Buf(1)<0 ) THEN
!
!     DICTIONARY NOT ATTACHED---TRY TO LOCATE DICT IN CORE
!
                  M = -Buf(1)
                  CALL read(*300,*320,Scrin,Buf(2),M,0,Nread)
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
                     CALL bisloc(*60,Buf(2),Z(L),Ldict,N,K)
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
                     IF ( Nlocs==1 ) THEN
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( Ngrids>Nlocs ) THEN
                        msg(1) = 1052
                        spag_nextblock_1 = 19
                        CYCLE SPAG_DispatchLoop_1
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
                              IF ( Z(indx)/=0 ) EXIT SPAG_Loop_1_1
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
                  CALL read(*260,*280,Scrin,Buf(2),Buf(1),0,Nread)
                  N = Buf(1) + 1
               ENDIF
               CALL write(Scrout,Buf,N,0)
               Gpewds = Gpewds + N
            ENDIF
         ENDDO SPAG_Loop_1_1
         Z(K+mdict-1) = Z(indx)
         IF ( Z(K+1)==2 ) THEN
            Buf(4) = Buf(I+3)
            Ngrids = 1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         IF ( Ldict-Nlocs+1/=mdict ) THEN
            msg(1) = 1056
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
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
            GOTO 60
         ENDIF
!
!     HERE ON END-OF-RECORD ON GPECT
!
 80      CALL write(Scrout,0,0,1)
         maxgpe = max0(maxgpe,Gpewds)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     HERE ON END-OF-FILE ON GPECT---TEST FOR COMPLETION OF GPECTX
!
 100     CALL close(Scrin,Clsrew)
         CALL close(Scrout,Clsrew)
         IF ( Ngps/=nsil ) THEN
            msg(1) = 1110
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
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
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
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
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     BEGIN A PASS - OPEN GPECTX
!
         Ipvt = Igpx
         Jj = itab - 3
         DO indx = itab , ntab
            Z(indx) = 0
         ENDDO
         CALL gopen(gpectx,Z(Buf1),openr)
         spag_nextblock_1 = 8
      CASE (8)
!
!     READ A RECORD FROM GPECTX INTO CORE
!
         IF ( ivpt+maxgpe>=Jj .OR. Ipvt>Maxipv ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL read(*340,*120,gpectx,Z(ivpt),maxgpe+1,1,Nread)
         msg(1) = 1220
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
 120     Icol = Ipvt + Nread
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
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ELSE
               Kk = Ii - Ipvt
               IF ( Kk>Maxii ) THEN
                  msg(1) = 1232
                  spag_nextblock_1 = 19
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  IF ( Jj<=Icol ) THEN
                     spag_nextblock_1 = 11
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  Z(Jj) = Z(Ii+mdict)
                  Z(Jj+1) = orf(ppoint,Kk)
                  Z(Jj+2) = 0
                  indx = itab + Z(Jj)/ksft - 1
                  IF ( indx>ntab ) THEN
                     msg(1) = 1238
                     spag_nextblock_1 = 19
                     CYCLE SPAG_DispatchLoop_1
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
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     FORM THE LIST OF NON-NULL COLUMNS TO BE BUILT FOR THIS PIVOT
!
            IF ( Union==0 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Union/=Oldcod ) THEN
               CALL decode(Union,Scalas,Nsca)
               Oldcod = Union
            ENDIF
            Z(Ipvt+2) = Icol
            IF ( Icol+Nsca>=Jj ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
         spag_nextblock_1 = 9
      CASE (9)
         L1 = Ii + mdict + 1
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
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ELSE
               M = J - Irowp
               IF ( Irow+Nbrwds+M>=Jj ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL mrge(Z(Irow),Nbrwds,Z(Irowp),M)
               Nrow = Irow + Nbrwds - 1
               IF ( Nrow>=Jj ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
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
                     IF ( Kk>=Jj ) THEN
                        spag_nextblock_1 = 11
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
            Nrow = Kk - 1
            Nbrwds = Kk - Irow
         ENDIF
         Ii = L2 + 1
         IF ( Ii<Icol ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Z(Ipvt+3) = Irow
!
!     NOW ALLOCATE STORAGE FOR COLUMNS OF XGG ASSOCIATED WITH THIS PIVOT
!
         Imat = Nrow + 1
         Nbrcol = Irow - Icol
         Nbrrow = Imat - Irow
         Nbrwds = Prec*Nbrcol*Nbrrow
         Nmat = Imat + Nbrwds - 1
         IF ( Nmat>=Jj ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO I = Imat , Nmat
            Z(I) = 0
         ENDDO
         Z(Ipvt+4) = Imat
         Ii = Nmat + 1
         spag_nextblock_1 = 10
      CASE (10)
!
!     ADVANCE POINTER AND TRY TO GET ANOTHER PIVOT ALLOCATED
!
         Ilist = Jj + 3
         Npvt = Ipvt
         IF ( Nrec==Ngps ) THEN
!
!     HERE WHEN LAST PIVOT POINT HAS BEEN READ AND ALLOCATED
!
            last = .TRUE.
            Op = Clsrew
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ELSE
            Ipvt = Ii
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (11)
!
!     HERE WHEN STORAGE EXCEEDED DURING PROCESSING OF A PIVOT.
!     IF FIRST PIVOT ON PASS, INSUFFICIENT CORE FOR MODULE.
!     OTHERWISE, BACKSPACE GPECTX AND PREPARE TO PROCESS ALL
!     PIVOTS IN CORE WHICH HAVE BEEN COMPLETELY ALLOCATED.
!
         IF ( Ipvt==Igpx ) CALL mesage(-8,0,ma1h)
         CALL bckrec(gpectx)
         Nrec = Nrec - 1
         spag_nextblock_1 = 12
      CASE (12)
         Op = Cls
         IF ( Ipvt==Igpx ) CALL mesage(-8,0,ma1h)
         spag_nextblock_1 = 13
      CASE (13)
!
!     CLOSE GPECTX. OPEN XBLOCK.
!
         CALL close(gpectx,Op)
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
                     SPAG_Loop_2_2: DO
                        CALL filpos(xblock,Z(Jj))
                        Ipvt = rshift(Z(Jj+1),kons)
                        IF ( Ipvt>Npvt ) THEN
                           spag_nextblock_1 = 18
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
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
                        IF ( Nrowp>=Igpx ) THEN
                           spag_nextblock_1 = 14
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        Nrow = Imat - 1
                        id = Z(Irowp)
                        CALL bisloc(*140,id,Z(Irow),1,(Imat-Irow),Irowx)
                        Irowx = Irow + Irowx - 1
                        DO K = Irowp , Nrowp
                           spag_nextblock_2 = 1
                           SPAG_DispatchLoop_2: DO
                              SELECT CASE (spag_nextblock_2)
                              CASE (1)
                                 DO I = Irowx , Nrow
                                    IF ( Z(K)==Z(I) ) THEN
                                       spag_nextblock_2 = 2
                                       CYCLE SPAG_DispatchLoop_2
                                    ENDIF
                                 ENDDO
                                 spag_nextblock_1 = 15
                                 CYCLE SPAG_DispatchLoop_1
                              CASE (2)
                                 Z(K) = (I-Irow)*Prec
                                 Irowx = I + 1
                                 EXIT SPAG_DispatchLoop_2
                              END SELECT
                           ENDDO SPAG_DispatchLoop_2
                        ENDDO
                        Nbrrow = Nrowp - Irowp + 1
!
!     PREPARE TO READ EACH COLUMN OF ELEMENT MATRIX
!
                        Ncol = Irow - 1
                        Icolx = Icol
                        Nbrwds = Z(Ielem+3)*Prec
                        IF ( Z(Ielem+2)==2 ) Nbrwds = Prec
                        IF ( Nbrwds>Maxnpr ) THEN
                           spag_nextblock_1 = 16
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        DO I = 1 , Nsca
                           spag_nextblock_3 = 1
                           SPAG_DispatchLoop_3: DO
                              SELECT CASE (spag_nextblock_3)
                              CASE (1)
!
!     READ A COLUMN OF THE ELEMENT MATRIX AND DETERMINE ADDRESS
!     OF FIRST WORD OF ASSOCIATED COLUMN OF XGG IN CORE.
!
                                 CALL read(*360,*380,xblock,Z,Nbrwds,0,Nread)
                                 Col = Z(Ipvt) + Scalas(I)
                                 DO K = Icolx , Ncol
                                    IF ( Col==Z(K) ) THEN
                                       spag_nextblock_3 = 2
                                       CYCLE SPAG_DispatchLoop_3
                                    ENDIF
                                 ENDDO
                                 spag_nextblock_1 = 17
                                 CYCLE SPAG_DispatchLoop_1
                              CASE (2)
                                 Imatn = Imat + (Imat-Irow)*(K-Icol)*Prec
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
 122                             factor = y(Ielem+5)
                                 spag_nextblock_3 = 3
                                 CYCLE SPAG_DispatchLoop_3
 124                             factor = Wtmass
                                 spag_nextblock_3 = 3
                                 CYCLE SPAG_DispatchLoop_3
 126                             factor = y(Ielem+5)*Wtmass
                                 spag_nextblock_3 = 3
                              CASE (3)
                                 IF ( Prec==2 ) THEN
                                    M = Nbrwds/2
                                    DO K = 1 , M
                                       IF ( piez .AND. (I==Nsca .OR. mod(K,4)==0) ) zd(K) = 0.D0
                                       zd(K) = factor*zd(K)
                                    ENDDO
                                    spag_nextblock_3 = 5
                                    CYCLE SPAG_DispatchLoop_3
                                 ELSE
                                    DO K = 1 , Nbrwds
!
!     FOR PIEZOELECTRIC COUPLED PROBLEMS, ANY STRUCTURAL DAMPING COEFF.
!     SHOULD MULTIPLY ONLY THE UNCOUPLED STRUCTURAL TERMS. SO, SKIP
!     EVERY 4TH TERM IN A COLUMN AND SKIP EVERY 4TH COLUMN
!
                                       IF ( piez .AND. (I==Nsca .OR. mod(K,4)==0) ) y(K) = 0.
                                       y(K) = factor*y(K)
                                    ENDDO
                                    spag_nextblock_3 = 4
                                    CYCLE SPAG_DispatchLoop_3
                                 ENDIF
!
!     NOW ADD TERMS OF THE ELEMENT MATRIX INTO XGG
!
 128                             IF ( Prec==2 ) THEN
                                    spag_nextblock_3 = 5
                                    CYCLE SPAG_DispatchLoop_3
                                 ENDIF
                                 spag_nextblock_3 = 4
                              CASE (4)
!
!     DO ARITHMETIC IN SINGLE PRECISION
!
                                 DO K = 1 , Nbrrow
                                    J = Imatn + Z(Irowp+K-1)
!WKBI 1/95
                                    yj = y(J)
                                    y(J) = y(J) + y(K)
!WKBNB 1/95  FOLLOWING CODE WILL CAUSE A TRUE ZERO WHEN SUBTRACTING SAME NO.
                                    IF ( y(K)/=0.0 ) THEN
                                       yj = yj/y(K)
                                       IF ( yj<=-.999999999998 .AND. yj>=-1.000000000001 ) y(J) = 0.0
                                    ENDIF
!WKBNE 1/95
                                 ENDDO
                                 CYCLE
                              CASE (5)
!
!     DO ARITHMETIC IN DOUBLE PRECISION
!
                                 DO K = 1 , Nbrrow
                                    J = Imatn + Z(Irowp+K-1)
                                    is(1) = Z(J)
                                    is(2) = Z(J+1)
!WKBI 1/95
                                    xdd = xd(1)
                                    xd(1) = xd(1) + zd(K)
!WKBNB 1/95 FOLLOWING CODE WILL CAUSE A TRUE ZERO WHEN SUBTRACTING SAME NO.
!  WITHOUT THIS CODE, A SYMMETRIC MATRIX MIGHT HAVE UNSYMMETRIC TERMS ON
!  THE HP AND ULTRIX (SEE DEMO D01011A, MATRIX KAA, COLUMN 84 ROW 70)
                                    IF ( zd(K)/=0.0D0 ) THEN
                                       xdd = xdd/zd(K)
                                       IF ( xdd<=-.999999999998 .AND. xdd>=-1.000000000001 ) xd(1) = 0.0D0
                                    ENDIF
!WKBNE 1/95
                                    Z(J) = is(1)
                                    Z(J+1) = is(2)
!
                                 ENDDO
                                 EXIT SPAG_DispatchLoop_3
                              END SELECT
                           ENDDO SPAG_DispatchLoop_3
!
!     END OF DO LOOPS
!
                        ENDDO
                        Jj = Z(Jj+2)
                        IF ( Jj<Ilist ) EXIT SPAG_Loop_2_2
                     ENDDO SPAG_Loop_2_2
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
         DO
!
!     PREPARE TO PACK ALL COLUMNS FOR CURRENT PIVOT
!
            Col1 = Z(Ipvt)
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
                  spag_nextblock_1 = 19
                  CYCLE SPAG_DispatchLoop_1
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
                     spag_nextblock_1 = 19
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     CALL wrttrl(mcb)
                     RETURN
                  ENDIF
               ELSE
                  first = .FALSE.
                  openr = Rd
                  openw = Wrt
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               Ipvt = Imat + Nbrwds
            ENDIF
         ENDDO
         spag_nextblock_1 = 14
      CASE (14)
         msg(1) = 1344
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
      CASE (15)
         msg(1) = 1346
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
      CASE (16)
         msg(1) = 1352
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
      CASE (17)
         msg(1) = 1362
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
      CASE (18)
         msg(1) = 1332
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
 140     msg(1) = 1345
         spag_nextblock_1 = 19
      CASE (19)
         WRITE (output,99001) Sfm , msg(1)
99001    FORMAT (A25,' 3102, LOGIC ERROR EMA - ',I4)
         spag_nextblock_1 = 20
      CASE (20)
         WRITE (output,99002)
99002    FORMAT (/,' *** CONTENTS OF /MA1XX/')
         WRITE (output,99007) ihq
         WRITE (output,99003)
99003    FORMAT (/,' FIRST 250 WORDS OF OPEN CORE')
         J = 250
         WRITE (output,99007) (Z(I),I=1,J)
         CALL mesage(-61,0,0)
         spag_nextblock_1 = 21
      CASE (21)
         CALL fname(msg(3),msg(1))
         WRITE (output,99004) Sfm , (msg(I),I=1,4)
99004    FORMAT (A25,' 3001, ATTEMPT TO OPEN DATA SET ',2A4,', FILE (',I4,') IN SUBROUTINE EMA (',I4,                               &
                &') WHICH WAS NOT DEFINED IN FIST.')
         spag_nextblock_1 = 20
         CYCLE SPAG_DispatchLoop_1
      CASE (22)
         CALL fname(msg(3),msg(1))
         WRITE (output,99005) Sfm , (msg(I),I=1,4)
99005    FORMAT (A25,' 3002, EOF ENCOUNTERED WHILE READING DATA SET ',2A4,', (FILE',I5,') IN SUBROUTINE EMA (',I4,1H))
         spag_nextblock_1 = 20
         CYCLE SPAG_DispatchLoop_1
      CASE (23)
         CALL fname(msg(3),msg(1))
         WRITE (output,99006) Sfm , (msg(I),I=1,4)
99006    FORMAT (A25,' 3003, ATTEMPT TO READ PAST END OF LOGICAL RECORD IN',' DATA SET ',2A4,' (FILE',I5,') IN SUBROUTINE EMA (',I4,&
                &1H))
         spag_nextblock_1 = 20
         CYCLE SPAG_DispatchLoop_1
 160     msg(3) = Scrout
         msg(4) = 1005
         spag_nextblock_1 = 21
         CYCLE SPAG_DispatchLoop_1
 180     msg(3) = xemd
         msg(4) = 1014
         spag_nextblock_1 = 23
         CYCLE SPAG_DispatchLoop_1
 200     msg(3) = xemd
         msg(4) = 1017
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 220     msg(3) = Scrin
         msg(4) = 1032
         spag_nextblock_1 = 23
         CYCLE SPAG_DispatchLoop_1
 240     msg(3) = Scrin
         msg(4) = 1035
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 260     msg(3) = Scrin
         msg(4) = 1036
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 280     msg(3) = Scrin
         msg(4) = 1036
         spag_nextblock_1 = 23
         CYCLE SPAG_DispatchLoop_1
 300     msg(3) = Scrin
         msg(4) = 1040
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 320     msg(3) = Scrin
         msg(4) = 1040
         spag_nextblock_1 = 23
         CYCLE SPAG_DispatchLoop_1
 340     msg(3) = gpectx
         msg(4) = 1221
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 360     msg(3) = xblock
         msg(4) = 1360
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 380     msg(3) = xblock
         msg(4) = 1360
         spag_nextblock_1 = 23
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99007 FORMAT (5X,10I10)
END SUBROUTINE ema
