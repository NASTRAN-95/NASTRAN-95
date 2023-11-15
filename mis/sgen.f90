
SUBROUTINE sgen
!
!     THIS MODULE PREPARES THE INPUT FILES TO NASTRAN FROM A SUBSTRUCTUR
!     FORMULATION IN ORDER TO RUN THE SOLUTION PHASE OF NASTRAN.
!     3 MAJOR STEPS ARE-
!
!     1.  READ CONSTRAINT AND DYNAMICS DATA, CONVERT TO PSEUDO-STRUCTURE
!         DATA, AND OUTPUT ON GP4S AND DYNS.
!
!     2.  READ LOAD COMBO. DATA AND ASSEMBLE SCALAR LOAD SETS ON OUTPUT
!         FILE GP3S.
!
!     3.  BUILD DUMMY FILES FOR EXECUTION- CASEI, GPL, EQEXIN, GPDT,
!         BGPDT, CSTM, AND SIL.
!
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf1 , Buf2 , Buf3 , Dry , Ibuf , Incr , Iptr , Irow , Ity , Luset , Name(2) , Nogpdt , Nono , Nrow , Nss , Nz , Outt ,  &
         & Two(2) , Z(4)
   REAL Rz(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Dry , Name , Luset , Nogpdt
   COMMON /sgencm/ Nono , Nss , Iptr , Buf1 , Buf2 , Buf3 , Nz
   COMMON /system/ Ibuf , Outt
   COMMON /two   / Two
   COMMON /unpakx/ Ity , Irow , Nrow , Incr
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Rz
!
! Local variable declarations
!
   INTEGER andf , complf , korsz , orf
   INTEGER bgpdt , buf4 , casec , casei , cases , code , ctypeo(2,8) , ctypes(2,8) , dareas(2) , delays(2) , dphses(2) , dynam ,    &
         & dyns , eqex , eqss , file , flag , geom3 , geom4 , gp3s , gp4s , gpdt , gpl , i , ibs1 , ibs2 , ibs3 , ic , icode(4,9) , &
         & icomp(32) , idx , ifl , ig , igr , igrd , ild , inam , ip , ipt , irec , is , iss1 , isub , item , itest , iz , izl , j ,&
         & jg , jpt , large , ldcd , lid , lload(3) , loadc(2) , lods , lsload(3) , ltab(4,9) , lvec , mcb(7) , minus(3) , mpcs(2) ,&
         & n , nc , ncasec(2) , ngrd , nj , nl , nlimit(3) , nodyn , nog4 , nptr , nset , nsgen(2) , nsil , nsild , ntypec ,        &
         & ntyped , nvec , nwds
   REAL fact , rtemp(10) , rtemp2(10)
   LOGICAL nolc , nols , psuedo , stest
   INTEGER nzb , pvec , scrt , scrt2 , sidc , sids , sil , spcs(2) , spcs1(2) , spcsd(2) , temp(10) , temp2(10) , tics(2) , type(2) &
         & , xxxx
   EXTERNAL andf , complf , orf
!
! End of declarations
!
   EQUIVALENCE (Rz(1),Z(1)) , (temp(1),rtemp(1)) , (temp2(1),rtemp2(1)) , (ctypes(1,1),mpcs(1)) , (ctypes(1,2),spcs(1)) ,           &
    & (ctypes(1,3),spcs1(1)) , (ctypes(1,4),spcsd(1)) , (ctypes(1,5),dareas(1)) , (ctypes(1,6),delays(1)) , (ctypes(1,7),dphses(1)) &
    & , (ctypes(1,8),tics(1))
   DATA minus , nlimit/3* - 1 , 3*2147483647/ , eqss/4HEQSS/ , lods/4HLODS/
   DATA casec , geom3 , geom4 , dynam/101 , 102 , 103 , 104/ , cases , casei , gpl , eqex , gpdt/201 , 202 , 203 , 204 , 205/ ,     &
      & bgpdt , sil , gp3s , gp4s , dyns/206 , 207 , 208 , 209 , 210/ , scrt , scrt2/201 , 202/
   DATA pvec/4HPVEC/ , nsgen/4HSGEN , 4H    /
!
!     BULK DATA CARD CODES
!
!             MPCS
!             SPCS
!             SPCS1
!             SPCSD
!             LOADC
!             DAREAS
!             DELAYS
!             DPHASES
!             TICS
   DATA icode/1110 , 11 , 0 , 0 , 810 , 8 , 0 , 0 , 710 , 7 , 0 , 0 , 610 , 6 , 0 , 0 , 500 , 5 , 0 , 0 , 9027 , 90 , 0 , 0 , 9137 ,&
      & 91 , 0 , 0 , 9277 , 92 , 0 , 0 , 9307 , 93 , 0 , 0/
!
   DATA ntypec/4/
   DATA ntyped/4/
!             MPC
!             SPC
!             SPC1
!             SPCD
!             LOADC
!             DAREA
!             DELAY
!             DPHASE
!             TIC
   DATA ltab/4901 , 49 , 17 , 1 , 5501 , 55 , 16 , 2 , 5481 , 58 , 12 , 3 , 5110 , 51 , 256 , 4 , 500 , 5 , 264 , 0 , 27 , 17 ,     &
      & 182 , 5 , 37 , 18 , 183 , 6 , 77 , 19 , 184 , 7 , 6607 , 66 , 137 , 8/
   DATA lload/4551 , 61 , 84/ , lsload/5401 , 54 , 25/
   DATA mpcs/4HMPCS , 4H    / , spcs/4HSPCS , 4H    / , spcs1/4HSPCS , 4H1   / , spcsd/4HSPCS , 4HD   / , loadc/4HLOAD , 4HC   / ,  &
      & dareas/4HDARE , 4HAS  / , delays/4HDELA , 4HYS  / , dphses/4HDPHA , 4HSES / , tics/4HTICS , 4H    /
   DATA ncasec/4HCASE , 4HCC  /
   DATA ctypeo/4HMPC  , 4H     , 4HSPC  , 4H     , 4HSPC1 , 4H     , 4HSPCD , 4H     , 4HDARE , 4HA    , 4HDELA , 4HY    , 4HDPHA , &
       &4HSE   , 4HTIC  , 4H    /
   DATA xxxx/4HXXXX/
!
!     INITIALIZE
!
   Ity = 1
   Incr = 1
   Nono = 0
   large = Two(2)
   Nz = korsz(Z(1))
   ibs1 = Nz - Ibuf + 1
   ibs2 = ibs1 - Ibuf - 1
   ibs3 = ibs2 - Ibuf
   Buf1 = ibs3 - Ibuf
   Buf2 = Buf1 - Ibuf
   Buf3 = Buf2 - Ibuf
   buf4 = Buf3 - Ibuf
   Nz = buf4 - 1
   IF ( Nz<=0 ) GOTO 4000
   IF ( Name(1)==xxxx .AND. Name(2)==xxxx ) GOTO 3400
!
!     INITIALIZE LUSET AND NOGPDT FLAGS
!
   Luset = 0
   Nogpdt = -1
!
!     FORM TABLES OF REFERENCED SID-S FOR LOAD, MPC, AND SPC
!     CASE CONTROL CARDS.
!
!
!     OPEN  SOF , GET EQSS ITEM , READ  SIL DATA INTO CORE
!
   CALL sofopn(Z(ibs1),Z(ibs2),Z(ibs3))
   CALL sfetch(Name,eqss,1,flag)
   item = eqss
   IF ( flag/=1 ) THEN
!
!     ERRORS
!
      n = 2 - flag
      GOTO 3900
   ELSE
      CALL suread(Z(1),Nz,nwds,flag)
      IF ( flag/=2 ) GOTO 4000
      Nss = Z(3)
      iz = nwds + 1
!
!     READ SIL GROUP INTO CORE
!
      CALL sjump(Nss)
      CALL suread(Z(iz),Nz-iz+1,nsil,flag)
      IF ( flag/=2 ) GOTO 4000
      ipt = iz + nsil - 2
!
!     FIND LENGTH OF VECTOR = LUSET
!
      ic = Z(ipt+1)
      CALL decode(ic,icomp,nc)
      Luset = Z(ipt) + nc - 1
      Nogpdt = Luset
!
!     READ EQSS ( G ,IP, AND C AT A TIME) AND CONVERT IP TO SIL .
!     WRITE ON SCRT
!
      is = 0
      file = scrt
      CALL gopen(scrt,Z(Buf2),1)
      CALL sfetch(Name,eqss,1,flag)
      nj = 1
      CALL sjump(nj)
      DO
!
         CALL suread(temp,3,nwds,flag)
         IF ( flag/=1 ) THEN
            is = is + 1
            CALL write(scrt,temp,0,1)
            IF ( is>=Nss ) THEN
               CALL close(scrt,1)
!
!     READ CONVERTED EQSS INTO CORE, STORE POINTERS TO THE BASIC  SUBS
!     IN  Z(IPTR) TO Z(NPTR)
!     CORE WILL CONTAIN-
!       1. 4 WORD HEADER
!       2. 2*NSS NAMES
!       3. NSS+1 POINTERS TO EACH BASIC SUBST.BLOCK
!       4. NSS BLOCKS OF G, IP, C DATA
!       5. NZB LEFT OVER
!
!
               Iptr = iz
               nptr = Iptr
               isub = Iptr + Nss + 1
               nzb = Nz - isub + 1
               file = scrt
               CALL gopen(scrt,Z(Buf2),0)
               DO i = 1 , Nss
                  Z(nptr) = isub
                  nptr = nptr + 1
                  CALL read(*4200,*2,scrt,Z(isub),nzb,1,nwds)
                  GOTO 4000
 2                isub = isub + nwds
                  nzb = nzb - nwds
                  IF ( nzb<=0 ) GOTO 4000
               ENDDO
               Z(nptr) = isub
               CALL close(scrt,1)
!
!     ***  GEOM4 DATA CONVERSION  ***
!
!          IN  - MPCS,SPCS,SPCS1,SPCSD CARDS
!          OUT - MPC ,SPC ,SPC1 ,SCPD  ON SCRT
!
               file = geom4
               nog4 = 0
               CALL preloc(*700,Z(Buf1),geom4)
               mcb(1) = geom4
               CALL rdtrl(mcb)
               CALL gopen(scrt,Z(Buf2),1)
               stest = .FALSE.
!
!     ***  MPCS CARDS  ***
!
!          IN  - NAME(2), G, C, F
!          OUT - SIL, 0, F
!
               CALL locate(*600,Z(Buf1),icode(1,1),idx)
               CALL write(scrt,icode(1,1),3,0)
               stest = .TRUE.
               icode(4,1) = 1
               type(1) = mpcs(1)
               type(2) = mpcs(2)
               ifl = 0
               lid = 0
               EXIT
            ENDIF
         ELSE
            ipt = iz + 2*temp(2) - 2
            temp(2) = Z(ipt)
            CALL write(scrt,temp,3,0)
         ENDIF
      ENDDO
   ENDIF
 100  CALL read(*4200,*500,geom4,j,1,0,nwds)
   IF ( j/=lid ) nsild = 0
   lid = j
   CALL write(scrt,j,1,0)
 200  DO
      CALL read(*4200,*500,geom4,temp,5,0,nwds)
      IF ( temp(3)==-1 ) THEN
!
!     FINISHED ONE LOGICAL CARD, WRITE -1 FLAGS
!
         CALL write(scrt,minus,3,0)
         ifl = 0
         GOTO 100
      ELSE
         IF ( temp(3)/=0 ) THEN
!
!     FIND  REQUESTED SUBSTRUCTURE
!
            DO i = 1 , Nss
               inam = 2*i + 3
               IF ( Z(inam)==temp(1) .AND. Z(inam+1)==temp(2) ) GOTO 220
            ENDDO
!
!     SUBSTRUCTURE NOT FOUND
!
            WRITE (Outt,99005) Uwm , temp(1) , temp(2) , type , Name
         ENDIF
         CYCLE
!
!     FOUND SUBSTRUCTURE NAME
!
 220     ipt = Iptr + i - 1
         igrd = Z(ipt)
         ngrd = (Z(ipt+1)-Z(ipt))/3
!
!     SEARCH FOR GRID POINT
!
         CALL bisloc(*300,temp(3),Z(igrd),3,ngrd,igr)
         ig = igr + igrd - 1
         DO WHILE ( Z(ig-3)==Z(ig) )
            IF ( ig<=igrd ) EXIT
            ig = ig - 3
         ENDDO
         DO
            code = Z(ig+2)
!
!     FIND   THE  COMPONENT
!
            CALL decode(code,icomp,nc)
            IF ( temp(4)==0 ) temp(4) = 1
            DO i = 1 , nc
               IF ( temp(4)==icomp(i)+1 ) THEN
                  ic = i
                  GOTO 400
               ENDIF
            ENDDO
            IF ( Z(ig+3)/=Z(ig) ) GOTO 300
            IF ( ig+3>=igrd+3*ngrd ) GOTO 300
            ig = ig + 3
         ENDDO
      ENDIF
   ENDDO
!
!     BAD COMPONENT
!
 300  Nono = 1
   WRITE (Outt,99001) Ufm , (temp(i),i=1,4) , type , Name
!
!     MESSAGE FORMATS
!
99001 FORMAT (A23,' 6022, SUBSTRUCTURE ',2A4,', GRID POINT',I9,', COMPONENTS',I9,1H,,/30X,'REFERENCED ON ',2A4,                     &
             &' CARD, DO NOT EXIST ON SOLUTION STRUCTURE ',2A4)
   GOTO 200
!
!     WRITE CONVERTED DATA ON SCRT
!
 400  temp(6) = Z(ig+1) + ic - 1
   temp(7) = 0
   temp(8) = temp(5)
   CALL write(scrt,temp(6),3,0)
!
!     CHECK FOR DUPLICATE DEPENDENT SIL-S
!
   IF ( ifl==0 ) THEN
      IF ( nsild/=0 ) THEN
         DO i = 1 , nsild
            IF ( Z(isub+i-1)==temp(6) ) THEN
               Nono = 1
               WRITE (Outt,99002) Ufm , j , temp(1) , temp(2) , temp(3) , temp(4)
99002          FORMAT (A23,' 6362, MPCS SET',I9,' IS ILLEGAL.',//5X,'SUBSTRUCTURE ',2A4,' GRID POINT',I9,' COMPONENT',I5,           &
                      &' SPECIFIES A NON-UNIQUE DEPENDENT DEGREE OF FREEDOM')
            ENDIF
         ENDDO
         IF ( nsild>nzb ) GOTO 4000
      ENDIF
      Z(isub+nsild) = temp(6)
      nsild = nsild + 1
      ifl = 1
   ENDIF
   GOTO 200
!
!     FINISHED ALL MPCS CARDS, WRITE EOR AND UPDATE TRAILER
!
 500  CALL write(scrt,temp,0,1)
!
!     TURN OFF MPCS BIT
!
   j = (icode(2,1)-1)/16
   i = icode(2,1) - 16*j
   mcb(j+2) = andf(complf(Two(i+16)),mcb(j+2))
!
!     TURN ON MPC BIT
!
   j = (ltab(2,1)-1)/16
   i = ltab(2,1) - 16*j
   mcb(j+2) = orf(Two(i+16),mcb(j+2))
!
!     ***  SPCS CARDS  ***
!
!          IN  - SID, NAME(2), G, C, G, C, G, C, ..., -1, -1
!          OUT - SID, SIL, 0, 0 - REPEATED FOR EACH GRID
!
 600  CALL sgena(spcs,Z(Buf1),mcb,geom4,icode(1,2),0,scrt,ltab(1,2),1)
!
!     ***  SPCS1 CARDS  ***
!
!          IN  - SID, NAME(2), C, G, G, G, ..., -1
!          OUT - SID, 0, SIL, -1 - REPEATED FOR EACH GRID
!
   CALL sgenb(spcs1,Z(Buf1),mcb,geom4,icode(1,3),0,scrt,ltab(1,3),1)
!
!     ***  SPCSD CARDS  ***
!
!          IN  - SID, NAME(2), G, C, Y, ..., -1, -1, -1
!          OUT - SID, SIL, 0, Y - REPEATED FOR EACH GRID
!
   CALL sgena(spcsd,Z(Buf1),mcb,geom4,icode(1,4),1,scrt,ltab(1,4),1)
!
!     END OF CONSTRAINT CARD CONVERSION
!
   CALL close(geom4,1)
   CALL close(scrt,1)
   mcb(1) = gp4s
   CALL wrttrl(mcb)
   GOTO 800
 700  nog4 = 1
!
!     ***  DYNAMICS DATA CONVERSION  ***
!
!          IN  - DAREAS,DELAYS,DPHASES,TICS CARDS
!          OUT - DAREA ,DELAY ,DPHASE ,TIC  ON SCRT
!
 800  file = dynam
   nodyn = 0
   CALL preloc(*900,Z(Buf1),dynam)
   mcb(1) = dynam
   CALL rdtrl(mcb)
   CALL gopen(scrt2,Z(Buf2),1)
!
!     ***  DAREAS  CARDS ***
!
!          IN  - SID, NAME(2), G, C, A, ..., -1, -1, -1
!          OUT - SID, SIL, 0, A - REPEATED FOR EACH GRID
!
   CALL sgena(dareas,Z(Buf1),mcb,dynam,icode(1,6),1,scrt2,ltab(1,6),1)
!
!     ***  DELAYS CARDS  ***
!
!          IN  - SID, NAME(2), G, C, T, ..., -1, -1, -1
!          OUT - SID, SIL, 0, T - REPEATED FOR EACH GRID
!
   CALL sgena(delays,Z(Buf1),mcb,dynam,icode(1,7),1,scrt2,ltab(1,7),1)
!
!    ***  DPHASES CARDS  ***
!
!         IN  - SID, NAME(2), G, C, TH, ..., -1, -1, -1
!         OUT - SID, SIL, 0, TH - REPEATED FOR EACH GRID
!
   CALL sgena(dphses,Z(Buf1),mcb,dynam,icode(1,8),1,scrt2,ltab(1,8),1)
!
!     ***  TICS CARDS  ***
!
!          IN  - SID, NAME(2), G, C, U, V, ..., -1, -1, -1, -1
!          OUT - SID, SIL, 0, U, V - REPEATED FOR EACH GRID
!
   CALL sgena(tics,Z(Buf1),mcb,dynam,icode(1,9),2,scrt2,ltab(1,9),2)
!
!     END OF DYNAMICS CONVERSION
!
   CALL close(dynam,1)
   CALL close(scrt2,1)
   mcb(1) = dyns
   CALL wrttrl(mcb)
   GOTO 1000
 900  nodyn = 1
!
!     MERGE CONVERTED DATA WITH EXISTING DATA - GEOM4
!
 1000 IF ( nog4/=1 ) CALL sgenm(ntypec,geom4,scrt,gp4s,icode(1,1),ltab(1,1),ctypes(1,1),ctypeo(1,1))
!
!     MERGE CONVERTED DATA WITH EXISTING DATA - DYNAMICS
!
   IF ( nodyn/=1 ) CALL sgenm(ntyped,dynam,scrt2,dyns,icode(1,6),ltab(1,6),ctypes(1,1),ctypeo(1,1))
!
!
!     ***  GEOM3 PROCESSING  ***
!
!     THE LOAD VECTORS ARE COMBINED BY THE FACTORS
!     GIVEN ON THE LOADC CARDS AND MERGED WITH SLOAD CARDS
!
   nolc = .TRUE.
   nols = .TRUE.
   CALL preloc(*1600,Z(Buf1),geom4)
   CALL locate(*1600,Z(Buf1),icode(1,5),idx)
!
!     READ FIRST GROUP OF LODS ITEM FOR SOLUTION STRUCTURE
!
   item = lods
   CALL sfetch(Name,lods,1,flag)
   IF ( flag==1 ) THEN
      CALL suread(Z(1),Nz,nwds,itest)
      IF ( itest==1 ) GOTO 4000
      IF ( itest==3 ) GOTO 3800
      Nss = Z(4)
      iss1 = 5
      nl = Z(3)
      ipt = 2*Nss + 5
      izl = 2*Nss + ipt + 2
      Z(ipt) = izl
      Z(ipt+1) = 0
      IF ( izl+Nss+nl<=Nz ) THEN
!
!     READ REMAINDER OF LODS INTO OPEN CORE AT Z(IZL)
!
         DO i = 1 , Nss
            CALL suread(Z(izl),Nz-izl+1,nwds,itest)
            IF ( itest==1 ) GOTO 4000
            IF ( itest==3 ) GOTO 3800
            izl = izl + nwds
            jg = ipt + 2*i
            Z(jg) = izl
            Z(jg+1) = Z(jg-1) + nwds - 1
         ENDDO
!
!     CORE NOW CONTAINS
!
!            WORDS                 CONTENTS
!        ------------------    -----------------------------------
!        1--(IPT-1)            HEADER GROUP
!        IPT--IPT+2*(NSS+1)    LOAD DATA POINTER, NO. OF PRIOR LOAD
!                                  VECTORS (2 WORDS PER STRUCTURE)
!        IPT+2*(NSS+1)+1 --=   NO OF LOADS + LOAD SET IDS
!                                  GROUPED BY BASIC STRUCTURE
!
!     READ LOADC DATA CARDS AND CONVERT
!
!          IN  - SET ID, FACTOR, (NAME(2),SET,FACTOR) (REPEATED)
!          OUT - SET ID, FACTOR, (VECTOR NO.,FACTOR)
!
         type(1) = loadc(1)
         type(2) = loadc(2)
         CALL open(*4100,scrt,Z(Buf2),1)
      ELSE
!
!     INSUFFICIENT CORE
!
         CALL close(geom4,1)
         GOTO 4000
      ENDIF
   ELSEIF ( flag==2 .OR. flag==4 .OR. flag==5 ) THEN
      n = 2 - flag
      GOTO 3900
   ELSE
!
!     LODS ITEM DOES NOT EXIST
!
      nolc = .TRUE.
      GOTO 1600
   ENDIF
 1100 CALL read(*4200,*1500,geom4,temp,2,0,nwds)
   CALL write(scrt,temp,2,0)
   lid = temp(1)
   nolc = .FALSE.
 1200 DO
!
!     READ AN ENTRY
!
      CALL fread(geom4,temp,4,0)
      IF ( temp(3)==-1 ) THEN
!
!     END OF LOGICAL LOADC CARD
!
         CALL write(scrt,temp,0,1)
         GOTO 1100
      ELSE
!
!     FIND SUBSTRUCTURE AND SET
!
         DO i = 1 , Nss
            inam = iss1 + 2*(i-1)
            IF ( Z(inam)==temp(1) .AND. Z(inam+1)==temp(2) ) GOTO 1300
         ENDDO
!
!     SUBSTRUCTURE NOT FOUND
!
         WRITE (Outt,99005) Uwm , temp(1) , temp(2) , type , Name
      ENDIF
   ENDDO
!
!     FOUND SUBSTRUCTURE NAME
!
 1300 jpt = ipt + 2*i - 2
!
!     POINTER TO LODS DATA FOR THIS SUBSTRUCTURE
!
   ild = Z(jpt)
!
!     NUMBER OF SETS IN LODS DATA FOR THIS SUBSTRUCTURE
!
   nset = Z(ild)
!
!     FIND LOADC SET IN LODS DATA
!
   IF ( nset/=0 ) THEN
      DO i = 1 , nset
         ip = ild + i
         IF ( Z(ip)==temp(3) ) THEN
            lvec = Z(jpt+1) + i
            GOTO 1400
         ENDIF
      ENDDO
   ENDIF
!
!     SET NOT FOUND
!
   Nono = 1
   WRITE (Outt,99003) Ufm , Name , lid , temp(3) , temp(1) , temp(2)
99003 FORMAT (A23,' 6331, SOLUTION SUBSTRUCTURE ',2A4,' - LOADC SET',I9,' REFERENCES UNDEFINED LOAD',/30X,'SET',I9,                 &
             &' OF BASIC SUBSTRUCTURE ',2A4)
   GOTO 1200
 1400 temp(1) = lvec
   temp(2) = temp(4)
   CALL write(scrt,temp,2,0)
   GOTO 1200
!
!     END OF LOADC RECORD
!
 1500 CALL close(scrt,1)
 1600 CALL close(geom4,1)
!
!     MERGE CONVERTED LOAD DATA WITH SLOAD DATA.
!
!
!     IF ANY ERRORS WERE DETECTED, SKIP LOAD COMPUTATION
!
   IF ( Nono/=0 ) GOTO 3400
   CALL gopen(gp3s,Z(buf4),1)
!
!     COPY LOAD CARDS TO GP3S
!
   CALL preloc(*1900,Z(Buf1),geom3)
   ldcd = 0
   CALL locate(*1800,Z(Buf1),lload,idx)
   ldcd = 1
   CALL write(gp3s,lload,3,0)
   DO
      CALL read(*4200,*1700,geom3,Z(1),Nz,0,nwds)
      CALL write(gp3s,Z(1),Nz,0)
   ENDDO
 1700 CALL write(gp3s,Z(1),nwds,1)
!
!     POSITION TO SLOAD CARDS
!
 1800 CALL locate(*1900,Z(Buf1),lsload,idx)
   nols = .FALSE.
 1900 IF ( nols ) CALL close(geom3,1)
   IF ( .NOT.(nols .AND. nolc) ) CALL write(gp3s,lsload,3,0)
   IF ( .NOT.(nolc) ) THEN
!
!     COPY LOAD VECTORS TO SCRATCH FILE
!
      file = scrt2
      item = pvec
      IF ( Dry<0 ) THEN
!
!     IN DRY RUN MODE, LOADS PSEUDO-EXIST
!
         psuedo = .TRUE.
      ELSE
         CALL mtrxi(scrt2,Name,pvec,Z(Buf3),flag)
         IF ( flag==1 ) THEN
!
!     LOADS EXIST
!
            psuedo = .FALSE.
            CALL gopen(scrt2,Z(Buf3),0)
            irec = 1
            mcb(1) = scrt2
            CALL rdtrl(mcb)
            nvec = mcb(2)
            Luset = mcb(3)
            IF ( 2*Luset>=Nz ) THEN
!
!     INSUFFICIENT CORE
!
               CALL close(scrt2,1)
               CALL close(gp3s,1)
               CALL close(geom3,1)
               GOTO 4000
            ENDIF
         ELSEIF ( flag==3 .OR. flag==4 .OR. flag==5 ) THEN
            n = 2 - flag
            GOTO 3900
         ELSEIF ( flag==6 ) THEN
            GOTO 4100
         ELSE
            flag = 3
            n = 2 - flag
            GOTO 3900
         ENDIF
      ENDIF
   ENDIF
!
!     MERGE REAL AND ARTIFICIAL SLOAD CARDS
!
   sidc = 0
   Irow = 1
   Nrow = Luset
   IF ( .NOT.nolc ) CALL open(*4100,scrt,Z(Buf2),0)
 2000 IF ( .NOT.(nols) ) THEN
      file = geom3
      CALL read(*4200,*2100,geom3,temp2,3,0,nwds)
      GOTO 2200
   ENDIF
 2100 IF ( nolc ) GOTO 3300
   temp2(1) = large
 2200 sids = temp2(1)
   IF ( nolc ) GOTO 2500
   IF ( sidc<=sids ) THEN
!
!     READ THE SID AND FACTOR OF THE LOADC CARD ITSELF
!
      file = scrt
      CALL read(*2300,*4300,scrt,temp,2,0,nwds)
   ENDIF
   GOTO 2400
 2300 temp(1) = large
   nolc = .TRUE.
   CALL close(scrt,1)
   IF ( nols ) GOTO 3300
 2400 DO i = 1 , Luset
      Rz(i) = 0.0
   ENDDO
   sidc = temp(1)
   fact = rtemp(2)
   IF ( .NOT.nolc ) THEN
      IF ( .NOT.nols ) THEN
!
!     BOTH LOADC AND SLOAD CARDS ARE PRESENT
!
         IF ( sids>=sidc ) GOTO 2700
         GOTO 3000
      ELSE
!
!     NO MORE SLOAD CARDS ARE PRESENT
!
         sids = large
         GOTO 2700
      ENDIF
   ENDIF
 2500 IF ( nols ) GOTO 3300
!
!     NO MORE LOADC CARDS, WRITE ENTIRE  SLOAD  RECORD
!
   CALL write(gp3s,temp2,3,0)
   file = geom3
   DO
      CALL read(*4200,*2600,geom3,Z(1),Nz,0,nwds)
      CALL write(gp3s,Z(1),Nz,0)
   ENDDO
 2600 CALL write(gp3s,Z(1),nwds,1)
   GOTO 3300
 2700 DO
!
!     READ LOADC DATA, FIND VECTOR, UNPACK, MULT BY FACTOR, AND ADD
!     TO FIND A MATRIX COLUMN,USING FWDREC, CHANGE ON 16
!
      file = scrt
      CALL read(*4200,*2900,scrt,temp,2,0,nwds)
      IF ( .NOT.(temp(1)==0 .OR. psuedo .OR. temp(2)==0) ) THEN
         n = temp(1) - irec
         IF ( n<0 ) THEN
            n = -n
            DO i = 1 , n
               CALL bckrec(scrt2)
            ENDDO
         ELSEIF ( n/=0 ) THEN
            DO i = 1 , n
               CALL fwdrec(*2800,scrt2)
            ENDDO
         ENDIF
!
!     NOW SCRT2 IS POSITIONED TO THE DESIRED LOAD VECTOR.  UNPACK IT AND
!     FACTOR AND ADD IT TO VECTOR AT TOP OF OPEN CORE
!
         irec = temp(1) + 1
         CALL unpack(*2700,scrt2,Rz(Luset+1))
         DO i = 1 , Luset
            Rz(i) = rtemp(2)*fact*Rz(Luset+i) + Rz(i)
         ENDDO
      ENDIF
   ENDDO
!
!     CANT FIND LOAD VECTOR
!
 2800 WRITE (Outt,99004) Sfm , temp(1) , nvec , Luset , Name
99004 FORMAT (A25,' 6332, CANT FIND LOAD VECTOR NUMBER',I9,' IN LOAD ','MATRIX OF',I9,' COLUMNS',/32X,'BY',I9,                      &
             &' ROWS FOR SOLUTION STRUCTURE ',2A4)
   Nono = 1
   GOTO 3300
!
!     HERE WHEN FINISHED COMBINING VECTORS FOR ONE LOADC CARD
!
 2900 IF ( sidc<sids ) GOTO 3200
 3000 DO
      iz = temp2(2)
      Rz(iz) = Rz(iz) + rtemp2(3)
      file = geom3
      CALL read(*4200,*3100,geom3,temp2,3,0,nwds)
      IF ( temp2(1)/=sids ) THEN
         sids = temp2(1)
         GOTO 3200
      ENDIF
   ENDDO
 3100 nols = .TRUE.
!
!     WRITE OUT LOAD VECTOR IN SLOAD FORMAT
!
 3200 temp(1) = min0(sids,sidc)
   DO i = 1 , Luset
      IF ( Rz(i)/=0.0 ) THEN
         temp(2) = i
         rtemp(3) = Rz(i)
         CALL write(gp3s,temp,3,0)
      ENDIF
   ENDDO
   IF ( sids==sidc ) GOTO 2000
   GOTO 2200
!
!     ALL LOADS PROCESSED
!
 3300 CALL write(gp3s,0,0,1)
   CALL write(gp3s,nlimit,3,1)
   CALL close(scrt,1)
   CALL close(gp3s,1)
   CALL close(scrt2,1)
   CALL close(geom3,1)
   mcb(1) = gp3s
!
!     TURN ON SLOAD BIT IN GP3S TRAILER
!     ALSO LOAD CARD BIT IF LOAD CARDS EXIST
!
   DO i = 2 , 7
      mcb(i) = 0
   ENDDO
   j = (lsload(2)-1)/16
   i = lsload(2) - 16*j
   mcb(j+2) = Two(i+16)
   IF ( ldcd/=0 ) THEN
      j = (lload(2)-1)/16
      i = lload(2) - 16*j
      mcb(j+2) = orf(mcb(j+2),Two(i+16))
   ENDIF
   CALL wrttrl(mcb)
!
!     SPLIT CASE CONTROL  INTO SUBSTRUCTURE AND NORMAL NASTRAN
!
 3400 CALL open(*4100,casec,Z(Buf1),0)
   CALL open(*4100,cases,Z(Buf2),1)
   CALL open(*4100,casei,Z(Buf3),1)
   file = cases
 3500 CALL read(*3700,*3600,casec,Z(1),Nz,0,nwds)
 3600 IF ( Z(1)==ncasec(1) .AND. Z(2)==ncasec(2) ) file = casei
   CALL write(cases,Z,nwds,1)
   IF ( file==casei ) CALL write(casei,Z(1),nwds,1)
   GOTO 3500
 3700 mcb(1) = casec
   CALL rdtrl(mcb)
   mcb(1) = cases
   CALL wrttrl(mcb)
   mcb(1) = casei
   CALL wrttrl(mcb)
   CALL close(casec,1)
   CALL close(casei,1)
   CALL close(cases,1)
   IF ( Name(1)==xxxx .AND. Name(2)==xxxx ) RETURN
   IF ( Nono==0 ) THEN
!
!     GENERATE  FICTITIOUS GP1 DATA BLOCKS
!
!
!     ***  GPL FILE  ***
!
!     GPL HEADER RECORD HAS 3 WORD, (SEE GP1)
!     SET THE 3RD WORD, MULTIPLIER MULT, TO 1000
!
      DO i = 2 , 7
         mcb(i) = 0
      ENDDO
      mcb(1) = gpl
      file = gpl
      n = -1
      CALL open(*4500,gpl,Z(Buf1),1)
      CALL fname(gpl,temp(1))
      temp(3) = 1000
      CALL write(gpl,temp(1),3,1)
      DO i = 1 , Luset
         CALL write(gpl,i,1,0)
      ENDDO
      CALL write(gpl,i,0,1)
      DO i = 1 , Luset
         temp(1) = i
         temp(2) = 1000*i
         CALL write(gpl,temp,2,0)
      ENDDO
      CALL write(gpl,i,0,1)
      CALL close(gpl,1)
      mcb(2) = Luset
      CALL wrttrl(mcb)
   ENDIF
!
!     ***  EQEXIN FILE  ***
!
   mcb(1) = eqex
   CALL gopen(eqex,Z(Buf1),1)
   DO i = 1 , Luset
      temp(1) = i
      temp(2) = i
      CALL write(eqex,temp,2,0)
   ENDDO
   CALL write(eqex,temp,0,1)
   DO i = 1 , Luset
      temp(1) = i
      temp(2) = 10*i + 2
      CALL write(eqex,temp,2,0)
   ENDDO
   CALL write(eqex,temp,0,1)
   CALL close(eqex,1)
   mcb(2) = Luset
   CALL wrttrl(mcb)
!
!     ***  GPDT FILE  ***
!
   mcb(1) = gpdt
   DO i = 3 , 7
      temp(i) = 0
   ENDDO
   temp(2) = -1
   CALL gopen(gpdt,Z(Buf1),1)
   DO i = 1 , Luset
      temp(1) = i
      CALL write(gpdt,temp,7,0)
   ENDDO
   CALL write(gpdt,temp,0,1)
   CALL close(gpdt,1)
   mcb(2) = Luset
   CALL wrttrl(mcb)
   IF ( Nono==0 ) THEN
!
!     ***  BGPDT FILE  ***
!
      mcb(1) = bgpdt
      DO i = 2 , 4
         temp(i) = 0
      ENDDO
      temp(1) = -1
      CALL gopen(bgpdt,Z(Buf1),1)
      DO i = 1 , Luset
         CALL write(bgpdt,temp,4,0)
      ENDDO
      CALL write(bgpdt,temp,0,1)
      CALL close(bgpdt,1)
      mcb(2) = Luset
      CALL wrttrl(mcb)
   ENDIF
!
!     ***  SIL FILE  ***
!
   mcb(1) = sil
   CALL gopen(sil,Z(Buf1),1)
   DO i = 1 , Luset
      CALL write(sil,i,1,0)
   ENDDO
   CALL write(sil,i,0,1)
   CALL close(sil,1)
!
!
   mcb(2) = Luset
   mcb(3) = Luset
   CALL wrttrl(mcb)
   IF ( Nono/=0 ) Dry = -2
   CALL sofcls
   RETURN
 3800 n = -itest - 4
 3900 IF ( Dry<0 ) n = iabs(n)
   Dry = -2
   CALL smsg(n,item,Name)
   RETURN
 4000 n = -8
   GOTO 4400
 4100 n = -1
   GOTO 4400
 4200 n = -2
   GOTO 4400
 4300 n = -3
 4400 CALL sofcls
   IF ( Dry<0 ) n = iabs(n)
   Dry = -2
 4500 CALL mesage(n,file,nsgen)
   RETURN
99005 FORMAT (A25,' 6329, SUBSTRUCTURE ',2A4,' REFERENCED ON ',2A4,' CARD',/30X,'IS NOT A COMPONENT BASIC SUBSTRUCTURE OF ',        &
             &'SOLUTION STRUCTURE ',2A4,/30X,'THIS CARD WILL BE IGNORED')
END SUBROUTINE sgen
