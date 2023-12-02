!*==sgen.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
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
   USE c_blank
   USE c_sgencm
   USE c_system
   USE c_two
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bgpdt , casec , casei , cases , dynam , dyns , eqex , eqss , geom3 , geom4 , gp3s , gp4s , gpdt , gpl , lods , &
                   & ntypec , ntyped , pvec , scrt , scrt2 , sil , xxxx
   INTEGER :: buf4 , code , file , flag , i , ibs1 , ibs2 , ibs3 , ic , idx , ifl , ig , igr , igrd , ild , inam , ip , ipt , irec ,&
            & is , iss1 , isub , item , itest , iz , izl , j , jg , jpt , large , ldcd , lid , lvec , n , nc , ngrd , nj , nl ,     &
            & nodyn , nog4 , nptr , nset , nsil , nsild , nvec , nwds , nzb , sidc , sids
   INTEGER , DIMENSION(2,8) , SAVE :: ctypeo
   INTEGER , DIMENSION(2,8) :: ctypes
   INTEGER , DIMENSION(2) , SAVE :: dareas , delays , dphses , loadc , mpcs , ncasec , nsgen , spcs , spcs1 , spcsd , tics
   REAL :: fact
   INTEGER , DIMENSION(4,9) , SAVE :: icode , ltab
   INTEGER , DIMENSION(32) :: icomp
   INTEGER , DIMENSION(3) , SAVE :: lload , lsload , minus , nlimit
   INTEGER , DIMENSION(7) :: mcb
   LOGICAL :: nolc , nols , psuedo , stest
   REAL , DIMENSION(10) :: rtemp , rtemp2
   INTEGER , DIMENSION(10) :: temp , temp2
   INTEGER , DIMENSION(2) :: type
   INTEGER , DIMENSION(4) :: z
   EXTERNAL andf , bckrec , bisloc , close , complf , decode , fname , fread , fwdrec , gopen , korsz , locate , mesage , mtrxi ,   &
          & open , orf , preloc , rdtrl , read , sfetch , sgena , sgenb , sgenm , sjump , smsg , sofcls , sofopn , suread , unpack ,&
          & write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (Rz(1),Z(1)) , (temp(1),rtemp(1)) , (temp2(1),rtemp2(1)) , (ctypes(1,1),mpcs(1)) , (ctypes(1,2),spcs(1)) ,           &
!>>>>    & (ctypes(1,3),spcs1(1)) , (ctypes(1,4),spcsd(1)) , (ctypes(1,5),dareas(1)) , (ctypes(1,6),delays(1)) , (ctypes(1,7),dphses(1)) &
!>>>>    & , (ctypes(1,8),tics(1))
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         ity = 1
         incr = 1
         nono = 0
         large = two(2)
         nz = korsz(z(1))
         ibs1 = nz - ibuf + 1
         ibs2 = ibs1 - ibuf - 1
         ibs3 = ibs2 - ibuf
         buf1 = ibs3 - ibuf
         buf2 = buf1 - ibuf
         buf3 = buf2 - ibuf
         buf4 = buf3 - ibuf
         nz = buf4 - 1
         IF ( nz<=0 ) THEN
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( name(1)==xxxx .AND. name(2)==xxxx ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     INITIALIZE LUSET AND NOGPDT FLAGS
!
         luset = 0
         nogpdt = -1
!
!     FORM TABLES OF REFERENCED SID-S FOR LOAD, MPC, AND SPC
!     CASE CONTROL CARDS.
!
!
!     OPEN  SOF , GET EQSS ITEM , READ  SIL DATA INTO CORE
!
         CALL sofopn(z(ibs1),z(ibs2),z(ibs3))
         CALL sfetch(name,eqss,1,flag)
         item = eqss
         IF ( flag/=1 ) THEN
!
!     ERRORS
!
            n = 2 - flag
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL suread(z(1),nz,nwds,flag)
            IF ( flag/=2 ) THEN
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            nss = z(3)
            iz = nwds + 1
!
!     READ SIL GROUP INTO CORE
!
            CALL sjump(nss)
            CALL suread(z(iz),nz-iz+1,nsil,flag)
            IF ( flag/=2 ) THEN
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ipt = iz + nsil - 2
!
!     FIND LENGTH OF VECTOR = LUSET
!
            ic = z(ipt+1)
            CALL decode(ic,icomp,nc)
            luset = z(ipt) + nc - 1
            nogpdt = luset
!
!     READ EQSS ( G ,IP, AND C AT A TIME) AND CONVERT IP TO SIL .
!     WRITE ON SCRT
!
            is = 0
            file = scrt
            CALL gopen(scrt,z(buf2),1)
            CALL sfetch(name,eqss,1,flag)
            nj = 1
            CALL sjump(nj)
            SPAG_Loop_1_1: DO
!
               CALL suread(temp,3,nwds,flag)
               IF ( flag/=1 ) THEN
                  is = is + 1
                  CALL write(scrt,temp,0,1)
                  IF ( is>=nss ) THEN
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
                     iptr = iz
                     nptr = iptr
                     isub = iptr + nss + 1
                     nzb = nz - isub + 1
                     file = scrt
                     CALL gopen(scrt,z(buf2),0)
                     DO i = 1 , nss
                        z(nptr) = isub
                        nptr = nptr + 1
                        CALL read(*420,*2,scrt,z(isub),nzb,1,nwds)
                        spag_nextblock_1 = 21
                        CYCLE SPAG_DispatchLoop_1
 2                      isub = isub + nwds
                        nzb = nzb - nwds
                        IF ( nzb<=0 ) THEN
                           spag_nextblock_1 = 21
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDDO
                     z(nptr) = isub
                     CALL close(scrt,1)
!
!     ***  GEOM4 DATA CONVERSION  ***
!
!          IN  - MPCS,SPCS,SPCS1,SPCSD CARDS
!          OUT - MPC ,SPC ,SPC1 ,SCPD  ON SCRT
!
                     file = geom4
                     nog4 = 0
                     CALL preloc(*80,z(buf1),geom4)
                     mcb(1) = geom4
                     CALL rdtrl(mcb)
                     CALL gopen(scrt,z(buf2),1)
                     stest = .FALSE.
!
!     ***  MPCS CARDS  ***
!
!          IN  - NAME(2), G, C, F
!          OUT - SIL, 0, F
!
                     CALL locate(*60,z(buf1),icode(1,1),idx)
                     CALL write(scrt,icode(1,1),3,0)
                     stest = .TRUE.
                     icode(4,1) = 1
                     type(1) = mpcs(1)
                     type(2) = mpcs(2)
                     ifl = 0
                     lid = 0
                     EXIT SPAG_Loop_1_1
                  ENDIF
               ELSE
                  ipt = iz + 2*temp(2) - 2
                  temp(2) = z(ipt)
                  CALL write(scrt,temp,3,0)
               ENDIF
            ENDDO SPAG_Loop_1_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*420,*40,geom4,j,1,0,nwds)
         IF ( j/=lid ) nsild = 0
         lid = j
         CALL write(scrt,j,1,0)
         spag_nextblock_1 = 3
      CASE (3)
         SPAG_Loop_1_3: DO
            CALL read(*420,*40,geom4,temp,5,0,nwds)
            IF ( temp(3)==-1 ) THEN
!
!     FINISHED ONE LOGICAL CARD, WRITE -1 FLAGS
!
               CALL write(scrt,minus,3,0)
               ifl = 0
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( temp(3)/=0 ) THEN
!
!     FIND  REQUESTED SUBSTRUCTURE
!
                  DO i = 1 , nss
                     inam = 2*i + 3
                     IF ( z(inam)==temp(1) .AND. z(inam+1)==temp(2) ) GOTO 5
                  ENDDO
!
!     SUBSTRUCTURE NOT FOUND
!
                  WRITE (outt,99005) uwm , temp(1) , temp(2) , type , name
               ENDIF
               CYCLE
!
!     FOUND SUBSTRUCTURE NAME
!
 5             ipt = iptr + i - 1
               igrd = z(ipt)
               ngrd = (z(ipt+1)-z(ipt))/3
!
!     SEARCH FOR GRID POINT
!
               CALL bisloc(*20,temp(3),z(igrd),3,ngrd,igr)
               ig = igr + igrd - 1
               SPAG_Loop_2_2: DO WHILE ( z(ig-3)==z(ig) )
                  IF ( ig<=igrd ) EXIT SPAG_Loop_2_2
                  ig = ig - 3
               ENDDO SPAG_Loop_2_2
               DO
                  code = z(ig+2)
!
!     FIND   THE  COMPONENT
!
                  CALL decode(code,icomp,nc)
                  IF ( temp(4)==0 ) temp(4) = 1
                  DO i = 1 , nc
                     IF ( temp(4)==icomp(i)+1 ) THEN
                        ic = i
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  IF ( z(ig+3)/=z(ig) ) EXIT SPAG_Loop_1_3
                  IF ( ig+3>=igrd+3*ngrd ) EXIT SPAG_Loop_1_3
                  ig = ig + 3
               ENDDO
            ENDIF
         ENDDO SPAG_Loop_1_3
!
!     BAD COMPONENT
!
 20      nono = 1
         WRITE (outt,99001) ufm , (temp(i),i=1,4) , type , name
!
!     MESSAGE FORMATS
!
99001    FORMAT (A23,' 6022, SUBSTRUCTURE ',2A4,', GRID POINT',I9,', COMPONENTS',I9,1H,,/30X,'REFERENCED ON ',2A4,                  &
                &' CARD, DO NOT EXIST ON SOLUTION STRUCTURE ',2A4)
         spag_nextblock_1 = 3
      CASE (4)
!
!     WRITE CONVERTED DATA ON SCRT
!
         temp(6) = z(ig+1) + ic - 1
         temp(7) = 0
         temp(8) = temp(5)
         CALL write(scrt,temp(6),3,0)
!
!     CHECK FOR DUPLICATE DEPENDENT SIL-S
!
         IF ( ifl==0 ) THEN
            IF ( nsild/=0 ) THEN
               DO i = 1 , nsild
                  IF ( z(isub+i-1)==temp(6) ) THEN
                     nono = 1
                     WRITE (outt,99002) ufm , j , temp(1) , temp(2) , temp(3) , temp(4)
99002                FORMAT (A23,' 6362, MPCS SET',I9,' IS ILLEGAL.',//5X,'SUBSTRUCTURE ',2A4,' GRID POINT',I9,' COMPONENT',I5,     &
                            &' SPECIFIES A NON-UNIQUE DEPENDENT DEGREE OF FREEDOM')
                  ENDIF
               ENDDO
               IF ( nsild>nzb ) THEN
                  spag_nextblock_1 = 21
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            z(isub+nsild) = temp(6)
            nsild = nsild + 1
            ifl = 1
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     FINISHED ALL MPCS CARDS, WRITE EOR AND UPDATE TRAILER
!
 40      CALL write(scrt,temp,0,1)
!
!     TURN OFF MPCS BIT
!
         j = (icode(2,1)-1)/16
         i = icode(2,1) - 16*j
         mcb(j+2) = andf(complf(two(i+16)),mcb(j+2))
!
!     TURN ON MPC BIT
!
         j = (ltab(2,1)-1)/16
         i = ltab(2,1) - 16*j
         mcb(j+2) = orf(two(i+16),mcb(j+2))
!
!     ***  SPCS CARDS  ***
!
!          IN  - SID, NAME(2), G, C, G, C, G, C, ..., -1, -1
!          OUT - SID, SIL, 0, 0 - REPEATED FOR EACH GRID
!
 60      CALL sgena(spcs,z(buf1),mcb,geom4,icode(1,2),0,scrt,ltab(1,2),1)
!
!     ***  SPCS1 CARDS  ***
!
!          IN  - SID, NAME(2), C, G, G, G, ..., -1
!          OUT - SID, 0, SIL, -1 - REPEATED FOR EACH GRID
!
         CALL sgenb(spcs1,z(buf1),mcb,geom4,icode(1,3),0,scrt,ltab(1,3),1)
!
!     ***  SPCSD CARDS  ***
!
!          IN  - SID, NAME(2), G, C, Y, ..., -1, -1, -1
!          OUT - SID, SIL, 0, Y - REPEATED FOR EACH GRID
!
         CALL sgena(spcsd,z(buf1),mcb,geom4,icode(1,4),1,scrt,ltab(1,4),1)
!
!     END OF CONSTRAINT CARD CONVERSION
!
         CALL close(geom4,1)
         CALL close(scrt,1)
         mcb(1) = gp4s
         CALL wrttrl(mcb)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 80      nog4 = 1
         spag_nextblock_1 = 5
      CASE (5)
!
!     ***  DYNAMICS DATA CONVERSION  ***
!
!          IN  - DAREAS,DELAYS,DPHASES,TICS CARDS
!          OUT - DAREA ,DELAY ,DPHASE ,TIC  ON SCRT
!
         file = dynam
         nodyn = 0
         CALL preloc(*100,z(buf1),dynam)
         mcb(1) = dynam
         CALL rdtrl(mcb)
         CALL gopen(scrt2,z(buf2),1)
!
!     ***  DAREAS  CARDS ***
!
!          IN  - SID, NAME(2), G, C, A, ..., -1, -1, -1
!          OUT - SID, SIL, 0, A - REPEATED FOR EACH GRID
!
         CALL sgena(dareas,z(buf1),mcb,dynam,icode(1,6),1,scrt2,ltab(1,6),1)
!
!     ***  DELAYS CARDS  ***
!
!          IN  - SID, NAME(2), G, C, T, ..., -1, -1, -1
!          OUT - SID, SIL, 0, T - REPEATED FOR EACH GRID
!
         CALL sgena(delays,z(buf1),mcb,dynam,icode(1,7),1,scrt2,ltab(1,7),1)
!
!    ***  DPHASES CARDS  ***
!
!         IN  - SID, NAME(2), G, C, TH, ..., -1, -1, -1
!         OUT - SID, SIL, 0, TH - REPEATED FOR EACH GRID
!
         CALL sgena(dphses,z(buf1),mcb,dynam,icode(1,8),1,scrt2,ltab(1,8),1)
!
!     ***  TICS CARDS  ***
!
!          IN  - SID, NAME(2), G, C, U, V, ..., -1, -1, -1, -1
!          OUT - SID, SIL, 0, U, V - REPEATED FOR EACH GRID
!
         CALL sgena(tics,z(buf1),mcb,dynam,icode(1,9),2,scrt2,ltab(1,9),2)
!
!     END OF DYNAMICS CONVERSION
!
         CALL close(dynam,1)
         CALL close(scrt2,1)
         mcb(1) = dyns
         CALL wrttrl(mcb)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 100     nodyn = 1
         spag_nextblock_1 = 6
      CASE (6)
!
!     MERGE CONVERTED DATA WITH EXISTING DATA - GEOM4
!
         IF ( nog4/=1 ) CALL sgenm(ntypec,geom4,scrt,gp4s,icode(1,1),ltab(1,1),ctypes(1,1),ctypeo(1,1))
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
         CALL preloc(*140,z(buf1),geom4)
         CALL locate(*140,z(buf1),icode(1,5),idx)
!
!     READ FIRST GROUP OF LODS ITEM FOR SOLUTION STRUCTURE
!
         item = lods
         CALL sfetch(name,lods,1,flag)
         IF ( flag==1 ) THEN
            CALL suread(z(1),nz,nwds,itest)
            IF ( itest==1 ) THEN
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( itest==3 ) THEN
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            nss = z(4)
            iss1 = 5
            nl = z(3)
            ipt = 2*nss + 5
            izl = 2*nss + ipt + 2
            z(ipt) = izl
            z(ipt+1) = 0
            IF ( izl+nss+nl<=nz ) THEN
!
!     READ REMAINDER OF LODS INTO OPEN CORE AT Z(IZL)
!
               DO i = 1 , nss
                  CALL suread(z(izl),nz-izl+1,nwds,itest)
                  IF ( itest==1 ) THEN
                     spag_nextblock_1 = 21
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( itest==3 ) THEN
                     spag_nextblock_1 = 19
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  izl = izl + nwds
                  jg = ipt + 2*i
                  z(jg) = izl
                  z(jg+1) = z(jg-1) + nwds - 1
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
               CALL open(*400,scrt,z(buf2),1)
            ELSE
!
!     INSUFFICIENT CORE
!
               CALL close(geom4,1)
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( flag==2 .OR. flag==4 .OR. flag==5 ) THEN
            n = 2 - flag
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     LODS ITEM DOES NOT EXIST
!
            nolc = .TRUE.
            GOTO 140
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         CALL read(*420,*120,geom4,temp,2,0,nwds)
         CALL write(scrt,temp,2,0)
         lid = temp(1)
         nolc = .FALSE.
         spag_nextblock_1 = 8
      CASE (8)
         SPAG_Loop_1_4: DO
!
!     READ AN ENTRY
!
            CALL fread(geom4,temp,4,0)
            IF ( temp(3)==-1 ) THEN
!
!     END OF LOGICAL LOADC CARD
!
               CALL write(scrt,temp,0,1)
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     FIND SUBSTRUCTURE AND SET
!
               DO i = 1 , nss
                  inam = iss1 + 2*(i-1)
                  IF ( z(inam)==temp(1) .AND. z(inam+1)==temp(2) ) EXIT SPAG_Loop_1_4
               ENDDO
!
!     SUBSTRUCTURE NOT FOUND
!
               WRITE (outt,99005) uwm , temp(1) , temp(2) , type , name
            ENDIF
         ENDDO SPAG_Loop_1_4
!
!     FOUND SUBSTRUCTURE NAME
!
         jpt = ipt + 2*i - 2
!
!     POINTER TO LODS DATA FOR THIS SUBSTRUCTURE
!
         ild = z(jpt)
!
!     NUMBER OF SETS IN LODS DATA FOR THIS SUBSTRUCTURE
!
         nset = z(ild)
!
!     FIND LOADC SET IN LODS DATA
!
         IF ( nset/=0 ) THEN
            DO i = 1 , nset
               ip = ild + i
               IF ( z(ip)==temp(3) ) THEN
                  lvec = z(jpt+1) + i
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
!
!     SET NOT FOUND
!
         nono = 1
         WRITE (outt,99003) ufm , name , lid , temp(3) , temp(1) , temp(2)
99003    FORMAT (A23,' 6331, SOLUTION SUBSTRUCTURE ',2A4,' - LOADC SET',I9,' REFERENCES UNDEFINED LOAD',/30X,'SET',I9,              &
                &' OF BASIC SUBSTRUCTURE ',2A4)
         spag_nextblock_1 = 8
      CASE (9)
         temp(1) = lvec
         temp(2) = temp(4)
         CALL write(scrt,temp,2,0)
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     END OF LOADC RECORD
!
 120     CALL close(scrt,1)
 140     CALL close(geom4,1)
!
!     MERGE CONVERTED LOAD DATA WITH SLOAD DATA.
!
!
!     IF ANY ERRORS WERE DETECTED, SKIP LOAD COMPUTATION
!
         IF ( nono/=0 ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(gp3s,z(buf4),1)
!
!     COPY LOAD CARDS TO GP3S
!
         CALL preloc(*200,z(buf1),geom3)
         ldcd = 0
         CALL locate(*180,z(buf1),lload,idx)
         ldcd = 1
         CALL write(gp3s,lload,3,0)
         DO
            CALL read(*420,*160,geom3,z(1),nz,0,nwds)
            CALL write(gp3s,z(1),nz,0)
         ENDDO
 160     CALL write(gp3s,z(1),nwds,1)
!
!     POSITION TO SLOAD CARDS
!
 180     CALL locate(*200,z(buf1),lsload,idx)
         nols = .FALSE.
 200     IF ( nols ) CALL close(geom3,1)
         IF ( .NOT.(nols .AND. nolc) ) CALL write(gp3s,lsload,3,0)
         IF ( .NOT.(nolc) ) THEN
!
!     COPY LOAD VECTORS TO SCRATCH FILE
!
            file = scrt2
            item = pvec
            IF ( dry<0 ) THEN
!
!     IN DRY RUN MODE, LOADS PSEUDO-EXIST
!
               psuedo = .TRUE.
            ELSE
               CALL mtrxi(scrt2,name,pvec,z(buf3),flag)
               IF ( flag==1 ) THEN
!
!     LOADS EXIST
!
                  psuedo = .FALSE.
                  CALL gopen(scrt2,z(buf3),0)
                  irec = 1
                  mcb(1) = scrt2
                  CALL rdtrl(mcb)
                  nvec = mcb(2)
                  luset = mcb(3)
                  IF ( 2*luset>=nz ) THEN
!
!     INSUFFICIENT CORE
!
                     CALL close(scrt2,1)
                     CALL close(gp3s,1)
                     CALL close(geom3,1)
                     spag_nextblock_1 = 21
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSEIF ( flag==3 .OR. flag==4 .OR. flag==5 ) THEN
                  n = 2 - flag
                  spag_nextblock_1 = 20
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( flag==6 ) THEN
                  GOTO 400
               ELSE
                  flag = 3
                  n = 2 - flag
                  spag_nextblock_1 = 20
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
!
!     MERGE REAL AND ARTIFICIAL SLOAD CARDS
!
         sidc = 0
         irow = 1
         nrow = luset
         IF ( .NOT.nolc ) CALL open(*400,scrt,z(buf2),0)
         spag_nextblock_1 = 10
      CASE (10)
         IF ( .NOT.(nols) ) THEN
            file = geom3
            CALL read(*420,*220,geom3,temp2,3,0,nwds)
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 220     IF ( nolc ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         temp2(1) = large
         spag_nextblock_1 = 11
      CASE (11)
         sids = temp2(1)
         IF ( nolc ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( sidc<=sids ) THEN
!
!     READ THE SID AND FACTOR OF THE LOADC CARD ITSELF
!
            file = scrt
            CALL read(*240,*440,scrt,temp,2,0,nwds)
         ENDIF
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 240     temp(1) = large
         nolc = .TRUE.
         CALL close(scrt,1)
         IF ( nols ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
         DO i = 1 , luset
            rz(i) = 0.0
         ENDDO
         sidc = temp(1)
         fact = rtemp(2)
         IF ( .NOT.nolc ) THEN
            IF ( nols ) THEN
!
!     NO MORE SLOAD CARDS ARE PRESENT
!
               sids = large
!
!     BOTH LOADC AND SLOAD CARDS ARE PRESENT
!
            ELSEIF ( sids<sidc ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            GOTO 280
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
         IF ( nols ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     NO MORE LOADC CARDS, WRITE ENTIRE  SLOAD  RECORD
!
         CALL write(gp3s,temp2,3,0)
         file = geom3
         DO
            CALL read(*420,*260,geom3,z(1),nz,0,nwds)
            CALL write(gp3s,z(1),nz,0)
         ENDDO
 260     CALL write(gp3s,z(1),nwds,1)
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 280     DO
!
!     READ LOADC DATA, FIND VECTOR, UNPACK, MULT BY FACTOR, AND ADD
!     TO FIND A MATRIX COLUMN,USING FWDREC, CHANGE ON 16
!
            file = scrt
            CALL read(*420,*320,scrt,temp,2,0,nwds)
            IF ( .NOT.(temp(1)==0 .OR. psuedo .OR. temp(2)==0) ) THEN
               n = temp(1) - irec
               IF ( n<0 ) THEN
                  n = -n
                  DO i = 1 , n
                     CALL bckrec(scrt2)
                  ENDDO
               ELSEIF ( n/=0 ) THEN
                  DO i = 1 , n
                     CALL fwdrec(*300,scrt2)
                  ENDDO
               ENDIF
!
!     NOW SCRT2 IS POSITIONED TO THE DESIRED LOAD VECTOR.  UNPACK IT AND
!     FACTOR AND ADD IT TO VECTOR AT TOP OF OPEN CORE
!
               irec = temp(1) + 1
               CALL unpack(*280,scrt2,rz(luset+1))
               DO i = 1 , luset
                  rz(i) = rtemp(2)*fact*rz(luset+i) + rz(i)
               ENDDO
            ENDIF
         ENDDO
!
!     CANT FIND LOAD VECTOR
!
 300     WRITE (outt,99004) sfm , temp(1) , nvec , luset , name
99004    FORMAT (A25,' 6332, CANT FIND LOAD VECTOR NUMBER',I9,' IN LOAD ','MATRIX OF',I9,' COLUMNS',/32X,'BY',I9,                   &
                &' ROWS FOR SOLUTION STRUCTURE ',2A4)
         nono = 1
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
!
!     HERE WHEN FINISHED COMBINING VECTORS FOR ONE LOADC CARD
!
 320     IF ( sidc<sids ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 14
      CASE (14)
         DO
            iz = temp2(2)
            rz(iz) = rz(iz) + rtemp2(3)
            file = geom3
            CALL read(*420,*340,geom3,temp2,3,0,nwds)
            IF ( temp2(1)/=sids ) THEN
               sids = temp2(1)
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
 340     nols = .TRUE.
         spag_nextblock_1 = 15
      CASE (15)
!
!     WRITE OUT LOAD VECTOR IN SLOAD FORMAT
!
         temp(1) = min0(sids,sidc)
         DO i = 1 , luset
            IF ( rz(i)/=0.0 ) THEN
               temp(2) = i
               rtemp(3) = rz(i)
               CALL write(gp3s,temp,3,0)
            ENDIF
         ENDDO
         IF ( sids/=sidc ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (16)
!
!     ALL LOADS PROCESSED
!
         CALL write(gp3s,0,0,1)
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
         mcb(j+2) = two(i+16)
         IF ( ldcd/=0 ) THEN
            j = (lload(2)-1)/16
            i = lload(2) - 16*j
            mcb(j+2) = orf(mcb(j+2),two(i+16))
         ENDIF
         CALL wrttrl(mcb)
         spag_nextblock_1 = 17
      CASE (17)
!
!     SPLIT CASE CONTROL  INTO SUBSTRUCTURE AND NORMAL NASTRAN
!
         CALL open(*400,casec,z(buf1),0)
         CALL open(*400,cases,z(buf2),1)
         CALL open(*400,casei,z(buf3),1)
         file = cases
         spag_nextblock_1 = 18
      CASE (18)
         CALL read(*380,*360,casec,z(1),nz,0,nwds)
 360     IF ( z(1)==ncasec(1) .AND. z(2)==ncasec(2) ) file = casei
         CALL write(cases,z,nwds,1)
         IF ( file==casei ) CALL write(casei,z(1),nwds,1)
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 380     mcb(1) = casec
         CALL rdtrl(mcb)
         mcb(1) = cases
         CALL wrttrl(mcb)
         mcb(1) = casei
         CALL wrttrl(mcb)
         CALL close(casec,1)
         CALL close(casei,1)
         CALL close(cases,1)
         IF ( name(1)==xxxx .AND. name(2)==xxxx ) RETURN
         IF ( nono==0 ) THEN
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
            CALL open(*460,gpl,z(buf1),1)
            CALL fname(gpl,temp(1))
            temp(3) = 1000
            CALL write(gpl,temp(1),3,1)
            DO i = 1 , luset
               CALL write(gpl,i,1,0)
            ENDDO
            CALL write(gpl,i,0,1)
            DO i = 1 , luset
               temp(1) = i
               temp(2) = 1000*i
               CALL write(gpl,temp,2,0)
            ENDDO
            CALL write(gpl,i,0,1)
            CALL close(gpl,1)
            mcb(2) = luset
            CALL wrttrl(mcb)
         ENDIF
!
!     ***  EQEXIN FILE  ***
!
         mcb(1) = eqex
         CALL gopen(eqex,z(buf1),1)
         DO i = 1 , luset
            temp(1) = i
            temp(2) = i
            CALL write(eqex,temp,2,0)
         ENDDO
         CALL write(eqex,temp,0,1)
         DO i = 1 , luset
            temp(1) = i
            temp(2) = 10*i + 2
            CALL write(eqex,temp,2,0)
         ENDDO
         CALL write(eqex,temp,0,1)
         CALL close(eqex,1)
         mcb(2) = luset
         CALL wrttrl(mcb)
!
!     ***  GPDT FILE  ***
!
         mcb(1) = gpdt
         DO i = 3 , 7
            temp(i) = 0
         ENDDO
         temp(2) = -1
         CALL gopen(gpdt,z(buf1),1)
         DO i = 1 , luset
            temp(1) = i
            CALL write(gpdt,temp,7,0)
         ENDDO
         CALL write(gpdt,temp,0,1)
         CALL close(gpdt,1)
         mcb(2) = luset
         CALL wrttrl(mcb)
         IF ( nono==0 ) THEN
!
!     ***  BGPDT FILE  ***
!
            mcb(1) = bgpdt
            DO i = 2 , 4
               temp(i) = 0
            ENDDO
            temp(1) = -1
            CALL gopen(bgpdt,z(buf1),1)
            DO i = 1 , luset
               CALL write(bgpdt,temp,4,0)
            ENDDO
            CALL write(bgpdt,temp,0,1)
            CALL close(bgpdt,1)
            mcb(2) = luset
            CALL wrttrl(mcb)
         ENDIF
!
!     ***  SIL FILE  ***
!
         mcb(1) = sil
         CALL gopen(sil,z(buf1),1)
         DO i = 1 , luset
            CALL write(sil,i,1,0)
         ENDDO
         CALL write(sil,i,0,1)
         CALL close(sil,1)
!
!
         mcb(2) = luset
         mcb(3) = luset
         CALL wrttrl(mcb)
         IF ( nono/=0 ) dry = -2
         CALL sofcls
         RETURN
      CASE (19)
         n = -itest - 4
         spag_nextblock_1 = 20
      CASE (20)
         IF ( dry<0 ) n = iabs(n)
         dry = -2
         CALL smsg(n,item,name)
         RETURN
      CASE (21)
         n = -8
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 400     n = -1
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 420     n = -2
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 440     n = -3
         spag_nextblock_1 = 22
      CASE (22)
         CALL sofcls
         IF ( dry<0 ) n = iabs(n)
         dry = -2
 460     CALL mesage(n,file,nsgen)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99005 FORMAT (A25,' 6329, SUBSTRUCTURE ',2A4,' REFERENCED ON ',2A4,' CARD',/30X,'IS NOT A COMPONENT BASIC SUBSTRUCTURE OF ',        &
             &'SOLUTION STRUCTURE ',2A4,/30X,'THIS CARD WILL BE IGNORED')
END SUBROUTINE sgen
