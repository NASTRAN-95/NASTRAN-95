
SUBROUTINE flbelm
   IMPLICIT NONE
   INTEGER Af , Afdict , Afmat , Bgpdt , Conect , Cstm , Dkgg , Ect , Eqexin , Fbelm , Frelm , Geom2 , Geom3 , Ibgpdt , Ibuf1 ,     &
         & Ibuf2 , Ibuf3 , Ibuf4 , Ibuf5 , Icore , Igrav , Igrid , Isil , Kgdict , Kgmat , Lcore , Mpt , Nbgpdt , Ngrav , Ngrid ,   &
         & Nofree , Nograv , Nout , Nsil , Sil , Uset , Usetf , Usets , Z(1)
   LOGICAL Error
   REAL Sysbuf
   CHARACTER*23 Ufm
   COMMON /blank / Nograv , Nofree
   COMMON /flbfil/ Geom2 , Ect , Bgpdt , Sil , Mpt , Geom3 , Cstm , Uset , Eqexin , Usetf , Usets , Af , Dkgg , Fbelm , Frelm ,     &
                 & Conect , Afmat , Afdict , Kgmat , Kgdict
   COMMON /flbptr/ Error , Icore , Lcore , Ibgpdt , Nbgpdt , Isil , Nsil , Igrav , Ngrav , Igrid , Ngrid , Ibuf1 , Ibuf2 , Ibuf3 ,  &
                 & Ibuf4 , Ibuf5
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER card(10) , cflstr(2) , cfree(2) , elm2d(7,3) , elmfl(4,3) , file , grid(4) , i , id(3) , ids , ielmt , j , jloc , lelmt ,&
         & lgrid , mcb(7) , n , n2d , name(2) , nelm , nelmt , nfl , ngrdf , ngrds , ngrdt , ntype , nwds , nz
!
!     READS CFLSTR AND CFREE BULK DATA AND BUILDS INCORE TABLES TO
!     DESCRIBE THE CONNECTIVITY BETWEEN THE STRUCTURE AND FLUID
!
   DATA cflstr/7610 , 76/ , cfree/4810 , 48/ , mcb/7*0/
   DATA name/4HFLBE , 4HLM  /
!
!     TWO DIMENSIONAL STRUCTURAL ELEMENTS DESCRIPTIONS
!
   DATA n2d/7/
!
!                     TRIA1  TRIA2   TRMEM  QUAD1  QUAD2  QDMEM  SHEAR
!     1  IFP CARD NUMBERS
!     2  NUMBER OF GRIDS
!     3  NUMBER OF WORDS IN ECT RECORD
!
   DATA elm2d/52 , 53 , 56 , 57 , 58 , 60 , 61 , 3 , 3 , 3 , 4 , 4 , 4 , 4 , 6 , 6 , 6 , 7 , 7 , 7 , 6/
!
!     FLUID ELEMENT DESCRIPTIONS
!
   DATA nfl/4/
!
!                     FHEX1     FHEX2     FTETRA    FWEDGE
!    1  IFP CARD NUMBERS
!    2  NUMBER OF GRIDS
!    3  NUMBER OF WORDS IN ECT RECORD
!
   DATA elmfl/333 , 334 , 335 , 336 , 8 , 8 , 4 , 6 , 10 , 10 , 6 , 8/
!
!
!     READ BGPDT INTO OPEN CORE
!
   Ibgpdt = 1
   file = Bgpdt
   CALL gopen(Bgpdt,Z(Ibuf1),0)
   nz = Ibuf3 - 1
   CALL read(*1500,*100,Bgpdt,Z(Ibgpdt),nz,1,Nbgpdt)
   n = -8
   CALL mesage(n,file,name)
   GOTO 1700
 100  Icore = Ibgpdt + Nbgpdt
   ngrdt = Nbgpdt/4
   CALL close(Bgpdt,1)
!
!     LOCATE CFLSTR CARDS ON GEOM2 AND READ THEM INTO ELEMENT TABLE
!     IN CORE.   ONE ELEMENT TABLE RECORD WILL LOOK AS FOLLOWS -
!
!                  WORD      DESCRIPTION
!
!                  1         STRUCTURE ELEMENT ID
!                  2         FLUID ELEMENT ID
!                  3-6       ZERO
!                  7         GRAV LOAD ID
!
   file = Geom2
   CALL preloc(*1400,Z(Ibuf1),Geom2)
   CALL locate(*1700,Z(Ibuf1),cflstr,id)
   ielmt = Icore
 200  CALL read(*1500,*300,Geom2,id,2,0,n)
   DO
      CALL read(*1500,*1600,Geom2,ids,1,0,n)
      IF ( ids<0 ) GOTO 200
      IF ( Icore+7>=Ibuf3 ) THEN
         n = -8
         CALL mesage(n,file,name)
         GOTO 1700
      ELSE
         Z(Icore) = ids
         Z(Icore+1) = id(1)
         Z(Icore+2) = 0
         Z(Icore+3) = 0
         Z(Icore+4) = 0
         Z(Icore+5) = 0
         Z(Icore+6) = id(2)
         Icore = Icore + 7
      ENDIF
   ENDDO
!
 300  nelmt = Icore - ielmt
   nelm = nelmt/7
!
!     SORT ELEMENT TABLE BY STRUCTUREAL ELEMENT ID
!
   CALL sort(0,0,7,1,Z(ielmt),nelmt)
!
!     READ ECT AND PROCESS 2D STRUCTURAL ELEMENTS
!
   file = Ect
   CALL gopen(Ect,Z(Ibuf2),0)
 400  DO
      CALL read(*700,*1500,Ect,card,3,0,n)
      DO i = 1 , n2d
         IF ( card(3)==elm2d(i,1) ) GOTO 500
      ENDDO
!
!     SKIP RECORD BECAUSE NOT ACCEPTABLE 2D ELEMENT TYPE
!
      CALL fwdrec(*1400,Ect)
   ENDDO
!
!     PROCESS THE 2D ELEMENT
!
 500  ngrds = elm2d(i,2)
   nwds = elm2d(i,3)
!
!     READ DATA FOR ONE 2D ELEMENT
!
 600  CALL read(*1400,*400,Ect,card,nwds,0,n)
!
!     CHECK IF STRUCTURAL ELEMENT IS CONNECTED TO ANY FLUID ELEMENT
!     MAKE SURE BISLOC FINDS FIRST OF SEVERAL POSSIBLE ENTRIES
!
   CALL bisloc(*600,card(1),Z(ielmt),7,nelm,jloc)
   DO WHILE ( jloc/=1 .AND. Z(ielmt+jloc-8)==card(1) )
      jloc = jloc - 7
   ENDDO
   DO
!
!     INSERT ELEMENT GRID POINTS INTO ELEMENT TABLE WORDS 3-6
!
      DO i = 1 , ngrds
         Z(ielmt+jloc+i) = card(i+2)
      ENDDO
      IF ( ngrds==3 ) Z(ielmt+jloc+4) = -1
!
!     CHECK IF NEXT ENTRY IS FOR THE SAME STRUCTURAL ELEMENT
!
      IF ( jloc+7>=nelmt .OR. Z(ielmt+jloc+6)/=card(1) ) GOTO 600
      jloc = jloc + 7
   ENDDO
!
!
!     PASS THROUGH ELEMENT TABLE AND CHECK THAT EACH ENTRY HAS GRIDS.
!     ALSO SWITCH THE STRUCTURE AND FLUID ELEMENTS IN THE TABLE FOR
!     FUTURE WORD WITH FLUID ID.
!
 700  lelmt = ielmt + nelmt - 1
   DO i = ielmt , lelmt , 7
      ids = Z(i)
      Z(i) = Z(i+1)
      IF ( Z(i+2)==0 ) THEN
         Error = .TRUE.
         WRITE (Nout,99001) Ufm , ids
99001    FORMAT (A23,' 8002, ELEMENT ID',I9,' ON A CFLSTR CARD DOES NOT ','REFERENCE A VALID 2D STRUCTURAL ELEMENT.')
         ids = 0
      ENDIF
      Z(i+1) = ids
   ENDDO
!
!     ALLOCATE AND ZERO THE GRID POINT CONNECTIVE TABLE AT THE BOTTOM
!     OF CORE
!
!     TABLE ENTRIES WILL BE AS FOLLOWS
!
!     POSITIVE LESS THEN 1,000,000  - NUMBER OF STRUCTURAL POINTS
!                                     CONNECTED TO THIS FLUID POINT
!     MULTIPLES OF 1,000,000        - NUMBER OF FREE SURFACE POINTS
!                                     CONNECTED TO THIS FLUID POINT
!     NEGATIVE                      - NUMBER OF STRUCTURAL POINTS
!                                     CONNECTED TO THIS STRUCTURAL
!                                     POINT
!
   Igrid = Ibuf3 - ngrdt - 1
   IF ( Igrid<Icore ) THEN
      n = -8
      CALL mesage(n,file,name)
      GOTO 1700
   ELSE
      Ngrid = ngrdt
      lgrid = Ibuf3 - 1
      DO i = Igrid , lgrid
         Z(i) = 0
      ENDDO
!
!     LOCATE CFREE CARDS ON GEOM2 AND ADD THEM TO THE ELEMENT TABLE.
!     THESE ELEMENT RECORDS WILL APPEAR AS FOLLOWS
!
!                  WORD      DESCRIPTION
!
!                  1         FLUID ELEMENT ID
!                  2         -1
!                  3         FACE ID
!                  4-6       ZERO
!                  7         GRAV ID
!
      file = Geom2
      CALL locate(*800,Z(Ibuf1),cfree,id)
      Nofree = 1
      DO
         CALL read(*1500,*900,Geom2,id,3,0,n)
         IF ( Icore+7>=Igrid ) THEN
            n = -8
            CALL mesage(n,file,name)
            GOTO 1700
         ELSE
            Z(Icore) = id(1)
            Z(Icore+1) = -1
            Z(Icore+2) = id(3)
            Z(Icore+3) = 0
            Z(Icore+4) = 0
            Z(Icore+5) = 0
            Z(Icore+6) = id(2)
            Icore = Icore + 7
         ENDIF
      ENDDO
   ENDIF
!
!     NO CFREE CARDS - THIS IMPLIES THAT THERE WILL BE NO FREE SURFACE
!
 800  Nofree = -1
!
!     COMPLETE CORE ALLOCATION FOR THIS PHASE
!
 900  nelmt = Icore - ielmt
   nelm = nelmt/7
   CALL close(Geom2,1)
!
!     SORT ELEMENT TABLE BY FLUID ID
!
   CALL sort(0,0,7,1,Z(ielmt),nelmt)
!
!     OPEN FBELM AND FRELM SCRATCH FILES
!
   CALL gopen(Fbelm,Z(Ibuf1),1)
   CALL gopen(Frelm,Z(Ibuf3),1)
!
!     READ ECT AND PROCESS FLUID ELEMENTS
!
   file = Ect
   CALL rewind(Ect)
   CALL fwdrec(*1500,Ect)
 1000 DO
      CALL read(*1300,*1600,Ect,card,3,0,n)
      DO i = 1 , nfl
         IF ( card(3)==elmfl(i,1) ) GOTO 1100
      ENDDO
!
!     SKIP RECORD BECAUSE NOT FLUID ELEMENT TYPE
!
      CALL fwdrec(*1400,Ect)
   ENDDO
!
!     PRECESS FLUID ELEMENT
!
 1100 ntype = elmfl(i,1)
   nwds = elmfl(i,3)
!
!     READ DATA FOR ONE FLUID ELEMENT
!
 1200 CALL read(*1400,*1000,Ect,card,nwds,0,n)
!
!     FIND IF FLUID ELEMENT IS ON FREE SURFACE OR STRUCTURAL BOUNDARY.
!     MAKE SURE BISLOC FINDS THE FIRST OF SEVERAL POSSIBLE ENTRIES.
!
   CALL bisloc(*1200,card(1),Z(ielmt),7,nelm,jloc)
   DO WHILE ( jloc/=1 .AND. Z(ielmt+jloc-8)==card(1) )
      jloc = jloc - 7
   ENDDO
   DO
!
!     DETERMINE IF ENTRY IS EITHER A BOUNDARY OR FREE SURFACE
!     DESCRIPTION - IGNORE ENTRY IF IT WAS IN ERROR DURING STRUCTURAL
!     ELEMENT PROCESSING
!
      IF ( Z(ielmt+jloc)>0 ) THEN
!
!     THIS ENTRY DESCRIBES THE FLUID / STRUCTURE BOUNDARY - FIND THE
!     FLUID GRID POINTS WHICH COINCIDE WITH THE STRUCTURAL POINTS
!
         CALL flface(ntype,card,Z(ielmt+jloc-1),grid)
         IF ( .NOT.(Error) ) THEN
!
!     INCLUDE CONNECTIONS IN GRID POINT CONNECTIVITY TABLE
!        1) NUMBER OF STRUCTURE GRID POINTS CONNECTED TO EACH FLUID
!        2) NUMBER OF STRUCTURAL GRID POINTS CONNECTED TO EACH
!           STRUCTURE POINT
!
            ngrdf = 4
            IF ( grid(4)<0 ) ngrdf = 3
            ngrds = 4
            IF ( Z(ielmt+jloc+4)<0 ) ngrds = 3
            DO i = 1 , ngrdf
               j = grid(i) - 1
               Z(Igrid+j) = Z(Igrid+j) + ngrds
            ENDDO
            DO i = 1 , ngrds
               j = Z(ielmt+jloc+i) - 1
               Z(Igrid+j) = Z(Igrid+j) - ngrds
            ENDDO
!
!     WRITE 12 WORD RECORD FOR THIS ENTRY ON FBELM
!
!                  WORD      DESCRIPTION
!
!                  1         FLUID ELEMENT ID
!                  2         STRUCTURAL ELEMENT ID
!                  3-6       STRUCTURE GRID POINTS
!                  7         GRAVITY LOAD ID
!                  8         MATERIAL ID
!                  9-12      FLUID GRID POINTS
!
            CALL write(Fbelm,Z(ielmt+jloc-1),7,0)
            CALL write(Fbelm,card(2),1,0)
            CALL write(Fbelm,grid,4,0)
         ENDIF
      ELSEIF ( Z(ielmt+jloc)==-1 ) THEN
!
!     THIS ENTRY DESCRIBES THE FREE SURFACE - FIND THE FLUIDS GRID
!     POINTS WHICH DEFINE THE FACE ID GIVEN
!
         CALL flface(ntype,card,Z(ielmt+jloc-1),grid)
         IF ( .NOT.(Error) ) THEN
!
!     INCLUDE CONNECTIONS IN GRID POINT CONNECTIVITY TABLE
!        1) NUMBER OF FREE SURFACE POINTS CONNECTED TO THIS FREE
!           SURFACE POINT
!
            ngrdf = 4
            IF ( grid(4)<0 ) ngrdf = 3
            DO i = 1 , ngrdf
               j = grid(i) - 1
               Z(Igrid+j) = Z(Igrid+j) + ngrdf*1000000
            ENDDO
!
!     WRITE 7 WORD RECORD ON FRELM FILE
!
!                  WORD      DESCRIPTION
!
!                  1         FLUID ELEMENT ID
!                  2         MATERIAL FLAG
!                  3-6       FLUID GRID POINTS
!                  7         GRAVITY LOAD ID
!
            Z(ielmt+jloc) = card(2)
            CALL write(Frelm,Z(ielmt+jloc-1),2,0)
            CALL write(Frelm,grid,4,0)
            CALL write(Frelm,Z(ielmt+jloc+5),1,0)
         ENDIF
      ENDIF
!
!     FLAG THE ELEMENT TABLE ENTRY AS BEEN PROCESSED AND CHECK IF
!     THE NEXT ENTRY IS FOR THE SAME FLUID ELEMENT
!
      Z(ielmt+jloc) = -2
      IF ( jloc+7>=nelmt .OR. Z(ielmt+jloc+6)/=card(1) ) GOTO 1200
      jloc = jloc + 7
   ENDDO
!
 1300 CALL close(Ect,1)
   CALL close(Fbelm,1)
   CALL close(Frelm,1)
   mcb(1) = Fbelm
   mcb(2) = ngrdt
   mcb(3) = nelm
   CALL wrttrl(mcb)
   mcb(1) = Frelm
   CALL wrttrl(mcb)
!
!     MAKE ONE FINAL PASS THROUGH ELEMENT TABLE AND VERIFY THAT
!     EVERY FLUID ELEMENT WAS PROCESSED
!
   lelmt = ielmt + nelmt - 1
   DO i = ielmt , lelmt , 7
      IF ( Z(i+1)/=-2 ) THEN
         IF ( Z(i+1)==-1 ) THEN
!
            Error = .TRUE.
            WRITE (Nout,99002) Ufm , Z(i)
99002       FORMAT (A23,' 8004. ELEMENT ID',I9,' ON A CFFREE CARD DOES NOT ','REFERENCE A VALID FLUID ELEMENT.')
         ELSE
            Error = .TRUE.
            WRITE (Nout,99003) Ufm , Z(i)
99003       FORMAT (A23,' 8003. ELEMENT ID',I9,' ON A CFLSTR CARD DOES NOT ','REFERENCE A VALID FLUID ELEMENT.')
         ENDIF
      ENDIF
!
   ENDDO
!
!     ELEMENT TABLE IS NO LONGER NEEDED SO DELETE IT AND RETURN
!
   Icore = ielmt
   RETURN
!
!     ERROR CONDITIONS
!
 1400 n = -1
   CALL mesage(n,file,name)
   GOTO 1700
 1500 n = -2
   CALL mesage(n,file,name)
   GOTO 1700
 1600 n = -3
   CALL mesage(n,file,name)
!
!     NO FLUID / STRUCTURE BOUNDARY DEFINED.  FATAL ERROR BECAUSE DMAP
!     CANNOT HANDLE THIS CONDITION
!
 1700 Error = .TRUE.
   WRITE (Nout,99004) Ufm
!
!     ERROR FORMATS
!
99004 FORMAT (A23,' 8001. THERE MUST BE A FLUID/STRUCTURE BOUNDARY IN ','HYDROELASTIC ANALYSIS.')
   RETURN
END SUBROUTINE flbelm