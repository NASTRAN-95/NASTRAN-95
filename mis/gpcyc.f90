
SUBROUTINE gpcyc
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ctype(2) , Isk(6) , Itwo(32) , Iua , Iz(1) , Ksystm(65) , Nogo , Nout , Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /bitpos/ Isk , Iua
   COMMON /blank / Ctype , Nogo
   COMMON /system/ Ksystm
   COMMON /two   / Itwo
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Iz
!
! Local variable declarations
!
   INTEGER andf , korsz
   INTEGER blk , cycd , cyjoin(2) , cyl , eqexin , file , geom4 , i , ib(5) , ibb(4) , iblen , ibuf1 , ibuf2 , ic1 , ic2 , icid ,   &
         & icm , idum , if , iflag , ip , ip1 , isam , isid , isid1 , isid2 , ityp , ix1 , ix2 , k , l , lcyj , lua , luset , m ,   &
         & mcb(7) , name(2) , ncord , nent , nocnt , nocy , noeq , nopar , nosid1 , np , ns1 , ns2 , nx , nz , rec , rot , scr1 ,   &
         & scr2 , sph , uset
   EXTERNAL andf
!
! End of declarations
!
!
!     GPCYC IS THE GEOMETRY PROCESSOR FOR CYCLIC PROBLEM
!
!     INPUT DATA BLOCKS - GEOM4,EQEXIN,USET
!
!     OUTPUT DATA BLOCKS - CYCD
!
!     PARAMETERS  CTYPE - INPUT,BCD -
!                 NOGO  - OUTPUT--+1 UNLESS ERROR--THEN-1
!
!     SCRATCH FILES (2)
!     DEFINITION OF VARIABLES
!     NZ       OPEN CORE LENGTH
!     NX       ORIGINAL OPEN CORE
!     NENT     NUMBER OF ENTRIES IN EQEXIN
!     ITYP     PROBLEM TYPE (ROT=0 ,OTHERWISE=1)
!     LCYJ     LENGTH OF CJOIN CARDS
!     ISID1    POINTER TO START OF SIDE 1 CZRDS
!     ISID2    POINTER TO START OF SIDE 2 CZRDS
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   DATA geom4 , eqexin , uset , cycd , scr1 , scr2 , name/101 , 102 , 103 , 201 , 301 , 302 , 4HGPCY , 4HC   /
   DATA rot/4HROT / , rec , cyl , sph/1HR , 1HC , 1HS/
   DATA cyjoin/5210 , 52/
   DATA nocy , nosid1 , isid1 , iblen , icm , isam , nocnt , nopar/4024 , 4025 , 4026 , 4027 , 4028 , 4029 , 4030 , 4032/
   DATA noeq , ncord/4037 , 4039/
   DATA mcb/7*0/ , blk/1H /
!
!
   nz = korsz(Iz)
   Nogo = 1
!
!     IBUF1 IS PRELOC BUFFER
!
   ibuf1 = nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   nz = ibuf2 - 1
   nx = nz
   IF ( nz<=0 ) CALL mesage(-8,0,name)
!
!     PUT  SECOND RECORD OF EQEXIN ITO CORE
!
   file = eqexin
   CALL gopen(eqexin,Iz(ibuf1),0)
   CALL fwdrec(*1400,eqexin)
   CALL read(*1400,*100,eqexin,Iz,nz,0,iflag)
   CALL mesage(-8,0,name)
 100  CALL close(eqexin,1)
   nent = iflag/2
!
!     DECIDE ON TYPE
!
   ityp = 1
   IF ( Ctype(1)==rot ) ityp = 0
!
!     FIND  CYJOIN CARDS ON GEOM4
!
   file = geom4
   CALL preloc(*1200,Iz(ibuf1),geom4)
   CALL locate(*1500,Iz(ibuf1),cyjoin,idum)
   nz = nz - iflag
   k = iflag + 1
   CALL read(*1400,*200,geom4,Iz(k),nz,0,lcyj)
   CALL mesage(-8,0,name)
 200  CALL close(geom4,1)
   lcyj = lcyj + k - 1
   IF ( Iz(k)==1 ) THEN
!
!     FIND SIDE TWO DATA
!
      l = k
      DO WHILE ( l<=lcyj )
         IF ( Iz(l)==-1 ) THEN
!
!     END OF CARD FOUND
!
            IF ( l+1>lcyj ) EXIT
            IF ( Iz(l+1)==2 ) THEN
!
!     FOUND SIDE TWO LIST
!
               isid2 = l + 1
               IF ( ityp/=0 ) THEN
!
!     1. DIHEDRAL TYPE
!
!     BUILD FIVE WORD LIST
!
!
!     FIVE WORD ENTRY FOR EACH POINT IN SIDE 1 OR SIDE TWO LOOKS AS
!         FOLLOWS
!     1        SIDE (1,2)
!     2        COORD SYS (R = 1,C = 1,S = 2,BLANK = 0)
!     3        CODE ( 1 = GRID   2 = SCALAR)
!     4        INTERNAL INDEX (SIL)
!     5        GRID ID (EXTERNAL)
!
                  l = k
                  CALL gopen(scr1,Iz(ibuf1),1)
                  GOTO 800
               ELSE
!
!     CHECK LENGTH OF SIDE TWO LIST
!
                  ns1 = isid2 - k - 4
                  ns2 = lcyj - isid2 - 3
                  IF ( ns1==ns2 ) THEN
!
!     BUILD 5 WORDS FOR EACH PAIR
!
!
!     FIVE WORD ENTRY FOR EACH PAIR APPEARS AS FOLLOWS
!
!     1        CODE(1 = GRID   2 = SCALAR)
!     2        INTERNAL INDEX (SIL)      SIDE 1
!     3        GRID ID (EXTERNAL)       SIDE 1
!     4        INTERNAL INDEX (SIL)      SIDE 2
!     5        GRID ID (EXTERNAL)       SIDE 2
!
                     CALL gopen(scr1,Iz(ibuf1),1)
                     l = isid2 + 3
                     k = k + 3
                     DO i = 1 , ns1
                        IF ( Iz(k)/=Iz(l) ) THEN
                           ip = Iz(k)
                           CALL bisloc(*1600,ip,Iz(1),2,nent,m)
                           ix1 = Iz(m+1)/10
                           ic1 = Iz(m+1) - ix1*10
                           ip = Iz(l)
                           CALL bisloc(*1600,ip,Iz(1),2,nent,m)
                           ix2 = Iz(m+1)/10
                           ic2 = Iz(m+1) - ix2*10
                           IF ( ic1==ic2 ) THEN
                              ib(1) = ic1
                              ib(2) = ix1
                              ib(3) = Iz(k)
                              ib(4) = ix2
                              ib(5) = Iz(l)
                              CALL write(scr1,ib,5,0)
                              k = k + 1
                              l = l + 1
                           ELSE
                              WRITE (Nout,99014) Ufm , icm
                              WRITE (Nout,99001) Iz(k) , Iz(l)
99001                         FORMAT ('0THE CODE FOR GRID POINT',I10,' DOES NOT MATCH THE CODE',' FOR GRID POINT',I10)
                              GOTO 1700
                           ENDIF
                        ELSE
                           WRITE (Nout,99014) Ufm , isam
                           WRITE (Nout,99002) Iz(k)
99002                      FORMAT ('0GRID POINT',I10,' APPEARS IN BOTH SIDE LISTS.')
                           GOTO 1700
                        ENDIF
                     ENDDO
                     GOTO 300
                  ELSE
                     WRITE (Nout,99014) Ufm , iblen
                     WRITE (Nout,99003)
99003                FORMAT ('0NUMBER OF ENTRIES IN SIDE 1 NOT EQUAL TO NUMBER IN ','SIDE 2')
                     Nogo = -1
                     GOTO 1700
                  ENDIF
               ENDIF
            ELSEIF ( ityp==1 ) THEN
               l = l + 1
            ELSE
               WRITE (Nout,99014) Ufm , isid1
               WRITE (Nout,99004)
99004          FORMAT ('0TOO MANY SIDE 1 CARDS.')
               GOTO 1700
            ENDIF
         ELSE
            l = l + 1
         ENDIF
      ENDDO
      WRITE (Nout,99014) Ufm , nosid1
      WRITE (Nout,99005)
99005 FORMAT ('0NO SIDE 2 DATA FOUND.')
   ELSE
      WRITE (Nout,99014) Ufm , nosid1
      WRITE (Nout,99006)
99006 FORMAT ('0NO SIDE 1 DATA FOUND.')
   ENDIF
   GOTO 1700
 300  CALL write(scr1,0,0,1)
   CALL close(scr1,1)
!
!     SET UP USET
!
   CALL gopen(uset,Iz(ibuf1),0)
   file = uset
   nz = nx
   CALL read(*1400,*400,uset,Iz,nz,0,luset)
   CALL mesage(-8,0,name)
 400  CALL close(uset,1)
!
!     SET UP REDUCED USET TABLE
!
   k = 0
   m = Itwo(Iua)
   DO i = 1 , luset
      IF ( andf(Iz(i),m)/=0 ) THEN
         k = k + 1
         Iz(i) = -k
      ELSE
         Iz(i) = 0
      ENDIF
   ENDDO
   lua = k
!
!     FORM SILA VALUES
!
   file = scr1
   CALL gopen(scr1,Iz(ibuf1),0)
   CALL gopen(scr2,Iz(ibuf2),1)
   IF ( ityp/=0 ) GOTO 900
 500  CALL read(*1400,*600,scr1,ib(1),5,0,iflag)
   np = 1
   if = 0
   IF ( ib(1)==1 ) np = 6
   k = 0
   DO
      l = ib(2) + k
      m = ib(4) + k
!
!     IF NEITHER IGNORE
!
      IF ( Iz(l)/=0 .OR. Iz(m)/=0 ) THEN
         IF ( Iz(l)<0 .AND. Iz(m)<0 ) THEN
            if = if + 1
            ibb(1) = iabs(Iz(l))
            ibb(2) = ib(3)
            ibb(3) = iabs(Iz(m))
            ibb(4) = ib(5)
            CALL write(scr2,ibb,4,0)
         ELSE
            WRITE (Nout,99012) Uwm , nocnt
            m = k + 1
            WRITE (Nout,99007) m , ib(3) , ib(5)
99007       FORMAT ('0COMPONENT',I4,' OF GRID POINTS',I10,5H AND ,I10,' CANNOT BE CONNECTED.')
         ENDIF
      ENDIF
      k = k + 1
      IF ( k==np ) THEN
         IF ( if==0 ) THEN
            WRITE (Nout,99012) Uwm , nopar
            WRITE (Nout,99008) ib(3) , ib(5)
99008       FORMAT ('0NO COMPONENTS OF GRID POINTS',I10,5H AND ,I10,' WERE CONNECTED.')
         ENDIF
         GOTO 500
      ENDIF
   ENDDO
!
!     CLOSE UP
!
 600  CALL write(scr2,0,0,1)
   CALL close(scr1,1)
   CALL close(scr2,1)
!
!     BUILD CYCD
!
   DO i = 1 , lua
      Iz(i) = 0
   ENDDO
   file = scr2
   CALL gopen(scr2,Iz(ibuf1),0)
   IF ( ityp/=0 ) GOTO 1100
   DO
      CALL read(*1400,*700,scr2,ibb,4,0,iflag)
      k = ibb(1)
      m = ibb(3)
      IF ( Iz(k)/=0 ) THEN
         WRITE (Nout,99014) Ufm , noeq
         WRITE (Nout,99013) ibb(2)
         Nogo = -1
      ENDIF
      IF ( Iz(m)/=0 ) THEN
         WRITE (Nout,99014) Ufm , noeq
         WRITE (Nout,99013) ibb(4)
         Nogo = -1
      ENDIF
      Iz(k) = m
      Iz(m) = -k
   ENDDO
!
!     END OF PAIRS
!
 700  CALL close(scr2,1)
   CALL gopen(cycd,Iz(ibuf1),1)
   CALL write(cycd,Iz(1),lua,1)
   CALL close(cycd,1)
   mcb(1) = cycd
   mcb(2) = ityp + 1
   mcb(3) = lua
   CALL wrttrl(mcb)
   IF ( Nogo/=-1 ) RETURN
   GOTO 1700
 800  icid = Iz(l+1)
   isid = Iz(l)
   IF ( icid==rec ) icid = 1
   IF ( icid==cyl ) icid = 1
   IF ( icid==sph ) icid = 2
   IF ( icid==blk ) icid = 0
   l = l + 3
   DO WHILE ( Iz(l)/=-1 )
      ip = Iz(l)
      CALL bisloc(*1600,ip,Iz(1),2,nent,m)
      ib(1) = isid
      ib(2) = icid
      ib(4) = Iz(m+1)/10
      ib(3) = Iz(m+1) - ib(4)*10
      ib(5) = ip
      CALL write(scr1,ib,5,0)
      l = l + 1
   ENDDO
!
!     END OF LIST
!
   IF ( l>=lcyj ) GOTO 300
   l = l + 1
   GOTO 800
!
!     END OF CYJOIN LISTS
!
!
!     PRODUCE CYCD CODES
!
 900  CALL read(*1400,*600,scr1,ib(1),5,0,iflag)
   np = 1
   IF ( ib(3)==1 ) np = 6
   if = 0
   k = 0
   IF ( ib(1)==2 ) ib(1) = ib(1) + 1
 1000 l = ib(4) + k
   IF ( Iz(l)/=0 ) THEN
!
!     POINT IS IN  A SET
!
      ibb(2) = iabs(Iz(l))
      ibb(3) = ib(5)
      IF ( ib(3)==2 ) THEN
!
!     EVEN
!
         m = 0
      ELSEIF ( ib(2)==1 ) THEN
!
!     RECTANGULAR OR CYL
!
         IF ( mod(k+1,2)==1 ) THEN
            m = 0
         ELSE
            m = 1
         ENDIF
      ELSEIF ( ib(2)==2 ) THEN
!
!     SPH
!
         IF ( k<2 .OR. k==5 .OR. np<3 .OR. np==6 ) THEN
            m = 0
         ELSE
            m = 1
         ENDIF
      ELSE
!
!     COORD SYS = 0
!
         WRITE (Nout,99014) Ufm , ncord
         WRITE (Nout,99009) ibb(3)
99009    FORMAT ('0NO COORDINATE SYSTEM DEFINED FOR GRID POINT',I10)
         Nogo = -1
         m = 0
      ENDIF
      ibb(1) = ib(1) + m
      if = if + 1
      CALL write(scr2,ibb,3,0)
   ENDIF
   k = k + 1
   IF ( k/=np ) GOTO 1000
   IF ( if==0 ) THEN
      WRITE (Nout,99012) Uwm , nopar
      WRITE (Nout,99010) ib(5)
99010 FORMAT ('0NO COMPONENTS OF GRID POINT',I10,' WERE IN THE A SET')
   ENDIF
   GOTO 900
 1100 DO
!
!     BUILD CYCD FOR DIH
!
      CALL read(*1200,*700,scr2,ibb,3,0,iflag)
      k = ibb(2)
      IF ( Iz(k)/=0 ) THEN
         WRITE (Nout,99014) Ufm , noeq
         WRITE (Nout,99013) ibb(3)
         Nogo = -1
      ENDIF
      Iz(k) = ibb(1)
   ENDDO
!
!     ERROR MESSAGES
!
 1200 ip1 = -1
 1300 CALL mesage(ip1,file,name)
   RETURN
 1400 ip1 = -2
   GOTO 1300
 1500 WRITE (Nout,99014) Ufm , nocy
   WRITE (Nout,99011)
99011 FORMAT ('0NO CYJOIN CARDS WERE SUPPLIED.')
   GOTO 1700
 1600 CALL mesage(-30,2,ip)
 1700 CALL mesage(-61,0,name)
99012 FORMAT (A25,I5)
99013 FORMAT ('0GRID POINT',I10,' IS LISTED MORE THAN ONCE.')
99014 FORMAT (A23,I5)
END SUBROUTINE gpcyc
