!*==gpcyc.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gpcyc
   USE c_bitpos
   USE c_blank
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blk , cycd , cyl , eqexin , geom4 , iblen , icm , isam , isid1 , ncord , nocnt , nocy , noeq , nopar , nosid1 ,&
                   & rec , rot , scr1 , scr2 , sph , uset
   INTEGER , DIMENSION(2) , SAVE :: cyjoin , name
   INTEGER :: file , i , ibuf1 , ibuf2 , ic1 , ic2 , icid , idum , if , iflag , ip , ip1 , isid , isid2 , ityp , ix1 , ix2 , k , l ,&
            & lcyj , lua , luset , m , nent , nout , np , ns1 , ns2 , nx , nz , sysbuf
   INTEGER , DIMENSION(5) :: ib
   INTEGER , DIMENSION(4) :: ibb
   INTEGER , DIMENSION(7) , SAVE :: mcb
   EXTERNAL andf , bisloc , close , fwdrec , gopen , korsz , locate , mesage , preloc , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   DATA geom4 , eqexin , uset , cycd , scr1 , scr2 , name/101 , 102 , 103 , 201 , 301 , 302 , 4HGPCY , 4HC   /
   DATA rot/4HROT / , rec , cyl , sph/1HR , 1HC , 1HS/
   DATA cyjoin/5210 , 52/
   DATA nocy , nosid1 , isid1 , iblen , icm , isam , nocnt , nopar/4024 , 4025 , 4026 , 4027 , 4028 , 4029 , 4030 , 4032/
   DATA noeq , ncord/4037 , 4039/
   DATA mcb/7*0/ , blk/1H /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         nz = korsz(iz)
         nogo = 1
!
!     IBUF1 IS PRELOC BUFFER
!
         ibuf1 = nz - sysbuf
         ibuf2 = ibuf1 - sysbuf
         nz = ibuf2 - 1
         nx = nz
         IF ( nz<=0 ) CALL mesage(-8,0,name)
!
!     PUT  SECOND RECORD OF EQEXIN ITO CORE
!
         file = eqexin
         CALL gopen(eqexin,iz(ibuf1),0)
         CALL fwdrec(*140,eqexin)
         CALL read(*140,*20,eqexin,iz,nz,0,iflag)
         CALL mesage(-8,0,name)
 20      CALL close(eqexin,1)
         nent = iflag/2
!
!     DECIDE ON TYPE
!
         ityp = 1
         IF ( ctype(1)==rot ) ityp = 0
!
!     FIND  CYJOIN CARDS ON GEOM4
!
         file = geom4
         CALL preloc(*120,iz(ibuf1),geom4)
         CALL locate(*160,iz(ibuf1),cyjoin,idum)
         nz = nz - iflag
         k = iflag + 1
         CALL read(*140,*40,geom4,iz(k),nz,0,lcyj)
         CALL mesage(-8,0,name)
 40      CALL close(geom4,1)
         lcyj = lcyj + k - 1
         IF ( iz(k)==1 ) THEN
!
!     FIND SIDE TWO DATA
!
            l = k
            SPAG_Loop_1_1: DO WHILE ( l<=lcyj )
               IF ( iz(l)==-1 ) THEN
!
!     END OF CARD FOUND
!
                  IF ( l+1>lcyj ) EXIT SPAG_Loop_1_1
                  IF ( iz(l+1)==2 ) THEN
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
                        CALL gopen(scr1,iz(ibuf1),1)
                        DO
                           icid = iz(l+1)
                           isid = iz(l)
                           IF ( icid==rec ) icid = 1
                           IF ( icid==cyl ) icid = 1
                           IF ( icid==sph ) icid = 2
                           IF ( icid==blk ) icid = 0
                           l = l + 3
                           DO WHILE ( iz(l)/=-1 )
                              ip = iz(l)
                              CALL bisloc(*180,ip,iz(1),2,nent,m)
                              ib(1) = isid
                              ib(2) = icid
                              ib(4) = iz(m+1)/10
                              ib(3) = iz(m+1) - ib(4)*10
                              ib(5) = ip
                              CALL write(scr1,ib,5,0)
                              l = l + 1
                           ENDDO
!
!     END OF LIST
!
                           IF ( l>=lcyj ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           l = l + 1
                        ENDDO
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
                           CALL gopen(scr1,iz(ibuf1),1)
                           l = isid2 + 3
                           k = k + 3
                           DO i = 1 , ns1
                              IF ( iz(k)/=iz(l) ) THEN
                                 ip = iz(k)
                                 CALL bisloc(*180,ip,iz(1),2,nent,m)
                                 ix1 = iz(m+1)/10
                                 ic1 = iz(m+1) - ix1*10
                                 ip = iz(l)
                                 CALL bisloc(*180,ip,iz(1),2,nent,m)
                                 ix2 = iz(m+1)/10
                                 ic2 = iz(m+1) - ix2*10
                                 IF ( ic1==ic2 ) THEN
                                    ib(1) = ic1
                                    ib(2) = ix1
                                    ib(3) = iz(k)
                                    ib(4) = ix2
                                    ib(5) = iz(l)
                                    CALL write(scr1,ib,5,0)
                                    k = k + 1
                                    l = l + 1
                                 ELSE
                                    WRITE (nout,99014) ufm , icm
                                    WRITE (nout,99001) iz(k) , iz(l)
99001                               FORMAT ('0THE CODE FOR GRID POINT',I10,' DOES NOT MATCH THE CODE',' FOR GRID POINT',I10)
                                    spag_nextblock_1 = 5
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                              ELSE
                                 WRITE (nout,99014) ufm , isam
                                 WRITE (nout,99002) iz(k)
99002                            FORMAT ('0GRID POINT',I10,' APPEARS IN BOTH SIDE LISTS.')
                                 spag_nextblock_1 = 5
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                           ENDDO
                           spag_nextblock_1 = 2
                        ELSE
                           WRITE (nout,99014) ufm , iblen
                           WRITE (nout,99003)
99003                      FORMAT ('0NUMBER OF ENTRIES IN SIDE 1 NOT EQUAL TO NUMBER IN ','SIDE 2')
                           nogo = -1
                           spag_nextblock_1 = 5
                        ENDIF
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ELSEIF ( ityp==1 ) THEN
                     l = l + 1
                  ELSE
                     WRITE (nout,99014) ufm , isid1
                     WRITE (nout,99004)
99004                FORMAT ('0TOO MANY SIDE 1 CARDS.')
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSE
                  l = l + 1
               ENDIF
            ENDDO SPAG_Loop_1_1
            WRITE (nout,99014) ufm , nosid1
            WRITE (nout,99005)
99005       FORMAT ('0NO SIDE 2 DATA FOUND.')
         ELSE
            WRITE (nout,99014) ufm , nosid1
            WRITE (nout,99006)
99006       FORMAT ('0NO SIDE 1 DATA FOUND.')
         ENDIF
         spag_nextblock_1 = 5
      CASE (2)
         CALL write(scr1,0,0,1)
         CALL close(scr1,1)
!
!     SET UP USET
!
         CALL gopen(uset,iz(ibuf1),0)
         file = uset
         nz = nx
         CALL read(*140,*60,uset,iz,nz,0,luset)
         CALL mesage(-8,0,name)
 60      CALL close(uset,1)
!
!     SET UP REDUCED USET TABLE
!
         k = 0
         m = itwo(iua)
         DO i = 1 , luset
            IF ( andf(iz(i),m)/=0 ) THEN
               k = k + 1
               iz(i) = -k
            ELSE
               iz(i) = 0
            ENDIF
         ENDDO
         lua = k
!
!     FORM SILA VALUES
!
         file = scr1
         CALL gopen(scr1,iz(ibuf1),0)
         CALL gopen(scr2,iz(ibuf2),1)
         IF ( ityp/=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_2: DO
            CALL read(*140,*80,scr1,ib(1),5,0,iflag)
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
               IF ( iz(l)/=0 .OR. iz(m)/=0 ) THEN
                  IF ( iz(l)<0 .AND. iz(m)<0 ) THEN
                     if = if + 1
                     ibb(1) = iabs(iz(l))
                     ibb(2) = ib(3)
                     ibb(3) = iabs(iz(m))
                     ibb(4) = ib(5)
                     CALL write(scr2,ibb,4,0)
                  ELSE
                     WRITE (nout,99012) uwm , nocnt
                     m = k + 1
                     WRITE (nout,99007) m , ib(3) , ib(5)
99007                FORMAT ('0COMPONENT',I4,' OF GRID POINTS',I10,5H AND ,I10,' CANNOT BE CONNECTED.')
                  ENDIF
               ENDIF
               k = k + 1
               IF ( k==np ) THEN
                  IF ( if==0 ) THEN
                     WRITE (nout,99012) uwm , nopar
                     WRITE (nout,99008) ib(3) , ib(5)
99008                FORMAT ('0NO COMPONENTS OF GRID POINTS',I10,5H AND ,I10,' WERE CONNECTED.')
                  ENDIF
                  CYCLE SPAG_Loop_1_2
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
!
!     CLOSE UP
!
 80      CALL write(scr2,0,0,1)
         CALL close(scr1,1)
         CALL close(scr2,1)
!
!     BUILD CYCD
!
         DO i = 1 , lua
            iz(i) = 0
         ENDDO
         file = scr2
         CALL gopen(scr2,iz(ibuf1),0)
         IF ( ityp/=0 ) THEN
            DO
!
!     BUILD CYCD FOR DIH
!
               CALL read(*120,*100,scr2,ibb,3,0,iflag)
               k = ibb(2)
               IF ( iz(k)/=0 ) THEN
                  WRITE (nout,99014) ufm , noeq
                  WRITE (nout,99013) ibb(3)
                  nogo = -1
               ENDIF
               iz(k) = ibb(1)
            ENDDO
            GOTO 120
         ELSE
            DO
               CALL read(*140,*100,scr2,ibb,4,0,iflag)
               k = ibb(1)
               m = ibb(3)
               IF ( iz(k)/=0 ) THEN
                  WRITE (nout,99014) ufm , noeq
                  WRITE (nout,99013) ibb(2)
                  nogo = -1
               ENDIF
               IF ( iz(m)/=0 ) THEN
                  WRITE (nout,99014) ufm , noeq
                  WRITE (nout,99013) ibb(4)
                  nogo = -1
               ENDIF
               iz(k) = m
               iz(m) = -k
            ENDDO
         ENDIF
!
!     END OF PAIRS
!
 100     CALL close(scr2,1)
         CALL gopen(cycd,iz(ibuf1),1)
         CALL write(cycd,iz(1),lua,1)
         CALL close(cycd,1)
         mcb(1) = cycd
         mcb(2) = ityp + 1
         mcb(3) = lua
         CALL wrttrl(mcb)
         IF ( nogo/=-1 ) RETURN
         spag_nextblock_1 = 5
      CASE (3)
!
!     END OF CYJOIN LISTS
!
!
!     PRODUCE CYCD CODES
!
         CALL read(*140,*80,scr1,ib(1),5,0,iflag)
         np = 1
         IF ( ib(3)==1 ) np = 6
         if = 0
         k = 0
         IF ( ib(1)==2 ) ib(1) = ib(1) + 1
         DO
            l = ib(4) + k
            IF ( iz(l)/=0 ) THEN
!
!     POINT IS IN  A SET
!
               ibb(2) = iabs(iz(l))
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
                  WRITE (nout,99014) ufm , ncord
                  WRITE (nout,99009) ibb(3)
99009             FORMAT ('0NO COORDINATE SYSTEM DEFINED FOR GRID POINT',I10)
                  nogo = -1
                  m = 0
               ENDIF
               ibb(1) = ib(1) + m
               if = if + 1
               CALL write(scr2,ibb,3,0)
            ENDIF
            k = k + 1
            IF ( k==np ) THEN
               IF ( if==0 ) THEN
                  WRITE (nout,99012) uwm , nopar
                  WRITE (nout,99010) ib(5)
99010             FORMAT ('0NO COMPONENTS OF GRID POINT',I10,' WERE IN THE A SET')
               ENDIF
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     ERROR MESSAGES
!
 120     ip1 = -1
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(ip1,file,name)
         RETURN
 140     ip1 = -2
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 160     WRITE (nout,99014) ufm , nocy
         WRITE (nout,99011)
99011    FORMAT ('0NO CYJOIN CARDS WERE SUPPLIED.')
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 180     CALL mesage(-30,2,ip)
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(-61,0,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99012 FORMAT (A25,I5)
99013 FORMAT ('0GRID POINT',I10,' IS LISTED MORE THAN ONCE.')
99014 FORMAT (A23,I5)
END SUBROUTINE gpcyc
