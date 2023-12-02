!*==lodapp.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE lodapp
   IMPLICIT NONE
   USE c_blank
   USE c_names
   USE c_packx
   USE c_parmeg
   USE c_sof
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(4000) :: adump
   REAL , SAVE :: blnk
   INTEGER :: i , ib1 , ib2 , ib3 , iblk , ibuf1 , ichk , icore , idrc1 , idrc2 , idry , idump , igo , ii , iloop , imdi , imergn , &
            & ind , ips , irw , iss , itest , itime1 , itime2 , itloap , itlods , itused , iuapp , iuoap , iuove , iuvec , k , l ,  &
            & lmergt , lnewlt , lvect , m , n , naf , nas , nbasa , nbasn , nbasp , nchave , ncnt , ncore , ndel1 , nfini , nitem , &
            & niz , nl , nlbasa , nlbasp , nldsa , nldsp , nloada , nloadn , nloadp , nloads , nloop , nmergf , nmergn , nmergs ,   &
            & nmrvcf , nmrvcn , nmrvcs , nnew1 , nnewf , nnews , npf , nps , ns , ns1 , nstart , nsubs , nwds , nwords
   INTEGER , DIMENSION(1) :: icorx , iz
   INTEGER , SAVE :: ipapp , ipoap , iscr1 , iscr2 , iscr3 , iscr4 , iscr5 , iscr6 , iscr7 , iscr8 , nloap , nlods , npapp , npoap ,&
                   & npove , npvec
   LOGICAL :: llsub , lmerg , lpapp , lpoap , lpove , lpvec
   INTEGER , DIMENSION(7) :: mcbloc
   INTEGER , DIMENSION(2) :: name , namell , nn
   INTEGER , DIMENSION(2) , SAVE :: nprog
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     THIS MODULE APPENDS NEW LOAD VECTORS (PAPP AND POAP) TO THE
!     SUBSTRUCTURE  -NAME-.  THE NEW VECTORS ARE MERGED WITH ALREADY
!     EXISTING (PVEC AND POVE) MATRICES OR ARE SIMPLY RECOPIED AS
!     THE NEW PVEC AND POVE ITEMS. LOAP DATA IS ALSO MERGED WITH THE
!     LODS DATA OR IS SIMPLY COPIED AS THE NEW LODS ITEM.
!
!     SOF ITEMS -
!
!     LOAP - APPENDED LOAD SET IDENTIFICATION TABLE
!     PAPP - APPENDED LOAD MATRICES (G-SET)
!     POAP - APPENDED LOAD MATRICES (O-SET)
!     LODS - LOAD SET IDENTIFICATION TABLE **BECOMES THE NEW LODS**
!     PVEC - LOAD MATRICES (G-SET)         **BECOMES THE NEW PVEC**
!     POVE - LOAD MATRICES (O-SET)         **BECOMES THE NEW POVE**
!
   !>>>>EQUIVALENCE (Icorx(1),Rz(1))
   !>>>>EQUIVALENCE (Rz(1),Iz(1))
   DATA ipapp , ipoap/101 , 102/
   DATA iscr1 , iscr2 , iscr3 , iscr4 , iscr5 , iscr6 , iscr7 , iscr8/301 , 302 , 303 , 304 , 305 , 306 , 307 , 308/
   DATA npapp , npoap , npvec , npove , nloap , nlods/4HPAPP , 4HPOAP , 4HPVEC , 4HPOVE , 4HLOAP , 4HLODS/
   DATA nprog/4HLODA , 4HPP  /
   DATA blnk/4H    /
!
!     INITIALIZE PARAMETERS
!
   CALL tmtogo(itime1)
   name(1) = buf(1)
   name(2) = buf(2)
   idry = buf(3)
   ncore = korsz(iz(1))
!
!     INITIALIZE OPEN CORE - THERE ARE NIZ WORDS AVAILABLE
!
   ib1 = ncore - (isbuff+1)
   ib2 = ib1 - isbuff - 1
   ib3 = ib2 - isbuff
   ibuf1 = ib3 - isbuff
   niz = ibuf1 - 1
   nstart = 1
!
!     TEST CORE
!
   nchave = niz
   IF ( nchave>0 ) THEN
      CALL sofopn(iz(ib1),iz(ib2),iz(ib3))
!
!     CHECK STATUS OF SUBSTRUCTURE BEING REFERENCED - NAME
!
      CALL sfetch(name,nlods,3,igo)
      IF ( igo==4 ) GOTO 100
      IF ( idry>=0 ) THEN
!
!     CHECK LOCATION OF THE PAPP VECTOR - EITHER ON FILE IPAPP OR SOF
!
         iz(1) = ipapp
         CALL rdtrl(iz(1))
         IF ( iz(1)>0 ) THEN
            lpapp = .TRUE.
            iuapp = ipapp
         ELSE
            lpapp = .FALSE.
            iuapp = iscr1
            nitem = npapp
            CALL mtrxi(iuapp,name,nitem,0,itest)
            IF ( itest/=1 ) GOTO 200
            lpapp = .TRUE.
         ENDIF
!
!     CHECK STATUS OF THE POAP VECTOR
!     FIRST GET THE NAME OF THE LOWER LEVEL SUBSTRUCTURE WHERE
!     THE POAP ITEM TO BE USED IS LOCATED
!
         lpoap = .FALSE.
         llsub = .FALSE.
         CALL fndlvl(name,namell)
         IF ( namell(1)==blnk ) GOTO 100
         IF ( name(1)/=namell(1) .OR. name(2)/=namell(2) ) llsub = .TRUE.
         IF ( llsub ) THEN
            iz(1) = ipoap
            CALL rdtrl(iz(1))
            IF ( iz(1)>0 ) THEN
               lpoap = .TRUE.
               iuoap = ipoap
            ELSE
               iuoap = iscr2
               nitem = npoap
               CALL mtrxi(iuoap,namell,nitem,0,itest)
               IF ( itest==1 ) lpoap = .TRUE.
            ENDIF
         ENDIF
!
!     ESTABLISH TYPE OF CASE BEING RUN, I.E. THE CASE NO. BEING DEFINED
!     BY  NN(1) AND NN(2).
!
         nn(1) = 0
         nn(2) = 0
!
!     CHECK STATUS OF PVEC AND POVE VECTORS
!
!     1) PVEC
!
         lpvec = .TRUE.
         iz(1) = 0
         CALL softrl(name,npvec,iz(1))
         IF ( iz(1)/=1 ) lpvec = .FALSE.
!
!     2) POVE
!
         lpove = .TRUE.
         iz(1) = 0
         IF ( llsub ) CALL softrl(namell,npove,iz(1))
         IF ( iz(1)/=1 ) lpove = .FALSE.
!
!     KNOWING THE STATUS OF PAPP, PVEC, POAP, POVE DEFINE CASE NO.
!
         IF ( lpapp ) THEN
            nn(1) = 2
            IF ( lpvec ) nn(1) = 1
         ELSE
            nn(1) = 4
            IF ( lpvec ) nn(1) = 3
         ENDIF
         IF ( lpoap ) THEN
            nn(2) = 2
            IF ( lpove ) nn(2) = 1
         ELSE
            nn(2) = 4
            IF ( lpove ) nn(2) = 3
         ENDIF
         igo = nn(2)
!
!     KNOWING NN(1) AND NN(2) THE CASE IS DEFINED
!
         IF ( nn(1)==1 ) THEN
            IF ( igo==1 .OR. igo==4 ) GOTO 50
            IF ( igo==2 .OR. igo==3 ) GOTO 300
         ENDIF
         IF ( nn(1)==2 ) THEN
            IF ( igo==1 .OR. igo==3 ) GOTO 300
            IF ( igo==2 .OR. igo==4 ) GOTO 50
         ENDIF
         IF ( nn(1)==3 ) GOTO 300
         IF ( nn(1)==4 ) GOTO 300
      ENDIF
!
!     READ IN LOAP DATA
!
 50   irw = 1
      CALL sfetch(name,nloap,irw,itloap)
      IF ( itloap>1 ) GOTO 200
      CALL suread(iz(nstart),-1,nwds,ichk)
      nl = iz(nstart+2)
      ns = iz(nstart+3)
      nfini = 4 + ns*3 + nl
      nstart = 5 + ns*2
      nas = 1
      naf = nfini
      nchave = nfini
      nsubs = ns
      IF ( nchave<=niz ) THEN
         nbasn = 4 + nsubs*2 + 1
         DO iloop = 1 , nsubs
            CALL suread(iz(nbasn),-1,nwds,ichk)
            nbasn = iz(nbasn) + 1 + nbasn
         ENDDO
         nstart = naf + 1
         nps = nstart
         lmerg = .TRUE.
!
!     IF DRY RUN (IDRY .LT. 0) CHECK FOR LODS ITEM
!
         IF ( idry>=0 ) THEN
            IF ( .NOT.(lpvec) ) THEN
!
!     SIMPLE COPY OF NEW APPENDED LOADS TO SOF
!
!     NEW  LODS  ITEM
!
               nitem = nlods
               lmerg = .FALSE.
               itest = 3
               irw = 2
               CALL sfetch(name,nitem,irw,itest)
               IF ( itest/=3 ) THEN
                  WRITE (lp,99001) sfm , nitem , name
99001             FORMAT (A25,' 6954, THE ,A4,62H ITEM EXISTS BUT HAS NO ','ASSOCIATED PVEC ITEM FOR SUBSTRUCTURE ',2A4)
                  idry = -2
               ELSE
                  nwds = 4 + 2*iz(nas+3)
                  CALL suwrt(iz(nas),nwds,2)
                  nbasn = nas + nwds
                  DO n = 1 , nsubs
                     nwds = iz(nbasn) + 1
                     CALL suwrt(iz(nbasn),nwds,2)
                     nbasn = nbasn + nwds
                  ENDDO
!
!     END OF ITEM CALL TO SUWRT
!
                  CALL suwrt(0,0,3)
!
!     NEW  PVEC  ITEM
!
                  CALL mtrxo(iuapp,name,npvec,0,itest)
!
!     NEW  POVE  ITEM  IF ANY
!
                  CALL bug(nprog(1),101,lpoap,1)
!
!     MODULE IS FINISHED WITH THE DIRECT COPY CASE
!
                  IF ( lpoap ) CALL mtrxo(iuoap,namell,npove,0,itest)
                  WRITE (lp,99002) uim , name
99002             FORMAT (A29,' 6901, ADDITIONAL LOADS HAVE BEEN SUCCESSFULLY ','MERGED FOR SUBSTRUCTURE ',2A4)
               ENDIF
               GOTO 500
            ENDIF
         ENDIF
!
!     ITS BEEN DETERMINED THAT A MERGE OPERATION WILL TAKE PLACE.  THE
!     ONLY CHECK NOW IS TO SEE IF A LODS ITEM EXISTS.
!
         irw = 1
         nitem = nlods
         CALL sfetch(name,nitem,irw,itlods)
         IF ( itlods/=1 .AND. idry>0 ) GOTO 200
         IF ( itlods/=1 .AND. idry<0 ) GOTO 500
         CALL suread(iz(nstart),-1,nwds,ichk)
         ncnt = nwds + naf
         nl = iz(nps+2)
         ns = iz(nps+3)
         nfini = nps + 3 + 3*ns + nl
         npf = nfini
         nstart = npf + 1
         nchave = nfini
         IF ( nchave<=niz ) THEN
            nbasn = nps + 3 + 2*nsubs + 1
            DO iloop = 1 , nsubs
               CALL suread(iz(nbasn),-1,nwds,ichk)
               nbasn = nbasn + iz(nbasn) + 1
            ENDDO
            nldsa = iz(nas+2)
            nldsp = iz(nps+2)
            nloads = nldsa + nldsp
            nbasa = nas + 3 + 2*nsubs + 1
            nbasp = nps + 3 + 2*nsubs + 1
            nlbasa = iz(nbasa)
            nlbasp = iz(nbasp)
!
!     CHECK FOR DUPLICATE LOAD IDS IN THE  LOAP  AND  LODS  ITEMS.
!
            DO l = 1 , nsubs
               IF ( nlbasp/=0 .AND. nlbasa/=0 ) THEN
                  DO m = 1 , nlbasa
                     DO n = 1 , nlbasp
                        IF ( iz(nbasa+m)==iz(nbasp+n) .AND. iz(nbasa+m)/=0 ) THEN
                           WRITE (lp,99003) ufm , iz(nbasa+m) , name
99003                      FORMAT (A23,' 6955, DUPLICATE LOAD IDS DURING APPEND OPERATION.','  LOAD ID NO.',I9,' SUBSTRUCTURE ',2A4)
                           idry = -2
                        ENDIF
                     ENDDO
                  ENDDO
               ENDIF
               nbasa = nbasa + nlbasa + 1
               nbasp = nbasp + nlbasp + 1
               nlbasa = iz(nbasa)
               nlbasp = iz(nbasp)
            ENDDO
!
!     END OF RUN IF A DRY RUN(IDRY .LT. 0)
!
            IF ( idry<0 ) GOTO 500
!
!     CALCULATE LENGTH OF THE MERG AND  N E W  LODS TABLE AND THEIR
!     LOCATIONS IN OPEN CORE
!
            lmergt = 2*nsubs
            nmergs = npf + 1
            nmergf = npf + lmergt
            lnewlt = 4 + 3*nsubs + nloads
            nnews = nmergf + 1
            nnewf = nmergf + lnewlt
!
!     CREATE THE NEW LODS TABLE IN OPEN CORE - GROUP  0  FIRST
!
            iz(nnews) = iz(nas)
            iz(nnews+1) = iz(nas+1)
            iz(nnews+2) = nloads
            iz(nnews+3) = nsubs
            nloop = 2*nsubs
            nnew1 = nnews + 3
            ndel1 = nas + 3
            DO ns1 = 1 , nloop
               iz(nnew1+ns1) = iz(ndel1+ns1)
            ENDDO
!
!     COMPLETION OF THE NEW LODS TABLE - GROUPS  1  THRU  NSUBS  --  AND
!     CREATION OF THE MERGE TABLE
!
            nbasn = nnew1 + nloop + 1
            nbasa = nas + 3 + 2*nsubs + 1
            nbasp = nps + 3 + 2*nsubs + 1
            nloada = iz(nbasa)
            nloadp = iz(nbasp)
            nloadn = nloada + nloadp
            nmergn = nmergs
            imergn = 1
!
!     ZERO THE MERG TABLE LOCATION
!
            DO i = 1 , lmergt
               iz(npf+i) = 0
            ENDDO
            DO iloop = 1 , nsubs
               iz(nbasn) = nloadn
               IF ( nloadp/=0 ) THEN
                  DO n = 1 , nloadp
                     iz(nbasn+n) = iz(nbasp+n)
                  ENDDO
               ENDIF
               nbasn = nbasn + nloadp
               IF ( nloada/=0 ) THEN
                  DO n = 1 , nloada
                     iz(nbasn+n) = iz(nbasa+n)
                  ENDDO
               ENDIF
!
!     LOCATION IN THE MERGE TABLE OF THE  1(S)
!
               imergn = imergn + nloadp
               iz(nmergn) = imergn
               imergn = imergn + nloada
               iz(nmergn+1) = nloada
               nmergn = nmergn + 2
               nbasn = nbasn + nloada + 1
               IF ( iloop/=nsubs ) THEN
                  nbasa = nbasa + nloada + 1
                  nbasp = nbasp + nloadp + 1
                  nloada = iz(nbasa)
                  nloadp = iz(nbasp)
                  nloadn = nloada + nloadp
               ENDIF
            ENDDO
!
!     END OF GENERATION OF NEW LODS ITEM AND CREATION OF MERGE TABLE
!
!     CALCULATE BEGINNING LOCATION OF MERGE VECTOR IN OPEN CORE
!
            nmrvcs = nnewf + 1
            nmergn = nmergs
            nmrvcn = nmrvcs - 2
            lvect = iz(nnews+2)
            nmrvcf = nnewf + lvect
            nchave = nmrvcf
            IF ( nchave<=niz ) THEN
!
!     FILL THE MERGE VECTOR WITH  1(S)  ACCORDING TO THE MERGE TABLE
!
!     1) ZERO FIRST
!
               DO i = 1 , lvect
                  rz(nmrvcs-1+i) = 0.
               ENDDO
!
!     2) NOW FILL
!
               DO iloop = 1 , nsubs
                  idrc1 = iz(nmergn)
                  idrc2 = iz(nmergn+1)
                  IF ( idrc2/=0 ) THEN
                     DO n = 1 , idrc2
                        rz(nmrvcn+idrc1+n) = 1.0
                     ENDDO
                  ENDIF
                  nmergn = nmergn + 2
               ENDDO
!
!     WRITE THE MERGE VECTOR ON SCRATCH  5  USING  PACK-SEE COMMON PACKX
!     THIS IS A COLUMN PARTITIONING VECTOR (REFERRED TO AS A ROW VECTOR
!     BY MERGE)
!
               itypin = 1
               itypot = 1
               ifirst = 1
               ilast = lvect
               incr = 1
               CALL gopen(iscr5,iz(ibuf1),iwrtrw)
!
!     ZERO THE TRAILER INFO. LOCATIONS
!
               DO i = 1 , 7
                  mcbloc(i) = 0
               ENDDO
               mcbloc(1) = iscr5
               mcbloc(3) = lvect
               mcbloc(4) = irect
               mcbloc(5) = irsp
               CALL pack(rz(nmrvcs),iscr5,mcbloc(1))
               CALL close(iscr5,irew)
               CALL wrttrl(mcbloc(1))
               idump = -iscr5
               CALL dmpfil(idump,adump,4000)
!
!     READ IN THE  PVEC  AND  POVE(IF EXISTS)  USING MTRXI
!
!     1) PVEC
!
               iuvec = iscr3
               CALL mtrxi(iuvec,name,npvec,0,ichk)
!
!     2) POVE
!
               iuove = iscr4
               IF ( lpove ) CALL mtrxi(iuove,namell,npove,0,ichk)
!
!     SET UP TO CALL MERGE FOR  PAPP  AND  PVEC
!
               idump = -iuvec
               CALL dmpfil(idump,adump,4000)
               idump = -iuapp
               CALL dmpfil(idump,adump,4000)
               icore = nmrvcf + 1
               iz(icore) = iscr5
               CALL rdtrl(iz(icore))
!
!     SETUP NULL ROW PARTITIONING VECTOR USING  ISCR8
!     THIS IS A ROW PARTITIONING VECTOR REFERRED TO AS A COLUMN VECTOR
!     BY MERGE)
!
               mcbk11(1) = iuvec
               CALL rdtrl(mcbk11(1))
               mcbk12(1) = iuapp
               CALL rdtrl(mcbk12(1))
               DO k = 1 , 7
                  mcbk21(k) = 0
                  mcbk22(k) = 0
               ENDDO
               iz(icore+7) = iscr8
               iz(icore+8) = 0
               iz(icore+9) = mcbk11(3)
               iz(icore+10) = irect
               iz(icore+11) = irsp
               iz(icore+12) = 0
               iz(icore+13) = 0
               ncnt = icore + 13
!
               CALL gopen(iscr8,iz(ibuf1),iwrtrw)
               itypin = 1
               itypot = 1
               ifirst = 1
               ilast = 1
               incr = 1
               CALL pack(0,iscr8,iz(icore+7))
               CALL close(iscr8,irew)
               CALL wrttrl(iz(icore+7))
               lcore = ib3 - icore - 15
               i = icore + 15
               irule = 0
               mcbk(1) = iscr6
               mcbk(2) = mcbk11(2) + mcbk12(2)
               mcbk(3) = mcbk11(3)
               mcbk(4) = irect
               mcbk(5) = mcbk11(5)
               mcbk(6) = 0
               mcbk(7) = 0
               CALL merge(iz(icore),iz(icore+7),iz(i))
               CALL wrttrl(mcbk(1))
!
!     SETUP TO MERGE  POVE  AND  POAP(IF THEY EXIST)
!
               idump = -iuove
               CALL dmpfil(idump,adump,4000)
               idump = -iuoap
               CALL dmpfil(idump,adump,4000)
               IF ( lpove ) THEN
                  mcbk11(1) = iuove
                  CALL rdtrl(mcbk11(1))
                  mcbk12(1) = iuoap
                  CALL rdtrl(mcbk12(1))
                  DO k = 1 , 7
                     mcbk21(k) = 0
                     mcbk22(k) = 0
                  ENDDO
                  irule = 0
                  mcbk(1) = iscr7
                  mcbk(2) = mcbk11(2) + mcbk12(2)
                  mcbk(3) = mcbk11(3)
                  mcbk(4) = irect
                  mcbk(5) = mcbk11(5)
                  mcbk(6) = 0
                  mcbk(7) = 0
                  CALL merge(iz(icore),iz(icore+7),iz(i))
                  CALL wrttrl(mcbk(1))
               ENDIF
!
!     CHECK TIME REMAINING AND RETURN WITH USER FATAL MESSAGE IF NOT
!     ENOUGH REMAINING
!
               CALL tmtogo(itime2)
               IF ( itime2<=0 ) GOTO 400
!
!     CALCULATE TIME USED IN REACHING THIS LOCATION
!
               itused = itime1 - itime2
!
!     CONTINUE IF ITIME2 IS GREATER THAN ITUSED
!
               IF ( itime2<itused ) GOTO 400
!
!     WRITE NEW LODS ITEM TO SOF
!
!     1) DELETE OLD LODS ITEM
!
               CALL delete(name,nlods,ichk)
!
!     DELETE LODS ITEMS ON ANY SUBSTRUCTURE SECONDARY TO NAME - THIS
!     WILL ALLOW THE NEW LODS ITEM TO BE COPIED DURING FUTURE EQUIV
!     OPERATIONS
!
               ii = itcode(nlods)
               CALL fdsub(name,ind)
               CALL fmdi(ind,imdi)
               ips = andf(icorx(imdi+1),1023)
               IF ( ips==0 ) THEN
                  DO
                     iss = andf(rshift(icorx(imdi+1),10),1023)
                     IF ( iss==0 ) EXIT
                     CALL fmdi(iss,imdi)
                     iblk = andf(icorx(imdi+ii),65535)
                     IF ( iblk/=0 .AND. iblk/=65535 ) CALL retblk(iblk)
                     icorx(imdi+ii) = 0
                     mdiup = .TRUE.
                  ENDDO
               ENDIF
!
!     2) BEGIN WRITING
!
               ichk = 3
               irw = 2
               CALL sfetch(name,nlods,irw,ichk)
               nwords = 4 + 2*iz(nnews+3)
               CALL suwrt(iz(nnews),nwords,2)
               nbasn = nnews + nwords
               DO n = 1 , nsubs
                  nwords = iz(nbasn) + 1
                  CALL suwrt(iz(nbasn),nwords,2)
                  nbasn = nbasn + nwords
               ENDDO
               CALL suwrt(0,0,3)
!
!     WRITE NEW  PVEC  AND  POVE(IF IT EXISTS)  TO SOF
!
               CALL delete(name,npvec,ichk)
               CALL mtrxo(iscr6,name,npvec,0,ichk)
               IF ( lpove ) CALL delete(namell,npove,ichk)
               IF ( lpove ) CALL mtrxo(iscr7,namell,npove,0,ichk)
!
               WRITE (lp,99004) uim , name
99004          FORMAT (A29,' 6900, LOADS HAVE BEEN SUCCESSFULLY APPENDED FOR ','SUBSTRUCTURE ',2A4)
               GOTO 500
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   WRITE (lp,99005) ufm , nchave
99005 FORMAT (A23,' 6951, INSUFFICIENT CORE TO LOAD TABLES',/5X,'IN MODULE LODAPP, CORE =',I8)
   CALL mesage(-8,nprog,0)
!
 100  WRITE (lp,99006) sfm , name
99006 FORMAT (A25,' 6952, REQUESTED SUBSTRUCTURE ',2A4,' DOES NOT EXIST')
   idry = -2
   GOTO 500
 200  WRITE (lp,99007) sfm , nitem , name
99007 FORMAT (A25,' 6101, REQUESTED SOF ITEM DOES NOT EXIST.  ITEM ',A4,' SUBSTRUCTURE ',2A4)
   idry = -2
   GOTO 500
 300  WRITE (lp,99008) sfm , name
99008 FORMAT (A25,' 6953, A WRONG COMBINATION OF LOAD VECTORS EXISTS ','FOR SUBSTRUCTURE ',2A4)
   idry = -2
   GOTO 500
 400  WRITE (lp,99009) ufm , itime2
99009 FORMAT (A23,' 6956, INSUFFICIENT TIME REMAINING FOR MODULE ','LODAPP, TIME LEFT =',I8)
   idry = -2
 500  CALL sofcls
!
!     RETURN VALUE OF DRY PARAMETER
!
   buf(3) = idry
END SUBROUTINE lodapp
