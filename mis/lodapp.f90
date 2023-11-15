
SUBROUTINE lodapp
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf(3) , Icdp , Icorx(1) , Icsp , Idiag , Iefnrw , Ifirst , Ilast , Ilower , Incr , Inorew , Iodum(8) , Ird , Irdp ,     &
         & Irdrew , Irect , Irew , Irsp , Irule , Isbuff , Isqure , Isym , Itypin , Itypot , Iupper , Iwrt , Iwrtrw , Iz(1) ,       &
         & Lcore , Lp , Mcbk(7) , Mcbk11(7) , Mcbk12(7) , Mcbk21(7) , Mcbk22(7) , Mdidum(4) , Nxtdum(15)
   REAL Ditdum(6) , Rz(1)
   LOGICAL Ditup , Mdiup
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Buf
   COMMON /names / Ird , Irdrew , Iwrt , Iwrtrw , Irew , Inorew , Iefnrw , Irsp , Irdp , Icsp , Icdp , Isqure , Irect , Idiag ,     &
                 & Iupper , Ilower , Isym
   COMMON /packx / Itypin , Itypot , Ifirst , Ilast , Incr
   COMMON /parmeg/ Mcbk , Mcbk11 , Mcbk21 , Mcbk12 , Mcbk22 , Lcore , Irule
   COMMON /sof   / Ditdum , Iodum , Mdidum , Nxtdum , Ditup , Mdiup
   COMMON /system/ Isbuff , Lp
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Rz
!
! Local variable declarations
!
   REAL adump(4000) , blnk
   INTEGER andf , itcode , korsz , rshift
   INTEGER i , ib1 , ib2 , ib3 , iblk , ibuf1 , ichk , icore , idrc1 , idrc2 , idry , idump , igo , ii , iloop , imdi , imergn ,    &
         & ind , ipapp , ipoap , ips , irw , iscr1 , iscr2 , iscr3 , iscr4 , iscr5 , iscr6 , iscr7 , iscr8 , iss , itest , itime1 , &
         & itime2 , itloap , itlods , itused , iuapp , iuoap , iuove , iuvec , k , l , lmergt , lnewlt , lvect , m , mcbloc(7) , n ,&
         & naf , name(2) , namell(2) , nas , nbasa , nbasn , nbasp , nchave , ncnt , ncore , ndel1 , nfini , nitem , niz , nl ,     &
         & nlbasa , nlbasp , nldsa , nldsp , nloada , nloadn , nloadp , nloads , nloap , nlods , nloop , nmergf , nmergn , nmergs , &
         & nmrvcf , nmrvcn
   LOGICAL llsub , lmerg , lpapp , lpoap , lpove , lpvec
   INTEGER nmrvcs , nn(2) , nnew1 , nnewf , nnews , npapp , npf , npoap , npove , nprog(2) , nps , npvec , ns , ns1 , nstart ,      &
         & nsubs , nwds , nwords
   EXTERNAL andf , rshift
!
! End of declarations
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
   EQUIVALENCE (Icorx(1),Rz(1))
   EQUIVALENCE (Rz(1),Iz(1))
   DATA ipapp , ipoap/101 , 102/
   DATA iscr1 , iscr2 , iscr3 , iscr4 , iscr5 , iscr6 , iscr7 , iscr8/301 , 302 , 303 , 304 , 305 , 306 , 307 , 308/
   DATA npapp , npoap , npvec , npove , nloap , nlods/4HPAPP , 4HPOAP , 4HPVEC , 4HPOVE , 4HLOAP , 4HLODS/
   DATA nprog/4HLODA , 4HPP  /
   DATA blnk/4H    /
!
!     INITIALIZE PARAMETERS
!
   CALL tmtogo(itime1)
   name(1) = Buf(1)
   name(2) = Buf(2)
   idry = Buf(3)
   ncore = korsz(Iz(1))
!
!     INITIALIZE OPEN CORE - THERE ARE NIZ WORDS AVAILABLE
!
   ib1 = ncore - (Isbuff+1)
   ib2 = ib1 - Isbuff - 1
   ib3 = ib2 - Isbuff
   ibuf1 = ib3 - Isbuff
   niz = ibuf1 - 1
   nstart = 1
!
!     TEST CORE
!
   nchave = niz
   IF ( nchave>0 ) THEN
      CALL sofopn(Iz(ib1),Iz(ib2),Iz(ib3))
!
!     CHECK STATUS OF SUBSTRUCTURE BEING REFERENCED - NAME
!
      CALL sfetch(name,nlods,3,igo)
      IF ( igo==4 ) GOTO 100
      IF ( idry>=0 ) THEN
!
!     CHECK LOCATION OF THE PAPP VECTOR - EITHER ON FILE IPAPP OR SOF
!
         Iz(1) = ipapp
         CALL rdtrl(Iz(1))
         IF ( Iz(1)>0 ) THEN
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
            Iz(1) = ipoap
            CALL rdtrl(Iz(1))
            IF ( Iz(1)>0 ) THEN
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
         Iz(1) = 0
         CALL softrl(name,npvec,Iz(1))
         IF ( Iz(1)/=1 ) lpvec = .FALSE.
!
!     2) POVE
!
         lpove = .TRUE.
         Iz(1) = 0
         IF ( llsub ) CALL softrl(namell,npove,Iz(1))
         IF ( Iz(1)/=1 ) lpove = .FALSE.
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
      CALL suread(Iz(nstart),-1,nwds,ichk)
      nl = Iz(nstart+2)
      ns = Iz(nstart+3)
      nfini = 4 + ns*3 + nl
      nstart = 5 + ns*2
      nas = 1
      naf = nfini
      nchave = nfini
      nsubs = ns
      IF ( nchave<=niz ) THEN
         nbasn = 4 + nsubs*2 + 1
         DO iloop = 1 , nsubs
            CALL suread(Iz(nbasn),-1,nwds,ichk)
            nbasn = Iz(nbasn) + 1 + nbasn
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
                  WRITE (Lp,99001) Sfm , nitem , name
99001             FORMAT (A25,' 6954, THE ,A4,62H ITEM EXISTS BUT HAS NO ','ASSOCIATED PVEC ITEM FOR SUBSTRUCTURE ',2A4)
                  idry = -2
               ELSE
                  nwds = 4 + 2*Iz(nas+3)
                  CALL suwrt(Iz(nas),nwds,2)
                  nbasn = nas + nwds
                  DO n = 1 , nsubs
                     nwds = Iz(nbasn) + 1
                     CALL suwrt(Iz(nbasn),nwds,2)
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
                  WRITE (Lp,99002) Uim , name
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
         CALL suread(Iz(nstart),-1,nwds,ichk)
         ncnt = nwds + naf
         nl = Iz(nps+2)
         ns = Iz(nps+3)
         nfini = nps + 3 + 3*ns + nl
         npf = nfini
         nstart = npf + 1
         nchave = nfini
         IF ( nchave<=niz ) THEN
            nbasn = nps + 3 + 2*nsubs + 1
            DO iloop = 1 , nsubs
               CALL suread(Iz(nbasn),-1,nwds,ichk)
               nbasn = nbasn + Iz(nbasn) + 1
            ENDDO
            nldsa = Iz(nas+2)
            nldsp = Iz(nps+2)
            nloads = nldsa + nldsp
            nbasa = nas + 3 + 2*nsubs + 1
            nbasp = nps + 3 + 2*nsubs + 1
            nlbasa = Iz(nbasa)
            nlbasp = Iz(nbasp)
!
!     CHECK FOR DUPLICATE LOAD IDS IN THE  LOAP  AND  LODS  ITEMS.
!
            DO l = 1 , nsubs
               IF ( nlbasp/=0 .AND. nlbasa/=0 ) THEN
                  DO m = 1 , nlbasa
                     DO n = 1 , nlbasp
                        IF ( Iz(nbasa+m)==Iz(nbasp+n) .AND. Iz(nbasa+m)/=0 ) THEN
                           WRITE (Lp,99003) Ufm , Iz(nbasa+m) , name
99003                      FORMAT (A23,' 6955, DUPLICATE LOAD IDS DURING APPEND OPERATION.','  LOAD ID NO.',I9,' SUBSTRUCTURE ',2A4)
                           idry = -2
                        ENDIF
                     ENDDO
                  ENDDO
               ENDIF
               nbasa = nbasa + nlbasa + 1
               nbasp = nbasp + nlbasp + 1
               nlbasa = Iz(nbasa)
               nlbasp = Iz(nbasp)
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
            Iz(nnews) = Iz(nas)
            Iz(nnews+1) = Iz(nas+1)
            Iz(nnews+2) = nloads
            Iz(nnews+3) = nsubs
            nloop = 2*nsubs
            nnew1 = nnews + 3
            ndel1 = nas + 3
            DO ns1 = 1 , nloop
               Iz(nnew1+ns1) = Iz(ndel1+ns1)
            ENDDO
!
!     COMPLETION OF THE NEW LODS TABLE - GROUPS  1  THRU  NSUBS  --  AND
!     CREATION OF THE MERGE TABLE
!
            nbasn = nnew1 + nloop + 1
            nbasa = nas + 3 + 2*nsubs + 1
            nbasp = nps + 3 + 2*nsubs + 1
            nloada = Iz(nbasa)
            nloadp = Iz(nbasp)
            nloadn = nloada + nloadp
            nmergn = nmergs
            imergn = 1
!
!     ZERO THE MERG TABLE LOCATION
!
            DO i = 1 , lmergt
               Iz(npf+i) = 0
            ENDDO
            DO iloop = 1 , nsubs
               Iz(nbasn) = nloadn
               IF ( nloadp/=0 ) THEN
                  DO n = 1 , nloadp
                     Iz(nbasn+n) = Iz(nbasp+n)
                  ENDDO
               ENDIF
               nbasn = nbasn + nloadp
               IF ( nloada/=0 ) THEN
                  DO n = 1 , nloada
                     Iz(nbasn+n) = Iz(nbasa+n)
                  ENDDO
               ENDIF
!
!     LOCATION IN THE MERGE TABLE OF THE  1(S)
!
               imergn = imergn + nloadp
               Iz(nmergn) = imergn
               imergn = imergn + nloada
               Iz(nmergn+1) = nloada
               nmergn = nmergn + 2
               nbasn = nbasn + nloada + 1
               IF ( iloop/=nsubs ) THEN
                  nbasa = nbasa + nloada + 1
                  nbasp = nbasp + nloadp + 1
                  nloada = Iz(nbasa)
                  nloadp = Iz(nbasp)
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
            lvect = Iz(nnews+2)
            nmrvcf = nnewf + lvect
            nchave = nmrvcf
            IF ( nchave<=niz ) THEN
!
!     FILL THE MERGE VECTOR WITH  1(S)  ACCORDING TO THE MERGE TABLE
!
!     1) ZERO FIRST
!
               DO i = 1 , lvect
                  Rz(nmrvcs-1+i) = 0.
               ENDDO
!
!     2) NOW FILL
!
               DO iloop = 1 , nsubs
                  idrc1 = Iz(nmergn)
                  idrc2 = Iz(nmergn+1)
                  IF ( idrc2/=0 ) THEN
                     DO n = 1 , idrc2
                        Rz(nmrvcn+idrc1+n) = 1.0
                     ENDDO
                  ENDIF
                  nmergn = nmergn + 2
               ENDDO
!
!     WRITE THE MERGE VECTOR ON SCRATCH  5  USING  PACK-SEE COMMON PACKX
!     THIS IS A COLUMN PARTITIONING VECTOR (REFERRED TO AS A ROW VECTOR
!     BY MERGE)
!
               Itypin = 1
               Itypot = 1
               Ifirst = 1
               Ilast = lvect
               Incr = 1
               CALL gopen(iscr5,Iz(ibuf1),Iwrtrw)
!
!     ZERO THE TRAILER INFO. LOCATIONS
!
               DO i = 1 , 7
                  mcbloc(i) = 0
               ENDDO
               mcbloc(1) = iscr5
               mcbloc(3) = lvect
               mcbloc(4) = Irect
               mcbloc(5) = Irsp
               CALL pack(Rz(nmrvcs),iscr5,mcbloc(1))
               CALL close(iscr5,Irew)
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
               Iz(icore) = iscr5
               CALL rdtrl(Iz(icore))
!
!     SETUP NULL ROW PARTITIONING VECTOR USING  ISCR8
!     THIS IS A ROW PARTITIONING VECTOR REFERRED TO AS A COLUMN VECTOR
!     BY MERGE)
!
               Mcbk11(1) = iuvec
               CALL rdtrl(Mcbk11(1))
               Mcbk12(1) = iuapp
               CALL rdtrl(Mcbk12(1))
               DO k = 1 , 7
                  Mcbk21(k) = 0
                  Mcbk22(k) = 0
               ENDDO
               Iz(icore+7) = iscr8
               Iz(icore+8) = 0
               Iz(icore+9) = Mcbk11(3)
               Iz(icore+10) = Irect
               Iz(icore+11) = Irsp
               Iz(icore+12) = 0
               Iz(icore+13) = 0
               ncnt = icore + 13
!
               CALL gopen(iscr8,Iz(ibuf1),Iwrtrw)
               Itypin = 1
               Itypot = 1
               Ifirst = 1
               Ilast = 1
               Incr = 1
               CALL pack(0,iscr8,Iz(icore+7))
               CALL close(iscr8,Irew)
               CALL wrttrl(Iz(icore+7))
               Lcore = ib3 - icore - 15
               i = icore + 15
               Irule = 0
               Mcbk(1) = iscr6
               Mcbk(2) = Mcbk11(2) + Mcbk12(2)
               Mcbk(3) = Mcbk11(3)
               Mcbk(4) = Irect
               Mcbk(5) = Mcbk11(5)
               Mcbk(6) = 0
               Mcbk(7) = 0
               CALL merge(Iz(icore),Iz(icore+7),Iz(i))
               CALL wrttrl(Mcbk(1))
!
!     SETUP TO MERGE  POVE  AND  POAP(IF THEY EXIST)
!
               idump = -iuove
               CALL dmpfil(idump,adump,4000)
               idump = -iuoap
               CALL dmpfil(idump,adump,4000)
               IF ( lpove ) THEN
                  Mcbk11(1) = iuove
                  CALL rdtrl(Mcbk11(1))
                  Mcbk12(1) = iuoap
                  CALL rdtrl(Mcbk12(1))
                  DO k = 1 , 7
                     Mcbk21(k) = 0
                     Mcbk22(k) = 0
                  ENDDO
                  Irule = 0
                  Mcbk(1) = iscr7
                  Mcbk(2) = Mcbk11(2) + Mcbk12(2)
                  Mcbk(3) = Mcbk11(3)
                  Mcbk(4) = Irect
                  Mcbk(5) = Mcbk11(5)
                  Mcbk(6) = 0
                  Mcbk(7) = 0
                  CALL merge(Iz(icore),Iz(icore+7),Iz(i))
                  CALL wrttrl(Mcbk(1))
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
               ips = andf(Icorx(imdi+1),1023)
               IF ( ips==0 ) THEN
                  DO
                     iss = andf(rshift(Icorx(imdi+1),10),1023)
                     IF ( iss==0 ) EXIT
                     CALL fmdi(iss,imdi)
                     iblk = andf(Icorx(imdi+ii),65535)
                     IF ( iblk/=0 .AND. iblk/=65535 ) CALL retblk(iblk)
                     Icorx(imdi+ii) = 0
                     Mdiup = .TRUE.
                  ENDDO
               ENDIF
!
!     2) BEGIN WRITING
!
               ichk = 3
               irw = 2
               CALL sfetch(name,nlods,irw,ichk)
               nwords = 4 + 2*Iz(nnews+3)
               CALL suwrt(Iz(nnews),nwords,2)
               nbasn = nnews + nwords
               DO n = 1 , nsubs
                  nwords = Iz(nbasn) + 1
                  CALL suwrt(Iz(nbasn),nwords,2)
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
               WRITE (Lp,99004) Uim , name
99004          FORMAT (A29,' 6900, LOADS HAVE BEEN SUCCESSFULLY APPENDED FOR ','SUBSTRUCTURE ',2A4)
               GOTO 500
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   WRITE (Lp,99005) Ufm , nchave
99005 FORMAT (A23,' 6951, INSUFFICIENT CORE TO LOAD TABLES',/5X,'IN MODULE LODAPP, CORE =',I8)
   CALL mesage(-8,nprog,0)
!
 100  WRITE (Lp,99006) Sfm , name
99006 FORMAT (A25,' 6952, REQUESTED SUBSTRUCTURE ',2A4,' DOES NOT EXIST')
   idry = -2
   GOTO 500
 200  WRITE (Lp,99007) Sfm , nitem , name
99007 FORMAT (A25,' 6101, REQUESTED SOF ITEM DOES NOT EXIST.  ITEM ',A4,' SUBSTRUCTURE ',2A4)
   idry = -2
   GOTO 500
 300  WRITE (Lp,99008) Sfm , name
99008 FORMAT (A25,' 6953, A WRONG COMBINATION OF LOAD VECTORS EXISTS ','FOR SUBSTRUCTURE ',2A4)
   idry = -2
   GOTO 500
 400  WRITE (Lp,99009) Ufm , itime2
99009 FORMAT (A23,' 6956, INSUFFICIENT TIME REMAINING FOR MODULE ','LODAPP, TIME LEFT =',I8)
   idry = -2
 500  CALL sofcls
!
!     RETURN VALUE OF DRY PARAMETER
!
   Buf(3) = idry
END SUBROUTINE lodapp
