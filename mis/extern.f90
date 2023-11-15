
SUBROUTINE extern(Nex,Ngrav,Gvect,Ilist,Pg,N1,Iharm)
!
!     GENERATES EXTERNAL LOADS
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bgpdt , Cstm , Est , Icm , Idit , Iditfl , Idum(14) , Iest(45) , Ii , Iihmat , Ilid , Incur , Isil , Itya , Ityb ,       &
         & Iz(1) , Jdum , Jj , Lcare , Mpt , Mptfil , Nn(7) , Nnhmat , Nobld , Nrowsp , Old , Sil , Slt , Sysbuf
   REAL Core(1)
   COMMON /blank / Nrowsp
   COMMON /gpta1 / Jdum
   COMMON /hmatdd/ Iihmat , Nnhmat , Mptfil , Iditfl
   COMMON /loadx / Lcare , Slt , Bgpdt , Old , Cstm , Sil , Isil , Est , Mpt , Nn , Nobld , Idit , Icm , Ilid
   COMMON /packx / Itya , Ityb , Ii , Jj , Incur
   COMMON /pindex/ Iest
   COMMON /system/ Sysbuf
   COMMON /tranx / Idum
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   INTEGER Iharm , N1 , Nex , Ngrav
   REAL Gvect(1)
   INTEGER Ilist(1) , Pg(1)
!
! Local variable declarations
!
   INTEGER all , casecc , file , flag , hccens , hcflds , i , ib , ibuf1 , ibuf2 , ibuf3 , ibuf4 , ibuf5 , ido , iii , ij , ilsym , &
         & ip1 , ipre , iset , isetno , j , jopen , kcc , kkkk , kset , lcore , mcore , mset , n , name(2) , nbdys , ncc , ncore ,  &
         & nelout , nextz , ngrold , nloop , nograv , nset , permbd , remfls , ret , scr6 , setno
!
! End of declarations
!
   EQUIVALENCE (Core(1),Iz(1))
   DATA casecc , permbd , hcflds , remfls , scr6 , hccens , name/110 , 112 , 304 , 305 , 306 , 307 , 4HEXTE , 4HRN  /
!
   Iest(1) = -1
   Idum(1) = 0
   jopen = 0
   ipre = 0
   Incur = 1
   Ii = 1
   Jj = Nrowsp
   Ngrav = 0
   Old = 0
   Icm = 1
   Itya = 1
   Ityb = 1
   ibuf1 = Lcare - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   ibuf4 = ibuf3 - Sysbuf
   ibuf5 = ibuf4 - Sysbuf
   lcore = ibuf5 - Sysbuf
   CALL gopen(Slt,Core(ibuf1),0)
   CALL gopen(Bgpdt,Core(ibuf2),0)
   file = Cstm
   CALL open(*100,Cstm,Core(ibuf3),0)
   Icm = 0
   CALL skprec(Cstm,1)
 100  CALL gopen(Sil,Core(ibuf4),0)
   file = Slt
   Isil = 0
   IF ( lcore<Nrowsp ) GOTO 1200
!
   iii = 1
   DO nloop = 1 , N1
!
      Ilid = Ilist(iii)
      IF ( Ilid/=0 ) THEN
         DO i = 1 , Nrowsp
            Core(i) = 0.0
         ENDDO
         nograv = 0
         ngrold = Ngrav
      ELSE
         CALL skprec(Slt,1)
         GOTO 950
      ENDIF
 150  DO
         CALL read(*1000,*900,Slt,Nobld,1,0,flag)
         CALL fread(Slt,ido,1,0)
         IF ( nograv==1 ) GOTO 1100
         IF ( Nobld==-20 ) EXIT
         IF ( Nobld==3 .OR. Nobld==4 ) THEN
            DO j = 1 , ido
               CALL tpont
            ENDDO
            CYCLE
         ELSEIF ( Nobld==5 .OR. Nobld==6 ) THEN
            DO j = 1 , ido
               CALL fpont
            ENDDO
            CYCLE
         ELSEIF ( Nobld==7 ) THEN
            DO j = 1 , ido
               CALL sload
            ENDDO
            CYCLE
         ELSEIF ( Nobld==8 ) THEN
            IF ( nograv==2 ) GOTO 1100
            DO j = 1 , ido
               CALL grav(Ngrav,Gvect(1),Nex,Ilist(1),nloop)
            ENDDO
            nograv = 1
            CYCLE
         ELSEIF ( Nobld==9 ) THEN
            DO j = 1 , ido
               CALL pload
            ENDDO
            CYCLE
         ELSEIF ( Nobld==10 ) THEN
!
!     RFORCE CARDS
!
            DO j = 1 , ido
               CALL rforce(lcore)
            ENDDO
            CYCLE
         ELSEIF ( Nobld==11 ) THEN
!
!     PRESAX CARDS
!
            DO j = 1 , ido
               CALL presax(Iharm)
            ENDDO
            CYCLE
         ELSEIF ( Nobld==12 ) THEN
!
!     QHBDY CARDS
!
            DO j = 1 , ido
               CALL qhbdy
            ENDDO
            CYCLE
         ELSEIF ( Nobld==13 ) THEN
!
!     QVOL CARDS (MODIFIED USER ENTRYS)
!
            DO j = 1 , ido
               CALL qvol
            ENDDO
            CYCLE
         ELSEIF ( Nobld==14 ) THEN
!
!     QBDY1 CARDS (MODIFIED USER ENTRYS)
!
            kkkk = 1
         ELSEIF ( Nobld==15 ) THEN
!
!     QBDY2 CARDS (MODIFIED USER ENTRYS)
!
            kkkk = 2
         ELSEIF ( Nobld==16 ) THEN
!
!     QVECT CARDS (MODIFIED USER ENTRYS)
!
            kkkk = 3
         ELSEIF ( Nobld==17 ) THEN
!
!     PLOAD3 CARDS
!
            DO j = 1 , ido
               CALL pload3
            ENDDO
            CYCLE
         ELSEIF ( Nobld==18 ) THEN
!
!     PLOAD1 CARDS
!
            IF ( ipre==1 ) GOTO 160
            ipre = 1
            lcore = lcore - Sysbuf - 1
            mcore = lcore - Nrowsp - 1
            IF ( lcore<Nrowsp ) GOTO 1200
            CALL premat(Core(Nrowsp+1),Core(Nrowsp+1),Core(lcore),mcore,ncore,Mpt,Idit)
            GOTO 160
         ELSEIF ( Nobld==19 ) THEN
!
!     PLOADX CARDS
!
            DO j = 1 , ido
               CALL ploadx
            ENDDO
            CYCLE
         ELSEIF ( Nobld==20 .OR. Nobld==21 .OR. Nobld==22 .OR. Nobld==23 .OR. Nobld==24 ) THEN
            EXIT
         ELSEIF ( Nobld==25 ) THEN
!
!     PLOAD4 CARDS
!
            CALL pload4(ibuf5,ido,jopen)
            CYCLE
         ELSE
            DO j = 1 , ido
               CALL direct
            ENDDO
            CYCLE
         ENDIF
         DO j = 1 , ido
            CALL qloadl(kkkk)
         ENDDO
         CYCLE
 160     DO j = 1 , ido
            CALL plbar1(ido,lcore)
         ENDDO
      ENDDO
!
!     CEMLOOP, SPCFLD, GEMLOOP, MDIPOLE, AND REMFLUX CARDS
!
!     BRING HEAT MATERIALS INTO CORE
!
      IF ( ipre==1 ) THEN
!
!     NO DO LOOP ON IDO. IN EANDM WE WILL READ ALL CARDS
!
         CALL eandm(Nobld,ido,nextz,lcore,nbdys,all,nelout)
         GOTO 150
      ELSE
         ipre = 1
!
!     1ST AND LAST AVAILABLE LOCATIONS IN OPEN CORE
!
         Iihmat = Nrowsp
         Nnhmat = lcore
         Mptfil = Mpt
         Iditfl = Idit
         CALL prehma(Core)
!
!     NOW NNHMAT CONTAINS LAST LOCATION OF MATERIAL INFO
!
         nextz = Nnhmat + 1
!
!     OPEN HCFLDS TO CONTAIN APPLIED MAGNETIC FIELD LOAD
!
         lcore = lcore - Sysbuf
         IF ( lcore>nextz ) THEN
!
!     STORE SILS  ON PERMBDY, IF ANY, INTO OPEN CORE
!
            nbdys = 0
            file = permbd
            CALL open(*250,permbd,Core(lcore+1),0)
            CALL fwdrec(*1000,permbd)
            CALL read(*1000,*200,permbd,Core(nextz),lcore-nextz+1,0,nbdys)
         ENDIF
         GOTO 1200
      ENDIF
 200  CALL close(permbd,1)
 250  nextz = nextz + nbdys
!
!     NOW CHECK FOR FORCE REQUESTS ON CASECC(MAGNETIC FIELD REQUESTS)
!     MAKE A UNIQUE LIST OF ELEMENT ID-S CORRESPONDING TO ALL SUBCASES.
!     IF A SUBCASE REQUESTS ALL, NO LIST IS NECESSARY.
!
      all = 0
      nelout = 0
      ij = 0
!
!     1ST GET MAXIMUM LENGTH OF CASE CONTROL IN ORDER TO STORE ELEMENT
!     ID-S
!
      ncc = 0
      CALL gopen(casecc,Core(lcore+1),0)
 300  CALL read(*400,*350,casecc,Core(nextz),lcore-nextz+1,0,kcc)
      GOTO 1200
 350  ncc = max0(ncc,kcc)
      GOTO 300
 400  CALL rewind(casecc)
      CALL fwdrec(*1000,casecc)
      kset = nextz + ncc
!
 450  CALL read(*850,*500,casecc,Core(nextz),lcore-nextz+1,0,ncc)
      GOTO 1200
 500  setno = Iz(nextz+25)
      IF ( setno==0 ) GOTO 450
      IF ( setno>0 ) THEN
!
!     CREATE UNIQUE LIST OF ELEMENT ID-S
!
         ilsym = Iz(nextz+165)
         isetno = ilsym + Iz(ilsym+nextz-1) + nextz
         DO
            iset = isetno + 2
            nset = Iz(isetno+1) + iset - 1
            IF ( Iz(isetno)==setno ) THEN
!
!     PICK UP ELEMENT ID-S. STORE IN UNIQUE LIST
!
               i = iset
               GOTO 600
            ELSE
               isetno = nset + 1
!
!     IF SET CANNOT BE FOUND, SET TO ALL. BUT SHOULD NOT HAPPEN
!
               IF ( isetno>=ncc+nextz-1 ) EXIT
            ENDIF
         ENDDO
      ENDIF
!
!     ALL
!
 550  all = 1
      nelout = 0
      GOTO 850
 600  IF ( i==nset ) GOTO 700
      IF ( Iz(i+1)>0 ) GOTO 700
      ib = Iz(i)
      n = -Iz(i+1)
      i = i + 1
      ASSIGN 650 TO ret
      GOTO 800
 650  ib = ib + 1
      IF ( ib>n ) GOTO 750
      GOTO 800
 700  ib = Iz(i)
      ASSIGN 750 TO ret
      GOTO 800
 750  i = i + 1
!
!     DONE WITH THIS SET. GO BACK FOR ANOTHER
!
      IF ( i>nset ) GOTO 450
      GOTO 600
!
!     SEARCH LIST OF ELEMENT ID-S. ADD ID TO LIST IF NOT A DUPLICATE
!
 800  IF ( ij/=0 ) THEN
         DO j = mset , ij
            IF ( Iz(j)==ib ) GOTO ret
         ENDDO
         ij = ij + 1
         IF ( ij>=lcore ) GOTO 550
         Iz(ij) = ib
         nelout = nelout + 1
         GOTO ret
      ELSE
         mset = kset
         Iz(mset) = ib
         nelout = 1
         ij = mset
         GOTO ret
      ENDIF
!
!     DONE WITH ALL CASES. IF ALL.NE.1, MOVE THE ID-S UP IN CORE
!
 850  CALL close(casecc,1)
      IF ( all/=1 ) THEN
!
         DO j = 1 , nelout
            Iz(nextz+j-1) = Iz(mset+j-1)
         ENDDO
         nextz = nextz + nelout
      ENDIF
!
      CALL gopen(hcflds,Core(lcore+1),1)
      i = lcore - Sysbuf
      j = i - Sysbuf
      lcore = j - Sysbuf
      IF ( lcore<=nextz ) GOTO 1200
      CALL gopen(remfls,Core(i+1),1)
      CALL gopen(hccens,Core(j+1),1)
      CALL gopen(scr6,Core(lcore+1),1)
      CALL eandm(Nobld,ido,nextz,lcore,nbdys,all,nelout)
      GOTO 150
!
!
 900  IF ( ngrold/=Ngrav ) CYCLE
      CALL pack(Core,Pg,Pg(1))
 950  iii = iii + 1
!
   ENDDO
!
   CALL close(Bgpdt,1)
   IF ( Icm==0 ) CALL close(Cstm,1)
   CALL close(Slt,1)
   CALL close(Sil,1)
   IF ( ipre==1 ) THEN
      CALL close(hcflds,1)
      CALL close(remfls,1)
      CALL close(hccens,1)
      CALL close(scr6,1)
   ENDIF
   RETURN
!
!     FILE ERRORS
!
 1000 ip1 = -2
   GOTO 1300
 1100 ip1 = -7
   GOTO 1300
 1200 ip1 = -8
 1300 CALL mesage(ip1,file,name(1))
END SUBROUTINE extern
