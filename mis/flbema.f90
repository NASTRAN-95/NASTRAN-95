
SUBROUTINE flbema(Type)
   IMPLICIT NONE
   INTEGER Af , Afdict , Afmat , Bgpdt , Conect , Cstm , Dkgg , Ect , Eqexin , Fbelm , Frelm , Geom2 , Geom3 , Ibgpdt , Ibuf1 ,     &
         & Ibuf2 , Ibuf3 , Ibuf4 , Ibuf5 , Icore , Igrav , Igrid , Isil , Kgdict , Kgmat , Lcore , Mpt , Nbgpdt , Ngrav , Ngrid ,   &
         & Norew , Nout , Nsil , Rd , Rdrew , Rew , Sil , Uset , Usetf , Usets , Wrt , Wrtrew , Z(1)
   LOGICAL Error
   CHARACTER*25 Sfm , Uwm
   REAL Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /flbfil/ Geom2 , Ect , Bgpdt , Sil , Mpt , Geom3 , Cstm , Uset , Eqexin , Usetf , Usets , Af , Dkgg , Fbelm , Frelm ,     &
                 & Conect , Afmat , Afdict , Kgmat , Kgdict
   COMMON /flbptr/ Error , Icore , Lcore , Ibgpdt , Nbgpdt , Isil , Nsil , Igrav , Ngrav , Igrid , Ngrid , Ibuf1 , Ibuf2 , Ibuf3 ,  &
                 & Ibuf4 , Ibuf5
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Z
   INTEGER Type
   INTEGER alloc(3) , colsil(12) , dict(2) , file , i , icode , icol , icpos , ifcol , ilcol , iloc , j , jcore , kcol , kcore ,    &
         & luset , mcb(7) , n , name(2) , ncol , ncore , nloc , nrow , ntpers , nwds , optc , optw , outmat , rowsil(4) , terms(288)&
         & , typin , typout , xdict , xmat
   LOGICAL skip
!
!     ASSEMBLES THE AF OR DKGG MATRIX UTITLIZING THE ELEMENT
!     MATRICES GENERATED IN FLBEMG
!
!     TYPE = 1  AFF MATRIX
!     TYPE = 2  DKGG MATRIX
!
   DATA name/4HFLBE , 4HMA  /
!
!
!     ASSIGN FILES DEPENDING ON TYPE
!
   IF ( Type==2 ) THEN
!
!     DKGG MATRIX
!
      outmat = Dkgg
      xmat = Kgmat
      xdict = Kgdict
   ELSE
!
!     AF MATRIX
!
      outmat = Af
      xmat = Afmat
      xdict = Afdict
   ENDIF
!
!     ALLOCATE COLUMN POINTER VECTOR IN TOP OF CORE
!
   mcb(1) = Uset
   CALL rdtrl(mcb)
   luset = mcb(3)
   icol = 1
   ncol = luset
   DO i = 1 , ncol
      Z(i) = 0
   ENDDO
!
!     INITILIZE OPEN AND CLOSE OPTIONS
!
   optw = Wrtrew
   optc = Norew
!
!     POSITION CONNECT FILE TO PROPER RECORD
!
   file = Conect
   CALL open(*900,Conect,Z(Ibuf1),Rdrew)
   IF ( Type==2 ) CALL skpfil(Conect,1)
   CALL fwdrec(*1000,Conect)
   CALL close(Conect,Norew)
!
!     INITIALIZE PACK - UNPACK DATA
!
   typin = 2
   typout = 2
   mcb(1) = outmat
   mcb(2) = 0
   mcb(3) = luset
   mcb(4) = 3 - Type
   mcb(5) = typout
   mcb(6) = 0
   mcb(7) = 0
!
!     SET UP CORE POINTERS
!
   Icore = ncol + 1
   Lcore = Ibuf2 - 1
   ncore = Lcore - Icore
   IF ( ncore<200 ) GOTO 1200
!
   skip = .FALSE.
   ilcol = 0
!
!
!     ALLOCATE ALL AVALABLE CORE FOR THIS PASS BY USE OF CONECT FILE
!
 100  ifcol = ilcol + 1
   jcore = Icore
   file = Conect
!
   CALL gopen(Conect,Z(Ibuf1),Rd)
!
   IF ( .NOT.(skip) ) CALL read(*200,*1200,Conect,alloc,3,1,n)
   DO
!
      Isil = alloc(1)
      Z(Isil) = jcore
      Z(jcore) = jcore + 1
      jcore = jcore + 1 + alloc(2) + 2*alloc(3)
      IF ( jcore>Lcore ) THEN
!
!     INSUFFICIENT CORE FOR NEXT COLUMN - SET FLAG TO SAVE CURRENT
!     CONECT ALLOCATION RECORD
!
         skip = .TRUE.
         GOTO 300
      ELSE
         ilcol = Isil
         CALL read(*200,*1200,Conect,alloc,3,1,n)
      ENDIF
   ENDDO
!
!     END OF RECORD ON CONECT - ALL COLUMNS ALLOCATED
!
 200  ilcol = luset
   optc = Rew
!
 300  CALL close(Conect,optc)
!
!     OPEN DICTIONARY AND MATRIX FILES AND PREPARE TO MAKE PASS
!
   CALL gopen(xdict,Z(Ibuf1),Rdrew)
   CALL gopen(xmat,Z(Ibuf2),Rdrew)
   icpos = 0
 400  DO
!
!     READ XDICT ENTRY AND DETERMINE IF COLUMN IS IN CORE FOR THIS
!     PASS
!
      file = xdict
      CALL read(*1000,*800,xdict,dict,2,0,n)
      Isil = dict(1)
      IF ( Isil>=ifcol .AND. Isil<=ilcol ) THEN
!
!     THE COLUMN IS IN CORE - OBTAIN MATRIX DATA FROM XMAT FILE IF
!     WE DO NOT ALREADY HAVE IT
!
         IF ( dict(2)==icpos ) GOTO 600
         icpos = dict(2)
         file = xmat
         CALL filpos(xmat,icpos)
         CALL read(*1000,*1100,xmat,rowsil,4,0,n)
         CALL read(*1000,*1100,xmat,colsil,4,0,n)
         nrow = 4
         IF ( rowsil(4)<0 ) nrow = 3
         ncol = 4
         IF ( colsil(4)<0 ) ncol = 3
         CALL read(*1000,*500,xmat,terms,289,0,nwds)
         icode = 1
         GOTO 1300
      ENDIF
   ENDDO
!
!     EXPAND COLSIL TO INCLUDE ALL SILS
!
 500  IF ( nwds>=162 ) THEN
      DO i = 1 , 4
         j = 4 - i
         colsil(3*j+1) = colsil(j+1)
         colsil(3*j+2) = colsil(j+1) + 1
         colsil(3*j+3) = colsil(j+1) + 2
      ENDDO
      ncol = ncol*3
   ENDIF
   ntpers = 2
   IF ( nwds>=54 ) ntpers = 6
!
!     LOCATE POSITION OF MATRIX TERMS FOR DESIRED SIL
!
 600  DO kcol = 1 , ncol
      IF ( colsil(kcol)==Isil ) GOTO 700
   ENDDO
   icode = 2
   GOTO 1300
!
 700  iloc = (kcol-1)*nrow*ntpers + 1
!
!     EXTRACT MATRIX TERMS AND STORE THEM IN CORE
!
   icode = 3
   jcore = Z(Isil)
   IF ( jcore==0 ) GOTO 1300
   kcore = Z(jcore)
   DO i = 1 , nrow
      Z(kcore) = rowsil(i)
      IF ( ntpers==2 ) Z(kcore) = -rowsil(i)
      kcore = kcore + 1
      DO j = 1 , ntpers
         Z(kcore) = terms(iloc)
         iloc = iloc + 1
         kcore = kcore + 1
      ENDDO
   ENDDO
   Z(jcore) = kcore
!
   GOTO 400
!
!     END OF FILE ON XDICT - PREPARE TO PACK OUT COLUMNS IN CORE
!
 800  CALL close(xdict,optc)
   CALL close(xmat,optc)
   CALL gopen(outmat,Z(Ibuf1),optw)
!
!     PACK OUT COLUMNS
!
   DO i = ifcol , ilcol
      CALL bldpk(typin,typout,outmat,0,0)
      IF ( Z(i)/=0 ) THEN
!
         iloc = Z(i) + 1
         nloc = Z(iloc-1) - iloc
         CALL pakcol(Z(iloc),nloc)
      ENDIF
!
      CALL bldpkn(outmat,0,mcb)
   ENDDO
!
   CALL close(outmat,optc)
!
!     RETURN FOR ADDITIONAL PASS IF MORE NONZERO COLUMNS REMAIN
!
   optw = Wrt
   IF ( ilcol<luset ) GOTO 100
!
!     ALL COLUMNS PROCESSED - WRITE TRAILER AND RETURN
!
   CALL wrttrl(mcb)
   RETURN
!
!     ERROR CONDITIONS
!
 900  n = -1
!
   CALL mesage(n,file,name)
   GOTO 1300
 1000 n = -2
   CALL mesage(n,file,name)
   GOTO 1300
 1100 n = -3
   CALL mesage(n,file,name)
   GOTO 1300
 1200 n = -8
   CALL mesage(n,file,name)
 1300 DO
!
      WRITE (Nout,99001) Sfm , icode
99001 FORMAT (A25,' 8010, LOGIC ERROR IN SUBROUTINE FLBEMA - CODE',I3/)
      n = -61
      CALL mesage(n,file,name)
   ENDDO
END SUBROUTINE flbema