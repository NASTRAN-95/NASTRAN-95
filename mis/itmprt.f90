
SUBROUTINE itmprt(Name,Item,Nz,Iopt)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   CHARACTER*1 Ccore(2000)
   REAL Core(1) , Head1(96) , Head2(96)
   INTEGER Icore(4) , Inx(6) , Inx1(2) , Inx2(26) , Line , Machx , Nlpp , Otpe , Sysbuf , Two1(32)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /machin/ Machx
   COMMON /output/ Head1 , Head2
   COMMON /system/ Sysbuf , Otpe , Inx , Nlpp , Inx1 , Line , Inx2
   COMMON /two   / Two1
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   INTEGER Iopt , Nz
   REAL Item
   REAL Name(2)
!
! Local variable declarations
!
   REAL alphc , alphc1 , bgss , blank , cont , cparen , cstm , d , ec , ec1 , ec2 , eqss , flag , itm , loap , lods , oparen ,      &
      & plts , subs(3) , uned
   INTEGER i , icode , icomp , intgc , inum , iout , irec , ist , iv , ix , left , llen , ngrd , nout , np , nred , ns , nsub , rc
   INTEGER numtyp
!
! End of declarations
!
!
!     WILL PRINT SOF ITEM - USING  E15.7,I10, OR ALPHA FORMAT
!
   EQUIVALENCE (Ccore,Core)
   EQUIVALENCE (Icore(1),Core(1))
   DATA oparen , cparen , ec , ec1 , ec2 , intgc , alphc , alphc1 , cont , uned , d/4H(1X  , 4H)    , 4H,1P, , 4HE13. , 4H6    ,    &
       &4H,I13 , 4H,9X, , 4HA4   , 4HCONT , 4HINUE , 4HD   /
   DATA blank , subs , itm/4H     , 4HSUBS , 4HTRUC , 4HTURE , 4HITEM/
   DATA eqss/4HEQSS/ , bgss/4HBGSS/ , cstm/4HCSTM/ , plts/4HPLTS/ , lods/4HLODS/ , loap/4HLOAP/
!
!
!     TEST FOR FORMATED TABLE PRINT
!
   IF ( Iopt==2 ) THEN
      IF ( Item==eqss ) THEN
!
!     PERFORM FORMATED LISTING OF TABLE
!
!     EQSS TABLE
!
         CALL sfetch(Name,Item,1,rc)
         IF ( rc/=1 ) RETURN
         CALL suread(Core(1),4,nout,rc)
         IF ( rc/=1 ) GOTO 1100
         nsub = Icore(3)
         CALL suread(Core(1),Nz,nout,rc)
         IF ( rc/=2 ) GOTO 1100
         ist = 1 + nout
         left = Nz - nout
         DO i = 1 , nsub
            CALL suread(Core(ist),left,nout,rc)
            IF ( rc/=2 .AND. rc/=3 ) GOTO 1100
            icomp = 1 + 2*(i-1)
            CALL cmiwrt(1,Name,Core(icomp),ist,nout,Core,Icore)
         ENDDO
         CALL suread(Core(ist),left,nout,rc)
         IF ( rc/=2 .AND. rc/=3 ) GOTO 1100
         CALL cmiwrt(8,Name,0,ist,nout,Core,Icore)
         RETURN
      ELSEIF ( Item==bgss ) THEN
!
!     BGSS TABLE
!
         CALL sfetch(Name,Item,1,rc)
         IF ( rc/=1 ) RETURN
         ngrd = 1
         CALL sjump(ngrd)
         IF ( ngrd<0 ) GOTO 1100
         ist = 1
         CALL suread(Core(ist),Nz,nout,rc)
         IF ( rc/=2 .AND. rc/=3 ) GOTO 1100
         CALL cmiwrt(2,Name,Name,ist,nout,Core,Icore)
         RETURN
      ELSEIF ( Item==cstm ) THEN
!
!     CSTM TABLE
!
         CALL sfetch(Name,Item,1,rc)
         IF ( rc/=1 ) RETURN
         ngrd = 1
         CALL sjump(ngrd)
         IF ( ngrd<0 ) GOTO 1100
         ist = 1
         CALL suread(Core(ist),Nz,nout,rc)
         IF ( rc/=2 .OR. rc/=3 ) GOTO 1100
         CALL cmiwrt(3,Name,Name,ist,nout,Core,Icore)
         RETURN
      ELSEIF ( Item==plts ) THEN
!
!     PLTS TABLE
!
         CALL sfetch(Name,Item,1,rc)
         IF ( rc/=1 ) RETURN
         CALL suread(Core(1),3,nout,rc)
         IF ( rc/=1 ) GOTO 1100
         ist = 1
         CALL suread(Core(ist),Nz,nout,rc)
         IF ( rc/=2 .AND. rc/=3 ) GOTO 1100
         CALL cmiwrt(4,Name,Name,ist,nout,Core,Icore)
         RETURN
      ELSEIF ( Item==lods ) THEN
!
!     LODS TABLE
!
         icode = 5
         GOTO 1000
      ELSEIF ( Item==loap ) THEN
!
!     LOAP TABLE
!
         icode = 7
         GOTO 1000
      ENDIF
   ENDIF
!
!     PERFORM UNFORMATED DUMP OF TABLE
!
 100  CALL sfetch(Name,Item,1,rc)
   IF ( rc/=1 ) GOTO 900
   DO i = 1 , 96
      Head2(i) = blank
   ENDDO
   DO i = 1 , 3
      Head2(i) = subs(i)
   ENDDO
   Head2(5) = Name(1)
   Head2(6) = Name(2)
   Head2(8) = itm
   Head2(10) = Item
   CALL page
   Head2(12) = cont
   Head2(13) = uned
   Head2(14) = d
   inum = Nz/2 - 1
   ns = inum + 1
   llen = 0
   Core(1) = oparen
   irec = 0
 200  WRITE (Otpe,99001) irec
99001 FORMAT ('0GROUP NO.',I4)
   irec = irec + 1
   Line = Line + 2
   IF ( Line>=Nlpp ) CALL page
   ix = inum
   nred = 0
   np = inum - 1
   iv = 4
 300  ix = ix + 1
   iout = 4
   nred = nred + 1
   np = np + 1
   CALL suread(Core(ix),1,flag,rc)
   IF ( rc<2 ) THEN
      i = numtyp(Core(ix)) + 1
      IF ( i==1 .AND. iv/=4 ) i = iv
      iv = i
      IF ( i==1 .OR. i==2 ) THEN
!
!     INTEGER  (3)
!
         iout = 3
         IF ( llen+13>132 ) GOTO 700
         Icore(nred+1) = intgc
         GOTO 500
      ELSEIF ( i==4 ) THEN
!
!     ALPHA   (2)
!
         iout = 2
         IF ( llen+6<=132 ) GOTO 600
         GOTO 700
      ELSE
!
!     REAL NUMBER  (1)
!
         iout = 1
         IF ( llen+13>132 ) GOTO 700
      ENDIF
   ELSEIF ( rc==2 ) THEN
      GOTO 700
   ELSE
      GOTO 800
   ENDIF
 400  Core(nred+1) = ec
   Core(nred+2) = ec1
   Core(nred+3) = ec2
   nred = nred + 2
 500  llen = llen + 13
   GOTO 300
 600  Core(nred+1) = alphc
   Core(nred+2) = alphc1
   nred = nred + 1
   GOTO 500
!
!     BUFFER FULL - END RECORD   PRINT LINE
!
 700  Core(nred+1) = cparen
   IF ( nred==1 ) WRITE (Otpe,99002)
99002 FORMAT ('0END OF GROUP - NULL GROUP')
   IF ( nred/=1 ) THEN
      IF ( Machx==2 .OR. Machx==5 ) WRITE (Otpe,Core) (Icore(i),i=ns,np)
      IF ( Machx/=2 .AND. Machx/=5 ) CALL wrtfmt(Icore(ns),np-ns+1,Ccore)
   ENDIF
   Line = Line + 1
   IF ( Line>=Nlpp ) CALL page
   llen = 0
   nred = 1
   np = inum
   Core(inum+1) = Core(ix)
   ix = inum + 1
   IF ( iout==1 ) GOTO 400
   IF ( iout==2 ) GOTO 600
   IF ( iout==3 ) THEN
      Icore(nred+1) = intgc
      GOTO 500
   ELSEIF ( iout==4 ) THEN
      GOTO 200
   ENDIF
!
!     END OF ITEM
!
 800  WRITE (Otpe,99003)
99003 FORMAT ('0END OF ITEM')
 900  RETURN
!
 1000 CALL sfetch(Name,Item,1,rc)
   IF ( rc/=1 ) RETURN
   CALL suread(Core(1),4,nout,rc)
   IF ( rc==1 ) THEN
      nsub = Icore(4)
      CALL suread(Core(1),Nz,nout,rc)
      IF ( rc==2 ) THEN
         ist = 1 + nout
         left = Nz - nout
         DO i = 1 , nsub
            CALL suread(Core(ist),left,nout,rc)
            IF ( rc/=2 .AND. rc/=3 ) GOTO 1100
            icomp = 1 + 2*(i-1)
            CALL cmiwrt(icode,Name,Core(icomp),ist,nout,Core,Icore)
            icode = 6
         ENDDO
         RETURN
      ENDIF
   ENDIF
!
!     INSUFFICIENT CORE OR ILLEGAL ITEM FORMAT - FORCE PHYSICAL DUMP
!
 1100 WRITE (Otpe,99004) Uwm , Item , Name
99004 FORMAT (A25,' 6231, INSUFFICIENT CORE AVAILABLE OR ILLEGAL ITEM ','FORMAT REQUIRES AN UNFORMATED',/31X,                       &
             &'DUMP TO BE PERFORM FOR ITEM ',A4,' OF SUBSTRUCTURE ',2A4)
   GOTO 100
END SUBROUTINE itmprt
