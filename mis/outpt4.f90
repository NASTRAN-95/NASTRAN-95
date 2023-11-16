
SUBROUTINE outpt4
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER D(3) , Ibuff , Ii , Incr , Itu , Ix(3) , Jj , Line , Mach , Nbpw , Nlpp , Nout , Nwd(4) , P1 , P2 , P3
   CHARACTER*80 Dsnames(80)
   REAL Dum1(6) , Dum2(2) , Dum22(22) , Dum3(2) , Prc(2) , X(1) , Xns(1)
   DOUBLE PRECISION Dx(1) , Dxns(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / P1 , P2 , P3
   COMMON /dsname/ Dsnames
   COMMON /machin/ Mach
   COMMON /system/ Ibuff , Nout , Dum1 , Nlpp , Dum2 , Line , Dum3 , D , Dum22 , Nbpw
   COMMON /type  / Prc , Nwd
   COMMON /unpakx/ Itu , Ii , Jj , Incr
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ X
!
! Local variable declarations
!
   INTEGER block(20) , buf1 , i , ifirst , inp(13) , input , ipt , j , j1 , j2 , k , l , lcor , ln , m , mrow , name(2) , nc ,      &
         & ndict , nm1 , none(2) , nr , nw , nwds , nword1 , nwords , sub(2) , tab1(6) , tab2(6) , trl(8) , zero
   LOGICAL bo , cp , dp , sp , sparse
   CHARACTER*6 dns , ds , spa
   CHARACTER*11 fm , fmd , unf
   INTEGER korsz
   REAL rzero
!
! End of declarations
!
!
!     COPY MATRIX DATA BLOCKS ONTO A FORTRAN TAPE, BINARY OR ASCII
!     FORMATS, IN DENSE MATRIX FORM (FROM FIRST TO LAST NON-ZERO TERMS
!     OF COLUMNS), OR IN SPARSE FORM (BY STRINGS)
!
!     A LOGICAL OUTPUT RECORD, WHICH CAN BE ONE OR MORE PHYSICAL RECORES
!     BEGINS WITH 3 INTEGER WORD THEN AN ARRAY OF DATA
!
!     FIRST  INTEGER WORD = LOGICAL RECORD NUMBER, OR COLUMN NUMBER
!     SECOND INTEGER WORD = ROW POSITION OF 1ST NONZERO TERM IN COLUMN
!                         = 0, SPARSE MATRIX (BINARY ONLY)
!                       .LT.0, SPARSE MATRIX ROW POSITION (ASCII ONLY)
!     THIRD  INTEGER WORD = NW, LENGTH OF ARRAY DATA THAT FOLLOW
!                           NW IS BASED ON S.P. WORD COUNT (BINARY ONLY)
!                           NW IS DATA PRECISION TYPE DEPENDENT (ASCII)
!
!     OUTPUT4 DOES NOT HANDLE TABLE DATA BLOCK, EXECPT 6 SPECIAL TABLES
!     KELM, MELM, BELM, KDICT, MDICT, AND BDICT.
!
!
!     OUTPUT4   IN1,IN2,IN3,IN4,IN5 // V,N,P1 / V,N,P2 / V,N,P3  $
!
!     PARAMETERS P1, P2 AND P3 ARE INTEGERS
!
!     P1 = 0, NO ACTION TAKEN BEFORE WRITE (DEFAULT)
!        =-1, REWIND TAPE BEFORE WRITE
!        =-2, AT END, WRITE E-O-F MARK AND REWIND TAPE
!        =-3, BOTH -1 AND -2
!        =-9, NOT AVAILABLE
!
!     P2 = N, FORTRAN OUTPUT UNIT N (N = 11,...,24)
!        =-N, MATRIX WILL BE WRITTEN OUT IN SPARSE FORMAT ONTO UNIT N.
!
!     P3 = 1, FILE OUTPUT IN FORTRAN BINARY FORMAT (UNFORMATTED)
!        = 2, FILE OUTPUT IN BCD FORMAT (ASCII, FORMATTED)
!          .  NO MIXED INTEGERS AND REAL NUMBERS IN A FORMATTED RECORD.
!             THE RECORD LENGTH IS LESS THAN 132 BYTES.
!          .  IF INPUT MATRIX TO BE COPIED OUT IS IN S.P., INTEGERS ARE
!             WRITTEN OUT IN I13, AND S.P.REAL DATA IN 10E13.6.
!          .  IF INPUT MATRIX TO BE COPIED OUT IS IN D.P., INTEGERS ARE
!             WRITTEN OUT IN I16, AND D.P.REAL DATA IN 8D16.9.
!        = 3, FORMATS I16 AHD 8E16.9 ARE USED TO COPY INTEGERS AND S.P.
!             REAL DATA OUT TO OUTPUT TAPE. P3=3 IS USED ONLY FOR
!             MACHINE WITH LONG WORDS (60 OR MORE BITS PER WORD)
!
!     THESE OUTPUT FORMATS CAN BE CHANGED EASILY BY ALTERING FORMATS
!     40, 50, 60 AND 370. MAKE SURE AN OUTPUT LINE DOES NOT EXCEED 132
!     COLUMNS. OTHERWISE, IT WOULD BE FOLDED IN PRINTOUT OR SCREEN
!     LISTING.
!
!     WRITTEN BY G.CHAN/UNISYS  3/93
!
!WKBI
!WKBI
   EQUIVALENCE (X(1),Xns(1))
   EQUIVALENCE (X(1),Ix(1),Dx(1)) , (Xns(1),Dxns(1)) , (nm1,name(1))
   DATA inp/4HUT1  , 4HUT2  , 4HUT3  , 4HINPT , 4HINP1 , 4HINP2 , 4HINP3 , 4HINP4 , 4HINP5 , 4HINP6 , 4HINP7 , 4HINP8 , 4HINP9/
   DATA tab1/4HKELM , 4HMELM , 4HBELM , 4HKDIC , 4HMDIC , 4HBDIC/ , tab2/4HHKEL , 4HHMEL , 4HHBEL , 4HHKDI , 4HHMDI , 4HHBDI/
   DATA dns , spa/'DENSE ' , 'SPARSE'/rzero , zero/0. , 0/
   DATA fmd , unf/'FORMATTED  ' , 'UNFORMATTED'/
   DATA none , sub/4H (NO , 4HNE)  , 4HOUTP , 4HT4  /
!WKBI
   DATA ifirst/0/
!
   sparse = P2<0
   P2 = iabs(P2)
   IF ( P2>10 .AND. P2<=24 ) THEN
      bo = P3/=1
      Ii = 1
      Incr = 1
      lcor = korsz(X(1))
      buf1 = lcor - Ibuff
!
      fm = unf
      IF ( bo ) fm = fmd
!WKBNB
      IF ( .NOT.(bo .OR. ifirst/=0) ) THEN
         CLOSE (UNIT=P2)
         OPEN (UNIT=P2,STATUS='NEW',ACCESS='SEQUENTIAL',FORM=fm,ERR=100,FILE=Dsnames(P2))
      ENDIF
      ifirst = 1
!WBKNE
      IF ( P1==-1 .OR. P1==-3 ) REWIND P2
!
      DO ipt = 1 , 5
         ndict = 0
         input = 100 + ipt
         trl(1) = input
         CALL rdtrl(trl(1))
         IF ( trl(1)<=0 ) CYCLE
         CALL fname(input,name)
         IF ( nm1/=none(1) .OR. name(2)/=none(2) ) THEN
            IF ( trl(7)/=0 .OR. trl(8)/=0 ) THEN
               IF ( trl(4)>=1 .AND. trl(4)<=8 ) THEN
                  nc = trl(2)
                  nr = trl(3)
                  Itu = trl(5)
                  IF ( nc==0 .OR. nr==0 .OR. (Itu<1 .OR. Itu>4) ) GOTO 10
                  nwds = Nwd(Itu)
                  IF ( nr*nwds>=buf1 ) CALL mesage(-8,lcor,sub)
                  dp = Itu==2 .OR. Itu==4
                  sp = .NOT.dp
                  cp = sp .AND. P3==3 .AND. Nbpw>=60
                  IF ( cp ) sp = .FALSE.
                  IF ( bo .AND. sparse .AND. nc>2000 ) WRITE (Nout,99001) Uwm
99001             FORMAT (A25,' FROM OUTPUT4 MODULE. ON ASCII TAPE AND SPARSE ',                                                    &
                         &'MATRIX OUTPUT, EACH STRING OF DATA IS WRITTEN OUT TO THE',/5X,                                           &
                         &'OUTPUT TAPE AS A FORTRAN FORMATTED REDORD. FATAL ERROR',                                                 &
                         &' COULD OCCUR WHEN NO. OF RECORDS EXCEED SYSTEM I/O LIMIT')
!
!     OPEN INPUT DATA BLOCK TO READ WITH REWIND
!
                  CALL open(*200,input,X(buf1),0)
                  CALL fwdrec(*200,input)
!
                  block(1) = input
!
!     WRITE TRAILER RECORD ON OUTPUT TAPE.
!     SET FORM (TRL(4)) TO NEGATIVE IF ASCII RECORDS IS REQUESTED
!
                  k = -trl(4)
                  IF ( .NOT.bo ) WRITE (P2) nc , nr , trl(4) , Itu , name
                  IF ( bo ) WRITE (P2,99007) nc , nr , k , Itu , name
!
                  IF ( sparse ) THEN
!
!     SPARSE MATRIX OUPUT -
!     WRITE A RECORD FOR EACH MATRIX COLUMN, IN PACKED STRINGS DATA
!     IF MATRIX IS NOT WRITTEN IN STRINGS, SEND THE MATRIX TO THE DENSE
!     MATRIX METHOD
!
                     CALL rectyp(input,k)
                     IF ( k/=0 ) THEN
!
!     BLOCK(2) = STRING TYPE, 1,2,3 OR 4
!     BLOCK(4) = FIRST ROW POSITION ON A MATRIX COLUMN
!     BLOCK(5) = POINTER TO STRING IN XNS ARRAY
!     BLOCK(6) = NO. OF TERMS IN STRING
!
                        nwords = Nwd(Itu)
                        nword1 = nwords - 1
                        DO k = 1 , nc
                           block(8) = -1
                           nw = 0
                           DO
                              CALL getstr(*2,block)
                              IF ( bo ) THEN
!
!     NOTE - FOR THE BCD OUTPUT RECORD, THE 2ND INTEGER WORD, ZERO
!     BEFORE, IS REPLACED BY THE NEGATIVE OF THE ROW POSITION (IN A
!     MATRIX COLUMN).
!     DOUBLE THE POINTER TO THE STRING IN XNS ARRAY, J1, IF DATA TYPE IS
!     COMPLEX, BUT NOT THE LENGTH LN.
!
                                 ln = block(6)
                                 j1 = block(5)
                                 IF ( block(2)>=3 ) j1 = j1*2
                                 j2 = j1 + ln - 1
                                 mrow = -block(4)
!
!                   ZERO REPLACED   EXACT LENGTH OF XNS, OR DXNS
!                               /   /
                                 IF ( sp ) WRITE (P2,99008) k , mrow , ln , (Xns(j),j=j1,j2)
                                 IF ( cp ) WRITE (P2,99009) k , mrow , ln , (Xns(j),j=j1,j2)
                                 IF ( dp ) WRITE (P2,99010) k , mrow , ln , (Dxns(j),j=j1,j2)
                              ELSE
                                 ln = block(6)*nwords
                                 j1 = block(5)*nwords - nword1
                                 j2 = j1 + ln - 1
!
                                 nw = nw + 1
                                 Ix(nw) = block(4) + 65536*block(6)
                                 l = 1
                                 DO j = j1 , j2
                                    X(l+nw) = Xns(j)
                                    l = l + 1
                                 ENDDO
                                 nw = nw + ln
                                 IF ( nw>=buf1 ) CALL mesage(-8,lcor,sub)
                              ENDIF
                              CALL endget(block)
                           ENDDO
 2                         IF ( nw>0 ) WRITE (P2) k , zero , nw , (X(j),j=1,nw)
!
                        ENDDO
                        GOTO 6
                     ELSE
                        CALL rewind(input)
                        CALL fwdrec(*200,input)
                     ENDIF
                  ENDIF
!
!     DENSE MATRIX OUTPUT -
!     WRITE THE MATRIX COLUMNS FROM FIRST TO LAST NON-ZERO TERMS
!
                  DO k = 1 , nc
                     Ii = 0
                     CALL unpack(*4,input,X)
                     Jj = (Jj-Ii+1)*nwds
                     IF ( .NOT.(bo) ) THEN
!
                        WRITE (P2) k , Ii , Jj , (X(l),l=1,Jj)
                        CYCLE
                     ENDIF
!
 4                   m = Jj/2
                     IF ( sp ) WRITE (P2,99008) k , Ii , Jj , (X(l),l=1,Jj)
                     IF ( cp ) WRITE (P2,99009) k , Ii , Jj , (X(l),l=1,Jj)
                     IF ( dp ) WRITE (P2,99010) k , Ii , Jj , (Dx(l),l=1,m)
!
!
                  ENDDO
!
!     WRITE AN EXTRA NCOL+1 COLUMN RECORD OUT TO P2, AND AT LEAST ONE
!     VALUE OF ZERO
!
 6                m = 1
                  k = nc + 1
                  IF ( bo ) THEN
                     IF ( sp ) WRITE (P2,99008) k , m , m , rzero
                     IF ( cp .OR. dp ) WRITE (P2,99009) k , m , m , rzero
                  ELSE
                     WRITE (P2) k , m , m , rzero
                  ENDIF
                  ds = dns
                  IF ( sparse ) ds = spa
                  WRITE (Nout,99002) Uim , name , P2 , inp(P2-10) , fm , ds , (trl(l),l=2,7)
99002             FORMAT (A29,' FROM OUTPUT4 MODULE. DATA BLOCK ',2A4,' WAS WRITTEN',' OUT TO FORTRAN TAPE',I3,' (',A4,')',/5X,     &
                        & 'IN ',A11,' RECORDS. ',A6,' MATRIX FORM.  TRAILER =',5I6,I9)
                  CALL close(input,1)
                  CYCLE
               ENDIF
            ENDIF
!
!     INPUT FILE IS A TABLE DATA BLOCK
!     ONLY 6 SPECIAL TABLES ARE ALLOWED
!
 10         DO i = 1 , 6
               IF ( nm1==tab1(i) .OR. nm1==tab2(i) ) GOTO 20
            ENDDO
            IF ( bo ) WRITE (P2,99011) Uwm , input , name , (trl(j),j=2,7)
            WRITE (Nout,99011) Uwm , input , name , (trl(j),j=2,7)
            CALL close(input,1)
         ENDIF
         CYCLE
!
!     KELM, MELM AND BELM (AND HKELM, HMELM AND HBELM) TALBES
!
 20      IF ( sparse ) WRITE (Nout,99003) Uwm , name , P2 , P3
99003    FORMAT (A25,'. PARAMETER P2 FOR SPARSE MATRIX IS MEANINGLESS FOR',' THE ',2A8,' INPUT FILE.   P2,P3 =',2I4,/)
         CALL open(*200,input,X(buf1),0)
         CALL fwdrec(*200,input)
         k = -trl(4)
         IF ( .NOT.bo ) WRITE (P2) nc , nr , trl(4) , Itu , name
         IF ( bo ) WRITE (P2,99007) nc , nr , k , Itu , name
         j = 1
         k = 0
         IF ( i<4 ) THEN
            dp = trl(2)==2
            sp = .NOT.dp
            cp = sp .AND. P3==3 .AND. Nbpw>=60
            IF ( cp ) sp = .FALSE.
         ENDIF
 40      k = k + 1
         CALL read(*80,*60,input,X,buf1-1,1,m)
         CALL mesage(-8,0,sub)
 60      IF ( i>=4 ) THEN
!
!     KDICT, MDICT AND BDICT (AND HKDICT, HMDICT AND HBDICT) TABLES.
!     INTEGERIZE THE DAMPING CONSTANT (BY 10**8) BEFORE OUTPUT THE ARRAY
!
            ndict = Ix(3) + 5
            DO i = 8 , m , ndict
               Ix(i) = ifix(X(i)*100000000.)
            ENDDO
            IF ( .NOT.bo ) WRITE (P2) k , j , m , (Ix(l),l=1,m)
            IF ( bo .AND. .NOT.cp ) WRITE (P2,99012) k , j , m , (Ix(l),l=1,m)
            IF ( bo .AND. cp ) WRITE (P2,99013) k , j , m , (Ix(l),l=1,m)
         ELSEIF ( bo ) THEN
!
            IF ( dp ) THEN
               m = m/2
               WRITE (P2,99010) k , j , m , (Dx(l),l=1,m)
            ELSE
               IF ( sp ) WRITE (P2,99008) k , j , m , (X(l),l=1,m)
               IF ( cp ) WRITE (P2,99009) k , j , m , (X(l),l=1,m)
            ENDIF
         ELSE
            WRITE (P2) k , j , m , (X(l),l=1,m)
         ENDIF
         GOTO 40
!
 80      IF ( .NOT.bo ) WRITE (P2) k , j , j , zero
         IF ( bo .AND. .NOT.cp ) WRITE (P2,99012) k , j , j , zero
         IF ( bo .AND. cp ) WRITE (P2,99013) k , j , j , zero
         IF ( ndict/=0 ) WRITE (Nout,99004) Uim , name , inp(P2-10)
99004    FORMAT (A29,'. THE DAMPING CONSTANT TERMS FROM ',2A4,' WERE ','MULTIPLIED BY 10**8, AND INTEGERIZED',/5X,                  &
                &'BEFORE WRITING OUT TO ',A4,' OUTPUT FILE')
         CALL close(input,1)
!
      ENDDO
!
      IF ( P1==-2 .OR. P1==-3 ) THEN
         ENDFILE P2
         REWIND P2
         CLOSE (UNIT=P2)
      ENDIF
      GOTO 99999
   ENDIF
!
!     ERRORS
!
 100  WRITE (Nout,99005) Ufm , P2
99005 FORMAT (A23,'. CANNOT OPEN OUTPUT FORTRAN FILE. UNIT =',I4)
   GOTO 300
 200  WRITE (Nout,99006) Uwm , input
99006 FORMAT (A25,'. OUTPT4 CANNOT OPEN INPUT DATA BLOCK',I5)
!
 300  CALL mesage(-37,0,sub)
99007 FORMAT (1X,4I13,5X,2A4)
99008 FORMAT (1X,3I13,/,(1X,10E13.6))
99009 FORMAT (1X,3I16,/,(1X,8E16.9))
99010 FORMAT (1X,3I16,/,(1X,8D16.9))
99011 FORMAT (A25,'. INPUT DATA BLOCK',I5,2H, ,2A4,', IS A TABLE OR A ','NULL MATRIX. OUTPUT4 MODULE HANDLES ONLY MATRICES',/5X,    &
             &'TRAILER =',6I6)
99012 FORMAT (1X,3I13,/,(1X,10I13))
99013 FORMAT (1X,3I13,/,(1X,8I16))
99999 RETURN
END SUBROUTINE outpt4