
SUBROUTINE inptt1
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ksystm(65) , Line , Mach , Nb , Nlpp , Nout , P1 , P2 , P3(2) , P4(2)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   REAL X(1)
   COMMON /blank / P1 , P2 , P3 , P4
   COMMON /machin/ Mach
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ X
!
! Local variable declarations
!
   INTEGER andf , korsz , rshift
   INTEGER bcdbin(4) , dx(3) , i , idhdr(7) , idhdrx(7) , ifile , in , inbuf , inn(10) , iold , ipt1 , ipt4 , iptx , ix , j , j1 ,  &
         & j2 , kf , lcor , mask , mete , mfiv , mfor , mnin , mone , msc , msix , mtre , mtwo , name(2) , namex(2) , nf , nnt ,    &
         & none(2) , nt(5,3) , oubuf , out(5) , output , p3x(2) , subnam(2) , tapcod(2) , trl(7) , zero
   LOGICAL tapbit
   LOGICAL tapeup
   EXTERNAL andf , rshift
!
! End of declarations
!
!
!     READ DATA BLOCK(S) FROM A NASTRAN USER TAPE WHICH MUST BE SET UP.
!
!     CALL TO THIS MODULE IS
!
!     INPUTT1  /O1,O2,O3,O4,O5/V,N,P1/V,N,P2/V,N,P3/V,N,P4  $
!
!     PARAMETERS P1 AND P2 ARE INTEGER INPUT, P3 AND P4 ARE BCD
!
!               P1= 0, NO ACTION TAKEN BEFORE READ (DEFAULT)
!                 =+N, SKIP FORWARD N DATA BLOCKS BEFORE READ
!                 =-1, USER TAPE IS REWOUND BEFORE READ
!                 =-2, A NEW REEL IS MOUNTED BEFORE READ
!                 =-3, THE NAMES OF ALL DATA BLOCKS ON USER TAPE ARE
!                      PRINTED AND READ OCCURS AT BEGINNING OF TAPE
!                 =-4, AN OUTPUT TAPE IS TO BE DISMOUNTED
!                      AFTER AN END-OF-FILE MARK IS WRITTEN.
!                      A NEW INPUT REEL WILL THEN BE MOUNTED.
!                 =-5, SEARCH USER TAPE FOR FIRST VERSION OF DATA
!                      BLOCKS REQUESTED.
!                      IF ANY ARE NOT FOUND, A FATAL TERMINATION
!                      OCCURS.
!                 =-6, SEARCH USER TAPE FOR FINAL VERSION OF DATA
!                      BLOCKS REQUESTED.
!                      IF ANY ARE NOT FOUND, A FATAL TERMINATION
!                      OCCURS.
!                 =-7, SEARCH USER TAPE FOR FIRST VERSION OF DATA
!                      BLOCKS REQUESTED.
!                      IF ANY ARE NOT FOUND, A WARNING OCCURS.
!                 =-8, SEARCH USER TAPE FOR FINAL VERSION OF DATA
!                      BLOCKS REQUESTED.
!                      IF ANY ARE NOT FOUND, A WARNING OCCURS.
!                 =-9, REWIND AND UNLOAD USER TAPE
!
!               P2= 0, FILE NAME IS INPT
!                 = 1, FILE NAME IS INP1
!                 = 2, FILE NAME IS INP2
!                 = 3, FILE NAME IS INP3
!                 = 4, FILE NAME IS INP4
!                 = 5, FILE NAME IS INP5
!                 = 6, FILE NAME IS INP6
!                 = 7, FILE NAME IS INP7
!                 = 8, FILE NAME IS INP8
!                 = 9, FILE NAME IS INP9
!                 THE MPL DEFAULT VALUE FOR P2 IS 0
!
!               P3=    TAPE ID CODE FOR USER TAPE, AN ALPHANUMERIC
!                      VARIABLE WHOSE VALUE MUST MATCH A CORRESPONDING
!                      VALUE ON THE USER TAPE.
!                      THIS CHECK IS DEPENDENT ON THE VALUE OF
!                      P1 AS FOLLOWS..
!                       *P1*             *TAPE ID CHECKED*
!                        +N                     NO
!                         0                     NO
!                        -1                    YES
!                        -2                    YES (ON NEW REEL)
!                        -3                    YES (WARNING CHECK)
!                        -4                    YES (ON NEW REEL)
!                        -5                    YES
!                        -6                    YES
!                        -7                    YES
!                        -8                    YES
!                        -9                     NO
!                      THE MPL DEFAULT VALUE FOR P3 IS XXXXXXXX
!
!
   EQUIVALENCE (Ksystm(1),Nb) , (Ksystm(2),Nout) , (Ksystm(9),Nlpp) , (Ksystm(12),Line)
   DATA subnam/4HINPT , 4HT1  / , msc/4HMSC /
   DATA out/201 , 202 , 203 , 204 , 205/ , mask/65535/
   DATA zero , mone , mtwo , mtre , mfor/0 , -1 , -2 , -3 , -4/ , mfiv , msix , mete , mnin/ - 5 , -6 , -8 , -9/
   DATA inn/4HINPT , 4HINP1 , 4HINP2 , 4HINP3 , 4HINP4 , 4HINP5 , 4HINP6 , 4HINP7 , 4HINP8 , 4HINP9/
   DATA idhdr/4HNAST , 4HRAN  , 4HUSER , 4H TAP , 4HE ID , 4H COD , 4HE - /
   DATA bcdbin/4HBCD  , 4H     , 4HBINA , 4HRY  /
   DATA none/4H (NO , 4HNE) / , ipt1 , ipt4/1H1 , 1H4/
!
!
   iptx = ipt1
   IF ( P4(1)/=msc ) GOTO 100
!
!
   ENTRY input1
!     ============
!
!     INPUT1 HANDELS MSC/OUTPUT1 DATA.
!     INPUT1 IS CALLED FROM INPTT1, WITH P4 = 'MSC', OR IT IS CALLED
!     FROM INPTT4
!
   iptx = ipt4
   IF ( P3(1)==bcdbin(1) .AND. P3(2)==bcdbin(2) ) GOTO 2200
   IF ( P3(1)==bcdbin(3) .AND. P3(2)==bcdbin(4) ) GOTO 2200
   WRITE (Nout,99001) Uim
99001 FORMAT (A29,'. INPUTT1 IS REQUESTED TO READ INPUT TAPE GENERATED',' IN MSC/OUTPUT1 COMPATIBLE RECORDS')
!
 100  lcor = korsz(X) - 2*Nb
   IF ( lcor<=0 ) CALL mesage(-8,lcor,subnam)
   inbuf = lcor + 1
   oubuf = inbuf + Nb
   tapcod(1) = P3(1)
   tapcod(2) = P3(2)
   IF ( P2<0 .OR. P2>9 ) THEN
!
      WRITE (Nout,99002) Ufm , iptx , P2
99002 FORMAT (A23,' 4112, MODULE INPUTT',A1,' - ILLEGAL VALUE FOR ','SECOND PARAMETER =',I20)
      GOTO 2300
   ELSE
      in = inn(P2+1)
      IF ( iptx==ipt4 ) WRITE (Nout,99003) Uim , Nb , in
99003 FORMAT (A29,', CURRENT NASTRAN BUFFER SIZE IS',I9,' WORDS',/5X,'SYNCHRONIZED BUFFSIZE IS REQUIRED IN CURRENT NASTRAN AND',    &
             &' THE VERSION THAT WROTE ',A4,' TAPE (OR FILE)',/5X,3(4H====),/)
      ifile = in
      IF ( Mach<5 ) THEN
         tapeup = tapbit(in)
         IF ( .NOT.tapeup ) THEN
!
            WRITE (Nout,99004) Ufm , in
99004       FORMAT (A23,' 4127, USER TAPE ',A4,' NOT SET UP.')
            GOTO 2300
         ENDIF
      ENDIF
      IF ( P1<mnin ) GOTO 1600
!
      IF ( P1==mnin ) THEN
!
         CALL unload(in)
         RETURN
      ELSEIF ( P1<mfor ) THEN
!
!
!     SEARCH MODE
!
!
!     EXAMINE OUTPUT REQUESTS AND FILL NAME TABLE
!
         nnt = 0
         DO i = 1 , 5
            output = out(i)
            trl(1) = output
            CALL rdtrl(trl)
            IF ( trl(1)>0 ) THEN
               CALL fname(output,name)
               IF ( iptx==ipt4 .AND. name(1)==none(1) .AND. name(2)==none(2) ) THEN
                  nt(i,2) = name(1)
                  nt(i,3) = name(2)
               ELSE
                  nt(i,1) = 0
                  nt(i,2) = name(1)
                  nt(i,3) = name(2)
                  nnt = nnt + 1
                  CYCLE
               ENDIF
            ENDIF
            nt(i,1) = -1
         ENDDO
!
         IF ( nnt>0 ) THEN
!
!     CHECK TAPE ID LABEL.
!
            CALL open(*1100,in,X(inbuf),0)
            CALL read(*1800,*1900,in,dx,3,0,nf)
            CALL read(*1800,*1900,in,idhdrx,7,0,nf)
            DO kf = 1 , 7
               IF ( idhdrx(kf)/=idhdr(kf) ) GOTO 2000
            ENDDO
            CALL read(*1800,*1900,in,p3x,2,1,nf)
            IF ( p3x(1)/=P3(1) .OR. p3x(2)/=P3(2) ) GOTO 1700
            CALL skpfil(in,1)
!
!
!     BEGIN SEARCH OF TAPE.
!
            kf = 0
            GOTO 600
         ELSE
            CALL page2(-2)
            WRITE (Nout,99005) Uwm , iptx
99005       FORMAT (A25,' 4137,  ALL OUTPUT DATA BLOCKS FOR INPUTT',A1,' ARE PURGED.')
            RETURN
         ENDIF
      ELSEIF ( P1==mtre ) THEN
!
!     OBTAIN LIST OF DATA BLOCKS ON USER TAPE.
!
         CALL open(*1100,in,X(inbuf),0)
         CALL read(*1800,*1900,in,dx,3,0,nf)
         CALL read(*1800,*1900,in,idhdrx,7,0,nf)
         DO kf = 1 , 7
            IF ( idhdrx(kf)/=idhdr(kf) ) GOTO 2000
         ENDDO
         CALL read(*1800,*1900,in,p3x,2,1,nf)
         IF ( p3x(1)/=P3(1) .OR. p3x(2)/=P3(2) ) THEN
!
            WRITE (Nout,99006) Uwm , p3x , P3
99006       FORMAT (A25,' 4135, USER TAPE ID CODE -',2A4,'- DOES NOT MATCH ','THIRD INPUTT1 DMAP PARAMETER -',2A4,2H-.)
            Line = Line + 2
         ENDIF
         CALL skpfil(in,1)
         kf = 0
         GOTO 400
      ELSEIF ( P1<=zero ) THEN
!
         IF ( P1==mtwo .OR. P1==mfor ) THEN
!
!     P1 = -2 OR P1 = -4 IS ACCEPTABLE ONLY ON IBM OR UNIVAC
!
            IF ( Mach/=2 .AND. Mach/=3 ) GOTO 1600
!
            iold = -P1/2
            CALL open(*1100,in,X(inbuf),2)
            CALL tpswit(in,iold,1,tapcod)
         ENDIF
!
         IF ( P1==mone .OR. P1==mtwo .OR. P1==mfor ) THEN
!
!     OPEN USER TAPE TO READ WITH REWIND AHD TAPE ID CHECK
!
            IF ( P1==mone .OR. P1==mtwo .OR. P1==mfor .OR. iptx/=ipt4 ) THEN
               CALL open(*1100,in,X(inbuf),0)
               CALL read(*1800,*1900,in,dx,3,0,nf)
               CALL read(*1800,*1900,in,idhdrx,7,0,nf)
               DO kf = 1 , 7
                  IF ( idhdrx(kf)/=idhdr(kf) ) GOTO 2000
               ENDDO
               CALL read(*1800,*1900,in,p3x,2,1,nf)
               IF ( p3x(1)/=P3(1) .OR. p3x(2)/=P3(2) ) GOTO 1700
               CALL skpfil(in,1)
               GOTO 200
            ENDIF
         ENDIF
!
!     OPEN USER TAPE TO READ WITHOUT REWIND AND NO TAPE ID CHECK
!
         CALL open(*1100,in,X(inbuf),2)
         IF ( iptx==ipt4 ) CALL fwdrec(*1900,ix)
      ELSE
!
         CALL open(*1100,in,X(inbuf),2)
         DO i = 1 , P1
            CALL read(*1500,*1500,in,namex,2,0,nf)
            CALL skpfil(in,1)
         ENDDO
      ENDIF
   ENDIF
!
 200  DO i = 1 , 5
      output = out(i)
      trl(1) = output
      CALL rdtrl(trl)
      IF ( trl(1)<=0 ) CYCLE
      CALL fname(output,name)
      IF ( name(1)==none(1) .AND. name(2)==none(2) ) CYCLE
!
!     PASS FILE NAME HEADER RECORD
!
      CALL read(*1300,*1400,in,namex,2,0,nf)
!
!     READ TRAILER RECORD, SIX WORDS (OR 3 WORDS, IPTX=4 ONLY)
!
      CALL read(*1300,*250,in,trl(2),6,1,nf)
      GOTO 300
!
!     JUST A NOTE, FROM G.CHAN/UNISYS -
!     LEVEL 17.5 USED 2 RECORDS HERE FOR THE MATRIX NAME (2 BCD WORDS,
!     1ST RECORD) AND 7 TRAILER WORDS (2ND RECORD)
!
 250  IF ( iptx/=ipt4 .OR. nf<3 ) GOTO 1400
      trl(5) = trl(2)
      trl(6) = trl(3)
      trl(7) = trl(4)
      DO j = 2 , 7
         j1 = j/2 + 4
         j2 = mod(j-1,2)*16
         trl(j) = andf(rshift(trl(j1),j2),mask)
      ENDDO
!
!     OPEN OUTPUT DATA BLOCK TO WRITE WITH REWIND
!
 300  CALL open(*1200,output,X(oubuf),1)
!
!     COPY CONTENTS OF USER TAPE ONTO OUTPUT DATA BLOCK, INCLUDING
!     FILE NAME IN RECORD 0
!
      CALL cpyfil(in,output,X,lcor,nf)
!
!     CLOSE OUTPUT DATA BLOCK WITH REWIND AND EOF
!
      CALL close(output,1)
!
!     WRITE TRAILER
!
      trl(1) = output
      CALL wrttrl(trl)
      CALL page2(-3)
      WRITE (Nout,99007) Uim , name , in , namex
99007 FORMAT (A29,' 4105,     DATA BLOCK ',2A4,' RETRIEVED FROM USER ','TAPE',A4,/5X,'NAME OF DATA BLOCK WHEN PLACED ON USER ',     &
             &'TAPE WAS ',2A4)
!
   ENDDO
!
!     CLOSE NASTRAN USER TAPE WITHOUT REWIND
!
   CALL close(in,2)
   RETURN
 400  CALL page1
   Line = Line + 5
   WRITE (Nout,99008) in
99008 FORMAT (1H0,50X,A4,14H FILE CONTENTS,/46X,4HFILE,18X,4HNAME/1H0)
   DO
      CALL read(*500,*2100,in,namex,2,1,nf)
      CALL skpfil(in,1)
      kf = kf + 1
      Line = Line + 1
      WRITE (Nout,99009) kf , namex
99009 FORMAT (45X,I5,18X,2A4)
      IF ( Line>=Nlpp ) GOTO 400
   ENDDO
 500  CALL rewind(in)
   CALL skpfil(in,1)
   GOTO 200
 600  CALL read(*900,*2100,in,namex,2,0,nf)
   kf = kf + 1
!
   DO i = 1 , 5
      name(1) = nt(i,2)
      name(2) = nt(i,3)
      IF ( nt(i,1)<0 ) CYCLE
      IF ( name(1)/=namex(1) .OR. name(2)/=namex(2) ) CYCLE
      nt(i,1) = nt(i,1) + 1
      IF ( nt(i,1)==1 .OR. P1==msix .OR. P1==mete ) THEN
         CALL read(*1300,*650,in,trl(2),6,1,nf)
         GOTO 700
      ELSE
         CALL page2(-3)
         WRITE (Nout,99010) Uwm , name , kf , in
99010    FORMAT (A25,' 4138,  DATA BLOCK ',2A4,' (DATA BLOCK COUNT =',I5,') HAS PREVIOUSLY BEEN RETRIEVED FROM',/36X,'USER TAPE ',  &
               & A4,' AND WILL BE IGNORED.')
         EXIT
      ENDIF
 650  IF ( iptx/=ipt4 .OR. nf<3 ) GOTO 1400
      trl(5) = trl(2)
      trl(6) = trl(3)
      trl(7) = trl(4)
      DO j = 2 , 7
         j1 = j/2 + 4
         j2 = mod(j-1,2)*16
         trl(j) = andf(rshift(trl(j1),j2),mask)
      ENDDO
 700  output = out(i)
      CALL open(*1200,output,X(oubuf),1)
      CALL cpyfil(in,output,X,lcor,nf)
      CALL close(output,1)
      trl(1) = output
      CALL wrttrl(trl)
      CALL page2(-2)
      WRITE (Nout,99011) Uim , name , in , kf
99011 FORMAT (A29,' 4139, DATA BLOCK ',2A4,' RETRIEVED FROM USER TAPE ',A4,' (DATA BLOCK COUNT =',I5,1H))
      IF ( nt(i,1)>1 ) THEN
         WRITE (Nout,99012) Uwm
99012    FORMAT (A25,' 4140, SECONDARY VERSION OF DATA BLOCK HAS REPLACED',' EARLIER ONE.')
         CALL page2(-2)
      ELSE
         nnt = nnt - 1
      ENDIF
      GOTO 800
   ENDDO
!
   CALL skpfil(in,1)
 800  IF ( nnt<=0 .AND. P1/=msix .AND. P1/=mete ) GOTO 1000
   GOTO 600
!
 900  IF ( nnt>0 ) THEN
      CALL page2(-7)
      IF ( P1==mfiv .OR. P1==msix ) THEN
!
         WRITE (Nout,99013) Ufm
99013    FORMAT (A23,' 4142, ONE OR MORE DATA BLOCKS NOT FOUND ON USER ','TAPE',/)
         DO i = 1 , 5
            IF ( nt(i,1)==0 ) THEN
               WRITE (Nout,99014) nt(i,2) , nt(i,3)
99014          FORMAT (20X,'NAME OF DATA BLOCK = ',2A4)
               Line = Line + 1
            ENDIF
         ENDDO
         GOTO 2300
      ELSE
         WRITE (Nout,99015) Uwm
99015    FORMAT (A25,' 4141, ONE OR MORE DATA BLOCKS NOT FOUND ON USER ','TAPE.')
         DO i = 1 , 5
            IF ( nt(i,1)==0 ) THEN
               WRITE (Nout,99016) nt(i,2) , nt(i,3)
99016          FORMAT (20X,21HNAME OF DATA BLOCK = ,2A4)
            ENDIF
         ENDDO
         IF ( P1==mfiv .OR. P1==msix ) GOTO 2300
      ENDIF
   ENDIF
!
 1000 CALL skpfil(in,-1)
   CALL close(in,2)
   RETURN
!
!     ERRORS
!
 1100 WRITE (Nout,99017) Sfm , iptx , in
99017 FORMAT (A25,' 4107, MODULE INPTT',A1,' UNABLE TO OPEN NASTRAN ','FILE ',A4,1H.)
   GOTO 2300
!
 1200 WRITE (Nout,99018) Sfm , iptx , output
99018 FORMAT (A25,' 4108, SUBROUTINE INPTT',A1,' UNABLE TO OPEN OUTPUT',' DATA BLOCK',I5)
   GOTO 2300
!
 1300 CALL mesage(-2,ifile,subnam)
!
 1400 CALL mesage(-3,ifile,subnam)
!
 1500 WRITE (Nout,99019) Ufm , iptx , P1 , in , i
99019 FORMAT (A22,' 4111, MODULE INPUTT',A1,' IS UNABLE TO SKIP FORWARD',I10,' DATA BLOCKS ON PERMANENT NASTRAN FILE ',A4,1H.,/5X,  &
             &'NUMBER OF DATA BLOCKS SKIPPED =',I5)
   Line = Line + 1
   GOTO 2300
!
 1600 WRITE (Nout,99020) Ufm , iptx , P1
99020 FORMAT (A23,' 4113, MODULE INPUTT',A1,' - ILLEGAL VALUE FOR ','FIRST PARAMETER =',I20)
   GOTO 2300
!
 1700 WRITE (Nout,99021) Ufm , p3x , iptx , P3
99021 FORMAT (A23,' 4136, USER TAPE ID CODE -',2A4,'- DOES NOT MATCH ','THIRD INPUTT',A1,' DMAP PARAMETER -',2A4,2H-.)
   GOTO 2300
!
 1800 WRITE (Nout,99022) Ufm , iptx , in
99022 FORMAT (A23,' 4132, MODULE INPUTT',A1,' - END-OF-FILE ENCOUNTERED',' WHILE ATTEMPTING TO READ TAPE ID CODE ON USER TAPE ',A4, &
             &1H.)
   GOTO 2300
!
 1900 WRITE (Nout,99023) Ufm , iptx , in
99023 FORMAT (A23,' 4133, MODULE INPUTT',A1,' - END-OF-RECORD ','ENCOUNTERED WHILE ATTEMPTING TO READ TAPE ID CODE ON ',            &
            & 'USER TAPE ',A4,1H.)
   GOTO 2300
!
 2000 WRITE (Nout,99024) Ufm , iptx , idhdrx
99024 FORMAT (A23,' 4134, MODULE INPUTT',A1,' - ILLEGAL TAPE CODE HEADER = ',7A4)
   GOTO 2300
!
 2100 WRITE (Nout,99025) Sfm , iptx
99025 FORMAT (A25,' 4106, MODULE INPUTT',A1,' - SHORT RECORD.')
   GOTO 2300
!
 2200 WRITE (Nout,99026) Ufm , P3
99026 FORMAT (A23,', ILLEGAL TAPE LABEL NAME -',2A4,'-  POSSIBLY ','THE 4TH PARAMETER OF INPTT4 IS IN ERROR')
!
!
 2300 Line = Line + 2
   CALL mesage(-61,0,0)
!
END SUBROUTINE inptt1
