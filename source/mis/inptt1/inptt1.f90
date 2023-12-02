!*==inptt1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE inptt1
   USE c_blank
   USE c_machin
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(4) , SAVE :: bcdbin
   INTEGER , DIMENSION(3) :: dx
   INTEGER :: i , ifile , in , inbuf , iold , iptx , ix , j , j1 , j2 , kf , lcor , line , nb , nf , nlpp , nnt , nout , oubuf ,    &
            & output
   INTEGER , DIMENSION(7) , SAVE :: idhdr
   INTEGER , DIMENSION(7) :: idhdrx , trl
   INTEGER , DIMENSION(10) , SAVE :: inn
   INTEGER , SAVE :: ipt1 , ipt4 , mask , mete , mfiv , mfor , mnin , mone , msc , msix , mtre , mtwo , zero
   INTEGER , DIMENSION(2) :: name , namex , p3x , tapcod
   INTEGER , DIMENSION(2) , SAVE :: none , subnam
   INTEGER , DIMENSION(5,3) :: nt
   INTEGER , DIMENSION(5) , SAVE :: out
   LOGICAL :: tapeup
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Ksystm(1),Nb) , (Ksystm(2),Nout) , (Ksystm(9),Nlpp) , (Ksystm(12),Line)
   DATA subnam/4HINPT , 4HT1  / , msc/4HMSC /
   DATA out/201 , 202 , 203 , 204 , 205/ , mask/65535/
   DATA zero , mone , mtwo , mtre , mfor/0 , -1 , -2 , -3 , -4/ , mfiv , msix , mete , mnin/ - 5 , -6 , -8 , -9/
   DATA inn/4HINPT , 4HINP1 , 4HINP2 , 4HINP3 , 4HINP4 , 4HINP5 , 4HINP6 , 4HINP7 , 4HINP8 , 4HINP9/
   DATA idhdr/4HNAST , 4HRAN  , 4HUSER , 4H TAP , 4HE ID , 4H COD , 4HE - /
   DATA bcdbin/4HBCD  , 4H     , 4HBINA , 4HRY  /
   DATA none/4H (NO , 4HNE) / , ipt1 , ipt4/1H1 , 1H4/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         iptx = ipt1
         IF ( p4(1)/=msc ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
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
         IF ( p3(1)==bcdbin(1) .AND. p3(2)==bcdbin(2) ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( p3(1)==bcdbin(3) .AND. p3(2)==bcdbin(4) ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         WRITE (nout,99001) uim
99001    FORMAT (A29,'. INPUTT1 IS REQUESTED TO READ INPUT TAPE GENERATED',' IN MSC/OUTPUT1 COMPATIBLE RECORDS')
         spag_nextblock_1 = 2
      CASE (2)
!
         lcor = korsz(x) - 2*nb
         IF ( lcor<=0 ) CALL mesage(-8,lcor,subnam)
         inbuf = lcor + 1
         oubuf = inbuf + nb
         tapcod(1) = p3(1)
         tapcod(2) = p3(2)
         IF ( p2<0 .OR. p2>9 ) THEN
!
            WRITE (nout,99002) ufm , iptx , p2
99002       FORMAT (A23,' 4112, MODULE INPUTT',A1,' - ILLEGAL VALUE FOR ','SECOND PARAMETER =',I20)
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ELSE
            in = inn(p2+1)
            IF ( iptx==ipt4 ) WRITE (nout,99003) uim , nb , in
99003       FORMAT (A29,', CURRENT NASTRAN BUFFER SIZE IS',I9,' WORDS',/5X,                                                         &
                   &'SYNCHRONIZED BUFFSIZE IS REQUIRED IN CURRENT NASTRAN AND',' THE VERSION THAT WROTE ',A4,' TAPE (OR FILE)',/5X, &
                  & 3(4H====),/)
            ifile = in
            IF ( mach<5 ) THEN
               tapeup = tapbit(in)
               IF ( .NOT.tapeup ) THEN
!
                  WRITE (nout,99004) ufm , in
99004             FORMAT (A23,' 4127, USER TAPE ',A4,' NOT SET UP.')
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            IF ( p1<mnin ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
            IF ( p1==mnin ) THEN
!
               CALL unload(in)
               RETURN
            ELSEIF ( p1<mfor ) THEN
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
                  CALL open(*60,in,x(inbuf),0)
                  CALL read(*160,*180,in,dx,3,0,nf)
                  CALL read(*160,*180,in,idhdrx,7,0,nf)
                  DO kf = 1 , 7
                     IF ( idhdrx(kf)/=idhdr(kf) ) THEN
                        spag_nextblock_1 = 9
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  CALL read(*160,*180,in,p3x,2,1,nf)
                  IF ( p3x(1)/=p3(1) .OR. p3x(2)/=p3(2) ) THEN
                     spag_nextblock_1 = 8
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL skpfil(in,1)
!
!
!     BEGIN SEARCH OF TAPE.
!
                  kf = 0
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  CALL page2(-2)
                  WRITE (nout,99005) uwm , iptx
99005             FORMAT (A25,' 4137,  ALL OUTPUT DATA BLOCKS FOR INPUTT',A1,' ARE PURGED.')
                  RETURN
               ENDIF
            ELSEIF ( p1==mtre ) THEN
!
!     OBTAIN LIST OF DATA BLOCKS ON USER TAPE.
!
               CALL open(*60,in,x(inbuf),0)
               CALL read(*160,*180,in,dx,3,0,nf)
               CALL read(*160,*180,in,idhdrx,7,0,nf)
               DO kf = 1 , 7
                  IF ( idhdrx(kf)/=idhdr(kf) ) THEN
                     spag_nextblock_1 = 9
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
               CALL read(*160,*180,in,p3x,2,1,nf)
               IF ( p3x(1)/=p3(1) .OR. p3x(2)/=p3(2) ) THEN
!
                  WRITE (nout,99006) uwm , p3x , p3
99006             FORMAT (A25,' 4135, USER TAPE ID CODE -',2A4,'- DOES NOT MATCH ','THIRD INPUTT1 DMAP PARAMETER -',2A4,2H-.)
                  line = line + 2
               ENDIF
               CALL skpfil(in,1)
               kf = 0
               SPAG_Loop_1_1: DO
                  CALL page1
                  line = line + 5
                  WRITE (nout,99007) in
99007             FORMAT (1H0,50X,A4,14H FILE CONTENTS,/46X,4HFILE,18X,4HNAME/1H0)
                  DO
                     CALL read(*20,*200,in,namex,2,1,nf)
                     CALL skpfil(in,1)
                     kf = kf + 1
                     line = line + 1
                     WRITE (nout,99008) kf , namex
99008                FORMAT (45X,I5,18X,2A4)
                     IF ( line>=nlpp ) CYCLE SPAG_Loop_1_1
                  ENDDO
                  GOTO 20
               ENDDO SPAG_Loop_1_1
            ELSEIF ( p1<=zero ) THEN
!
               IF ( p1==mtwo .OR. p1==mfor ) THEN
!
!     P1 = -2 OR P1 = -4 IS ACCEPTABLE ONLY ON IBM OR UNIVAC
!
                  IF ( mach/=2 .AND. mach/=3 ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
!
                  iold = -p1/2
                  CALL open(*60,in,x(inbuf),2)
                  CALL tpswit(in,iold,1,tapcod)
               ENDIF
!
               IF ( p1==mone .OR. p1==mtwo .OR. p1==mfor ) THEN
!
!     OPEN USER TAPE TO READ WITH REWIND AHD TAPE ID CHECK
!
                  IF ( p1==mone .OR. p1==mtwo .OR. p1==mfor .OR. iptx/=ipt4 ) THEN
                     CALL open(*60,in,x(inbuf),0)
                     CALL read(*160,*180,in,dx,3,0,nf)
                     CALL read(*160,*180,in,idhdrx,7,0,nf)
                     DO kf = 1 , 7
                        IF ( idhdrx(kf)/=idhdr(kf) ) THEN
                           spag_nextblock_1 = 9
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDDO
                     CALL read(*160,*180,in,p3x,2,1,nf)
                     IF ( p3x(1)/=p3(1) .OR. p3x(2)/=p3(2) ) THEN
                        spag_nextblock_1 = 8
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     CALL skpfil(in,1)
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
!
!     OPEN USER TAPE TO READ WITHOUT REWIND AND NO TAPE ID CHECK
!
               CALL open(*60,in,x(inbuf),2)
               IF ( iptx==ipt4 ) CALL fwdrec(*180,ix)
            ELSE
!
               CALL open(*60,in,x(inbuf),2)
               DO i = 1 , p1
                  CALL read(*140,*140,in,namex,2,0,nf)
                  CALL skpfil(in,1)
               ENDDO
            ENDIF
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
         DO i = 1 , 5
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  output = out(i)
                  trl(1) = output
                  CALL rdtrl(trl)
                  IF ( trl(1)<=0 ) CYCLE
                  CALL fname(output,name)
                  IF ( name(1)==none(1) .AND. name(2)==none(2) ) CYCLE
!
!     PASS FILE NAME HEADER RECORD
!
                  CALL read(*100,*120,in,namex,2,0,nf)
!
!     READ TRAILER RECORD, SIX WORDS (OR 3 WORDS, IPTX=4 ONLY)
!
                  CALL read(*100,*2,in,trl(2),6,1,nf)
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
!
!     JUST A NOTE, FROM G.CHAN/UNISYS -
!     LEVEL 17.5 USED 2 RECORDS HERE FOR THE MATRIX NAME (2 BCD WORDS,
!     1ST RECORD) AND 7 TRAILER WORDS (2ND RECORD)
!
 2                IF ( iptx/=ipt4 .OR. nf<3 ) GOTO 120
                  trl(5) = trl(2)
                  trl(6) = trl(3)
                  trl(7) = trl(4)
                  DO j = 2 , 7
                     j1 = j/2 + 4
                     j2 = mod(j-1,2)*16
                     trl(j) = andf(rshift(trl(j1),j2),mask)
                  ENDDO
                  spag_nextblock_2 = 2
               CASE (2)
!
!     OPEN OUTPUT DATA BLOCK TO WRITE WITH REWIND
!
                  CALL open(*80,output,x(oubuf),1)
!
!     COPY CONTENTS OF USER TAPE ONTO OUTPUT DATA BLOCK, INCLUDING
!     FILE NAME IN RECORD 0
!
                  CALL cpyfil(in,output,x,lcor,nf)
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
                  WRITE (nout,99009) uim , name , in , namex
99009             FORMAT (A29,' 4105,     DATA BLOCK ',2A4,' RETRIEVED FROM USER ','TAPE',A4,/5X,                                   &
                         &'NAME OF DATA BLOCK WHEN PLACED ON USER ','TAPE WAS ',2A4)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO
!
!     CLOSE NASTRAN USER TAPE WITHOUT REWIND
!
         CALL close(in,2)
         RETURN
 20      CALL rewind(in)
         CALL skpfil(in,1)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         CALL read(*40,*200,in,namex,2,0,nf)
         kf = kf + 1
!
         SPAG_Loop_1_2: DO i = 1 , 5
            spag_nextblock_3 = 1
            SPAG_DispatchLoop_3: DO
               SELECT CASE (spag_nextblock_3)
               CASE (1)
                  name(1) = nt(i,2)
                  name(2) = nt(i,3)
                  IF ( nt(i,1)<0 ) CYCLE
                  IF ( name(1)/=namex(1) .OR. name(2)/=namex(2) ) CYCLE
                  nt(i,1) = nt(i,1) + 1
                  IF ( nt(i,1)==1 .OR. p1==msix .OR. p1==mete ) THEN
                     CALL read(*100,*22,in,trl(2),6,1,nf)
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
                  ELSE
                     CALL page2(-3)
                     WRITE (nout,99010) uwm , name , kf , in
99010                FORMAT (A25,' 4138,  DATA BLOCK ',2A4,' (DATA BLOCK COUNT =',I5,') HAS PREVIOUSLY BEEN RETRIEVED FROM',/36X,   &
                            &'USER TAPE ',A4,' AND WILL BE IGNORED.')
                     EXIT SPAG_Loop_1_2
                  ENDIF
 22               IF ( iptx/=ipt4 .OR. nf<3 ) GOTO 120
                  trl(5) = trl(2)
                  trl(6) = trl(3)
                  trl(7) = trl(4)
                  DO j = 2 , 7
                     j1 = j/2 + 4
                     j2 = mod(j-1,2)*16
                     trl(j) = andf(rshift(trl(j1),j2),mask)
                  ENDDO
                  spag_nextblock_3 = 2
               CASE (2)
                  output = out(i)
                  CALL open(*80,output,x(oubuf),1)
                  CALL cpyfil(in,output,x,lcor,nf)
                  CALL close(output,1)
                  trl(1) = output
                  CALL wrttrl(trl)
                  CALL page2(-2)
                  WRITE (nout,99011) uim , name , in , kf
99011             FORMAT (A29,' 4139, DATA BLOCK ',2A4,' RETRIEVED FROM USER TAPE ',A4,' (DATA BLOCK COUNT =',I5,1H))
                  IF ( nt(i,1)>1 ) THEN
                     WRITE (nout,99012) uwm
99012                FORMAT (A25,' 4140, SECONDARY VERSION OF DATA BLOCK HAS REPLACED',' EARLIER ONE.')
                     CALL page2(-2)
                  ELSE
                     nnt = nnt - 1
                  ENDIF
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               END SELECT
            ENDDO SPAG_DispatchLoop_3
         ENDDO SPAG_Loop_1_2
!
         CALL skpfil(in,1)
         spag_nextblock_1 = 5
      CASE (5)
         IF ( nnt<=0 .AND. p1/=msix .AND. p1/=mete ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
 40      IF ( nnt>0 ) THEN
            CALL page2(-7)
            IF ( p1==mfiv .OR. p1==msix ) THEN
!
               WRITE (nout,99013) ufm
99013          FORMAT (A23,' 4142, ONE OR MORE DATA BLOCKS NOT FOUND ON USER ','TAPE',/)
               DO i = 1 , 5
                  IF ( nt(i,1)==0 ) THEN
                     WRITE (nout,99014) nt(i,2) , nt(i,3)
99014                FORMAT (20X,'NAME OF DATA BLOCK = ',2A4)
                     line = line + 1
                  ENDIF
               ENDDO
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSE
               WRITE (nout,99015) uwm
99015          FORMAT (A25,' 4141, ONE OR MORE DATA BLOCKS NOT FOUND ON USER ','TAPE.')
               DO i = 1 , 5
                  IF ( nt(i,1)==0 ) THEN
                     WRITE (nout,99016) nt(i,2) , nt(i,3)
99016                FORMAT (20X,21HNAME OF DATA BLOCK = ,2A4)
                  ENDIF
               ENDDO
               IF ( p1==mfiv .OR. p1==msix ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
         CALL skpfil(in,-1)
         CALL close(in,2)
         RETURN
!
!     ERRORS
!
 60      WRITE (nout,99017) sfm , iptx , in
99017    FORMAT (A25,' 4107, MODULE INPTT',A1,' UNABLE TO OPEN NASTRAN ','FILE ',A4,1H.)
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
!
 80      WRITE (nout,99018) sfm , iptx , output
99018    FORMAT (A25,' 4108, SUBROUTINE INPTT',A1,' UNABLE TO OPEN OUTPUT',' DATA BLOCK',I5)
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
!
 100     CALL mesage(-2,ifile,subnam)
!
 120     CALL mesage(-3,ifile,subnam)
!
 140     WRITE (nout,99019) ufm , iptx , p1 , in , i
99019    FORMAT (A22,' 4111, MODULE INPUTT',A1,' IS UNABLE TO SKIP FORWARD',I10,' DATA BLOCKS ON PERMANENT NASTRAN FILE ',A4,1H.,   &
               & /5X,'NUMBER OF DATA BLOCKS SKIPPED =',I5)
         line = line + 1
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
!
         WRITE (nout,99020) ufm , iptx , p1
99020    FORMAT (A23,' 4113, MODULE INPUTT',A1,' - ILLEGAL VALUE FOR ','FIRST PARAMETER =',I20)
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
!
         WRITE (nout,99021) ufm , p3x , iptx , p3
99021    FORMAT (A23,' 4136, USER TAPE ID CODE -',2A4,'- DOES NOT MATCH ','THIRD INPUTT',A1,' DMAP PARAMETER -',2A4,2H-.)
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
!
 160     WRITE (nout,99022) ufm , iptx , in
99022    FORMAT (A23,' 4132, MODULE INPUTT',A1,' - END-OF-FILE ENCOUNTERED',' WHILE ATTEMPTING TO READ TAPE ID CODE ON USER TAPE ', &
               & A4,1H.)
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
!
 180     WRITE (nout,99023) ufm , iptx , in
99023    FORMAT (A23,' 4133, MODULE INPUTT',A1,' - END-OF-RECORD ','ENCOUNTERED WHILE ATTEMPTING TO READ TAPE ID CODE ON ',         &
                &'USER TAPE ',A4,1H.)
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
!
         WRITE (nout,99024) ufm , iptx , idhdrx
99024    FORMAT (A23,' 4134, MODULE INPUTT',A1,' - ILLEGAL TAPE CODE HEADER = ',7A4)
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
!
 200     WRITE (nout,99025) sfm , iptx
99025    FORMAT (A25,' 4106, MODULE INPUTT',A1,' - SHORT RECORD.')
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
!
         WRITE (nout,99026) ufm , p3
99026    FORMAT (A23,', ILLEGAL TAPE LABEL NAME -',2A4,'-  POSSIBLY ','THE 4TH PARAMETER OF INPTT4 IS IN ERROR')
         spag_nextblock_1 = 11
      CASE (11)
!
!
         line = line + 2
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE inptt1
