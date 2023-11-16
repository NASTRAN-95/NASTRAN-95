
SUBROUTINE paraml
!
!     TO SELECT PARAMETERS FROM A GINO DATA BLOCK
!
!     PARAML  DB/ /C,N,OP/V,N,P1/V,N,P2/V,N,RSP/V,N,INTEG/V,N,RDP/
!                  V,N,BCD/V,N,SPLX/V,N,DPLX $
!
!     INPUT GINO FILE -
!       DB = TABLE  INPUT FILE IF OP='TABLEi'
!       DB = MATRIX INPUT FILE IF OP='MATRIX','NULL', etc.
!     OUTPUT GINO FILE -
!       NONE
!     INPUT PARAMETER -
!       OP    = OPERATION FLAG, ONE OF THE FOLLOWING KEY WORDS,
!               'MATRIX', 'NULL', 'PRESENCE', 'TRAILER', OR
!               'TABLE1' - ABSTRACT FROM 1 INPUT WORD TO FORM ALL OUTPUT
!                          DATA TYPE (INTEGER, S.P /D.P. REAL S.P./D.P.
!                          COMPLEX) AND 4-BYTE BCD WORD (1 WORD)
!               'TABLE2' - ABSTRACT FROM 2 INPUT WORDS TO FORM ALL
!                          OUTPUT DATA TYPE, AND 8-BYTE BCD (2 WORDS)
!               'TABLE4' - ABSTRACT FORM 4 INPUT WORDS TO FORM S.P./D.P.
!                          COMPLEX NUMBER
!               'TABLE1/2/4' OPERATES ONLY IN TABLE  DATA BLOCK, AND
!                THE OTHERS  OPERATE  ONLY IN MATRIX DATA BLOCK.
!
!                IF 'PRESENCE' IS ABBREVIATED AS 'PRES  ', THE USER
!                PARAML INFORMATION MESSAGE IS NOT ECHOED OUT.
!
!     INPUT/OUTPUT PARAMETERS -
!       P1    = RECORD NO. IF DB IS A TABLE, OR
!       P1    = ROW NO. IF DB IS A MATRIX
!               (DEFAULT=1)
!       P2    = WORD POSITION INDEX (BASED ON S.P.REAL WORD COUNT)
!               IF DB IS A TABLE, OR
!       P2    = COLUMN NUMBER, IF DB IS A MATRIX DATA BLOCK, S.P. OR
!               D.P.
!               (DEFAULT=1)
!       (ROW FIRST AND COLUMN SECOND - IN CONSISTANT WITH SCALAR MODULE)
!     OUTPUT PARAMETERS -
!       RSP   = SINGLE PRECISION REAL
!               (DATA ABSTRACTED FROM 1 OR 2 INPUT WORDS)
!       INTEG = INTEGER (DATA ABSTRACTED FROM 1 INPUT WORD)
!       RDP   = DOUBLE PREC. FLOATING NUMBERS (FROM 1 OR 2 INPUT WORDS)
!       BCD   = 8-BYTE BCD WORD, BLANK FILLED IF NECCESSARY
!       SPLX  = SINGLE PRECISION COMPLEX (FROM 1 TO 4 INPUT WORDS)
!       DPLX  = DOUBLE PRECISION COMPLEX (FROM 1 TO 4 INPUT WORDS)
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bcd(2) , Ii , Il(2) , Il3 , Il4 , Il5 , Il6 , Il7 , Il8 , Il9 , Incr , Integ , Ityp , Ivps(1) , Iz(1) , Jj , Nout ,      &
         & Op(2) , P1 , P2 , Sysbuf
   DOUBLE PRECISION Dplx(2) , Dz(1) , Rdp
   REAL Rsp , Splx(2) , Vps(2) , Z(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Op , P1 , P2 , Rsp , Integ , Rdp , Bcd , Splx , Dplx
   COMMON /ilocal/ Il , Il3 , Il4 , Il5 , Il6 , Il7 , Il8 , Il9
   COMMON /system/ Sysbuf , Nout
   COMMON /unpakx/ Ityp , Ii , Jj , Incr
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /xvps  / Vps
   COMMON /zzzzzz/ Iz
!
! Local variable declarations
!
   INTEGER at(2) , atx , blank , col , ei(3) , first , flag , fnm(2) , i , ibuf1 , in1 , index , ixp1 , j , k , mcb(7) , name(2) ,  &
         & nmvps(2) , nz , opcd(7) , prec , recno , rl , row
   DOUBLE PRECISION dp(2)
   INTEGER korsz , numtyp
   LOGICAL mat , prt , tb1 , tb2 , tb4
   CHARACTER*7 nty(4)
   REAL sp(4) , x , y
   CHARACTER*10 type(4)
!
! End of declarations
!
   EQUIVALENCE (Vps(1),Ivps(1)) , (Z(1),Iz(1),Dz(1))
   EQUIVALENCE (sp(1),dp(1))
   DATA name/4HPARA , 4HML  / , blank/4H    / , at/4HAND  , 4HTHRU/
   DATA opcd/4HTABL , 4HMATR , 4HPRES , 4HNULL , 4HTRAI , 4HDTI  , 4HDMI /
   DATA first/12/ , in1/101/ , ei/2HE1 , 2HE2 , 2HE4/
   DATA nty/'ZERO' , 'INTEGER' , 'REAL' , 'BCD'/
   DATA type/'S.P. REAL ' , 'D.P. REAL ' , 'S.P. CMPLX' , 'D.P.CMPLX'/
!
!     SUPPRESS ALL PARAML CHECKING MESSAGES IF DIAG 37 IS ON
!
   CALL sswtch(37,i)
   prt = i==0
   nz = korsz(Iz)
   ibuf1 = nz - Sysbuf + 1
   IF ( ibuf1<=0 ) THEN
      j = -8
      CALL mesage(j,in1,name)
      GOTO 99999
   ELSE
      flag = 1
      mcb(1) = in1
      CALL rdtrl(mcb)
      IF ( mcb(1)>0 ) THEN
!
         prec = mcb(5)
         CALL fname(in1,fnm)
         DO j = 3 , 9
            CALL fndpar(-j,Il(j))
         ENDDO
         IF ( Op(1)/=opcd(3) .OR. Op(2)/=blank ) THEN
            IF ( Op(1)==opcd(4) ) GOTO 500
            IF ( prt ) THEN
               CALL page2(first)
               first = 5
               WRITE (Nout,99014) Uim , Op
            ENDIF
!
!     IDENTIFY OPCODE
!
            DO i = 1 , 7
               IF ( Op(1)==opcd(i) ) THEN
                  IF ( i==1 ) GOTO 700
                  IF ( i==2 ) GOTO 2000
                  IF ( i==3 ) GOTO 100
                  IF ( i==4 ) GOTO 500
                  IF ( i==5 ) GOTO 600
                  IF ( i==6 .OR. i==7 ) GOTO 400
               ENDIF
            ENDDO
            GOTO 200
         ENDIF
      ELSE
!
!     INPUT PURGED.  RETURN IF OP(1) IS NOT 'PRES'
!
         IF ( Op(1)/=opcd(3) ) GOTO 99999
         flag = -1
         CALL fndpar(-5,Il5)
         IF ( prt .AND. Op(2)/=blank ) WRITE (Nout,99014) Uim , Op
      ENDIF
   ENDIF
 100  Integ = flag
   Ivps(Il5) = flag
   nmvps(1) = Ivps(Il5-3)
   nmvps(2) = Ivps(Il5-2)
   IF ( prt .AND. Op(2)/=blank ) WRITE (Nout,99021) Integ , nmvps
   GOTO 99999
 200  WRITE (Nout,99001) Ufm , Op
99001 FORMAT (A23,', ILLEGAL OP REQUEST TO MODULE PARAML - ',2A4)
 300  CALL mesage(-37,0,name)
!
 400  IF ( prt ) THEN
      WRITE (Nout,99002) Uim
99002 FORMAT (A29,', NEW PARAMETERS USED IN PARAML MODULE:',//5X,'PARAML  DB//C,N,OP/C,N,P1/V,N,P2/V,N,RSP/V,N,INT/V,N,RDP/',       &
             &'V,N,BCD/V,N,CSX/V,N,CDX  $',/13X,'OP      = OPCODE, ONE OF THE FOLLOWING KEY WORDS, BCD INPUT, N','O DEFAULT',/23X,  &
             &43H'MATRIX', 'NULL', 'PRESENCE', 'TRAILER', OR,/23X,28H'TABLE1', 'TABLE2', 'TABLE4',/13X,                             &
             &'P1,P2   = RECORD NO. AND WORD POSITION IF OP= TABLEi',/21X,'= ROW AND COLUMN INDEXES IF OP= MATRIX,  INTEGERS INPUT',&
            & /21X,'= P2 GIVES THE VALUE OF P1 TRAILER WORD IF OP= TRAILER',/13X,                                                   &
             &'RSP,RDP = SINGLE PRECISION AND DOUBLE PREC. REAL, OUTPUT',/23X,                                                      &
             &'(DEFAULTS ARE 0.0 AND 0.D+0,  PREVIOUS DEFAULTS WARE ONES',/13X,'INT,BCD = INTEGER AND 2-BCD WORDS OUTPUT',/23X,     &
             &'INT =-1,',' IF NULL MATRIX AND OP= NULL, OR PURGED DB AND OP= PRESENCE',/13X,                                        &
             &'CSX,CDX = SINGLE PRECISION AND DOUBLE PRECISION COMPLEX, ','OUTPUT',//5X,'EXAMPLE - ',                               &
             &'ABSTRACT THE 3RD COL. 9TH ROW ELEMENT OF KGG MATRIX, AND',/15X,                                                      &
             &'ABSTRACT THE 3RD RECORD AND 9TH WORD  OF EPT DATA BLCOK',//5X,                                                       &
             &'PARAML  KGG//*MATRIX*/C,N,9/C,N,3/V,N,R93//V,N,D93//V,N,CS93',/5X,                                                   &
             &'PARAML  EPT//*TABLE1*/C,N,3/C,N,9//V,N,I39/V,N,D39',/)
      IF ( i==6 ) WRITE (Nout,99003)
99003 FORMAT (5X,'SUGGESTION- REPLACE THE OPCODE ''DTI'' BY ''TABLE1''')
      IF ( i==7 ) WRITE (Nout,99004)
!
!     OP = PRESENCE
!     TEST FOR PRESENCE OF DATA BLOCK
!
99004 FORMAT (5X,'SUGGESTION- REPLACE THE OPCODE ''DMI'' BY ''MATRIX''',/18X,'AND NOTE THAT P1 IS ROW NUMBER AND P2 IS COLUMN NO.')
   ENDIF
   GOTO 200
!
!     OP = NULL
!     TEST FOR NULL MATRIX DATA BLOCK
!
 500  IF ( mcb(7)==0 ) flag = -1
   GOTO 100
!
!     OP = TRAILER
!     PLACE THE (P1+1) WORD OF THE TRAILER IN P2
!
 600  IF ( P1<=0 .OR. P1>=7 ) THEN
      WRITE (Nout,99005) Ufm , P1
99005 FORMAT (A23,', 2ND PARAMETER IN PARAML MODULE IS ILLEGAL',I5)
      GOTO 300
   ELSE
      P2 = mcb(P1+1)
      Ivps(Il3) = P2
      nmvps(1) = Ivps(Il3-3)
      nmvps(2) = Ivps(Il3-2)
      IF ( prt ) WRITE (Nout,99021) P2 , nmvps
      GOTO 99999
   ENDIF
!
!     OP = TABLE
!     PROCESS TABLE TYPE DATA BLOCK
!
 700  tb1 = .FALSE.
   tb2 = .FALSE.
   tb4 = .FALSE.
   IF ( Op(2)==ei(1) ) tb1 = .TRUE.
   IF ( Op(2)==ei(2) ) tb2 = .TRUE.
   IF ( Op(2)==ei(3) ) tb4 = .TRUE.
   IF ( .NOT.tb1 .AND. .NOT.tb2 .AND. .NOT.tb4 ) GOTO 200
   mat = .FALSE.
   recno = P1
   index = P2
   IF ( tb2 ) ixp1 = index + 1
   IF ( tb4 ) ixp1 = index + 3
   atx = at(1)
   IF ( tb4 ) atx = at(2)
   CALL open(*2400,in1,Iz(ibuf1),0)
   CALL skprec(in1,recno)
   CALL read(*2500,*800,in1,Iz,ibuf1-1,1,rl)
   j = -8
   CALL mesage(j,in1,name)
   GOTO 99999
 800  IF ( index>rl ) GOTO 2500
   IF ( Il4<=0 ) GOTO 1100
!
!     OUTPUT REQUEST IN S.P. REAL
!
   IF ( prt ) THEN
      IF ( .NOT.tb1 ) THEN
         WRITE (Nout,99015) fnm , recno , index , atx , ixp1
      ELSE
         WRITE (Nout,99016) fnm , recno , index
      ENDIF
   ENDIF
 900  nmvps(1) = Ivps(Il4-3)
   nmvps(2) = Ivps(Il4-2)
   IF ( .NOT.(tb4) ) THEN
      IF ( tb2 ) THEN
         k = -1
         IF ( index+1>rl ) GOTO 1000
         sp(1) = Z(index)
         sp(2) = Z(index+1)
!WKBI
         IF ( sp(2)==0.0 ) dp(1) = sp(1)
!WKBR RSP = SNGL(DP(1))
         Rsp = sp(1)
         k = numtyp(Rsp) + 1
         IF ( k==2 .OR. k==4 ) GOTO 1000
      ELSE
         Rsp = Z(index)
         IF ( .NOT.(mat) ) THEN
            k = numtyp(Rsp) + 1
            IF ( k==2 .OR. k==4 ) GOTO 1000
         ENDIF
      ENDIF
      IF ( prt ) WRITE (Nout,99017) Rsp , nmvps
      Vps(Il4) = Rsp
      GOTO 1100
   ENDIF
!
 1000 IF ( prt ) THEN
      WRITE (Nout,99018) nmvps
      IF ( k>0 ) WRITE (Nout,99019) Uwm , nty(k) , nmvps
      IF ( k==-1 ) WRITE (Nout,99020) Uwm , nmvps
   ENDIF
!
 1100 IF ( .NOT.(Il5<=0 .OR. mat) ) THEN
!
!     OUTPUT REQUEST IS INTEGER
!
      IF ( prt ) THEN
         IF ( tb1 ) WRITE (Nout,99016) fnm , recno , index
         IF ( .NOT.tb1 ) WRITE (Nout,99015) fnm , recno , index , atx , ixp1
      ENDIF
      nmvps(1) = Ivps(Il5-3)
      nmvps(2) = Ivps(Il5-2)
      k = 0
      IF ( .NOT.(tb2 .OR. tb4) ) THEN
         Integ = Iz(index)
         k = numtyp(Integ) + 1
         IF ( k<=2 ) THEN
            Ivps(Il5) = Integ
            IF ( prt ) WRITE (Nout,99021) Integ , nmvps
            GOTO 1200
         ENDIF
      ENDIF
!
      IF ( prt ) THEN
         WRITE (Nout,99018) nmvps
         IF ( k>0 ) WRITE (Nout,99019) Uwm , nty(k) , nmvps
         IF ( k==0 ) WRITE (Nout,99006) Uwm , nmvps
99006    FORMAT (A25,' - ILLEGAL INTEGER ABSTRACTION FROM 2 OR 4 DATA ','WORDS.  OUPUT PARAMETER ',2A4,' NOT SAVED')
      ENDIF
   ENDIF
!
 1200 IF ( Il6>0 ) THEN
!
!     OUTPUT REQUEST IN D.P. REAL
!
      IF ( prt ) THEN
         IF ( tb1 ) WRITE (Nout,99016) fnm , recno , index
         IF ( .NOT.tb1 ) WRITE (Nout,99015) fnm , recno , index , atx , ixp1
      ENDIF
      nmvps(1) = Ivps(Il6-3)
      nmvps(2) = Ivps(Il6-2)
      IF ( mat ) THEN
         IF ( prec==1 ) dp(1) = dble(Z(index))
      ELSEIF ( tb2 ) THEN
         k = -1
         j = 0
         IF ( index+1>rl ) GOTO 1300
         sp(1) = Z(index)
         sp(2) = Z(index+1)
!WKBD 9/93      X = SNGL(DP(1))
         x = sp(1)
         j = numtyp(x) + 1
         IF ( j==2 .OR. j==4 ) GOTO 1300
      ELSE
         IF ( tb4 ) GOTO 1300
         k = numtyp(Z(index)) + 1
         IF ( k==2 .OR. k==4 ) GOTO 1300
         dp(1) = dble(Z(index))
      ENDIF
!WKBI
      IF ( sp(2)==0.0 ) dp(1) = sp(1)
!WKBR  570 RDP = DP(1)
      Rdp = dp(1)
      Vps(Il6) = sp(1)
      Vps(Il6+1) = sp(2)
      IF ( prt ) WRITE (Nout,99022) Rdp , nmvps
   ENDIF
   GOTO 1400
!
 1300 IF ( prt ) THEN
      WRITE (Nout,99018) nmvps
      IF ( j==2 .OR. j==4 ) k = j
      IF ( k>0 ) WRITE (Nout,99019) Uwm , nty(k) , nmvps
      IF ( k==-1 ) WRITE (Nout,99020) Uwm , nmvps
   ENDIF
!
 1400 IF ( .NOT.(Il7<=0 .OR. mat) ) THEN
!
!     OUTPUT REQUEST IN BCD
!
      IF ( prt ) THEN
         IF ( tb1 ) WRITE (Nout,99016) fnm , recno , index
         IF ( .NOT.tb1 ) WRITE (Nout,99015) fnm , recno , index , atx , ixp1
      ENDIF
      nmvps(1) = Ivps(Il7-3)
      nmvps(2) = Ivps(Il7-2)
      k = 0
      IF ( .NOT.(tb4) ) THEN
         Bcd(1) = Iz(index)
         Bcd(2) = blank
         k = numtyp(Bcd(1)) + 1
         IF ( k==4 ) THEN
            IF ( .NOT.(tb1) ) THEN
               k = -1
               IF ( index+1>rl ) GOTO 1450
               Bcd(2) = Iz(index+1)
               k = numtyp(Bcd(2)) + 1
               IF ( k/=4 ) GOTO 1450
            ENDIF
            Ivps(Il7) = Bcd(1)
            Ivps(Il7+1) = Bcd(2)
            IF ( prt ) WRITE (Nout,99007) Bcd , nmvps
99007       FORMAT (1H+,70X,2A4,'   = ',2A4)
            GOTO 1500
         ENDIF
      ENDIF
!
 1450 IF ( prt ) THEN
         WRITE (Nout,99018) nmvps
         IF ( k>0 ) WRITE (Nout,99019) Uwm , nty(k) , nmvps
         IF ( k==0 ) WRITE (Nout,99008) Uwm , nmvps
99008    FORMAT (A25,' - ILLEGAL BCD ABSTRACTION FROM 4 DATA WORDS. ',' PARAMETER ',2A4,'NOT SAVED')
         IF ( k==-1 ) WRITE (Nout,99020) Uwm , nmvps
      ENDIF
   ENDIF
!
 1500 IF ( Il8<=0 ) GOTO 1700
!
!     OUTPUT REQUEST IN S.P. COMPLEX
!
   IF ( prt ) THEN
      IF ( tb1 ) WRITE (Nout,99016) fnm , recno , index
      IF ( .NOT.tb1 ) WRITE (Nout,99015) fnm , recno , index , atx , ixp1
   ENDIF
   nmvps(1) = Ivps(Il8-3)
   nmvps(2) = Ivps(Il8-2)
   k = -1
   j = 0
   IF ( tb4 ) THEN
      IF ( index+3>rl ) GOTO 1600
      sp(1) = Z(index)
      sp(2) = Z(index+1)
      sp(3) = Z(index+2)
      sp(4) = Z(index+3)
!WKBR SPLX(1) = SNGL(DP(1))
      Splx(1) = sp(1)
!WKBR SPLX(2) = SNGL(DP(2))
      Splx(2) = sp(3)
   ELSE
      Splx(1) = Z(index)
      Splx(2) = 0.0
      IF ( .NOT.(tb1 .OR. mat) ) THEN
         IF ( index+1>rl ) GOTO 1600
         Splx(2) = Z(index+1)
      ENDIF
   ENDIF
   j = numtyp(Splx(1)) + 1
   k = numtyp(Splx(2)) + 1
   IF ( j/=2 .AND. j/=4 .AND. k/=2 .AND. j/=4 ) THEN
      Vps(Il8) = Splx(1)
      Vps(Il8+1) = Splx(2)
      IF ( prt ) WRITE (Nout,99023) Splx , nmvps
      GOTO 1700
   ENDIF
!
 1600 IF ( prt ) THEN
      WRITE (Nout,99018) nmvps
      IF ( j==2 .OR. j==4 ) k = j
      IF ( k==0 ) WRITE (Nout,99019) Uwm , nty(k) , nmvps
      IF ( k==-1 ) WRITE (Nout,99020) Uwm , nmvps
   ENDIF
!
 1700 IF ( Il9<=0 ) THEN
!
      CALL close(in1,1)
      GOTO 99999
   ELSE
!
!     OUTPUT REQUEST IN D.P. COMPLEX
!
      IF ( prt ) THEN
         IF ( tb1 ) WRITE (Nout,99016) fnm , recno , index
         IF ( .NOT.tb1 ) WRITE (Nout,99015) fnm , recno , index , atx , ixp1
      ENDIF
      nmvps(1) = Ivps(Il9-3)
      nmvps(2) = Ivps(Il9-2)
      k = -1
      j = 0
      IF ( tb4 ) THEN
         IF ( index+3>rl ) GOTO 1900
         sp(1) = Z(index)
         sp(2) = Z(index+1)
         sp(3) = Z(index+2)
         sp(4) = Z(index+3)
!WKBR X = SNGL(DP(1))
         x = sp(1)
!WKBR Y = SNGL(DP(2))
         y = sp(3)
         j = numtyp(x) + 1
         k = numtyp(y) + 1
         IF ( j==2 .OR. j==4 .OR. k==2 .OR. k==4 ) GOTO 1900
         dp(1) = dble(Z(index))
         dp(2) = 0.D0
      ELSE
         k = numtyp(Z(index)) + 1
         IF ( k==2 .OR. k==4 ) GOTO 1900
         dp(1) = dble(Z(index))
         dp(2) = 0.D0
         IF ( .NOT.(tb1 .OR. mat) ) THEN
            IF ( index+1>rl ) GOTO 1900
            k = numtyp(Z(index+1)) + 1
            IF ( k==2 .OR. k==4 ) GOTO 1900
            dp(2) = dble(Z(index+1))
         ENDIF
      ENDIF
   ENDIF
 1800 Dplx(1) = dp(1)
   Dplx(2) = dp(2)
   Vps(Il9) = sp(1)
   Vps(Il9+1) = sp(2)
   Vps(Il9+2) = sp(3)
   Vps(Il9+3) = sp(4)
   IF ( prt ) WRITE (Nout,99009) Dplx , nmvps
99009 FORMAT (1H+,70X,1H(,D15.8,1H,,D15.8,1H),'  = ',2A4)
   CALL close(in1,1)
   GOTO 99999
!
 1900 IF ( prt ) THEN
      WRITE (Nout,99018) nmvps
      IF ( j==2 .OR. j==4 ) k = j
      IF ( k>0 ) WRITE (Nout,99019) Uwm , nty(k) , nmvps
      IF ( k==-1 ) WRITE (Nout,99020) Uwm , nmvps
   ENDIF
   CALL close(in1,1)
   GOTO 99999
!
!     OP = MATRIX
!     PROCESS MATRIX TYPE DATA BLOCK
!
 2000 row = P1
   col = P2
   Ityp = mcb(5)
   IF ( Il5>0 ) THEN
      IF ( prt ) WRITE (Nout,99024) row , col , type(Ityp) , fnm
      nmvps(1) = Ivps(Il5-3)
      nmvps(2) = Ivps(Il5-2)
      IF ( prt ) THEN
         WRITE (Nout,99010) nmvps
99010    FORMAT (1H+,70X,'(INVALID INTEGER) = ',2A4)
         WRITE (Nout,99025) Uwm , nmvps
      ENDIF
   ENDIF
   IF ( Il7>0 ) THEN
      IF ( prt ) WRITE (Nout,99024) row , col , type(Ityp) , fnm
      nmvps(1) = Ivps(Il7-3)
      nmvps(2) = Ivps(Il7-2)
      IF ( prt ) THEN
         WRITE (Nout,99011) nmvps
99011    FORMAT (1H+,70X,'(INVALID BCD WORD)= ',2A4)
         WRITE (Nout,99025) Uwm , nmvps
      ENDIF
   ENDIF
!
   IF ( Il4<=0 .AND. Il6<=0 .AND. Il8<=0 .AND. Il9<=0 ) GOTO 99999
!
!     OUTPUT REQUEST - IL4 - S.P. REAL
!                      IL5 - INTEGER
!                      IL6 - D.P. REAL
!                      IL7 - BCD
!                      IL8 - S.P. COMPLEX
!                      IL9 - D.P. COMPLEX
!
   mat = .TRUE.
   tb1 = .FALSE.
   tb2 = .FALSE.
   tb4 = .FALSE.
   recno = P2
   index = P1
   rl = 999999
   Ii = 1
   Jj = mcb(3)
   Incr = 1
   CALL gopen(in1,Iz(ibuf1),0)
   CALL skprec(in1,col-1)
   CALL unpack(*2300,in1,Z)
   IF ( Ityp==2 ) THEN
   ELSEIF ( Ityp==3 .OR. Ityp==4 ) THEN
      GOTO 2200
   ELSE
      GOTO 900
!
!     INPUT MATRIX PRECISION TYPE = 1, S.P. REAL
!
   ENDIF
!
!     MATRIX PRECISION TYPE = 2, D.P. REAL
!
 2100 IF ( Il4>0 ) THEN
      IF ( prt ) WRITE (Nout,99024) row , col , type(Ityp) , fnm
      Rsp = Dz(row)
      Vps(Il4) = Rsp
      nmvps(1) = Ivps(Il4-3)
      nmvps(2) = Ivps(Il4-2)
      IF ( prt ) WRITE (Nout,99017) Rsp , nmvps
   ENDIF
   IF ( Il6>0 ) THEN
      IF ( prt ) WRITE (Nout,99024) row , col , type(Ityp) , fnm
      Rdp = Dz(row)
      dp(1) = Rdp
      Vps(Il6) = sp(1)
      Vps(Il6+1) = sp(2)
      nmvps(1) = Ivps(Il6-3)
      nmvps(2) = Ivps(Il6-2)
      IF ( prt ) WRITE (Nout,99022) Rdp , nmvps
   ENDIF
   IF ( Il8>0 ) THEN
      IF ( prt ) WRITE (Nout,99024) row , col , type(Ityp) , fnm
      Splx(1) = Dz(row)
      Splx(2) = 0.0
      Vps(Il8) = Splx(1)
      Vps(Il8+1) = Splx(2)
      nmvps(1) = Ivps(Il8-3)
      nmvps(2) = Ivps(Il8-2)
      IF ( prt ) WRITE (Nout,99023) Splx , nmvps
   ENDIF
   IF ( Il9<=0 ) THEN
      CALL close(in1,1)
      GOTO 99999
   ELSE
      IF ( prt ) WRITE (Nout,99024) row , col , type(Ityp) , fnm
      dp(1) = Dz(row)
      dp(2) = 0.D0
      nmvps(1) = Ivps(Il9-3)
      nmvps(2) = Ivps(Il9-2)
      GOTO 1800
   ENDIF
!
!     INPUT MATRIX PRECISION TYPE = 3 OR 4, COMPLEX
!
 2200 IF ( Il4>0 ) THEN
      IF ( prt ) WRITE (Nout,99024) row , col , type(Ityp) , fnm
      nmvps(1) = Ivps(Il4-3)
      nmvps(2) = Ivps(Il4-2)
      IF ( prt ) THEN
         WRITE (Nout,99012) nmvps
99012    FORMAT (1H+,70X,' (INVALID S.P. REAL NUMBER)  = ',2A4)
         WRITE (Nout,99025) Uwm , nmvps
      ENDIF
   ENDIF
   IF ( Il6>0 ) THEN
      IF ( prt ) WRITE (Nout,99024) row , col , type(Ityp) , fnm
      nmvps(1) = Ivps(Il6-3)
      nmvps(2) = Ivps(Il6-2)
      IF ( prt ) WRITE (Nout,99013) nmvps
99013 FORMAT (1H+,70X,' (INVALID D.P.REAL NUMBER)  = ',2A4)
   ENDIF
   IF ( Il8<=0 .AND. Il9<=0 ) THEN
      CALL close(in1,1)
      GOTO 99999
   ELSEIF ( Ityp==4 ) THEN
!
!     INPUT MATRIX PRECISION TYPE = 4, D.P.COMPLEX
!
      IF ( Il8>0 ) THEN
         IF ( prt ) WRITE (Nout,99024) row , col , type(Ityp) , fnm
         Splx(1) = sngl(Dz(row))
         Splx(2) = sngl(Dz(row+1))
         Vps(Il8) = Splx(1)
         Vps(Il8+1) = Splx(2)
         nmvps(1) = Ivps(Il8-3)
         nmvps(2) = Ivps(Il8-2)
         IF ( prt ) WRITE (Nout,99023) Splx , nmvps
      ENDIF
      IF ( Il9<=0 ) THEN
         CALL close(in1,1)
         GOTO 99999
      ELSE
         IF ( prt ) WRITE (Nout,99024) row , col , type(Ityp) , fnm
         dp(1) = Dz(row)
         dp(2) = Dz(row+1)
         nmvps(1) = Ivps(Il9-3)
         nmvps(2) = Ivps(Il9-2)
         GOTO 1800
      ENDIF
   ELSE
!
!     INPUT MATRIX PRECISION TYPE = 3, S.P.COMPLEX
!
      IF ( Il8>0 ) THEN
         IF ( prt ) WRITE (Nout,99024) row , col , type(Ityp) , fnm
         Splx(1) = Z(row)
         Splx(2) = Z(row+1)
         Vps(Il8) = Splx(1)
         Vps(Il8+1) = Splx(2)
         nmvps(1) = Ivps(Il8-3)
         nmvps(2) = Ivps(Il8-2)
         IF ( prt ) WRITE (Nout,99023) Splx , nmvps
      ENDIF
      IF ( Il9<=0 ) THEN
         CALL close(in1,1)
         GOTO 99999
      ELSE
         IF ( prt ) WRITE (Nout,99024) row , col , type(Ityp) , fnm
         dp(1) = dble(Z(row))
         dp(2) = dble(Z(row+1))
         nmvps(1) = Ivps(Il9-3)
         nmvps(2) = Ivps(Il9-2)
         GOTO 1800
      ENDIF
   ENDIF
!
!     NULL INPUT MATRIX ELEMENT
!
 2300 Z(row) = 0.
   Z(row+1) = 0.
   Dz(row) = 0.D0
   Dz(row+1) = 0.D0
   IF ( Ityp==1 ) GOTO 900
   IF ( Ityp==2 ) GOTO 2100
   IF ( Ityp==3 .OR. Ityp==4 ) GOTO 2200
   CALL close(in1,1)
   GOTO 99999
!
!     ERRORS
!
 2400 j = -1
   CALL mesage(j,in1,name)
   GOTO 99999
 2500 j = -2
   CALL mesage(j,in1,name)
99014 FORMAT (A29,' FROM PARAML MODULE  - ',2A4,2H -,/5X,'(ALL PARAML MESSAGES CAN BE SUPPRESSED BY DIAG 37)',/)
99015 FORMAT (5X,'INPUT FILE ',2A4,' RECORD',I6,' WORDS',I6,1X,A4,I5,'  =')
99016 FORMAT (5X,'INPUT FILE ',2A4,' RECORD',I6,' WORD',I6,13X,1H=)
99017 FORMAT (1H+,70X,E15.8,'   = ',2A4)
99018 FORMAT (1H+,70X,'(INVALID REQUEST) = ',2A4)
99019 FORMAT (A25,' - ILLEGAL OUTPUT REQUESTED. ORIG. DATA TYPE IS ',A7,',  PARAMETER ',2A4,' NOT SAVED')
99020 FORMAT (A25,' - E-O-R ENCOUNTERED.  PARAMETER ',2A4,' NOT SAVED')
99021 FORMAT (1H+,70X,I15,'   = ',2A4)
99022 FORMAT (1H+,70X,D15.8,'   = ',2A4)
99023 FORMAT (1H+,70X,1H(,E15.8,1H,,E15.8,1H),'  = ',2A4)
99024 FORMAT (5X,'ELEMENT (',I5,'-ROW,',I5,'-COL) OF ',A10,' INPUT ','FILE ',2A4,2H =)
99025 FORMAT (A25,' - OUTPUT PARAMETER ',2A4,' NOT SAVED')
!
99999 RETURN
END SUBROUTINE paraml
