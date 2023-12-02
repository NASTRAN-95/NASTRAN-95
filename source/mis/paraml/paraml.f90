!*==paraml.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
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
   USE c_blank
   USE c_ilocal
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_xvps
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: at , name
   INTEGER :: atx , col , flag , i , ibuf1 , index , ixp1 , j , k , nz , prec , recno , rl , row
   INTEGER , SAVE :: blank , first , in1
   REAL(REAL64) , DIMENSION(2) :: dp
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(3) , SAVE :: ei
   INTEGER , DIMENSION(2) :: fnm , nmvps
   INTEGER , DIMENSION(1) :: ivps
   LOGICAL :: mat , prt , tb1 , tb2 , tb4
   INTEGER , DIMENSION(7) :: mcb
   CHARACTER(7) , DIMENSION(4) , SAVE :: nty
   INTEGER , DIMENSION(7) , SAVE :: opcd
   REAL , DIMENSION(4) :: sp
   CHARACTER(10) , DIMENSION(4) , SAVE :: type
   REAL :: x , y
   REAL , DIMENSION(1) :: z
   EXTERNAL close , fname , fndpar , gopen , korsz , mesage , numtyp , open , page2 , rdtrl , read , skprec , sswtch , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (Vps(1),Ivps(1)) , (Z(1),Iz(1),Dz(1))
   !>>>>EQUIVALENCE (sp(1),dp(1))
   DATA name/4HPARA , 4HML  / , blank/4H    / , at/4HAND  , 4HTHRU/
   DATA opcd/4HTABL , 4HMATR , 4HPRES , 4HNULL , 4HTRAI , 4HDTI  , 4HDMI /
   DATA first/12/ , in1/101/ , ei/2HE1 , 2HE2 , 2HE4/
   DATA nty/'ZERO' , 'INTEGER' , 'REAL' , 'BCD'/
   DATA type/'S.P. REAL ' , 'D.P. REAL ' , 'S.P. CMPLX' , 'D.P.CMPLX'/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     SUPPRESS ALL PARAML CHECKING MESSAGES IF DIAG 37 IS ON
!
         CALL sswtch(37,i)
         prt = i==0
         nz = korsz(iz)
         ibuf1 = nz - sysbuf + 1
         IF ( ibuf1<=0 ) THEN
            j = -8
            CALL mesage(j,in1,name)
            RETURN
         ELSE
            flag = 1
            mcb(1) = in1
            CALL rdtrl(mcb)
            IF ( mcb(1)>0 ) THEN
!
               prec = mcb(5)
               CALL fname(in1,fnm)
               DO j = 3 , 9
                  CALL fndpar(-j,il(j))
               ENDDO
               IF ( op(1)/=opcd(3) .OR. op(2)/=blank ) THEN
                  IF ( op(1)==opcd(4) ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( prt ) THEN
                     CALL page2(first)
                     first = 5
                     WRITE (nout,99014) uim , op
                  ENDIF
!
!     IDENTIFY OPCODE
!
                  DO i = 1 , 7
                     IF ( op(1)==opcd(i) ) THEN
                        IF ( i==1 ) THEN
                           spag_nextblock_1 = 8
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        IF ( i==2 ) THEN
                           spag_nextblock_1 = 19
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        IF ( i==3 ) THEN
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        IF ( i==4 ) THEN
                           spag_nextblock_1 = 6
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        IF ( i==5 ) THEN
                           spag_nextblock_1 = 7
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        IF ( i==6 .OR. i==7 ) THEN
                           spag_nextblock_1 = 5
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDIF
                  ENDDO
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
!
!     INPUT PURGED.  RETURN IF OP(1) IS NOT 'PRES'
!
               IF ( op(1)/=opcd(3) ) RETURN
               flag = -1
               CALL fndpar(-5,il5)
               IF ( prt .AND. op(2)/=blank ) WRITE (nout,99014) uim , op
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         integ = flag
         ivps(il5) = flag
         nmvps(1) = ivps(il5-3)
         nmvps(2) = ivps(il5-2)
         IF ( prt .AND. op(2)/=blank ) WRITE (nout,99021) integ , nmvps
         RETURN
      CASE (3)
         WRITE (nout,99001) ufm , op
99001    FORMAT (A23,', ILLEGAL OP REQUEST TO MODULE PARAML - ',2A4)
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(-37,0,name)
         spag_nextblock_1 = 5
      CASE (5)
!
         IF ( prt ) THEN
            WRITE (nout,99002) uim
99002       FORMAT (A29,', NEW PARAMETERS USED IN PARAML MODULE:',//5X,'PARAML  DB//C,N,OP/C,N,P1/V,N,P2/V,N,RSP/V,N,INT/V,N,RDP/', &
                   &'V,N,BCD/V,N,CSX/V,N,CDX  $',/13X,'OP      = OPCODE, ONE OF THE FOLLOWING KEY WORDS, BCD INPUT, N','O DEFAULT', &
                  & /23X,43H'MATRIX', 'NULL', 'PRESENCE', 'TRAILER', OR,/23X,28H'TABLE1', 'TABLE2', 'TABLE4',/13X,                  &
                   &'P1,P2   = RECORD NO. AND WORD POSITION IF OP= TABLEi',/21X,                                                    &
                   &'= ROW AND COLUMN INDEXES IF OP= MATRIX,  INTEGERS INPUT',/21X,                                                 &
                   &'= P2 GIVES THE VALUE OF P1 TRAILER WORD IF OP= TRAILER',/13X,                                                  &
                   &'RSP,RDP = SINGLE PRECISION AND DOUBLE PREC. REAL, OUTPUT',/23X,                                                &
                   &'(DEFAULTS ARE 0.0 AND 0.D+0,  PREVIOUS DEFAULTS WARE ONES',/13X,'INT,BCD = INTEGER AND 2-BCD WORDS OUTPUT',    &
                  & /23X,'INT =-1,',' IF NULL MATRIX AND OP= NULL, OR PURGED DB AND OP= PRESENCE',/13X,                             &
                   &'CSX,CDX = SINGLE PRECISION AND DOUBLE PRECISION COMPLEX, ','OUTPUT',//5X,'EXAMPLE - ',                         &
                   &'ABSTRACT THE 3RD COL. 9TH ROW ELEMENT OF KGG MATRIX, AND',/15X,                                                &
                   &'ABSTRACT THE 3RD RECORD AND 9TH WORD  OF EPT DATA BLCOK',//5X,                                                 &
                   &'PARAML  KGG//*MATRIX*/C,N,9/C,N,3/V,N,R93//V,N,D93//V,N,CS93',/5X,                                             &
                   &'PARAML  EPT//*TABLE1*/C,N,3/C,N,9//V,N,I39/V,N,D39',/)
            IF ( i==6 ) WRITE (nout,99003)
99003       FORMAT (5X,'SUGGESTION- REPLACE THE OPCODE ''DTI'' BY ''TABLE1''')
            IF ( i==7 ) WRITE (nout,99004)
!
!     OP = PRESENCE
!     TEST FOR PRESENCE OF DATA BLOCK
!
99004       FORMAT (5X,'SUGGESTION- REPLACE THE OPCODE ''DMI'' BY ''MATRIX''',/18X,                                                 &
                   &'AND NOTE THAT P1 IS ROW NUMBER AND P2 IS COLUMN NO.')
         ENDIF
         spag_nextblock_1 = 3
      CASE (6)
!
!     OP = NULL
!     TEST FOR NULL MATRIX DATA BLOCK
!
         IF ( mcb(7)==0 ) flag = -1
         spag_nextblock_1 = 2
      CASE (7)
!
!     OP = TRAILER
!     PLACE THE (P1+1) WORD OF THE TRAILER IN P2
!
         IF ( p1<=0 .OR. p1>=7 ) THEN
            WRITE (nout,99005) ufm , p1
99005       FORMAT (A23,', 2ND PARAMETER IN PARAML MODULE IS ILLEGAL',I5)
            spag_nextblock_1 = 4
         ELSE
            p2 = mcb(p1+1)
            ivps(il3) = p2
            nmvps(1) = ivps(il3-3)
            nmvps(2) = ivps(il3-2)
            IF ( prt ) WRITE (nout,99021) p2 , nmvps
            RETURN
         ENDIF
      CASE (8)
!
!     OP = TABLE
!     PROCESS TABLE TYPE DATA BLOCK
!
         tb1 = .FALSE.
         tb2 = .FALSE.
         tb4 = .FALSE.
         IF ( op(2)==ei(1) ) tb1 = .TRUE.
         IF ( op(2)==ei(2) ) tb2 = .TRUE.
         IF ( op(2)==ei(3) ) tb4 = .TRUE.
         IF ( .NOT.tb1 .AND. .NOT.tb2 .AND. .NOT.tb4 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         mat = .FALSE.
         recno = p1
         index = p2
         IF ( tb2 ) ixp1 = index + 1
         IF ( tb4 ) ixp1 = index + 3
         atx = at(1)
         IF ( tb4 ) atx = at(2)
         CALL open(*80,in1,iz(ibuf1),0)
         CALL skprec(in1,recno)
         CALL read(*100,*20,in1,iz,ibuf1-1,1,rl)
         j = -8
         CALL mesage(j,in1,name)
         RETURN
 20      IF ( index>rl ) GOTO 100
         IF ( il4<=0 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     OUTPUT REQUEST IN S.P. REAL
!
         IF ( prt ) THEN
            IF ( .NOT.tb1 ) THEN
               WRITE (nout,99015) fnm , recno , index , atx , ixp1
            ELSE
               WRITE (nout,99016) fnm , recno , index
            ENDIF
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         nmvps(1) = ivps(il4-3)
         nmvps(2) = ivps(il4-2)
         IF ( .NOT.(tb4) ) THEN
            IF ( tb2 ) THEN
               k = -1
               IF ( index+1>rl ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               sp(1) = z(index)
               sp(2) = z(index+1)
!WKBI
               IF ( sp(2)==0.0 ) dp(1) = sp(1)
!WKBR RSP = SNGL(DP(1))
               rsp = sp(1)
               k = numtyp(rsp) + 1
               IF ( k==2 .OR. k==4 ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               rsp = z(index)
               IF ( .NOT.(mat) ) THEN
                  k = numtyp(rsp) + 1
                  IF ( k==2 .OR. k==4 ) THEN
                     spag_nextblock_1 = 10
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDIF
            IF ( prt ) WRITE (nout,99017) rsp , nmvps
            vps(il4) = rsp
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
!
         IF ( prt ) THEN
            WRITE (nout,99018) nmvps
            IF ( k>0 ) WRITE (nout,99019) uwm , nty(k) , nmvps
            IF ( k==-1 ) WRITE (nout,99020) uwm , nmvps
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
!
         IF ( .NOT.(il5<=0 .OR. mat) ) THEN
!
!     OUTPUT REQUEST IS INTEGER
!
            IF ( prt ) THEN
               IF ( tb1 ) WRITE (nout,99016) fnm , recno , index
               IF ( .NOT.tb1 ) WRITE (nout,99015) fnm , recno , index , atx , ixp1
            ENDIF
            nmvps(1) = ivps(il5-3)
            nmvps(2) = ivps(il5-2)
            k = 0
            IF ( .NOT.(tb2 .OR. tb4) ) THEN
               integ = iz(index)
               k = numtyp(integ) + 1
               IF ( k<=2 ) THEN
                  ivps(il5) = integ
                  IF ( prt ) WRITE (nout,99021) integ , nmvps
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
            IF ( prt ) THEN
               WRITE (nout,99018) nmvps
               IF ( k>0 ) WRITE (nout,99019) uwm , nty(k) , nmvps
               IF ( k==0 ) WRITE (nout,99006) uwm , nmvps
99006          FORMAT (A25,' - ILLEGAL INTEGER ABSTRACTION FROM 2 OR 4 DATA ','WORDS.  OUPUT PARAMETER ',2A4,' NOT SAVED')
            ENDIF
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
!
         IF ( il6>0 ) THEN
!
!     OUTPUT REQUEST IN D.P. REAL
!
            IF ( prt ) THEN
               IF ( tb1 ) WRITE (nout,99016) fnm , recno , index
               IF ( .NOT.tb1 ) WRITE (nout,99015) fnm , recno , index , atx , ixp1
            ENDIF
            nmvps(1) = ivps(il6-3)
            nmvps(2) = ivps(il6-2)
            IF ( mat ) THEN
               IF ( prec==1 ) dp(1) = dble(z(index))
            ELSEIF ( tb2 ) THEN
               k = -1
               j = 0
               IF ( index+1>rl ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               sp(1) = z(index)
               sp(2) = z(index+1)
!WKBD 9/93      X = SNGL(DP(1))
               x = sp(1)
               j = numtyp(x) + 1
               IF ( j==2 .OR. j==4 ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               IF ( tb4 ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               k = numtyp(z(index)) + 1
               IF ( k==2 .OR. k==4 ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               dp(1) = dble(z(index))
            ENDIF
!WKBI
            IF ( sp(2)==0.0 ) dp(1) = sp(1)
!WKBR  570 RDP = DP(1)
            rdp = dp(1)
            vps(il6) = sp(1)
            vps(il6+1) = sp(2)
            IF ( prt ) WRITE (nout,99022) rdp , nmvps
         ENDIF
         spag_nextblock_1 = 14
      CASE (13)
!
         IF ( prt ) THEN
            WRITE (nout,99018) nmvps
            IF ( j==2 .OR. j==4 ) k = j
            IF ( k>0 ) WRITE (nout,99019) uwm , nty(k) , nmvps
            IF ( k==-1 ) WRITE (nout,99020) uwm , nmvps
         ENDIF
         spag_nextblock_1 = 14
      CASE (14)
!
         IF ( .NOT.(il7<=0 .OR. mat) ) THEN
!
!     OUTPUT REQUEST IN BCD
!
            IF ( prt ) THEN
               IF ( tb1 ) WRITE (nout,99016) fnm , recno , index
               IF ( .NOT.tb1 ) WRITE (nout,99015) fnm , recno , index , atx , ixp1
            ENDIF
            nmvps(1) = ivps(il7-3)
            nmvps(2) = ivps(il7-2)
            k = 0
            IF ( .NOT.(tb4) ) THEN
               bcd(1) = iz(index)
               bcd(2) = blank
               k = numtyp(bcd(1)) + 1
               IF ( k==4 ) THEN
                  IF ( .NOT.(tb1) ) THEN
                     k = -1
                     IF ( index+1>rl ) GOTO 30
                     bcd(2) = iz(index+1)
                     k = numtyp(bcd(2)) + 1
                     IF ( k/=4 ) GOTO 30
                  ENDIF
                  ivps(il7) = bcd(1)
                  ivps(il7+1) = bcd(2)
                  IF ( prt ) WRITE (nout,99007) bcd , nmvps
99007             FORMAT (1H+,70X,2A4,'   = ',2A4)
                  spag_nextblock_1 = 15
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
 30         IF ( prt ) THEN
               WRITE (nout,99018) nmvps
               IF ( k>0 ) WRITE (nout,99019) uwm , nty(k) , nmvps
               IF ( k==0 ) WRITE (nout,99008) uwm , nmvps
99008          FORMAT (A25,' - ILLEGAL BCD ABSTRACTION FROM 4 DATA WORDS. ',' PARAMETER ',2A4,'NOT SAVED')
               IF ( k==-1 ) WRITE (nout,99020) uwm , nmvps
            ENDIF
         ENDIF
         spag_nextblock_1 = 15
      CASE (15)
!
         IF ( il8>0 ) THEN
!
!     OUTPUT REQUEST IN S.P. COMPLEX
!
            IF ( prt ) THEN
               IF ( tb1 ) WRITE (nout,99016) fnm , recno , index
               IF ( .NOT.tb1 ) WRITE (nout,99015) fnm , recno , index , atx , ixp1
            ENDIF
            nmvps(1) = ivps(il8-3)
            nmvps(2) = ivps(il8-2)
            k = -1
            j = 0
            IF ( tb4 ) THEN
               IF ( index+3>rl ) GOTO 40
               sp(1) = z(index)
               sp(2) = z(index+1)
               sp(3) = z(index+2)
               sp(4) = z(index+3)
!WKBR SPLX(1) = SNGL(DP(1))
               splx(1) = sp(1)
!WKBR SPLX(2) = SNGL(DP(2))
               splx(2) = sp(3)
            ELSE
               splx(1) = z(index)
               splx(2) = 0.0
               IF ( .NOT.(tb1 .OR. mat) ) THEN
                  IF ( index+1>rl ) GOTO 40
                  splx(2) = z(index+1)
               ENDIF
            ENDIF
            j = numtyp(splx(1)) + 1
            k = numtyp(splx(2)) + 1
            IF ( j/=2 .AND. j/=4 .AND. k/=2 .AND. j/=4 ) THEN
               vps(il8) = splx(1)
               vps(il8+1) = splx(2)
               IF ( prt ) WRITE (nout,99023) splx , nmvps
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
 40         IF ( prt ) THEN
               WRITE (nout,99018) nmvps
               IF ( j==2 .OR. j==4 ) k = j
               IF ( k==0 ) WRITE (nout,99019) uwm , nty(k) , nmvps
               IF ( k==-1 ) WRITE (nout,99020) uwm , nmvps
            ENDIF
         ENDIF
         spag_nextblock_1 = 16
      CASE (16)
!
         IF ( il9<=0 ) THEN
!
            CALL close(in1,1)
            RETURN
         ELSE
!
!     OUTPUT REQUEST IN D.P. COMPLEX
!
            IF ( prt ) THEN
               IF ( tb1 ) WRITE (nout,99016) fnm , recno , index
               IF ( .NOT.tb1 ) WRITE (nout,99015) fnm , recno , index , atx , ixp1
            ENDIF
            nmvps(1) = ivps(il9-3)
            nmvps(2) = ivps(il9-2)
            k = -1
            j = 0
            IF ( tb4 ) THEN
               IF ( index+3>rl ) THEN
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               sp(1) = z(index)
               sp(2) = z(index+1)
               sp(3) = z(index+2)
               sp(4) = z(index+3)
!WKBR X = SNGL(DP(1))
               x = sp(1)
!WKBR Y = SNGL(DP(2))
               y = sp(3)
               j = numtyp(x) + 1
               k = numtyp(y) + 1
               IF ( j==2 .OR. j==4 .OR. k==2 .OR. k==4 ) THEN
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               dp(1) = dble(z(index))
               dp(2) = 0.D0
            ELSE
               k = numtyp(z(index)) + 1
               IF ( k==2 .OR. k==4 ) THEN
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               dp(1) = dble(z(index))
               dp(2) = 0.D0
               IF ( .NOT.(tb1 .OR. mat) ) THEN
                  IF ( index+1>rl ) THEN
                     spag_nextblock_1 = 18
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  k = numtyp(z(index+1)) + 1
                  IF ( k==2 .OR. k==4 ) THEN
                     spag_nextblock_1 = 18
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  dp(2) = dble(z(index+1))
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 17
      CASE (17)
         dplx(1) = dp(1)
         dplx(2) = dp(2)
         vps(il9) = sp(1)
         vps(il9+1) = sp(2)
         vps(il9+2) = sp(3)
         vps(il9+3) = sp(4)
         IF ( prt ) WRITE (nout,99009) dplx , nmvps
99009    FORMAT (1H+,70X,1H(,D15.8,1H,,D15.8,1H),'  = ',2A4)
         CALL close(in1,1)
         RETURN
      CASE (18)
!
         IF ( prt ) THEN
            WRITE (nout,99018) nmvps
            IF ( j==2 .OR. j==4 ) k = j
            IF ( k>0 ) WRITE (nout,99019) uwm , nty(k) , nmvps
            IF ( k==-1 ) WRITE (nout,99020) uwm , nmvps
         ENDIF
         CALL close(in1,1)
         RETURN
      CASE (19)
!
!     OP = MATRIX
!     PROCESS MATRIX TYPE DATA BLOCK
!
         row = p1
         col = p2
         ityp = mcb(5)
         IF ( il5>0 ) THEN
            IF ( prt ) WRITE (nout,99024) row , col , type(ityp) , fnm
            nmvps(1) = ivps(il5-3)
            nmvps(2) = ivps(il5-2)
            IF ( prt ) THEN
               WRITE (nout,99010) nmvps
99010          FORMAT (1H+,70X,'(INVALID INTEGER) = ',2A4)
               WRITE (nout,99025) uwm , nmvps
            ENDIF
         ENDIF
         IF ( il7>0 ) THEN
            IF ( prt ) WRITE (nout,99024) row , col , type(ityp) , fnm
            nmvps(1) = ivps(il7-3)
            nmvps(2) = ivps(il7-2)
            IF ( prt ) THEN
               WRITE (nout,99011) nmvps
99011          FORMAT (1H+,70X,'(INVALID BCD WORD)= ',2A4)
               WRITE (nout,99025) uwm , nmvps
            ENDIF
         ENDIF
!
         IF ( il4<=0 .AND. il6<=0 .AND. il8<=0 .AND. il9<=0 ) RETURN
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
         recno = p2
         index = p1
         rl = 999999
         ii = 1
         jj = mcb(3)
         incr = 1
         CALL gopen(in1,iz(ibuf1),0)
         CALL skprec(in1,col-1)
         CALL unpack(*60,in1,z)
         IF ( ityp==2 ) THEN
         ELSEIF ( ityp==3 .OR. ityp==4 ) THEN
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ELSE
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
!
!     INPUT MATRIX PRECISION TYPE = 1, S.P. REAL
!
         ENDIF
         spag_nextblock_1 = 20
      CASE (20)
!
!     MATRIX PRECISION TYPE = 2, D.P. REAL
!
         IF ( il4>0 ) THEN
            IF ( prt ) WRITE (nout,99024) row , col , type(ityp) , fnm
            rsp = dz(row)
            vps(il4) = rsp
            nmvps(1) = ivps(il4-3)
            nmvps(2) = ivps(il4-2)
            IF ( prt ) WRITE (nout,99017) rsp , nmvps
         ENDIF
         IF ( il6>0 ) THEN
            IF ( prt ) WRITE (nout,99024) row , col , type(ityp) , fnm
            rdp = dz(row)
            dp(1) = rdp
            vps(il6) = sp(1)
            vps(il6+1) = sp(2)
            nmvps(1) = ivps(il6-3)
            nmvps(2) = ivps(il6-2)
            IF ( prt ) WRITE (nout,99022) rdp , nmvps
         ENDIF
         IF ( il8>0 ) THEN
            IF ( prt ) WRITE (nout,99024) row , col , type(ityp) , fnm
            splx(1) = dz(row)
            splx(2) = 0.0
            vps(il8) = splx(1)
            vps(il8+1) = splx(2)
            nmvps(1) = ivps(il8-3)
            nmvps(2) = ivps(il8-2)
            IF ( prt ) WRITE (nout,99023) splx , nmvps
         ENDIF
         IF ( il9<=0 ) THEN
            CALL close(in1,1)
            RETURN
         ELSE
            IF ( prt ) WRITE (nout,99024) row , col , type(ityp) , fnm
            dp(1) = dz(row)
            dp(2) = 0.D0
            nmvps(1) = ivps(il9-3)
            nmvps(2) = ivps(il9-2)
            spag_nextblock_1 = 17
         ENDIF
      CASE (21)
!
!     INPUT MATRIX PRECISION TYPE = 3 OR 4, COMPLEX
!
         IF ( il4>0 ) THEN
            IF ( prt ) WRITE (nout,99024) row , col , type(ityp) , fnm
            nmvps(1) = ivps(il4-3)
            nmvps(2) = ivps(il4-2)
            IF ( prt ) THEN
               WRITE (nout,99012) nmvps
99012          FORMAT (1H+,70X,' (INVALID S.P. REAL NUMBER)  = ',2A4)
               WRITE (nout,99025) uwm , nmvps
            ENDIF
         ENDIF
         IF ( il6>0 ) THEN
            IF ( prt ) WRITE (nout,99024) row , col , type(ityp) , fnm
            nmvps(1) = ivps(il6-3)
            nmvps(2) = ivps(il6-2)
            IF ( prt ) WRITE (nout,99013) nmvps
99013       FORMAT (1H+,70X,' (INVALID D.P.REAL NUMBER)  = ',2A4)
         ENDIF
         IF ( il8<=0 .AND. il9<=0 ) THEN
            CALL close(in1,1)
            RETURN
         ELSEIF ( ityp==4 ) THEN
!
!     INPUT MATRIX PRECISION TYPE = 4, D.P.COMPLEX
!
            IF ( il8>0 ) THEN
               IF ( prt ) WRITE (nout,99024) row , col , type(ityp) , fnm
               splx(1) = sngl(dz(row))
               splx(2) = sngl(dz(row+1))
               vps(il8) = splx(1)
               vps(il8+1) = splx(2)
               nmvps(1) = ivps(il8-3)
               nmvps(2) = ivps(il8-2)
               IF ( prt ) WRITE (nout,99023) splx , nmvps
            ENDIF
            IF ( il9<=0 ) THEN
               CALL close(in1,1)
               RETURN
            ELSE
               IF ( prt ) WRITE (nout,99024) row , col , type(ityp) , fnm
               dp(1) = dz(row)
               dp(2) = dz(row+1)
               nmvps(1) = ivps(il9-3)
               nmvps(2) = ivps(il9-2)
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
!
!     INPUT MATRIX PRECISION TYPE = 3, S.P.COMPLEX
!
            IF ( il8>0 ) THEN
               IF ( prt ) WRITE (nout,99024) row , col , type(ityp) , fnm
               splx(1) = z(row)
               splx(2) = z(row+1)
               vps(il8) = splx(1)
               vps(il8+1) = splx(2)
               nmvps(1) = ivps(il8-3)
               nmvps(2) = ivps(il8-2)
               IF ( prt ) WRITE (nout,99023) splx , nmvps
            ENDIF
            IF ( il9<=0 ) THEN
               CALL close(in1,1)
               RETURN
            ELSE
               IF ( prt ) WRITE (nout,99024) row , col , type(ityp) , fnm
               dp(1) = dble(z(row))
               dp(2) = dble(z(row+1))
               nmvps(1) = ivps(il9-3)
               nmvps(2) = ivps(il9-2)
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     NULL INPUT MATRIX ELEMENT
!
 60      z(row) = 0.
         z(row+1) = 0.
         dz(row) = 0.D0
         dz(row+1) = 0.D0
         IF ( ityp==1 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ityp==2 ) THEN
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ityp==3 .OR. ityp==4 ) THEN
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL close(in1,1)
         RETURN
!
!     ERRORS
!
 80      j = -1
         CALL mesage(j,in1,name)
         RETURN
 100     j = -2
         CALL mesage(j,in1,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
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
END SUBROUTINE paraml
