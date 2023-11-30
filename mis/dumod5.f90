
SUBROUTINE dumod5
!
!     MSFC ROUTINE, TO CONVERT NASTRAN TABULAR DATA BLOCKS INTO 2-
!     DIMENSIONAL DATA BLOCKS (S.P. REAL ONLY) FOR CONVENIENCE IN
!     MANIPULATION AND OUTPUT, SPECIALLY TO BE USED WITH OUTPUT5 AND
!     INPUT5.
!
!     THIS VERSION WAS MODIFIED BY R. MOORE/MSFC IN JAN. 1989
!     TO ALLOW SELECTION OF EITHER 8 OR 16 VALUES PER ELEMENT BY
!     USING A 7TH PARAMETER ON DMAP
!
!     DUMMOD5  T1,T2,T3,T4,T5/O1,O2,O3,O4,O5/C,N,P1/C,N,P2/C,N,P3
!              C,N,P4/C,N,P5/C,N,Q/C,N,R $
!
!     TI  = INPUT GINO FILE, OEF1, OQG1 OR SIMILAR TYPE OF TABULAR
!           DATA BLOCKS
!     OI  = OUTPUT GINO DATA BLOCK, PACKED, BUT NOT QUITE A REGULAR
!           NASTRAN MATRIX BLOCK, SEE PICTURE BELOW
!           IF OI IS PURGED (NOT PRESENT), MATRIX BLOCK IS WRITTEN OUT
!           TO FORTRAN UNIT 15 (INP1) DIRECTLY, IN BINARY RECORDS,
!           BANDED MATRIX FORM (FROM FIRST NON-ZERO TO LAST NON-ZERO
!           ELEMENTS), D.R. OR S.P.
!     PI  = TI TABLE IS MAPPED INTO A PI X 8 2-DIMENSIONAL BLOCKS.
!           EACH BLOCK IS PACKED AS A COLUMN OF A MATRIX
!     Q   = ELEMENT/GRID POINT ID PRINT-PUNCH CONTROL
!         = -1, NO PRINT AND NO PUNCH
!         =  0, PRINT ONLY, NO PUNCH
!         = +1, BOTH PRINT AND PUNCH
!         = /2/ CONTENTS OF OUTPUT TAPE, INP1, WILL BE PRINTED OUT
!     R   = SWITCH TO CHANGE FROM 8 TO 16 VALUES IN TABLE MAPPING
!           DEFAULT = 0 WHICH SETS TO 8.    R = 1 SETS IT TO 16
!
!     CDC USER ONLY - FORTRAN UNIT 11 (UT1) IS USED INSTEAD OF 15 (INP1)
!
!
!           |<------ 8 OR 16 ------->|
!           ==========================
!         / I                        I \
!        /  I------- TABULAR --------I  \
!       P1  I         DATA           I  BLOCK 1 (MATRIX COLUMN 1)
!        \  I-------- BLOCKS --------I  /
!         \ I                        I /
!           ==========================
!         / I                        I \
!        /  I------------------------I  \
!       P1  I                        I  BLOCK 2 (MATRIX COLUMN 2)
!
!     WRITTEN BY SOMEBODY FOR MARSHALL SPACE FLIGHT CENTER (MSFC).
!     MODIFIED BY G.CHAN/UNISYS TO EMPLOY OPEN-CORE SPACE INSTEAD OF
!     THE FIXED DIMENSION ARRAYS, AND TO EXPAND FROM ONE INPUT DATA
!     BLOCK TO FIVE. IF A CORRESPONDING OUTPUT FILE IS MISSING OR
!     PURGED, THE DATA BLOCKS ARE WRITTEN DIRECTLY TO FORTRAN TAPE
!     (UNIT 15, INP1) USING OUTPUT5 BINARY FORMAT.
!
!     CONTENTS OF INP1 TAPE IF IT IS WRITTEN -
!
!         RECORD   WORD     CONTENT                           TYPE
!         ------  ------   ----------------------------------------
!            0              TAPE HEADER RECORD
!                   1-2     'XXXXXXXX', TAPE ID              2*BCD
!                   3-4     MACHINE TYPE                     2*BCD
!                   5-7     DATE                             3*INT
!                    8      SYSTEM BUFFSIZE                    INT
!                    9      0 (BINARY TAPE)                    INT
!            1              FIRST MATRIX HEADER
!                    1      0                                  INT
!                   2,3     1,1                              2*INT
!                    4      A DOUBLE PRECISION ZERO           D.P.
!                   5-10    6 WORDS FROM MATRIX TRAILER      6*INT
!                           (COL,ROW,FORM,TYPE,MAX,DENSITY-
!                            TYPE=1 OR 3, DENSITY=1)
!                  11-12    MATRIX DMAP NAME                 2*BCD
!            2       1      1 (FIRST COLUMN ID)                INT
!                    2      LOCATION OF FIST NON-ZERO ELEMENT  INT
!                    3      LOCATION OF LAST NON-ZERO ELEMENT  INT
!                   4-N     S.P. DATA                         REAL
!            3       1      2 (SECOND COLUMN ID)               INT
!                   2-N     SAME AS RECORD 1
!            :      1-N     REPEAT FOR MORE COLUMNS
!
!            X       1      X (X-TH COLUMN ID, A NUL COLUMN)   INT
!                   2-3     1,1                                INT
!                   4-5     0.0,0.0                           REAL)
!
!            M      1-N     LAST COLUMN, SAME AS RECORD 1
!           M+1      1      -1 (ELEM) OR -2 (GRID)             INT
!                    2      1                                  INT
!                    3      LENGTH OF ELEM./GRID ID LIST, L    INT
!                  4-L+4    LIST OF ELEMENT OR GRID IDS        INT
!
!           M+2             SECOND MATRIX HEADER
!            :       :      REPEAT 1 THRU (M+1) FOR THE SECOND MATRIX
!
!            :       :      REPEAT, UP TO 5 OUTPUT DATA BLOCKS PER TAPE
!
!     COMMENTS FROM G.C. -
!     (1) THIS MODULE IS VERY LIMITED IN SCOPE. IT HANDLES ONLY SOME
!         SPECIAL TYPES OF TABULAR INPUT DATA BLOCKS. THE (PI X 8) MATRI
!         SPACE IS FOR PRINT/PUNCH PURPOSE. THE ORIGINAL PROGRAM SEEMS
!         TO BE WRITTEN TO MEET A PARTICULAR JOB REQUIREMENT.
!
!     (2) CURRENT MODULE HANDLES ONLY SINGLE PRECISION DATA
!
!     (3) THE PROCEDURE TO READ AND/OR WRITE THE TAPE IS COMMONLY USED
!         AMONG INPUTT5, OUTPUT5, AND DUMMOD5. ANY PROCEDURE CHANGE
!         SHOULD BE MADE TO ALL THREE MODULES.
!
   IMPLICIT NONE
   INTEGER Date(3) , Dumm(88) , Ibuf , Ii , Ijhalf(3) , Incr , Iz(8) , Jj , Lpch , Mach , Mchnam , Nout , P(5) , Q , R , Typin ,    &
         & Typout
   CHARACTER*80 Dsnames(80)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   REAL Z(1)
   COMMON /blank / P , Q , R
   COMMON /dsname/ Dsnames
   COMMON /machin/ Mach , Ijhalf , Mchnam
   COMMON /packx / Typin , Typout , Ii , Jj , Incr
   COMMON /system/ Ibuf , Nout , Dumm , Lpch
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   INTEGER buf1 , buf2 , core , eg(2) , half , half1 , i , i6or8 , id(5001) , ie , ifirst , ijk , im , infile(2) , input , ir(5001) &
         & , irdlmt , is , itype , j , jb , je , jm , jsq , k , kk , kount , l , last , lj , loc , loca , locid , loop , m , mcb(7) &
         & , mt(2) , name(2) , neltp , newlt , nrop , nwds , nwds1 , nwds2 , nxir , nxzh , one , outfil(2) , outpt , pv , retn ,    &
         & save(2,5) , tape , tapp , tapx , temp(10) , trl(7) , type , unvc(2) , xx , zero
   LOGICAL debug , none
   DOUBLE PRECISION dtemp , dzero
   REAL epsi
   INTEGER korsz , numtyp
!WKBNB
!WKBNE
   EQUIVALENCE (Z(1),Iz(1)) , (Date(1),Dumm(13))
!WKBI
   DATA ifirst/0/
   DATA tape , irdlmt , id , im , ie , xx , epsi/15 , 5000 , 5001*0 , 1H- , 1H= , 4HXXXX , 1.0E-30/
   DATA zero , one , eg , name/0 , 1 , 4HELEM , 4HGRID , 4HDUMO , 4HD5  /
   DATA unvc , mt/4HUNIV , 4HAC   , 2*4H    /
   DATA debug , dzero , save/.FALSE. , 0.D0 , 10*1H /
!
   IF ( Mach==12 ) tape = 11
   CALL page
   WRITE (Nout,99001) P , Q , R
99001 FORMAT ('0*** MODULE DUMMOD5 CALLED BY USER DMAP ALTER.',/5X,'PARAMETERS ARE    P=',5(I5,1H,),5X,'Q=',I5,5X,'R=',I4,/)
   i6or8 = 8
   IF ( R==1 ) i6or8 = 16
   Incr = 1
   Typin = 1
   Typout = 1
   Ii = 1
   tapx = -1
   tapp = -1
   core = korsz(Z)
   buf1 = core - Ibuf + 1
   buf2 = buf1 - Ibuf
   core = buf2 - 1
   half = core/2
   half1 = half + 1
!WKBNB
   IF ( ifirst==0 ) THEN
      CLOSE (UNIT=tape)
      OPEN (UNIT=tape,FILE=Dsnames(tape),FORM='UNFORMATTED',STATUS='UNKNOWN')
      ifirst = 1
   ENDIF
!WKBNE
!
   DO loop = 1 , 5
      input = 100 + loop
      outpt = 200 + loop
      trl(1) = input
      CALL rdtrl(trl(1))
      IF ( trl(1)<=0 ) CYCLE
      CALL fname(input,infile)
!
!     INPUT DATA PRECISION TYPE IS S.P. ONLY
!
      type = 1
!
      IF ( P(loop)<=0 ) P(loop) = pv
      pv = P(loop)
      Jj = P(loop)*i6or8
      DO j = 1 , Jj
         Z(j+half) = 0.0
      ENDDO
      CALL gopen(input,Z(buf1),0)
      mcb(1) = outpt
      CALL rdtrl(mcb)
      none = .FALSE.
      IF ( mcb(1)<=0 ) none = .TRUE.
      IF ( none ) THEN
         tapx = tapx + 1
         IF ( tapx>0 ) THEN
            save(1,tapx) = infile(1)
            save(2,tapx) = infile(2)
         ENDIF
      ELSE
         CALL gopen(outpt,Z(buf2),1)
         CALL fname(outpt,outfil)
         CALL makmcb(mcb,outpt,0,2,1)
      ENDIF
      i = 1
      nxzh = 0
      nxir = 0
      CALL read(*500,*50,input,temp,10,1,m)
      nwds = temp(10)
      neltp = temp(3)
!     IF (NELTP.GE.11 .AND. NELTP.LE.14) GO TO 320
!               CELAS1            CELAS4
      GOTO 200
 50   CALL mesage(-37,0,name)
 100  CALL read(*500,*150,input,temp,10,1,m)
      nwds = temp(10)
      IF ( temp(3)/=neltp ) GOTO 200
      IF ( Q>=1 ) THEN
         is = im
         kk = half + nxzh
         WRITE (Nout,99020) is , i , (Z(j),j=half1,kk)
      ENDIF
      i = i + 1
      IF ( none ) THEN
!
         ASSIGN 450 TO retn
         GOTO 300
      ELSE
         CALL pack(Z(half1),outpt,mcb)
         GOTO 450
      ENDIF
 150  CALL mesage(-61,input,name)
!  60 IF (TEMP(3).GE.11 .AND. TEMP(3).LE.14) GO TO 320
!                 CELAS1              CELAS4
 200  newlt = temp(3)
      nwds1 = nwds - 1
      nwds2 = nwds - 2
      DO l = 1 , Jj
         Z(l) = 0.0
      ENDDO
      DO l = 1 , irdlmt
         ir(l) = 0
      ENDDO
      CALL read(*550,*600,input,ir(1),1,0,m)
      kount = 0
      DO jsq = 1 , irdlmt
         kount = kount + 1
         loc = nwds1*jsq - nwds2
         CALL read(*550,*600,input,Z(loc),nwds1,0,m)
!     LAST = LOC + NWDS1 - 1
         last = kount*i6or8
         CALL read(*550,*250,input,ir(jsq+1),1,0,m)
      ENDDO
 250  m = nwds*kount
      ijk = 0
      DO j = 1 , m , nwds
         ijk = ijk + 1
         nrop = (ir(ijk)-1)/10
         locid = nxir + ijk
         id(locid) = nrop*100 + newlt
         loca = (ijk*i6or8) - (i6or8-1) + nxzh
         lj = nwds1*ijk - nwds1
         kk = loca + nwds + half
         IF ( kk>core ) CALL mesage(-8,0,name)
         DO jm = 1 , nwds1
            Z(loca+jm-1+half) = Z(lj+jm)
         ENDDO
      ENDDO
      nxir = nxir + jsq
      nxzh = nxzh + last
      GOTO 100
 300  DO jb = Ii , Jj
!WKBNB 8/94 ALPHA-VMS
         itype = numtyp(Z(jb+half))
         IF ( itype>1 ) THEN
!WKBNE 8/94 ALPHA-VMS
            IF ( abs(Z(jb+half))>epsi ) GOTO 350
         ENDIF
      ENDDO
      WRITE (tape) i , one , one , (zero,j=1,type)
      IF ( debug ) WRITE (Nout,99002) i , one , one , (zero,j=1,type)
99002 FORMAT (' +++ZEROS/DUMMOD5- ',7I5)
      GOTO 400
 350  je = Jj
      DO j = Ii , Jj
!WKBNB 8/94 ALPHA-VMS
         itype = numtyp(Z(je+half))
         IF ( itype>1 ) THEN
!WKBNE 8/94 ALPHA-VMS
            IF ( abs(Z(je+half))>epsi ) EXIT
         ENDIF
         je = je - 1
      ENDDO
      IF ( type==1 ) THEN
      ELSEIF ( type==4 ) THEN
         j = mod(jb,4)
         IF ( j==0 ) j = 4
         jb = jb - j + 1
         j = mod(je,4)
         IF ( j==0 ) j = 4
         je = je - j + 4
      ELSE
         IF ( mod(jb,2)==0 ) jb = jb - 1
         IF ( mod(je,2)==1 ) je = je + 1
      ENDIF
      WRITE (tape) i , jb , je , (Z(j+half),j=jb,je)
      IF ( debug ) WRITE (Nout,99003) i , jb , je
99003 FORMAT (' +++DATA RECORD/DUMMOD5- ',3I5)
 400  GOTO retn
!
 450  DO l = 1 , Jj
         Z(l+half) = 0.0
      ENDDO
      nxzh = 0
      nxir = 0
      GOTO 200
 500  IF ( Q>=0 ) THEN
         is = ie
         kk = half + nxzh
         WRITE (Nout,99020) is , i , (Z(j),j=half1,kk)
      ENDIF
      ASSIGN 650 TO retn
      IF ( none ) THEN
         IF ( tapx<=0 ) THEN
!
!     WRITE TAPE HEADER AND MATRIX HEADER
!     (CURRENTLY, OUTPUT TAPE IS WRITTEN OUT IN SINGLE PRECISION ONLY)
!     CHANGE IN 89 VERSION -
!     MUST SET MATRIX DENSITY IN MATRIX TRAILER TO NON-ZERO IF INPUT5
!     IS TO BE USED
!
            tapx = 1
            save(1,tapx) = infile(1)
            save(2,tapx) = infile(2)
            mt(1) = Mchnam
            IF ( Mach==3 ) THEN
               mt(1) = unvc(1)
               mt(2) = unvc(2)
            ENDIF
            WRITE (tape) xx , xx , mt , Date , Ibuf , zero
            IF ( debug ) WRITE (Nout,99004) xx , xx , mt , Date , Ibuf , zero
99004       FORMAT ('0+++TAPE HEADER/DUMMOD5-',/3X,2A4,1X,2A4,3I4,2I6)
         ENDIF
         IF ( tapx/=tapp ) THEN
            tapp = tapx
            trl(5) = Typout
            trl(7) = 1
            WRITE (tape) zero , one , one , dzero , (trl(k),k=2,7) , infile
            IF ( debug ) WRITE (Nout,99005) zero , one , one , dzero , (trl(k),k=2,7) , infile
99005       FORMAT (' +++MATRIX HEADER/DUMMOD5- ',3I5,D8.0,6I5,1X,2A4)
         ENDIF
         GOTO 300
      ELSE
         CALL pack(Z(half1),outpt,mcb)
         mcb(3) = Jj
         CALL wrttrl(mcb)
         IF ( Q==2 ) WRITE (Nout,99006) (mcb(j),j=1,5)
99006    FORMAT (/2X,'MCB=',6I8)
         GOTO 650
      ENDIF
! 320 CALL READ (*330,*40 ,INPUT,IR(1),1,0,M)
!     CALL READ (*330,*350,INPUT, Z(1),1,0,M)
!     Z(1) = 0.0
!     GO TO 320
 550  WRITE (Nout,99007) infile
99007 FORMAT (/5X,'*** EOF ENCOUNTERED ON INPUT ',2A4,' DATA BLOCK')
      GOTO 700
 600  WRITE (Nout,99008) infile
99008 FORMAT (/5X,'*** INPUT ',2A4,'DATA BLOCK IS EMPTY')
      GOTO 700
 650  IF ( .NOT.none ) WRITE (Nout,99009) Uim , infile , outfil
99009 FORMAT (A29,', MODULE DUMMOD5 SUCCESSFULLY PROCESSED TABULAR ','DATA FROM ',2A4,' TO DATA BLOCK ',2A4,/5X,                    &
             &'IN GINO PACKED FORM')
      IF ( none ) WRITE (Nout,99010) Uim , infile , tape
99010 FORMAT (A29,', MODULE DUMMOD5 SUCCESSFULLY COPIED TABULAR DATA ','FROM ',2A4,' TO OUTPUT TAPE',/5X,'(FORTRAN UNIT',I4,        &
             &') IN BANDED MATRIX FORM')
      IF ( Q>0 ) WRITE (Lpch,99011) (id(j),j=1,nxir)
99011 FORMAT (8I10)
      l = eg(1)
      IF ( newlt<=0 ) THEN
         l = eg(2)
         DO j = 1 , nxir
            id(j) = id(j)/100
         ENDDO
      ENDIF
      WRITE (Nout,99012) l , infile , (id(j),j=1,nxir)
99012 FORMAT (//5X,A4,'-ID ARRAY FOLLOWS/FROM ',2A4,(/5X,15I8))
      IF ( none ) THEN
         i = -1
         IF ( newlt==0 ) i = -2
         WRITE (tape) i , one , nxir , (id(j),j=1,nxir)
         IF ( debug ) WRITE (Nout,99013) i , one , nxir
99013    FORMAT (' +++ELEM/GRID ID RECORD/DUMMOD5- ',3I5)
      ENDIF
 700  CALL close(input,1)
      IF ( .NOT.none ) CALL close(outpt,1)
   ENDDO
!
   IF ( tapx<=0 ) GOTO 99999
   WRITE (Nout,99014) Uim , tape , (save(1,j),save(2,j),j=1,tapx)
99014 FORMAT (A29,', FOLLOWING DATA BLOCKS WERE COPIED TO FORTRAN UNIT',I3,' BY MODULE DUMMOD5',/5X,                                &
             &'USING UNFORMATTED (BINARY) WRITE',/6X,5(2A4,3X))
   ENDFILE tape
   REWIND tape
!
!     TO READ THE OUTPUT TAPE, Q=/2/
!
   IF ( iabs(Q)<2 ) GOTO 99999
   CALL page1
   k = 1
   READ (tape,END=800) mcb , j , i
   WRITE (Nout,99015) mcb , j
99015 FORMAT (//,'  TAPEID=',2A4,'   FROM ',A4,A2,' MACHINE,  DATE',I5,1H/,I2,1H/,I2,'  BINARY TAPE.   BUFFSIZE=',I7//)
   DO
      READ (tape,END=900) i , jb , je , (Z(j),j=jb,je)
      IF ( i<0 ) THEN
!
         l = eg(-i)
         WRITE (Nout,99016) l , (Iz(j),j=jb,je)
99016    FORMAT (//2X,A4,'-ID LIST -',/,(1X,10I10))
      ELSEIF ( i==0 ) THEN
         BACKSPACE tape
         READ (tape,END=900) i , jb , je , dtemp , (Iz(j),j=1,8)
         WRITE (Nout,99017) k , Iz(7) , Iz(8) , (Iz(j),j=1,6)
99017    FORMAT (//,'  DATA BLOCK',I3,3X,2A4,'  TRAILER=',6I5)
         k = k + 1
      ELSE
         WRITE (Nout,99018) i , jb , je , (Z(j),j=jb,je)
99018    FORMAT (//,'  COLUMN RECORD =',I3,'   JB,JE =',2I5,/,(1X,10E13.6))
      ENDIF
   ENDDO
 800  WRITE (Nout,99019)
99019 FORMAT (//,'  EMPTY TAPE')
 900  REWIND tape
99020 FORMAT ('  COLUMN',A1,I5,/,(2X,8E16.6))
99999 RETURN
END SUBROUTINE dumod5
