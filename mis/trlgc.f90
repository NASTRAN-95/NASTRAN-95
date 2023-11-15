
SUBROUTINE trlgc(Tmldtb,Trl,Dit,Itrl,Fct,Fco,Tol,Iflag)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Consts(5) , Degra , Dummy , Twopi , Z(1) , Za(4)
   INTEGER Ii , Ii1 , Incr , It1 , It2 , Iz(1) , Jj , Mach , Ncont , Sysbuf
   COMMON /blank / Dummy , Ncont
   COMMON /condas/ Consts
   COMMON /machin/ Mach
   COMMON /packx / It1 , It2 , Ii , Jj , Incr
   COMMON /system/ Sysbuf
   COMMON /zblpkx/ Za , Ii1
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Dit , Fco , Fct , Iflag , Itrl , Tmldtb , Tol , Trl
!
! Local variable declarations
!
   LOGICAL dec
   REAL deltat , ft , q1 , q2 , q3 , rt , t , to , tt , zrad
   INTEGER file , i , ibuf1 , ibuf2 , ibuf3 , igroup , ilen , iltab , ip , ip1 , iq , iq1 , iq2 , iq3 , iqvec , iret , ist , itab , &
         & iterm , itid , itlist(13) , ivs , j , k , l , lrec , ltab , lx , m , mcb(7) , mcb1(7) , name(2) , ngroup , noload ,      &
         & nout , nqvect , nstep , ntabl , nterm , nz
   INTEGER korsz , numtyp
!
! End of declarations
!
!
!     THE PURPOSE OF THIS SUBROUTINE IS TO PRODUCE A MATRIX OF FUNCTIONS
!     OF TIME.  EACH COLUMN IS A TIME STEP (AS DEFINE BY TRL) AND EACH
!     TERM IN A COLUMN CORRESPONDS TO A UNIQUE FUNCTION OF TIME (EITHER
!     BY TABLE FROM TLOAD, TIME DELAY, OR QVECT)
!
!     INPUTS (3)
!         TMLDTB - TABLE SHOWING TIME DEPENDANT DATA
!         TRL    - TIME STEP LIST
!         DIT    - DIRECT INPUT TABLES
!         ITRL   - SELECTED TRL SET NUMBER FROM CASECC
!
!     OUTPUTS(3)
!         FCT    - TIME FUNCTIONS AT ALL TIMES
!         FCO    - TIME FUNCTIONS AT OUTPUT TIMES
!         IFLAG  - -1 IMPLIES ALL TIMES OUTPUT (I.E. FCO = FCT)
!         TOL    - TABLE OF OUTPUT TIMES
!
!     THE FORMAT OF THE  TMLDTB TABLE IS AS FOLLOWS
!         REC NO.  WORD  DESCRIPTION
!         0        1-2   TABLE NAME
!         1        1     TERM NUMBER
!                  2     TLOAD ID
!                  3     TLOAD TYPE(1,2)
!                  4     TAU ( FROM DELAY CARDS--REAL)
!                  5     TID (TABLES FROM TLOAD1 CARD)
!                  5     T1   CONSTANTS FROM TLOAD 2 CARDS
!                  6     T2
!                  7     F
!                  8     P
!                  9     C
!                  10    B
!                  11    QVECT POINTER INTO SECOND RECORD
!
!         WORDS  1 THRU 11 ARE REPEATED FOR EACH UNIQUE TIME FUNCTION
!
!         2        1    I1   QVECT TABLE ID'S
!                  2    I2
!                  3    I3
!                  4    V1   QVECT ORIENTATION VECTORS
!                  5    V2
!                  6    V3
!                  7    V4
!                  8    V5
!                  9    V6
!
!     CORE LAYOUT IS AS FOLLOWS $                                POINT
!     ========================================  ===============  =====
!     TERM DESCRIPTORS (11 WORDS PER TERM)      11*NTERM WORDS   ITERM
!     QVECT STUFF      (9  WORDS PER QVECT)     9*NQVECT WORDS   IQVECT
!     TRL   STUFF      (3 WORDS PER GROUP)      3*NGROUP WORDS+1 TGROUP
!     TABLE LIST       (1 WORD  PER UNIQUETAB)  NTAB WORDS+1     ITAB
!     TABLE DATA        PRETAB STORED           LTAB WORDS       ILTAB
!     TERM VALUES                               NTERM WORDS      IVS
!
!     3    BUFFERS      FCT                                      IBUF1
!                       FCO                                      IBUF2
!                       TOL                                      IBUF3
!
   EQUIVALENCE (Z(1),Iz(1)) , (Consts(2),Twopi) , (Consts(4),Degra)
   DATA name/4HTRLG , 4HC   /
   DATA itlist/4 , 1105 , 11 , 1 , 1205 , 12 , 2 , 1305 , 13 , 3 , 1405 , 14 , 4/
!
   dec = Mach==5 .OR. Mach==6 .OR. Mach==21
   noload = 0
   mcb(1) = Tmldtb
   CALL rdtrl(mcb)
   IF ( mcb(2)<=0 ) noload = -1
   mcb(2) = 100
   igroup = 1
   Iflag = -1
   nz = korsz(Z)
   ibuf1 = nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   nz = ibuf3 - 1
   IF ( nz<=0 ) CALL mesage(-8,0,name)
!
!     BRING IN  TIME DATA
!
   IF ( noload/=0 ) GOTO 300
   iterm = 1
   lrec = 11
   file = Tmldtb
   CALL gopen(Tmldtb,Iz(ibuf1),0)
   CALL read(*900,*100,Tmldtb,Iz(iterm),nz,0,ilen)
   CALL mesage(-8,0,name)
 100  nterm = ilen/lrec
   iqvec = iterm + ilen
   nz = nz - ilen
!
!     BRING IN  QVECT DATA
!
   CALL read(*900,*200,Tmldtb,Iz(iqvec),nz,0,ilen)
   CALL mesage(-8,0,name)
 200  nqvect = ilen/9
   igroup = iqvec + ilen
   nz = nz - ilen
   CALL close(Tmldtb,1)
!
!     FIND TRL STUFF FOR CORE
!
 300  file = Trl
   CALL open(*1100,Trl,Iz(ibuf1),0)
   CALL fread(Trl,Iz(igroup),3,1)
   CALL skprec(Trl,Iz(igroup+2))
 400  CALL read(*1200,*500,Trl,Iz(igroup),nz,0,ilen)
   CALL mesage(-8,0,name)
 500  IF ( Iz(igroup)/=Itrl ) GOTO 400
   ngroup = (ilen-1)/3
   itab = igroup + ilen
   nz = nz - ilen
   CALL close(Trl,1)
   IF ( noload==0 ) THEN
!
!     BUILD LIST OF UNIQUE TABLES
!
      ntabl = 1
      k = itab + ntabl
      Iz(k) = 0
      DO i = 1 , nterm
         k = iterm + lrec*(i-1) + 4
         IF ( Iz(k-2)==3 ) THEN
            itid = Iz(k)
            ASSIGN 520 TO iret
            GOTO 580
         ENDIF
 520     k = iterm + lrec*(i-1) + 10
         IF ( Iz(k)==0 ) CYCLE
!
!     LOOK AT QVECT  TABLE  ID S
!
         iq = (Iz(k)-1)*9 + iqvec
         itid = Iz(iq)
         ASSIGN 540 TO iret
         GOTO 580
 540     itid = Iz(iq+1)
         ASSIGN 560 TO iret
         GOTO 580
 560     itid = Iz(iq+2)
         ASSIGN 650 TO iret
!
!     SEARCH TABLE LIST
!
 580     l = numtyp(itid)
         IF ( dec .AND. itid>16000 .AND. itid<=99999999 ) l = 1
         IF ( itid>0 .AND. l==1 ) THEN
            DO l = 1 , ntabl
               k = itab + l
               IF ( Iz(k)==itid ) GOTO 600
            ENDDO
!
!     NEW TABLE
!
            ntabl = ntabl + 1
            k = itab + ntabl
            Iz(k) = itid
         ENDIF
 600     GOTO iret
 650  ENDDO
      Iz(itab) = ntabl
      iltab = itab + ntabl + 1
      nz = nz - ntabl - 1
!
!     BRING IN TABLE STUFF
!
      ltab = 0
      IF ( ntabl/=1 ) CALL pretab(Dit,Iz(iltab),Iz(iltab),Iz(ibuf1),nz,ltab,Iz(itab),itlist)
      nz = nz - ltab
      ivs = iltab + ltab
      IF ( nz<nterm ) CALL mesage(-8,0,name)
!
!     SET UP FOR PACK
!
      It1 = 1
      It2 = 1
      Ii = 1
      Jj = nterm
      Incr = 1
      CALL makmcb(mcb,Fct,nterm,2,It2)
      CALL makmcb(mcb1,Fco,nterm,2,It2)
!
!     OPEN OUTPUT FILES
!
      CALL gopen(Fct,Iz(ibuf1),1)
   ENDIF
   file = Tol
   to = 0.0
   IF ( Ncont>2 ) THEN
!
!     BRING BACK LAST TIME FOR CONTINUE MODE
!
      CALL open(*1100,Tol,Iz(ibuf2),0)
      CALL fread(Tol,to,-Ncont-1,0)
      CALL fread(Tol,to,1,1)
      CALL close(Tol,1)
   ENDIF
   CALL open(*1100,Tol,Iz(ibuf2),1)
   CALL fname(Tol,Za)
   CALL write(Tol,Za,2,0)
   IF ( noload==0 ) THEN
!
!     DETERMINE IF ALL TIME STEPS OUTPUT
!
      DO i = 1 , ngroup
         k = igroup + (i-1)*3 + 3
         IF ( Iz(k)/=1 ) GOTO 700
      ENDDO
      Iflag = -1
   ENDIF
   GOTO 800
 700  Iflag = 1
   CALL gopen(Fco,Iz(ibuf3),1)
 800  t = to
   ist = -1
   DO i = 1 , ngroup
!
!     PICK UP  TIME CONSTANTS
!
      k = igroup + (i-1)*3 + 1
      nstep = Iz(k)
      IF ( i==ngroup ) nstep = nstep + 1
      nout = Iz(k+2)
      deltat = Z(k+1)
      IF ( i==1 ) nstep = nstep + 1
      DO j = 1 , nstep
         IF ( noload==0 ) THEN
            DO l = 1 , nterm
               ip = iterm + (l-1)*lrec
               m = Iz(ip+2) - 2
               IF ( m==2 ) THEN
!
!     TLOAD2  CARD2
!
                  tt = t - Z(ip+3) - Z(ip+4)
                  zrad = Z(ip+7)*Degra
                  IF ( tt==0.0 ) THEN
!
!     TT = 0.0  TRY  LIMITS OF EXPRESSION
!
                     IF ( Z(ip+9)/=0.0 ) THEN
!
!     FT = 0.0
!
                        ft = 0.0
                     ELSE
                        ft = cos(zrad)
                     ENDIF
                  ELSEIF ( tt<0.0 .OR. tt>Z(ip+5)-Z(ip+4) ) THEN
                     ft = 0.0
                  ELSE
                     ft = tt**Z(ip+9)*exp(Z(ip+8)*tt)*cos(Twopi*Z(ip+6)*tt+zrad)
                  ENDIF
               ELSE
!
!     TLOAD1  CARD
!
                  tt = t - Z(ip+3)
                  CALL tab(Iz(ip+4),tt,ft)
               ENDIF
!
!     NOW TRY FOR  QVECT  STUFF
!
               IF ( Iz(ip+10)/=0 ) THEN
!
!     EVALUATE  QVECT FUNCTION
!
                  iq = (Iz(ip+10)-1)*9 + iqvec
                  tt = t - Z(ip+3)
!
!     CHECK FOR CONSTANT FLUX VALUE (FLOATING POINT).
!     IF TIME DEPENDENT, CALL TABLE LOOKUP.
!
                  iq1 = Iz(iq)
                  q1 = Z(iq)
                  lx = numtyp(iq1)
                  IF ( dec .AND. iq1>16000 .AND. iq1<=99999999 ) lx = 1
                  IF ( iq1>0 .AND. lx==1 ) CALL tab(iq1,tt,q1)
                  iq2 = Iz(iq+1)
                  q2 = Z(iq+1)
                  lx = numtyp(iq2)
                  IF ( dec .AND. iq2>16000 .AND. iq2<=99999999 ) lx = 1
                  IF ( iq2>0 .AND. lx==1 ) CALL tab(iq2,tt,q2)
                  iq3 = Iz(iq+2)
                  q3 = Z(iq+2)
                  lx = numtyp(iq3)
                  IF ( dec .AND. iq3>16000 .AND. iq3<=99999999 ) lx = 1
                  IF ( iq3>0 .AND. lx==1 ) CALL tab(iq3,tt,q3)
                  IF ( Z(iq+6)/=0.0 .OR. Z(iq+6)/=0.0 .OR. Z(iq+7)/=0.0 .OR. Z(iq+8)/=0.0 ) THEN
!
!     V2   0
!
                     ft = sqrt((q1*Z(iq+3)+q2*Z(iq+4)+q3*Z(iq+5))**2+(q1*Z(iq+6)+q2*Z(iq+7)+q3*Z(iq+8))**2)*ft
                  ELSE
!
!     V2 = 0
!
                     rt = q1*Z(iq+3) + q2*Z(iq+4) + q3*Z(iq+5)
                     IF ( rt>0.0 ) rt = 0.0
                     ft = -rt*ft
                  ENDIF
               ENDIF
!
!     PUT IN FT
!
               m = ivs + l - 1
               Z(m) = ft
            ENDDO
!
!     COLUMN BUILT
!
            CALL pack(Z(ivs),Fct,mcb)
         ENDIF
         IF ( i/=ngroup .OR. j/=nstep-1 ) THEN
            IF ( j/=1 .AND. j/=nstep ) THEN
               IF ( mod(j+ist,nout)/=0 ) GOTO 820
            ENDIF
         ENDIF
!
!     OUTPUT TIME
!
         CALL write(Tol,t,1,0)
         IF ( Iflag/=-1 ) CALL pack(Z(ivs),Fco,mcb1)
         IF ( j==nstep ) deltat = Z(k+4)
 820     t = t + deltat
      ENDDO
      ist = 0
   ENDDO
!
!     ALL OUTPUT
!
   CALL write(Tol,0,0,1)
   CALL close(Tol,1)
   IF ( noload==0 ) THEN
      CALL close(Fct,1)
      CALL wrttrl(mcb)
      IF ( Iflag/=-1 ) THEN
         CALL close(Fco,1)
         CALL wrttrl(mcb1)
      ENDIF
   ENDIF
   mcb(1) = Tol
   CALL wrttrl(mcb)
   RETURN
!
!     ERROR MESSAGES
!
 900  ip1 = -2
 1000 CALL mesage(ip1,file,name)
   RETURN
 1100 ip1 = -1
   GOTO 1000
!
!     NO PROPER TSTEP CARD FOUND
!
 1200 CALL mesage(-31,Itrl,name)
END SUBROUTINE trlgc
