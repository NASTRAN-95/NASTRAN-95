
SUBROUTINE rcovqv
   IMPLICIT NONE
   INTEGER Buf1 , Buf2 , Buf3 , Buf4 , Fss(2) , Icore , Iopt , Ireq , Lbasic , Lcore , Loop , Lreq , Lui , Mcba(7) , Mcbb(7) ,      &
         & Mcbc(7) , Mcbd(7) , Mpyz , Mrecvr , Neigv , Nosort , Nout , Pa , Prec , Qa , Rfno , Rss(2) , Scrm , Signab , Signc ,     &
         & Sof1 , Sof2 , Sof3 , Sysbuf , Tflag , Ua
   REAL Dry , Energy , Pthres , Qthres , Range(2) , Step , Uimpro , Uinms(2,5) , Uthres , Z(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Dry , Loop , Step , Fss , Rfno , Neigv , Lui , Uinms , Nosort , Uthres , Pthres , Qthres
   COMMON /mpyadx/ Mcba , Mcbb , Mcbc , Mcbd , Mpyz , Tflag , Signab , Signc , Prec , Scrm
   COMMON /rcovcm/ Mrecvr , Ua , Pa , Qa , Iopt , Rss , Energy , Uimpro , Range , Ireq , Lreq , Lbasic
   COMMON /rcovcr/ Icore , Lcore , Buf1 , Buf2 , Buf3 , Buf4 , Sof1 , Sof2 , Sof3
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Z
   INTEGER bgg , bmtx , i , in , item , k4gg , k4mx , kgg , kmtx , malcom , mgg , mmtx , n , name(2) , qvec , rc , scr1 , scr2 ,    &
         & scr4 , scr5 , scr6 , scr7 , scr8
   REAL file
   INTEGER korsz
   LOGICAL reigen , reqf
!
!     THIS SUBROUTINE CALCULATES THE REACTION FORCES FOR THE REQUESTED
!     SUBSTRUCTURE
!
!    9,               UVEC       ,SRD
   DATA qvec , kmtx , mmtx , bmtx , k4mx/4HQVEC , 4HKMTX , 4HMMTX , 4HBMTX , 4HK4MX/
!     DATA    UVEC  , SRD   / 4HUVEC, 1                              /
   DATA kgg , mgg , bgg , k4gg , name/103 , 104 , 109 , 110 , 4HRCOV , 4HQV  /
   DATA scr1 , scr2 , scr4 , scr5 , scr6 , scr7 , scr8/301 , 302 , 304 , 305 , 306 , 307 , 308/
!
!
!     CHECK TO SEE IF QVEC HAS ALREADY BEEN CALCULATED
!
   CALL mtrxi(scr4,Rss,qvec,0,rc)
   IF ( rc/=1 ) THEN
!
!     INITILIZE FOR QVEC CALCULATIONS
!
      Prec = 0
      Tflag = 0
      Scrm = scr5
      Signab = 1
      Mpyz = korsz(Z(1)) - Lreq
      reqf = .FALSE.
      IF ( Fss(1)==Rss(1) .AND. Fss(2)==Rss(2) ) reqf = .TRUE.
      reigen = .FALSE.
      malcom = 0
      IF ( Ua==scr1 ) THEN
         scr2 = 301
         scr1 = 302
      ENDIF
!
!     CHECK THE DISPLACEMENT MATRIX
!
      Mcbb(1) = Ua
      CALL rdtrl(Mcbb)
      IF ( Mcbb(1)<=0 ) GOTO 800
!
!     BRANCH ON RIGID FORMAT
!
      IF ( Rfno>9 ) THEN
         n = 7
         CALL mesage(n,0,name)
         GOTO 800
      ELSE
         IF ( Rfno==3 ) THEN
!
!     NORMAL MODES
!
!     CHECK IF THE EIGEN VECTORS ARE COMPLEX
!
            IF ( Mcbb(5)>=3 ) THEN
!
!     COMPLEX NORMAL MODES
!
!     Q = KU + BV + MA
!
!     CALCULATE THE COMPLEX VELOCITIES AND ACCLERATION VECTORS FOR
!     THE EIGENVECTORS
!
               in = Ua
!
!     SEE MALCOM TAGG RECOMMENDATION, 25 LINES ABOVE
!
               CALL sofcls
               IF ( malcom==1 ) GOTO 100
!
               CALL rcovva(in,0,0,scr6,scr7,scr8,Rss,Z(1),Z(1),Z(1))
               IF ( in<=0 ) GOTO 800
!
!     INDICATE ZERO LOAD VECTOR FOR NORMAL MODES
!
               Mcbc(1) = 0
            ELSE
!
!     REAL NORMAL MODES
!
!      Q = KU + MA   WHERE A = -(2*PI*FREQ)**2 * U
!
               reigen = .TRUE.
!
!     MALCOM TAGG OF MDC, IN MSFC, RECOMMANDED THAT FOR RIGID FORMAT 3
!     THE SPC REACTION FORCE SHOULD NOT CONTAIN THE MASS TERM.  JULY/86
!     I.E.    Q = KU ONLY   (DROP THE MA TERM)
!     THUS,   GO TO 250 THEN TO 500
!
!     MARCH 1989 - MALCOM RECOMMENDATION REMOVED. IT CAUSES IMBALANCED
!     SPC FORCES
!
!     MALCOM = 1
               IF ( malcom/=1 ) THEN
!
!     CALCULATE THE ACCLERATION VECTOR FOR REAL NORMAL MODES
!
                  in = Ua
                  CALL rcovva(in,0,0,0,0,scr8,Rss,Z(1),Z(1),Z(1))
                  IF ( in<=0 ) GOTO 800
               ENDIF
!
!
!     INDICATE A POSITIVE SIGN ON THE M * A MULTIPLY
!
               Signab = 1
               Mcbc(1) = 0
               IF ( malcom==1 ) GOTO 600
            ENDIF
         ELSEIF ( Rfno==4 .OR. Rfno==5 .OR. Rfno==6 .OR. Rfno==7 ) THEN
            n = 7
            CALL mesage(n,0,name)
            GOTO 800
         ELSEIF ( Rfno==8 .OR. Rfno==9 ) THEN
!
!     DYNAMIC ANALYSIS
!
!     Q = KU + BV + MA - P
!
!
!     SPLIT DISPLACEMENT, VELOCITIES AND ACCELERATIONS ONTO SEPERATE
!     FILES
!
            in = Ua
            CALL rcovva(in,1,0,scr6,scr7,scr8,Rss,Z(1),Z(1),Z(1))
            IF ( in<=0 ) GOTO 800
!
!     SETUP TO SUBTRACT LOAD VECTOR
!
            Signc = -1
            Mcbc(1) = Pa
            IF ( Pa>0 ) CALL rdtrl(Mcbc)
         ELSE
!
!     STATIC SOUTION
!
!     Q = KU - P
!
!     SET UP LOAD VECTOR FOR SUBSTRACTION
!
            Signc = -1
            Mcbc(1) = Pa
            IF ( Pa>0 ) CALL rdtrl(Mcbc)
            GOTO 600
         ENDIF
!
!     COMMON PROCESSING FOR DYNAMICS AND NORMAL MODES
!
!
!     MULTIPLY AND ADD    SCR1 = MA - P
!
         IF ( reqf ) THEN
            Mcba(1) = mgg
            CALL rdtrl(Mcba)
            IF ( Mcba(1)>0 ) GOTO 50
         ENDIF
         CALL mtrxi(scr4,Rss,mmtx,0,rc)
         IF ( rc/=1 ) GOTO 200
         Mcba(1) = scr4
         CALL rdtrl(Mcba)
      ENDIF
 50   Mcbb(1) = scr8
      CALL rdtrl(Mcbb)
      CALL makmcb(Mcbd,scr1,Mcbb(3),Mcbb(4),Mcbb(5))
      CALL sofcls
!
      CALL mpyad(Z(1),Z(1),Z(1))
!
 100  DO i = 1 , 7
         Mcbc(i) = Mcbd(i)
      ENDDO
      Signc = 1
      CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
   ELSE
      Qa = scr4
      RETURN
   ENDIF
!
!     MULTIPLY AND ADD   SCR8 = K4V + MCBC
!
 200  IF ( reigen .OR. Rfno==9 ) GOTO 400
   IF ( reqf ) THEN
      Mcba(1) = k4gg
      CALL rdtrl(Mcba)
      IF ( Mcba(1)>0 ) GOTO 300
   ENDIF
   CALL mtrxi(scr4,Rss,k4mx,0,rc)
   IF ( rc/=1 ) GOTO 400
   Mcba(1) = scr4
   CALL rdtrl(Mcba)
 300  Mcbb(1) = scr7
   CALL rdtrl(Mcbb)
   CALL makmcb(Mcbd,scr8,Mcbb(3),Mcbb(4),Mcbb(5))
   Signab = 1
   CALL sofcls
!
   CALL mpyad(Z(1),Z(1),Z(1))
!
   DO i = 1 , 7
      Mcbc(i) = Mcbd(i)
   ENDDO
   Signc = 1
   CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
 400  IF ( reigen ) GOTO 600
!
!     MULTIPLY AND ADD   SCR1 = BV + MCBC
!
   IF ( reqf ) THEN
      Mcba(1) = bgg
      CALL rdtrl(Mcba)
      IF ( Mcba(1)>0 ) GOTO 500
   ENDIF
   CALL mtrxi(scr4,Rss,bmtx,0,rc)
   IF ( rc/=1 ) GOTO 600
   Mcba(1) = scr4
   CALL rdtrl(Mcba)
 500  Mcbb(1) = scr7
   CALL rdtrl(Mcbb)
   CALL makmcb(Mcbd,scr1,Mcbb(3),Mcbb(4),Mcbb(5))
   Signab = 1
   CALL sofcls
!
   CALL mpyad(Z(1),Z(1),Z(1))
!
   DO i = 1 , 7
      Mcbc(i) = Mcbd(i)
   ENDDO
   Signc = 1
   CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
!
!     COMMON PROCESSING FOR ALL RIGID FORMATS
!
!
!     MULTIPLY AND ADD  Q = KU + MCBC
!
 600  IF ( reqf ) THEN
      Mcba(1) = kgg
      CALL rdtrl(Mcba)
      IF ( Mcba(1)>0 ) GOTO 700
   ENDIF
   item = kmtx
   file = scr7
   CALL mtrxi(scr7,Rss,kmtx,0,rc)
   IF ( rc/=1 ) THEN
!
!     ERRORS
!
      IF ( rc==6 ) THEN
         CALL mesage(n,0,name)
      ELSE
         CALL smsg(rc-2,item,Rss)
      ENDIF
      GOTO 800
   ELSE
      Mcba(1) = scr7
      CALL rdtrl(Mcba)
   ENDIF
 700  Mcbb(1) = scr6
   IF ( reigen .OR. Rfno<=2 ) Mcbb(1) = Ua
   CALL rdtrl(Mcbb)
   CALL makmcb(Mcbd,scr4,Mcbb(3),Mcbb(4),Mcbb(5))
   Signab = 1
   CALL sofcls
!
   CALL mpyad(Z(1),Z(1),Z(1))
!
   CALL wrttrl(Mcbd)
   CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
!
!     COPY REACTIONS TO SOF
!
   CALL mtrxo(scr4,Rss,qvec,0,rc)
   Qa = scr4
!
   RETURN
 800  Qa = 0
   WRITE (Nout,99001) Swm
99001 FORMAT (A27,' 6318, OUTPUT REQUEST FOR REACTIONS FORCES IGNORED.')
!
END SUBROUTINE rcovqv