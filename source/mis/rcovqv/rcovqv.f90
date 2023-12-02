!*==rcovqv.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE rcovqv
   IMPLICIT NONE
   USE c_blank
   USE c_mpyadx
   USE c_rcovcm
   USE c_rcovcr
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bgg , bmtx , k4gg , k4mx , kgg , kmtx , mgg , mmtx , qvec , scr1 , scr2 , scr4 , scr5 , scr6 , scr7 , scr8
   REAL :: file
   INTEGER :: i , in , item , malcom , n , rc
   INTEGER , DIMENSION(2) , SAVE :: name
   LOGICAL :: reigen , reqf
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
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
   CALL mtrxi(scr4,rss,qvec,0,rc)
   IF ( rc/=1 ) THEN
!
!     INITILIZE FOR QVEC CALCULATIONS
!
      prec = 0
      tflag = 0
      scrm = scr5
      signab = 1
      mpyz = korsz(z(1)) - lreq
      reqf = .FALSE.
      IF ( fss(1)==rss(1) .AND. fss(2)==rss(2) ) reqf = .TRUE.
      reigen = .FALSE.
      malcom = 0
      IF ( ua==scr1 ) THEN
         scr2 = 301
         scr1 = 302
      ENDIF
!
!     CHECK THE DISPLACEMENT MATRIX
!
      mcbb(1) = ua
      CALL rdtrl(mcbb)
      IF ( mcbb(1)<=0 ) GOTO 800
!
!     BRANCH ON RIGID FORMAT
!
      IF ( rfno>9 ) THEN
         n = 7
         CALL mesage(n,0,name)
         GOTO 800
      ELSE
         IF ( rfno==3 ) THEN
!
!     NORMAL MODES
!
!     CHECK IF THE EIGEN VECTORS ARE COMPLEX
!
            IF ( mcbb(5)>=3 ) THEN
!
!     COMPLEX NORMAL MODES
!
!     Q = KU + BV + MA
!
!     CALCULATE THE COMPLEX VELOCITIES AND ACCLERATION VECTORS FOR
!     THE EIGENVECTORS
!
               in = ua
!
!     SEE MALCOM TAGG RECOMMENDATION, 25 LINES ABOVE
!
               CALL sofcls
               IF ( malcom==1 ) GOTO 100
!
               CALL rcovva(in,0,0,scr6,scr7,scr8,rss,z(1),z(1),z(1))
               IF ( in<=0 ) GOTO 800
!
!     INDICATE ZERO LOAD VECTOR FOR NORMAL MODES
!
               mcbc(1) = 0
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
                  in = ua
                  CALL rcovva(in,0,0,0,0,scr8,rss,z(1),z(1),z(1))
                  IF ( in<=0 ) GOTO 800
               ENDIF
!
!
!     INDICATE A POSITIVE SIGN ON THE M * A MULTIPLY
!
               signab = 1
               mcbc(1) = 0
               IF ( malcom==1 ) GOTO 600
            ENDIF
         ELSEIF ( rfno==4 .OR. rfno==5 .OR. rfno==6 .OR. rfno==7 ) THEN
            n = 7
            CALL mesage(n,0,name)
            GOTO 800
         ELSEIF ( rfno==8 .OR. rfno==9 ) THEN
!
!     DYNAMIC ANALYSIS
!
!     Q = KU + BV + MA - P
!
!
!     SPLIT DISPLACEMENT, VELOCITIES AND ACCELERATIONS ONTO SEPERATE
!     FILES
!
            in = ua
            CALL rcovva(in,1,0,scr6,scr7,scr8,rss,z(1),z(1),z(1))
            IF ( in<=0 ) GOTO 800
!
!     SETUP TO SUBTRACT LOAD VECTOR
!
            signc = -1
            mcbc(1) = pa
            IF ( pa>0 ) CALL rdtrl(mcbc)
         ELSE
!
!     STATIC SOUTION
!
!     Q = KU - P
!
!     SET UP LOAD VECTOR FOR SUBSTRACTION
!
            signc = -1
            mcbc(1) = pa
            IF ( pa>0 ) CALL rdtrl(mcbc)
            GOTO 600
         ENDIF
!
!     COMMON PROCESSING FOR DYNAMICS AND NORMAL MODES
!
!
!     MULTIPLY AND ADD    SCR1 = MA - P
!
         IF ( reqf ) THEN
            mcba(1) = mgg
            CALL rdtrl(mcba)
            IF ( mcba(1)>0 ) GOTO 50
         ENDIF
         CALL mtrxi(scr4,rss,mmtx,0,rc)
         IF ( rc/=1 ) GOTO 200
         mcba(1) = scr4
         CALL rdtrl(mcba)
      ENDIF
 50   mcbb(1) = scr8
      CALL rdtrl(mcbb)
      CALL makmcb(mcbd,scr1,mcbb(3),mcbb(4),mcbb(5))
      CALL sofcls
!
      CALL mpyad(z(1),z(1),z(1))
!
 100  DO i = 1 , 7
         mcbc(i) = mcbd(i)
      ENDDO
      signc = 1
      CALL sofopn(z(sof1),z(sof2),z(sof3))
   ELSE
      qa = scr4
      RETURN
   ENDIF
!
!     MULTIPLY AND ADD   SCR8 = K4V + MCBC
!
 200  IF ( reigen .OR. rfno==9 ) GOTO 400
   IF ( reqf ) THEN
      mcba(1) = k4gg
      CALL rdtrl(mcba)
      IF ( mcba(1)>0 ) GOTO 300
   ENDIF
   CALL mtrxi(scr4,rss,k4mx,0,rc)
   IF ( rc/=1 ) GOTO 400
   mcba(1) = scr4
   CALL rdtrl(mcba)
 300  mcbb(1) = scr7
   CALL rdtrl(mcbb)
   CALL makmcb(mcbd,scr8,mcbb(3),mcbb(4),mcbb(5))
   signab = 1
   CALL sofcls
!
   CALL mpyad(z(1),z(1),z(1))
!
   DO i = 1 , 7
      mcbc(i) = mcbd(i)
   ENDDO
   signc = 1
   CALL sofopn(z(sof1),z(sof2),z(sof3))
 400  IF ( reigen ) GOTO 600
!
!     MULTIPLY AND ADD   SCR1 = BV + MCBC
!
   IF ( reqf ) THEN
      mcba(1) = bgg
      CALL rdtrl(mcba)
      IF ( mcba(1)>0 ) GOTO 500
   ENDIF
   CALL mtrxi(scr4,rss,bmtx,0,rc)
   IF ( rc/=1 ) GOTO 600
   mcba(1) = scr4
   CALL rdtrl(mcba)
 500  mcbb(1) = scr7
   CALL rdtrl(mcbb)
   CALL makmcb(mcbd,scr1,mcbb(3),mcbb(4),mcbb(5))
   signab = 1
   CALL sofcls
!
   CALL mpyad(z(1),z(1),z(1))
!
   DO i = 1 , 7
      mcbc(i) = mcbd(i)
   ENDDO
   signc = 1
   CALL sofopn(z(sof1),z(sof2),z(sof3))
!
!     COMMON PROCESSING FOR ALL RIGID FORMATS
!
!
!     MULTIPLY AND ADD  Q = KU + MCBC
!
 600  IF ( reqf ) THEN
      mcba(1) = kgg
      CALL rdtrl(mcba)
      IF ( mcba(1)>0 ) GOTO 700
   ENDIF
   item = kmtx
   file = scr7
   CALL mtrxi(scr7,rss,kmtx,0,rc)
   IF ( rc/=1 ) THEN
!
!     ERRORS
!
      IF ( rc==6 ) THEN
         CALL mesage(n,0,name)
      ELSE
         CALL smsg(rc-2,item,rss)
      ENDIF
      GOTO 800
   ELSE
      mcba(1) = scr7
      CALL rdtrl(mcba)
   ENDIF
 700  mcbb(1) = scr6
   IF ( reigen .OR. rfno<=2 ) mcbb(1) = ua
   CALL rdtrl(mcbb)
   CALL makmcb(mcbd,scr4,mcbb(3),mcbb(4),mcbb(5))
   signab = 1
   CALL sofcls
!
   CALL mpyad(z(1),z(1),z(1))
!
   CALL wrttrl(mcbd)
   CALL sofopn(z(sof1),z(sof2),z(sof3))
!
!     COPY REACTIONS TO SOF
!
   CALL mtrxo(scr4,rss,qvec,0,rc)
   qa = scr4
!
   RETURN
 800  qa = 0
   WRITE (nout,99001) swm
99001 FORMAT (A27,' 6318, OUTPUT REQUEST FOR REACTIONS FORCES IGNORED.')
!
END SUBROUTINE rcovqv
