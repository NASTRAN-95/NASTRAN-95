
SUBROUTINE outpt1
!
!     COPY DATA BLOCK(S) ONTO NASTRAN USER TAPE WHICH MUST BE SET-UP.
!
!     CALL TO THIS MODULE IS
!
!     OUTPUT1   IN1,IN2,IN3,IN4,IN5//V,N,P1/V,N,P2/V,N,P3 $
!
!               P1 = 0, NO ACTION TAKEN BEFORE WRITE (DEFAULT)
!                  =+N, SKIP FORWARD N DATA BLOCKS BEFORE WRITE
!                  =-1, USER TAPE IS REWOUND BEFORE WRITE
!                  =-2, A NEW REEL IS MOUNTED BEFORE WRITE
!                  =-3, THE NAMES OF ALL DATA BLOCKS ON USER TAPE ARE
!                       PRINTED AND WRITE OCCURS AT THE END OF TAPE
!                  =-4, AN INPUT TAPE IS TO BE DISMOUNTED.
!                       A NEW OUTPUT REEL WILL THEN BE MOUNTED.
!                  =-9, WRITE EOF, REWIND AND UNLOAD.
!
!               P2 = 0, FILE NAME IS INPT (DEFAULT)
!                  = 1, FILE NAME IS INP1
!                  = 2, FILE NAME IS INP2
!                  = 3, FILE NAME IS INP3
!                  = 4, FILE NAME IS INP4
!                  = 5, FILE NAME IS INP5
!                  = 6, FILE NAME IS INP6
!                  = 7, FILE NAME IS INP7
!                  = 8, FILE NAME IS INP8
!                  = 9, FILE NAME IS INP9
!
!               P3 = TAPE ID CODE FOR USER TAPE, AN ALPHANUMERIC
!                    VARIABLE WHOSE VALUE WILL BE WRITTEN ON A USER
!                    TAPE. THE WRITTING OF THIS ITEM IS DEPENDENT ON
!                    THE VALUE OF P1 AS FOLLOWS..
!                          *P1*             *TAPE ID WRITTEN*
!                           +N                     NO
!                            0                     NO
!                           -1                    YES
!                           -2                    YES (ON NEW REEL)
!                           -3                     NO (WARNING CHECK)
!                           -4                    YES (ON NEW REEL)
!                           -9                     NO
!                    DEFAULT VALUE FOR P3 IS XXXXXXXX
!
!
   IMPLICIT NONE
   INTEGER D(3) , Ksystm(65) , Line , Mach , Nb , Nlpp , Nout , P1 , P2 , P3(2) , X(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / P1 , P2 , P3
   COMMON /machin/ Mach
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ X
   INTEGER dx(3) , i , idhdr(7) , idhdrx(7) , ii , in(5) , inbuf , input , iold , kf , lcor , mfor , mm , mnin , mone , mtre ,      &
         & mtwo , name(2) , namex(2) , nf , ott(10) , oubuf , out , p3x(2) , subnam(2) , tapcod(2) , trl(7) , zero
   INTEGER korsz
   LOGICAL tapbit
   LOGICAL tapeup
   EQUIVALENCE (Ksystm(1),Nb) , (Ksystm(2),Nout) , (Ksystm(9),Nlpp) , (Ksystm(12),Line) , (Ksystm(15),D(1))
   DATA subnam/4HOUTP , 4HT1  /
   DATA in/101 , 102 , 103 , 104 , 105/
   DATA zero , mone , mtwo , mtre , mfor , mnin/0 , -1 , -2 , -3 , -4 , -9/
   DATA ott/4HINPT , 4HINP1 , 4HINP2 , 4HINP3 , 4HINP4 , 4HINP5 , 4HINP6 , 4HINP7 , 4HINP8 , 4HINP9/
   DATA idhdr/4HNAST , 4HRAN  , 4HUSER , 4H TAP , 4HE ID , 4H COD , 4HE - /
!
!
   lcor = korsz(X) - 2*Nb
   IF ( lcor<=0 ) THEN
      mm = -8
      input = -lcor
      CALL mesage(mm,input,subnam)
      GOTO 99999
   ELSE
      inbuf = lcor + 1
      oubuf = inbuf + Nb
      tapcod(1) = P3(1)
      tapcod(2) = P3(2)
      IF ( P2<0 .OR. P2>9 ) THEN
         WRITE (Nout,99001) Ufm , P2
99001    FORMAT (A23,' 4119, MODULE OUTPUT1 - ILLEGAL VALUE FOR SECOND ','PARAMETER =',I20)
         Line = Line + 2
!
         mm = -37
         CALL mesage(mm,input,subnam)
         GOTO 99999
      ELSE
         out = ott(P2+1)
         IF ( Mach<5 ) THEN
            tapeup = tapbit(out)
            IF ( .NOT.tapeup ) THEN
               WRITE (Nout,99002) Ufm , out
99002          FORMAT (A23,' 4127, USER TAPE ',A4,' NOT SET UP.')
               Line = Line + 2
               mm = -37
               CALL mesage(mm,input,subnam)
               GOTO 99999
            ENDIF
         ENDIF
         IF ( P1<mnin ) GOTO 700
         IF ( P1>mnin .AND. P1<mfor ) GOTO 700
!
         IF ( P1==mnin ) THEN
!
            CALL eof(out)
            CALL unload(out)
            RETURN
         ELSEIF ( P1==mtre ) THEN
!
!     OBTAIN LIST OF DATA BLOCKS ON USER TAPE.
!
            CALL open(*500,out,X(oubuf),0)
            CALL read(*800,*900,out,dx,3,0,nf)
            CALL read(*800,*900,out,idhdrx,7,0,nf)
            DO kf = 1 , 7
               IF ( idhdrx(kf)/=idhdr(kf) ) GOTO 1000
            ENDDO
            CALL read(*800,*900,out,p3x,2,1,nf)
            IF ( p3x(1)/=P3(1) .OR. p3x(2)/=P3(2) ) THEN
               WRITE (Nout,99003) Uwm , p3x , P3
99003          FORMAT (A25,' 4131, USER TAPE ID CODE -',2A4,'- DOES NOT MATCH ','THIRD OUTPUT1 DMAP PARAMETER -',2A4,2H-.)
               Line = Line + 2
            ENDIF
            CALL skpfil(out,1)
            kf = 0
            GOTO 200
         ELSE
            IF ( P1>zero ) THEN
!
               CALL gopen(out,X(oubuf),2)
               DO i = 1 , P1
                  CALL read(*600,*600,out,namex,2,1,nf)
                  CALL skpfil(out,1)
               ENDDO
               CALL close(out,2)
!
            ELSEIF ( P1==mtwo .OR. P1==mfor ) THEN
!
!     P1 = -2 OR P1 = -4 IS ACCEPTABLE ONLY ON IBM OR UNIVAC
!
               IF ( Mach/=2 .AND. Mach/=3 ) GOTO 700
!
               iold = 3 + P1/2
               CALL gopen(out,X(oubuf),3)
               CALL tpswit(out,iold,2,tapcod)
            ENDIF
!
!     OPEN USER TAPE TO WRITE WITHOUT REWIND
!
            CALL gopen(out,X(oubuf),3)
            IF ( P1==mone .OR. P1==mtwo .OR. P1==mfor ) THEN
               CALL rewind(out)
               CALL write(out,D,3,0)
               CALL write(out,idhdr,7,0)
               CALL write(out,P3,2,1)
               CALL eof(out)
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
 100  DO i = 1 , 5
      input = in(i)
      trl(1) = input
      CALL rdtrl(trl)
      IF ( trl(1)>0 ) THEN
         CALL fname(input,name)
!
!     OPEN INPUT DATA BLOCK TO READ WITH REWIND.
!
         CALL open(*400,input,X(inbuf),0)
         CALL write(out,name,2,0)
         CALL write(out,trl(2),6,1)
!
!     LEVEL 17.5, THE ABOVE 8 WORD RECORD WAS WRITTEN OUT IN 2 RECORDS
!     2 BCD WORD NAME, AND 7 TRAILER WORDS
!
!     COPY CONTENTS OF INPUT DATA BLOCK ONTO USER TAPE.
!
         CALL cpyfil(input,out,X,lcor,nf)
!
!     CLOSE INPUT DATA BLOCK WITH REWIND
!
         CALL close(input,1)
!
         CALL eof(out)
         CALL page2(-4)
         WRITE (Nout,99004) Uim , name , out , (trl(ii),ii=2,7)
99004    FORMAT (A29,' 4114',//5X,'DATA BLOCK ',2A4,' WRITTEN ON NASTRAN FILE ',A4,', TRLR  =',6I10)
      ENDIF
!
   ENDDO
!
!     CLOSE NASTRAN USER TAPE WITHOUT REWIND, BUT WITH END-OF-FILE
!
   CALL close(out,3)
   RETURN
 200  CALL page1
   Line = Line + 5
   WRITE (Nout,99005) out
99005 FORMAT (//50X,A4,14H FILE CONTENTS,/46X,4HFILE,18X,4HNAME,//)
   DO
      CALL read(*300,*1100,out,namex,2,1,nf)
      CALL skpfil(out,1)
      kf = kf + 1
      Line = Line + 1
      WRITE (Nout,99006) kf , namex
99006 FORMAT (45X,I5,18X,2A4)
      IF ( Line>=Nlpp ) GOTO 200
   ENDDO
 300  CALL skpfil(out,-1)
!
   CALL close(out,2)
   CALL gopen(out,X(oubuf),3)
   GOTO 100
!
!     ERRORS
!
 400  mm = -1
   CALL mesage(mm,input,subnam)
   GOTO 99999
 500  WRITE (Nout,99007) Sfm , out
99007 FORMAT (A25,' 4117, SUBROUTINE OUTPT1 UNABLE TO OPEN NASTRAN FILE',A4,1H.)
   Line = Line + 2
   mm = -37
   CALL mesage(mm,input,subnam)
   GOTO 99999
 600  WRITE (Nout,99008) Ufm , P1 , out , i
99008 FORMAT (A23,' 4118, MODULE OUTPUT1 IS UNABLE TO SKIP FORWARD',I10,' DATA BLOCKS ON PERMANENT NASTRAN FILE ',A4,1H.,/5X,       &
             &'NUMBER OF DATA BLOCKS SKIPPED =',I6)
   Line = Line + 3
   mm = -37
   CALL mesage(mm,input,subnam)
   GOTO 99999
 700  WRITE (Nout,99009) Ufm , P1
99009 FORMAT (A23,' 4120, MODULE OUTPUT1 - ILLEGAL VALUE FOR FIRST ','PARAMETER =',I20)
   Line = Line + 2
   mm = -37
   CALL mesage(mm,input,subnam)
   GOTO 99999
 800  WRITE (Nout,99010) Ufm , out
99010 FORMAT (A23,' 4128, MODULE OUTPUT1 - END-OF-FILE ENCOUNTERED ','WHILE ATTEMPTING TO READ TAPE ID CODE ON USER TAPE ',A4)
   Line = Line + 2
   mm = -37
   CALL mesage(mm,input,subnam)
   GOTO 99999
 900  WRITE (Nout,99011) Ufm , out
99011 FORMAT (A23,' 4129, MODULE OUTPUT1 - END-OF-RECORD ENCOUNTERED ','WHILE ATTEMPTING TO READ TAPE ID CODE ON USER TAPE ',A4)
   Line = Line + 2
   mm = -37
   CALL mesage(mm,input,subnam)
   GOTO 99999
 1000 WRITE (Nout,99012) Ufm , (idhdrx(kf),kf=1,7)
99012 FORMAT (A23,' 4130, MODULE OUTPUT1 - ILLEGAL TAPE CODE HEADER = ',7A4)
   Line = Line + 2
   mm = -37
   CALL mesage(mm,input,subnam)
   GOTO 99999
 1100 WRITE (Nout,99013) Sfm
99013 FORMAT (A25,' 4115, MODULE OUTPUT1 - SHORT RECORD.')
   Line = Line + 2
   mm = -37
   CALL mesage(mm,input,subnam)
!
99999 RETURN
END SUBROUTINE outpt1
