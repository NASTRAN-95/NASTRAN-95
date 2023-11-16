
SUBROUTINE tabfmt
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER H1(32) , H2(32) , H3(32) , Hx(32,40) , Ix(1) , Junk1(6) , Junk2(2) , La , Line , Na(2,21) , Nb , Nlpp , No , P(2) , P2 , &
         & P3 , Re(21) , X(14)
   REAL Rx(14) , T1(32) , T2(32) , T3(32)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / P , P2 , P3
   COMMON /output/ T1 , T2 , T3 , H1 , H2 , H3
   COMMON /system/ Nb , No , Junk1 , Nlpp , Junk2 , Line
   COMMON /tabftx/ La , Na , Hx , Re
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Ix
!
! Local variable declarations
!
   INTEGER a , b , eid , f , i , ib , ival , j , kf , l , lc , ls , m , m1 , m2 , m3 , nam(2) , name(2) , none(2) , one , r , rl ,  &
         & subnam(2) , t(7) , two , wd , y , z(2) , zero
   INTEGER korsz
   REAL pi
!
! End of declarations
!
!
!     MODULE MAIN PROGRAM FOR DMAP MODULE TABPRT
!
!     THE CALL TO THIS MODULE IS
!
!     TABPRT TDB // C,N,KEY / C,N,OPT1 / C,N,OPT2 $
!
!            TDB IS THE TABLE DATA BLOCK TO BE PRINTED.
!
!            KEY IS THE BCD VALUE WHICH DETERMINES THE FORMAT BY
!                 WHICH THE TABLE IS PRINTED.
!                 THERE IS NO DEFAULT VALUE FOR KEY.
!
!            OPT1 IS A SKIP FACTOR BETWEEN DATA LINES.
!                 OPT1.EQ.0 MEANS  NO SPACE BETWEEN DATA LINES.
!                 OPT1.NE.0 MEANS ONE SPACE BETWEEN DATA LINES.
!                 THE DEFAULT VALUE FOR OPT1 IS 0
!
!            OPT2 IS ZERO BY DEFAULT.
!                 SKIP FILE-NAME AND KEY CHECKING IF OPT2 IS NON-ZERO.
!
   EQUIVALENCE (Rx(1),X(1),Ix(1))
   DATA none/4H (NO , 4HNE) /
   DATA subnam/4HTABF , 4HMT  /
   DATA f/101/
   DATA zero/4H  0 / , one/4H  1 / , two/4H  2 /
!
   lc = korsz(X) - Nb
   ib = lc + 1
   IF ( lc<=0 ) CALL mesage(-8,lc,subnam)
   ls = 1
   IF ( P2/=0 ) ls = 2
!
   DO i = 1 , La
      IF ( P(1)==Na(1,i) .AND. P(2)==Na(2,i) ) GOTO 100
   ENDDO
   GOTO 3100
!
 100  CALL fname(f,name)
   IF ( name(1)==none(1) .AND. name(2)==none(2) ) GOTO 3200
   t(1) = f
   CALL rdtrl(t)
   IF ( t(1)<=0 ) GOTO 3200
   CALL open(*3200,f,X(ib),0)
   CALL read(*3300,*3400,f,nam,2,Re(i),kf)
   IF ( nam(1)/=P(1) .OR. nam(2)/=P(2) ) THEN
      IF ( P3==0 ) GOTO 3100
   ENDIF
!
   IF ( i==2 ) GOTO 400
   IF ( i==3 ) THEN
!
!     PRINT CONTENTS OF TABLE DATA BLOCK CSTM
!
      m1 = 2
      m2 = 9
      m3 = 10
      ASSIGN 900 TO r
      GOTO 2900
   ELSEIF ( i==4 ) THEN
      GOTO 400
   ELSEIF ( i==5 ) THEN
      GOTO 1100
   ELSEIF ( i==6 ) THEN
      GOTO 1100
   ELSEIF ( i==7 ) THEN
!
!     PRINT CONTENTS OF TABLE DATA BLOCK GPDT
!
      m1 = 2
      m2 = 13
      m3 = 14
      ASSIGN 1600 TO r
      GOTO 2900
   ELSEIF ( i==8 ) THEN
!
!     PRINT CONTENTS OF TABLE DATA BLOCK GPTT
!
!
!     RECORD 0
!
      m1 = 2
      m2 = 15
      m3 = 16
      ASSIGN 1800 TO r
      GOTO 2900
   ELSEIF ( i==9 ) THEN
!
!     PRINT CONTENTS OF TABLE DATA BLOCK GPCT
!
      m1 = 19
      m2 = 20
      m3 = 21
      ASSIGN 2200 TO r
      GOTO 2900
   ELSEIF ( i==10 ) THEN
   ELSEIF ( i==11 ) THEN
   ELSEIF ( i==12 ) THEN
   ELSEIF ( i==13 ) THEN
   ELSEIF ( i==14 ) THEN
   ELSEIF ( i==15 ) THEN
   ELSEIF ( i==16 ) THEN
   ELSEIF ( i==17 ) THEN
   ELSEIF ( i==18 ) THEN
   ELSEIF ( i==19 ) THEN
   ELSEIF ( i==20 ) THEN
   ELSEIF ( i/=21 ) THEN
!
!     PRINT CONTENTS OF TABLE DATA BLOCK BGPDT.
!
      m1 = 2
      m2 = 3
      m3 = 4
      ASSIGN 200 TO r
      GOTO 2900
   ENDIF
   GOTO 3000
 200  H1(19) = P(1)
   H1(20) = P(2)
   H1(24) = one
   IF ( lc<4 ) GOTO 3500
   j = 0
   DO
      CALL read(*3300,*300,f,X,4,0,kf)
      j = j + 1
      Line = Line + ls
      IF ( Line>Nlpp ) THEN
         CALL page
         WRITE (No,99016)
         Line = ls
      ENDIF
      IF ( P2/=0 ) WRITE (No,99016)
      WRITE (No,99001) j , X(1) , (Rx(l),l=2,4)
99001 FORMAT (20X,I10,I13,1X,1P,3E20.5)
   ENDDO
 300  IF ( kf/=0 ) GOTO 3600
   GOTO 3000
!
!     PRINT CONTENTS OF TABLE DATA BLOCK GPL.
!
!
!     RECORD 1
!
 400  m1 = 2
   m2 = 5
   m3 = 6
   ASSIGN 500 TO r
   GOTO 2900
 500  H1(19) = P(1)
   H1(20) = P(2)
   H1(24) = one
   IF ( lc<5 ) GOTO 3500
   j = -4
   DO
      CALL read(*3300,*600,f,X,5,0,kf)
      j = j + 5
      Line = Line + ls
      IF ( Line>Nlpp ) THEN
         CALL page
         WRITE (No,99016)
         Line = ls
      ENDIF
      IF ( P2/=0 ) WRITE (No,99016)
      WRITE (No,99017) j , (X(l),l=1,5)
   ENDDO
 600  IF ( kf/=0 ) THEN
      j = j + 5
      Line = Line + ls
      IF ( Line>Nlpp ) THEN
         CALL page
         WRITE (No,99016)
         Line = ls
      ENDIF
      IF ( P2/=0 ) WRITE (No,99016)
      WRITE (No,99017) j , (X(l),l=1,kf)
   ENDIF
!
!     RECORD 2
!
   IF ( i==4 ) GOTO 3000
   m1 = 2
   m2 = 7
   m3 = 8
   ASSIGN 700 TO r
   GOTO 2900
 700  H1(19) = P(1)
   H1(20) = P(2)
   H1(24) = two
   IF ( lc<6 ) GOTO 3500
   j = -2
   DO
      CALL read(*3300,*800,f,X,6,0,kf)
      j = j + 3
      Line = Line + ls
      IF ( Line>Nlpp ) THEN
         CALL page
         WRITE (No,99016)
         Line = ls
      ENDIF
      IF ( P2/=0 ) WRITE (No,99016)
      WRITE (No,99018) j , (X(l),l=1,6)
   ENDDO
 800  IF ( kf/=0 ) THEN
      j = j + 3
      Line = Line + ls
      IF ( Line>Nlpp ) THEN
         CALL page
         WRITE (No,99016)
         Line = ls
      ENDIF
      IF ( P2/=0 ) WRITE (No,99016)
      WRITE (No,99018) j , (X(l),l=1,kf)
   ENDIF
!
   IF ( mod(kf,2)/=0 ) GOTO 3600
   GOTO 3000
 900  H1(19) = P(1)
   H1(20) = P(2)
   H1(24) = one
   IF ( lc<14 ) GOTO 3500
   j = 0
   DO
      CALL read(*3300,*1000,f,X,14,0,kf)
      j = j + 1
      Line = Line + ls + 2
      IF ( Line>Nlpp ) THEN
         CALL page
         WRITE (No,99016)
         Line = ls + 2
      ENDIF
      IF ( P2/=0 ) WRITE (No,99016)
      WRITE (No,99002) j , X(1) , X(2) , Rx(6) , Rx(7) , Rx(8) , Rx(3) , Rx(9) , Rx(10) , Rx(11) , Rx(4) , Rx(12) , Rx(13) , Rx(14) &
                     & , Rx(5)
99002 FORMAT (10X,I10,I10,I10,1P,3E20.8,10X,1P,E20.8/40X,1P,3E20.8,10X,1P,E20.8/40X,1P,3E20.8,10X,1P,E20.8)
   ENDDO
!
!     PRINT CONTENTS OF TABLE DATA BLOCK GPLD
!
 1000 IF ( kf/=0 ) GOTO 3600
   GOTO 3000
!
!     PRINT CONTENTS OF TABLE DATA BLOCK EQEXIN
!
 1100 m1 = 2
   m2 = 11
   m3 = 12
   ASSIGN 1200 TO r
   GOTO 2900
 1200 H1(19) = P(1)
   H1(20) = P(2)
   H1(24) = one
 1300 IF ( lc<8 ) GOTO 3500
   j = -3
   DO
      CALL read(*3300,*1400,f,X,8,0,kf)
      j = j + 4
      Line = Line + ls
      IF ( Line>Nlpp ) THEN
         CALL page
         WRITE (No,99016)
         Line = ls
      ENDIF
      IF ( P2/=0 ) WRITE (No,99016)
      WRITE (No,99019) j , (X(l),l=1,8)
   ENDDO
 1400 IF ( kf/=0 ) THEN
      j = j + 4
      Line = Line + ls
      IF ( Line>Nlpp ) THEN
         CALL page
         WRITE (No,99016)
         Line = ls
      ENDIF
      IF ( P2/=0 ) WRITE (No,99016)
      WRITE (No,99019) j , (X(l),l=1,kf)
   ENDIF
!
!     RECORD 2
!
   IF ( H1(24)==two ) GOTO 3000
   m1 = 2
   m2 = 11
   m3 = 22
   ASSIGN 1500 TO r
   GOTO 2900
 1500 H1(19) = P(1)
   H1(20) = P(2)
   H1(24) = two
!
!     PRINT CONTENTS OF TABLE DATA BLOCK EQDYN
!
   GOTO 1300
 1600 H1(19) = P(1)
   H1(20) = P(2)
   H1(24) = one
   IF ( lc<7 ) GOTO 3500
   j = 0
   DO
      CALL read(*3300,*1700,f,X,7,0,kf)
      j = j + 1
      Line = Line + ls
      IF ( Line>Nlpp ) THEN
         CALL page
         WRITE (No,99016)
         Line = ls
      ENDIF
      IF ( P2/=0 ) WRITE (No,99016)
      WRITE (No,99003) X(1) , X(2) , Rx(3) , Rx(4) , Rx(5) , X(6) , X(7)
99003 FORMAT (7X,I8,10X,I8,10X,3(1P,E12.5,5X),5X,I8,10X,I8)
   ENDDO
 1700 IF ( kf/=0 ) GOTO 3600
   GOTO 3000
 1800 H1(19) = P(1)
   H1(20) = P(2)
   H1(24) = zero
   IF ( (lc/3)*3/=0 ) THEN
      ival = (lc/3)*3
      CALL read(*3300,*1900,f,X,ival,0,kf)
   ENDIF
   GOTO 3500
 1900 wd = ((kf-1)/3) + 1
   IF ( kf==0 ) wd = ((lc-1)/3) + 1
   DO j = 1 , wd
      Line = Line + ls
      IF ( Line>Nlpp ) THEN
         CALL page
         WRITE (No,99016)
         Line = ls
      ENDIF
      IF ( P2/=0 ) WRITE (No,99016)
      IF ( X(3*j-1)==-1 ) THEN
         WRITE (No,99004) j , X(3*j-2) , X(3*j-1) , X(3*j)
99004    FORMAT (7X,I8,10X,I8,14X,6X,I3,22X,I8)
      ELSE
         WRITE (No,99005) j , X(3*j-2) , Rx(3*j-1) , X(3*j)
99005    FORMAT (7X,I8,10X,I8,14X,1P,E12.5,19X,I8)
      ENDIF
   ENDDO
!
!     RECORD 1 AND ALL OTHERS
!
   m1 = 17
   m2 = 1
   m3 = 18
   ASSIGN 2000 TO r
   GOTO 2900
 2000 DO rl = 1 , j
      IF ( X(3*rl)==0 ) CYCLE
      IF ( (lc-3*wd)<4 ) GOTO 3500
      CALL read(*3300,*3400,f,y,1,0,kf)
      CALL read(*3300,*2100,f,z,2,0,kf)
 2050 DO
         CALL read(*3300,*3400,f,eid,1,0,kf)
!
!     ELEMENT ID EQUALS ZERO INDICATES THE END OF DATA FOR CURRENT TYPE
!
         IF ( (lc-3*wd)<z(2) ) GOTO 3500
!
!     ELEMENT ID LESS THAN ZERO INDICATES NONEXISTENT TEMPERATURE VALUES
!
         IF ( eid<0 ) THEN
         ELSEIF ( eid==0 ) THEN
            CALL read(*3300,*2100,f,z,2,0,kf)
            CYCLE
         ELSE
            ival = 3*wd + 1
            CALL read(*3300,*3400,f,X(ival),z(2),0,kf)
         ENDIF
         a = 3*wd + 1
         b = a + 7
         IF ( b>=(a+z(2)) ) b = a + z(2) - 1
         Line = Line + ls
         IF ( Line>Nlpp ) THEN
            CALL page
            WRITE (No,99020) X(3*rl) , y
            WRITE (No,99006)
99006       FORMAT (14H0   ELEMENT ID,8X,5H( 1 ),9X,5H( 2 ),9X,5H( 3 ),9X,5H( 4 ),9X,5H( 5 ),9X,5H( 6 ),9X,5H( 7 ),9X,5H( 8 ))
            WRITE (No,99016)
            Line = ls + 3
         ENDIF
         IF ( eid<0 ) THEN
            WRITE (No,99021) eid
         ELSEIF ( eid==0 ) THEN
            CALL read(*3300,*2100,f,z,2,0,kf)
         ELSE
            WRITE (No,99021) eid , (Rx(l),l=a,b)
            IF ( P2/=0 ) WRITE (No,99016)
            IF ( b/=(a+z(2)-1) ) THEN
               a = a + 8
               EXIT
            ENDIF
         ENDIF
      ENDDO
      DO
         b = a + 7
         IF ( b>=(a+z(2)) ) b = a + z(2) - 1
         Line = Line + ls
         IF ( Line>Nlpp ) THEN
            CALL page
            WRITE (No,99020) X(3*rl) , y
            WRITE (No,99016)
            Line = ls + 3
         ENDIF
         WRITE (No,99007) (Rx(l),l=a,b)
99007    FORMAT (17X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5)
         IF ( P2/=0 ) WRITE (No,99016)
         IF ( b==(a+z(2)-1) ) GOTO 2050
         a = a + 8
      ENDDO
 2100 ENDDO
   IF ( kf/=0 ) GOTO 3600
   GOTO 3000
 2200 IF ( lc<12 ) GOTO 3500
   j = 0
 2300 CALL read(*2800,*3400,f,pi,1,0,kf)
   j = j + 1
   wd = 10
   Line = Line + ls
   IF ( Line>Nlpp ) THEN
      CALL page
      WRITE (No,99016)
      Line = ls
   ENDIF
   IF ( P2/=0 ) WRITE (No,99016)
   CALL read(*3300,*2700,f,m,1,0,kf)
   CALL read(*3300,*2400,f,X,10,0,kf)
   GOTO 2500
 2400 wd = kf
 2500 WRITE (No,99022) j , pi , m , (X(l),l=1,wd)
   IF ( m<=10 ) GOTO 2300
   DO
      Line = Line + ls
      IF ( Line>Nlpp ) THEN
         CALL page
         WRITE (No,99016)
         Line = ls
      ENDIF
      IF ( P2/=0 ) WRITE (No,99016)
      CALL read(*3300,*2600,f,X,10,0,kf)
      WRITE (No,99023) (X(l),l=1,wd)
   ENDDO
 2600 wd = kf
   WRITE (No,99023) (X(l),l=1,wd)
   GOTO 2300
 2700 m = 0
   WRITE (No,99022) pi , m
   GOTO 2300
!
!     PRINT CONTENTS OF
!
!
!
!
!
!
!
!
!
!
!
!
 2800 IF ( j/=0 ) GOTO 3000
   GOTO 3300
!
!
!     INTERNAL ROUTINE TO SET HEADINGS AND INITIALIZE LINE COUNTER.
!     -------------------------------------------------------------
!
 2900 DO m = 1 , 32
      H1(m) = Hx(m,m1)
      H2(m) = Hx(m,m2)
      H3(m) = Hx(m,m3)
   ENDDO
   Line = Nlpp
   GOTO r
!
!
!     PRINT TRAILER OF TABLE DATA BLOCK
!     ---------------------------------
!
 3000 WRITE (No,99008) (t(l),l=2,7)
99008 FORMAT (15H0*** TRAILER = ,6I18)
!
!
!     DO NOT CALL PEXIT SINCE THIS IS AN OUTPUT PROCESSOR.
!
   CALL close(f,1)
   GOTO 99999
!
!
 3100 WRITE (No,99009) Uwm , P
99009 FORMAT (A25,' 2094, SUBROUTINE TABFMT, KEYNAME ',2A4,' NOT IN LIST OF AVAILABLE KEYNAMES.')
!
   WRITE (No,99010) (Na(1,l),Na(2,l),l=1,La)
99010 FORMAT ('0*** LIST OF RECOGNIZED KEYNAMES FOLLOWS...',/(20X,2A4))
   CALL close(f,1)
   GOTO 99999
!
 3200 WRITE (No,99011) Uwm
99011 FORMAT (A25,' 2095, SUBROUTINE TABFMT, PURGED INPUT.')
   CALL close(f,1)
   GOTO 99999
!
 3300 WRITE (No,99012) Uwm
99012 FORMAT (A25,' 2096, SUBROUTINE TABFMT, EOF ENCOUNTERED.')
   CALL close(f,1)
   GOTO 99999
!
 3400 WRITE (No,99013) Uwm
99013 FORMAT (A25,' 2097, SUBROUTINE TABFMT, EOR ENCOUNTERED.')
   CALL close(f,1)
   GOTO 99999
!
 3500 WRITE (No,99014) Uwm
99014 FORMAT (A25,' 2098, SUBROUTINE TABFMT, INSUFFICIENT CORE.')
   CALL close(f,1)
   GOTO 99999
!
 3600 WRITE (No,99015) Uwm , kf
99015 FORMAT (A25,' 2099, SUBROUTINE TABFMT, KF =',I10)
   CALL close(f,1)
!
!
99016 FORMAT (1H )
99017 FORMAT (11X,I8,5(9X,I8,3X))
99018 FORMAT (11X,I8,2X,3(9X,I8,4X,I12))
99019 FORMAT (7X,I8,4(7X,I8,6X,I8))
99020 FORMAT (9X,I8,11X,I8,11X,I8,13X,I8)
99021 FORMAT (4X,I8,3X,8(2X,1P,E12.5))
99022 FORMAT (3X,I8,12(2X,I8))
99023 FORMAT (31X,10(2X,I8))
!
99999 RETURN
END SUBROUTINE tabfmt
