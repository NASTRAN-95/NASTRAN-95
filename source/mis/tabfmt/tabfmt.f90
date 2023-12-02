!*==tabfmt.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tabfmt
   IMPLICIT NONE
   USE C_BLANK
   USE C_OUTPUT
   USE C_SYSTEM
   USE C_TABFTX
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: a , b , eid , i , ib , ival , j , kf , l , lc , ls , m , m1 , m2 , m3 , r , rl , wd , y
   INTEGER , SAVE :: f , one , two , zero
   INTEGER , DIMENSION(2) :: nam , name , z
   INTEGER , DIMENSION(2) , SAVE :: none , subnam
   REAL :: pi
   REAL , DIMENSION(14) :: rx
   INTEGER , DIMENSION(7) :: t
   INTEGER , DIMENSION(14) :: x
   EXTERNAL close , fname , korsz , mesage , open , page , rdtrl , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
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
   !>>>>EQUIVALENCE (Rx(1),X(1),Ix(1))
   DATA none/4H (NO , 4HNE) /
   DATA subnam/4HTABF , 4HMT  /
   DATA f/101/
   DATA zero/4H  0 / , one/4H  1 / , two/4H  2 /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         lc = korsz(x) - Nb
         ib = lc + 1
         IF ( lc<=0 ) CALL mesage(-8,lc,subnam)
         ls = 1
         IF ( P2/=0 ) ls = 2
!
         DO i = 1 , La
            IF ( P(1)==Na(1,i) .AND. P(2)==Na(2,i) ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
!
         CALL fname(f,name)
         IF ( name(1)==none(1) .AND. name(2)==none(2) ) GOTO 460
         t(1) = f
         CALL rdtrl(t)
         IF ( t(1)<=0 ) GOTO 460
         CALL open(*460,f,x(ib),0)
         CALL read(*480,*500,f,nam,2,Re(i),kf)
         IF ( nam(1)/=P(1) .OR. nam(2)/=P(2) ) THEN
            IF ( P3==0 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
         IF ( i==2 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( i==3 ) THEN
!
!     PRINT CONTENTS OF TABLE DATA BLOCK CSTM
!
            m1 = 2
            m2 = 9
            m3 = 10
            ASSIGN 140 TO r
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==4 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==5 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==6 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==7 ) THEN
!
!     PRINT CONTENTS OF TABLE DATA BLOCK GPDT
!
            m1 = 2
            m2 = 13
            m3 = 14
            ASSIGN 240 TO r
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
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
            ASSIGN 280 TO r
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==9 ) THEN
!
!     PRINT CONTENTS OF TABLE DATA BLOCK GPCT
!
            m1 = 19
            m2 = 20
            m3 = 21
            ASSIGN 360 TO r
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
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
            ASSIGN 20 TO r
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 20      H1(19) = P(1)
         H1(20) = P(2)
         H1(24) = one
         IF ( lc<4 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = 0
         DO
            CALL read(*480,*40,f,x,4,0,kf)
            j = j + 1
            Line = Line + ls
            IF ( Line>Nlpp ) THEN
               CALL page
               WRITE (No,99016)
               Line = ls
            ENDIF
            IF ( P2/=0 ) WRITE (No,99016)
            WRITE (No,99001) j , x(1) , (rx(l),l=2,4)
99001       FORMAT (20X,I10,I13,1X,1P,3E20.5)
         ENDDO
 40      IF ( kf==0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
!     PRINT CONTENTS OF TABLE DATA BLOCK GPL.
!
!
!     RECORD 1
!
         m1 = 2
         m2 = 5
         m3 = 6
         ASSIGN 60 TO r
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 60      H1(19) = P(1)
         H1(20) = P(2)
         H1(24) = one
         IF ( lc<5 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = -4
         DO
            CALL read(*480,*80,f,x,5,0,kf)
            j = j + 5
            Line = Line + ls
            IF ( Line>Nlpp ) THEN
               CALL page
               WRITE (No,99016)
               Line = ls
            ENDIF
            IF ( P2/=0 ) WRITE (No,99016)
            WRITE (No,99017) j , (x(l),l=1,5)
         ENDDO
 80      IF ( kf/=0 ) THEN
            j = j + 5
            Line = Line + ls
            IF ( Line>Nlpp ) THEN
               CALL page
               WRITE (No,99016)
               Line = ls
            ENDIF
            IF ( P2/=0 ) WRITE (No,99016)
            WRITE (No,99017) j , (x(l),l=1,kf)
         ENDIF
!
!     RECORD 2
!
         IF ( i==4 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         m1 = 2
         m2 = 7
         m3 = 8
         ASSIGN 100 TO r
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 100     H1(19) = P(1)
         H1(20) = P(2)
         H1(24) = two
         IF ( lc<6 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = -2
         DO
            CALL read(*480,*120,f,x,6,0,kf)
            j = j + 3
            Line = Line + ls
            IF ( Line>Nlpp ) THEN
               CALL page
               WRITE (No,99016)
               Line = ls
            ENDIF
            IF ( P2/=0 ) WRITE (No,99016)
            WRITE (No,99018) j , (x(l),l=1,6)
         ENDDO
 120     IF ( kf/=0 ) THEN
            j = j + 3
            Line = Line + ls
            IF ( Line>Nlpp ) THEN
               CALL page
               WRITE (No,99016)
               Line = ls
            ENDIF
            IF ( P2/=0 ) WRITE (No,99016)
            WRITE (No,99018) j , (x(l),l=1,kf)
         ENDIF
!
         IF ( mod(kf,2)==0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 140     H1(19) = P(1)
         H1(20) = P(2)
         H1(24) = one
         IF ( lc<14 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = 0
         DO
            CALL read(*480,*160,f,x,14,0,kf)
            j = j + 1
            Line = Line + ls + 2
            IF ( Line>Nlpp ) THEN
               CALL page
               WRITE (No,99016)
               Line = ls + 2
            ENDIF
            IF ( P2/=0 ) WRITE (No,99016)
            WRITE (No,99002) j , x(1) , x(2) , rx(6) , rx(7) , rx(8) , rx(3) , rx(9) , rx(10) , rx(11) , rx(4) , rx(12) , rx(13) ,  &
                           & rx(14) , rx(5)
99002       FORMAT (10X,I10,I10,I10,1P,3E20.8,10X,1P,E20.8/40X,1P,3E20.8,10X,1P,E20.8/40X,1P,3E20.8,10X,1P,E20.8)
         ENDDO
!
!     PRINT CONTENTS OF TABLE DATA BLOCK GPLD
!
 160     IF ( kf==0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!     PRINT CONTENTS OF TABLE DATA BLOCK EQEXIN
!
         m1 = 2
         m2 = 11
         m3 = 12
         ASSIGN 180 TO r
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 180     H1(19) = P(1)
         H1(20) = P(2)
         H1(24) = one
         spag_nextblock_1 = 5
      CASE (5)
         IF ( lc<8 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = -3
         DO
            CALL read(*480,*200,f,x,8,0,kf)
            j = j + 4
            Line = Line + ls
            IF ( Line>Nlpp ) THEN
               CALL page
               WRITE (No,99016)
               Line = ls
            ENDIF
            IF ( P2/=0 ) WRITE (No,99016)
            WRITE (No,99019) j , (x(l),l=1,8)
         ENDDO
 200     IF ( kf/=0 ) THEN
            j = j + 4
            Line = Line + ls
            IF ( Line>Nlpp ) THEN
               CALL page
               WRITE (No,99016)
               Line = ls
            ENDIF
            IF ( P2/=0 ) WRITE (No,99016)
            WRITE (No,99019) j , (x(l),l=1,kf)
         ENDIF
!
!     RECORD 2
!
         IF ( H1(24)==two ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         m1 = 2
         m2 = 11
         m3 = 22
         ASSIGN 220 TO r
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 220     H1(19) = P(1)
         H1(20) = P(2)
!
!     PRINT CONTENTS OF TABLE DATA BLOCK EQDYN
!
         H1(24) = two
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 240     H1(19) = P(1)
         H1(20) = P(2)
         H1(24) = one
         IF ( lc<7 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = 0
         DO
            CALL read(*480,*260,f,x,7,0,kf)
            j = j + 1
            Line = Line + ls
            IF ( Line>Nlpp ) THEN
               CALL page
               WRITE (No,99016)
               Line = ls
            ENDIF
            IF ( P2/=0 ) WRITE (No,99016)
            WRITE (No,99003) x(1) , x(2) , rx(3) , rx(4) , rx(5) , x(6) , x(7)
99003       FORMAT (7X,I8,10X,I8,10X,3(1P,E12.5,5X),5X,I8,10X,I8)
         ENDDO
 260     IF ( kf==0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 280     H1(19) = P(1)
         H1(20) = P(2)
         H1(24) = zero
         IF ( (lc/3)*3/=0 ) THEN
            ival = (lc/3)*3
            CALL read(*480,*300,f,x,ival,0,kf)
         ENDIF
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 300     wd = ((kf-1)/3) + 1
         IF ( kf==0 ) wd = ((lc-1)/3) + 1
         DO j = 1 , wd
            Line = Line + ls
            IF ( Line>Nlpp ) THEN
               CALL page
               WRITE (No,99016)
               Line = ls
            ENDIF
            IF ( P2/=0 ) WRITE (No,99016)
            IF ( x(3*j-1)==-1 ) THEN
               WRITE (No,99004) j , x(3*j-2) , x(3*j-1) , x(3*j)
99004          FORMAT (7X,I8,10X,I8,14X,6X,I3,22X,I8)
            ELSE
               WRITE (No,99005) j , x(3*j-2) , rx(3*j-1) , x(3*j)
99005          FORMAT (7X,I8,10X,I8,14X,1P,E12.5,19X,I8)
            ENDIF
         ENDDO
!
!     RECORD 1 AND ALL OTHERS
!
         m1 = 17
         m2 = 1
         m3 = 18
         ASSIGN 320 TO r
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 320     DO rl = 1 , j
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  IF ( x(3*rl)==0 ) CYCLE
                  IF ( (lc-3*wd)<4 ) THEN
                     spag_nextblock_1 = 11
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL read(*480,*500,f,y,1,0,kf)
                  CALL read(*480,*340,f,z,2,0,kf)
                  spag_nextblock_2 = 2
               CASE (2)
                  SPAG_Loop_2_1: DO
                     CALL read(*480,*500,f,eid,1,0,kf)
!
!     ELEMENT ID EQUALS ZERO INDICATES THE END OF DATA FOR CURRENT TYPE
!
                     IF ( (lc-3*wd)<z(2) ) THEN
                        spag_nextblock_1 = 11
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
!     ELEMENT ID LESS THAN ZERO INDICATES NONEXISTENT TEMPERATURE VALUES
!
                     IF ( eid<0 ) THEN
                     ELSEIF ( eid==0 ) THEN
                        CALL read(*480,*340,f,z,2,0,kf)
                        CYCLE
                     ELSE
                        ival = 3*wd + 1
                        CALL read(*480,*500,f,x(ival),z(2),0,kf)
                     ENDIF
                     a = 3*wd + 1
                     b = a + 7
                     IF ( b>=(a+z(2)) ) b = a + z(2) - 1
                     Line = Line + ls
                     IF ( Line>Nlpp ) THEN
                        CALL page
                        WRITE (No,99020) x(3*rl) , y
                        WRITE (No,99006)
99006                   FORMAT (14H0   ELEMENT ID,8X,5H( 1 ),9X,5H( 2 ),9X,5H( 3 ),9X,5H( 4 ),9X,5H( 5 ),9X,5H( 6 ),9X,5H( 7 ),9X,  &
                               &5H( 8 ))
                        WRITE (No,99016)
                        Line = ls + 3
                     ENDIF
                     IF ( eid<0 ) THEN
                        WRITE (No,99021) eid
                     ELSEIF ( eid==0 ) THEN
                        CALL read(*480,*340,f,z,2,0,kf)
                     ELSE
                        WRITE (No,99021) eid , (rx(l),l=a,b)
                        IF ( P2/=0 ) WRITE (No,99016)
                        IF ( b/=(a+z(2)-1) ) THEN
                           a = a + 8
                           EXIT SPAG_Loop_2_1
                        ENDIF
                     ENDIF
                  ENDDO SPAG_Loop_2_1
                  DO
                     b = a + 7
                     IF ( b>=(a+z(2)) ) b = a + z(2) - 1
                     Line = Line + ls
                     IF ( Line>Nlpp ) THEN
                        CALL page
                        WRITE (No,99020) x(3*rl) , y
                        WRITE (No,99016)
                        Line = ls + 3
                     ENDIF
                     WRITE (No,99007) (rx(l),l=a,b)
99007                FORMAT (17X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5)
                     IF ( P2/=0 ) WRITE (No,99016)
                     IF ( b==(a+z(2)-1) ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     a = a + 8
                  ENDDO
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
 340     ENDDO
         IF ( kf==0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 360     IF ( lc<12 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = 0
         spag_nextblock_1 = 6
      CASE (6)
         CALL read(*440,*500,f,pi,1,0,kf)
         j = j + 1
         wd = 10
         Line = Line + ls
         IF ( Line>Nlpp ) THEN
            CALL page
            WRITE (No,99016)
            Line = ls
         ENDIF
         IF ( P2/=0 ) WRITE (No,99016)
         CALL read(*480,*420,f,m,1,0,kf)
         CALL read(*480,*380,f,x,10,0,kf)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 380     wd = kf
         spag_nextblock_1 = 7
      CASE (7)
         WRITE (No,99022) j , pi , m , (x(l),l=1,wd)
         IF ( m<=10 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO
            Line = Line + ls
            IF ( Line>Nlpp ) THEN
               CALL page
               WRITE (No,99016)
               Line = ls
            ENDIF
            IF ( P2/=0 ) WRITE (No,99016)
            CALL read(*480,*400,f,x,10,0,kf)
            WRITE (No,99023) (x(l),l=1,wd)
         ENDDO
 400     wd = kf
         WRITE (No,99023) (x(l),l=1,wd)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 420     m = 0
         WRITE (No,99022) pi , m
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
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
 440     IF ( j==0 ) GOTO 480
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
!
!
!     INTERNAL ROUTINE TO SET HEADINGS AND INITIALIZE LINE COUNTER.
!     -------------------------------------------------------------
!
         DO m = 1 , 32
            H1(m) = Hx(m,m1)
            H2(m) = Hx(m,m2)
            H3(m) = Hx(m,m3)
         ENDDO
         Line = Nlpp
         GOTO r
      CASE (9)
!
!
!     PRINT TRAILER OF TABLE DATA BLOCK
!     ---------------------------------
!
         WRITE (No,99008) (t(l),l=2,7)
99008    FORMAT (15H0*** TRAILER = ,6I18)
!
!
!     DO NOT CALL PEXIT SINCE THIS IS AN OUTPUT PROCESSOR.
!
         CALL close(f,1)
         RETURN
      CASE (10)
!
!
         WRITE (No,99009) Uwm , P
99009    FORMAT (A25,' 2094, SUBROUTINE TABFMT, KEYNAME ',2A4,' NOT IN LIST OF AVAILABLE KEYNAMES.')
!
         WRITE (No,99010) (Na(1,l),Na(2,l),l=1,La)
99010    FORMAT ('0*** LIST OF RECOGNIZED KEYNAMES FOLLOWS...',/(20X,2A4))
         CALL close(f,1)
         RETURN
!
 460     WRITE (No,99011) Uwm
99011    FORMAT (A25,' 2095, SUBROUTINE TABFMT, PURGED INPUT.')
         CALL close(f,1)
         RETURN
!
 480     WRITE (No,99012) Uwm
99012    FORMAT (A25,' 2096, SUBROUTINE TABFMT, EOF ENCOUNTERED.')
         CALL close(f,1)
         RETURN
!
 500     WRITE (No,99013) Uwm
99013    FORMAT (A25,' 2097, SUBROUTINE TABFMT, EOR ENCOUNTERED.')
         CALL close(f,1)
         RETURN
      CASE (11)
!
         WRITE (No,99014) Uwm
99014    FORMAT (A25,' 2098, SUBROUTINE TABFMT, INSUFFICIENT CORE.')
         CALL close(f,1)
         RETURN
      CASE (12)
!
         WRITE (No,99015) Uwm , kf
99015    FORMAT (A25,' 2099, SUBROUTINE TABFMT, KF =',I10)
         CALL close(f,1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
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
END SUBROUTINE tabfmt
