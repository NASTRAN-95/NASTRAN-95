!*==tmtsio.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE tmtsio(Debug1) !HIDESTARS (*,Debug1)
   IMPLICIT NONE
   USE c_ginox
   USE c_ntime
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zblpkx
   USE c_zntpkx
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Debug1
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(15) :: ablk , bblk
   INTEGER :: buf1 , buf2 , end , f1 , f2 , i , iret , j , k , kerr , m , m10 , m100 , mm , mx , n , n10 , nbrstr , nwds , type
   INTEGER , DIMENSION(2) , SAVE :: files , isubr
   REAL :: flag , fm , fn , rpack , t1 , t2 , t3 , t4 , time1 , time2 , tprrec , tprwrd
   INTEGER , SAVE :: i1000 , i1001 , zero
   INTEGER , DIMENSION(7) :: mcb
   REAL , DIMENSION(23) :: t
   REAL , DIMENSION(1) :: x , z
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     TMTSIO TIME TESTS GINO AND THE PACK ROUTINES
!
!     COMMENT FORM G.CHAN/UNISYS   5/91
!     BASICALLY THIS ROUTINE IS SAME AS TIMTS1.
!
   !>>>>EQUIVALENCE (Zd(1),Z(1)) , (Xd(1),X(1)) , (T(1),Tgino)
   DATA files/301 , 304/ , zero/0/
   DATA i1000/1000/ , i1001/1001/
   DATA isubr/4HTMTS , 4HIO  /
!
!
!     CHECK KORSZ AND DUMMY SUBROUTINES HERE.
!     IF NASTRAN SUBROUTINES WERE NOT COMPILED WITH STATIC OPTION, BUF1
!     COULD BE NEGATIVE HERE.
!     CALL DUMMY NEXT TO SEE WHETHER THE RIGHT DUMMY ROUTINE IS SET UP
!     FOR THIS MACHINE
!
   kerr = 1
   buf1 = korsz(a)
   IF ( buf1<=0 ) GOTO 1800
   IF ( Debug1>0 ) WRITE (output,99001)
99001 FORMAT (' -LINK1 DEBUG- TMTSIO CALLINS DUMMY NEXT')
!WKBD CALL DUMMY
!
!     NOTE - ISY77 (WHICH IS BULKDATA OPTION) AND TGINO DETERMINE TO
!            SKIP TMTSIO AND TMTSLP OR NOT. DIAG 35 CAN NOT BE USED AT
!            THIS POINT SINCE THE DIAG CARD HAS NOT BEEN READ YET.
!
   IF ( tgino>0. .AND. isy77/=-3 ) RETURN 1
!
!     INITIALIZE
!
   CALL page1
   WRITE (output,99002)
99002 FORMAT ('0*** USER INFORMATION MESSAGE 225, GINO TIME CONSTANTS ','ARE BEING COMPUTED',/5X,                                   &
             &'(SEE NASINFO FILE FOR ELIMINATION OF THESE COMPUTATIONS)')
   IF ( tgino>0. ) WRITE (output,99003) t
99003 FORMAT ('0*** EXISTING TIME CONSTANTS IN /NTIME/ -',/,2(/5X,9F8.3))
   n = 50
   m = n
   type = iprec
!     NITEMS = 23
!
   f1 = files(1)
   f2 = files(2)
   buf1 = buf1 - sysbuf
   buf2 = buf1 - sysbuf
   end = n*m
   IF ( end>=buf1-1 ) CALL mesage(-8,0,isubr)
   DO i = 1 , end
      a(i) = i
   ENDDO
   n10 = n*10
   m10 = m/10
   IF ( m10<=0 ) m10 = 1
   fn = n
   fm = m
!
!     WRITE TEST
!
   IF ( Debug1>0 ) WRITE (output,99004) nbuff3 , ig
99004 FORMAT (' -LINK1 DEBUG- OPEN OUTPUT FILE NEXT FOR WRITE. NBUFF3 =',I5,/5X,'GINO BUFADD 75 WORDS =',/,(2X,11I7))
   CALL open(*1700,f1,a(buf1),1)
   IF ( Debug1>0 ) THEN
      WRITE (output,99005) nbuff3 , ig
99005 FORMAT (' -LINK1 DEBUG- FILE OPEN OK. NBUFF3 =',I5,/5X,'GINO BUFADD 75 WORDS =',/,(2X,11I7))
      WRITE (output,99006) iwr(41)
99006 FORMAT (5X,'RWFLG(41) =',I7,//,' -LINK1 DEBUG- CALLING SECOND NEXT')
   ENDIF
   CALL cputim(t1,t1,1)
   DO i = 1 , n
      CALL write(f1,a,m,1)
   ENDDO
   CALL cputim(t2,t2,1)
   IF ( Debug1>0 ) WRITE (output,99007)
99007 FORMAT (' -LINK1 DEBUG- CLOSE FILE NEXT')
   CALL close(f1,1)
   IF ( Debug1>0 ) WRITE (output,99008)
99008 FORMAT (' -LINK1 DEBUG- OPEN ANOTHER OUTPUT FILE NEXT FOR WRITE')
   CALL open(*1700,f2,a(buf2),1)
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      CALL write(f2,a,m10,1)
   ENDDO
   CALL cputim(t4,t4,1)
   CALL close(f2,1)
   ASSIGN 200 TO iret
!
!     INTERNAL ROUTINE TO STORE TIMING DATA IN /NTIME/ COMMON BLOCK
!
 100  time1 = t2 - t1
   time2 = t4 - t3
   tprrec = 1.0E6*(time2-time1)/(9.0*fn)
   tprwrd = (1.0E6*time1-fn*tprrec)/(fn*fm)
   GOTO iret
 200  tgino = tprwrd
   rgino = tprrec
!
!     READ TEST
!
   IF ( Debug1>0 ) WRITE (output,99009)
99009 FORMAT (' -LINK1 DEBUG- OPEN INPUT FILE NEXT FOR READ')
   CALL open(*1700,f1,a(buf1),0)
   CALL cputim(t1,t1,1)
   DO i = 1 , n
      CALL read(*1700,*1700,f1,a(i1000),m,1,flag)
   ENDDO
   CALL cputim(t2,t2,1)
   CALL close(f1,2)
   CALL open(*1700,f2,a(buf2),0)
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      CALL read(*1700,*1700,f2,a(i1000),m10,1,flag)
   ENDDO
   CALL cputim(t4,t4,1)
   CALL close(f2,2)
   ASSIGN 300 TO iret
   GOTO 100
 300  tgino = tgino + tprwrd
   rgino = rgino + tprrec
!
!     BACKWARD READ TEST
!
   CALL open(*1700,f1,a(buf1),2)
   CALL cputim(t1,t1,1)
   DO i = 1 , n
      CALL bckrec(f1)
      CALL read(*1700,*1700,f1,a(i1000),m,1,flag)
      CALL bckrec(f1)
   ENDDO
   CALL cputim(t2,t2,1)
   CALL close(f1,1)
   CALL open(*1700,f2,a(buf2),2)
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      CALL bckrec(f2)
      CALL read(*1700,*1700,f2,a(i1000),m10,1,flag)
      CALL bckrec(f2)
   ENDDO
   CALL cputim(t4,t4,1)
   CALL close(f2,1)
   ASSIGN 400 TO iret
   GOTO 100
 400  tgino = tgino + tprwrd
   tgino = tgino/3.0
   rgino = rgino + tprrec
   rgino = rgino/3.0
!
!     BLDPK TEST
!
   CALL open(*1700,f1,a(buf1),1)
   CALL makmcb(mcb,f1,m,2,type)
   CALL cputim(t1,t1,1)
   DO i = 1 , n
      CALL bldpk(type,type,f1,0,0)
      DO j = 1 , m
         z(1) = 1.0
         iz = j
         CALL zblpki
      ENDDO
      CALL bldpkn(f1,0,mcb)
   ENDDO
   CALL cputim(t2,t2,1)
   CALL wrttrl(mcb)
   CALL close(f1,1)
   CALL makmcb(mcb,f2,m10,2,type)
   CALL open(*1700,f2,a(buf2),1)
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      CALL bldpk(type,type,f2,0,0)
      DO j = 1 , m10
         z(1) = 2.0
         iz = j
         CALL zblpki
      ENDDO
      CALL bldpkn(f2,0,mcb)
   ENDDO
   CALL cputim(t4,t4,1)
   CALL wrttrl(mcb)
   CALL close(f2,1)
   ASSIGN 500 TO iret
   GOTO 100
 500  tbldpk = tprwrd
   rbldpk = tprrec
!
!     INTPK TEST
!
   CALL open(*1700,f1,a(buf1),0)
   CALL cputim(t1,t1,1)
   DO i = 1 , n
      CALL intpk(*1700,f1,0,type,0)
      DO j = 1 , m
         CALL zntpki
         IF ( ix/=j ) GOTO 1600
         IF ( eol/=0 ) THEN
            IF ( ix/=m ) GOTO 1600
         ENDIF
      ENDDO
      IF ( eol==0 ) GOTO 1600
   ENDDO
   CALL cputim(t2,t2,1)
   CALL close(f1,1)
   CALL open(*1700,f2,a(buf2),0)
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      CALL intpk(*1700,f2,0,type,0)
      DO j = 1 , m10
         CALL zntpki
         IF ( ix/=j ) GOTO 1600
         IF ( eol/=0 ) THEN
            IF ( ix/=m10 ) GOTO 1600
         ENDIF
      ENDDO
      IF ( eol==0 ) GOTO 1600
   ENDDO
   CALL cputim(t4,t4,1)
   CALL close(f2,1)
   ASSIGN 600 TO iret
   GOTO 100
 600  tintpk = tprwrd
   rintpk = tprrec
!
!     PACK TEST
!
   CALL makmcb(mcb,f1,m,2,type)
   typin1 = type
   typou1 = type
   i1 = 1
   j1 = m
   incr1 = 1
   mx = m*type
   DO i = 1 , mx
      a(i+1000) = i
   ENDDO
   CALL open(*1700,f1,a(buf1),1)
   CALL cputim(t1,t1,1)
   DO i = 1 , n
      CALL pack(a(i1001),f1,mcb)
   ENDDO
   CALL cputim(t2,t2,1)
   CALL wrttrl(mcb)
   CALL close(f1,1)
   CALL makmcb(mcb,f2,m10,2,type)
   j1 = m10
   CALL open(*1700,f2,a(buf2),1)
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      CALL pack(a(i1001),f2,mcb)
   ENDDO
   CALL cputim(t4,t4,1)
   CALL wrttrl(mcb)
   CALL close(f2,1)
   ASSIGN 700 TO iret
   GOTO 100
 700  tpack = tprwrd
   rpack = tprrec
!
!     UNPACK TEST
!
   typou2 = type
   i2 = 1
   j2 = m
   incr2 = 1
   CALL open(*1700,f1,a(buf1),0)
   CALL cputim(t1,t1,1)
   DO i = 1 , n
      CALL unpack(*1700,f1,a(i1001))
   ENDDO
   CALL cputim(t2,t2,1)
   CALL close(f1,1)
   j2 = m10
   CALL open(*1700,f2,a(buf2),0)
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      CALL unpack(*1700,f2,a(i1001))
   ENDDO
   CALL cputim(t4,t4,1)
   CALL close(f2,2)
   ASSIGN 800 TO iret
   GOTO 100
 800  tunpak = tprwrd
   runpak = tprrec
!
!     PUTSTR TEST
!
   kerr = 2
   ablk(1) = f1
   ablk(2) = type
   ablk(3) = 1
   CALL gopen(f1,a(buf1),1)
   nwds = type
   IF ( type==3 ) nwds = 2
   CALL cputim(t1,t1,1)
   DO i = 1 , n
      ablk(4) = 0
      ablk(8) = -1
      DO j = 1 , 10
         nbrstr = m10
         DO
            CALL putstr(ablk)
            IF ( nbrstr==0 ) GOTO 1800
            ablk(7) = min0(ablk(6),nbrstr)
            ablk(4) = ablk(4) + ablk(7) + 4
            mm = ablk(7)*nwds
            DO k = 1 , mm
               x(1) = a(k)
            ENDDO
            IF ( ablk(7)==nbrstr ) THEN
               IF ( j==10 ) ablk(8) = 1
               CALL endput(ablk)
               EXIT
            ELSE
               CALL endput(ablk)
               nbrstr = nbrstr - ablk(7)
            ENDIF
         ENDDO
      ENDDO
   ENDDO
   CALL cputim(t2,t2,1)
   CALL close(f1,1)
   m100 = max0(m10/10,1)
   CALL gopen(f2,a(buf2),1)
   kerr = 3
   bblk(1) = f2
   bblk(2) = type
   bblk(3) = 1
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      bblk(4) = 0
      bblk(8) = -1
      DO j = 1 , 10
         nbrstr = m100
         DO
            CALL putstr(bblk)
            IF ( nbrstr==0 ) GOTO 1800
            bblk(7) = min0(bblk(6),nbrstr)
            bblk(4) = bblk(4) + bblk(7) + 4
            mm = bblk(7)*nwds
            DO k = 1 , mm
               x(1) = a(k)
            ENDDO
            IF ( bblk(7)==nbrstr ) THEN
               IF ( j==10 ) bblk(8) = 1
               CALL endput(bblk)
               EXIT
            ELSE
               nbrstr = nbrstr - bblk(7)
            ENDIF
         ENDDO
      ENDDO
   ENDDO
   CALL cputim(t4,t4,1)
   CALL close(f2,1)
   ASSIGN 900 TO iret
   GOTO 100
 900  tputst = tprwrd
   rputst = tprrec
!
!     GETSTR TEST (GET STRING FORWARD)
!
   CALL gopen(f1,a(buf1),0)
   CALL cputim(t1,t1,1)
   DO i = 1 , n
      ablk(8) = -1
      DO
         CALL getstr(*1000,ablk)
         mm = ablk(6)*nwds
         DO k = 1 , mm
            x(1) = a(k)
         ENDDO
         CALL endget(ablk)
      ENDDO
 1000 ENDDO
   CALL cputim(t2,t2,1)
!     CALL CLOSE  (F1,1)
   CALL gopen(f2,a(buf2),0)
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      bblk(8) = -1
      DO
         CALL getstr(*1100,bblk)
         mm = bblk(6)*nwds
         DO k = 1 , mm
            x(1) = a(k)
         ENDDO
         CALL endget(bblk)
      ENDDO
 1100 ENDDO
   CALL cputim(t4,t4,1)
!     CALL CLOSE  (F2,1)
   ASSIGN 1200 TO iret
   GOTO 100
 1200 tgetst = tprwrd
   rgetst = tprrec
!
!     GETSTB TEST, (GET BACKWARD STRING)
!     F1 AND F2 FILES ARE STILL OPENED, AND POSITIONED AT THE END
!
!     CALL GOPEN (F1,A(BUF1),0)
!     CALL REWIND (F1)
!     CALL SKPFIL (F1,N+1)
   CALL cputim(t1,t1,1)
   DO i = 1 , n
      ablk(8) = -1
      DO
         CALL getstb(*1300,ablk)
         mm = ablk(6)*nwds
         DO k = 1 , mm
            x(1) = a(k)
         ENDDO
         CALL endgtb(ablk)
      ENDDO
 1300 ENDDO
   CALL cputim(t2,t2,1)
   CALL close(f1,1)
!     CALL GOPEN  (F2,A(BUF2),0)
!     CALL REWIND (F2)
!     CALL SKPFIL (F2,N10+1)
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      bblk(8) = -1
      DO
         CALL getstb(*1400,bblk)
         mm = bblk(6)*nwds
         DO k = 1 , mm
            x(1) = a(k)
         ENDDO
         CALL endgtb(bblk)
      ENDDO
 1400 ENDDO
   CALL cputim(t4,t4,1)
   CALL close(f2,1)
   ASSIGN 1500 TO iret
   GOTO 100
 1500 tgetsb = tprwrd
   IF ( Debug1>0 ) WRITE (output,99010)
99010 FORMAT (' -LINK1 DEBUG- TMTSIO FINISHED')
   RETURN
!
!     INTERNAL ROUTINE CALLED FOR AN ABORT IN THE INTPK TEST
!
 1600 WRITE (output,99011) sfm
99011 FORMAT (A25,' 2197, ABORT CALLED DURING TIME TEST OF INTPK')
!
!     ABNORMAL RETURNS FROM GINO - ALL FATAL ERRORS
!
 1700 CALL mesage(-61,0,0)
 1800 WRITE (output,99012) kerr
99012 FORMAT ('0*** TMTSIO FATAL ERROR',I7)
   GOTO 1700
END SUBROUTINE tmtsio
