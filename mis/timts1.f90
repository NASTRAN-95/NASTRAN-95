
SUBROUTINE timts1
   IMPLICIT NONE
   REAL A(1) , X(1) , Z(1)
   INTEGER Eol , Eor , I1 , I2 , Incr1 , Incr2 , Ix , Iz , J1 , J2 , M , N , Opt1 , Opt2 , Output , Sysbuf , Type , Typin1 ,        &
         & Typou1 , Typou2
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   DOUBLE PRECISION Xd(2) , Zd(2)
   COMMON /blank / N , M , Type , Opt1 , Opt2
   COMMON /packx / Typin1 , Typou1 , I1 , J1 , Incr1
   COMMON /system/ Sysbuf , Output
   COMMON /unpakx/ Typou2 , I2 , J2 , Incr2
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zblpkx/ Zd , Iz
   COMMON /zntpkx/ Xd , Ix , Eol , Eor
   COMMON /zzzzzz/ A
   INTEGER ablk(15) , bblk(15) , bck(4) , bld(16) , buf1 , buf2 , end , f1 , f2 , files(2) , get(16) , i , i1000 , i1001 , int(16) ,&
         & iret , isubr(2) , j , k , kerr , m10 , m100 , mask(9) , mcb(7) , mm , mx , n10 , name(4) , nbrstr , nmask , nwds , p ,   &
         & pak(16) , put(16) , rd(4) , unp(16) , wrt(4)
   INTEGER andf , korsz
   REAL flag , fm , fn , t1 , t2 , t3 , t4 , time1 , time2 , tprrec , tprwrd
   EXTERNAL andf
!
!     TIMTS1 TIME TESTS GINO AND THE PACK ROUTINES
!
   EQUIVALENCE (Zd(1),Z(1)) , (Xd(1),X(1))
   DATA files/301 , 302/ , rd/1H  , 1H  , 1H  , 4HREAD/
   DATA i1000/1000/ , i1001/1001/ , wrt/1H  , 1H  , 4H   W , 4HRITE/ , bck/4H   B , 4HACKW , 4HARD  , 4HREAD/
   DATA bld/1H  , 4HBLDP , 4HK( R , 4HSP ) , 1H  , 4HBLDP , 4HK( R , 4HDP ) , 1H  , 4HBLDP , 4HK( C , 4HSP ) , 1H  , 4HBLDP ,       &
      & 4HK( C , 4HDP )/
   DATA int/1H  , 4HINTP , 4HK( R , 4HSP ) , 1H  , 4HINTP , 4HK( R , 4HDP ) , 1H  , 4HINTP , 4HK( C , 4HSP ) , 1H  , 4HINTP ,       &
      & 4HK( C , 4HDP )/
   DATA pak/1H  , 4H PAC , 4HK( R , 4HSP ) , 1H  , 4H PAC , 4HK( R , 4HDP ) , 1H  , 4H PAC , 4HK( C , 4HSP ) , 1H  , 4H PAC ,       &
      & 4HK( C , 4HDP )/
   DATA unp/1H  , 4HUNPA , 4HK( R , 4HSP ) , 1H  , 4HUNPA , 4HK( R , 4HDP ) , 1H  , 4HUNPA , 4HK( C , 4HSP ) , 1H  , 4HUNPA ,       &
      & 4HK( C , 4HDP )/
   DATA put/4H   P , 4HUTST , 4HR( R , 4HSP ) , 4H   P , 4HUTST , 4HR( R , 4HDP ) , 4H   P , 4HUTST , 4HR( C , 4HSP ) , 4H   P ,    &
       &4HUTST , 4HR( C , 4HDP )/
   DATA get/4H   G , 4HETST , 4HR( R , 4HSP ) , 4H   G , 4HETST , 4HR( R , 4HDP ) , 4H   G , 4HETST , 4HR( C , 4HSP ) , 4H   G ,    &
       &4HETST , 4HR( C , 4HDP )/
   DATA nmask/9/
   DATA isubr/4HTIMT , 4HS1  /
!
!     INITIALIZE
!
   CALL page1
   f1 = files(1)
   f2 = files(2)
   buf1 = korsz(A) - Sysbuf
   buf2 = buf1 - Sysbuf
   end = N*M
   IF ( end>=buf1-1 ) CALL mesage(-8,0,isubr)
   DO i = 1 , end
      A(i) = i
   ENDDO
   n10 = N*10
   m10 = M/10
   IF ( m10<=0 ) m10 = 1
   fn = N
   fm = M
   p = 4*(Type-1) + 1
   mask(1) = 1
   DO i = 2 , nmask
      mask(i) = 2*mask(i-1)
   ENDDO
   WRITE (Output,99001) N , M , Type , Opt1 , Opt2
99001 FORMAT (1H ,20X,25HNASTRAN TIME TEST C   N =,I4,5H, M =,I4,8H, TYPE =,I4,8H, OPT1 =,I4,8H, OPT2 =,I4)
!
!     WRITE TEST
!
   IF ( andf(Opt2,mask(1))==0 ) GOTO 300
   CALL open(*1300,f1,A(buf1),1)
   CALL cputim(t1,t1,1)
   DO i = 1 , N
      CALL write(f1,A,M,1)
   ENDDO
   CALL cputim(t2,t1,1)
   CALL close(f1,1)
   CALL open(*1300,f2,A(buf2),1)
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      CALL write(f2,A,m10,1)
   ENDDO
   CALL cputim(t4,t4,1)
   CALL close(f2,1)
   ASSIGN 100 TO iret
   name(1) = wrt(1)
   name(2) = wrt(2)
   name(3) = wrt(3)
   name(4) = wrt(4)
   GOTO 1100
!
!     READ TEST
!
 100  IF ( andf(Opt2,mask(2))/=0 ) THEN
      CALL open(*1300,f1,A(buf1),0)
      CALL cputim(t1,t1,1)
      DO i = 1 , N
         CALL read(*1300,*1300,f1,A(i1000),M,1,flag)
      ENDDO
      CALL cputim(t2,t2,1)
      CALL close(f1,2)
      CALL open(*1300,f2,A(buf2),0)
      CALL cputim(t3,t3,1)
      DO i = 1 , n10
         CALL read(*1300,*1300,f2,A(i1000),m10,1,flag)
      ENDDO
      CALL cputim(t4,t4,1)
      CALL close(f2,2)
      ASSIGN 200 TO iret
      name(1) = rd(1)
      name(2) = rd(2)
      name(3) = rd(3)
      name(4) = rd(4)
      GOTO 1100
   ENDIF
!
!     BACKWARD READ TEST
!
 200  IF ( andf(Opt2,mask(3))/=0 ) THEN
      CALL open(*1300,f1,A(buf1),2)
      CALL cputim(t1,t1,1)
      DO i = 1 , N
         CALL bckrec(f1)
         CALL read(*1300,*1300,f1,A(i1000),M,1,flag)
         CALL bckrec(f1)
      ENDDO
      CALL cputim(t2,t2,1)
      CALL close(f1,1)
      CALL open(*1300,f2,A(buf2),2)
      CALL cputim(t3,t3,1)
      DO i = 1 , n10
         CALL bckrec(f2)
         CALL read(*1300,*1300,f2,A(i1000),m10,1,flag)
         CALL bckrec(f2)
      ENDDO
      CALL cputim(t4,t4,1)
      CALL close(f2,1)
      ASSIGN 300 TO iret
      name(1) = bck(1)
      name(2) = bck(2)
      name(3) = bck(3)
      name(4) = bck(4)
      GOTO 1100
   ENDIF
!
!     BLDPK TEST
!
 300  IF ( andf(Opt2,mask(4))==0 ) GOTO 500
   CALL open(*1300,f1,A(buf1),1)
   CALL makmcb(mcb,f1,M,2,Type)
   CALL cputim(t1,t1,1)
   DO i = 1 , N
      CALL bldpk(Type,Type,f1,0,0)
      DO j = 1 , M
         Z(1) = 1.0
         Iz = j
         CALL zblpki
      ENDDO
      CALL bldpkn(f1,0,mcb)
   ENDDO
   CALL cputim(t2,t2,1)
   CALL wrttrl(mcb)
   CALL close(f1,1)
   CALL makmcb(mcb,f2,m10,2,Type)
   CALL open(*1300,f2,A(buf2),1)
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      CALL bldpk(Type,Type,f2,0,0)
      DO j = 1 , m10
         Z(1) = 2.0
         Iz = j
         CALL zblpki
      ENDDO
      CALL bldpkn(f2,0,mcb)
   ENDDO
   CALL cputim(t4,t4,1)
   CALL wrttrl(mcb)
   CALL close(f2,1)
   ASSIGN 400 TO iret
   name(1) = bld(p)
   name(2) = bld(p+1)
   name(3) = bld(p+2)
   name(4) = bld(p+3)
   GOTO 1100
!
!     INTPK TEST
!
 400  IF ( andf(Opt2,mask(5))/=0 ) THEN
      CALL open(*1300,f1,A(buf1),0)
      CALL cputim(t1,t1,1)
      DO i = 1 , N
         CALL intpk(*1300,f1,0,Type,0)
         DO j = 1 , M
            CALL zntpki
            IF ( Ix/=j ) GOTO 1200
            IF ( Eol/=0 ) THEN
               IF ( Ix/=M ) GOTO 1200
            ENDIF
         ENDDO
         IF ( Eol==0 ) GOTO 1200
      ENDDO
      CALL cputim(t2,t2,1)
      CALL close(f1,1)
      CALL open(*1300,f2,A(buf2),0)
      CALL cputim(t3,t3,1)
      DO i = 1 , n10
         CALL intpk(*1300,f2,0,Type,0)
         DO j = 1 , m10
            CALL zntpki
            IF ( Ix/=j ) GOTO 1200
            IF ( Eol/=0 ) THEN
               IF ( Ix/=m10 ) GOTO 1200
            ENDIF
         ENDDO
         IF ( Eol==0 ) GOTO 1200
      ENDDO
      CALL cputim(t4,t4,1)
      CALL close(f2,1)
      ASSIGN 500 TO iret
      name(1) = int(p)
      name(2) = int(p+1)
      name(3) = int(p+2)
      name(4) = int(p+3)
      GOTO 1100
   ENDIF
!
!     PACK TEST
!
 500  IF ( andf(Opt2,mask(6))==0 ) GOTO 700
   CALL makmcb(mcb,f1,M,2,Type)
   Typin1 = Type
   Typou1 = Type
   I1 = 1
   J1 = M
   Incr1 = 1
   mx = M*Type
   DO i = 1 , mx
      A(i+1000) = i
   ENDDO
   CALL open(*1300,f1,A(buf1),1)
   CALL cputim(t1,t1,1)
   DO i = 1 , N
      CALL pack(A(i1001),f1,mcb)
   ENDDO
   CALL cputim(t2,t2,1)
   CALL wrttrl(mcb)
   CALL close(f1,1)
   CALL makmcb(mcb,f2,m10,2,Type)
   J1 = m10
   CALL open(*1300,f2,A(buf2),1)
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      CALL pack(A(i1001),f2,mcb)
   ENDDO
   CALL cputim(t4,t4,1)
   CALL wrttrl(mcb)
   CALL close(f2,1)
   ASSIGN 600 TO iret
   name(1) = pak(p)
   name(2) = pak(p+1)
   name(3) = pak(p+2)
   name(4) = pak(p+3)
   GOTO 1100
!
!     UNPACK TEST
!
 600  IF ( andf(Opt2,mask(7))/=0 ) THEN
      Typou2 = Type
      I2 = 1
      J2 = M
      Incr2 = 1
      CALL open(*1300,f1,A(buf1),0)
      CALL cputim(t1,t1,1)
      DO i = 1 , N
         CALL unpack(*1300,f1,A(i1001))
      ENDDO
      CALL cputim(t2,t2,1)
      CALL close(f1,1)
      J2 = m10
      CALL open(*1300,f2,A(buf2),0)
      CALL cputim(t3,t3,1)
      DO i = 1 , n10
         CALL unpack(*1300,f2,A(i1001))
      ENDDO
      CALL cputim(t4,t4,1)
      CALL close(f2,2)
      ASSIGN 700 TO iret
      name(1) = unp(p)
      name(2) = unp(p+1)
      name(3) = unp(p+2)
      name(4) = unp(p+3)
      GOTO 1100
   ENDIF
!
!     PUTSTR TEST
!
!
 700  IF ( andf(Opt2,mask(8))==0 ) GOTO 1000
   kerr = 1
   ablk(1) = f1
   ablk(2) = Type
   ablk(3) = 1
   CALL gopen(f1,A(buf1),1)
   nwds = Type
   IF ( Type==3 ) nwds = 2
   CALL cputim(t1,t1,1)
   DO i = 1 , N
      ablk(4) = 0
      ablk(8) = -1
      DO j = 1 , 10
         nbrstr = m10
         DO
            CALL putstr(ablk)
            IF ( nbrstr==0 ) GOTO 1400
            ablk(7) = min0(ablk(6),nbrstr)
            ablk(4) = ablk(4) + ablk(7) + 4
            mm = ablk(7)*nwds
            DO k = 1 , mm
               X(1) = A(k)
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
   CALL gopen(f2,A(buf2),1)
   kerr = 2
   bblk(1) = f2
   bblk(2) = Type
   bblk(3) = 1
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      bblk(4) = 0
      bblk(8) = -1
      DO j = 1 , 10
         nbrstr = m100
         DO
            CALL putstr(bblk)
            IF ( nbrstr==0 ) GOTO 1400
            bblk(7) = min0(bblk(6),nbrstr)
            bblk(4) = bblk(4) + bblk(7) + 4
            mm = bblk(7)*nwds
            DO k = 1 , mm
               X(1) = A(k)
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
   ASSIGN 800 TO iret
   name(1) = put(p)
   name(2) = put(p+1)
   name(3) = put(p+2)
   name(4) = put(p+3)
   GOTO 1100
!
!     GETSTR TEST
!
 800  IF ( andf(Opt2,mask(9))/=0 ) THEN
      CALL gopen(f1,A(buf1),0)
      CALL cputim(t1,t1,1)
      DO i = 1 , N
         ablk(8) = -1
         DO
            CALL getstr(*850,ablk)
            mm = ablk(6)*nwds
            DO k = 1 , mm
               X(1) = A(k)
            ENDDO
            CALL endget(ablk)
         ENDDO
 850  ENDDO
      CALL cputim(t2,t2,1)
      CALL close(f1,1)
      CALL gopen(f2,A(buf2),0)
      CALL cputim(t3,t3,1)
      DO i = 1 , n10
         bblk(8) = -1
         DO
            CALL getstr(*900,bblk)
            mm = bblk(6)*nwds
            DO k = 1 , mm
               X(1) = A(k)
            ENDDO
            CALL endget(bblk)
         ENDDO
 900  ENDDO
      CALL cputim(t4,t4,1)
      CALL close(f2,1)
      ASSIGN 1000 TO iret
      name(1) = get(p)
      name(2) = get(p+1)
      name(3) = get(p+2)
      name(4) = get(p+3)
      GOTO 1100
   ENDIF
!
 1000 RETURN
!
!
!     INTERNAL ROUTINE TO WRITE OUTPUT ONTO THE OUTPUT FILE
!
 1100 time1 = t2 - t1
   time2 = t4 - t3
   tprrec = 1.0E6*(time2-time1)/(9.0*fn)
   tprwrd = (1.0E6*time1-fn*tprrec)/(fn*fm)
!
   WRITE (Output,99002) name , time1 , name , time2 , tprwrd , tprrec
99002 FORMAT (1H0,4A4,'   N     M-WORD RECORDS -- TIME1 = ',E12.5,' SECONDS'/1X,4A4,' 10*N M/10-WORD RECORDS -- TIME2 = ',E12.5,    &
             &' SECONDS'/1H0,'IF THE MODEL IS TIME = (N*M)TPRWRD + N*TPRREC, THEN'/1H0,16X,'     -- TIME PER WORD   (TPRWRD) = ',   &
            & E12.5,' MICRO','SECONDS  --  DATA FOR USE IN COMMON /NTIME/'/1X,16X,'     -- TIME PER RECORD (TPRREC) = ',E12.5,      &
             &' MICRO','SECONDS')
!
   GOTO iret
!
!     INTERNAL ROUTINE CALLED FOR AN ABORT IN THE INTPK TEST
!
 1200 WRITE (Output,99003) Sfm , int(p) , int(p+1) , int(p+2) , int(p+3)
99003 FORMAT (A25,' 2197, ABORT CALLED DURING TIME TEST OF ',4A4)
!
!     ABNORMAL RETURNS FROM GINO--ALL FATAL ERRORS
!
 1300 CALL mesage(-61,0,0)
 1400 WRITE (Output,99004) kerr
99004 FORMAT (23H0*** TIMTS1 FATAL ERROR,I4)
   GOTO 1300
END SUBROUTINE timts1
