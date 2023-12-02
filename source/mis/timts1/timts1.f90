!*==timts1.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE timts1
   IMPLICIT NONE
   USE c_blank
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zblpkx
   USE c_zntpkx
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(15) :: ablk , bblk
   INTEGER , DIMENSION(4) , SAVE :: bck , rd , wrt
   INTEGER , DIMENSION(16) , SAVE :: bld , get , int , pak , put , unp
   INTEGER :: buf1 , buf2 , end , f1 , f2 , i , iret , j , k , kerr , m10 , m100 , mm , mx , n10 , nbrstr , nwds , p
   INTEGER , DIMENSION(2) , SAVE :: files , isubr
   REAL :: flag , fm , fn , t1 , t2 , t3 , t4 , time1 , time2 , tprrec , tprwrd
   INTEGER , SAVE :: i1000 , i1001 , nmask
   INTEGER , DIMENSION(9) :: mask
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(4) :: name
   REAL , DIMENSION(1) :: x , z
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
!     TIMTS1 TIME TESTS GINO AND THE PACK ROUTINES
!
   !>>>>EQUIVALENCE (Zd(1),Z(1)) , (Xd(1),X(1))
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
   buf1 = korsz(a) - sysbuf
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
   p = 4*(type-1) + 1
   mask(1) = 1
   DO i = 2 , nmask
      mask(i) = 2*mask(i-1)
   ENDDO
   WRITE (output,99001) n , m , type , opt1 , opt2
99001 FORMAT (1H ,20X,25HNASTRAN TIME TEST C   N =,I4,5H, M =,I4,8H, TYPE =,I4,8H, OPT1 =,I4,8H, OPT2 =,I4)
!
!     WRITE TEST
!
   IF ( andf(opt2,mask(1))==0 ) GOTO 300
   CALL open(*1300,f1,a(buf1),1)
   CALL cputim(t1,t1,1)
   DO i = 1 , n
      CALL write(f1,a,m,1)
   ENDDO
   CALL cputim(t2,t1,1)
   CALL close(f1,1)
   CALL open(*1300,f2,a(buf2),1)
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      CALL write(f2,a,m10,1)
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
 100  IF ( andf(opt2,mask(2))/=0 ) THEN
      CALL open(*1300,f1,a(buf1),0)
      CALL cputim(t1,t1,1)
      DO i = 1 , n
         CALL read(*1300,*1300,f1,a(i1000),m,1,flag)
      ENDDO
      CALL cputim(t2,t2,1)
      CALL close(f1,2)
      CALL open(*1300,f2,a(buf2),0)
      CALL cputim(t3,t3,1)
      DO i = 1 , n10
         CALL read(*1300,*1300,f2,a(i1000),m10,1,flag)
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
 200  IF ( andf(opt2,mask(3))/=0 ) THEN
      CALL open(*1300,f1,a(buf1),2)
      CALL cputim(t1,t1,1)
      DO i = 1 , n
         CALL bckrec(f1)
         CALL read(*1300,*1300,f1,a(i1000),m,1,flag)
         CALL bckrec(f1)
      ENDDO
      CALL cputim(t2,t2,1)
      CALL close(f1,1)
      CALL open(*1300,f2,a(buf2),2)
      CALL cputim(t3,t3,1)
      DO i = 1 , n10
         CALL bckrec(f2)
         CALL read(*1300,*1300,f2,a(i1000),m10,1,flag)
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
 300  IF ( andf(opt2,mask(4))==0 ) GOTO 500
   CALL open(*1300,f1,a(buf1),1)
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
   CALL open(*1300,f2,a(buf2),1)
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
   ASSIGN 400 TO iret
   name(1) = bld(p)
   name(2) = bld(p+1)
   name(3) = bld(p+2)
   name(4) = bld(p+3)
   GOTO 1100
!
!     INTPK TEST
!
 400  IF ( andf(opt2,mask(5))/=0 ) THEN
      CALL open(*1300,f1,a(buf1),0)
      CALL cputim(t1,t1,1)
      DO i = 1 , n
         CALL intpk(*1300,f1,0,type,0)
         DO j = 1 , m
            CALL zntpki
            IF ( ix/=j ) GOTO 1200
            IF ( eol/=0 ) THEN
               IF ( ix/=m ) GOTO 1200
            ENDIF
         ENDDO
         IF ( eol==0 ) GOTO 1200
      ENDDO
      CALL cputim(t2,t2,1)
      CALL close(f1,1)
      CALL open(*1300,f2,a(buf2),0)
      CALL cputim(t3,t3,1)
      DO i = 1 , n10
         CALL intpk(*1300,f2,0,type,0)
         DO j = 1 , m10
            CALL zntpki
            IF ( ix/=j ) GOTO 1200
            IF ( eol/=0 ) THEN
               IF ( ix/=m10 ) GOTO 1200
            ENDIF
         ENDDO
         IF ( eol==0 ) GOTO 1200
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
 500  IF ( andf(opt2,mask(6))==0 ) GOTO 700
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
   CALL open(*1300,f1,a(buf1),1)
   CALL cputim(t1,t1,1)
   DO i = 1 , n
      CALL pack(a(i1001),f1,mcb)
   ENDDO
   CALL cputim(t2,t2,1)
   CALL wrttrl(mcb)
   CALL close(f1,1)
   CALL makmcb(mcb,f2,m10,2,type)
   j1 = m10
   CALL open(*1300,f2,a(buf2),1)
   CALL cputim(t3,t3,1)
   DO i = 1 , n10
      CALL pack(a(i1001),f2,mcb)
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
 600  IF ( andf(opt2,mask(7))/=0 ) THEN
      typou2 = type
      i2 = 1
      j2 = m
      incr2 = 1
      CALL open(*1300,f1,a(buf1),0)
      CALL cputim(t1,t1,1)
      DO i = 1 , n
         CALL unpack(*1300,f1,a(i1001))
      ENDDO
      CALL cputim(t2,t2,1)
      CALL close(f1,1)
      j2 = m10
      CALL open(*1300,f2,a(buf2),0)
      CALL cputim(t3,t3,1)
      DO i = 1 , n10
         CALL unpack(*1300,f2,a(i1001))
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
 700  IF ( andf(opt2,mask(8))==0 ) GOTO 1000
   kerr = 1
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
            IF ( nbrstr==0 ) GOTO 1400
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
   kerr = 2
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
            IF ( nbrstr==0 ) GOTO 1400
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
   ASSIGN 800 TO iret
   name(1) = put(p)
   name(2) = put(p+1)
   name(3) = put(p+2)
   name(4) = put(p+3)
   GOTO 1100
!
!     GETSTR TEST
!
 800  IF ( andf(opt2,mask(9))/=0 ) THEN
      CALL gopen(f1,a(buf1),0)
      CALL cputim(t1,t1,1)
      DO i = 1 , n
         ablk(8) = -1
         DO
            CALL getstr(*850,ablk)
            mm = ablk(6)*nwds
            DO k = 1 , mm
               x(1) = a(k)
            ENDDO
            CALL endget(ablk)
         ENDDO
 850  ENDDO
      CALL cputim(t2,t2,1)
      CALL close(f1,1)
      CALL gopen(f2,a(buf2),0)
      CALL cputim(t3,t3,1)
      DO i = 1 , n10
         bblk(8) = -1
         DO
            CALL getstr(*900,bblk)
            mm = bblk(6)*nwds
            DO k = 1 , mm
               x(1) = a(k)
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
   WRITE (output,99002) name , time1 , name , time2 , tprwrd , tprrec
99002 FORMAT (1H0,4A4,'   N     M-WORD RECORDS -- TIME1 = ',E12.5,' SECONDS'/1X,4A4,' 10*N M/10-WORD RECORDS -- TIME2 = ',E12.5,    &
             &' SECONDS'/1H0,'IF THE MODEL IS TIME = (N*M)TPRWRD + N*TPRREC, THEN'/1H0,16X,'     -- TIME PER WORD   (TPRWRD) = ',   &
            & E12.5,' MICRO','SECONDS  --  DATA FOR USE IN COMMON /NTIME/'/1X,16X,'     -- TIME PER RECORD (TPRREC) = ',E12.5,      &
             &' MICRO','SECONDS')
!
   GOTO iret
!
!     INTERNAL ROUTINE CALLED FOR AN ABORT IN THE INTPK TEST
!
 1200 WRITE (output,99003) sfm , int(p) , int(p+1) , int(p+2) , int(p+3)
99003 FORMAT (A25,' 2197, ABORT CALLED DURING TIME TEST OF ',4A4)
!
!     ABNORMAL RETURNS FROM GINO--ALL FATAL ERRORS
!
 1300 CALL mesage(-61,0,0)
 1400 WRITE (output,99004) kerr
99004 FORMAT (23H0*** TIMTS1 FATAL ERROR,I4)
   GOTO 1300
END SUBROUTINE timts1
