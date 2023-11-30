
SUBROUTINE ifpmdc
   IMPLICIT NONE
   LOGICAL Abort
   INTEGER Apprch , Dum(17) , Ibits(18) , Ibuf , Id(2) , Im1(6) , Im10(3) , Im2(5) , Im3(4) , Im4(4) , Im5(2) , Im6 , Im7(8) , Im8 ,&
         & Im9(7) , Iparpt , Isfim , Isft(4) , Itwo(32) , Jrun , Kn , Knt , Kor(1) , Lbd , Lcc , M(50) , M1(35) , M1f(35) , Mach ,  &
         & Mf(50) , Mis , Nbits , Ncds , Ncpw , Nopen , Nout , T1(2,1) , X
   REAL D1(52) , D2(3) , D3(6) , D4(18) , Dum1(17) , Dum2(41) , Rm(1) , Rm1(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /ifpdta/ Id , Kn , D1 , M , Mf , M1 , M1f , D2 , Nopen , D3 , Knt , D4
   COMMON /ifpx0 / Lbd , Lcc , Ibits , Iparpt
   COMMON /ifpx1 / Ncds , T1
   COMMON /machin/ Mach
   COMMON /system/ Ibuf , Nout , Abort , Dum , Apprch , Dum1 , Nbits , X , Ncpw , Dum2 , Jrun
   COMMON /two   / Itwo
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xsrtcm/ Im1 , Im2 , Im3 , Im4 , Im5 , Im6 , Im7 , Im8 , Isft , Im9 , Isfim , Im10 , Mis
   COMMON /zzzzzz/ Kor
   INTEGER andf , lshift , orf , rshift
   REAL cd(6)
   LOGICAL cf , diag
   INTEGER cnt , con(38) , exi , i , ibuf1 , ick(6) , icycl , iefm , iend , ieof , ifil , ilst , inc(2) , iod , ionf , ipos , isc , &
         & ists , itm , ivc(2) , j , ji , k , k1 , karl , l , ncore , nf , nw , ret , test , xi(2)
   EXTERNAL andf , lshift , orf , rshift
!
!     IFPMDC MODIFIES BULK DATA CARDS GIVEN THE INFORMATION ON IFIL
!
   !>>>>EQUIVALENCE (Rm(1),M(1)) , (Rm1(1),M1(1)) , (ick(1),cd(1)) , (k,ick(1))
   DATA con/4H     , 4H   0 , 4H   1 , 4H   2 , 4H   3 , 4H   4 , 4H   5 , 4H   6 , 4H   7 , 4H   8 , 4H   9 , 4H   A , 4H   B ,    &
       &4H   C , 4H   D , 4H   E , 4H   F , 4H   G , 4H   H , 4H   I , 4H   J , 4H   K , 4H   L , 4H   M , 4H   N , 4H   O ,        &
      & 4H   P , 4H   Q , 4H   R , 4H   S , 4H   T , 4H   U , 4H   V , 4H   W , 4H   X , 4H   Y , 4H   Z , 4H    /
   DATA ifil , ieof , icycl , iefm , iend , diag/213 , 0 , 0 , -32767 , 4HZZZZ , .FALSE./
!
   IF ( ieof==-1 ) GOTO 1100
   cnt = 0
   IF ( ieof==1 ) GOTO 200
!
!     FIRST CALL INITIALIZE OPEN FILE ADJUST CORE
!
   ibuf1 = Nopen + 2*Ibuf
   Nopen = Nopen - Ibuf
   DO i = 1 , 38
      con(i) = andf(con(i),Im3(4))
   ENDDO
   IF ( Nopen<=0 ) THEN
      WRITE (Nout,99001) Ufm
99001 FORMAT (A23,' 303, NO OPEN CORE IFP')
      GOTO 1400
   ELSE
      cf = .FALSE.
      iod = 0
      isc = 0
      nf = 0
      ionf = 0
      ilst = 0
      ieof = 1
      CALL open(*1200,ifil,Kor(ibuf1+1),0)
   ENDIF
 100  CALL read(*1000,*1000,ifil,ick,6,0,nw)
!
!     CHECK INCOMING  CALL FOR VARY MATCH SORT, UNSORT AND/OR CONT
!
 200  IF ( k/=Kn ) THEN
!
!     NOT CARD WE ARE WORKING ON CHECK ALPH POSITION
!
      IF ( cf .OR. iod==Kn ) GOTO 1100
      iod = Kn
      isc = 0
      ASSIGN 300 TO exi
      xi(1) = T1(1,k)
      xi(2) = T1(2,k)
      GOTO 900
!
!     CARD TYPE FOUND TRY ID
!
   ELSEIF ( ick(2)<0 ) THEN
!
!     SORTED TYPE OF IDS NEED TO COUNT PARENTS IN THE GROUP
!
      IF ( cf .AND. nf/=0 .AND. isc==ick(2) .AND. cnt==1 ) GOTO 700
      IF ( cf .AND. nf/=0 .AND. isc==ick(2) ) GOTO 500
      IF ( cf ) GOTO 1100
      IF ( cnt/=1 ) THEN
         cnt = 1
         nf = 0
         ionf = 0
         ASSIGN 800 TO ret
         isc = isc - 1
      ENDIF
      IF ( isc>ick(2) ) GOTO 1100
      IF ( isc>=ick(2) ) GOTO 500
      GOTO 1300
   ELSE
      IF ( cf .AND. nf/=0 .AND. ilst==ick(2) .AND. cnt==1 ) GOTO 700
      IF ( cf .AND. nf/=0 .AND. ilst==ick(2) ) GOTO 500
      IF ( cf ) GOTO 1100
      nf = 0
      ionf = 0
      ASSIGN 100 TO ret
      IF ( M(1)<ick(2) ) GOTO 1100
      IF ( M(1)>ick(2) ) GOTO 1300
      ilst = ick(2)
      GOTO 500
   ENDIF
 300  ivc(1) = xi(1)
   ivc(2) = xi(2)
   ASSIGN 400 TO exi
   xi(1) = T1(1,Kn)
   xi(i) = T1(2,Kn)
   GOTO 900
 400  inc(1) = xi(1)
   inc(2) = xi(2)
   IF ( Mach/=2 ) THEN
      inc(1) = rshift(inc(1),1)
      ivc(1) = rshift(ivc(1),1)
   ENDIF
   IF ( inc(1)<ivc(1) ) GOTO 1100
   IF ( inc(1)>ivc(1) ) GOTO 1300
!
!     SHIFT IN CASE OF STAR
!
   inc(2) = rshift(inc(2),Nbits)
   ivc(2) = rshift(ivc(2),Nbits)
   IF ( inc(2)>=ivc(2) ) GOTO 1300
   GOTO 1100
!
!     FIND FIELD FORMAT DOES NOT COUNT FOR FIELD  1 OR 10 K1=COUNT
!
 500  DO i = 1 , 50
      IF ( Mf(i)==iefm ) GOTO 600
   ENDDO
   GOTO 1200
 600  nf = nf + i - 1
   cnt = 1
 700  k1 = ick(3)
!
!     FIND NUMBER OF FIELDS TO PITCH
!
   i = k1/10
   j = (k1-1)/10
   k1 = k1 - i - j - 1
!
!     CHECK TO SEE IF WE HAVE IT NOW
!
   IF ( k1>nf ) THEN
      IF ( M1(1)==0 .OR. M1(2)==0 ) GOTO 1100
      GOTO 1300
   ELSE
!
!     CHECK FORMAT FIELD FOR TYPE
!
      k1 = k1 - ionf
      IF ( Mf(k1)/=2 .AND. Mf(k1)/=0 ) THEN
         WRITE (Nout,99002) Ufm , T1(1,k) , T1(2,k) , Knt , ick(2) , ick(3)
99002    FORMAT (A23,' 0301, FIELD TO VARY IS NOT A REAL NUMBER. CARD ',2A4,'SORTED',I9,' ID',I9,' FIELD',I9)
         Abort = .TRUE.
         GOTO ret
      ELSE
         j = 0
         DO i = 1 , k1
            j = j + 1
            IF ( Mf(i)>2 ) j = j + 1
         ENDDO
!
!     PERFORM VARY
!
         IF ( cd(6)==0.0 ) THEN
            Rm(j) = Rm(j) + cd(4)*cd(5)
            Mf(k1) = 2
            IF ( diag ) WRITE (Nout,99004) Uim , T1(1,k) , T1(2,k) , Knt , ick(2) , ick(3) , Rm(j)
         ELSE
            Rm(j) = Rm(j)*(1.0+cd(4)*cd(5))**cd(6)
            IF ( diag ) WRITE (Nout,99004) Uim , T1(1,k) , T1(2,k) , Knt , ick(2) , ick(3) , Rm(j)
         ENDIF
!
!     SET RESTART BITS
!
         IF ( Apprch>=0 ) GOTO 750
!
!     CHECK FOR PARAM CARDS (82)
!
         IF ( Kn/=82 ) THEN
            j = Kn - 1
            GOTO 740
         ELSE
            DO i = Iparpt , Ncds
               IF ( M(1)==T1(1,i) .AND. M(2)==T1(2,i) ) GOTO 720
            ENDDO
            GOTO 750
         ENDIF
 720     j = i - 1
 740     karl = 1
         IF ( icycl==0 ) Ibits(karl) = orf(Ibits(karl),rshift(1,(X-1)))
         icycl = (j/31) + karl
         ipos = mod(j,31) + 2
         Ibits(icycl) = orf(Ibits(icycl),Itwo(ipos))
      ENDIF
 750  GOTO ret
   ENDIF
!
!     FOUND ID FIND FIELD
!
 800  CALL read(*1000,*1000,ifil,ick,6,0,nw)
   IF ( k/=Kn .OR. nf==0 .OR. isc/=ick(2) ) GOTO 200
   GOTO 700
!
!     CHANGE EXTERNAL BCD TO INTERNAL BCD FOR SORT TEST
!
 900  DO i = 1 , 2
      itm = xi(i)
      DO j = 1 , 4
         ji = 5 - j
         ists = Isft(ji)
         test = rshift(andf(itm,Im3(j)),ists)
         DO l = 1 , 37
            IF ( test==con(l) ) GOTO 920
         ENDDO
         l = 1
         EXIT
 920     itm = orf(andf(itm,Im4(j)),lshift(l,ists+Isfim))
         IF ( l==1 ) EXIT
      ENDDO
      xi(i) = itm
      IF ( l==1 ) EXIT
   ENDDO
   GOTO exi
!
!     END OF IFIL
!
 1000 CALL close(ifil,1)
   ieof = -1
   ncore = ncore + Ibuf
!
 1100 cf = .FALSE.
   ionf = nf
   IF ( M1(1)==0 .AND. M1(2)==0 ) cf = .TRUE.
   IF ( M1(1)==iend ) THEN
!
!     LAST TIME ENTERED MAKE SURE FILE IS USED UP
!
      IF ( ieof>=0 ) THEN
         DO
!
!     IFP IS DONE BUT VARY IS NOT   MESSAGES FOR ANY LEFT
!
            WRITE (Nout,99005) Ufm , T1(1,k) , T1(2,k) , ick(2) , ick(3)
            CALL read(*1000,*1000,ifil,ick,6,0,nw)
         ENDDO
      ENDIF
   ENDIF
   RETURN
 1200 WRITE (Nout,99003) Sfm
99003 FORMAT (A25,' 3037, ERROR IN IFPMDC')
   GOTO 1400
 1300 WRITE (Nout,99005) Ufm , T1(1,k) , T1(2,k) , ick(2) , ick(3)
   GOTO ret
 1400 Abort = .TRUE.
   Nopen = Nopen + Ibuf
   ieof = -1
   GOTO 1100
!
!     ERROR MESSAGES
!
99004 FORMAT (A29,' 3310, CARD TYPE ',2A4,' SORTED',I9,' ID',I9,' FIELD',I9,' CHANGED TO ',E16.8)
99005 FORMAT (A23,' 520, CARD TO VARY NOT FOUND. CARD ',2A4,' ID',I9,' FIELD',I9)
END SUBROUTINE ifpmdc