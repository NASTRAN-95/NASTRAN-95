
SUBROUTINE ifppvc(*,Ipvs,Jr)
   IMPLICIT NONE
   LOGICAL Abort
   REAL D1(52) , D2(3) , D3(6) , D4(18) , Z(1)
   INTEGER Dum(79) , Ibuf , Id(2) , Jrun , Kn , Knt , Kor(1) , M(50) , M1(35) , M1f(35) , Mf(50) , Ncds , Nopen , Nout , T1(2,1)
   CHARACTER*23 Ufm
   COMMON /ifpdta/ Id , Kn , D1 , M , Mf , M1 , M1f , D2 , Nopen , D3 , Knt , D4
   COMMON /ifpx1 / Ncds , T1
   COMMON /system/ Ibuf , Nout , Abort , Dum , Jrun
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Kor
   INTEGER Ipvs
   INTEGER Jr(1)
   REAL a
   INTEGER blank , i , ia , ib , ibuf1 , ics , idon , if0 , ifield , ifil , ii , ik , ioldn , ipar , ipar1 , iplus , ipp , isort ,  &
         & istar , istop , isv , it , ithr , ivar , ivar1 , j , k , kdum , l42 , ln , lst , ltj , n , n1 , n2 , name(2) , ncdsmx ,  &
         & nptp , nv , nw
   INTEGER khrfn1
!
!     IFPPVC TAKES 1PARM AND 1VARY CARDS AND MAKES A SCRATCH FILE
!     TO USE IN MODIFYING OTHER BULK DATA CARDS
!
   EQUIVALENCE (Kor(1),Z(1))
   DATA ncdsmx , ifil , iplus , istar , nptp , ithr , blank/343 , 213 , 1H+ , 1H* , 4HNPTP , 4HTHRU , 4H    /
   DATA ivar , ipar , ivar1 , ipar1 , name/4HAVAR , 4HAPAR , 4H1VAR , 4H1PAR , 4HIFPP , 4HVC  /
!
   istop = 0
   ics = 0
   ltj = 0
   isort = 0
   ipp = 2*Ibuf + 2
   ln = ipp
   ii = ipp - 1
   lst = 0
   nv = 0
   idon = 0
   if0 = 0
   ioldn = 0
   isv = 0
   iplus = khrfn1(blank,4,iplus,1)
   istar = khrfn1(blank,4,istar,1)
   CALL sswtch(42,l42)
   GOTO 200
!
!     READ NEW CARD
!
 100  CALL read(*1500,*1500,nptp,Jr,20,1,kdum)
   Knt = Knt + 1
 200  it = khrfn1(blank,4,Jr(1),1)
   IF ( it==iplus .OR. it==istar ) THEN
      WRITE (Nout,99001) Ufm , Jr
99001 FORMAT (A23,' 312, NO CONTINUATION CARD ALLOWED ON 1PARM OR ','1VARY CARDS',/5X,'CARD- ',20A4)
      GOTO 1400
   ELSEIF ( Jr(1)==ivar1 ) THEN
!
!     1VARY CARDS START BUILDING SCRATCH FILE
!
      IF ( isort/=1 .AND. ltj==0 ) GOTO 600
      GOTO 400
   ELSEIF ( Jr(1)/=ipar1 ) THEN
!
!     CARDS ARE DONE
!
      idon = 1
      IF ( Jrun==0 ) GOTO 1300
      IF ( nv==0 .AND. Jrun>0 ) GOTO 1900
      IF ( nv/=0 ) GOTO 900
      GOTO 1200
   ELSE
!
!     1PARM CARDS
!
      Jr(1) = ipar
      IF ( l42==0 ) CALL rcard2(M1,M1f,nw,Jr)
      IF ( l42/=0 ) CALL rcard(M1,M1f,nw,Jr)
      IF ( nw/=10 ) GOTO 1600
!
!     CHECK FORMAT
!
      IF ( M1f(2)/=1 .OR. M1(3)<0 ) GOTO 1600
      IF ( M1(3)<Jrun ) GOTO 100
      IF ( M1f(3)/=0 .AND. M1f(3)/=1 ) GOTO 1600
      IF ( M1f(5)/=0 .AND. M1f(5)/=1 ) GOTO 1600
      IF ( M1f(7)/=0 .AND. M1f(7)/=1 ) GOTO 1600
      IF ( M1(4)<0 .OR. M1(6)<0 ) GOTO 1600
      IF ( M1(8)<0 .OR. M1f(9)/=0 ) GOTO 1600
      IF ( M1f(4)/=2 .AND. M1f(4)/=0 ) GOTO 1600
      IF ( M1f(6)/=2 .AND. M1f(6)/=0 ) GOTO 1600
      IF ( M1f(8)/=2 .AND. M1f(8)/=0 ) GOTO 1600
      IF ( Jrun==0 ) GOTO 100
      IF ( M1(3)/=Jrun .AND. isort==0 ) GOTO 500
      IF ( M1(3)/=Jrun ) GOTO 400
!
!     FORM LIST OF K  SK PAIRS FOR THIS J
!
      isort = 1
   ENDIF
 300  IF ( ipp>=Nopen ) GOTO 1700
   DO i = 3 , 7 , 2
      IF ( M1f(i)==0 .AND. M1f(i+1)/=0 ) GOTO 1600
      IF ( M1f(i)/=0 ) THEN
         Kor(ipp) = M1(i+1)
         Kor(ipp+1) = M1(i+2)
         ipp = ipp + 2
      ENDIF
   ENDDO
   GOTO 100
!
!     SORT LIST ERROR IF DUPLICATE
!
 400  i = ln
   it = 0
   n = ipp - i
   IF ( n>=3 ) THEN
      CALL sort(0,0,2,-1,Kor(i),n)
      it = Kor(i)
      j = n - 1
      DO k = 2 , j , 2
         IF ( Kor(i+k)==it ) THEN
            Abort = .TRUE.
            WRITE (Nout,99002) Ufm , it
99002       FORMAT (A23,' 314, DUPLICATE OR NO K  ON 1PARM CARDS FOR SOME J ','K =',I9)
         ENDIF
         it = Kor(i+k)
      ENDDO
   ENDIF
   isort = 0
   IF ( ics==0 ) THEN
      lst = n
      ln = ipp
   ENDIF
   IF ( Jr(1)==ivar1 ) GOTO 600
   IF ( idon==1 ) GOTO 1200
!
!     CHECK FOR DUPLICATE K ON 1PARM ON JRUN = 1
!
 500  IF ( Jrun/=1 ) GOTO 100
   IF ( ics==0 ) THEN
      ics = 1
      ltj = M1(3)
   ENDIF
   IF ( ltj==M1(3) ) GOTO 300
   ltj = M1(3)
   GOTO 400
!
!     IF LST = 0 USE ALL DEFAULT VALUES FOR SK
!
 600  ltj = 0
   nv = nv + 1
   Jr(1) = ivar
   IF ( l42==0 ) CALL rcard2(M1,M1f,nw,Jr)
   IF ( l42/=0 ) CALL rcard(M1,M1f,nw,Jr)
   IF ( nw<10 .OR. nw>12 ) GOTO 1600
!
!     CHECK FORMAT
!
   IF ( M1f(2)/=3 ) GOTO 1600
   IF ( M1f(3)/=1 .OR. M1f(4)/=1 ) GOTO 1600
   IF ( M1(5)<=0 .OR. M1(6)<=0 ) GOTO 1600
   IF ( M1f(5)/=0 .AND. M1f(5)/=2 ) GOTO 1600
   IF ( M1f(6)/=0 .AND. M1f(6)/=2 ) GOTO 1600
   IF ( M1f(7)/=0 .AND. M1f(7)/=1 ) GOTO 1600
   IF ( M1f(8)/=0 .AND. M1f(8)/=1 .AND. M1f(8)/=3 ) GOTO 1600
   IF ( M1f(9)/=0 .AND. M1f(9)/=1 ) GOTO 1600
   IF ( M1f(7)==0 .AND. M1f(8)==0 .AND. M1f(9)==0 ) GOTO 1600
   IF ( M1f(7)==1 .AND. M1(9)==0 ) GOTO 1600
   IF ( M1f(8)==1 .AND. M1(10)==0 ) GOTO 1600
   i = 0
   IF ( M1f(8)==3 ) i = 1
   IF ( M1f(9)==1 .AND. M1(i+11)==0 ) GOTO 1600
   IF ( M1f(8)==3 .AND. M1(10)/=ithr ) GOTO 1600
   IF ( M1f(8)==3 .AND. M1(9)>0 .AND. M1(12)<0 ) GOTO 1600
   IF ( M1f(8)==3 .AND. M1(9)<0 .AND. M1(12)>0 ) GOTO 1600
   IF ( Jrun==0 ) GOTO 100
   DO Kn = 1 , ncdsmx
      IF ( M1(3)==T1(1,Kn) .AND. M1(4)==T1(2,Kn) ) GOTO 700
   ENDDO
   WRITE (Nout,99003) Ufm , M1(3) , M1(4)
99003 FORMAT (A23,'316, CARD TYPE ',2A4,' NOT LEGAL ON 1VARY')
   GOTO 1400
 700  IF ( Kn/=ioldn .AND. ioldn/=0 ) GOTO 900
 800  ioldn = Kn
!
!     START A LIST WITH THIS NUMONIC
!
   ifield = M1(5)
   k = M1(6)
   ia = M1(7)
   ib = M1(8)
   IF ( M1f(8)==3 ) THEN
!
!     THRU OPTION
!
      n1 = M1(9)
      n2 = M1(12)
      IF ( n2<n1 ) THEN
         it = n1
         n1 = n2
         n2 = it
      ENDIF
      IF ( lst+isv+(iabs(n2-n1)*6)>Nopen ) GOTO 1700
      DO
         Kor(ln+isv) = Kn
         Kor(ln+isv+1) = n1
         Kor(ln+isv+2) = ifield
         Kor(ln+isv+3) = k
         Kor(ln+isv+4) = ia
         Kor(ln+isv+5) = ib
         isv = isv + 6
         n1 = n1 + 1
         IF ( n1>n2 ) GOTO 100
      ENDDO
   ELSE
      IF ( lst+isv+18>Nopen ) GOTO 1700
      DO i = 7 , 9
         IF ( M1f(i)/=0 ) THEN
            Kor(ln+isv) = Kn
            Kor(ln+isv+1) = M1(i+2)
            Kor(ln+isv+2) = ifield
            Kor(ln+isv+3) = k
            Kor(ln+isv+4) = ia
            Kor(ln+isv+5) = ib
            isv = isv + 6
         ENDIF
      ENDDO
      GOTO 100
   ENDIF
!
!     THIS TYPE OF CARD IS DONE SORT LIST AND MAKE FILE
!     SORT ON ID THEN FIELD THEN K
!
 900  IF ( isv/=6 ) THEN
      CALL sort(0,0,6,-2,Kor(ln),isv)
      CALL sort(0,0,6,-3,Kor(ln),isv)
      CALL sort(0,0,6,-4,Kor(ln),isv)
   ENDIF
!
!     FIX UP CORE FOR THIS BUFFER AND OPEN FILE
!
   IF ( if0==0 ) THEN
      ibuf1 = Nopen + 2*Ibuf
      Nopen = Nopen - Ibuf
      if0 = 1
      IF ( lst+isv>Nopen ) GOTO 1700
      CALL open(*1800,ifil,Kor(ibuf1+1),1)
   ENDIF
!
!     TEST FOR DUPLICATE K FOR SAME FIELD AND ID PLUS SORT AND REG
!
   IF ( isv/=6 ) THEN
      it = Kor(ln+1)
      ics = Kor(ln+2)
      ik = Kor(ln+3)
      DO i = 7 , isv , 6
         IF ( it==Kor(ln+i) .AND. ics==Kor(ln+i+1) .AND. ik==Kor(ln+i+2) ) THEN
            Abort = .TRUE.
            WRITE (Nout,99004) Ufm , it , ics , ik
99004       FORMAT (A23,'314, DUPLICATE K FOR  ID',I9,' FIELD',I9,' K',I9)
         ENDIF
         IF ( it<0 .AND. Kor(ln+i)>0 ) EXIT
         IF ( it>0 .AND. Kor(ln+i)<0 ) THEN
            j = Kor(ln)
            WRITE (Nout,99005) Ufm , T1(1,j) , T1(2,j)
99005       FORMAT (A23,'316, ILLEGAL TO USE SORTED COUNT AND REGULAR ID ON ','SAME TYPE OF CARD ',2A4)
         ENDIF
         it = Kor(ln+i)
         ics = Kor(ln+i+1)
         ik = Kor(ln+i+2)
      ENDDO
   ENDIF
!
!     PUT OUT CARDS SORT TYPE OF IDS (NEG) DO IN REVERSE
!     FIND VALUES OF SK FOR EACH K
!
   n = 6
   i = ln
   IF ( Kor(ln+1)<=0 ) THEN
      n = -6
      i = ln + isv - 6
   ENDIF
 1000 a = 0.0
   IF ( Kor(i+3)==Jrun .AND. lst==0 ) a = 1.0
   IF ( lst/=0 ) THEN
      DO k = 1 , lst , 2
         IF ( Kor(i+3)==Kor(ii+k) ) THEN
            a = Z(ii+k+1)
            EXIT
         ENDIF
      ENDDO
   ENDIF
   Z(i+3) = a
   it = Kor(i+1)
   ics = Kor(i+2)
   IF ( it<=0 .OR. ics/=2 ) THEN
      j = ics/10
      j = j*10
      IF ( j/=ics ) THEN
         j = (ics-1)/10
         j = j*10
         IF ( j/=ics-1 ) GOTO 1100
      ENDIF
   ENDIF
   Abort = .TRUE.
   j = Kor(ln)
   WRITE (Nout,99006) Ufm , T1(1,j) , T1(2,j) , it , ics
99006 FORMAT (A23,'31, CARD TYPE ',2A4,' ID =',I9,' HAS ILLEGAL FIELD',I9)
 1100 IF ( .NOT.(Abort .OR. a==0.0) ) CALL write(ifil,Kor(i),6,0)
   i = i + n
   isv = isv - iabs(n)
   IF ( isv>0 ) GOTO 1000
   isv = 0
   IF ( idon/=1 ) GOTO 800
 1200 IF ( Jrun/=0 ) THEN
      CALL write(ifil,0,0,1)
      CALL close(ifil,1)
      Ipvs = 1
   ENDIF
 1300 IF ( istop==0 ) RETURN
   IF ( istop==1 ) RETURN 1
!
!     ERROR MESSAGES
!
 1400 Abort = .TRUE.
   IF ( idon==1 ) GOTO 1200
   GOTO 100
 1500 WRITE (Nout,99007) Ufm
99007 FORMAT (A23,', NO BULK DATA CARDS TO MODIFY.  ERROR IN IFPPVC')
   istop = 1
   GOTO 1200
 1600 i = Knt + 1
   WRITE (Nout,99008) Ufm , M1(1) , M1(2) , i , Jr
99008 FORMAT (A23,' 317, ILLEGAL DATA OR FORMAT ON CARD ',2A4,' SORTED',I8,/5X,'CARD- ',20A4)
   GOTO 1400
 1700 WRITE (Nout,99009) Ufm
99009 FORMAT (A23,' 3008, NOT ENOUGH CORE FOR 1PARM AND 1VARY CARDS')
   GOTO 1400
 1800 CALL mesage(-1,ifil,name)
 1900 WRITE (Nout,99010) Ufm
99010 FORMAT (A23,', NO 1VARY CARDS TO GO WITH 1PARM CARDS.  ERROR IN ','IFPPVC')
   IF ( isort/=1 .AND. ltj==0 ) GOTO 1400
   GOTO 400
END SUBROUTINE ifppvc
