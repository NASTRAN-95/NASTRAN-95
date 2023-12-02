!*==ifppvc.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifppvc(Ipvs,Jr) !HIDESTARS (*,Ipvs,Jr)
   USE c_ifpdta
   USE c_ifpx1
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ipvs
   INTEGER , DIMENSION(1) :: Jr
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a
   INTEGER , SAVE :: blank , ifil , ipar , ipar1 , iplus , istar , ithr , ivar , ivar1 , ncdsmx , nptp
   INTEGER :: i , ia , ib , ibuf1 , ics , idon , if0 , ifield , ii , ik , ioldn , ipp , isort , istop , isv , it , j , k , kdum ,   &
            & l42 , ln , lst , ltj , n , n1 , n2 , nv , nw
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(1) :: z
   EXTERNAL close , khrfn1 , mesage , open , rcard , rcard2 , read , sort , sswtch , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     IFPPVC TAKES 1PARM AND 1VARY CARDS AND MAKES A SCRATCH FILE
!     TO USE IN MODIFYING OTHER BULK DATA CARDS
!
   !>>>>EQUIVALENCE (Kor(1),Z(1))
   DATA ncdsmx , ifil , iplus , istar , nptp , ithr , blank/343 , 213 , 1H+ , 1H* , 4HNPTP , 4HTHRU , 4H    /
   DATA ivar , ipar , ivar1 , ipar1 , name/4HAVAR , 4HAPAR , 4H1VAR , 4H1PAR , 4HIFPP , 4HVC  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         istop = 0
         ics = 0
         ltj = 0
         isort = 0
         ipp = 2*ibuf + 2
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
         spag_nextblock_1 = 3
      CASE (2)
!
!     READ NEW CARD
!
         CALL read(*20,*20,nptp,Jr,20,1,kdum)
         knt = knt + 1
         spag_nextblock_1 = 3
      CASE (3)
         it = khrfn1(blank,4,Jr(1),1)
         IF ( it==iplus .OR. it==istar ) THEN
            WRITE (nout,99001) ufm , Jr
99001       FORMAT (A23,' 312, NO CONTINUATION CARD ALLOWED ON 1PARM OR ','1VARY CARDS',/5X,'CARD- ',20A4)
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Jr(1)==ivar1 ) THEN
!
!     1VARY CARDS START BUILDING SCRATCH FILE
!
            IF ( isort==1 .OR. ltj/=0 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Jr(1)/=ipar1 ) THEN
!
!     CARDS ARE DONE
!
            idon = 1
            IF ( jrun==0 ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( nv==0 .AND. jrun>0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( nv==0 ) THEN
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     1PARM CARDS
!
            Jr(1) = ipar
            IF ( l42==0 ) CALL rcard2(m1,m1f,nw,Jr)
            IF ( l42/=0 ) CALL rcard(m1,m1f,nw,Jr)
            IF ( nw/=10 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     CHECK FORMAT
!
            IF ( m1f(2)/=1 .OR. m1(3)<0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m1(3)<jrun ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m1f(3)/=0 .AND. m1f(3)/=1 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m1f(5)/=0 .AND. m1f(5)/=1 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m1f(7)/=0 .AND. m1f(7)/=1 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m1(4)<0 .OR. m1(6)<0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m1(8)<0 .OR. m1f(9)/=0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m1f(4)/=2 .AND. m1f(4)/=0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m1f(6)/=2 .AND. m1f(6)/=0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m1f(8)/=2 .AND. m1f(8)/=0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( jrun==0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m1(3)/=jrun .AND. isort==0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m1(3)/=jrun ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     FORM LIST OF K  SK PAIRS FOR THIS J
!
            isort = 1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         IF ( ipp>=nopen ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = 3 , 7 , 2
            IF ( m1f(i)==0 .AND. m1f(i+1)/=0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m1f(i)/=0 ) THEN
               kor(ipp) = m1(i+1)
               kor(ipp+1) = m1(i+2)
               ipp = ipp + 2
            ENDIF
         ENDDO
         spag_nextblock_1 = 2
      CASE (5)
!
!     SORT LIST ERROR IF DUPLICATE
!
         i = ln
         it = 0
         n = ipp - i
         IF ( n>=3 ) THEN
            CALL sort(0,0,2,-1,kor(i),n)
            it = kor(i)
            j = n - 1
            DO k = 2 , j , 2
               IF ( kor(i+k)==it ) THEN
                  abort = .TRUE.
                  WRITE (nout,99002) ufm , it
99002             FORMAT (A23,' 314, DUPLICATE OR NO K  ON 1PARM CARDS FOR SOME J ','K =',I9)
               ENDIF
               it = kor(i+k)
            ENDDO
         ENDIF
         isort = 0
         IF ( ics==0 ) THEN
            lst = n
            ln = ipp
         ENDIF
         IF ( Jr(1)==ivar1 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( idon==1 ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     CHECK FOR DUPLICATE K ON 1PARM ON JRUN = 1
!
         IF ( jrun/=1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ics==0 ) THEN
            ics = 1
            ltj = m1(3)
         ENDIF
         IF ( ltj==m1(3) ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ltj = m1(3)
         spag_nextblock_1 = 5
      CASE (7)
!
!     IF LST = 0 USE ALL DEFAULT VALUES FOR SK
!
         ltj = 0
         nv = nv + 1
         Jr(1) = ivar
         IF ( l42==0 ) CALL rcard2(m1,m1f,nw,Jr)
         IF ( l42/=0 ) CALL rcard(m1,m1f,nw,Jr)
         IF ( nw<10 .OR. nw>12 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CHECK FORMAT
!
         IF ( m1f(2)/=3 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m1f(3)/=1 .OR. m1f(4)/=1 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m1(5)<=0 .OR. m1(6)<=0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m1f(5)/=0 .AND. m1f(5)/=2 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m1f(6)/=0 .AND. m1f(6)/=2 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m1f(7)/=0 .AND. m1f(7)/=1 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m1f(8)/=0 .AND. m1f(8)/=1 .AND. m1f(8)/=3 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m1f(9)/=0 .AND. m1f(9)/=1 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m1f(7)==0 .AND. m1f(8)==0 .AND. m1f(9)==0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m1f(7)==1 .AND. m1(9)==0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m1f(8)==1 .AND. m1(10)==0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i = 0
         IF ( m1f(8)==3 ) i = 1
         IF ( m1f(9)==1 .AND. m1(i+11)==0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m1f(8)==3 .AND. m1(10)/=ithr ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m1f(8)==3 .AND. m1(9)>0 .AND. m1(12)<0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m1f(8)==3 .AND. m1(9)<0 .AND. m1(12)>0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( jrun==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO kn = 1 , ncdsmx
            IF ( m1(3)==t1(1,kn) .AND. m1(4)==t1(2,kn) ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         WRITE (nout,99003) ufm , m1(3) , m1(4)
99003    FORMAT (A23,'316, CARD TYPE ',2A4,' NOT LEGAL ON 1VARY')
         spag_nextblock_1 = 15
      CASE (8)
         IF ( kn/=ioldn .AND. ioldn/=0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         ioldn = kn
!
!     START A LIST WITH THIS NUMONIC
!
         ifield = m1(5)
         k = m1(6)
         ia = m1(7)
         ib = m1(8)
         IF ( m1f(8)==3 ) THEN
!
!     THRU OPTION
!
            n1 = m1(9)
            n2 = m1(12)
            IF ( n2<n1 ) THEN
               it = n1
               n1 = n2
               n2 = it
            ENDIF
            IF ( lst+isv+(iabs(n2-n1)*6)>nopen ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO
               kor(ln+isv) = kn
               kor(ln+isv+1) = n1
               kor(ln+isv+2) = ifield
               kor(ln+isv+3) = k
               kor(ln+isv+4) = ia
               kor(ln+isv+5) = ib
               isv = isv + 6
               n1 = n1 + 1
               IF ( n1>n2 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ELSE
            IF ( lst+isv+18>nopen ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO i = 7 , 9
               IF ( m1f(i)/=0 ) THEN
                  kor(ln+isv) = kn
                  kor(ln+isv+1) = m1(i+2)
                  kor(ln+isv+2) = ifield
                  kor(ln+isv+3) = k
                  kor(ln+isv+4) = ia
                  kor(ln+isv+5) = ib
                  isv = isv + 6
               ENDIF
            ENDDO
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
!
!     THIS TYPE OF CARD IS DONE SORT LIST AND MAKE FILE
!     SORT ON ID THEN FIELD THEN K
!
         IF ( isv/=6 ) THEN
            CALL sort(0,0,6,-2,kor(ln),isv)
            CALL sort(0,0,6,-3,kor(ln),isv)
            CALL sort(0,0,6,-4,kor(ln),isv)
         ENDIF
!
!     FIX UP CORE FOR THIS BUFFER AND OPEN FILE
!
         IF ( if0==0 ) THEN
            ibuf1 = nopen + 2*ibuf
            nopen = nopen - ibuf
            if0 = 1
            IF ( lst+isv>nopen ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL open(*40,ifil,kor(ibuf1+1),1)
         ENDIF
!
!     TEST FOR DUPLICATE K FOR SAME FIELD AND ID PLUS SORT AND REG
!
         IF ( isv/=6 ) THEN
            it = kor(ln+1)
            ics = kor(ln+2)
            ik = kor(ln+3)
            SPAG_Loop_1_1: DO i = 7 , isv , 6
               IF ( it==kor(ln+i) .AND. ics==kor(ln+i+1) .AND. ik==kor(ln+i+2) ) THEN
                  abort = .TRUE.
                  WRITE (nout,99004) ufm , it , ics , ik
99004             FORMAT (A23,'314, DUPLICATE K FOR  ID',I9,' FIELD',I9,' K',I9)
               ENDIF
               IF ( it<0 .AND. kor(ln+i)>0 ) EXIT SPAG_Loop_1_1
               IF ( it>0 .AND. kor(ln+i)<0 ) THEN
                  j = kor(ln)
                  WRITE (nout,99005) ufm , t1(1,j) , t1(2,j)
99005             FORMAT (A23,'316, ILLEGAL TO USE SORTED COUNT AND REGULAR ID ON ','SAME TYPE OF CARD ',2A4)
               ENDIF
               it = kor(ln+i)
               ics = kor(ln+i+1)
               ik = kor(ln+i+2)
            ENDDO SPAG_Loop_1_1
         ENDIF
!
!     PUT OUT CARDS SORT TYPE OF IDS (NEG) DO IN REVERSE
!     FIND VALUES OF SK FOR EACH K
!
         n = 6
         i = ln
         IF ( kor(ln+1)<=0 ) THEN
            n = -6
            i = ln + isv - 6
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
         a = 0.0
         IF ( kor(i+3)==jrun .AND. lst==0 ) a = 1.0
         IF ( lst/=0 ) THEN
            SPAG_Loop_1_2: DO k = 1 , lst , 2
               IF ( kor(i+3)==kor(ii+k) ) THEN
                  a = z(ii+k+1)
                  EXIT SPAG_Loop_1_2
               ENDIF
            ENDDO SPAG_Loop_1_2
         ENDIF
         z(i+3) = a
         it = kor(i+1)
         ics = kor(i+2)
         IF ( it<=0 .OR. ics/=2 ) THEN
            j = ics/10
            j = j*10
            IF ( j/=ics ) THEN
               j = (ics-1)/10
               j = j*10
               IF ( j/=ics-1 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         abort = .TRUE.
         j = kor(ln)
         WRITE (nout,99006) ufm , t1(1,j) , t1(2,j) , it , ics
99006    FORMAT (A23,'31, CARD TYPE ',2A4,' ID =',I9,' HAS ILLEGAL FIELD',I9)
         spag_nextblock_1 = 12
      CASE (12)
         IF ( .NOT.(abort .OR. a==0.0) ) CALL write(ifil,kor(i),6,0)
         i = i + n
         isv = isv - iabs(n)
         IF ( isv>0 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         isv = 0
         IF ( idon/=1 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
         IF ( jrun/=0 ) THEN
            CALL write(ifil,0,0,1)
            CALL close(ifil,1)
            Ipvs = 1
         ENDIF
         spag_nextblock_1 = 14
      CASE (14)
         IF ( istop==0 ) RETURN
         IF ( istop==1 ) RETURN 1
         spag_nextblock_1 = 15
      CASE (15)
!
!     ERROR MESSAGES
!
         abort = .TRUE.
         IF ( idon/=1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 20      WRITE (nout,99007) ufm
99007    FORMAT (A23,', NO BULK DATA CARDS TO MODIFY.  ERROR IN IFPPVC')
         istop = 1
         spag_nextblock_1 = 13
      CASE (16)
         i = knt + 1
         WRITE (nout,99008) ufm , m1(1) , m1(2) , i , Jr
99008    FORMAT (A23,' 317, ILLEGAL DATA OR FORMAT ON CARD ',2A4,' SORTED',I8,/5X,'CARD- ',20A4)
         spag_nextblock_1 = 15
      CASE (17)
         WRITE (nout,99009) ufm
99009    FORMAT (A23,' 3008, NOT ENOUGH CORE FOR 1PARM AND 1VARY CARDS')
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 40      CALL mesage(-1,ifil,name)
         spag_nextblock_1 = 18
      CASE (18)
         WRITE (nout,99010) ufm
99010    FORMAT (A23,', NO 1VARY CARDS TO GO WITH 1PARM CARDS.  ERROR IN ','IFPPVC')
         IF ( isort==1 .OR. ltj/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 15
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ifppvc
