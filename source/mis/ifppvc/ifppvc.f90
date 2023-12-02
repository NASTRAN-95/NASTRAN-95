!*==ifppvc.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifppvc(Ipvs,Jr) !HIDESTARS (*,Ipvs,Jr)
   IMPLICIT NONE
   USE C_IFPDTA
   USE C_IFPX1
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
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
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
!
!     READ NEW CARD
!
         CALL read(*20,*20,nptp,Jr,20,1,kdum)
         Knt = Knt + 1
         spag_nextblock_1 = 3
      CASE (3)
         it = khrfn1(blank,4,Jr(1),1)
         IF ( it==iplus .OR. it==istar ) THEN
            WRITE (Nout,99001) Ufm , Jr
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
            IF ( Jrun==0 ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( nv==0 .AND. Jrun>0 ) THEN
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
            IF ( l42==0 ) CALL rcard2(M1,M1f,nw,Jr)
            IF ( l42/=0 ) CALL rcard(M1,M1f,nw,Jr)
            IF ( nw/=10 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     CHECK FORMAT
!
            IF ( M1f(2)/=1 .OR. M1(3)<0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M1(3)<Jrun ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M1f(3)/=0 .AND. M1f(3)/=1 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M1f(5)/=0 .AND. M1f(5)/=1 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M1f(7)/=0 .AND. M1f(7)/=1 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M1(4)<0 .OR. M1(6)<0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M1(8)<0 .OR. M1f(9)/=0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M1f(4)/=2 .AND. M1f(4)/=0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M1f(6)/=2 .AND. M1f(6)/=0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M1f(8)/=2 .AND. M1f(8)/=0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Jrun==0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M1(3)/=Jrun .AND. isort==0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M1(3)/=Jrun ) THEN
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
         IF ( ipp>=Nopen ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = 3 , 7 , 2
            IF ( M1f(i)==0 .AND. M1f(i+1)/=0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M1f(i)/=0 ) THEN
               Kor(ipp) = M1(i+1)
               Kor(ipp+1) = M1(i+2)
               ipp = ipp + 2
            ENDIF
         ENDDO
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
!
!     SORT LIST ERROR IF DUPLICATE
!
         i = ln
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
99002             FORMAT (A23,' 314, DUPLICATE OR NO K  ON 1PARM CARDS FOR SOME J ','K =',I9)
               ENDIF
               it = Kor(i+k)
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
         IF ( Jrun/=1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ics==0 ) THEN
            ics = 1
            ltj = M1(3)
         ENDIF
         IF ( ltj==M1(3) ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ltj = M1(3)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
!
!     IF LST = 0 USE ALL DEFAULT VALUES FOR SK
!
         ltj = 0
         nv = nv + 1
         Jr(1) = ivar
         IF ( l42==0 ) CALL rcard2(M1,M1f,nw,Jr)
         IF ( l42/=0 ) CALL rcard(M1,M1f,nw,Jr)
         IF ( nw<10 .OR. nw>12 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CHECK FORMAT
!
         IF ( M1f(2)/=3 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M1f(3)/=1 .OR. M1f(4)/=1 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M1(5)<=0 .OR. M1(6)<=0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M1f(5)/=0 .AND. M1f(5)/=2 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M1f(6)/=0 .AND. M1f(6)/=2 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M1f(7)/=0 .AND. M1f(7)/=1 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M1f(8)/=0 .AND. M1f(8)/=1 .AND. M1f(8)/=3 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M1f(9)/=0 .AND. M1f(9)/=1 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M1f(7)==0 .AND. M1f(8)==0 .AND. M1f(9)==0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M1f(7)==1 .AND. M1(9)==0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M1f(8)==1 .AND. M1(10)==0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i = 0
         IF ( M1f(8)==3 ) i = 1
         IF ( M1f(9)==1 .AND. M1(i+11)==0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M1f(8)==3 .AND. M1(10)/=ithr ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M1f(8)==3 .AND. M1(9)>0 .AND. M1(12)<0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M1f(8)==3 .AND. M1(9)<0 .AND. M1(12)>0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Jrun==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO Kn = 1 , ncdsmx
            IF ( M1(3)==T1(1,Kn) .AND. M1(4)==T1(2,Kn) ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         WRITE (Nout,99003) Ufm , M1(3) , M1(4)
99003    FORMAT (A23,'316, CARD TYPE ',2A4,' NOT LEGAL ON 1VARY')
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
         IF ( Kn/=ioldn .AND. ioldn/=0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         ioldn = Kn
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
            IF ( lst+isv+(iabs(n2-n1)*6)>Nopen ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO
               Kor(ln+isv) = Kn
               Kor(ln+isv+1) = n1
               Kor(ln+isv+2) = ifield
               Kor(ln+isv+3) = k
               Kor(ln+isv+4) = ia
               Kor(ln+isv+5) = ib
               isv = isv + 6
               n1 = n1 + 1
               IF ( n1>n2 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ELSE
            IF ( lst+isv+18>Nopen ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
            IF ( lst+isv>Nopen ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL open(*40,ifil,Kor(ibuf1+1),1)
         ENDIF
!
!     TEST FOR DUPLICATE K FOR SAME FIELD AND ID PLUS SORT AND REG
!
         IF ( isv/=6 ) THEN
            it = Kor(ln+1)
            ics = Kor(ln+2)
            ik = Kor(ln+3)
            SPAG_Loop_1_1: DO i = 7 , isv , 6
               IF ( it==Kor(ln+i) .AND. ics==Kor(ln+i+1) .AND. ik==Kor(ln+i+2) ) THEN
                  Abort = .TRUE.
                  WRITE (Nout,99004) Ufm , it , ics , ik
99004             FORMAT (A23,'314, DUPLICATE K FOR  ID',I9,' FIELD',I9,' K',I9)
               ENDIF
               IF ( it<0 .AND. Kor(ln+i)>0 ) EXIT SPAG_Loop_1_1
               IF ( it>0 .AND. Kor(ln+i)<0 ) THEN
                  j = Kor(ln)
                  WRITE (Nout,99005) Ufm , T1(1,j) , T1(2,j)
99005             FORMAT (A23,'316, ILLEGAL TO USE SORTED COUNT AND REGULAR ID ON ','SAME TYPE OF CARD ',2A4)
               ENDIF
               it = Kor(ln+i)
               ics = Kor(ln+i+1)
               ik = Kor(ln+i+2)
            ENDDO SPAG_Loop_1_1
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
         spag_nextblock_1 = 11
      CASE (11)
         a = 0.0
         IF ( Kor(i+3)==Jrun .AND. lst==0 ) a = 1.0
         IF ( lst/=0 ) THEN
            SPAG_Loop_1_2: DO k = 1 , lst , 2
               IF ( Kor(i+3)==Kor(ii+k) ) THEN
                  a = z(ii+k+1)
                  EXIT SPAG_Loop_1_2
               ENDIF
            ENDDO SPAG_Loop_1_2
         ENDIF
         z(i+3) = a
         it = Kor(i+1)
         ics = Kor(i+2)
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
         Abort = .TRUE.
         j = Kor(ln)
         WRITE (Nout,99006) Ufm , T1(1,j) , T1(2,j) , it , ics
99006    FORMAT (A23,'31, CARD TYPE ',2A4,' ID =',I9,' HAS ILLEGAL FIELD',I9)
         spag_nextblock_1 = 12
      CASE (12)
         IF ( .NOT.(Abort .OR. a==0.0) ) CALL write(ifil,Kor(i),6,0)
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
         IF ( Jrun/=0 ) THEN
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
         Abort = .TRUE.
         IF ( idon/=1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 20      WRITE (Nout,99007) Ufm
99007    FORMAT (A23,', NO BULK DATA CARDS TO MODIFY.  ERROR IN IFPPVC')
         istop = 1
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
      CASE (16)
         i = Knt + 1
         WRITE (Nout,99008) Ufm , M1(1) , M1(2) , i , Jr
99008    FORMAT (A23,' 317, ILLEGAL DATA OR FORMAT ON CARD ',2A4,' SORTED',I8,/5X,'CARD- ',20A4)
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
      CASE (17)
         WRITE (Nout,99009) Ufm
99009    FORMAT (A23,' 3008, NOT ENOUGH CORE FOR 1PARM AND 1VARY CARDS')
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 40      CALL mesage(-1,ifil,name)
         spag_nextblock_1 = 18
      CASE (18)
         WRITE (Nout,99010) Ufm
99010    FORMAT (A23,', NO 1VARY CARDS TO GO WITH 1PARM CARDS.  ERROR IN ','IFPPVC')
         IF ( isort==1 .OR. ltj/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ifppvc
