!*==ifpmdc.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifpmdc
   USE c_ifpdta
   USE c_ifpx0
   USE c_ifpx1
   USE c_machin
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_xsrtcm
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: cd
   LOGICAL :: cf
   INTEGER :: cnt , exi , i , ibuf1 , ilst , iod , ionf , ipos , isc , ists , itm , j , ji , k , k1 , karl , l , ncore , nf , nw ,  &
            & ret , test
   INTEGER , DIMENSION(38) , SAVE :: con
   LOGICAL , SAVE :: diag
   INTEGER , DIMENSION(6) :: ick
   INTEGER , SAVE :: icycl , iefm , iend , ieof , ifil
   INTEGER , DIMENSION(2) :: inc , ivc , xi
   REAL , DIMENSION(1) :: rm , rm1
   EXTERNAL andf , close , lshift , open , orf , read , rshift
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     IFPMDC MODIFIES BULK DATA CARDS GIVEN THE INFORMATION ON IFIL
!
   !>>>>EQUIVALENCE (Rm(1),M(1)) , (Rm1(1),M1(1)) , (ick(1),cd(1)) , (k,ick(1))
   DATA con/4H     , 4H   0 , 4H   1 , 4H   2 , 4H   3 , 4H   4 , 4H   5 , 4H   6 , 4H   7 , 4H   8 , 4H   9 , 4H   A , 4H   B ,    &
       &4H   C , 4H   D , 4H   E , 4H   F , 4H   G , 4H   H , 4H   I , 4H   J , 4H   K , 4H   L , 4H   M , 4H   N , 4H   O ,        &
      & 4H   P , 4H   Q , 4H   R , 4H   S , 4H   T , 4H   U , 4H   V , 4H   W , 4H   X , 4H   Y , 4H   Z , 4H    /
   DATA ifil , ieof , icycl , iefm , iend , diag/213 , 0 , 0 , -32767 , 4HZZZZ , .FALSE./
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( ieof==-1 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         cnt = 0
         IF ( ieof==1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     FIRST CALL INITIALIZE OPEN FILE ADJUST CORE
!
         ibuf1 = nopen + 2*ibuf
         nopen = nopen - ibuf
         DO i = 1 , 38
            con(i) = andf(con(i),im3(4))
         ENDDO
         IF ( nopen<=0 ) THEN
            WRITE (nout,99001) ufm
99001       FORMAT (A23,' 303, NO OPEN CORE IFP')
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSE
            cf = .FALSE.
            iod = 0
            isc = 0
            nf = 0
            ionf = 0
            ilst = 0
            ieof = 1
            CALL open(*140,ifil,kor(ibuf1+1),0)
         ENDIF
 20      CALL read(*120,*120,ifil,ick,6,0,nw)
         spag_nextblock_1 = 2
      CASE (2)
!
!     CHECK INCOMING  CALL FOR VARY MATCH SORT, UNSORT AND/OR CONT
!
         IF ( k/=kn ) THEN
!
!     NOT CARD WE ARE WORKING ON CHECK ALPH POSITION
!
            IF ( cf .OR. iod==kn ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            iod = kn
            isc = 0
            ASSIGN 40 TO exi
            xi(1) = t1(1,k)
            xi(2) = t1(2,k)
            spag_nextblock_1 = 6
!
!     CARD TYPE FOUND TRY ID
!
         ELSEIF ( ick(2)<0 ) THEN
!
!     SORTED TYPE OF IDS NEED TO COUNT PARENTS IN THE GROUP
!
            IF ( cf .AND. nf/=0 .AND. isc==ick(2) .AND. cnt==1 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( cf .AND. nf/=0 .AND. isc==ick(2) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( cf ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( cnt/=1 ) THEN
               cnt = 1
               nf = 0
               ionf = 0
               ASSIGN 100 TO ret
               isc = isc - 1
            ENDIF
            IF ( isc>ick(2) ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( isc<ick(2) ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 3
         ELSE
            IF ( cf .AND. nf/=0 .AND. ilst==ick(2) .AND. cnt==1 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( cf .AND. nf/=0 .AND. ilst==ick(2) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( cf ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            nf = 0
            ionf = 0
            ASSIGN 20 TO ret
            IF ( m(1)<ick(2) ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m(1)>ick(2) ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ilst = ick(2)
            spag_nextblock_1 = 3
         ENDIF
         CYCLE
 40      ivc(1) = xi(1)
         ivc(2) = xi(2)
         ASSIGN 60 TO exi
         xi(1) = t1(1,kn)
         xi(i) = t1(2,kn)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 60      inc(1) = xi(1)
         inc(2) = xi(2)
         IF ( mach/=2 ) THEN
            inc(1) = rshift(inc(1),1)
            ivc(1) = rshift(ivc(1),1)
         ENDIF
         IF ( inc(1)<ivc(1) ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( inc(1)>ivc(1) ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SHIFT IN CASE OF STAR
!
         inc(2) = rshift(inc(2),nbits)
         ivc(2) = rshift(ivc(2),nbits)
         IF ( inc(2)<ivc(2) ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (3)
!
!     FIND FIELD FORMAT DOES NOT COUNT FOR FIELD  1 OR 10 K1=COUNT
!
         DO i = 1 , 50
            IF ( mf(i)==iefm ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         GOTO 140
      CASE (4)
         nf = nf + i - 1
         cnt = 1
         spag_nextblock_1 = 5
      CASE (5)
         k1 = ick(3)
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
            IF ( m1(1)/=0 .AND. m1(2)/=0 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     CHECK FORMAT FIELD FOR TYPE
!
            k1 = k1 - ionf
            IF ( mf(k1)/=2 .AND. mf(k1)/=0 ) THEN
               WRITE (nout,99002) ufm , t1(1,k) , t1(2,k) , knt , ick(2) , ick(3)
99002          FORMAT (A23,' 0301, FIELD TO VARY IS NOT A REAL NUMBER. CARD ',2A4,'SORTED',I9,' ID',I9,' FIELD',I9)
               abort = .TRUE.
               GOTO ret
            ELSE
               j = 0
               DO i = 1 , k1
                  j = j + 1
                  IF ( mf(i)>2 ) j = j + 1
               ENDDO
!
!     PERFORM VARY
!
               IF ( cd(6)==0.0 ) THEN
                  rm(j) = rm(j) + cd(4)*cd(5)
                  mf(k1) = 2
                  IF ( diag ) WRITE (nout,99004) uim , t1(1,k) , t1(2,k) , knt , ick(2) , ick(3) , rm(j)
               ELSE
                  rm(j) = rm(j)*(1.0+cd(4)*cd(5))**cd(6)
                  IF ( diag ) WRITE (nout,99004) uim , t1(1,k) , t1(2,k) , knt , ick(2) , ick(3) , rm(j)
               ENDIF
!
!     SET RESTART BITS
!
               IF ( apprch>=0 ) GOTO 80
!
!     CHECK FOR PARAM CARDS (82)
!
               IF ( kn/=82 ) THEN
                  j = kn - 1
                  GOTO 70
               ELSE
                  DO i = iparpt , ncds
                     IF ( m(1)==t1(1,i) .AND. m(2)==t1(2,i) ) GOTO 65
                  ENDDO
                  GOTO 80
               ENDIF
 65            j = i - 1
 70            karl = 1
               IF ( icycl==0 ) ibits(karl) = orf(ibits(karl),rshift(1,(x-1)))
               icycl = (j/31) + karl
               ipos = mod(j,31) + 2
               ibits(icycl) = orf(ibits(icycl),itwo(ipos))
            ENDIF
 80         GOTO ret
         ENDIF
!
!     FOUND ID FIND FIELD
!
 100     CALL read(*120,*120,ifil,ick,6,0,nw)
         IF ( k==kn .AND. nf/=0 .AND. isc==ick(2) ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (6)
!
!     CHANGE EXTERNAL BCD TO INTERNAL BCD FOR SORT TEST
!
         SPAG_Loop_1_2: DO i = 1 , 2
            itm = xi(i)
            SPAG_Loop_2_1: DO j = 1 , 4
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     ji = 5 - j
                     ists = isft(ji)
                     test = rshift(andf(itm,im3(j)),ists)
                     DO l = 1 , 37
                        IF ( test==con(l) ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                     l = 1
                     EXIT SPAG_Loop_2_1
                  CASE (2)
                     itm = orf(andf(itm,im4(j)),lshift(l,ists+isfim))
                     IF ( l==1 ) EXIT SPAG_Loop_2_1
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO SPAG_Loop_2_1
            xi(i) = itm
            IF ( l==1 ) EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
         GOTO exi
!
!     END OF IFIL
!
 120     CALL close(ifil,1)
         ieof = -1
         ncore = ncore + ibuf
         spag_nextblock_1 = 7
      CASE (7)
!
         cf = .FALSE.
         ionf = nf
         IF ( m1(1)==0 .AND. m1(2)==0 ) cf = .TRUE.
         IF ( m1(1)==iend ) THEN
!
!     LAST TIME ENTERED MAKE SURE FILE IS USED UP
!
            IF ( ieof>=0 ) THEN
               DO
!
!     IFP IS DONE BUT VARY IS NOT   MESSAGES FOR ANY LEFT
!
                  WRITE (nout,99005) ufm , t1(1,k) , t1(2,k) , ick(2) , ick(3)
                  CALL read(*120,*120,ifil,ick,6,0,nw)
               ENDDO
            ENDIF
         ENDIF
         RETURN
 140     WRITE (nout,99003) sfm
99003    FORMAT (A25,' 3037, ERROR IN IFPMDC')
         spag_nextblock_1 = 9
      CASE (8)
         WRITE (nout,99005) ufm , t1(1,k) , t1(2,k) , ick(2) , ick(3)
         GOTO ret
      CASE (9)
         abort = .TRUE.
         nopen = nopen + ibuf
         ieof = -1
         spag_nextblock_1 = 7
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
!     ERROR MESSAGES
!
99004 FORMAT (A29,' 3310, CARD TYPE ',2A4,' SORTED',I9,' ID',I9,' FIELD',I9,' CHANGED TO ',E16.8)
99005 FORMAT (A23,' 520, CARD TO VARY NOT FOUND. CARD ',2A4,' ID',I9,' FIELD',I9)
END SUBROUTINE ifpmdc
