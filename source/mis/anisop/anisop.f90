!*==anisop.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE anisop
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3) :: a , b , c , xp , yp , zp
   INTEGER , SAVE :: bgpdt , ept , eqexin , geom1 , mpt , mpta
   INTEGER :: buf1 , buf2 , file , i , ibgpdt , icid , icord , idx , ieq , igrid , ii , iii , ijk , im , ip , ip4 , ipoint , isubb ,&
            & isubk , isubl , j , jj , jp , k , k3 , l , lcore , m , mcore , mid , n , ncid , ncjj , ncord1 , ncord2 , nmat1 ,      &
            & nmat6 , nn , num
   REAL :: dl
   INTEGER , DIMENSION(2) , SAVE :: ic1 , ic2 , imat6 , ipi , mat1 , nam
   INTEGER , DIMENSION(31) :: idum
   INTEGER , DIMENSION(7) :: itrl
   INTEGER , DIMENSION(1) :: iz
   REAL , DIMENSION(9) :: store , xd
   EXTERNAL bisloc , close , gopen , korsz , locate , mesage , preloc , rdtrl , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     COMPUTES DIRECTION COSINES FOR RECTANGULAR COORD. SYSTEMS
!     (W.R.T. BASIC COORD. SYSTEM) DESCRIBING ORIENTATION OF ANIS.
!     MATERIAL FOR ISOPARAMETRIC SOLIDS
!
!     ANISOP  GEOM1,EPT,BGPDT,EQEXIN,MPT/MPTA/S,N,ISOP $
!     EQUIV   MPTA,MPT/ISOP $
!     ISOP=-1 MEANS SUCH MATERIALS EXIST
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA geom1 , ept , bgpdt , eqexin , mpt , mpta/101 , 102 , 103 , 104 , 105 , 201/
   DATA ipi/7002 , 70/ , ic1/1801 , 18/ , ic2/2101 , 21/ , imat6/2503 , 25/ , mat1/103 , 1/
   DATA nam/4HANIS , 4HOP  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         Isop = 1
         lcore = korsz(Z)
         buf1 = lcore - Ibuf - 1
         buf2 = buf1 - Ibuf
         lcore = buf2 - 1
         IF ( lcore<=0 ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     GET LIST OF MAT1 AND MAT6 ID-S
!
         nmat1 = 0
         nmat6 = 0
         i = 0
!WKBI SPR93033 5/94
         file = mpt
         CALL preloc(*260,Z(buf1),mpt)
         CALL locate(*20,Z(buf1),mat1,idx)
         DO
!WKBD SPR93022 5/94      FILE  = MPT
            CALL read(*280,*20,mpt,idum,12,0,m)
            nmat1 = nmat1 + 1
            i = i + 1
            IF ( i>lcore ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            iz(i) = idum(1)
         ENDDO
!
!     DONE WITH MAT1
!
 20      CALL locate(*40,Z(buf1),imat6,idx)
         DO
            CALL read(*280,*40,mpt,idum,31,0,m)
            nmat6 = nmat6 + 1
            i = i + 1
            IF ( i>lcore ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            iz(i) = idum(1)
         ENDDO
 40      CALL close(mpt,1)
!
!     LOCATE PIHEX CARDS ON EPT AND FORM A LIST OF MATERIAL COORD.
!     SYSTEM ID
!
!WKBI SPR93033 5/94
         file = ept
         CALL preloc(*220,Z(buf1),ept)
         CALL locate(*220,Z(buf1),ipi,idx)
         spag_nextblock_1 = 2
      CASE (2)
!WKBD SPR93033 5/94      FILE = EPT
         CALL read(*280,*60,ept,idum,7,0,m)
!
         icid = idum(3)
         mid = idum(2)
!
!     IF CID = 0, MID MUST BE MAT1
!     IF CID IS NOT 0, MID MUST BE MAT6
!
         IF ( icid>0 ) THEN
            IF ( nmat6==0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ii = nmat1 + 1
            nn = nmat1 + nmat6
         ELSE
            IF ( nmat1==0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ii = 1
            nn = nmat1
         ENDIF
         DO iii = ii , nn
            IF ( mid==iz(iii) ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
         WRITE (Nout,99001) Ufm , mid
99001    FORMAT (A23,', MATERIAL',I8,', SPECIFIED ON A PIHEX CARD, DOES ','NOT REFERENCE THE PROPER MATERIAL TYPE',/5X,             &
                &'CID = 0 MEANS MAT1, CID NOT 0 MEANS MAT6')
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!     STORE ALL CID,MID PAIRS WHERE CID IS NOT 0
!
         IF ( icid/=0 ) THEN
            i = i + 1
            iz(i) = icid
            i = i + 1
            iz(i) = mid
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     LIST IS MADE. MOVE IT UP TO IZ(1)
!
 60      CALL close(ept,1)
         ncid = i - nmat1 - nmat6
         IF ( ncid==0 ) RETURN
!
         DO ii = 1 , ncid
            jj = nmat1 + nmat6 + ii
            iz(ii) = iz(jj)
         ENDDO
!
!     NOW MAKE A UNIQUE LIST OF CID-S
!
         ijk = ncid + 1
         num = 1
         iz(ijk) = iz(1)
         IF ( ncid/=2 ) THEN
            SPAG_Loop_1_1: DO ii = 3 , ncid , 2
               icid = iz(ii)
               DO jj = 1 , num
                  ncjj = ncid + jj
                  IF ( icid==iz(ncjj) ) CYCLE SPAG_Loop_1_1
               ENDDO
!
!     UNIQUE - LIST IT
!
               ijk = ijk + 1
               num = num + 1
               IF ( ijk>lcore ) THEN
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               iz(ijk) = icid
            ENDDO SPAG_Loop_1_1
         ENDIF
!
!     UNIQUE LIST IS MADE- CHECK AGAINST CORD1R AND CORD2R ID-S
!
         icord = ncid + num + 1
!
         ncord1 = 0
         ncord2 = 0
         file = geom1
         CALL preloc(*260,Z(buf1),geom1)
         CALL locate(*80,Z(buf1),ic1,idx)
         spag_nextblock_1 = 5
      CASE (5)
         DO WHILE ( icord+12<=lcore )
            CALL read(*280,*80,geom1,Z(icord),6,0,m)
!
!     COMPARE AGAINST CIDS ON PIHEX-S
!
            DO jj = 1 , num
               j = ncid + jj
               IF ( iz(icord)==iz(j) ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDDO
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
!
!     MATCH- RESERVE 13 WORDS SINCE THIS CORD1R WILL BE CONVERTED TO
!     CORD2R TYPE ENTRY LATER
!
         iz(j) = -iz(j)
         ncord1 = ncord1 + 1
         icord = icord + 13
         IF ( ncord1==num ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     TRY CORD2R
!
 80      CALL locate(*100,Z(buf1),ic2,idx)
         spag_nextblock_1 = 7
      CASE (7)
         DO WHILE ( icord+12<=lcore )
            CALL read(*280,*100,geom1,Z(icord),13,0,m)
!
!     COMPARE
!
            DO jj = 1 , num
               j = ncid + jj
               IF ( iz(icord)==iz(j) ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDDO
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
!
!     MATCH ON CORD2R. CHECK FOR RID. MUST BE 0
!
         IF ( iz(icord+3)/=0 ) THEN
            WRITE (Nout,99002) Ufm , iz(j)
99002       FORMAT (A23,', CORD2R',I8,' DEFINES A PIHEX CID BUT HAS NONZERO',' RID')
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
            iz(j) = -iz(j)
            ncord2 = ncord2 + 1
            icord = icord + 13
            IF ( ncord1+ncord2==num ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     EXHAUSTED CORD2R-S, BUT NOT ALL  CID-S ARE LOCATED
!
 100     DO jj = 1 , num
            j = ncid + jj
            IF ( iz(j)>=0 ) THEN
               WRITE (Nout,99003) Ufm , iz(j)
99003          FORMAT (A23,', CID',I8,' ON A PIHEX CARD IS NOT DEFINED TO BE ','CORD1R OR CORD2R')
            ENDIF
         ENDDO
         spag_nextblock_1 = 9
      CASE (9)
         CALL mesage(-61,0,nam)
         spag_nextblock_1 = 10
      CASE (10)
!
!
!     MATCHING IS COMPLETE
!
         CALL close(geom1,1)
!
!     CID,MATID PAIRS ARE IN Z(1)-Z(NCID). UNIQUE CID LIST IS IN
!     Z(NCID+1)-Z(NCID+NUM). THERE ARE NCORD1 CORD1R-S AND NCORD2
!     CORD2R-S AT 13 WORDS EACH STARTING AT Z(NCID+NUM+1).
!     NEXT AVAILABLE OPEN CORE IS AT Z(ICORD)
!
         DO jj = 1 , num
            j = ncid + jj
            iz(j) = -iz(j)
         ENDDO
!
!     FOR CID-S ON CORD1R WE MUST OBTAIN THE BASIC COORDINATES OF EACH
!     POINT FROM BGPDT. FIRST, THE EXTERNAL POINT NUMBERS ON CORD1R MUST
!     BE CONVERTED TO  INTERNAL.
!
         lcore = lcore - (icord-1)
         IF ( lcore<=0 ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         mcore = lcore
         ibgpdt = icord
         IF ( ncord1==0 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(bgpdt,Z(buf1),0)
         file = bgpdt
         CALL read(*280,*120,bgpdt,Z(ibgpdt),lcore,0,m)
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 120     CALL close(bgpdt,1)
         ieq = ibgpdt + m
         lcore = lcore - m
         CALL gopen(eqexin,Z(buf1),0)
         file = eqexin
         CALL read(*280,*140,eqexin,Z(ieq),lcore,0,m)
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 140     CALL close(eqexin,1)
         lcore = lcore - m
!
!     FOR EACH CORD1R ENTRY, FIND THE BASIC COORDINATES FOR EACH POINT
!     AND FORM A CORD2R ENTRY BACK WHERE THE CORD1R IS STORED
!
         DO j = 1 , ncord1
            ipoint = 13*(j-1) + ncid + num
            icid = iz(ipoint+1)
            DO k = 1 , 3
               isubk = ipoint + 3 + k
               k3 = 3*(k-1)
               igrid = iz(isubk)
               CALL bisloc(*240,igrid,Z(ieq),2,m/2,jp)
!
!     IM IS POINTER TO INTERNAL NUMBER. NOW FIND BGPDT ENTRY
!
               im = ieq + jp
               ip = 4*(iz(im)-1)
               DO l = 1 , 3
                  isubb = ibgpdt + ip + l
                  isubl = k3 + l
                  store(isubl) = Z(isubb)
               ENDDO
            ENDDO
!
!     WE HAVE THE BASIC COORDINATES OF THE 3 POINTS. STORE IT BACK INTO
!     THE CORD1R ENTRY. THE ENTRY STARTS AT Z(IPOINT+1)
!
            ip4 = ipoint + 4
            iz(ip4) = 0
            DO l = 1 , 9
               isubl = ip4 + l
               Z(isubl) = store(l)
            ENDDO
!
!     GO BACK FOR ANOTHER CORD1R
!
         ENDDO
         spag_nextblock_1 = 11
      CASE (11)
!
!     FOR EACH COORDINATE SYSTEM, COMPUTE THE 9 DIRECTION COSINES FROM
!     THE BASIC COORDINATE SYSTEM. Z(ICORD) IS THE NEXT AVAILABLE
!     LOCATION OF OPEN CORE SINCE WE NO LONGER NEED EQEXIN OR BGPDT
!     INFO.
!
         lcore = mcore
         CALL gopen(mpt,Z(buf1),0)
         CALL gopen(mpta,Z(buf2),1)
         IF ( icord+30>lcore ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     COPY MPT TO MPTA UNTIL MAT6 IS REACHED
!
         file = mpt
         spag_nextblock_1 = 12
      CASE (12)
         CALL read(*200,*300,mpt,Z(icord),3,0,m)
         CALL write(mpta,Z(icord),3,0)
         IF ( iz(icord)==imat6(1) ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO
            CALL read(*280,*160,mpt,Z(icord),lcore,0,m)
            CALL write(mpta,Z(icord),lcore,0)
         ENDDO
 160     CALL write(mpta,Z(icord),m,1)
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
      CASE (13)
!
!     MAT6 RECORD FOUND. EACH MAT6 CONTAINS 31 WORDS. INCREASE THAT
!     TO 40
!
         CALL read(*280,*180,mpt,Z(icord),31,0,m)
!
!     SEE IF THIS ID MATCHES A CID ON PIHEX) IT NEED NOT
!
         DO j = 2 , ncid , 2
            IF ( iz(j)==iz(icord) ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     NO MATCH. MAT6 NOT REFERENCED BY PIHEX. COPY IT TO MAT6 AND FILL
!     IN ALL 3 DIRECTION COSINES. THIS MAT6 IS NOT REFERENCED BY PIHEX
!
         DO k = 1 , 9
            xd(k) = 0.
         ENDDO
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
      CASE (14)
!
!     MATCH. NOW FIND IT IN CORD1R,CORD2R LIST
!
         icid = iz(j-1)
         DO ii = 1 , num
            ipoint = ncid + num + 13*(ii-1)
            IF ( icid==iz(ipoint+1) ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     LOGIC ERROR
!
         ENDDO
         WRITE (Nout,99004) Ufm
99004    FORMAT (A23,', NON-UNIQUE COORDINATE SYSTEMS ON PIHEX CARDS',/5X,'(SEE USER MANUAL P.2.4-233(05/30/86))')
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (15)
!
         iz(ipoint+1) = -iz(ipoint+1)
         a(1) = Z(ipoint+5)
         a(2) = Z(ipoint+6)
         a(3) = Z(ipoint+7)
         b(1) = Z(ipoint+8)
         b(2) = Z(ipoint+9)
         b(3) = Z(ipoint+10)
         c(1) = Z(ipoint+11)
         c(2) = Z(ipoint+12)
         c(3) = Z(ipoint+13)
!
!     ZP AXIS IS B-A. YP IS ZP X (C-A). XP IS YP X ZP
!
         zp(1) = b(1) - a(1)
         zp(2) = b(2) - a(2)
         zp(3) = b(3) - a(3)
         store(1) = c(1) - a(1)
         store(2) = c(2) - a(2)
         store(3) = c(3) - a(3)
         yp(1) = zp(2)*store(3) - zp(3)*store(2)
         yp(2) = zp(3)*store(1) - zp(1)*store(3)
         yp(3) = zp(1)*store(2) - zp(2)*store(1)
         xp(1) = yp(2)*zp(3) - yp(3)*zp(2)
         xp(2) = yp(3)*zp(1) - yp(1)*zp(3)
         xp(3) = yp(1)*zp(2) - yp(2)*zp(1)
!
!     NOW COMPUTE DIRECTION COSINES BETWEEN XP,YP, ZP AND BASIC X,Y,Z
!     X=(1,0,0),Y=(0,1,0),Z=(0,0,1)
!     COS(THETA)=(DP.D)/(LENGTH OF DP)*(LENGTH OF D) WHERE DP=XP,YP,OR
!     ZP   AND D=X,Y,OR Z. LENGTH OF D=1
!
         dl = sqrt(xp(1)**2+xp(2)**2+xp(3)**2)
         xd(1) = xp(1)/dl
         xd(2) = xp(2)/dl
         xd(3) = xp(3)/dl
         dl = sqrt(yp(1)**2+yp(2)**2+yp(3)**2)
         xd(4) = yp(1)/dl
         xd(5) = yp(2)/dl
         xd(6) = yp(3)/dl
         dl = sqrt(zp(1)**2+zp(2)**2+zp(3)**2)
         xd(7) = zp(1)/dl
         xd(8) = zp(2)/dl
         xd(9) = zp(3)/dl
         spag_nextblock_1 = 16
      CASE (16)
!
!     WRITE OUT NEW MAT6 RECORD WITH DIRECTION COSINES APPENDED
!
         CALL write(mpta,Z(icord),31,0)
!
!     GET ANOTHER MAT6
!
         CALL write(mpta,xd,9,0)
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
!
!     MAT6 RECORD FINISHED. WRITE EOR, COPY REMAINDER OF MPT, AND CHECK
!     TO SEE THAT ALL PIHEX CID-S HAVE BEEN ACCOUNTED FOR.
!
 180     CALL write(mpta,0,0,1)
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
!
!     MPT EXHAUSTED
!
 200     CALL close(mpt,1)
         CALL close(mpta,1)
         itrl(1) = mpt
         CALL rdtrl(itrl)
         itrl(1) = mpta
         CALL wrttrl(itrl)
         Isop = -1
         spag_nextblock_1 = 17
      CASE (17)
         RETURN
!
!WKBDB 5/94 SPR 93033
!  310 WRITE  (NOUT,320) UWM
!  320 FORMAT (A25,', EITHER EPT IS PURGED OR NO PIHEX CARDS FOUND ON ',
!     1       'EPT IN ANISOP')
!WKBDE 5/94 SPR 93033
!WKBI  SPR 93033 5/94
 220     CALL close(ept,1)
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
 240     WRITE (Nout,99005) Ufm , igrid
99005    FORMAT (A23,', EXTERNAL GRID',I8,' CANNOT BE FOUND ON EQEXIN IN ','ANISOP')
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
!WKBDB SPR93033 5/94
! 1001 N = -1
!      GO TO 1010
!WKBDE SPR93033 5/94
!WKBIB SPR93033 5/94
 260     CALL close(mpt,1)
         CALL close(geom1,1)
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
!WKBIE SPR93033 5/94
 280     n = -2
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
 300     n = -3
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
      CASE (18)
         file = 0
         n = -8
         spag_nextblock_1 = 19
      CASE (19)
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE anisop
