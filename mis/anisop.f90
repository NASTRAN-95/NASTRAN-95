
SUBROUTINE anisop
   IMPLICIT NONE
   INTEGER Ibuf , Isop , Iz(1) , Nout
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   REAL Z(1)
   COMMON /blank / Isop
   COMMON /system/ Ibuf , Nout
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
   REAL a(3) , b(3) , c(3) , dl , store(9) , xd(9) , xp(3) , yp(3) , zp(3)
   INTEGER bgpdt , buf1 , buf2 , ept , eqexin , file , geom1 , i , ibgpdt , ic1(2) , ic2(2) , icid , icord , idum(31) , idx , ieq , &
         & igrid , ii , iii , ijk , im , imat6(2) , ip , ip4 , ipi(2) , ipoint , isubb , isubk , isubl , itrl(7) , j , jj , jp , k ,&
         & k3 , l , lcore , m , mat1(2) , mcore , mid , mpt , mpta , n , nam(2) , ncid , ncjj , ncord1 , ncord2 , nmat1 , nmat6 ,   &
         & nn , num
   INTEGER korsz
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
!
   Isop = 1
   lcore = korsz(Z)
   buf1 = lcore - Ibuf - 1
   buf2 = buf1 - Ibuf
   lcore = buf2 - 1
   IF ( lcore<=0 ) GOTO 3200
!
!     GET LIST OF MAT1 AND MAT6 ID-S
!
   nmat1 = 0
   nmat6 = 0
   i = 0
!WKBI SPR93033 5/94
   file = mpt
   CALL preloc(*2900,Z(buf1),mpt)
   CALL locate(*100,Z(buf1),mat1,idx)
   DO
!WKBD SPR93022 5/94      FILE  = MPT
      CALL read(*3000,*100,mpt,idum,12,0,m)
      nmat1 = nmat1 + 1
      i = i + 1
      IF ( i>lcore ) GOTO 3200
      Iz(i) = idum(1)
   ENDDO
!
!     DONE WITH MAT1
!
 100  CALL locate(*200,Z(buf1),imat6,idx)
   DO
      CALL read(*3000,*200,mpt,idum,31,0,m)
      nmat6 = nmat6 + 1
      i = i + 1
      IF ( i>lcore ) GOTO 3200
      Iz(i) = idum(1)
   ENDDO
 200  CALL close(mpt,1)
!
!     LOCATE PIHEX CARDS ON EPT AND FORM A LIST OF MATERIAL COORD.
!     SYSTEM ID
!
!WKBI SPR93033 5/94
   file = ept
   CALL preloc(*2700,Z(buf1),ept)
   CALL locate(*2700,Z(buf1),ipi,idx)
!WKBD SPR93033 5/94      FILE = EPT
 300  CALL read(*3000,*600,ept,idum,7,0,m)
!
   icid = idum(3)
   mid = idum(2)
!
!     IF CID = 0, MID MUST BE MAT1
!     IF CID IS NOT 0, MID MUST BE MAT6
!
   IF ( icid>0 ) THEN
      IF ( nmat6==0 ) GOTO 400
      ii = nmat1 + 1
      nn = nmat1 + nmat6
   ELSE
      IF ( nmat1==0 ) GOTO 400
      ii = 1
      nn = nmat1
   ENDIF
   DO iii = ii , nn
      IF ( mid==Iz(iii) ) GOTO 500
   ENDDO
 400  WRITE (Nout,99001) Ufm , mid
99001 FORMAT (A23,', MATERIAL',I8,', SPECIFIED ON A PIHEX CARD, DOES ','NOT REFERENCE THE PROPER MATERIAL TYPE',/5X,                &
             &'CID = 0 MEANS MAT1, CID NOT 0 MEANS MAT6')
   GOTO 1300
!
!     STORE ALL CID,MID PAIRS WHERE CID IS NOT 0
!
 500  IF ( icid/=0 ) THEN
      i = i + 1
      Iz(i) = icid
      i = i + 1
      Iz(i) = mid
   ENDIF
   GOTO 300
!
!     LIST IS MADE. MOVE IT UP TO IZ(1)
!
 600  CALL close(ept,1)
   ncid = i - nmat1 - nmat6
   IF ( ncid==0 ) RETURN
!
   DO ii = 1 , ncid
      jj = nmat1 + nmat6 + ii
      Iz(ii) = Iz(jj)
   ENDDO
!
!     NOW MAKE A UNIQUE LIST OF CID-S
!
   ijk = ncid + 1
   num = 1
   Iz(ijk) = Iz(1)
   IF ( ncid/=2 ) THEN
      DO ii = 3 , ncid , 2
         icid = Iz(ii)
         DO jj = 1 , num
            ncjj = ncid + jj
            IF ( icid==Iz(ncjj) ) GOTO 650
         ENDDO
!
!     UNIQUE - LIST IT
!
         ijk = ijk + 1
         num = num + 1
         IF ( ijk>lcore ) GOTO 3200
         Iz(ijk) = icid
 650  ENDDO
   ENDIF
!
!     UNIQUE LIST IS MADE- CHECK AGAINST CORD1R AND CORD2R ID-S
!
   icord = ncid + num + 1
!
   ncord1 = 0
   ncord2 = 0
   file = geom1
   CALL preloc(*2900,Z(buf1),geom1)
   CALL locate(*900,Z(buf1),ic1,idx)
 700  DO WHILE ( icord+12<=lcore )
      CALL read(*3000,*900,geom1,Z(icord),6,0,m)
!
!     COMPARE AGAINST CIDS ON PIHEX-S
!
      DO jj = 1 , num
         j = ncid + jj
         IF ( Iz(icord)==Iz(j) ) GOTO 800
      ENDDO
   ENDDO
   GOTO 3200
!
!     MATCH- RESERVE 13 WORDS SINCE THIS CORD1R WILL BE CONVERTED TO
!     CORD2R TYPE ENTRY LATER
!
 800  Iz(j) = -Iz(j)
   ncord1 = ncord1 + 1
   icord = icord + 13
   IF ( ncord1/=num ) GOTO 700
   GOTO 1400
!
!     TRY CORD2R
!
 900  CALL locate(*1200,Z(buf1),ic2,idx)
 1000 DO WHILE ( icord+12<=lcore )
      CALL read(*3000,*1200,geom1,Z(icord),13,0,m)
!
!     COMPARE
!
      DO jj = 1 , num
         j = ncid + jj
         IF ( Iz(icord)==Iz(j) ) GOTO 1100
      ENDDO
   ENDDO
   GOTO 3200
!
!     MATCH ON CORD2R. CHECK FOR RID. MUST BE 0
!
 1100 IF ( Iz(icord+3)/=0 ) THEN
      WRITE (Nout,99002) Ufm , Iz(j)
99002 FORMAT (A23,', CORD2R',I8,' DEFINES A PIHEX CID BUT HAS NONZERO',' RID')
      GOTO 1300
   ELSE
!
      Iz(j) = -Iz(j)
      ncord2 = ncord2 + 1
      icord = icord + 13
      IF ( ncord1+ncord2/=num ) GOTO 1000
      GOTO 1400
   ENDIF
!
!     EXHAUSTED CORD2R-S, BUT NOT ALL  CID-S ARE LOCATED
!
 1200 DO jj = 1 , num
      j = ncid + jj
      IF ( Iz(j)>=0 ) THEN
         WRITE (Nout,99003) Ufm , Iz(j)
99003    FORMAT (A23,', CID',I8,' ON A PIHEX CARD IS NOT DEFINED TO BE ','CORD1R OR CORD2R')
      ENDIF
   ENDDO
 1300 CALL mesage(-61,0,nam)
!
!
!     MATCHING IS COMPLETE
!
 1400 CALL close(geom1,1)
!
!     CID,MATID PAIRS ARE IN Z(1)-Z(NCID). UNIQUE CID LIST IS IN
!     Z(NCID+1)-Z(NCID+NUM). THERE ARE NCORD1 CORD1R-S AND NCORD2
!     CORD2R-S AT 13 WORDS EACH STARTING AT Z(NCID+NUM+1).
!     NEXT AVAILABLE OPEN CORE IS AT Z(ICORD)
!
   DO jj = 1 , num
      j = ncid + jj
      Iz(j) = -Iz(j)
   ENDDO
!
!     FOR CID-S ON CORD1R WE MUST OBTAIN THE BASIC COORDINATES OF EACH
!     POINT FROM BGPDT. FIRST, THE EXTERNAL POINT NUMBERS ON CORD1R MUST
!     BE CONVERTED TO  INTERNAL.
!
   lcore = lcore - (icord-1)
   IF ( lcore<=0 ) GOTO 3200
   mcore = lcore
   ibgpdt = icord
   IF ( ncord1==0 ) GOTO 1700
   CALL gopen(bgpdt,Z(buf1),0)
   file = bgpdt
   CALL read(*3000,*1500,bgpdt,Z(ibgpdt),lcore,0,m)
   GOTO 3200
 1500 CALL close(bgpdt,1)
   ieq = ibgpdt + m
   lcore = lcore - m
   CALL gopen(eqexin,Z(buf1),0)
   file = eqexin
   CALL read(*3000,*1600,eqexin,Z(ieq),lcore,0,m)
   GOTO 3200
 1600 CALL close(eqexin,1)
   lcore = lcore - m
!
!     FOR EACH CORD1R ENTRY, FIND THE BASIC COORDINATES FOR EACH POINT
!     AND FORM A CORD2R ENTRY BACK WHERE THE CORD1R IS STORED
!
   DO j = 1 , ncord1
      ipoint = 13*(j-1) + ncid + num
      icid = Iz(ipoint+1)
      DO k = 1 , 3
         isubk = ipoint + 3 + k
         k3 = 3*(k-1)
         igrid = Iz(isubk)
         CALL bisloc(*2800,igrid,Z(ieq),2,m/2,jp)
!
!     IM IS POINTER TO INTERNAL NUMBER. NOW FIND BGPDT ENTRY
!
         im = ieq + jp
         ip = 4*(Iz(im)-1)
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
      Iz(ip4) = 0
      DO l = 1 , 9
         isubl = ip4 + l
         Z(isubl) = store(l)
      ENDDO
!
!     GO BACK FOR ANOTHER CORD1R
!
   ENDDO
!
!     FOR EACH COORDINATE SYSTEM, COMPUTE THE 9 DIRECTION COSINES FROM
!     THE BASIC COORDINATE SYSTEM. Z(ICORD) IS THE NEXT AVAILABLE
!     LOCATION OF OPEN CORE SINCE WE NO LONGER NEED EQEXIN OR BGPDT
!     INFO.
!
 1700 lcore = mcore
   CALL gopen(mpt,Z(buf1),0)
   CALL gopen(mpta,Z(buf2),1)
   IF ( icord+30>lcore ) GOTO 3200
!
!     COPY MPT TO MPTA UNTIL MAT6 IS REACHED
!
   file = mpt
 1800 CALL read(*2500,*3100,mpt,Z(icord),3,0,m)
   CALL write(mpta,Z(icord),3,0)
   IF ( Iz(icord)==imat6(1) ) GOTO 2000
   DO
      CALL read(*3000,*1900,mpt,Z(icord),lcore,0,m)
      CALL write(mpta,Z(icord),lcore,0)
   ENDDO
 1900 CALL write(mpta,Z(icord),m,1)
   GOTO 1800
!
!     MAT6 RECORD FOUND. EACH MAT6 CONTAINS 31 WORDS. INCREASE THAT
!     TO 40
!
 2000 CALL read(*3000,*2400,mpt,Z(icord),31,0,m)
!
!     SEE IF THIS ID MATCHES A CID ON PIHEX) IT NEED NOT
!
   DO j = 2 , ncid , 2
      IF ( Iz(j)==Iz(icord) ) GOTO 2100
   ENDDO
!
!     NO MATCH. MAT6 NOT REFERENCED BY PIHEX. COPY IT TO MAT6 AND FILL
!     IN ALL 3 DIRECTION COSINES. THIS MAT6 IS NOT REFERENCED BY PIHEX
!
   DO k = 1 , 9
      xd(k) = 0.
   ENDDO
   GOTO 2300
!
!     MATCH. NOW FIND IT IN CORD1R,CORD2R LIST
!
 2100 icid = Iz(j-1)
   DO ii = 1 , num
      ipoint = ncid + num + 13*(ii-1)
      IF ( icid==Iz(ipoint+1) ) GOTO 2200
!
!     LOGIC ERROR
!
   ENDDO
   WRITE (Nout,99004) Ufm
99004 FORMAT (A23,', NON-UNIQUE COORDINATE SYSTEMS ON PIHEX CARDS',/5X,'(SEE USER MANUAL P.2.4-233(05/30/86))')
   GOTO 1300
!
 2200 Iz(ipoint+1) = -Iz(ipoint+1)
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
!
!     WRITE OUT NEW MAT6 RECORD WITH DIRECTION COSINES APPENDED
!
 2300 CALL write(mpta,Z(icord),31,0)
   CALL write(mpta,xd,9,0)
!
!     GET ANOTHER MAT6
!
   GOTO 2000
!
!     MAT6 RECORD FINISHED. WRITE EOR, COPY REMAINDER OF MPT, AND CHECK
!     TO SEE THAT ALL PIHEX CID-S HAVE BEEN ACCOUNTED FOR.
!
 2400 CALL write(mpta,0,0,1)
   GOTO 1800
!
!     MPT EXHAUSTED
!
 2500 CALL close(mpt,1)
   CALL close(mpta,1)
   itrl(1) = mpt
   CALL rdtrl(itrl)
   itrl(1) = mpta
   CALL wrttrl(itrl)
   Isop = -1
 2600 RETURN
!
!WKBDB 5/94 SPR 93033
!  310 WRITE  (NOUT,320) UWM
!  320 FORMAT (A25,', EITHER EPT IS PURGED OR NO PIHEX CARDS FOUND ON ',
!     1       'EPT IN ANISOP')
!WKBDE 5/94 SPR 93033
!WKBI  SPR 93033 5/94
 2700 CALL close(ept,1)
   GOTO 2600
 2800 WRITE (Nout,99005) Ufm , igrid
99005 FORMAT (A23,', EXTERNAL GRID',I8,' CANNOT BE FOUND ON EQEXIN IN ','ANISOP')
   GOTO 1300
!
!WKBDB SPR93033 5/94
! 1001 N = -1
!      GO TO 1010
!WKBDE SPR93033 5/94
!WKBIB SPR93033 5/94
 2900 CALL close(mpt,1)
   CALL close(geom1,1)
   GOTO 2600
!WKBIE SPR93033 5/94
 3000 n = -2
   GOTO 3300
 3100 n = -3
   GOTO 3300
 3200 file = 0
   n = -8
 3300 CALL mesage(n,file,nam)
END SUBROUTINE anisop