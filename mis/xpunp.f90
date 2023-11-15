
SUBROUTINE xpunp
!
!     THIS SUBROUTINE POOLS AND UNPOOLS FILES AS PRESCRIBED BY XFIAT
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Almsk , Apndmk , Buf1(1) , Comm(20) , Cursno , Dculg , Ddbn(1) , Dfnu(1) , Dmxlg , Dnaf , Dpd(6) , Entn1 , Entn2 ,       &
         & Entn3 , Entn4 , Fculg , Fcum(1) , Fcus(1) , Fdbn(1) , Fequ(1) , Fiat(7) , File(1) , Fist(2) , Fknd(1) , Flag , Fmat(1) , &
         & Fmxlg , Fntu(1) , Fnx , Fon(1) , Ford(1) , Fpun(1) , Funlg , Ibufsz , Lmsk , Lxmsk , Macsft , Md(401) , Minp(1) , Mlgn , &
         & Mlsn(1) , Mout(1) , Mscr(1) , Outtap , Pfist , Rmsk , Rxmsk , S , Sal(1) , Scornt , Sdbn(1) , Slgn , Sntu(1) , Sord(1) , &
         & Sos(1501) , Tapmsk , Thcrmk , Xf1at(5) , Zap
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /system/ Ibufsz , Outtap
   COMMON /xdpl  / Dpd
   COMMON /xfiat / Fiat
   COMMON /xfist / Fist
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xpfist/ Pfist
   COMMON /xsfa1 / Md , Sos , Comm , Xf1at
   COMMON /zzzzzz/ Buf1
!
! Local variable declarations
!
   INTEGER andf , lshift , orf
   INTEGER block(1000) , dfnusv , entn1x , entn5 , fn , fstidx , head(2) , header(8) , hold , i , ii , isw1 , isw2 , j , k , kk ,   &
         & l , lmt3 , lmt4 , n , ncnt , nn , npunp(2) , nx , pool
   EXTERNAL andf , lshift , orf
!
! End of declarations
!
   EQUIVALENCE (Dpd(1),Dnaf) , (Dpd(2),Dmxlg) , (Dpd(3),Dculg) , (Dpd(4),Ddbn(1)) , (Dpd(6),Dfnu(1)) , (Fiat(1),Funlg) ,            &
    & (Fiat(2),Fmxlg) , (Fiat(3),Fculg) , (Fiat(4),Fequ(1)) , (Fiat(4),File(1)) , (Fiat(4),Ford(1)) , (Fiat(5),Fdbn(1)) ,           &
    & (Fiat(7),Fmat(1)) , (Md(1),Mlgn) , (Md(2),Mlsn(1)) , (Md(3),Minp(1)) , (Md(4),Mout(1)) , (Md(5),Mscr(1)) , (Sos(1),Slgn) ,    &
    & (Sos(2),Sdbn(1)) , (Sos(4),Sal(1)) , (Sos(4),Sntu(1)) , (Sos(4),Sord(1)) , (Xf1at(1),Fntu(1)) , (Xf1at(1),Fon(1)) ,           &
    & (Xf1at(2),Fpun(1)) , (Xf1at(3),Fcum(1)) , (Xf1at(4),Fcus(1)) , (Xf1at(5),Fknd(1))
   EQUIVALENCE (Comm(1),Almsk) , (Comm(2),Apndmk) , (Comm(3),Cursno) , (Comm(4),Entn1) , (Comm(5),Entn2) , (Comm(6),Entn3) ,        &
    & (Comm(7),Entn4) , (Comm(8),Flag) , (Comm(9),Fnx) , (Comm(10),Lmsk) , (Comm(11),Lxmsk) , (Comm(12),Macsft) , (Comm(13),Rmsk) , &
    & (Comm(14),Rxmsk) , (Comm(15),S) , (Comm(16),Scornt) , (Comm(17),Tapmsk) , (Comm(18),Thcrmk) , (Comm(19),Zap)
   DATA n/1000/ , pool/4HPOOL/ , entn5/2/ , npunp/4HXPUN , 4HP   /
!
!     ENTRY SIZE NUMBERS,  1=FIAT, 4=DPD
!
   isw1 = 0
   isw2 = 0
   entn1x = Entn1 - 1
   Fist(2) = 1 + Pfist
!
!     COMPUTE INDEX FOR DUMMY ENTRY IN FIST
!
   fstidx = Fist(2)*2 + 1
   lmt3 = Fculg*Entn1
!
!     CHECK FOR ANY FILES TO POOL
!
   Fist(fstidx) = 101
   DO i = 1 , lmt3 , Entn1
      IF ( Fpun(i)>=0 ) CYCLE
      nn = andf(Almsk,Fpun(i))
      Fpun(i) = 0
      IF ( Fmat(i)==0 .AND. Fmat(i+1)==0 .AND. Fmat(i+2)==0 ) THEN
         IF ( .NOT.(Entn1==11 .AND. (Fmat(i+5)/=0 .OR. Fmat(i+6)/=0 .OR. Fmat(i+7)/=0)) ) THEN
            nn = 1
            GOTO 50
         ENDIF
      ENDIF
      CALL xpolck(Fdbn(i),Fdbn(i+1),fn,nx)
      IF ( fn==0 ) THEN
         IF ( isw1==0 ) THEN
            isw1 = 1
            CALL open(*300,pool,Buf1,2)
            CALL xfilps(Dnaf)
            CALL close(pool,2)
            CALL open(*300,pool,Buf1,3)
            Fnx = Dnaf
         ENDIF
         Fist(fstidx+1) = i + entn5
         CALL open(*300,101,Buf1(Ibufsz+1),0)
         ncnt = 0
!
!     WRITE SPECIAL FILE HEADER RECORD -- XPOOL DICT NAME    ( 2 WORDS )
!                                       + DATA BLOCK TRAILER ( 3 WORDS
!                                                         OR   6 WORDS )
!
         CALL write(pool,Fdbn(i),2,0)
         IF ( Entn1==11 ) THEN
            CALL write(pool,Fmat(i),3,0)
            CALL write(pool,Fmat(i+5),3,1)
         ELSE
            CALL write(pool,Fmat(i),3,1)
         ENDIF
!
!     READ AND WRITE 1ST 2 WORDS OF DATA BLOCK HEADER.
!     THEN CALL CPYFIL TO COPY REMAINDER OF FILE.
!
         CALL read(*400,*500,101,head,2,0,Flag)
         CALL write(pool,head,2,0)
         CALL cpyfil(101,pool,block,n,Flag)
         ncnt = andf(Lxmsk,lshift(Flag/1000+1,16))
         CALL eof(pool)
         CALL close(101,1)
!
!     ADD FILE NAME OF FILE JUST POOLED TO DPD
!
         j = Dculg*Entn4 + 1
         Dculg = Dculg + 1
         IF ( Dculg>Dmxlg ) GOTO 200
         Dfnu(j) = orf(Dnaf,ncnt)
         Ddbn(j) = Fdbn(i)
         Ddbn(j+1) = Fdbn(i+1)
         CALL sswtch(3,l)
         IF ( l==1 ) THEN
            CALL page2(-2)
            WRITE (Outtap,99001) Ddbn(j) , Ddbn(j+1) , head(1) , head(2)
99001       FORMAT (16H0POOL FILE NAME ,2A4,17H DATA BLOCK NAME ,2A4)
         ENDIF
         Dnaf = Dnaf + 1
         Fnx = Fnx + 1
      ELSE
         j = nx
      ENDIF
 50   hold = andf(Rxmsk,File(i))
      lmt4 = i + entn1x
      DO kk = i , lmt4
         File(kk) = 0
      ENDDO
      File(i) = hold
      Fdbn(i) = Almsk
!
!     CHECK FOR EQUIV FILES
!
      IF ( nn/=1 ) THEN
!
!     THERE ARE EQUIV FILES
!
         Dfnu(j) = orf(S,Dfnu(j))
         dfnusv = Dfnu(j)
         DO k = 1 , lmt3 , Entn1
            IF ( Fequ(k)<0 .AND. i/=k ) THEN
               IF ( andf(Rmsk,File(i))/=andf(Rmsk,File(k)) ) CYCLE
!
!     THIS IS AN EQUIV FILE
!
               CALL xpolck(Fdbn(k),Fdbn(k+1),fn,nx)
               IF ( fn/=0 ) THEN
                  IF ( Dfnu(nx)==dfnusv ) GOTO 55
                  Ddbn(nx) = 0
                  Ddbn(nx+1) = 0
               ENDIF
               j = j + Entn4
               Dculg = Dculg + 1
               IF ( Dculg>Dmxlg ) GOTO 200
               Dfnu(j) = dfnusv
               Ddbn(j) = Fdbn(k)
               Ddbn(j+1) = Fdbn(k+1)
               lmt4 = k + entn1x
               DO kk = k , lmt4
                  File(kk) = 0
               ENDDO
 55            nn = nn - 1
               IF ( nn==1 ) GOTO 100
            ENDIF
         ENDDO
         GOTO 600
      ENDIF
 100  ENDDO
   IF ( isw1/=0 ) THEN
      CALL close(pool,1)
      Fnx = 1
   ENDIF
!
!     CHECK FOR ANY FILES TO UNPOOL
!
   Fist(fstidx) = 201
   DO
      fn = Dnaf
      DO i = 1 , lmt3 , Entn1
         IF ( Fpun(i)>0 .AND. Fpun(i)<fn ) THEN
            fn = Fpun(i)
            ii = i
         ENDIF
      ENDDO
      IF ( fn==Dnaf ) THEN
         IF ( isw2/=0 ) THEN
            CALL close(pool,1)
            Fnx = 1
         ENDIF
         RETURN
      ELSE
         Fpun(ii) = 0
         IF ( isw2==0 ) THEN
            isw2 = 1
            CALL open(*300,pool,Buf1,0)
            Fnx = 1
         ENDIF
         CALL xfilps(fn)
         Fnx = fn
         Fist(fstidx+1) = ii + entn5
         CALL open(*300,201,Buf1(Ibufsz+1),1)
!
!     READ SPECIAL FILE HEADER RECORD AND, IF DIAG 3 IS ON, PRINT MSG
!
         CALL read(*400,*500,pool,header,Entn1-3,1,Flag)
         CALL sswtch(3,l)
         IF ( l==1 ) THEN
            CALL page2(-2)
            WRITE (Outtap,99002) Fdbn(ii) , Fdbn(ii+1) , header(1) , header(2)
99002       FORMAT (17H0XUNPL-DICT NAME ,2A4,16H POOL FILE NAME ,2A4)
         ENDIF
!
!     COPY FILE USING CPYFIL
!
         CALL cpyfil(pool,201,block,n,Flag)
         CALL close(201,1)
         Fnx = Fnx + 1
         Fmat(ii) = header(3)
         Fmat(ii+1) = header(4)
         Fmat(ii+2) = header(5)
         IF ( Entn1==11 ) THEN
            Fmat(ii+5) = header(6)
            Fmat(ii+6) = header(7)
            Fmat(ii+7) = header(8)
         ENDIF
!
!     IS FILE EQUIVALENCED
!
         IF ( Fequ(ii)<0 ) THEN
!
!     YES, COPY SAME TRAILER INTO ALL EQUIV FILES
!
            hold = andf(Rmsk,File(ii))
            DO j = 1 , lmt3 , Entn1
               IF ( Fequ(j)<0 .AND. ii/=j ) THEN
                  IF ( hold==andf(Rmsk,File(j)) ) THEN
                     Fmat(j) = header(3)
                     Fmat(j+1) = header(4)
                     Fmat(j+2) = header(5)
                     IF ( Entn1==11 ) THEN
                        Fmat(j+5) = header(6)
                        Fmat(j+6) = header(7)
                        Fmat(j+7) = header(8)
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF
   ENDDO
!
!
 200  WRITE (Outtap,99003)
99003 FORMAT (1H0,23X,19H 1031, DPL OVERFLOW)
   GOTO 700
 300  WRITE (Outtap,99004)
99004 FORMAT (1H0,23X,62H 1032, POOL OR FILE BEING POOLED/UN-POOLED COULD NOT BE OPENED)
   GOTO 700
 400  WRITE (Outtap,99005)
99005 FORMAT (1H0,23X,39H 1033, ILLEGAL EOF ON FILE BEING POOLED)
   GOTO 700
 500  WRITE (Outtap,99006)
99006 FORMAT (1H0,23X,39H 1034, ILLEGAL EOR ON FILE BEING POOLED)
   GOTO 700
 600  WRITE (Outtap,99007)
99007 FORMAT (1H0,23X,33H 1035, EQUIV INDICATED,NONE FOUND)
 700  CALL page2(-4)
   WRITE (Outtap,99008) Sfm
99008 FORMAT (A25,1H.)
   CALL mesage(-37,0,npunp)
END SUBROUTINE xpunp
