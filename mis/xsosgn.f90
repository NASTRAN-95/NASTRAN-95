
SUBROUTINE xsosgn
!
!     THIS SUBROUTINE SCANS THE OSCAR TAPE AND GENERATES THE SOS + MD
!
!     LAST REVISED BY G.CHAN/UNISYS TO REMOVE THE VAX AND NOT-VAX
!     LOGICS, AND TO SYNCHRONIZE THE SCRATH FILE NAMES AS SET FORTH BY
!     THE XSEMX ROUTINES.   2/1990
!
   IMPLICIT NONE
   INTEGER Almsk , Apndmk , Buf1(1) , Comm(20) , Cursno , Dculg , Ddbn(2) , Dfnu(1) , Dmxlg , Dnaf , Dpd(1) , Entn1 , Entn2 ,       &
         & Entn3 , Entn4 , Entn5 , Entn6 , Fculg , Fcum(1) , Fcus(1) , Fdbn(2) , Fequ(1) , Fiat(1) , File(1) , Fist , Fknd(1) ,     &
         & Flag , Fmat(1) , Fmxlg , Fntu(1) , Fnx , Fon(1) , Ford(1) , Fpun(1) , Funlg , Ibufsz , J , K , Lmsk , Lxmsk , Md(401) ,  &
         & Minp(1) , Mlgn , Mlsn(1) , Mout(1) , Mscr(1) , Outtap , Rmsk , Rxmsk , S , Sal(1) , Scornt , Sdbn(1) , Slgn , Sntu(1) ,  &
         & Sord(1) , Sos(1501) , Str(30) , Tapmsk , Xf1at(1) , Zap
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /isosgn/ Entn5 , Entn6 , K , J , Str
   COMMON /system/ Ibufsz , Outtap
   COMMON /xdpl  / Dpd , Dmxlg , Dculg , Ddbn , Dfnu
   COMMON /xfiat / Fiat , Fmxlg , Fculg , File , Fdbn , Fmat
   COMMON /xfist / Fist
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xsfa1 / Md , Sos , Comm , Xf1at , Fpun , Fcum , Fcus , Fknd
   COMMON /zzzzzz/ Buf1
   INTEGER andf , khrfn1 , lshift , orf , rshift
   INTEGER blkcnt , block(100) , block1(93) , cond , i , iflag , isw , jump , l , ll , lll , n1 , nsosgn(2) , numbr(10) , nwds ,    &
         & oscar , rept , scrn1 , scrn2 , scrn3
   EXTERNAL andf , lshift , orf , rshift
!     LOGICAL         DEC
   EQUIVALENCE (Dpd(1),Dnaf) , (Fiat(1),Funlg) , (File(1),Fequ(1)) , (File(1),Ford(1)) , (block(8),block1(1))
   EQUIVALENCE (Md(1),Mlgn) , (Md(2),Mlsn(1)) , (Md(3),Minp(1)) , (Md(4),Mout(1)) , (Md(5),Mscr(1)) , (Sos(1),Slgn) ,               &
    & (Sos(2),Sdbn(1)) , (Sos(4),Sal(1),Sntu(1),Sord(1)) , (Comm(1),Almsk) , (Comm(2),Apndmk) , (Comm(3),Cursno) , (Comm(4),Entn1) ,&
    & (Comm(5),Entn2) , (Comm(6),Entn3) , (Comm(7),Entn4) , (Comm(8),Flag) , (Comm(9),Fnx) , (Comm(10),Lmsk) , (Comm(11),Lxmsk) ,   &
    & (Comm(13),Rmsk) , (Comm(14),Rxmsk) , (Comm(15),S) , (Comm(16),Scornt) , (Comm(17),Tapmsk) , (Comm(19),Zap) ,                  &
    & (Xf1at(1),Fntu(1),Fon(1))
   DATA jump/4HJUMP/ , rept/4HREPT/ , cond/4HCOND/
   DATA oscar/4HPOOL/ , scrn1 , scrn2/4HSCRA , 4HTCH0/
   DATA nsosgn/4HXSOS , 2HGN/
   DATA numbr/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9 , 1H0/
!
   iflag = 0
   CALL open(*1100,oscar,Buf1,2)
   CALL bckrec(oscar)
   CALL read(*800,*1200,oscar,block,7,0,Flag)
   IF ( block(2)==Cursno ) GOTO 200
   WRITE (Outtap,99001) Sfm , block(2) , Cursno
99001 FORMAT (A25,' 1014, POOL FILE MIS-POSITIONED ',2I7)
   CALL mesage(-37,0,nsosgn)
   GOTO 99999
!
!     READ OSCAR FORMAT HEADER + 1
!
 100  IF ( J>1400 .OR. K>390 ) GOTO 900
   CALL read(*800,*1200,oscar,block,7,0,Flag)
 200  block(3) = andf(Rmsk,block(3))
   IF ( block(6)>=0 ) THEN
      CALL fwdrec(*800,oscar)
      GOTO 100
   ELSEIF ( block(3)<=2 ) THEN
!
!     INPUT FILES
!
      Minp(K) = block(7)
      IF ( block(7)==0 ) THEN
!
!     ZERO INPUT FILES
!
         CALL read(*800,*1200,oscar,block(7),1,0,Flag)
         IF ( block(3)==2 ) THEN
!
!     TYPE O FORMAT - NO OUTPUTS
!
            Mout(K) = 0
            GOTO 700
         ELSE
            Mout(K) = block(7)
            GOTO 500
         ENDIF
      ELSE
         nwds = block(7)*Entn5
         ASSIGN 400 TO isw
      ENDIF
   ELSE
      IF ( block(3)==3 ) THEN
         l = rshift(andf(Lxmsk,block(7)),16) - block(2)
         IF ( block(4)/=jump ) THEN
            IF ( block(4)/=rept .AND. block(4)/=cond ) THEN
               CALL fwdrec(*800,oscar)
            ELSE
               IF ( l<0 ) iflag = -1
               CALL fwdrec(*800,oscar)
            ENDIF
         ELSEIF ( l<=1 ) THEN
            IF ( l<0 ) iflag = -1
            CALL fwdrec(*800,oscar)
         ELSE
            DO i = 1 , l
               CALL fwdrec(*800,oscar)
            ENDDO
         ENDIF
         GOTO 100
      ENDIF
      CALL fwdrec(*800,oscar)
      GOTO 100
   ENDIF
!
!     FILES READER
!
 300  CALL read(*800,*1200,oscar,block1,nwds+1,0,Flag)
   blkcnt = 0
   DO i = 1 , nwds , Entn5
      IF ( block1(i)==0 ) THEN
         blkcnt = blkcnt + 1
      ELSE
         Sos(J) = block1(i)
         Sos(J+1) = block1(i+1)
         Sos(J+2) = block1(i+2)
         J = J + 3
         IF ( J>1500 ) GOTO 1000
      ENDIF
   ENDDO
   GOTO isw
!
 400  Minp(K) = Minp(K) - blkcnt
   IF ( block(3)==2 ) THEN
      Mout(K) = 0
      GOTO 700
   ELSE
!
!     OUTPUT FILES
!
      Mout(K) = block1(nwds+1)
   ENDIF
 500  IF ( Mout(K)==0 ) THEN
!
!     ZERO OUTPUT FILES
!
      CALL read(*800,*1200,oscar,block1(nwds+1),1,0,Flag)
      GOTO 700
   ELSE
      nwds = Mout(K)*Entn6
      ASSIGN 600 TO isw
      GOTO 300
   ENDIF
!
 600  Mout(K) = Mout(K) - blkcnt
 700  CALL fwdrec(*800,oscar)
!
!     SCRATCH FILES
!
   Mscr(K) = block1(nwds+1)
   IF ( Mscr(K)/=0 ) THEN
      l = Mscr(K)
      scrn3 = scrn2
      lll = 1
      ll = 0
      DO i = 1 , l
         ll = ll + 1
         IF ( ll==10 ) scrn3 = khrfn1(scrn3,3,numbr(lll),1)
         Sos(J) = scrn1
         Sos(J+1) = khrfn1(scrn3,4,numbr(ll),1)
         IF ( ll==10 ) THEN
            ll = 0
            lll = lll + 1
         ENDIF
         IF ( Str(i)/=0 ) THEN
            n1 = Str(i)
            Sos(n1) = orf(Lmsk,block(2))
         ENDIF
         Str(i) = J + 2
         Sos(J+2) = Scornt + i
         J = J + 3
         IF ( J>1500 ) GOTO 1000
      ENDDO
   ENDIF
!
   Mlsn(K) = block(2)
   IF ( iflag/=0 ) Mlsn(K) = orf(S,Mlsn(K))
   IF ( Minp(K)+Mout(K)+Mscr(K)==0 ) GOTO 100
   K = K + Entn3
   IF ( K<=400 ) GOTO 100
   GOTO 1000
!
 800  CALL skpfil(oscar,-1)
 900  CALL close(oscar,2)
   Slgn = (J-1)/Entn2
   Mlgn = (K-1)/Entn3
   RETURN
!
!     SYSTEM FATAL MESSAGES
!
 1000 WRITE (Outtap,99002) Sfm
99002 FORMAT (A25,' 1011, MD OR SOS TABLE OVERFLOW')
   CALL mesage(-37,0,nsosgn)
   GOTO 99999
 1100 WRITE (Outtap,99003) Sfm
99003 FORMAT (A25,' 1012, POOL COULD NOT BE OPENED')
   CALL mesage(-37,0,nsosgn)
   GOTO 99999
 1200 WRITE (Outtap,99004) Sfm
99004 FORMAT (A25,' 1013, ILLEGAL EOR ON POOL')
   CALL mesage(-37,0,nsosgn)
99999 RETURN
END SUBROUTINE xsosgn
