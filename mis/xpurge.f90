
SUBROUTINE xpurge
!
!     THIS SUBROUTINE PURGES AND EQUATES FILES WITHIN FIAT AND DPD
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Almsk , Apndmk , Comm(20) , Cursno , Dculg , Ddbn(1) , Dfnu(1) , Dmxlg , Dnaf , Dpd(6) , Dum(21) , Dumm(14) , Entn1 ,    &
         & Entn2 , Entn3 , Entn4 , Fculg , Fcum(1) , Fcus(1) , Fdbn(1) , Fequ(1) , Fiat(7) , File(1) , Fist , Fknd(1) , Flag ,      &
         & Fmat(1) , Fmxlg , Fntu(1) , Fnx , Fon(1) , Ford(1) , Fpun(1) , Funlg , Hold , Ibufsz , Icfiat , J , K , Lmsk , Lxmsk ,   &
         & Macsft , Md(401) , Minp(1) , Mlgn , Mlsn(1) , Mout(1) , Mscr(1) , Nbpc , Nbpw , Ncpw , Nsav , Outtap , Prisav , Rmsk ,   &
         & Rxmsk , S , Sal(1) , Scornt , Sdbn(1) , Slgn , Sntu(1) , Sord(1) , Sos(1501) , Tapmsk , Thcrmk , Vps(1) , X(1) , Xf1at(5)&
         & , Zap
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /ipurge/ J , K , Nsav , Prisav , Hold
   COMMON /oscent/ X
   COMMON /system/ Ibufsz , Outtap , Dum , Icfiat , Dumm , Nbpc , Nbpw , Ncpw
   COMMON /xdpl  / Dpd
   COMMON /xfiat / Fiat
   COMMON /xfist / Fist
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xsfa1 / Md , Sos , Comm , Xf1at
   COMMON /xvps  / Vps
!
! Local variable declarations
!
   INTEGER andf , lshift , orf
   INTEGER fn , fnsav , i , iback , iexec , ij , incr , ivps , jpt , jx , l , lmt1 , lmt2 , lmt3 , lmt4 , lsav , m , m1 , n , ndbs ,&
         & nfculg , nn , nwds , purge1(2) , secchn , snsav , ssav , xj1 , xj2
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
   DATA purge1/4HXPUR , 4HGE  /
!
   CALL xsfadd
   K = -1
   GOTO 100
!
!
   ENTRY xequiv
!     ============
!
   CALL xsfadd
   K = +1
   Prisav = 0
   secchn = 0
 100  Entn1 = Icfiat
   lmt1 = Funlg*Entn1
   lmt2 = lmt1 + 1
   lmt3 = Fculg*Entn1
   IF ( Fculg>=Fmxlg ) THEN
!
!     POTENTIAL FIAT OVERFLOW- LOOK FOR OTHER SLOTS IN FIAT TAIL
!
      ASSIGN 200 TO iback
      GOTO 1200
   ELSE
      nfculg = lmt3 + 1
      incr = 1
   ENDIF
!
!     S = O 400000000000     Z 80000000
!
 200  S = lshift(1,Nbpw-1)
!
!     INITIALIZE FOR FIRST SET OF DATA BLOCKS
!
   nwds = X(1)
   i = 7
 300  ndbs = X(i)
!
!     FIND POSITION OF VPS POINTER WORD
!
   jpt = i + 2*ndbs + 1
   IF ( K==1 ) jpt = jpt + 1
   ivps = X(jpt)
   iexec = -1
!
!     TEST CONDITIONAL INDICATOR (NOT HERE, BELOW TO PERMIT UNPURGE-
!     UNEQUIV
!
   IF ( ivps>0 ) iexec = Vps(ivps)
!
!     TEST FOR PURGE OR EQUIV
!
   J = i + 1
   IF ( K<=0 ) GOTO 500
   GOTO 800
!
!     INCREMENT AND LOOK AT NEXT SET OF DATA BLOCKS
!
 400  i = jpt + 1
   IF ( i<nwds ) GOTO 300
   RETURN
!
!     PURGE LOGIC FOLLOWS
!
 500  xj1 = X(J)
   xj2 = X(J+1)
   DO n = 1 , lmt3 , Entn1
      IF ( xj1==Fdbn(n) .AND. xj2==Fdbn(n+1) ) THEN
         IF ( n<=lmt1 ) THEN
            IF ( iexec>=0 ) GOTO 700
            Hold = andf(Rxmsk,File(n))
            lmt4 = n + Entn1 - 1
            DO m = n , lmt4
               File(m) = 0
            ENDDO
            File(n) = Hold
!
            EXIT
         ELSEIF ( iexec>=0 ) THEN
!
            IF ( andf(Rmsk,File(n))==Zap ) THEN
!
!     UNPURGE (CLEAR THE ENTRY)
!
               lmt4 = n + Entn1 - 1
               DO m = n , lmt4
                  File(m) = 0
               ENDDO
            ENDIF
            GOTO 700
         ELSE
            File(n) = Zap
            GOTO 600
         ENDIF
      ENDIF
   ENDDO
   IF ( iexec>=0 ) GOTO 700
   File(nfculg) = Zap
   Fdbn(nfculg) = xj1
   Fdbn(nfculg+1) = xj2
   Fculg = Fculg + incr
   IF ( Fculg>=Fmxlg ) THEN
      ASSIGN 600 TO iback
      GOTO 1200
   ELSE
      nfculg = nfculg + Entn1
   ENDIF
!
 600  CALL xpolck(xj1,xj2,fn,l)
   IF ( fn/=0 ) THEN
      Ddbn(l) = 0
      Ddbn(l+1) = 0
   ENDIF
!
 700  J = J + 2
   IF ( J-2==i+1 .AND. K>0 ) J = J + 1
   IF ( J>=jpt ) GOTO 400
   GOTO 500
!
!     EQUIV LOGIC FOLLOWS
!
 800  xj1 = X(J)
   xj2 = X(J+1)
   DO n = 1 , lmt3 , Entn1
      IF ( xj1==Fdbn(n) .AND. xj2==Fdbn(n+1) ) THEN
         IF ( J==i+1 ) THEN
!
!     PRIMARY
!
            Prisav = andf(Rxmsk,File(n))
            IF ( iexec>=0 ) GOTO 1100
!
!     IF PRIMARY FILE IS PURGED OR HAS ZERO TRAILERS, PURGE SECONDARYS
!
            IF ( andf(Rmsk,Prisav)==Zap ) GOTO 700
            IF ( Fmat(n)==0 .AND. Fmat(n+1)==0 .AND. Fmat(n+2)==0 ) THEN
               IF ( .NOT.(Entn1==11 .AND. (Fmat(n+5)/=0 .OR. Fmat(n+6)/=0 .OR. Fmat(n+7)/=0)) ) GOTO 700
            ENDIF
            Fequ(n) = orf(S,Fequ(n))
            Nsav = n
            CALL xpolck(xj1,xj2,fnsav,lsav)
            IF ( fnsav/=0 ) Dfnu(lsav) = orf(S,Dfnu(lsav))
!
!     IF PRIMARY FILE CONTAINS OTHER UNEQUIV D.B.- CLEAR THEM
!
            DO jx = 1 , lmt3 , Entn1
               IF ( Fequ(jx)>=0 .AND. jx/=n ) THEN
                  IF ( Prisav==andf(Rxmsk,File(jx)) ) THEN
                     lmt4 = jx + Entn1 - 1
                     DO m = jx , lmt4
                        File(m) = 0
                     ENDDO
                     IF ( jx<=lmt1 ) File(jx) = Prisav
                  ENDIF
               ENDIF
            ENDDO
            GOTO 1100
!
!     SECONDARY
!
         ELSEIF ( iexec>=0 ) THEN
            IF ( Fequ(n)<0 .AND. Prisav==andf(Rxmsk,File(n)) ) THEN
!
!     UNEQUIV (CLEAR SEC EQUIV ENTRY)
!
               lmt4 = n + Entn1 - 1
               DO m = n , lmt4
                  File(m) = 0
               ENDDO
               IF ( n<=lmt1 ) File(n) = Prisav
               CALL xpolck(xj1,xj2,fn,l)
               IF ( fn/=0 ) THEN
                  Ddbn(l) = 0
                  Ddbn(l+1) = 0
               ENDIF
            ENDIF
            GOTO 1100
         ELSE
            IF ( Prisav/=0 ) THEN
               IF ( File(n)<0 .AND. andf(Rxmsk,File(n))/=Prisav ) secchn = andf(Rmsk,File(n))
               IF ( n<=lmt1 ) THEN
                  File(nfculg) = orf(andf(Lxmsk,File(n)),Prisav)
               ELSE
                  File(n) = orf(andf(Lxmsk,File(n)),Prisav)
                  Fequ(n) = orf(S,Fequ(n))
                  Fmat(n) = Fmat(Nsav)
                  Fmat(n+1) = Fmat(Nsav+1)
                  Fmat(n+2) = Fmat(Nsav+2)
                  IF ( Entn1==11 ) THEN
                     Fmat(n+5) = Fmat(Nsav+5)
                     Fmat(n+6) = Fmat(Nsav+6)
                     Fmat(n+7) = Fmat(Nsav+7)
                  ENDIF
                  GOTO 1000
               ENDIF
            ENDIF
            Hold = andf(Rxmsk,File(n))
            lmt4 = n + Entn1 - 1
            DO m = n , lmt4
               File(m) = 0
            ENDDO
            IF ( n<=lmt1 ) File(n) = Hold
            IF ( Prisav/=0 ) GOTO 900
            GOTO 1000
         ENDIF
      ENDIF
!
!     FILE IS NOT IN FIAT -- CHECK PARM FOR TYPE OF EQUIV
!
   ENDDO
   IF ( iexec>=0 ) THEN
!
!     ELIMINATE EQUIV FILES -- CHECK FOR PRIMARY FILE
!
      IF ( J/=i+1 ) THEN
!
!     SECONDARY FILE --  BREAK EQUIV
!
         CALL xpolck(xj1,xj2,snsav,ssav)
!
!     CHECK IF FILE EXISTS AND IS EQUIVED TO PRIMARY FILE
!
         IF ( snsav/=0 .AND. fnsav==snsav ) THEN
            Ddbn(ssav) = 0
            Ddbn(ssav+1) = 0
         ENDIF
      ELSE
!
!     PRIMARY FILE
!
!
!     LEAVE EQUIV FLAG FOR XDPH
!
         CALL xpolck(xj1,xj2,fnsav,lsav)
      ENDIF
      GOTO 1100
!
!     CHECK FOR PRIMARY FILE
!
   ELSEIF ( J/=i+1 ) THEN
!
!     -IF SECONDARY, WAS PRIMARY IN FIAT
!
      IF ( Prisav==0 ) GOTO 1000
!
!     -PRIMARY WAS IN FIAT, SET UP SECONDARY IN FIAT
!
      File(nfculg) = Prisav
   ELSE
!
!     -IF PRIMARY, IT MUST BE ON POOL
!
      CALL xpolck(xj1,xj2,fnsav,lsav)
      IF ( fnsav==0 ) GOTO 700
      Dfnu(lsav) = orf(S,Dfnu(lsav))
      GOTO 1100
   ENDIF
 900  Fequ(nfculg) = orf(S,Fequ(nfculg))
   Fdbn(nfculg) = xj1
   Fdbn(nfculg+1) = xj2
   Fmat(nfculg) = Fmat(Nsav)
   Fmat(nfculg+1) = Fmat(Nsav+1)
   Fmat(nfculg+2) = Fmat(Nsav+2)
   IF ( Entn1==11 ) THEN
      Fmat(nfculg+5) = Fmat(Nsav+5)
      Fmat(nfculg+6) = Fmat(Nsav+6)
      Fmat(nfculg+7) = Fmat(Nsav+7)
   ENDIF
   Fculg = Fculg + incr
   IF ( Fculg>=Fmxlg ) THEN
      ASSIGN 1000 TO iback
      GOTO 1200
   ELSE
      nfculg = nfculg + Entn1
   ENDIF
!
!     WAS SECONDARY FILE IN FIAT ALREADY EQUIV TO OTHERS
!
 1000 IF ( secchn/=0 ) THEN
!
!     SEC. FILE WAS EQUIV - DRAG ALONG ALL EQUIVS
!
      DO ij = 1 , lmt3 , Entn1
         IF ( File(ij)<0 ) THEN
            IF ( ij/=n ) THEN
               IF ( andf(Rmsk,File(ij))==secchn ) THEN
!
!     CREATE AN ENTRY IN OSCENT TO EXPLICITLY EQUIV THIS DB
!
                  m1 = nwds + 1
                  nwds = nwds + 6
                  X(m1) = 2
                  X(m1+1) = xj1
                  X(m1+2) = xj2
                  IF ( K==1 ) THEN
                     X(m1+3) = 0
                     nwds = nwds + 1
                     m1 = m1 + 1
                  ENDIF
                  X(m1+3) = Fdbn(ij)
                  X(m1+4) = Fdbn(ij+1)
                  X(m1+5) = ivps
               ENDIF
            ENDIF
         ENDIF
      ENDDO
   ENDIF
!
!     IS SECONDARY FILE ON POOL
!
   CALL xpolck(xj1,xj2,fn,l)
   IF ( fn/=0 ) THEN
!
!     WAS SEC. FILE ON POOL ALREADY EQUIV TO OTHERS
!
      IF ( Dfnu(l)<0 .AND. fnsav/=fn ) THEN
!
!     SEC. FILE ON POOL WAS EQUIV - DRAG ALONG ALL EQUIVS
!
         lmt4 = Dculg*Entn4
         m = lmt4 + 1
         DO ij = 1 , lmt4 , Entn4
            IF ( Dfnu(ij)<0 .AND. ij/=l ) THEN
               IF ( andf(Rmsk,Dfnu(ij))==fn ) THEN
                  IF ( fnsav==0 ) THEN
!
!     CREATE AN ENTRY IN OSCENT TO EXPLICITLY EQUIV THIS DB
!
                     m1 = nwds + 1
                     nwds = nwds + 6
                     X(m1) = 2
                     X(m1+1) = xj1
                     X(m1+2) = xj2
                     IF ( K==1 ) THEN
                        X(m1+3) = 0
                        nwds = nwds + 1
                        m1 = m1 + 1
                     ENDIF
                     X(m1+3) = Ddbn(ij)
                     X(m1+4) = Ddbn(ij+1)
                     X(m1+5) = ivps
                  ELSE
                     Ddbn(m) = Ddbn(ij)
                     Ddbn(m+1) = Ddbn(ij+1)
                     Dfnu(m) = Dfnu(lsav)
                     Dculg = Dculg + 1
                     IF ( Dculg>Dmxlg ) GOTO 1300
                     m = m + Entn4
                  ENDIF
                  Ddbn(ij) = 0
                  Ddbn(ij+1) = 0
               ENDIF
            ENDIF
         ENDDO
      ENDIF
!
!     IF SECONDARY FILE IS ON POOL AND PRIMARY IS NOT - DELETE SEC REF
!
      IF ( fnsav/=0 ) THEN
!
!     BOTH PRIMARY AND SECONDARY ON POOL - IF NOT SAME FILE,
!     DELETE OLD SEC REF AND ADD NEW SEC REF
!
         IF ( fnsav==fn ) GOTO 1100
         Ddbn(l) = 0
         Ddbn(l+1) = 0
      ELSE
         Ddbn(l) = 0
         Ddbn(l+1) = 0
         GOTO 1100
      ENDIF
!
!     IF SECONDARY FILE IS NOT ON POOL AND PRIMARY IS - ADD SEC REF
!
   ELSEIF ( fnsav==0 ) THEN
      GOTO 1100
   ENDIF
   m = Dculg*Entn4 + 1
   Ddbn(m) = xj1
   Ddbn(m+1) = xj2
   Dfnu(m) = Dfnu(lsav)
   Dculg = Dculg + 1
   IF ( Dculg>Dmxlg ) GOTO 1300
!
 1100 J = J + 2
   IF ( J-2==i+1 ) J = J + 1
   secchn = 0
   IF ( J<jpt ) GOTO 800
   Prisav = 0
   GOTO 400
 1200 IF ( Fculg>Fmxlg ) THEN
!
!     ERROR MESSAGES
!
      WRITE (Outtap,99001) Sfm
99001 FORMAT (A25,' 1201, FIAT OVERFLOW.')
      GOTO 1400
   ELSE
      DO nn = lmt2 , lmt3 , Entn1
         IF ( File(nn)>=0 .AND. andf(Zap,File(nn))/=Zap ) THEN
            IF ( Fmat(nn)==0 .AND. Fmat(nn+1)==0 .AND. Fmat(nn+2)==0 ) THEN
               IF ( .NOT.(Entn1==11 .AND. (Fmat(nn+5)/=0 .OR. Fmat(nn+6)/=0 .OR. Fmat(nn+7)/=0)) ) THEN
                  nfculg = nn
                  incr = 0
                  GOTO 1250
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      nfculg = nfculg + Entn1
      incr = 1
 1250 GOTO iback
   ENDIF
 1300 WRITE (Outtap,99002) Sfm
99002 FORMAT (A25,' 1202, DPL OVERFLOW.')
 1400 CALL mesage(-37,0,purge1)
END SUBROUTINE xpurge
