!*==xpurge.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE xpurge
!
!     THIS SUBROUTINE PURGES AND EQUATES FILES WITHIN FIAT AND DPD
!
   IMPLICIT NONE
   USE C_IPURGE
   USE C_OSCENT
   USE C_SYSTEM
   USE C_XDPL
   USE C_XFIAT
   USE C_XFIST
   USE C_XMSSG
   USE C_XSFA1
   USE C_XVPS
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: almsk , apndmk , cursno , dculg , dmxlg , dnaf , entn1 , entn2 , entn3 , entn4 , fculg , flag , fmxlg , fn , fnsav ,  &
            & fnx , funlg , i , iback , iexec , ij , incr , ivps , jpt , jx , l , lmsk , lmt1 , lmt2 , lmt3 , lmt4 , lsav , lxmsk , &
            & m , m1 , macsft , mlgn , n , ndbs , nfculg , nn , nwds , rmsk , rxmsk , s , scornt , secchn , slgn , snsav , ssav ,   &
            & tapmsk , thcrmk , xj1 , xj2 , zap
   INTEGER , DIMENSION(1) :: ddbn , dfnu , fcum , fcus , fdbn , fequ , file , fknd , fmat , fntu , fon , ford , fpun , minp , mlsn ,&
                           & mout , mscr , sal , sdbn , sntu , sord
   INTEGER , DIMENSION(2) , SAVE :: purge1
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (Dpd(1),Dnaf) , (Dpd(2),Dmxlg) , (Dpd(3),Dculg) , (Dpd(4),Ddbn(1)) , (Dpd(6),Dfnu(1)) , (Fiat(1),Funlg) ,            &
!>>>>    & (Fiat(2),Fmxlg) , (Fiat(3),Fculg) , (Fiat(4),Fequ(1)) , (Fiat(4),File(1)) , (Fiat(4),Ford(1)) , (Fiat(5),Fdbn(1)) ,           &
!>>>>    & (Fiat(7),Fmat(1)) , (Md(1),Mlgn) , (Md(2),Mlsn(1)) , (Md(3),Minp(1)) , (Md(4),Mout(1)) , (Md(5),Mscr(1)) , (Sos(1),Slgn) ,    &
!>>>>    & (Sos(2),Sdbn(1)) , (Sos(4),Sal(1)) , (Sos(4),Sntu(1)) , (Sos(4),Sord(1)) , (Xf1at(1),Fntu(1)) , (Xf1at(1),Fon(1)) ,           &
!>>>>    & (Xf1at(2),Fpun(1)) , (Xf1at(3),Fcum(1)) , (Xf1at(4),Fcus(1)) , (Xf1at(5),Fknd(1))
   !>>>>EQUIVALENCE (Comm(1),Almsk) , (Comm(2),Apndmk) , (Comm(3),Cursno) , (Comm(4),Entn1) , (Comm(5),Entn2) , (Comm(6),Entn3) ,        &
!>>>>    & (Comm(7),Entn4) , (Comm(8),Flag) , (Comm(9),Fnx) , (Comm(10),Lmsk) , (Comm(11),Lxmsk) , (Comm(12),Macsft) , (Comm(13),Rmsk) , &
!>>>>    & (Comm(14),Rxmsk) , (Comm(15),S) , (Comm(16),Scornt) , (Comm(17),Tapmsk) , (Comm(18),Thcrmk) , (Comm(19),Zap)
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
 100  entn1 = Icfiat
   lmt1 = funlg*entn1
   lmt2 = lmt1 + 1
   lmt3 = fculg*entn1
   IF ( fculg>=fmxlg ) THEN
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
 200  s = lshift(1,Nbpw-1)
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
   IF ( K>0 ) GOTO 800
   GOTO 500
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
   DO n = 1 , lmt3 , entn1
      IF ( xj1==fdbn(n) .AND. xj2==fdbn(n+1) ) THEN
         IF ( n<=lmt1 ) THEN
            IF ( iexec>=0 ) GOTO 700
            Hold = andf(rxmsk,file(n))
            lmt4 = n + entn1 - 1
            DO m = n , lmt4
               file(m) = 0
            ENDDO
            file(n) = Hold
!
            EXIT
         ELSEIF ( iexec>=0 ) THEN
!
            IF ( andf(rmsk,file(n))==zap ) THEN
!
!     UNPURGE (CLEAR THE ENTRY)
!
               lmt4 = n + entn1 - 1
               DO m = n , lmt4
                  file(m) = 0
               ENDDO
            ENDIF
            GOTO 700
         ELSE
            file(n) = zap
            GOTO 600
         ENDIF
      ENDIF
   ENDDO
   IF ( iexec>=0 ) GOTO 700
   file(nfculg) = zap
   fdbn(nfculg) = xj1
   fdbn(nfculg+1) = xj2
   fculg = fculg + incr
   IF ( fculg>=fmxlg ) THEN
      ASSIGN 600 TO iback
      GOTO 1200
   ELSE
      nfculg = nfculg + entn1
   ENDIF
!
 600  CALL xpolck(xj1,xj2,fn,l)
   IF ( fn/=0 ) THEN
      ddbn(l) = 0
      ddbn(l+1) = 0
   ENDIF
!
 700  J = J + 2
   IF ( J-2==i+1 .AND. K>0 ) J = J + 1
   IF ( J<jpt ) GOTO 500
   GOTO 400
!
!     EQUIV LOGIC FOLLOWS
!
 800  xj1 = X(J)
   xj2 = X(J+1)
   DO n = 1 , lmt3 , entn1
      IF ( xj1==fdbn(n) .AND. xj2==fdbn(n+1) ) THEN
         IF ( J==i+1 ) THEN
!
!     PRIMARY
!
            Prisav = andf(rxmsk,file(n))
            IF ( iexec>=0 ) GOTO 1100
!
!     IF PRIMARY FILE IS PURGED OR HAS ZERO TRAILERS, PURGE SECONDARYS
!
            IF ( andf(rmsk,Prisav)==zap ) GOTO 700
            IF ( fmat(n)==0 .AND. fmat(n+1)==0 .AND. fmat(n+2)==0 ) THEN
               IF ( .NOT.(entn1==11 .AND. (fmat(n+5)/=0 .OR. fmat(n+6)/=0 .OR. fmat(n+7)/=0)) ) GOTO 700
            ENDIF
            fequ(n) = orf(s,fequ(n))
            Nsav = n
            CALL xpolck(xj1,xj2,fnsav,lsav)
            IF ( fnsav/=0 ) dfnu(lsav) = orf(s,dfnu(lsav))
!
!     IF PRIMARY FILE CONTAINS OTHER UNEQUIV D.B.- CLEAR THEM
!
            DO jx = 1 , lmt3 , entn1
               IF ( fequ(jx)>=0 .AND. jx/=n ) THEN
                  IF ( Prisav==andf(rxmsk,file(jx)) ) THEN
                     lmt4 = jx + entn1 - 1
                     DO m = jx , lmt4
                        file(m) = 0
                     ENDDO
                     IF ( jx<=lmt1 ) file(jx) = Prisav
                  ENDIF
               ENDIF
            ENDDO
            GOTO 1100
!
!     SECONDARY
!
         ELSEIF ( iexec>=0 ) THEN
            IF ( fequ(n)<0 .AND. Prisav==andf(rxmsk,file(n)) ) THEN
!
!     UNEQUIV (CLEAR SEC EQUIV ENTRY)
!
               lmt4 = n + entn1 - 1
               DO m = n , lmt4
                  file(m) = 0
               ENDDO
               IF ( n<=lmt1 ) file(n) = Prisav
               CALL xpolck(xj1,xj2,fn,l)
               IF ( fn/=0 ) THEN
                  ddbn(l) = 0
                  ddbn(l+1) = 0
               ENDIF
            ENDIF
            GOTO 1100
         ELSE
            IF ( Prisav/=0 ) THEN
               IF ( file(n)<0 .AND. andf(rxmsk,file(n))/=Prisav ) secchn = andf(rmsk,file(n))
               IF ( n<=lmt1 ) THEN
                  file(nfculg) = orf(andf(lxmsk,file(n)),Prisav)
               ELSE
                  file(n) = orf(andf(lxmsk,file(n)),Prisav)
                  fequ(n) = orf(s,fequ(n))
                  fmat(n) = fmat(Nsav)
                  fmat(n+1) = fmat(Nsav+1)
                  fmat(n+2) = fmat(Nsav+2)
                  IF ( entn1==11 ) THEN
                     fmat(n+5) = fmat(Nsav+5)
                     fmat(n+6) = fmat(Nsav+6)
                     fmat(n+7) = fmat(Nsav+7)
                  ENDIF
                  GOTO 1000
               ENDIF
            ENDIF
            Hold = andf(rxmsk,file(n))
            lmt4 = n + entn1 - 1
            DO m = n , lmt4
               file(m) = 0
            ENDDO
            IF ( n<=lmt1 ) file(n) = Hold
            IF ( Prisav==0 ) GOTO 1000
            GOTO 900
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
            ddbn(ssav) = 0
            ddbn(ssav+1) = 0
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
      file(nfculg) = Prisav
   ELSE
!
!     -IF PRIMARY, IT MUST BE ON POOL
!
      CALL xpolck(xj1,xj2,fnsav,lsav)
      IF ( fnsav==0 ) GOTO 700
      dfnu(lsav) = orf(s,dfnu(lsav))
      GOTO 1100
   ENDIF
 900  fequ(nfculg) = orf(s,fequ(nfculg))
   fdbn(nfculg) = xj1
   fdbn(nfculg+1) = xj2
   fmat(nfculg) = fmat(Nsav)
   fmat(nfculg+1) = fmat(Nsav+1)
   fmat(nfculg+2) = fmat(Nsav+2)
   IF ( entn1==11 ) THEN
      fmat(nfculg+5) = fmat(Nsav+5)
      fmat(nfculg+6) = fmat(Nsav+6)
      fmat(nfculg+7) = fmat(Nsav+7)
   ENDIF
   fculg = fculg + incr
   IF ( fculg>=fmxlg ) THEN
      ASSIGN 1000 TO iback
      GOTO 1200
   ELSE
      nfculg = nfculg + entn1
   ENDIF
!
!     WAS SECONDARY FILE IN FIAT ALREADY EQUIV TO OTHERS
!
 1000 IF ( secchn/=0 ) THEN
!
!     SEC. FILE WAS EQUIV - DRAG ALONG ALL EQUIVS
!
      DO ij = 1 , lmt3 , entn1
         IF ( file(ij)<0 ) THEN
            IF ( ij/=n ) THEN
               IF ( andf(rmsk,file(ij))==secchn ) THEN
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
                  X(m1+3) = fdbn(ij)
                  X(m1+4) = fdbn(ij+1)
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
      IF ( dfnu(l)<0 .AND. fnsav/=fn ) THEN
!
!     SEC. FILE ON POOL WAS EQUIV - DRAG ALONG ALL EQUIVS
!
         lmt4 = dculg*entn4
         m = lmt4 + 1
         DO ij = 1 , lmt4 , entn4
            IF ( dfnu(ij)<0 .AND. ij/=l ) THEN
               IF ( andf(rmsk,dfnu(ij))==fn ) THEN
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
                     X(m1+3) = ddbn(ij)
                     X(m1+4) = ddbn(ij+1)
                     X(m1+5) = ivps
                  ELSE
                     ddbn(m) = ddbn(ij)
                     ddbn(m+1) = ddbn(ij+1)
                     dfnu(m) = dfnu(lsav)
                     dculg = dculg + 1
                     IF ( dculg>dmxlg ) GOTO 1300
                     m = m + entn4
                  ENDIF
                  ddbn(ij) = 0
                  ddbn(ij+1) = 0
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
         ddbn(l) = 0
         ddbn(l+1) = 0
      ELSE
         ddbn(l) = 0
         ddbn(l+1) = 0
         GOTO 1100
      ENDIF
!
!     IF SECONDARY FILE IS NOT ON POOL AND PRIMARY IS - ADD SEC REF
!
   ELSEIF ( fnsav==0 ) THEN
      GOTO 1100
   ENDIF
   m = dculg*entn4 + 1
   ddbn(m) = xj1
   ddbn(m+1) = xj2
   dfnu(m) = dfnu(lsav)
   dculg = dculg + 1
   IF ( dculg>dmxlg ) GOTO 1300
!
 1100 J = J + 2
   IF ( J-2==i+1 ) J = J + 1
   secchn = 0
   IF ( J<jpt ) GOTO 800
   Prisav = 0
   GOTO 400
 1200 IF ( fculg>fmxlg ) THEN
!
!     ERROR MESSAGES
!
      WRITE (Outtap,99001) Sfm
99001 FORMAT (A25,' 1201, FIAT OVERFLOW.')
      GOTO 1400
   ELSE
      DO nn = lmt2 , lmt3 , entn1
         IF ( file(nn)>=0 .AND. andf(zap,file(nn))/=zap ) THEN
            IF ( fmat(nn)==0 .AND. fmat(nn+1)==0 .AND. fmat(nn+2)==0 ) THEN
               IF ( .NOT.(entn1==11 .AND. (fmat(nn+5)/=0 .OR. fmat(nn+6)/=0 .OR. fmat(nn+7)/=0)) ) THEN
                  nfculg = nn
                  incr = 0
                  GOTO 1250
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      nfculg = nfculg + entn1
      incr = 1
 1250 GOTO iback
   ENDIF
 1300 WRITE (Outtap,99002) Sfm
99002 FORMAT (A25,' 1202, DPL OVERFLOW.')
 1400 CALL mesage(-37,0,purge1)
END SUBROUTINE xpurge
