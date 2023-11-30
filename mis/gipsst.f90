
SUBROUTINE gipsst
   IMPLICIT NONE
   REAL A , Dum(3) , Srard(10) , Z(1)
   INTEGER Bagpdt , Ccard(16) , Cstm , Eqaero , Gm , Go , Gsize , Gtka , Ieol , Ieor , Ii , Incr , Itc , Iz(28) , Izx(1) , J1 ,     &
         & Ksize , Ngset , Nkset , Nr , Out , Scard(10) , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Sila , Spline , Sysbuf , Useta
   CHARACTER*23 Ufm
   COMMON /gicom / Spline , Useta , Cstm , Bagpdt , Sila , Eqaero , Gm , Go , Gtka , Ksize , Gsize , Scr1 , Scr2 , Scr3 , Scr4 ,    &
                 & Scr5
   COMMON /system/ Sysbuf , Out
   COMMON /unpakx/ Itc , Ii , J1 , Incr
   COMMON /xmssg / Ufm
   COMMON /zntpkx/ A , Dum , Nr , Ieol , Ieor
   COMMON /zzzzzz/ Izx
   REAL an(6) , block(20) , bx , by , bz , pendc , rol(3) , sign , t(3) , t1 , t2 , t3 , t4 , tg(9) , tgs(18) , tl(9) , tt(9) ,     &
      & txl , tyl
   INTEGER buff , buff1 , buff2 , ctype , i , ibcc , ibtyp , ic , ifil , ipass , ipk , is , isng , j , jj , k , k2 , k3 , kcoln ,   &
         & kd , kn , lc , max , n , n1 , n2 , nbg , ncore , ncstm , nd , newid , nn , nogo , np , nrgs , ns(2) , nwds , nwr ,       &
         & oldid , pbgpt , pcstm , pg , pk , proe , prol , psil , pte , ptl , slope , tgkg(7) , trl(7) , type
   LOGICAL kcol , oxr , oyr , zap
   INTEGER korsz
!
!     THIS SUBROUTINE LOCATES ALL THE G AND K SET POINTS IN THE SPLINE
!     COORDINATE SYSTEM AND FORMS G FOR EACH SET THEN
!     INSERTS THE G INTO THE FULL SIZED G MATRIX
!
!
!     CHANGE IN EQUIV FOR SIZE OF SCARD OR CCARD
!     NEED TO CHANGE PENDC
!
   !>>>>EQUIVALENCE (Izx(1),Iz(1),Z(1),Scard(1),Srard(1)) , (Iz(11),Ccard(1)) , (Iz(27),Ngset) , (Iz(28),Nkset)
   DATA ns/4HGIPS , 4HST  /
   DATA tgs/18*0.0/
!
   pendc = 28
   nogo = 0
   oldid = -1
   lc = -1
   nwds = korsz(Iz)
   buff = nwds - Sysbuf
   buff1 = buff - Sysbuf
   buff2 = buff1 - Sysbuf
   trl(1) = Cstm
   CALL rdtrl(trl)
   IF ( trl(1)<0 ) trl(3) = 0
   ncstm = (trl(3)+1)*14
   pcstm = buff2 - ncstm
   trl(1) = Bagpdt
   CALL rdtrl(trl)
   nbg = trl(2)*4
   pbgpt = pcstm - nbg
   trl(1) = Scr1
   CALL rdtrl(trl)
   max = trl(2)
   IF ( trl(3)==0 ) GOTO 99999
   i = Scr2
   Scr2 = Scr3
   Scr3 = i
   ipass = 0
!
!     INITIAL CORE CHECK
!
   IF ( pbgpt<2*max ) GOTO 800
!
!     OPEN SCR1 TO LOOP ON G AND K SET RECORDS
!
   CALL gopen(Scr1,Iz(buff+1),0)
!
!     READ IN CSTM AT PCSTM + 14 ADD BASIC COORD SYSTEM
!
 100  Iz(pcstm) = 0
   Iz(pcstm+1) = 1
   DO i = 2 , 13
      Z(pcstm+i) = 0.0
   ENDDO
   Z(pcstm+5) = 1.0
   Z(pcstm+9) = 1.0
   Z(pcstm+13) = 1.0
   IF ( ncstm/=14 ) THEN
      ifil = Cstm
      CALL gopen(Cstm,Iz(buff1+1),0)
      CALL read(*700,*700,Cstm,Iz(pcstm+14),ncstm-14,1,nwr)
      CALL close(Cstm,1)
   ENDIF
   CALL pretrs(Iz(pcstm),ncstm)
!
!     READ IN BAGPDT AT PBGPT
!
   ifil = Bagpdt
   CALL gopen(Bagpdt,Iz(buff1+1),0)
   CALL read(*700,*700,Bagpdt,Iz(pbgpt),nbg,1,nwr)
   CALL close(Bagpdt,1)
!
!     READ SCR1 AND PROCESS A SPLINE DEPENDING ON TYPE
!
 200  n1 = max + 1
   ifil = Scr1
   CALL read(*600,*300,Scr1,Iz(1),n1,1,nwr)
 300  j = 2
   type = Iz(j)
   pg = pendc
   pk = pg + Ngset
   psil = pk + Nkset
   ipk = psil + Ngset
   np = Ngset + Nkset
!
!     USE A K POINT TO PICK UP POINTER TO BAGPDT FOR
!     COORDINATE SYSTEM ID OF SPLINE
!
   newid = Ccard(2)
   ctype = Ccard(8)
   k = pcstm
   j = pcstm + ncstm - 1
   IF ( newid/=oldid ) THEN
      DO i = k , j , 14
         IF ( Iz(i)==newid ) THEN
            proe = i + 2
            pte = i + 5
            oldid = newid
            GOTO 400
         ENDIF
      ENDDO
      ic = newid
      CALL mesage(30,25,ic)
      GOTO 1200
   ENDIF
 400  IF ( type==2 ) THEN
!
!     LINEAR SPLINE
!
      IF ( ctype==1 .OR. ctype==3 .OR. ctype==4 .OR. ctype==5 ) THEN
         kd = 2
      ELSE
!
!     CAERO2 PROSESSING   BODIES
!
         Scard(8) = newid
         Scard(9) = Scard(10)
         Scard(10) = -1.0
         ibtyp = Ccard(16)
         kd = 1
         DO i = 2 , 8
            tl(i) = 0.0
         ENDDO
         tl(1) = 1.0
         tl(5) = 1.0
         tl(9) = 1.0
      ENDIF
!
!     FIND CORD SYSTEM OF LINEAR SPLINE
!
      IF ( Scard(8)/=lc ) THEN
         DO i = k , j , 14
            IF ( Scard(8)==Iz(i) ) THEN
               lc = Scard(8)
               prol = i + 2
               ptl = i + 5
               GOTO 450
            ENDIF
         ENDDO
         ic = Scard(8)
         CALL mesage(30,25,ic)
         GOTO 1200
      ENDIF
 450  IF ( newid/=0 .OR. Scard(8)/=0 ) THEN
         t1 = Z(prol) - Z(proe)
         t2 = Z(prol+1) - Z(proe+1)
         t3 = Z(prol+2) - Z(proe+2)
         t1 = Z(pte+2)*t1 + Z(pte+5)*t2 + Z(pte+8)*t3
         t2 = Z(pte+5)*t1
         t3 = Z(pte+8)*t1
         t1 = Z(pte+2)*t1
         rol(1) = Z(prol) - t1
         rol(2) = Z(prol+1) - t2
         rol(3) = Z(prol+2) - t3
         t1 = Z(ptl+4)*Z(pte+8) - Z(ptl+7)*Z(pte+5)
         t2 = Z(ptl+7)*Z(pte+2) - Z(ptl+1)*Z(pte+8)
         t3 = Z(ptl+1)*Z(pte+5) - Z(ptl+4)*Z(pte+2)
         t4 = sqrt(t1*t1+t2*t2+t3*t3)
         IF ( t4==0.0 ) THEN
            WRITE (Out,99001) Ufm , Scard(1) , Ccard(1)
99001       FORMAT (A23,' 2261, PLANE OF LINEAR SPLINE',I9,' PERPENDICULAR TO PLANE OF AERO ELEMENT',I9)
            GOTO 1200
         ELSE
            tl(1) = t1/t4
            tl(4) = t2/t4
            tl(7) = t3/t4
            tl(2) = Z(pte+5)*tl(7) - Z(pte+8)*tl(4)
            tl(5) = Z(pte+8)*tl(1) - Z(pte+2)*tl(7)
            tl(8) = Z(pte+2)*tl(4) - Z(pte+5)*tl(1)
            tl(3) = Z(pte+2)
            tl(6) = Z(pte+5)
            tl(9) = Z(pte+8)
         ENDIF
      ENDIF
      DO i = 1 , np
!
!     BASIC CORD
!
         k = (Iz(pg+i)-1)*4
         bx = Z(pbgpt+k+1)
         by = Z(pbgpt+k+2)
         bz = Z(pbgpt+k+3)
         IF ( newid==0 .AND. Scard(8)==0 ) THEN
            Z(n1) = bx
            Z(n1+1) = by
         ELSE
            t1 = bx - rol(1)
            t2 = by - rol(2)
            t3 = bz - rol(3)
            Z(n1) = tl(1)*t1 + tl(4)*t2 + tl(7)*t3
            Z(n1+1) = tl(2)*t1 + tl(5)*t2 + tl(8)*t3
         ENDIF
         n1 = n1 + 2
      ENDDO
      IF ( ctype==2 ) THEN
         n1 = max + 1
         DO i = 1 , np
            Z(n1+1) = Z(n1)
            Z(n1) = 0.0
            n1 = n1 + 2
         ENDDO
      ENDIF
!
!     CHECK CORE
!
      k = max + 1
      j = k + 2*Ngset
      ncore = pbgpt - n1
      oyr = .FALSE.
      oxr = .FALSE.
      IF ( Srard(9)<0.0 ) oxr = .TRUE.
      IF ( Srard(10)<0.0 ) oyr = .TRUE.
      is = 3
      IF ( oxr ) is = is - 1
      IF ( oyr ) is = is - 1
      n = is*Ngset + 3
      nd = Nkset*(1+kd)
      nn = n*n + 3*n + n*nd + nd*Ngset*is
      IF ( nn>=ncore ) THEN
         ncore = buff2 - n1
         IF ( nn>ncore ) GOTO 1100
         zap = .TRUE.
      ENDIF
!
!     GET G FOR A LINEAR SPLINE
!
      CALL lsplin(Ngset,Iz(k),Nkset,Iz(j),0,kd,1,Scard(6),Scard(9),Scard(10),Scard(7),Iz(n1),ncore,isng)
      IF ( isng==2 ) GOTO 900
      IF ( nogo==1 ) GOTO 200
      IF ( ctype/=2 ) THEN
!
!     TRANSFORM G TO SPLINE COORDINATES
!
         tyl = 1.0
         txl = 0.0
         IF ( newid/=0 .OR. Scard(8)/=0 ) THEN
            tyl = Z(pte+1)*tl(2) + Z(pte+4)*tl(5) + Z(pte+7)*tl(8)
            txl = Z(pte+1)*tl(1) + Z(pte+4)*tl(4) + Z(pte+7)*tl(7)
         ENDIF
!
!     MOVE COLUMNS UP
!
         nrgs = Ngset*is
         k2 = nrgs + nrgs
         k3 = k2 + nrgs
         ncore = n1
         n1 = n1 + nrgs - 1
         n2 = n1
         DO i = 1 , Nkset
            DO k = 1 , nrgs
               Z(n2+k) = Z(n1+k)*txl + Z(n1+nrgs+k)*tyl
               Z(n2+nrgs+k) = Z(n1+k2+k)
            ENDDO
            n1 = n1 + k3
            n2 = n2 + k2
         ENDDO
         n1 = ncore
      ENDIF
   ELSE
!
!     SURFACE SPLINE
!
      IF ( ctype==2 ) GOTO 900
      is = 1
      ptl = pte
      DO i = 1 , 9
         tl(i) = Z(pte+i-1)
      ENDDO
      DO i = 1 , np
         k = (Iz(pg+i)-1)*4
!
!     BASIC COORDINATES
!
         bx = Z(pbgpt+k+1)
         by = Z(pbgpt+k+2)
         bz = Z(pbgpt+k+3)
         IF ( newid==0 ) THEN
            Z(n1) = bx
            Z(n1+1) = by
         ELSE
!
!     X AND Y OF SPLINE
!
            t1 = bx - Z(proe)
            t2 = by - Z(proe+1)
            t3 = bz - Z(proe+2)
            Z(n1) = Z(pte)*t1 + Z(pte+3)*t2 + Z(pte+6)*t3
            Z(n1+1) = Z(pte+1)*t1 + Z(pte+4)*t2 + Z(pte+7)*t3
         ENDIF
         n1 = n1 + 2
      ENDDO
      k = max + 1
      j = k + 2*Ngset
      ncore = pbgpt - n1
!
!     CORE CHECK
!
      n = Ngset + 3
      nd = Nkset*2
      nn = n*n + 3*n + n*nd + nd*Ngset
      IF ( nn>=ncore ) THEN
         ncore = buff2 - n1
         IF ( nn>ncore ) GOTO 1100
         zap = .TRUE.
      ENDIF
!
!     GET G FOR A SURFACE SPLINE
!
      CALL ssplin(Ngset,Iz(k),Nkset,Iz(j),0,0,1,1,Scard(6),Iz(n1),ncore,isng)
      IF ( isng==2 ) GOTO 900
      IF ( nogo==1 ) GOTO 200
!
!     REVERSE SIGN OF SLOPE COLUMN
!
      k = n1
      DO i = 1 , Nkset
         k = k + Ngset
         DO j = 1 , Ngset
            Z(k) = -Z(k)
            k = k + 1
         ENDDO
      ENDDO
   ENDIF
!
!     TRANSFORM G INTO GLOBAL
!
!
!                         T
!     OPEN SCR2 TO WRITE G   MATRIX
!                         KG
!
   CALL gopen(Scr2,Iz(buff1+1),1)
   CALL gopen(Scr3,Iz(buff2+1),0)
   tgkg(3) = Gsize
   tgkg(4) = 2
   tgkg(5) = 1
   tgkg(1) = Scr2
   tgkg(2) = 0
   tgkg(6) = 0
   tgkg(7) = 0
   ibcc = 1
   sign = 1.0
   slope = 1
   kcol = .FALSE.
   kn = 1
   kcoln = Iz(ipk+kn)
!
!     KCOLN PICKS UP COLUMN NUMBER TO INSERT
!     KN POINT TO COLUMN OF G MATRIX
!     SLOPE IS FLIP FLOP SWITCH FOR SLOPE COLUMN (KEEPS KCOL TRUE)
!
!
!     LOOP THROUGH COLUMNS OF GKT
!
   DO i = 1 , Ksize
      CALL bldpk(1,1,Scr2,block,1)
      IF ( kcoln==i ) kcol = .TRUE.
!
!     COPY A COLUMN OR OUTPUT A NULL COLUMN
!
      CALL intpk(*500,Scr3,0,1,0)
      IF ( kcol ) GOTO 1000
      DO
         CALL zntpki
         CALL bldpki(A,Nr,Scr2,block)
         IF ( Ieol/=0 ) GOTO 550
      ENDDO
 500  IF ( kcol ) THEN
!
!     LOOP THROUGH COLUMN OF G BUILDING COLUMN OF GKT
!
         DO j = 1 , Ngset
            Nr = Iz(psil+j)
            k = (Iz(pg+j)-1)*4
            CALL transs(Iz(pbgpt+k),tt)
            CALL gmmats(tt,3,3,1,tl,3,3,0,tg)
            IF ( type==2 ) THEN
!
!     TERMS OF LINEAR SPLINE
!
               IF ( ctype==2 ) THEN
!
!     BODIES
!
                  IF ( ibtyp==1 ) GOTO 510
                  IF ( ibtyp==3 ) GOTO 520
                  IF ( ibcc/=1 .AND. ibcc/=4 ) GOTO 510
                  GOTO 520
               ELSEIF ( is/=1 ) THEN
                  tgs(1) = tg(3)
                  tgs(4) = tg(6)
                  tgs(7) = tg(9)
                  tgs(11) = tg(1)
                  tgs(12) = tg(2)
                  tgs(14) = tg(4)
                  tgs(15) = tg(5)
                  tgs(17) = tg(7)
                  tgs(18) = tg(8)
                  GOTO 530
               ENDIF
            ENDIF
!
!     TERMS OF SURFACE SPLINE
!
            DO jj = 3 , 9 , 3
               A = tg(jj)*Z(n1)
               CALL bldpki(A,Nr,Scr2,block)
               Nr = Nr + 1
            ENDDO
            n1 = n1 + 1
            CYCLE
 510        tgs(1) = tg(3)*sign
            tgs(4) = tg(6)*sign
            tgs(7) = tg(9)*sign
            tgs(11) = -tg(2)*sign
            tgs(12) = tg(1)*sign
            tgs(14) = -tg(5)*sign
            tgs(15) = tg(4)*sign
            tgs(17) = -tg(8)*sign
            tgs(18) = tg(7)*sign
            GOTO 530
 520        tgs(1) = tg(2)
            tgs(4) = tg(5)
            tgs(7) = tg(8)
            tgs(11) = tg(3)
            tgs(12) = tg(1)
            tgs(14) = tg(6)
            tgs(15) = tg(4)
            tgs(17) = tg(9)
            tgs(18) = tg(7)
 530        t(1) = Z(n1)
            n1 = n1 + 1
            t(2) = 0.0
            t(3) = 0.0
            IF ( .NOT.(oxr) ) THEN
               t(2) = Z(n1)
               n1 = n1 + 1
            ENDIF
            IF ( .NOT.(oyr) ) THEN
               t(3) = Z(n1)
               n1 = n1 + 1
            ENDIF
            CALL gmmats(tgs,6,3,0,t,3,1,0,an)
            DO jj = 1 , 6
               CALL bldpki(an(jj),Nr,Scr2,block)
               Nr = Nr + 1
            ENDDO
         ENDDO
!
!     COLUMN FINISHED CHECKSLOPE COLUMN NEXT OR END OF G
!
         IF ( ctype/=3 ) THEN
            IF ( ctype==2 ) THEN
               IF ( ibtyp==1 ) sign = -sign
               IF ( ibtyp==2 ) THEN
                  ibcc = ibcc + 1
                  IF ( ibcc==3 ) sign = -sign
                  IF ( ibcc==5 ) sign = -sign
                  IF ( ibcc==5 ) ibcc = 1
!
!     KEEP SLOPE NEG FOR ZY BODIES AND REPROCESS SAME COLUMN TWICE
!
                  IF ( ibcc==2 .OR. ibcc==4 ) n1 = n1 - Ngset*is
                  IF ( ibcc>2 ) GOTO 550
               ENDIF
            ENDIF
            slope = -slope
            IF ( slope/=1 ) GOTO 550
         ELSE
            n1 = n1 + Ngset*is
         ENDIF
         kn = kn + 1
         IF ( kn<=Nkset ) kcoln = Iz(ipk+kn)
         kcol = .FALSE.
      ENDIF
 550  CALL bldpkn(Scr2,block,tgkg)
   ENDDO
!
!     SWITCH FILES FOR ANOTHER SPLINE
!
   CALL close(Scr2,1)
   CALL wrttrl(tgkg)
   CALL close(Scr3,1)
   i = Scr2
   Scr2 = Scr3
   Scr3 = i
   ipass = ipass + 1
   IF ( .NOT.(zap) ) GOTO 200
   GOTO 100
!
!     FINISHED SWITCH FILES SO OUTPUT IS SCR2
!
!
!     IF ALL DONE BE SURE SCR2 IS GTKA
!
 600  i = Scr2
   Scr2 = Scr3
   Scr3 = i
   IF ( Scr3==201 ) THEN
      CALL gopen(Scr2,Z(buff1),0)
      CALL gopen(Scr3,Z(buff2),1)
      tgkg(1) = Scr2
      CALL rdtrl(tgkg)
      n = tgkg(2)
      tgkg(1) = Scr3
      tgkg(2) = 0
      tgkg(6) = 0
      tgkg(7) = 0
      Incr = 1
      Itc = 1
      CALL cyct2b(Scr2,Scr3,n,Z,tgkg)
      CALL close(Scr2,1)
      CALL close(Scr3,1)
      CALL wrttrl(tgkg)
   ENDIF
   CALL close(Scr1,1)
   IF ( nogo==0 ) GOTO 99999
!
!     ERROR MESSAGES
!
   CALL mesage(-61,0,ns)
 700  CALL mesage(-3,ifil,ns)
 800  CALL mesage(-8,0,ns)
 900  WRITE (Out,99002) Ufm , Scard(1)
99002 FORMAT (A23,' 2260, SINGULAR MATRIX DEVELOPED WHILE PROCESSING ','SPLINE',I9)
   GOTO 1200
 1000 WRITE (Out,99003) Ufm , Scard(1)
99003 FORMAT (A23,' 2262, SPLINE',I9,' INCLUDES AERO BOX INCLUDED ON A',' EARLIER SPLINE')
   GOTO 1200
 1100 WRITE (Out,99004) Ufm , Scard(1)
99004 FORMAT (A23,' 2263, INSUFFICIENT CORE TO PROCESS SPLINE',I9)
 1200 nogo = 1
   GOTO 200
99999 RETURN
END SUBROUTINE gipsst