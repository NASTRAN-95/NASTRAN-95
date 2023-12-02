!*==gipsst.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gipsst
   USE c_gicom
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zntpkx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: an
   REAL , DIMENSION(20) :: block
   INTEGER :: buff , buff1 , buff2 , ctype , i , ibcc , ibtyp , ic , ifil , ipass , ipk , is , isng , j , jj , k , k2 , k3 , kcoln ,&
            & kd , kn , lc , max , n , n1 , n2 , nbg , ncore , ncstm , nd , newid , ngset , nkset , nn , nogo , np , nrgs , nwds ,  &
            & nwr , oldid , pbgpt , pcstm , pg , pk , proe , prol , psil , pte , ptl , slope , type
   REAL :: bx , by , bz , pendc , sign , t1 , t2 , t3 , t4 , txl , tyl
   INTEGER , DIMENSION(16) :: ccard
   INTEGER , DIMENSION(28) :: iz
   LOGICAL :: kcol , oxr , oyr , zap
   INTEGER , DIMENSION(2) , SAVE :: ns
   REAL , DIMENSION(3) :: rol , t
   INTEGER , DIMENSION(10) :: scard
   REAL , DIMENSION(10) :: srard
   REAL , DIMENSION(9) :: tg , tl , tt
   INTEGER , DIMENSION(7) :: tgkg , trl
   REAL , DIMENSION(18) , SAVE :: tgs
   REAL , DIMENSION(1) :: z
   EXTERNAL bldpk , bldpki , bldpkn , close , cyct2b , gmmats , gopen , intpk , korsz , lsplin , mesage , pretrs , rdtrl , read ,   &
          & ssplin , transs , wrttrl , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         pendc = 28
         nogo = 0
         oldid = -1
         lc = -1
         nwds = korsz(iz)
         buff = nwds - sysbuf
         buff1 = buff - sysbuf
         buff2 = buff1 - sysbuf
         trl(1) = cstm
         CALL rdtrl(trl)
         IF ( trl(1)<0 ) trl(3) = 0
         ncstm = (trl(3)+1)*14
         pcstm = buff2 - ncstm
         trl(1) = bagpdt
         CALL rdtrl(trl)
         nbg = trl(2)*4
         pbgpt = pcstm - nbg
         trl(1) = scr1
         CALL rdtrl(trl)
         max = trl(2)
         IF ( trl(3)==0 ) RETURN
         i = scr2
         scr2 = scr3
         scr3 = i
         ipass = 0
!
!     INITIAL CORE CHECK
!
         IF ( pbgpt<2*max ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     OPEN SCR1 TO LOOP ON G AND K SET RECORDS
!
         CALL gopen(scr1,iz(buff+1),0)
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ IN CSTM AT PCSTM + 14 ADD BASIC COORD SYSTEM
!
         iz(pcstm) = 0
         iz(pcstm+1) = 1
         DO i = 2 , 13
            z(pcstm+i) = 0.0
         ENDDO
         z(pcstm+5) = 1.0
         z(pcstm+9) = 1.0
         z(pcstm+13) = 1.0
         IF ( ncstm/=14 ) THEN
            ifil = cstm
            CALL gopen(cstm,iz(buff1+1),0)
            CALL read(*60,*60,cstm,iz(pcstm+14),ncstm-14,1,nwr)
            CALL close(cstm,1)
         ENDIF
         CALL pretrs(iz(pcstm),ncstm)
!
!     READ IN BAGPDT AT PBGPT
!
         ifil = bagpdt
         CALL gopen(bagpdt,iz(buff1+1),0)
         CALL read(*60,*60,bagpdt,iz(pbgpt),nbg,1,nwr)
         CALL close(bagpdt,1)
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ SCR1 AND PROCESS A SPLINE DEPENDING ON TYPE
!
         n1 = max + 1
         ifil = scr1
         CALL read(*40,*20,scr1,iz(1),n1,1,nwr)
 20      j = 2
         type = iz(j)
         pg = pendc
         pk = pg + ngset
         psil = pk + nkset
         ipk = psil + ngset
         np = ngset + nkset
!
!     USE A K POINT TO PICK UP POINTER TO BAGPDT FOR
!     COORDINATE SYSTEM ID OF SPLINE
!
         newid = ccard(2)
         ctype = ccard(8)
         k = pcstm
         j = pcstm + ncstm - 1
         IF ( newid/=oldid ) THEN
            DO i = k , j , 14
               IF ( iz(i)==newid ) THEN
                  proe = i + 2
                  pte = i + 5
                  oldid = newid
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            ic = newid
            CALL mesage(30,25,ic)
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         IF ( type==2 ) THEN
!
!     LINEAR SPLINE
!
            IF ( ctype==1 .OR. ctype==3 .OR. ctype==4 .OR. ctype==5 ) THEN
               kd = 2
            ELSE
!
!     CAERO2 PROSESSING   BODIES
!
               scard(8) = newid
               scard(9) = scard(10)
               scard(10) = -1.0
               ibtyp = ccard(16)
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
            IF ( scard(8)/=lc ) THEN
               DO i = k , j , 14
                  IF ( scard(8)==iz(i) ) THEN
                     lc = scard(8)
                     prol = i + 2
                     ptl = i + 5
                     GOTO 30
                  ENDIF
               ENDDO
               ic = scard(8)
               CALL mesage(30,25,ic)
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
 30         IF ( newid/=0 .OR. scard(8)/=0 ) THEN
               t1 = z(prol) - z(proe)
               t2 = z(prol+1) - z(proe+1)
               t3 = z(prol+2) - z(proe+2)
               t1 = z(pte+2)*t1 + z(pte+5)*t2 + z(pte+8)*t3
               t2 = z(pte+5)*t1
               t3 = z(pte+8)*t1
               t1 = z(pte+2)*t1
               rol(1) = z(prol) - t1
               rol(2) = z(prol+1) - t2
               rol(3) = z(prol+2) - t3
               t1 = z(ptl+4)*z(pte+8) - z(ptl+7)*z(pte+5)
               t2 = z(ptl+7)*z(pte+2) - z(ptl+1)*z(pte+8)
               t3 = z(ptl+1)*z(pte+5) - z(ptl+4)*z(pte+2)
               t4 = sqrt(t1*t1+t2*t2+t3*t3)
               IF ( t4==0.0 ) THEN
                  WRITE (out,99001) ufm , scard(1) , ccard(1)
99001             FORMAT (A23,' 2261, PLANE OF LINEAR SPLINE',I9,' PERPENDICULAR TO PLANE OF AERO ELEMENT',I9)
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  tl(1) = t1/t4
                  tl(4) = t2/t4
                  tl(7) = t3/t4
                  tl(2) = z(pte+5)*tl(7) - z(pte+8)*tl(4)
                  tl(5) = z(pte+8)*tl(1) - z(pte+2)*tl(7)
                  tl(8) = z(pte+2)*tl(4) - z(pte+5)*tl(1)
                  tl(3) = z(pte+2)
                  tl(6) = z(pte+5)
                  tl(9) = z(pte+8)
               ENDIF
            ENDIF
            DO i = 1 , np
!
!     BASIC CORD
!
               k = (iz(pg+i)-1)*4
               bx = z(pbgpt+k+1)
               by = z(pbgpt+k+2)
               bz = z(pbgpt+k+3)
               IF ( newid==0 .AND. scard(8)==0 ) THEN
                  z(n1) = bx
                  z(n1+1) = by
               ELSE
                  t1 = bx - rol(1)
                  t2 = by - rol(2)
                  t3 = bz - rol(3)
                  z(n1) = tl(1)*t1 + tl(4)*t2 + tl(7)*t3
                  z(n1+1) = tl(2)*t1 + tl(5)*t2 + tl(8)*t3
               ENDIF
               n1 = n1 + 2
            ENDDO
            IF ( ctype==2 ) THEN
               n1 = max + 1
               DO i = 1 , np
                  z(n1+1) = z(n1)
                  z(n1) = 0.0
                  n1 = n1 + 2
               ENDDO
            ENDIF
!
!     CHECK CORE
!
            k = max + 1
            j = k + 2*ngset
            ncore = pbgpt - n1
            oyr = .FALSE.
            oxr = .FALSE.
            IF ( srard(9)<0.0 ) oxr = .TRUE.
            IF ( srard(10)<0.0 ) oyr = .TRUE.
            is = 3
            IF ( oxr ) is = is - 1
            IF ( oyr ) is = is - 1
            n = is*ngset + 3
            nd = nkset*(1+kd)
            nn = n*n + 3*n + n*nd + nd*ngset*is
            IF ( nn>=ncore ) THEN
               ncore = buff2 - n1
               IF ( nn>ncore ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               zap = .TRUE.
            ENDIF
!
!     GET G FOR A LINEAR SPLINE
!
            CALL lsplin(ngset,iz(k),nkset,iz(j),0,kd,1,scard(6),scard(9),scard(10),scard(7),iz(n1),ncore,isng)
            IF ( isng==2 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( nogo==1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( ctype/=2 ) THEN
!
!     TRANSFORM G TO SPLINE COORDINATES
!
               tyl = 1.0
               txl = 0.0
               IF ( newid/=0 .OR. scard(8)/=0 ) THEN
                  tyl = z(pte+1)*tl(2) + z(pte+4)*tl(5) + z(pte+7)*tl(8)
                  txl = z(pte+1)*tl(1) + z(pte+4)*tl(4) + z(pte+7)*tl(7)
               ENDIF
!
!     MOVE COLUMNS UP
!
               nrgs = ngset*is
               k2 = nrgs + nrgs
               k3 = k2 + nrgs
               ncore = n1
               n1 = n1 + nrgs - 1
               n2 = n1
               DO i = 1 , nkset
                  DO k = 1 , nrgs
                     z(n2+k) = z(n1+k)*txl + z(n1+nrgs+k)*tyl
                     z(n2+nrgs+k) = z(n1+k2+k)
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
            IF ( ctype==2 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            is = 1
            ptl = pte
            DO i = 1 , 9
               tl(i) = z(pte+i-1)
            ENDDO
            DO i = 1 , np
               k = (iz(pg+i)-1)*4
!
!     BASIC COORDINATES
!
               bx = z(pbgpt+k+1)
               by = z(pbgpt+k+2)
               bz = z(pbgpt+k+3)
               IF ( newid==0 ) THEN
                  z(n1) = bx
                  z(n1+1) = by
               ELSE
!
!     X AND Y OF SPLINE
!
                  t1 = bx - z(proe)
                  t2 = by - z(proe+1)
                  t3 = bz - z(proe+2)
                  z(n1) = z(pte)*t1 + z(pte+3)*t2 + z(pte+6)*t3
                  z(n1+1) = z(pte+1)*t1 + z(pte+4)*t2 + z(pte+7)*t3
               ENDIF
               n1 = n1 + 2
            ENDDO
            k = max + 1
            j = k + 2*ngset
            ncore = pbgpt - n1
!
!     CORE CHECK
!
            n = ngset + 3
            nd = nkset*2
            nn = n*n + 3*n + n*nd + nd*ngset
            IF ( nn>=ncore ) THEN
               ncore = buff2 - n1
               IF ( nn>ncore ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               zap = .TRUE.
            ENDIF
!
!     GET G FOR A SURFACE SPLINE
!
            CALL ssplin(ngset,iz(k),nkset,iz(j),0,0,1,1,scard(6),iz(n1),ncore,isng)
            IF ( isng==2 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( nogo==1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     REVERSE SIGN OF SLOPE COLUMN
!
            k = n1
            DO i = 1 , nkset
               k = k + ngset
               DO j = 1 , ngset
                  z(k) = -z(k)
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
         CALL gopen(scr2,iz(buff1+1),1)
         CALL gopen(scr3,iz(buff2+1),0)
         tgkg(3) = gsize
         tgkg(4) = 2
         tgkg(5) = 1
         tgkg(1) = scr2
         tgkg(2) = 0
         tgkg(6) = 0
         tgkg(7) = 0
         ibcc = 1
         sign = 1.0
         slope = 1
         kcol = .FALSE.
         kn = 1
         kcoln = iz(ipk+kn)
!
!     KCOLN PICKS UP COLUMN NUMBER TO INSERT
!     KN POINT TO COLUMN OF G MATRIX
!     SLOPE IS FLIP FLOP SWITCH FOR SLOPE COLUMN (KEEPS KCOL TRUE)
!
!
!     LOOP THROUGH COLUMNS OF GKT
!
         DO i = 1 , ksize
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  CALL bldpk(1,1,scr2,block,1)
                  IF ( kcoln==i ) kcol = .TRUE.
!
!     COPY A COLUMN OR OUTPUT A NULL COLUMN
!
                  CALL intpk(*32,scr3,0,1,0)
                  IF ( kcol ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  DO
                     CALL zntpki
                     CALL bldpki(a,nr,scr2,block)
                     IF ( ieol/=0 ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
 32               IF ( kcol ) THEN
!
!     LOOP THROUGH COLUMN OF G BUILDING COLUMN OF GKT
!
                     DO j = 1 , ngset
                        spag_nextblock_3 = 1
                        SPAG_DispatchLoop_3: DO
                           SELECT CASE (spag_nextblock_3)
                           CASE (1)
                              nr = iz(psil+j)
                              k = (iz(pg+j)-1)*4
                              CALL transs(iz(pbgpt+k),tt)
                              CALL gmmats(tt,3,3,1,tl,3,3,0,tg)
                              IF ( type==2 ) THEN
!
!     TERMS OF LINEAR SPLINE
!
                                 IF ( ctype==2 ) THEN
!
!     BODIES
!
                                    IF ( ibtyp==1 ) THEN
                                       spag_nextblock_3 = 2
                                       CYCLE SPAG_DispatchLoop_3
                                    ENDIF
                                    IF ( ibtyp==3 ) THEN
                                       spag_nextblock_3 = 3
                                       CYCLE SPAG_DispatchLoop_3
                                    ENDIF
                                    IF ( ibcc==1 .OR. ibcc==4 ) THEN
                                       spag_nextblock_3 = 3
                                       CYCLE SPAG_DispatchLoop_3
                                    ENDIF
                                    spag_nextblock_3 = 2
                                    CYCLE SPAG_DispatchLoop_3
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
                                    spag_nextblock_3 = 4
                                    CYCLE SPAG_DispatchLoop_3
                                 ENDIF
                              ENDIF
!
!     TERMS OF SURFACE SPLINE
!
                              DO jj = 3 , 9 , 3
                                 a = tg(jj)*z(n1)
                                 CALL bldpki(a,nr,scr2,block)
                                 nr = nr + 1
                              ENDDO
                              n1 = n1 + 1
                           CASE (2)
                              tgs(1) = tg(3)*sign
                              tgs(4) = tg(6)*sign
                              tgs(7) = tg(9)*sign
                              tgs(11) = -tg(2)*sign
                              tgs(12) = tg(1)*sign
                              tgs(14) = -tg(5)*sign
                              tgs(15) = tg(4)*sign
                              tgs(17) = -tg(8)*sign
                              tgs(18) = tg(7)*sign
                              spag_nextblock_3 = 4
                           CASE (3)
                              tgs(1) = tg(2)
                              tgs(4) = tg(5)
                              tgs(7) = tg(8)
                              tgs(11) = tg(3)
                              tgs(12) = tg(1)
                              tgs(14) = tg(6)
                              tgs(15) = tg(4)
                              tgs(17) = tg(9)
                              tgs(18) = tg(7)
                              spag_nextblock_3 = 4
                           CASE (4)
                              t(1) = z(n1)
                              n1 = n1 + 1
                              t(2) = 0.0
                              t(3) = 0.0
                              IF ( .NOT.(oxr) ) THEN
                                 t(2) = z(n1)
                                 n1 = n1 + 1
                              ENDIF
                              IF ( .NOT.(oyr) ) THEN
                                 t(3) = z(n1)
                                 n1 = n1 + 1
                              ENDIF
                              CALL gmmats(tgs,6,3,0,t,3,1,0,an)
                              DO jj = 1 , 6
                                 CALL bldpki(an(jj),nr,scr2,block)
                                 nr = nr + 1
                              ENDDO
                              EXIT SPAG_DispatchLoop_3
                           END SELECT
                        ENDDO SPAG_DispatchLoop_3
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
                              IF ( ibcc==2 .OR. ibcc==4 ) n1 = n1 - ngset*is
                              IF ( ibcc>2 ) THEN
                                 spag_nextblock_2 = 2
                                 CYCLE SPAG_DispatchLoop_2
                              ENDIF
                           ENDIF
                        ENDIF
                        slope = -slope
                        IF ( slope/=1 ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ELSE
                        n1 = n1 + ngset*is
                     ENDIF
                     kn = kn + 1
                     IF ( kn<=nkset ) kcoln = iz(ipk+kn)
                     kcol = .FALSE.
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
                  CALL bldpkn(scr2,block,tgkg)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
!
!     SWITCH FILES FOR ANOTHER SPLINE
!
         CALL close(scr2,1)
         CALL wrttrl(tgkg)
         CALL close(scr3,1)
         i = scr2
         scr2 = scr3
         scr3 = i
         ipass = ipass + 1
         IF ( zap ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     FINISHED SWITCH FILES SO OUTPUT IS SCR2
!
!
!     IF ALL DONE BE SURE SCR2 IS GTKA
!
 40      i = scr2
         scr2 = scr3
         scr3 = i
         IF ( scr3==201 ) THEN
            CALL gopen(scr2,z(buff1),0)
            CALL gopen(scr3,z(buff2),1)
            tgkg(1) = scr2
            CALL rdtrl(tgkg)
            n = tgkg(2)
            tgkg(1) = scr3
            tgkg(2) = 0
            tgkg(6) = 0
            tgkg(7) = 0
            incr = 1
            itc = 1
            CALL cyct2b(scr2,scr3,n,z,tgkg)
            CALL close(scr2,1)
            CALL close(scr3,1)
            CALL wrttrl(tgkg)
         ENDIF
         CALL close(scr1,1)
         IF ( nogo==0 ) RETURN
!
!     ERROR MESSAGES
!
         CALL mesage(-61,0,ns)
 60      CALL mesage(-3,ifil,ns)
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(-8,0,ns)
         spag_nextblock_1 = 6
      CASE (6)
         WRITE (out,99002) ufm , scard(1)
99002    FORMAT (A23,' 2260, SINGULAR MATRIX DEVELOPED WHILE PROCESSING ','SPLINE',I9)
         spag_nextblock_1 = 9
      CASE (7)
         WRITE (out,99003) ufm , scard(1)
99003    FORMAT (A23,' 2262, SPLINE',I9,' INCLUDES AERO BOX INCLUDED ON A',' EARLIER SPLINE')
         spag_nextblock_1 = 9
      CASE (8)
         WRITE (out,99004) ufm , scard(1)
99004    FORMAT (A23,' 2263, INSUFFICIENT CORE TO PROCESS SPLINE',I9)
         spag_nextblock_1 = 9
      CASE (9)
         nogo = 1
         spag_nextblock_1 = 3
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gipsst
