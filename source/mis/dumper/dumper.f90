!*==dumper.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dumper
USE C_OUTPUT
USE C_SYSTEM
USE C_XCEITB
USE C_XGPI2
USE C_XGPIC
USE C_XVPS
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ap , bl , cl , dmapno , el , i , iexflg , ifin , io , iosbot , irn , ist , itype , ivps , iw , j , k , kk , ltu , mi ,&
            & ml , mplp , msave , ndb , nip , npage , nparm , ntu , nwe , ptype , recno , tp
   REAL , DIMENSION(1) :: avps , roscar
   INTEGER , SAVE :: con1 , con2 , ioff , ion , mask1 , mask2 , mask3 , mask4 , mask5
   REAL(REAL64) :: dprec , dprec1
   INTEGER , DIMENSION(96) , SAVE :: ihd
   INTEGER , DIMENSION(2) :: iname
   INTEGER , DIMENSION(3) , SAVE :: ixtra
   INTEGER , DIMENSION(300) :: loco
   INTEGER , DIMENSION(5) :: os
   INTEGER , DIMENSION(1) :: oscar
   REAL , DIMENSION(4) :: ra
   EXTERNAL andf , lshift , page , rshift
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE DUMPS THE OSCAR
!
   !>>>>EQUIVALENCE (Vps(1),Avps(1)) , (dprec,ra(1)) , (dprec1,ra(3)) , (oscar(1),roscar(1),Os(5)) , (Core(1),Os(1)) , (Iosbot,Os(3))
   DATA mask1 , mask2 , mask3 , mask4 , mask5/32767 , 32768 , 1073676288 , 1073741824 , 983040/
   DATA con1 , con2/4HCONS , 4HTANT/
   DATA ihd/2*4H     , 4H COS , 4HMIC  , 4H/ NA , 4HSTRA , 4HN DM , 4HAP C , 4HOMPI , 4HLER  , 4H- OS , 4HCAR  , 4HLIST , 4HING  ,  &
      & 82*4H    /
   DATA ixtra/4H(CON , 4HTINU , 4HED) /
   DATA ion , ioff/4HON   , 4HOFF /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE LOCO ARRAY - POINTS TO FIRST WORD IN MPL FOR MOD I
!
         j = 1
         i = 1
         SPAG_Loop_1_1: DO
            loco(i) = j
            j = j + Mpl(j)
            IF ( j>Lmpl ) THEN
!
               i = 1
               DO k = 1 , 96
                  Ihead(k) = ihd(k)
               ENDDO
               CALL page
               DO k = 1 , 3
                  Ihead(k+14) = ixtra(k)
               ENDDO
               EXIT SPAG_Loop_1_1
            ELSE
               i = i + 1
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_2: DO
!
!     PROCESS ENTRY HEADER
!
            DO WHILE ( mi/=11 )
               nwe = oscar(i)
               recno = oscar(i+1)
               mi = rshift(oscar(i+2),16)
               msave = loco(mi)
               itype = oscar(i+2) - lshift(rshift(oscar(i+2),16),16)
               iexflg = ioff
               IF ( oscar(i+5)<0 ) iexflg = ion
               dmapno = andf(Nosgn,oscar(i+5))
               Nlines = Nlines + 4
               IF ( Nlines>=Nlpp ) THEN
                  CALL page
                  Nlines = Nlines + 4
               ENDIF
               WRITE (Op,99001)
99001          FORMAT (/1X,18(4H****))
               WRITE (Op,99002) recno , itype , iexflg , oscar(i+3) , oscar(i+4) , dmapno
99002          FORMAT (2X,20HOSCAR RECORD NUMBER ,I3,5X,14HMODULE TYPE = ,I2,5X,16HEXECUTE FLAG -- ,A4,/2X,15HMODULE NAME -  ,2A4,  &
                     & 5X,21HDMAP INSTRUCTION NO. ,I3)
               i = i + 6
               nwe = nwe - 6
               IF ( itype==3 ) THEN
!
!     PROCESS CONTROL INSTRUCTIONS
!
                  irn = rshift(oscar(i),16)
                  IF ( mi/=11 .AND. mi/=12 ) THEN
                     Nlines = Nlines + 2
                     IF ( Nlines>=Nlpp ) THEN
                        CALL page
                        Nlines = Nlines + 2
                     ENDIF
                  ENDIF
                  IF ( mi/=11 .AND. mi/=12 ) WRITE (Op,99003) irn
99003             FORMAT (/10X,25HRE-ENTRY RECORD NUMBER = ,I4)
                  IF ( mi/=6 ) THEN
                     iw = oscar(i) - lshift(irn,16)
                     IF ( mi/=7 ) THEN
                        bl = rshift(Ceitbl(iw-1),16)
                        el = Ceitbl(iw-1) - lshift(bl,16)
                        ml = rshift(Ceitbl(iw),16)
                        cl = Ceitbl(iw) - lshift(ml,16)
                        Nlines = Nlines + 2
                        IF ( Nlines>=Nlpp ) THEN
                           CALL page
                           Nlines = Nlines + 2
                        ENDIF
                        IF ( mi==5 ) WRITE (Op,99004) bl , el , ml , cl , Ceitbl(iw+1) , Ceitbl(iw+2)
99004                   FORMAT (/20X,I5,1H/,I5,5X,I5,1H/,I5,5X,2A4)
                        IF ( mi==11 .OR. mi==12 ) WRITE (Op,99005) el , ml , cl
99005                   FORMAT (/20X,5X,1H/,I5,5X,I5,1H/,I5)
                     ELSE
!
!     CONDITIONAL INSTRUCTION
!
                        Nlines = Nlines + 2
                        IF ( Nlines>=Nlpp ) THEN
                           CALL page
                           Nlines = Nlines + 2
                        ENDIF
                        WRITE (Op,99006) Vps(iw-3) , Vps(iw-2)
99006                   FORMAT (/10X,21HPARAMETER FOR COND = ,2A4)
                     ENDIF
                  ENDIF
                  i = i + 1
               ELSEIF ( itype==4 ) THEN
!
!     PROCESS EXECUTIVE MODULES
!
                  IF ( mi<=3 ) THEN
!
!     PROCESS CHKPNT
!
                     ndb = oscar(i)
                     Nlines = Nlines + 2
                     IF ( Nlines>=Nlpp ) THEN
                        CALL page
                        Nlines = Nlines + 2
                     ENDIF
                     WRITE (Op,99007) ndb
99007                FORMAT (/10X,31HDATA BLOCKS TO BE CHECKPOINTED(,I2,2H ))
                     ist = i + 1
                     ifin = ist + 2*ndb - 1
                     npage = (10+ndb)/10 + 1
                     Nlines = Nlines + npage
                     IF ( Nlines>=Nlpp ) THEN
                        CALL page
                        Nlines = Nlines + npage
                     ENDIF
                     IF ( ndb/=0 ) WRITE (Op,99008) (oscar(k),k=ist,ifin)
99008                FORMAT ((20X,10(2A4,2X)),/)
                     i = i + 2*ndb + 1
                  ELSE
                     IF ( mi<=8 ) THEN
!
!     PROCESS SAVE
!
                        nparm = oscar(i)
                        Nlines = Nlines + 2
                        IF ( Nlines>=Nlpp ) THEN
                           CALL page
                           Nlines = Nlines + 2
                        ENDIF
                        WRITE (Op,99009) nparm
99009                   FORMAT (/10X,23HPARAMETERS TO BE SAVED(,I2,2H ))
                        j = 1
                        DO
                           ivps = oscar(i+1)
                           iname(1) = Vps(ivps-3)
                           iname(2) = Vps(ivps-2)
                           Nlines = Nlines + 1
                           IF ( Nlines>=Nlpp ) THEN
                              CALL page
                              Nlines = Nlines + 1
                           ENDIF
                           WRITE (Op,99010) iname(1) , iname(2) , oscar(i+2)
99010                      FORMAT (20X,2A4,2X,I5)
                           j = j + 1
                           i = i + 2
                           IF ( j>nparm ) THEN
                              i = i + 1
                              CYCLE SPAG_Loop_1_2
                           ENDIF
                        ENDDO
                     ENDIF
                     DO
                        ndb = oscar(i)
                        nwe = nwe - 1
                        Nlines = Nlines + 2
                        IF ( Nlines>=Nlpp ) THEN
                           CALL page
                           Nlines = Nlines + 2
                        ENDIF
                        IF ( mi==9 ) WRITE (Op,99011) ndb
99011                   FORMAT (/10X,25HDATA BLOCKS TO BE PURGED(,I2,2H ))
                        IF ( mi==10 ) WRITE (Op,99012) ndb
99012                   FORMAT (/10X,26HDATA BLOCKS TO BE EQUIVED(,I2,2H ))
                        ist = i + 1
                        ifin = ist + 2*ndb - 1
                        IF ( mi==10 ) THEN
                           ntu = rshift(oscar(ist+2),16)
                           ltu = oscar(ist+2) - lshift(ntu,16)
                           Nlines = Nlines + 1
                           IF ( Nlines>=Nlpp ) THEN
                              CALL page
                              Nlines = Nlines + 1
                           ENDIF
                           WRITE (Op,99013) oscar(ist) , oscar(ist+1) , ntu , ltu
99013                      FORMAT (20X,19HPRIMARY DATA BLOCK ,2A4,3X,I5,1H/,I5)
                           ist = ist + 3
                           ifin = ifin + 1
                           nwe = nwe - 3
                        ENDIF
                        npage = (10+ndb)/10 + 1
                        Nlines = Nlines + npage
                        IF ( Nlines>=Nlpp ) THEN
                           CALL page
                           Nlines = Nlines + npage
                        ENDIF
                        WRITE (Op,99014) (oscar(k),k=ist,ifin)
99014                   FORMAT ((20X,10(2A4,2X)),/)
                        nwe = nwe - 2*ndb + 2
                        IF ( mi==9 ) nwe = nwe - 2
                        ivps = oscar(ifin+1)
                        Nlines = Nlines + 1
                        IF ( Nlines>=Nlpp ) THEN
                           CALL page
                           Nlines = Nlines + 1
                        ENDIF
                        IF ( ivps<0 ) WRITE (Op,99015)
99015                   FORMAT (20X,35HDEFAULT PARAMETER - ALWAYS NEGATIVE)
                        IF ( ivps>=0 ) THEN
                           WRITE (Op,99016) Vps(ivps-3) , Vps(ivps-2)
99016                      FORMAT (20X,21HCONTROL PARAMETER IS ,2A4)
                        ENDIF
                        i = i + 2*ndb + 2
                        IF ( mi==10 ) i = i + 1
                        nwe = nwe - 1
                        IF ( nwe<=0 ) CYCLE SPAG_Loop_1_2
                     ENDDO
                  ENDIF
               ELSE
                  io = 1
                  nip = oscar(i)
                  Nlines = Nlines + 2
                  IF ( Nlines>=Nlpp ) THEN
                     CALL page
                     Nlines = Nlines + 2
                  ENDIF
                  WRITE (Op,99023) nip
99023             FORMAT (/10X,29HSUMMARY OF INPUT DATA BLOCKS(,I2,2H ))
                  j = 1
                  SPAG_Loop_3_3: DO
                     iname(1) = oscar(i+1)
                     iname(2) = oscar(i+2)
                     ntu = andf(oscar(i+3),mask1)
                     tp = rshift(andf(oscar(i+3),mask2),15)
                     ltu = rshift(andf(oscar(i+3),mask3),16)
                     ap = rshift(andf(oscar(i+3),mask4),30)
                     IF ( iname(1)==0 .AND. io==1 ) THEN
                        Nlines = Nlines + 1
                        IF ( Nlines>=Nlpp ) THEN
                           CALL page
                           Nlines = Nlines + 1
                        ENDIF
                        WRITE (Op,99024) j
99024                   FORMAT (20X,24H********INPUT DATA BLOCK,I3,8H IS NULL)
                     ELSEIF ( iname(1)==0 .AND. io==0 ) THEN
                        Nlines = Nlines + 1
                        IF ( Nlines>=Nlpp ) THEN
                           CALL page
                           Nlines = Nlines + 1
                        ENDIF
                        WRITE (Op,99025) j
99025                   FORMAT (20X,25H********OUTPUT DATA BLOCK,I3,8H IS NULL)
                     ELSE
                        Nlines = Nlines + 1
                        IF ( Nlines>=Nlpp ) THEN
                           CALL page
                           Nlines = Nlines + 1
                        ENDIF
                        WRITE (Op,99026) iname(1) , iname(2) , ap , ltu , tp , ntu
99026                   FORMAT (20X,2A4,3X,I1,1H/,I5,1H/,I1,1H/,I5)
                     ENDIF
                     i = i + 3
                     j = j + 1
                     IF ( j>nip ) THEN
                        IF ( itype==2 ) io = 0
!
!     PROCESS OUTPUT DATA BLOCKS
!
                        IF ( io==0 ) THEN
!
!     PROCESS PARAMETER SECTION
!
                           i = i + 2
                           nparm = oscar(i)
                           IF ( nparm==0 ) THEN
                              spag_nextblock_1 = 4
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           j = 1
                           mplp = msave + 7
                           Nlines = Nlines + 2
                           IF ( Nlines>=Nlpp ) THEN
                              CALL page
                              Nlines = Nlines + 2
                           ENDIF
                           WRITE (Op,99027) nparm
99027                      FORMAT (/10X,22HSUMMARY OF PARAMETERS(,I2,2H ))
                           EXIT SPAG_Loop_3_3
                        ELSE
                           io = 0
                           i = i + 1
                           nip = oscar(i)
                           Nlines = Nlines + 2
                           IF ( Nlines>=Nlpp ) THEN
                              CALL page
                              Nlines = Nlines + 2
                           ENDIF
                           WRITE (Op,99028) nip
99028                      FORMAT (/10X,30HSUMMARY OF OUTPUT DATA BLOCKS(,I2,2H ))
                           j = 1
                        ENDIF
                     ENDIF
                  ENDDO SPAG_Loop_3_3
                  EXIT SPAG_Loop_1_2
               ENDIF
            ENDDO
            RETURN
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 3
      CASE (3)
         SPAG_Loop_1_4: DO
            IF ( oscar(i+1)<=0 ) THEN
               ivps = andf(Nosgn,oscar(i+1))
               iname(1) = Vps(ivps-3)
               iname(2) = Vps(ivps-2)
               ptype = rshift(andf(Vps(ivps-1),mask5),16)
               Nlines = Nlines + 1
               IF ( Nlines>=Nlpp ) THEN
                  CALL page
                  Nlines = Nlines + 1
               ENDIF
               IF ( ptype==2 ) THEN
                  WRITE (Op,99018) iname(1) , iname(2) , avps(ivps)
               ELSEIF ( ptype==3 ) THEN
                  WRITE (Op,99019) iname(1) , iname(2) , Vps(ivps) , Vps(ivps+1)
               ELSEIF ( ptype==4 ) THEN
                  ra(1) = avps(ivps)
                  ra(2) = avps(ivps+1)
                  WRITE (Op,99020) iname(1) , iname(2) , dprec
               ELSEIF ( ptype==5 ) THEN
                  WRITE (Op,99021) iname(1) , iname(2) , avps(ivps) , avps(ivps+1)
               ELSEIF ( ptype==6 ) THEN
                  ra(1) = avps(ivps)
                  ra(2) = avps(ivps+1)
                  ra(3) = avps(ivps+2)
                  ra(4) = avps(ivps+3)
                  WRITE (Op,99022) iname(1) , iname(2) , dprec , dprec1
               ELSE
                  WRITE (Op,99017) iname(1) , iname(2) , Vps(ivps)
               ENDIF
               i = i + 1
               j = j + 1
               IF ( Mpl(mplp)>0 ) mplp = mplp + ptype/3 + 1
               IF ( ptype==6 ) mplp = mplp + 1
               mplp = mplp + 1
               IF ( j>nparm ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
!
!     SEARCH MPL FOR TYPE OF VARIABLE
!
               iname(1) = con1
               iname(2) = con2
               kk = iabs(Mpl(mplp))
               Nlines = Nlines + 1
               IF ( Nlines>=Nlpp ) THEN
                  CALL page
                  Nlines = Nlines + 1
               ENDIF
               IF ( kk==2 ) THEN
                  WRITE (Op,99018) iname(1) , iname(2) , roscar(i+2)
               ELSEIF ( kk==3 ) THEN
                  WRITE (Op,99019) iname(1) , iname(2) , oscar(i+2) , oscar(i+3)
                  EXIT SPAG_Loop_1_4
               ELSEIF ( kk==4 ) THEN
                  ra(1) = roscar(i+2)
                  ra(2) = roscar(i+3)
                  WRITE (Op,99020) iname(1) , iname(2) , dprec
                  EXIT SPAG_Loop_1_4
               ELSEIF ( kk==5 ) THEN
                  WRITE (Op,99021) iname(1) , iname(2) , roscar(i+2) , roscar(i+3)
                  EXIT SPAG_Loop_1_4
               ELSEIF ( kk==6 ) THEN
                  ra(1) = roscar(i+2)
                  ra(2) = roscar(i+3)
                  ra(3) = roscar(i+4)
                  ra(4) = roscar(i+5)
                  WRITE (Op,99022) iname(1) , iname(2) , dprec , dprec1
                  i = i + 5
                  IF ( Mpl(mplp)>0 ) mplp = mplp + 4
                  mplp = mplp + 1
                  j = j + 1
                  IF ( j<=nparm ) CYCLE
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  WRITE (Op,99017) iname(1) , iname(2) , oscar(i+2)
               ENDIF
               i = i + 2
               IF ( Mpl(mplp)>0 ) mplp = mplp + 1
               mplp = mplp + 1
               j = j + 1
               IF ( j>nparm ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_4
         i = i + 3
         IF ( Mpl(mplp)>0 ) mplp = mplp + 2
         mplp = mplp + 1
         j = j + 1
         IF ( j<=nparm ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     HAVE COMPLETED FUNCTIONAL MODULE
!
         i = i + 2
         IF ( itype==2 ) i = i - 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
99017 FORMAT (20X,2A4,5H(I  ),2X,I10)
99018 FORMAT (20X,2A4,5H(R  ),2X,E15.6)
99019 FORMAT (20X,2A4,5H(BCD),5X,2A4)
99020 FORMAT (20X,2A4,5H(RDP),2X,D24.15)
99021 FORMAT (20X,2A4,5H(CSP),2X,2E15.6)
99022 FORMAT (20X,2A4,5H(CDP),2X,2D24.15)
END SUBROUTINE dumper
