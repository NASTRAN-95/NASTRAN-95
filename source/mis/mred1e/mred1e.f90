!*==mred1e.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred1e
   USE c_blank
   USE c_packx
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(36) :: bigd
   INTEGER , SAVE :: dmr , nhbgss , nhcstm , nheqss , scr1
   INTEGER , DIMENSION(2) :: dmrnam
   INTEGER :: i , icode , ifile , iform , ii , imsg , irowd , it , itest , j , k , kolmns , l , locbgs , locdmr , locrgr , locsil , &
            & locstm , nbgss , nwdsd , nwdsrd , tiijd1 , tiijd2 , tittd , ttdijd , zeroij
   INTEGER , DIMENSION(7) :: itrlr
   INTEGER , DIMENSION(32) :: kompnt
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL , DIMENSION(1) :: rz
   REAL , DIMENSION(9) :: smald , ti , ttd
   EXTERNAL close , decode , fname , gmmats , makmcb , mesage , open , pack , pretrs , sfetch , smsg , sofcls , suread , transs ,   &
          & unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS SUBROUTINE GENERATES THE RIGID BODY MATRIX DMX IF FREEBODY
!     MODES ARE REQUESTED FOR THE MRED1 MODULE.
!
!     INPUT DATA
!     SOF -    BGSS   - BASIC GRID POINT IDENTIFICATION TABLE
!              EQSS   - SUBSTRUCTURE EQUIVALENCE TABLE
!              CSTM   - COORDINATE SYSTEM TRANSFORMATION MATRIX
!
!     OUTPUT DATA
!     GINO -   SCR1   - SCRATCH FILE HOLDING UNTRANSPOSED DMX MATRIX
!              DMR    - RIGID BODY MATRIX
!
!     PARAMETERS
!     INPUT  - DRY    - MODULE OPERATION FLAG
!              RGRID  - FREEBODY MODES FLAGS
!                       RGRID(1) .EQ. INTERNAL GRID POINT IDENTIFICATION
!                                     NUMBER (SET IN MRED1C)
!                       RGRID(2) .EQ. NUMBER OF THE CONTRIBUTING
!                                     SUBSTRUCTURE (SET IN MRED1)
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              KORLEN - LENGTH OF OPEN CORE
!              RGRID0 - FREE BODY MODE BASIC COORDINATES
!     OTHERS - NBGSS  - NUMBER OF INTERNAL GRID IDENTIFICATION POINTS
!              LOCBGS - BEGINNING ADDRESS OF BGSS DATA
!              LOCSTM - BEGINNING ADDRESS OF CSTM DATA
!              LOCSIL - BEGINNING ADDRESS OF SIL DATA
!              SMALD  - MATRIX OF COORDINATE LOCATION DIFFERENCES (3X3)
!              TI     - MATRIX OF COORDINATE TRANSFORMATIONS (3X3)
!              BIGD   - PARTITIONED MATRIX OF TRANSFORMATIONS (6X6)
!
!                                                  T
!              TTD    - TEMPORARY MATRIX HOLDING (T SMALD) (3X3)
!              KOMPNT - ARRAY HOLDING DECODED SIL COMPONENTS
!
   !>>>>EQUIVALENCE (Rz(1),Z(1))
   DATA modnam/4HMRED , 4H1E  /
   DATA nhbgss , nhcstm , nheqss/4HBGSS , 4HCSTM , 4HEQSS/
   DATA scr1 , dmr/301 , 204/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     TEST FOR MODULE ERRORS
!
         IF ( dry/=-2 ) THEN
!
!     TEST FOR FREEBODY MODES REQUEST
!
            IF ( rgrid(1)/=-1 ) THEN
!
!     READ BGSS DATA
!
               it = 1
               CALL sfetch(oldnam,nhbgss,1,itest)
               IF ( itest==3 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( itest==4 ) THEN
                  imsg = -2
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( itest==5 ) THEN
                  imsg = -3
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  CALL suread(z(korbgn),-1,nwdsrd,itest)
!
!     EXTRACT SUBSTRUCTURE IP DATA
!
                  nbgss = z(korbgn+2)
                  locbgs = korbgn
                  CALL suread(z(korbgn),-2,nwdsrd,itest)
                  korbgn = korbgn + nwdsrd
!
!     READ CSTM DATA
!
                  locstm = korbgn
                  IF ( korlen<=locstm ) THEN
!
!     PROCESS SYSTEM FATAL ERRORS
!
                     imsg = -8
                     ifile = 0
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     it = 2
                     CALL sfetch(oldnam,nhcstm,1,itest)
                     IF ( itest/=3 ) THEN
                        IF ( itest==4 ) THEN
                           imsg = -2
                           spag_nextblock_1 = 5
                           CYCLE SPAG_DispatchLoop_1
                        ELSEIF ( itest==5 ) THEN
                           imsg = -3
                           spag_nextblock_1 = 5
                           CYCLE SPAG_DispatchLoop_1
                        ELSE
                           CALL suread(z(locstm),-2,nwdsrd,itest)
                           CALL pretrs(z(locstm+3),nwdsrd-4)
                        ENDIF
                     ENDIF
!
!     CHECK FOR BASIC COORDINATES
!
                     DO i = 1 , 3
                        rgrid0(i) = 0.0
                     ENDDO
                     IF ( rgrid(1)/=0 ) THEN
!
!     EXTRACT FREEBODY BASIC COORDINATES
!
                        locrgr = locbgs + (4*(rgrid(1)-1))
                        DO i = 1 , 3
                           rgrid0(i) = rz(locrgr+i)
                        ENDDO
                     ENDIF
!
!     OPEN SCRATCH FILE
!
                     ifile = scr1
                     itrlr(1) = ifile
                     CALL open(*20,scr1,z(gbuf2),1)
                     typin = 1
                     typpck = 1
                     irowp = 1
                     lrowp = 6
                     incrp = 1
!
!     OPEN EQSS FILE AND CHECK OPEN CORE LENGTH
!
                     it = 3
                     CALL sfetch(oldnam,nheqss,1,itest)
                     IF ( itest==3 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( itest==4 ) THEN
                        imsg = -2
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ELSEIF ( itest==5 ) THEN
                        imsg = -3
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        locsil = locstm + nwdsrd
                        CALL suread(z(locsil),-1,nwdsrd,itest)
                        IF ( korlen<=locsil ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
!
!     READ UP TO SIL DATA
!
                        IF ( korlen<=2*nsil ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        DO i = 1 , ncsubs
                           CALL suread(z(locsil),-1,nwdsrd,itest)
                           IF ( korlen<=locsil+nwdsrd ) THEN
                              spag_nextblock_1 = 4
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDDO
!
!     GENERATE SMALD MATRIX (3X3)
!
!                **                               **
!                *                                 *
!                *    0.0      DELTA(Z)  -DELTA(Y) *
!                *                                 *
!        SMALD = * -DELTA(Z)     0.0      DELTA(X) *
!                *                                 *
!                *  DELTA(Y)  -DELTA(X)     0.0    *
!                *                                 *
!                **                               **
!
                        DO i = 1 , nbgss
                           ii = 4*(i-1)
                           smald(1) = 0.0
                           smald(2) = rz(locbgs+ii+3) - rgrid0(3)
                           smald(3) = -rz(locbgs+ii+2) + rgrid0(2)
                           smald(4) = -smald(2)
                           smald(5) = 0.0
                           smald(6) = rz(locbgs+ii+1) - rgrid0(1)
                           smald(7) = -smald(3)
                           smald(8) = -smald(6)
                           smald(9) = 0.0
!
!     SELECT TI, TTD MATRIX GENERATION
!
                           IF ( z(locbgs+ii)<0 ) THEN
!
!     SCALAR POINT ADDS NULL COLUMN TO BIGD
!     (CID .LT. 0)
!
                              DO j = 1 , 6
                                 bigd(j) = 0.0
                              ENDDO
                              irowp = 1
                              CALL pack(bigd(1),scr1,itrlr)
                              CYCLE
                           ELSEIF ( z(locbgs+ii)==0 ) THEN
!
!     GENERATE TI, TTD MATRICES (3X3)
!     (CID .EQ. 0)
!
                              DO j = 1 , 3
                                 DO k = 1 , 3
                                    l = k + 3*(j-1)
                                    ti(l) = 0.0
                                    IF ( j==k ) ti(l) = 1.0
                                    ttd(l) = smald(l)
                                 ENDDO
                              ENDDO
                           ELSE
!
!     GENERATE TI, TTD MATRICES (3X3)
!     (CID .GT. 0)
!
                              CALL transs(z(locbgs+ii),ti)
                              CALL gmmats(ti,3,3,0,smald,3,3,1,ttd)
                           ENDIF
!
!     GENERATE BIGD MATRIX (6X6)
!
!               **            **
!               *    .         *
!               *  T .  T      *
!               * T  . T SMALD *
!               *    .         *
!        BIGD = *..............*
!               *    .         *
!               *    .    T    *
!               * 0  .  T      *
!               *    .         *
!               **            **
!
                           DO j = 1 , 3
                              DO k = 1 , 3
                                 tittd = k + 3*(j-1)
                                 tiijd1 = k + 6*(j-1)
                                 ttdijd = tiijd1 + 3
                                 zeroij = tiijd1 + 18
                                 tiijd2 = tiijd1 + 21
                                 bigd(tiijd1) = ti(tittd)
                                 bigd(ttdijd) = ttd(tittd)
                                 bigd(zeroij) = 0.0
                                 bigd(tiijd2) = ti(tittd)
                              ENDDO
                           ENDDO
!
!     EXTRACT ROWS OF BIGD CORRESPONDING TO ACTIVE SIL COMPONENTS
!
                           CALL suread(z(locsil),2,nwdsrd,itest)
                           icode = z(locsil+1)
                           CALL decode(icode,kompnt,nwdsd)
                           DO j = 1 , nwdsd
                              irowd = 1 + 6*kompnt(j)
                              CALL pack(bigd(irowd),scr1,itrlr)
                           ENDDO
                        ENDDO
                        CALL close(scr1,1)
                        itrlr(3) = lrowp
!
!     READ SCR1 INTO TRANSPOSED FORM
!
                        CALL open(*20,scr1,z(gbuf1),0)
                        typunp = 1
                        irowup = 1
                        lrowup = 6
                        incrup = itrlr(2)
                        kolmns = itrlr(2)
                        korbgn = locbgs
                        IF ( korlen<=korbgn+lrowp*kolmns ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        DO i = 1 , kolmns
                           spag_nextblock_2 = 1
                           SPAG_DispatchLoop_2: DO
                              SELECT CASE (spag_nextblock_2)
                              CASE (1)
                                 CALL unpack(*2,scr1,z(korbgn))
                                 spag_nextblock_2 = 2
                                 CYCLE SPAG_DispatchLoop_2
 2                               j = korbgn
                                 DO k = 1 , 6
                                    rz(j) = 0.0
                                    j = j + incrup
                                 ENDDO
                                 spag_nextblock_2 = 2
                              CASE (2)
                                 korbgn = korbgn + 1
                                 EXIT SPAG_DispatchLoop_2
                              END SELECT
                           ENDDO SPAG_DispatchLoop_2
                        ENDDO
                        CALL close(scr1,1)
!
!     PLACE TRANSPOSED BIGD ONTO DMR OUTPUT FILE
!
                        ifile = dmr
                        CALL open(*20,dmr,z(gbuf2),1)
                        CALL fname(dmr,dmrnam)
                        CALL write(dmr,dmrnam,2,1)
                        locdmr = locbgs
                        lrowp = kolmns
                        iform = 2
                        CALL makmcb(itrlr,dmr,lrowp,iform,typin)
                        DO i = 1 , 6
                           CALL pack(z(locdmr),dmr,itrlr)
                           locdmr = locdmr + kolmns
                        ENDDO
                        CALL close(dmr,1)
                        CALL wrttrl(itrlr)
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         RETURN
 20      imsg = -1
         spag_nextblock_1 = 3
      CASE (3)
         CALL sofcls
         CALL mesage(imsg,ifile,modnam)
         spag_nextblock_1 = 2
      CASE (4)
!
!     PROCESS MODULE FATAL ERRORS
!
         imsg = -1
         spag_nextblock_1 = 5
      CASE (5)
         IF ( it<2 ) THEN
            CALL smsg(imsg,nhbgss,oldnam)
         ELSEIF ( it==2 ) THEN
!
            CALL smsg(imsg,nhcstm,oldnam)
         ELSE
!
            CALL smsg(imsg,nheqss,oldnam)
         ENDIF
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE mred1e
