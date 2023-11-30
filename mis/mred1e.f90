
SUBROUTINE mred1e
   IMPLICIT NONE
   INTEGER Dry , Gbuf1 , Gbuf2 , Idum1(6) , Idum2(3) , Idum3(6) , Idum4 , Idum5(3) , Idum6(4) , Incrp , Incrup , Irowp , Irowup ,   &
         & Korbgn , Korlen , Lrowp , Lrowup , Ncsubs , Nsil , Oldnam(2) , Rgrid(2) , Rname(2) , Typin , Typpck , Typunp , Z(1)
   REAL Rgrid0(3) , Rz(1)
   COMMON /blank / Oldnam , Dry , Idum1 , Gbuf1 , Gbuf2 , Idum2 , Korlen , Idum3 , Rgrid , Rname , Idum4 , Korbgn , Ncsubs , Idum5 ,&
                 & Nsil , Idum6 , Rgrid0
   COMMON /packx / Typin , Typpck , Irowp , Lrowp , Incrp
   COMMON /unpakx/ Typunp , Irowup , Lrowup , Incrup
   COMMON /zzzzzz/ Z
   REAL bigd(36) , smald(9) , ti(9) , ttd(9)
   INTEGER dmr , dmrnam(2) , i , icode , ifile , iform , ii , imsg , irowd , it , itest , itrlr(7) , j , k , kolmns , kompnt(32) ,  &
         & l , locbgs , locdmr , locrgr , locsil , locstm , modnam(2) , nbgss , nhbgss , nhcstm , nheqss , nwdsd , nwdsrd , scr1 ,  &
         & tiijd1 , tiijd2 , tittd , ttdijd , zeroij
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
!
!     TEST FOR MODULE ERRORS
!
   IF ( Dry/=-2 ) THEN
!
!     TEST FOR FREEBODY MODES REQUEST
!
      IF ( Rgrid(1)/=-1 ) THEN
!
!     READ BGSS DATA
!
         it = 1
         CALL sfetch(Oldnam,nhbgss,1,itest)
         IF ( itest==3 ) GOTO 400
         IF ( itest==4 ) THEN
            imsg = -2
            GOTO 500
         ELSEIF ( itest==5 ) THEN
            imsg = -3
            GOTO 500
         ELSE
            CALL suread(Z(Korbgn),-1,nwdsrd,itest)
!
!     EXTRACT SUBSTRUCTURE IP DATA
!
            nbgss = Z(Korbgn+2)
            locbgs = Korbgn
            CALL suread(Z(Korbgn),-2,nwdsrd,itest)
            Korbgn = Korbgn + nwdsrd
!
!     READ CSTM DATA
!
            locstm = Korbgn
            IF ( Korlen<=locstm ) THEN
!
!     PROCESS SYSTEM FATAL ERRORS
!
               imsg = -8
               ifile = 0
               GOTO 300
            ELSE
               it = 2
               CALL sfetch(Oldnam,nhcstm,1,itest)
               IF ( itest/=3 ) THEN
                  IF ( itest==4 ) THEN
                     imsg = -2
                     GOTO 500
                  ELSEIF ( itest==5 ) THEN
                     imsg = -3
                     GOTO 500
                  ELSE
                     CALL suread(Z(locstm),-2,nwdsrd,itest)
                     CALL pretrs(Z(locstm+3),nwdsrd-4)
                  ENDIF
               ENDIF
!
!     CHECK FOR BASIC COORDINATES
!
               DO i = 1 , 3
                  Rgrid0(i) = 0.0
               ENDDO
               IF ( Rgrid(1)/=0 ) THEN
!
!     EXTRACT FREEBODY BASIC COORDINATES
!
                  locrgr = locbgs + (4*(Rgrid(1)-1))
                  DO i = 1 , 3
                     Rgrid0(i) = Rz(locrgr+i)
                  ENDDO
               ENDIF
!
!     OPEN SCRATCH FILE
!
               ifile = scr1
               itrlr(1) = ifile
               CALL open(*200,scr1,Z(Gbuf2),1)
               Typin = 1
               Typpck = 1
               Irowp = 1
               Lrowp = 6
               Incrp = 1
!
!     OPEN EQSS FILE AND CHECK OPEN CORE LENGTH
!
               it = 3
               CALL sfetch(Oldnam,nheqss,1,itest)
               IF ( itest==3 ) GOTO 400
               IF ( itest==4 ) THEN
                  imsg = -2
                  GOTO 500
               ELSEIF ( itest==5 ) THEN
                  imsg = -3
                  GOTO 500
               ELSE
                  locsil = locstm + nwdsrd
                  CALL suread(Z(locsil),-1,nwdsrd,itest)
                  IF ( Korlen<=locsil ) GOTO 400
!
!     READ UP TO SIL DATA
!
                  IF ( Korlen<=2*Nsil ) GOTO 400
                  DO i = 1 , Ncsubs
                     CALL suread(Z(locsil),-1,nwdsrd,itest)
                     IF ( Korlen<=locsil+nwdsrd ) GOTO 400
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
                     smald(2) = Rz(locbgs+ii+3) - Rgrid0(3)
                     smald(3) = -Rz(locbgs+ii+2) + Rgrid0(2)
                     smald(4) = -smald(2)
                     smald(5) = 0.0
                     smald(6) = Rz(locbgs+ii+1) - Rgrid0(1)
                     smald(7) = -smald(3)
                     smald(8) = -smald(6)
                     smald(9) = 0.0
!
!     SELECT TI, TTD MATRIX GENERATION
!
                     IF ( Z(locbgs+ii)<0 ) THEN
!
!     SCALAR POINT ADDS NULL COLUMN TO BIGD
!     (CID .LT. 0)
!
                        DO j = 1 , 6
                           bigd(j) = 0.0
                        ENDDO
                        Irowp = 1
                        CALL pack(bigd(1),scr1,itrlr)
                        CYCLE
                     ELSEIF ( Z(locbgs+ii)==0 ) THEN
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
                        CALL transs(Z(locbgs+ii),ti)
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
                     CALL suread(Z(locsil),2,nwdsrd,itest)
                     icode = Z(locsil+1)
                     CALL decode(icode,kompnt,nwdsd)
                     DO j = 1 , nwdsd
                        irowd = 1 + 6*kompnt(j)
                        CALL pack(bigd(irowd),scr1,itrlr)
                     ENDDO
                  ENDDO
                  CALL close(scr1,1)
                  itrlr(3) = Lrowp
!
!     READ SCR1 INTO TRANSPOSED FORM
!
                  CALL open(*200,scr1,Z(Gbuf1),0)
                  Typunp = 1
                  Irowup = 1
                  Lrowup = 6
                  Incrup = itrlr(2)
                  kolmns = itrlr(2)
                  Korbgn = locbgs
                  IF ( Korlen<=Korbgn+Lrowp*kolmns ) GOTO 400
                  DO i = 1 , kolmns
                     CALL unpack(*2,scr1,Z(Korbgn))
                     GOTO 4
 2                   j = Korbgn
                     DO k = 1 , 6
                        Rz(j) = 0.0
                        j = j + Incrup
                     ENDDO
 4                   Korbgn = Korbgn + 1
                  ENDDO
                  CALL close(scr1,1)
!
!     PLACE TRANSPOSED BIGD ONTO DMR OUTPUT FILE
!
                  ifile = dmr
                  CALL open(*200,dmr,Z(Gbuf2),1)
                  CALL fname(dmr,dmrnam)
                  CALL write(dmr,dmrnam,2,1)
                  locdmr = locbgs
                  Lrowp = kolmns
                  iform = 2
                  CALL makmcb(itrlr,dmr,Lrowp,iform,Typin)
                  DO i = 1 , 6
                     CALL pack(Z(locdmr),dmr,itrlr)
                     locdmr = locdmr + kolmns
                  ENDDO
                  CALL close(dmr,1)
                  CALL wrttrl(itrlr)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
 100  RETURN
 200  imsg = -1
 300  CALL sofcls
   CALL mesage(imsg,ifile,modnam)
   GOTO 100
!
!     PROCESS MODULE FATAL ERRORS
!
 400  imsg = -1
 500  IF ( it<2 ) THEN
      CALL smsg(imsg,nhbgss,Oldnam)
   ELSEIF ( it==2 ) THEN
!
      CALL smsg(imsg,nhcstm,Oldnam)
   ELSE
!
      CALL smsg(imsg,nheqss,Oldnam)
   ENDIF
   GOTO 100
!
END SUBROUTINE mred1e