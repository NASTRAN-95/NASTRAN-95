!*==cmrd2e.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmrd2e(Iter)
USE C_BLANK
USE C_PACKX
USE C_SYSTEM
USE C_UNPAKX
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iter
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: dblkor , dicore , gib , gibbar , gibrl , gibtyp , hgh , hghbar , hghrl , him , himbar , himrl , himtyp , i , icore ,  &
            & ifile , iform , imsg , iprc , item , itest , ityp , itype , j , k , kk , kols1 , kols2 , kore , l , ll , luprt ,      &
            & nrows , nrows1 , nrows2 , sglkor , uprt
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(4) , SAVE :: itmlst
   INTEGER , DIMENSION(7) :: itrlr1 , itrlr2 , itrlr3
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL , DIMENSION(1) :: rz
   EXTERNAL close , gopen , makmcb , mesage , mtrxi , mtrxo , pack , rdtrl , smsg , sofcls , softrl , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     THIS SUBROUTINE CALCULATES THE H TRANSFORMATION MATRIX FOR THE
!     CMRED2 MODULE.
!
!     INPUT  DATA
!     GINO - HIM     - MODAL TRANSFORMATION MATRIX
!     SOF  - GIMS    - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS OF
!                      ORIGINAL SUBSTRUCTURE
!
!     OUTPUT  DATA
!     GINO  - HGH    - HORG PARTITION MATRIX
!     SOF   - HORG   - H TRANSFORMATION MATRIX FOR ORIGINAL SUBSTRUCTURE
!
!     PARAMETERS
!     INPUT - GBUF   - GINO BUFFERS
!             INFILE - INPUT FILE NUMBERS
!             OTFILE - OUTPUT FILE NUMBERS
!             ISCR   - SCRATCH FILE NUMBERS
!             KORLEN - LENGTH OF OPEN CORE
!             KORBGN - BEGINNING ADDRESS OF OPEN CORE
!             OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
!     OTHERS- HIM    - HIM PARTITION MATRIX FILE NUMBER (RIGHT SIDE)
!             HGH    - HORG MATRIX FILE NUMBER (RIGHT SIDE)
!             GIB    - GIMS INPUT FILE NUMBER (RIGHT SIDE)
!             HIMBAR - HIM PARTITION MATRIX FILE NUMBER (LEFT SIDE)
!             HGHBAR - HGH PARTITION MATRIX FILE NUMBER (LEFT SIDE)
!             GIBBAR - GIB PARTITION MATRIX FILE NUMBER (LEFT SIDE)
!             UPRT   - USET PARTITIONING VECTOR FILE NUMBER
!
   !>>>>EQUIVALENCE (Him,Iscr(10)) , (Gib,Iscr(6)) , (Uprt,Iscr(7)) , (Gibbar,Iscr(11)) , (Hghbar,Iscr(9)) , (Hgh,Iscr(9)) ,             &
!>>>>    & (Himbar,Iscr(8)) , (Rz(1),Z(1)) , (Dz(1),Z(1))
   DATA modnam/4HCMRD , 4H2E  /
   DATA itmlst/4HHORG , 4HHLFT , 4HGIMS , 4HUPRT/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     SET UP ROW PARTITION
!
         IF ( Dry==-2 ) RETURN
         item = itmlst(4)
         CALL mtrxi(uprt,Oldnam,item,0,itest)
         IF ( itest/=1 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL softrl(Oldnam,item,itrlr1)
         IF ( Korbgn+itrlr1(3)>=Korlen ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Typeu = itrlr1(5)
         Irowu = 1
         Nrowu = itrlr1(3)
         Incru = 1
         CALL gopen(uprt,Z(Gbuf1),0)
         CALL unpack(*20,uprt,rz(Korbgn))
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      DO i = 1 , Nrowu
            rz(Korbgn+i-1) = 0.0
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
         CALL close(uprt,1)
         luprt = Nrowu
         kore = Korbgn
         Korbgn = Korbgn + luprt
!
!     GET GIB MATRIX
!
         IF ( Iter==2 ) THEN
            itrlr1(1) = gibbar
            CALL rdtrl(itrlr1)
            gibrl = gibbar
         ELSE
            item = itmlst(3)
            CALL softrl(Oldnam,item,itrlr1)
            IF ( itest/=1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL mtrxi(gib,Oldnam,item,0,itest)
            IF ( itest/=1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            itrlr1(1) = gib
            gibrl = gib
         ENDIF
!
!     SET UP HGH TRAILER
!
         hghrl = hgh
         IF ( Iter==2 ) hghrl = hghbar
         nrows1 = itrlr1(3)
         kols1 = itrlr1(2)
         gibtyp = itrlr1(5)
         himrl = him
         IF ( Iter==2 ) himrl = himbar
         itrlr2(1) = himrl
         CALL rdtrl(itrlr2)
         nrows2 = itrlr2(3)
         kols2 = itrlr2(2)
         himtyp = itrlr2(5)
         iform = 2
         IF ( itrlr1(2)+itrlr1(3)==itrlr2(2)+itrlr2(3) ) iform = 1
         iprc = 1
         ityp = 0
         IF ( itrlr1(5)==2 .OR. itrlr1(5)==4 ) iprc = 2
         IF ( itrlr2(5)==2 .OR. itrlr2(5)==4 ) iprc = 2
         IF ( itrlr1(5)>=3 ) ityp = 2
         IF ( itrlr2(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(itrlr3,hghrl,luprt,iform,itype)
!
!     SET UP PACK/UNPACK PARAMETERS
!
         Typeop = itrlr3(5)
         Irowp = 1
         Nrowp = itrlr1(2) + itrlr1(3)
         Incrp = 1
         Incru = 1
         dblkor = Korbgn/2 + 1
         sglkor = 2*dblkor - 1
!
!     FORM HGH MATRIX
!
!                  **         **
!                  *     .     *
!        **   **   *  I  .  0  *
!        *     *   *     .     *
!        * HGH * = *...........*
!        *     *   *     .     *
!        **   **   * GIB . HIM *
!                  *     .     *
!                  **         **
!
         CALL gopen(hghrl,Z(Gbuf1),1)
!
!     PROCESS GIB MATRIX
!
         Typeu = itrlr1(5)
         Nrowu = itrlr1(3)
         Typinp = itrlr1(5)
         nrows = itrlr1(3)
         IF ( itrlr1(5)>2 ) nrows = 2*itrlr1(3)
         IF ( itrlr1(5)==1 .OR. itrlr1(5)==3 ) dicore = (sglkor+nrows)/2 + 1
         IF ( itrlr1(5)==2 .OR. itrlr1(5)==4 ) dicore = dblkor + nrows
         icore = 2*dicore - 1
         IF ( dicore+nrows>=Korlen ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(gibrl,Z(Gbuf2),0)
         DO i = 1 , kols1
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  k = 0
                  kk = 0
                  CALL unpack(*22,gibrl,dz(dblkor))
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
!
!     NULL GIB COLUMN
!
 22               IF ( gibtyp==2 .OR. gibtyp==4 ) THEN
                     DO j = 1 , nrows
                        dz(dblkor+j-1) = 0.0D0
                     ENDDO
                  ELSE
                     DO j = 1 , nrows
                        rz(sglkor+j-1) = 0.0
                     ENDDO
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
!
!     MOVE GIB DATA
!
                  DO j = 1 , luprt
                     IF ( rz(kore+j-1)==1.0 ) THEN
!
!     MOVE IDENTITY MATRIX DATA
!
                        k = k + 1
                        l = 1 + 2*(j-1)
                        IF ( gibtyp==2 ) THEN
                           dz(dicore+j-1) = 0.0D0
                           IF ( k==i ) dz(dicore+j-1) = 1.0D0
                        ELSEIF ( gibtyp==3 ) THEN
                           rz(icore+l-1) = 0.0
                           IF ( k==i ) rz(icore+l-1) = 1.0
                           rz(icore+l) = 0.0
                        ELSEIF ( gibtyp==4 ) THEN
                           dz(dicore+l-1) = 0.0D0
                           IF ( k==i ) dz(dicore+l-1) = 1.0D0
                           dz(dicore+l) = 0.0D0
                        ELSE
                           rz(icore+j-1) = 0.0
                           IF ( k==i ) rz(icore+j-1) = 1.0
                        ENDIF
                     ELSE
                        kk = kk + 1
                        l = 1 + 2*(kk-1)
                        ll = 1 + 2*(j-1)
                        IF ( gibtyp==2 ) THEN
                           dz(dicore+j-1) = dz(dblkor+kk-1)
                        ELSEIF ( gibtyp==3 ) THEN
                           rz(icore+ll-1) = rz(sglkor+l-1)
                           rz(icore+ll) = rz(sglkor+l)
                        ELSEIF ( gibtyp==4 ) THEN
                           dz(dicore+ll-1) = dz(dblkor+l-1)
                           dz(dicore+ll) = dz(dblkor+l)
                        ELSE
                           rz(icore+j-1) = rz(sglkor+kk-1)
                        ENDIF
                     ENDIF
                  ENDDO
                  CALL pack(dz(dicore),hghrl,itrlr3)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         CALL close(gibrl,1)
!
!     PROCESS HIM MATRIX
!
         Typeu = itrlr2(5)
         Nrowu = itrlr2(3)
         Typinp = itrlr2(5)
         nrows = itrlr2(3)
         IF ( itrlr2(5)>2 ) nrows = 2*itrlr2(3)
         IF ( itrlr2(5)==2 .OR. itrlr2(5)==4 ) dicore = (sglkor+nrows)/2 + 1
         IF ( itrlr2(5)==1 .OR. itrlr2(5)==3 ) dicore = dblkor + nrows
         icore = 2*dicore - 1
         IF ( dicore+nrows>=Korlen ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(himrl,Z(Gbuf2),0)
         DO i = 1 , kols2
            spag_nextblock_3 = 1
            SPAG_DispatchLoop_3: DO
               SELECT CASE (spag_nextblock_3)
               CASE (1)
                  kk = 0
                  CALL unpack(*24,himrl,dz(dblkor))
                  spag_nextblock_3 = 2
                  CYCLE SPAG_DispatchLoop_3
!
!     NULL HIM COLUMN
!
 24               IF ( himtyp==2 .OR. himtyp==4 ) THEN
                     DO j = 1 , nrows
                        dz(dblkor+j-1) = 0.0D0
                     ENDDO
                  ELSE
                     DO j = 1 , nrows
                        rz(sglkor+j-1) = 0.0
                     ENDDO
                  ENDIF
                  spag_nextblock_3 = 2
               CASE (2)
!
!     MOVE HIM MATRIX DATA
!
                  DO j = 1 , luprt
                     IF ( rz(kore+j-1)==1.0 ) THEN
!
!     MOVE ZERO MATRIX DATA
!
                        l = 1 + 2*(j-1)
                        IF ( himtyp==2 ) THEN
                           dz(dicore+j-1) = 0.0D0
                        ELSEIF ( himtyp==3 ) THEN
                           rz(icore+l-1) = 0.0
                           rz(icore+l) = 0.0
                        ELSEIF ( himtyp==4 ) THEN
                           dz(dicore+l-1) = 0.0D0
                           dz(dicore+l) = 0.0D0
                        ELSE
                           rz(icore+j-1) = 0.0
                        ENDIF
                     ELSE
                        kk = kk + 1
                        l = 1 + 2*(kk-1)
                        ll = 1 + 2*(j-1)
                        IF ( himtyp==2 ) THEN
                           dz(dicore+j-1) = dz(dblkor+kk-1)
                        ELSEIF ( himtyp==3 ) THEN
                           rz(icore+ll-1) = rz(sglkor+l-1)
                           rz(icore+ll) = rz(sglkor+l)
                        ELSEIF ( himtyp==4 ) THEN
                           dz(dicore+ll-1) = dz(dblkor+l-1)
                           dz(dicore+ll) = dz(dblkor+l)
                        ELSE
                           rz(icore+j-1) = rz(sglkor+kk-1)
                        ENDIF
                     ENDIF
                  ENDDO
                  CALL pack(dz(dicore),hghrl,itrlr3)
                  EXIT SPAG_DispatchLoop_3
               END SELECT
            ENDDO SPAG_DispatchLoop_3
         ENDDO
         CALL close(himrl,1)
         CALL close(hghrl,1)
         CALL wrttrl(itrlr3)
         Korbgn = kore
!
!     SAVE HGH ON SOF AS H(ORG,LFT) MATRIX
!
         item = itmlst(Iter)
         CALL mtrxo(hghrl,Oldnam,item,0,itest)
         IF ( itest==3 ) RETURN
         spag_nextblock_1 = 3
      CASE (3)
!
!     PROCESS MODULE FATAL ERRORS
!
         IF ( itest==4 ) THEN
!
            imsg = -2
         ELSEIF ( itest==5 ) THEN
            imsg = -3
         ELSEIF ( itest==6 ) THEN
!
            WRITE (Iprntr,99001) Ufm , modnam , item , Oldnam
99001       FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',' OF SOF ITEM ',A4,', SUBSTRUCTURE ',2A4,             &
                   &', IS PURGED.')
            Dry = -2
            RETURN
         ELSE
            WRITE (Iprntr,99002) Ufm , modnam , item , Oldnam
!
99002       FORMAT (A23,' 6211, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
            Dry = -2
            RETURN
         ENDIF
         CALL smsg(imsg,item,Oldnam)
         RETURN
      CASE (4)
!
!     PROCESS SYSTEM FATAL ERRORS
!
         imsg = -8
         ifile = 0
         CALL sofcls
         CALL mesage(imsg,ifile,modnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE cmrd2e
