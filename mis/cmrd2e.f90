
SUBROUTINE cmrd2e(Iter)
   IMPLICIT NONE
   INTEGER Dry , Gbuf1 , Gbuf2 , Gib , Gibbar , Hgh , Hghbar , Him , Himbar , Idum1 , Idum2(4) , Idum3 , Idum7 , Incrp , Incru ,    &
         & Infile(11) , Iprntr , Irowp , Irowu , Iscr(11) , Korbgn , Korlen , Nrowp , Nrowu , Typeop , Typeu , Typinp , Uprt , Z(1)
   DOUBLE PRECISION Dz(1)
   REAL Oldnam(2) , Otfile(6) , Rz(1)
   CHARACTER*23 Ufm
   COMMON /blank / Idum1 , Dry , Idum7 , Gbuf1 , Gbuf2 , Idum2 , Infile , Otfile , Iscr , Korlen , Korbgn , Oldnam
   COMMON /packx / Typinp , Typeop , Irowp , Nrowp , Incrp
   COMMON /system/ Idum3 , Iprntr
   COMMON /unpakx/ Typeu , Irowu , Nrowu , Incru
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER Iter
   INTEGER dblkor , dicore , gibrl , gibtyp , hghrl , himrl , himtyp , i , icore , ifile , iform , imsg , iprc , item , itest ,     &
         & itmlst(4) , itrlr1(7) , itrlr2(7) , itrlr3(7) , ityp , itype , j , k , kk , kols1 , kols2 , kore , l , ll , luprt ,      &
         & modnam(2) , nrows , nrows1 , nrows2 , sglkor
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
!
!     SET UP ROW PARTITION
!
   IF ( Dry==-2 ) RETURN
   item = itmlst(4)
   CALL mtrxi(Uprt,Oldnam,item,0,itest)
   IF ( itest/=1 ) GOTO 500
   CALL softrl(Oldnam,item,itrlr1)
   IF ( Korbgn+itrlr1(3)>=Korlen ) GOTO 600
   Typeu = itrlr1(5)
   Irowu = 1
   Nrowu = itrlr1(3)
   Incru = 1
   CALL gopen(Uprt,Z(Gbuf1),0)
   CALL unpack(*100,Uprt,Rz(Korbgn))
   GOTO 200
 100  DO i = 1 , Nrowu
      Rz(Korbgn+i-1) = 0.0
   ENDDO
 200  CALL close(Uprt,1)
   luprt = Nrowu
   kore = Korbgn
   Korbgn = Korbgn + luprt
!
!     GET GIB MATRIX
!
   IF ( Iter==2 ) THEN
      itrlr1(1) = Gibbar
      CALL rdtrl(itrlr1)
      gibrl = Gibbar
   ELSE
      item = itmlst(3)
      CALL softrl(Oldnam,item,itrlr1)
      IF ( itest/=1 ) GOTO 500
      CALL mtrxi(Gib,Oldnam,item,0,itest)
      IF ( itest/=1 ) GOTO 500
      itrlr1(1) = Gib
      gibrl = Gib
   ENDIF
!
!     SET UP HGH TRAILER
!
   hghrl = Hgh
   IF ( Iter==2 ) hghrl = Hghbar
   nrows1 = itrlr1(3)
   kols1 = itrlr1(2)
   gibtyp = itrlr1(5)
   himrl = Him
   IF ( Iter==2 ) himrl = Himbar
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
   IF ( dicore+nrows>=Korlen ) GOTO 600
   CALL gopen(gibrl,Z(Gbuf2),0)
   DO i = 1 , kols1
      k = 0
      kk = 0
      CALL unpack(*250,gibrl,Dz(dblkor))
      GOTO 300
!
!     NULL GIB COLUMN
!
 250  IF ( gibtyp==2 .OR. gibtyp==4 ) THEN
         DO j = 1 , nrows
            Dz(dblkor+j-1) = 0.0D0
         ENDDO
      ELSE
         DO j = 1 , nrows
            Rz(sglkor+j-1) = 0.0
         ENDDO
      ENDIF
!
!     MOVE GIB DATA
!
 300  DO j = 1 , luprt
         IF ( Rz(kore+j-1)==1.0 ) THEN
!
!     MOVE IDENTITY MATRIX DATA
!
            k = k + 1
            l = 1 + 2*(j-1)
            IF ( gibtyp==2 ) THEN
               Dz(dicore+j-1) = 0.0D0
               IF ( k==i ) Dz(dicore+j-1) = 1.0D0
            ELSEIF ( gibtyp==3 ) THEN
               Rz(icore+l-1) = 0.0
               IF ( k==i ) Rz(icore+l-1) = 1.0
               Rz(icore+l) = 0.0
            ELSEIF ( gibtyp==4 ) THEN
               Dz(dicore+l-1) = 0.0D0
               IF ( k==i ) Dz(dicore+l-1) = 1.0D0
               Dz(dicore+l) = 0.0D0
            ELSE
               Rz(icore+j-1) = 0.0
               IF ( k==i ) Rz(icore+j-1) = 1.0
            ENDIF
         ELSE
            kk = kk + 1
            l = 1 + 2*(kk-1)
            ll = 1 + 2*(j-1)
            IF ( gibtyp==2 ) THEN
               Dz(dicore+j-1) = Dz(dblkor+kk-1)
            ELSEIF ( gibtyp==3 ) THEN
               Rz(icore+ll-1) = Rz(sglkor+l-1)
               Rz(icore+ll) = Rz(sglkor+l)
            ELSEIF ( gibtyp==4 ) THEN
               Dz(dicore+ll-1) = Dz(dblkor+l-1)
               Dz(dicore+ll) = Dz(dblkor+l)
            ELSE
               Rz(icore+j-1) = Rz(sglkor+kk-1)
            ENDIF
         ENDIF
      ENDDO
      CALL pack(Dz(dicore),hghrl,itrlr3)
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
   IF ( dicore+nrows>=Korlen ) GOTO 600
   CALL gopen(himrl,Z(Gbuf2),0)
   DO i = 1 , kols2
      kk = 0
      CALL unpack(*350,himrl,Dz(dblkor))
      GOTO 400
!
!     NULL HIM COLUMN
!
 350  IF ( himtyp==2 .OR. himtyp==4 ) THEN
         DO j = 1 , nrows
            Dz(dblkor+j-1) = 0.0D0
         ENDDO
      ELSE
         DO j = 1 , nrows
            Rz(sglkor+j-1) = 0.0
         ENDDO
      ENDIF
!
!     MOVE HIM MATRIX DATA
!
 400  DO j = 1 , luprt
         IF ( Rz(kore+j-1)==1.0 ) THEN
!
!     MOVE ZERO MATRIX DATA
!
            l = 1 + 2*(j-1)
            IF ( himtyp==2 ) THEN
               Dz(dicore+j-1) = 0.0D0
            ELSEIF ( himtyp==3 ) THEN
               Rz(icore+l-1) = 0.0
               Rz(icore+l) = 0.0
            ELSEIF ( himtyp==4 ) THEN
               Dz(dicore+l-1) = 0.0D0
               Dz(dicore+l) = 0.0D0
            ELSE
               Rz(icore+j-1) = 0.0
            ENDIF
         ELSE
            kk = kk + 1
            l = 1 + 2*(kk-1)
            ll = 1 + 2*(j-1)
            IF ( himtyp==2 ) THEN
               Dz(dicore+j-1) = Dz(dblkor+kk-1)
            ELSEIF ( himtyp==3 ) THEN
               Rz(icore+ll-1) = Rz(sglkor+l-1)
               Rz(icore+ll) = Rz(sglkor+l)
            ELSEIF ( himtyp==4 ) THEN
               Dz(dicore+ll-1) = Dz(dblkor+l-1)
               Dz(dicore+ll) = Dz(dblkor+l)
            ELSE
               Rz(icore+j-1) = Rz(sglkor+kk-1)
            ENDIF
         ENDIF
      ENDDO
      CALL pack(Dz(dicore),hghrl,itrlr3)
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
!
!     PROCESS MODULE FATAL ERRORS
!
 500  IF ( itest==4 ) THEN
!
      imsg = -2
   ELSEIF ( itest==5 ) THEN
      imsg = -3
   ELSEIF ( itest==6 ) THEN
!
      WRITE (Iprntr,99001) Ufm , modnam , item , Oldnam
99001 FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',' OF SOF ITEM ',A4,', SUBSTRUCTURE ',2A4,', IS PURGED.')
      Dry = -2
      RETURN
   ELSE
      WRITE (Iprntr,99002) Ufm , modnam , item , Oldnam
!
99002 FORMAT (A23,' 6211, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
      Dry = -2
      RETURN
   ENDIF
   CALL smsg(imsg,item,Oldnam)
   RETURN
!
!     PROCESS SYSTEM FATAL ERRORS
!
 600  imsg = -8
   ifile = 0
   CALL sofcls
   CALL mesage(imsg,ifile,modnam)
   RETURN
!
END SUBROUTINE cmrd2e