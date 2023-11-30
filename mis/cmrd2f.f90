
SUBROUTINE cmrd2f(Kode)
   IMPLICIT NONE
   INTEGER Dry , Eqst , Fuset , Gbuf1 , Gib , Gibbar , Hgh , Hghbar , Him , Himbar , Icode , Idum1 , Idum2(2) , Idum3 , Idum4(9) ,  &
         & Idum5(10) , Idum6(4) , Idum7(4) , Incr , Infile(11) , Iprntr , Irow , Iscr(11) , Itrlra(7) , Itrlrb(7) , Itrlrc(7) ,     &
         & Itrlrd(7) , Jscr(3) , Jtrlra(7) , Jtrlrb(7) , Jtrlrc(7) , Jtrlre(7) , Kbarbb , Kbb , Khh , Kib , Kii , Kmm , Korbgn ,    &
         & Korlen , Lcore , Lkore , Lstzwd , Newnam(2) , Nrow , Nsub(3) , Nz , Oldnam(2) , Otfile(6) , Paa , Popt , Pove , Prec ,   &
         & Prec3 , Sbuf1 , Sbuf2 , Sbuf3 , Scr , Signab , Signc , T , Typin , Typout , Ub , Ui , Un , Uprt , Usetmr , Z(1)
   DOUBLE PRECISION Dz(1)
   LOGICAL Modes , Ponly , Symtry
   REAL Rprtn , Rz(1)
   CHARACTER*23 Ufm
   COMMON /bitpos/ Idum4 , Un , Idum5 , Ub , Ui
   COMMON /blank / Idum1 , Dry , Popt , Gbuf1 , Idum2 , Sbuf1 , Sbuf2 , Sbuf3 , Infile , Otfile , Iscr , Korlen , Korbgn , Oldnam , &
                 & Newnam , Symtry , Idum6 , Modes , Idum7 , Ponly , Lstzwd
   COMMON /mpy3tl/ Jtrlra , Jtrlrb , Jtrlre , Jtrlrc , Jscr , Lkore , Icode , Prec3
   COMMON /mpyadx/ Itrlra , Itrlrb , Itrlrc , Itrlrd , Nz , T , Signab , Signc , Prec , Scr
   COMMON /packx / Typin , Typout , Irow , Nrow , Incr
   COMMON /patx  / Lcore , Nsub , Fuset
   COMMON /system/ Idum3 , Iprntr
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER Kode
   INTEGER blanks , cmred2 , dblkor , i , iform , imsg , iprc , isub(4) , item , itest , itmlst(12) , itmnam(2) , itrlr1(7) ,       &
         & itrlr2(7) , itrlr3(7) , ityp , itype , j , kbarow , kcol , kmmcol , kmmrow , modnam(2) , numb , papp
!
!     THIS SUBROUTINE CALCULATES THE FINAL STRUCTURAL MATRICES FOR THE
!     CMRED2 MODULE.
!
!     INPUT  DATA
!     GINO - KBB    - STIFFNESS PARTITION MATRIX
!            KIB    - KIB STIFFNESS PATTITION MATRIX
!            HIE    - HIE PARTITION MATRIX
!            KII    - KII PARTITION MATRIX
!            HGH    - HORG PARTITION MATRIX
!            MAA    - MASS INPUT MATRIX
!            BAA    - DAMPING INPUT MATRIX
!            K4AA   - STIFFNESS INPUT MATRIX
!            PAA    - LOADS INPUT MATRIX
!     SOF  - GIMS   - G TRANSFORMATION MATRIX
!
!     OUTPUT DATA
!     GINO - KHH    - STIFFNESS MATRIX
!            MHH    - MASS MATRIX
!            BHH    - DAMPING MATRIX
!            K4HH   - K4HH MATRIX
!            PHH    - LOADS MATRIX
!     SOF  - KMTX   - STIFFNESS MATRIX
!            MMTX   - MASS MATRIX
!            PVEC   - LOADS MATRIX
!            PAPP   - APPENDED LOADS MATRIX
!            BMTX   - DAMPING MATRIX
!            K4MX   - K4MX STIFFNESS MATRIX
!
!     PARAMETERS
!     INPUT- POPT   - LOADS OPTION FLAG
!            GBUF   - GINO BUFFERS
!            INFILE - INPUT FILE NUMBERS
!            OTFILE - OUTPUT FILE NUMBERS
!            ISCR   - SCRATCH FILE NUMBERS
!            KORLEN - LENGTH OF OPEN CORE
!            KORBGN - BEGINNING ADDRESS OF OPEN CORE
!            OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
!     OTHERS-PAA    - LOADS INPUT FILE NUMBER
!            KHH    - STIFFNESS OUTPUT FILE NUMBER
!            POVE   - LOADS OUTPUT FILE NUMBER
!            UPRT   - PARTITION VECTOR FILE NUMBER
!            ZEROMB - ZERO PARTITION FILE NUMBER
!            KBB    - KBB INPUT FILE NUMBER
!            ZEROBM - ZERO PARTITION MATRIX
!            KIB    - KIB INPUT FILE NUMBER
!            KII    - KII INPUT FILE NUMBER
!            KBARBB - KBARBB FILE NU BER
!            GIB    - GIB INPUT FILE NUMBER
!            KMM    - KMM FILE NUMBER
!            HGH    - HORG INPUT FILE NUMBER
!
   EQUIVALENCE (Eqst,Infile(5)) , (Usetmr,Infile(6)) , (Paa,Infile(11)) , (Khh,Otfile(1)) , (Pove,Otfile(6)) , (Kbb,Iscr(1)) ,      &
    & (Kib,Iscr(2)) , (Kii,Iscr(4)) , (Him,Iscr(10)) , (Uprt,Iscr(1)) , (Himbar,Iscr(8)) , (Kbarbb,Iscr(5)) , (Kmm,Iscr(6)) ,       &
    & (Gib,Iscr(3)) , (Gibbar,Iscr(11)) , (Hghbar,Iscr(9)) , (Hgh,Iscr(8)) , (Rprtn,Iscr(1)) , (Rz(1),Z(1)) , (Dz(1),Z(1))
   DATA modnam/4HCMRD , 4H2F  / , papp/4HPAPP/ , blanks/4H    /
   DATA cmred2/26/
   DATA itmlst/4HKMTX , 4HHORG , 4HHLFT , 4HMMTX , 4HBMTX , 4HK4MX , 4HPVEC , 4HPAPP , 4HPOVE , 4HGIMS , 4HPOAP , 4HUPRT/
!
!     SELECT OPERATION MODE
!
   IF ( Dry==-2 ) RETURN
   IF ( .NOT.(Ponly .OR. Dry==0) ) THEN
!
!     SET UP NEW SUBSTRUCTURE
!
      IF ( .NOT.(Modes) ) THEN
         numb = 1
         CALL setlvl(Newnam,numb,Oldnam,itest,cmred2)
         IF ( itest==8 ) THEN
!
            WRITE (Iprntr,99001) Ufm
99001       FORMAT (A23,' 6518, ONE OF THE COMPONENT SUBSTRUCTURES HAS BEEN ','USED IN A PREVIOUS COMBINE OR REDUCE.')
            Dry = -2
            RETURN
         ENDIF
      ENDIF
!
!     CHECK FOR STIFFNESS MATRIX GENERATION
!
      itrlr1(1) = Khh
      CALL rdtrl(itrlr1)
      IF ( itrlr1(1)>=0 ) THEN
!
!     FORM PRELIMINARY STIFFNESS CALCULATION
!
!                                           T
!        **      **   **   **   **        ** **   **
!        *        *   *     *   *          * *     *
!        * KBARBB * = * KBB * + * GIB(BAR) * * KIB *
!        *        *   *     *   *          * *     *
!        **      **   **   **   **        ** **   **
!
         itrlr1(1) = Kbb
         CALL rdtrl(itrlr1)
         IF ( Symtry ) THEN
            item = itmlst(10)
            CALL softrl(Oldnam,item,itrlr2)
            itest = itrlr2(1)
            itmnam(1) = Oldnam(1)
            itmnam(2) = Oldnam(2)
            IF ( itest/=1 ) GOTO 100
            CALL mtrxi(Gib,Oldnam,item,0,itest)
            IF ( itest/=1 ) GOTO 100
            itrlr2(1) = Gib
         ELSE
            itrlr2(1) = Gibbar
            CALL rdtrl(itrlr2)
         ENDIF
         itrlr3(1) = Kib
         CALL rdtrl(itrlr3)
         DO i = 1 , 7
            Itrlra(i) = itrlr2(i)
            Itrlrb(i) = itrlr3(i)
            Itrlrc(i) = itrlr1(i)
         ENDDO
         iform = 1
         iprc = 1
         ityp = 0
         IF ( Itrlra(5)==2 .OR. Itrlra(5)==4 ) iprc = 2
         IF ( Itrlrb(5)==2 .OR. Itrlrb(5)==4 ) iprc = 2
         IF ( Itrlrc(5)==2 .OR. Itrlrc(5)==4 ) iprc = 2
         IF ( Itrlra(5)>=3 ) ityp = 2
         IF ( Itrlrb(5)>=3 ) ityp = 2
         IF ( Itrlrc(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(Itrlrd,Kbarbb,itrlr1(3),iform,itype)
         T = 1
         Signab = 1
         Signc = 1
         Prec = 0
         Scr = Iscr(7)
         Scr = Iscr(1)
         CALL sofcls
         dblkor = Korbgn/2 + 1
         Nz = Lstzwd - (2*dblkor-1)
         CALL mpyad(Dz(dblkor),Dz(dblkor),Dz(dblkor))
         CALL wrttrl(Itrlrd)
         kbarow = Itrlrd(3)
         kcol = Itrlrd(2)
!
!     FORM PRELIMINARY STIFFNESS CALCULATION
!
!                              T
!        **   **   **        ** **   ** **   **
!        *     *   *          * *     * *     *
!        * KMM * = * HIM(BAR) * * KII * * HIM *
!        *     *   *          * *     * *     *
!        **   **   **        ** **   ** **   **
!
         itrlr1(1) = Kii
         itrlr2(1) = Him
         CALL rdtrl(itrlr1)
         CALL rdtrl(itrlr2)
         DO i = 1 , 7
            Itrlra(i) = itrlr1(i)
            Itrlrb(i) = itrlr2(i)
            Itrlrc(i) = 0
         ENDDO
         iform = 2
         iprc = 1
         ityp = 0
         IF ( Itrlra(5)==2 .OR. Itrlra(5)==4 ) iprc = 2
         IF ( Itrlrb(5)==2 .OR. Itrlrb(5)==4 ) iprc = 2
         IF ( Itrlra(5)>=3 ) ityp = 2
         IF ( Itrlrb(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(Itrlrd,Iscr(2),itrlr2(3),iform,itype)
         Prec = 0
         T = 0
         Signab = 1
         Signc = 1
         Scr = Iscr(1)
         CALL mpyad(Dz(dblkor),Dz(dblkor),Dz(dblkor))
         CALL wrttrl(Itrlrd)
         itrlr1(1) = Him
         IF ( .NOT.Symtry ) itrlr1(1) = Himbar
         CALL rdtrl(itrlr1)
         DO i = 1 , 7
            Itrlra(i) = itrlr1(i)
            Itrlrb(i) = Itrlrd(i)
         ENDDO
         iform = 1
         iprc = 1
         ityp = 0
         IF ( Itrlra(5)==2 .OR. Itrlra(5)==4 ) iprc = 2
         IF ( Itrlrb(5)==2 .OR. Itrlrb(5)==4 ) iprc = 2
         IF ( Itrlra(5)>=3 ) ityp = 2
         IF ( Itrlrb(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(Itrlrd,Kmm,itrlr1(2),iform,itype)
         T = 1
         Prec = 0
         CALL mpyad(Dz(dblkor),Dz(dblkor),Dz(dblkor))
         CALL wrttrl(Itrlrd)
         kmmrow = Itrlrd(3)
         kmmcol = Itrlrd(2)
!
!     GENERATE MERGE PARTITION VECTOR
!
         Nrow = kcol + kmmcol
         DO i = 1 , Nrow
            Rz(Korbgn+i-1) = 0.0
            IF ( i>kcol ) Rz(Korbgn+i-1) = 1.0
         ENDDO
         Typin = 1
         Typout = 1
         Irow = 1
         Incr = 1
         iform = 7
         CALL makmcb(itrlr1,Rprtn,Nrow,iform,Typin)
         CALL gopen(Rprtn,Z(Gbuf1),1)
         CALL pack(Rz(Korbgn),Rprtn,itrlr1)
         CALL close(Rprtn,1)
         CALL wrttrl(itrlr1)
!
!     FORM STIFFNESS MATRIX
!
!                  **            **
!                  *        .     *
!        **   **   * KBARBB .  0  *
!        *     *   *        .     *
!        * KHH * = *..............*
!        *     *   *        .     *
!        **   **   *   0    . KMM *
!                  *        .     *
!                  **            **
!
         isub(1) = kcol
         isub(2) = kmmcol
         isub(3) = kbarow
         isub(4) = kmmrow
         itype = 1
         CALL gmmerg(Khh,Kbarbb,0,0,Kmm,Rprtn,Rprtn,isub,itype,Z(Korbgn),Korlen)
!
!     STORE KHH AS KMTX ON SOF
!
         CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
         CALL mtrxo(Khh,Newnam,itmlst(1),0,itest)
         item = itmlst(1)
         itmnam(1) = Newnam(1)
         itmnam(2) = Newnam(2)
         IF ( itest/=3 ) GOTO 100
      ENDIF
   ENDIF
!
!     LOCATE HGH MATRIX
!       KODE .EQ. 0, BOTH HORG, HLFT ON SOF
!       KODE .EQ. 1, HORG CALCULATED, HLFT ON SOF
!       KODE .EQ. 2, HORG ON SOF, HLFT CALCULATED
!       KODE .EQ. 3, BOTH HORG, HLFT CALCULATED
!
   item = itmlst(2)
   itmnam(1) = Oldnam(1)
   itmnam(2) = Oldnam(2)
   CALL mtrxi(Hgh,Oldnam,item,0,itest)
   IF ( itest==1 ) THEN
      IF ( .NOT.(Kode>1 .OR. Symtry) ) THEN
         item = itmlst(3)
         CALL mtrxi(Hghbar,Oldnam,item,0,itest)
         IF ( itest/=1 ) GOTO 100
      ENDIF
      Signab = 1
      Signc = 1
      Scr = Iscr(1)
      dblkor = Korbgn/2 + 1
      Nz = Lstzwd - (2*dblkor-1)
      itmnam(1) = Newnam(1)
      itmnam(2) = Newnam(2)
!
!     GENERATE MATRICES REQUESTED
!        I .EQ. 2, GENERATE MHH MATRIX
!        I .EQ. 3, GENERATE BHH MATRIX
!        I .EQ. 4, GENERATE K4HH MATRIX
!        I .EQ. 5, GENERATE PHH MATRIX
!
      DO i = 2 , 5
         itrlr1(1) = Infile(i+6)
         CALL rdtrl(itrlr1)
         IF ( itrlr1(1)>=0 ) THEN
            CALL sofcls
!
!     CALCULATE MATRIX REQUIRED
!
!                                     T
!        **          **   **        ** **          ** **   **
!        *            *   *          * *            * *     *
!        * (M,B,K4)HH * = * HGH(BAR) * * (M,B,K4)AA * * HGH *
!        *            *   *          * *            * *     *
!        **          **   **        ** **          ** **   **
!
!                              T
!        **   **   **        ** **   **
!        *     *   *          * *     *
!        * PHH * = * HGH(BAR) * * PAA *
!        *     *   *          * *     *
!        **   **   **        ** **   **
!
            itrlr2(1) = Hgh
            CALL rdtrl(itrlr2)
            IF ( i==5 ) THEN
               DO j = 1 , 7
                  Itrlrd(j) = itrlr1(j)
               ENDDO
               item = itmlst(7)
               IF ( Popt==papp ) item = itmlst(8)
            ELSE
               DO j = 1 , 7
                  Itrlra(j) = itrlr1(j)
                  Itrlrb(j) = itrlr2(j)
                  Itrlrc(j) = 0
               ENDDO
               iform = 2
               IF ( itrlr1(3)==itrlr2(2) ) iform = 1
               iprc = 1
               ityp = 0
               IF ( itrlr1(5)==2 .OR. itrlr1(5)==4 ) iprc = 2
               IF ( itrlr2(5)==2 .OR. itrlr2(5)==4 ) iprc = 2
               IF ( itrlr1(5)>=3 ) ityp = 2
               IF ( itrlr2(5)>=3 ) ityp = 2
               itype = iprc + ityp
               CALL makmcb(Itrlrd,Iscr(2),itrlr1(3),iform,itype)
               Prec = 0
               T = 0
               Signab = 1
               Signc = 1
               Scr = Iscr(1)
               CALL mpyad(Dz(dblkor),Dz(dblkor),Dz(dblkor))
               CALL wrttrl(Itrlrd)
               item = itmlst(i+2)
            ENDIF
            itrlr2(1) = Hgh
            IF ( .NOT.Symtry ) itrlr2(1) = Hghbar
            CALL rdtrl(itrlr2)
            DO j = 1 , 7
               Itrlra(j) = itrlr2(j)
               Itrlrb(j) = Itrlrd(j)
            ENDDO
            iform = 1
            iprc = 1
            ityp = 0
            IF ( Itrlrd(5)==2 .OR. Itrlrd(5)==4 ) iprc = 2
            IF ( itrlr2(5)==2 .OR. itrlr2(5)==4 ) iprc = 2
            IF ( Itrlrd(5)>=3 ) ityp = 2
            IF ( itrlr2(5)>=3 ) ityp = 2
            itype = iprc + ityp
            CALL makmcb(Itrlrd,Otfile(i),itrlr2(2),iform,itype)
            T = 1
            Prec = 0
            CALL mpyad(Dz(dblkor),Dz(dblkor),Dz(dblkor))
            CALL wrttrl(Itrlrd)
!
!     STORE MATRIX ON SOF
!        I .EQ. 2, STORE MHH AS MMTX
!        I .EQ. 3, STORE BHH AS BMTX
!        I .EQ. 4, STORE K4HH AS K4MX
!        I .EQ. 5, STORE PHH AS PVEC OR PAPP
!
            CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
            CALL mtrxo(Otfile(i),Newnam,item,0,itest)
            IF ( itest/=3 ) GOTO 100
         ENDIF
      ENDDO
!
!     TEST FOR LOAD PROCESSING
!
      IF ( Popt/=blanks ) THEN
         itmnam(1) = Oldnam(1)
         itmnam(2) = Oldnam(2)
         IF ( .NOT.Ponly ) THEN
!
!     PARTITION PAA VECTOR
!
            Lcore = Korlen
            Fuset = Usetmr
            CALL calcv(Uprt,Un,Ui,Ub,Z(Korbgn))
         ELSE
            itrlr1(1) = Eqst
            CALL rdtrl(itrlr1)
            Nsub(1) = itrlr1(6)
            Nsub(2) = itrlr1(7)
            item = itmlst(12)
            CALL mtrxi(Uprt,Oldnam,item,0,itest)
            IF ( itest/=1 ) GOTO 100
         ENDIF
         CALL gmprtn(Paa,Pove,0,0,0,0,Uprt,Nsub(1),Nsub(2),Z(Korbgn),Korlen)
!
!     SAVE POVE AS POVE OR POAP ON SOF
!
         IF ( .NOT.(Modes) ) THEN
            item = itmlst(9)
            IF ( Popt==papp ) item = itmlst(11)
            CALL mtrxo(Pove,Oldnam,item,0,itest)
            IF ( itest/=3 ) GOTO 100
         ENDIF
      ENDIF
      RETURN
   ENDIF
!
!     PROCESS MODULE ERRORS
!
 100  IF ( itest==4 ) THEN
!
      imsg = -2
   ELSEIF ( itest==5 ) THEN
      imsg = -3
   ELSEIF ( itest==6 ) THEN
      GOTO 200
   ELSE
      WRITE (Iprntr,99002) Ufm , modnam , item , itmnam
!
99002 FORMAT (A23,' 6211, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
      Dry = -2
      RETURN
   ENDIF
   CALL smsg(imsg,item,itmnam)
   RETURN
!
 200  WRITE (Iprntr,99003) Ufm , modnam , item , itmnam
99003 FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',' OF SOF ITEM ',A4,', SUBSTRUCTURE ',2A4,', IS PURGED.')
   Dry = -2
   RETURN
!
END SUBROUTINE cmrd2f
