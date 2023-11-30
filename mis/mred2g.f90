
SUBROUTINE mred2g(Kode)
   IMPLICIT NONE
   LOGICAL Bounds , Frebdy , Modes , Ponly
   INTEGER Cprtn , Dry , Eqst , Fuset , Gbuf1 , Gib , Hgh , Hie , Icode , Idum1 , Idum2(2) , Idum3 , Idum4(9) , Idum5(10) , Idum6(5)&
         & , Idum7(4) , Incr , Infile(12) , Iprntr , Irow , Iscr(10) , Itrlra(7) , Itrlrb(7) , Itrlrc(7) , Itrlrd(7) , Jscr(3) ,    &
         & Jtrlra(7) , Jtrlrb(7) , Jtrlrc(7) , Jtrlre(7) , Kbarbb , Kbb , Kee , Khh , Kib , Kii , Korbgn , Korlen , Lcore , Lkore , &
         & Lstzwd , Newnam(2) , Nrow , Nsub(3) , Nz , Oldnam(2) , Otfile(6) , Paa , Popt , Pove , Prec , Prec3 , Rprtn , Sbuf1 ,    &
         & Sbuf2 , Sbuf3 , Scr , Signab , Signc , T , Typin , Typout , Ub , Ui , Un , Uprt , Usetmr , Z(1) , Zerobe , Zeroeb
   REAL Dumm , Dummy(13) , Rz(1)
   DOUBLE PRECISION Dz(1)
   CHARACTER*23 Ufm
   COMMON /bitpos/ Idum4 , Un , Idum5 , Ub , Ui
   COMMON /blank / Idum1 , Dry , Popt , Gbuf1 , Idum2 , Sbuf1 , Sbuf2 , Sbuf3 , Infile , Otfile , Iscr , Korlen , Korbgn , Oldnam , &
                 & Newnam , Frebdy , Idum6 , Bounds , Modes , Idum7 , Ponly , Lstzwd
   COMMON /mpy3tl/ Jtrlra , Jtrlrb , Jtrlre , Jtrlrc , Jscr , Lkore , Icode , Prec3 , Dummy
   COMMON /mpyadx/ Itrlra , Itrlrb , Itrlrc , Itrlrd , Nz , T , Signab , Signc , Prec , Scr , Dumm
   COMMON /packx / Typin , Typout , Irow , Nrow , Incr
   COMMON /patx  / Lcore , Nsub , Fuset
   COMMON /system/ Idum3 , Iprntr
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER Kode
   INTEGER blanks , dblkor , i , iform , imsg , iprc , isub(4) , item , itest , itmlst(11) , itmnam(2) , itrlr1(7) , itrlr2(7) ,    &
         & itrlr3(7) , ityp , itype , j , kbarow , kcol , keecol , keerow , modnam(2) , mred2 , numb , papp
!
!     THIS SUBROUTINE CALCULATES THE FINAL STRUCTURAL MATRICES FOR THE
!     MRED2 MODULE.
!
!     INPUT DATA
!     GINO -   KBB    - STIFFNESS PARTITION MATRIX
!              KIB    - KIB  STIFFNESS PATTITION MATRIX
!              HIE    - HIE  PARTITION MATRIX
!              KII    - KII  PARTITION MATRIX
!              HGH    - HORG PARTITION MATRIX
!              MAA    - MASS INPUT MATRIX
!              BAA    - DAMPING INPUT MATRIX
!              K4AA   - STIFFNESS INPUT MATRIX
!              PAA    - LOADS INPUT MATRIX
!     SOF  -   GIMS   - G TRANSFORMATION MATRIX
!
!     OUTPUT DATA
!     GINO -   KHH    - STIFFNESS MATRIX
!              MHH    - MASS MATRIX
!              BHH    - DAMPING MATRIX
!              K4HH   - K4HH  MATRIX
!              PHH    - LOADS MATRIX
!     SOF  -   KMTX   - STIFFNESS MATRIX
!              MMTX   - MASS  MATRIX
!              PVEC   - LOADS MATRIX
!              PAPP   - APPENDED LOADS MATRIX
!              BMTX   - DAMPING MATRIX
!              K4MX   - K4MX STIFFNESS MATRIX
!
!     PARAMETERS
!     INPUT  - POPT   - LOADS OPTION FLAG
!              GBUF   - GINO BUFFERS
!              INFILE - INPUT   FILE NUMBERS
!              OTFILE - OUTPUT  FILE NUMBERS
!              ISCR   - SCRATCH FILE NUMBERS
!              KORLEN - LENGTH OF OPEN CORE
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
!     OTHERS - PAA    - LOADS INPUT FILE NUMBER
!              KHH    - STIFFNESS I
!              KHH    - STIFFNESS OUTPUT FILE NUMBER
!              POVE   - LOADS OUTPUT FILE NUMBER
!              UPRT   - PARTITION VECTOR FILE NUMBER
!              ZEROEB - ZERO PARTITION FILE NUMBER
!              KBB    - KBB INPUT FILE NUMBER
!              ZEROBE - ZERO PARTITION MATRIX
!              KIB    - KIB INPUT FILE NUMBER
!              KII    - KII INPUT FILE NUMBER
!              KBARBB - KBARBB FILE NU BER
!              GIB    - GIB INPUT FILE NUMBER
!              KEE    - KEE FILE NUMBER
!              HGH    - HORG INPUT FILE NUMBER
!
   EQUIVALENCE (Eqst,Infile(5)) , (Usetmr,Infile(5)) , (Paa,Infile(10)) , (Khh,Otfile(1)) , (Pove,Otfile(6))
   EQUIVALENCE (Zerobe,Iscr(1)) , (Uprt,Iscr(1)) , (Kib,Iscr(2)) , (Zeroeb,Iscr(3)) , (Kii,Iscr(3)) , (Kbb,Iscr(1)) , (Gib,Iscr(4)) &
    & , (Kbarbb,Iscr(5)) , (Kee,Iscr(6)) , (Hie,Iscr(7)) , (Hgh,Iscr(8)) , (Rprtn,Iscr(2)) , (Cprtn,Iscr(4)) , (Rz(1),Z(1)) ,       &
    & (Dz(1),Z(1))
   DATA modnam/4HMRED , 4H2G  / , papp , blanks/4HPAPP , 4H    /
   DATA itmlst/4HKMTX , 4HMMTX , 4HBMTX , 4HK4MX , 4HPVEC , 4HPAPP , 4HPOVE , 4HGIMS , 4HHORG , 4HPOAP , 4HUPRT/
   DATA mred2/27/
!
!     SELECT OPERATION
!     KODE = 1, NO SETLVL, NO STIFFNESS CALCULATIONS
!     KODE = 2, SETLVL, STIFFNESS CALCULATIONS
!     KODE = 3, NO SETLVL, NO STIFFNESS CALCULATIONS
!     KODE = 4, SETLVL, NO STIFFNESS CALCULATIONS
!
   IF ( Dry==-2 ) GOTO 99999
   IF ( Kode/=1 .AND. Kode/=3 ) THEN
!
!     SET UP NEW SUBSTRUCTURE
!
      IF ( .NOT.(Bounds .OR. Modes) ) THEN
         numb = 1
         CALL setlvl(Newnam,numb,Oldnam,itest,mred2)
         IF ( itest==8 ) THEN
            WRITE (Iprntr,99001) Ufm
99001       FORMAT (A23,' 6518, ONE OF THE COMPONENT SUBSTRUCTURES HAS BEEN ','USED IN A PREVIOUS COMBINE OR REDUCE.')
            Dry = -2
            GOTO 99999
         ENDIF
      ENDIF
      IF ( Kode/=4 ) THEN
!
!     FORM PRELIMINARY STIFFNESS CALCULATION
!
!                                      T
!        **      **   **   **   **   ** **   **
!        *        *   *     *   *     * *     *
!        * KBARBB * = * KBB * + * GIB * * KIB *
!        *        *   *     *   *     * *     *
!        **      **   **   **   **   ** **   **
!
         itrlr1(1) = Kbb
         CALL rdtrl(itrlr1)
         item = itmlst(8)
         itmnam(1) = Oldnam(1)
         itmnam(2) = Oldnam(2)
         CALL softrl(Oldnam,item,itrlr2)
         itest = itrlr2(1)
         IF ( itest/=1 ) GOTO 200
         CALL mtrxi(Gib,Oldnam,item,0,itest)
         IF ( itest/=1 ) GOTO 200
         CALL sofcls
         itrlr2(1) = Gib
         CALL rdtrl(itrlr2)
         itrlr3(1) = Kib
         CALL rdtrl(itrlr3)
         DO i = 1 , 7
            Itrlra(i) = itrlr2(i)
            Itrlrb(i) = itrlr3(i)
            Itrlrc(i) = itrlr1(i)
         ENDDO
         iform = 6
         iprc = 1
         ityp = 0
         IF ( (Itrlra(5)==2) .OR. (Itrlra(5)==4) ) iprc = 2
         IF ( (Itrlrb(5)==2) .OR. (Itrlrb(5)==4) ) iprc = 2
         IF ( (Itrlrc(5)==2) .OR. (Itrlrc(5)==4) ) iprc = 2
         IF ( Itrlra(5)>=3 ) ityp = 2
         IF ( Itrlrb(5)>=3 ) ityp = 2
         IF ( Itrlrc(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(Itrlrd,Kbarbb,itrlr1(3),iform,itype)
         T = 1
         Signab = 1
         Signc = 1
         Prec = 0
         Scr = Iscr(9)
         dblkor = Korbgn/2 + 1
         Nz = Lstzwd - (2*dblkor-1)
         CALL mpyad(Dz(dblkor),Dz(dblkor),Dz(dblkor))
         CALL wrttrl(Itrlrd)
         kbarow = Itrlrd(3)
         kcol = Itrlrd(2)
!
!     FORM PRELIMINARY STIFFNESS CALCULATION
!
!                         T
!        **   **   **   ** **   ** **   **
!        *     *   *     * *     * *     *
!        * KEE * = * HIE * * KII * * HIE *
!        *     *   *     * *     * *     *
!        **   **   **   ** **   ** **   **
!
         itrlr1(1) = Hie
         itrlr2(1) = Kii
         CALL rdtrl(itrlr1)
         CALL rdtrl(itrlr2)
         DO i = 1 , 7
            Jtrlra(i) = itrlr1(i)
            Jtrlrb(i) = itrlr2(i)
            Jtrlre(i) = 0
         ENDDO
         iprc = 1
         ityp = 0
         IF ( Jtrlra(5)==2 .OR. Jtrlra(5)==4 ) iprc = 2
         IF ( Jtrlrb(5)==2 .OR. Jtrlrb(5)==4 ) iprc = 2
         IF ( Jtrlra(5)>=3 .OR. Jtrlrb(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(Jtrlrc,Kee,itrlr1(2),iform,itype)
         Jscr(1) = Iscr(9)
         Jscr(2) = Iscr(2)
         Jscr(3) = Iscr(1)
         Lkore = Nz
         Icode = 0
         Prec3 = 0
         CALL mpy3dr(Dz(dblkor))
         CALL wrttrl(Jtrlrc)
         keerow = Jtrlrc(3)
         keecol = Jtrlrc(2)
!
!     GENERATE MERGE PARTITION VECTOR
!
         Nrow = kcol + keecol
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
!        **   **   *   0    . KEE *
!                  *        .     *
!                  **            **
!
         isub(1) = kcol
         isub(2) = keecol
         isub(3) = kbarow
         isub(4) = keerow
         iform = 6
         CALL gmmerg(Khh,Kbarbb,0,0,Kee,Rprtn,Rprtn,isub,iform,Z(Korbgn),Korlen)
!
!     STORE KHH AS KMTX ON SOF
!
         CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
         itmnam(1) = Newnam(1)
         itmnam(2) = Newnam(2)
         CALL mtrxo(Khh,Newnam,itmlst(1),0,itest)
         item = itmlst(1)
         IF ( itest==3 ) GOTO 100
         GOTO 200
      ENDIF
   ENDIF
!
!     LOCATE HGH MATRIX
!
   CALL mtrxi(Hgh,Oldnam,itmlst(9),0,itest)
   item = itmlst(9)
   itmnam(1) = Oldnam(1)
   itmnam(2) = Oldnam(2)
   IF ( itest/=1 ) GOTO 200
 100  Signab = 1
   Signc = 1
   Scr = Iscr(1)
   dblkor = Korbgn/2 + 1
   Lcore = Lstzwd - (2*dblkor-1)
!
!     GENERATE MATRICES REQUESTED
!     I = 2, GENERATE MHH MATRIX
!     I = 3, GENERATE BHH MATRIX
!     I = 4, GENERATE K4HH MATRIX
!     I = 5, GENERATE PHH MATRIX
!
   DO i = 2 , 5
      itrlr1(1) = Infile(i+5)
      CALL rdtrl(itrlr1)
      IF ( itrlr1(1)>=0 ) THEN
         CALL sofcls
!
!     CALCULATE MATRIX REQUIRED
!
!                                T
!        **          **   **   ** **          ** **   **
!        *            *   *     * *            * *     *
!        * (M,B,K4)HH * = * HGH * * (M,B,K4)AA * * HGH *
!        *            *   *     * *            * *     *
!        **          **   **   ** **          ** **   **
!
!                         T
!        **   **   **   ** **   **
!        *     *   *     * *     *
!        * PHH * = * HGH * * PAA *
!        *     *   *     * *     *
!        **   **   **   ** **   **
!
         itrlr2(1) = Hgh
         CALL rdtrl(itrlr2)
         item = itmlst(i)
         IF ( i==5 .AND. Popt==papp ) item = itmlst(6)
         DO j = 1 , 7
            Jtrlra(j) = itrlr2(j)
            Jtrlrb(j) = itrlr1(j)
            Jtrlre(j) = 0
         ENDDO
         iform = 6
         iprc = 1
         ityp = 0
         IF ( Jtrlra(5)==2 .OR. Jtrlra(5)==4 ) iprc = 2
         IF ( Jtrlrb(5)==2 .OR. Jtrlrb(5)==4 ) iprc = 2
         IF ( Jtrlra(5)>=3 .OR. Jtrlrb(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(Jtrlrc,Otfile(i),itrlr2(2),iform,itype)
         Jscr(1) = Iscr(9)
         Jscr(2) = Iscr(2)
         Jscr(3) = Iscr(1)
         Icode = 0
         IF ( i==5 ) Icode = 1
         Prec3 = 0
         CALL mpy3dr(Dz(dblkor))
         CALL wrttrl(Jtrlrc)
!
!     STORE MATRIX ON SOF
!     I = 2, STORE MHH AS MMTX
!     I = 3, STORE BHH AS BMTX
!     I = 4, STORE K4HH AS K4MX
!     I = 5, STORE PHH AS PVEC OR PAPP
!
         CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
         itmnam(1) = Newnam(1)
         itmnam(2) = Newnam(2)
         CALL mtrxo(Otfile(i),Newnam,item,0,itest)
         IF ( itest/=3 ) GOTO 200
      ENDIF
   ENDDO
!
!     TEST FOR LOAD PROCESSING
!
   IF ( Popt==blanks ) GOTO 99999
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
      item = itmlst(11)
      itmnam(1) = Oldnam(1)
      itmnam(2) = Oldnam(2)
      CALL mtrxi(Uprt,Oldnam,item,0,itest)
      IF ( itest/=1 ) GOTO 200
   ENDIF
   CALL gmprtn(Paa,Pove,0,0,0,0,Uprt,Nsub(1),Nsub(2),Z(Korbgn),Korlen)
!
!     SAVE POVE AS POVE OR POAP ON SOF
!
   IF ( Modes ) GOTO 99999
   item = itmlst(7)
   IF ( Popt==papp ) item = itmlst(10)
   CALL mtrxo(Pove,Oldnam,item,0,itest)
   IF ( itest==3 ) GOTO 99999
!
!     PROCESS MODULE ERRORS
!
 200  IF ( itest==2 ) THEN
      imsg = -11
   ELSEIF ( itest==3 ) THEN
      imsg = -1
      CALL smsg(imsg,item,itmnam)
      GOTO 99999
   ELSEIF ( itest==4 ) THEN
      imsg = -2
      CALL smsg(imsg,item,itmnam)
      GOTO 99999
   ELSEIF ( itest==5 ) THEN
      imsg = -3
      CALL smsg(imsg,item,itmnam)
      GOTO 99999
   ELSEIF ( itest==6 ) THEN
      imsg = -10
   ELSE
      imsg = -9
   ENDIF
   Dry = -2
   CALL smsg1(imsg,item,itmnam,modnam)
99999 RETURN
END SUBROUTINE mred2g
