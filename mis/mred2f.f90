
SUBROUTINE mred2f
   IMPLICIT NONE
!
! COMMON variable declarations
!
   LOGICAL Bounds , Frebdy
   INTEGER Cprtn , Dmr , Dry , Far , Fir , Fuset , Gbuf1 , Gbuf2 , Gib , Hgh , Hie , Him , Hir , Hirscr , Ident , Idum1 , Idum2 ,   &
         & Idum3 , Idum4(2) , Idum5(9) , Idum6(10) , Idum7 , Idum8(5) , Idum9(6) , Incrp , Incru , Infile(12) , Iprntr , Irowp ,    &
         & Irowu , Iscr(10) , Iscr11 , Itrlra(7) , Itrlrb(7) , Itrlrc(7) , Itrlrd(7) , Jtrlrb(7) , Jtrlrl(7) , Jtrlru(7) , Jtrlrx(7)&
         & , Korbgn , Korlen , Lcore , Lii , Lstzwd , Maa , Nrowp , Nrowu , Nsub(3) , Nzfbs , Nzmpy , Oldnam(2) , Precfb , Precmp , &
         & Rprtn , Sbuf1 , Sbuf2 , Sbuf3 , Scr , Sign , Signab , Signc , T , Typeop , Typinp , Typinu , Ub , Ui , Un , Usetmr ,     &
         & Z(1) , Zero
   DOUBLE PRECISION Dz(1)
   REAL Otfile(6) , Pprtn , Rz(1)
   COMMON /bitpos/ Idum5 , Un , Idum6 , Ub , Ui
   COMMON /blank / Idum1 , Dry , Idum7 , Gbuf1 , Gbuf2 , Idum2 , Sbuf1 , Sbuf2 , Sbuf3 , Infile , Otfile , Iscr , Korlen , Korbgn , &
                 & Oldnam , Idum4 , Frebdy , Idum8 , Bounds , Idum9 , Lstzwd , Iscr11
   COMMON /fbsx  / Jtrlrl , Jtrlru , Jtrlrb , Jtrlrx , Nzfbs , Precfb , Sign
   COMMON /mpyadx/ Itrlra , Itrlrb , Itrlrc , Itrlrd , Nzmpy , T , Signab , Signc , Precmp , Scr
   COMMON /packx / Typinp , Typeop , Irowp , Nrowp , Incrp
   COMMON /patx  / Lcore , Nsub , Fuset
   COMMON /system/ Idum3 , Iprntr
   COMMON /unpakx/ Typinu , Irowu , Nrowu , Incru
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER dblkor , farind , i , iform , ii , imsg , iprc , iscr7 , iscr8 , isub(4) , item , iter , itest , itmlst(4) , itrlr1(7) , &
         & itrlr2(7) , ityp , itype , j , modnam(2) , nrows , sglkor
   DOUBLE PRECISION dhirmg
   REAL hirmag , prec
!
! End of declarations
!
!
!     THIS SUBROUTINE COMPUTES THE FREEBODY EFFECTS FOR THE MRED2
!     MODULE.
!
!     INPUT DATA
!     GINO   - MAA    - SUBSTRUCTURE MASS MATRIX
!              DMR    - FREEBODY MATRIX
!     SOF    - GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS OF
!                       ORIGINAL SUBSTRUCTURE
!
!     OUTPUT DATA
!     GINO   - HGH    - HORG PARTITION MATRIX
!     SOF    - HORG   - H TRANSFORMATION MATRIX FOR ORIG. SUBSTRUCTURE
!
!     PARAMETERS
!     INPUT  - GBUF   - GINO BUFFERS
!              INFILE - INPUT FILE NUMBERS
!              ISCR   - SCRATCH FILE NUMBERS
!              KORLEN - LENGTH OF OPEN CORE
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              FREBDY - FREEBODY MODES OPTION FLAG
!     OTHERS - RPRTN  - ROW PARTITIONING VECTOR FILE NUMBER
!              LII    - LII PARTITION MATRIX FILE NUMBER (ISCR11)
!              IDENT  - IDENTITY MATRIX FILE NUMBER
!              ZERO   - ZERO MATRIX FILE NUMBER
!              HIE    - HIE PARTITION MATRIX FILE NUMBER
!              HIR    - HIR PARTITION MATRIX FILE NUMBER
!              HIRSCR - HIR SCRATCH PARTITION MATRIX FILE NUMBER
!              FBR    - FBR PARTITION MATRIX FILE NUMBER
!              FIR    - FIR PARTITION MATRIX FILE NUMBER
!              GIB    - GIMS INPUT FILE NUMBER
!              CPRTN  - COLUMN PARTITIONING VECTOR FILE NUMBER
!              HIM    - HIM PARTITION MATRIX FILE NUMBER
!              HGH    - HORG MATRIX FILE NUMBER
!
   EQUIVALENCE (Usetmr,Infile(5)) , (Maa,Infile(7)) , (Dmr,Infile(11)) , (Rprtn,Iscr(9)) , (Ident,Iscr(5)) , (Cprtn,Iscr(10)) ,     &
    & (Pprtn,Iscr(4)) , (Rz(1),Z(1)) , (Dz(1),Z(1)) , (Gib,Iscr(4)) , (Lii,Iscr11) , (Hirscr,Iscr(5)) , (Hgh,Iscr(8)) ,             &
    & (Zero,Iscr(6)) , (Him,Iscr(8)) , (Hie,Iscr(7)) , (Hir,Iscr(9)) , (Far,Iscr(9)) , (Fir,Iscr(10))
   DATA modnam/4HMRED , 4H2F  /
   DATA farind , iscr7 , iscr8/6 , 307 , 308/
   DATA itmlst/4HGIMS , 4HHORG , 4HUPRT , 4HLMTX/
!
!     TEST FREEBODY MODES CALCULATION FLAG
!
   IF ( Dry==-2 ) GOTO 99999
   itrlr2(1) = Dmr
   CALL rdtrl(itrlr2)
   IF ( itrlr2(1)<0 ) THEN
!
!     FREEBODY MODES NOT REQUESTED
!
      Hie = Him
      IF ( Hie==iscr7 ) Hgh = iscr8
      IF ( Hie==iscr8 ) Hgh = iscr7
      GOTO 300
   ELSE
!
!     COMPUTE FREEBODY MATRIX
!
!        **   **   **   ** **   **
!        *     *   *     * *     *
!        * FAR * = * MAA * * DMR *
!        *     *   *     * *     *
!        **   **   **   ** **   **
!
      CALL sofcls
      Frebdy = .TRUE.
      itrlr1(1) = Maa
      CALL rdtrl(itrlr1)
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
      CALL makmcb(Itrlrd,Far,itrlr1(3),iform,itype)
      T = 0
      Signab = 1
      Signc = 1
      prec = 0
      Scr = Iscr(4)
      dblkor = 1 + Korbgn/2
      Nzmpy = Lstzwd - 2*dblkor - 1
      CALL mpyad(Dz(dblkor),Dz(dblkor),Dz(dblkor))
      CALL wrttrl(Itrlrd)
!
!     PARTITION FAR INTO BOUNDARY, INTERIOR POINTS
!
!                  **   **
!                  *     *
!        **   **   * FBR *
!        *     *   *     *
!        * FAR * = *.....*
!        *     *   *     *
!        **   **   * FIR *
!                  *     *
!                  **   **
!
      Lcore = Nzmpy
      Fuset = Usetmr
      CALL calcv(Pprtn,Un,Ui,Ub,Z(Korbgn))
      CALL gmprtn(Far,Fir,0,0,0,0,Pprtn,Nsub(1),Nsub(2),Z(Korbgn),Korlen)
!
!     CALCULATE FREEBODY TRANSFORMATION MATRIX
!
!                       T
!        **   ** **   ** **   **    **   **
!        *     * *     * *     *    *     *
!        * LII * * LII * * HIR * = -* FIR *
!        *     * *     * *     *    *     *
!        **   ** **   ** **   **    **   **
!
      IF ( Bounds ) THEN
         item = itmlst(4)
         CALL softrl(Oldnam,item,Jtrlrl)
         itest = Jtrlrl(1)
         IF ( itest==1 ) THEN
            Jtrlrl(1) = Lii
            CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
            CALL mtrxi(Lii,Oldnam,item,0,itest)
            IF ( itest/=1 ) GOTO 400
            CALL sofcls
            GOTO 100
         ENDIF
      ENDIF
      Jtrlrl(1) = Lii
      CALL rdtrl(Jtrlrl)
   ENDIF
 100  Jtrlrb(1) = Fir
   CALL rdtrl(Jtrlrb)
   iform = 2
   iprc = 1
   ityp = 0
   IF ( Jtrlrl(5)==2 .OR. Jtrlrl(5)==4 ) iprc = 2
   IF ( Jtrlrb(5)==2 .OR. Jtrlrb(5)==4 ) iprc = 2
   IF ( Jtrlrl(5)>=3 ) ityp = 2
   IF ( Jtrlrb(5)>=3 ) ityp = 2
   itype = iprc + ityp
   CALL makmcb(Jtrlrx,Hir,Jtrlrb(3),iform,itype)
   Nzfbs = Nzmpy
   Precfb = itype
   Sign = -1
   CALL fbs(Z(Korbgn),Z(Korbgn))
   CALL wrttrl(Jtrlrx)
!
!     UNPACK HIR COLUMNS FOR SCALING
!
   Typinu = Jtrlrx(5)
   Irowu = 1
   Nrowu = Jtrlrx(3)
   Incru = Jtrlrx(5)
   Typinp = Jtrlrx(5)
   Typeop = Jtrlrx(5)
   Irowp = 1
   Nrowp = Jtrlrx(3)
   Incrp = Jtrlrx(5)
   CALL gopen(Hir,Z(Gbuf1),0)
   iform = Jtrlrx(4)
   CALL makmcb(itrlr1,Hirscr,Jtrlrx(3),iform,Jtrlrx(5))
   CALL gopen(Hirscr,Z(Gbuf2),1)
   sglkor = 2*dblkor - 1
   DO i = 1 , farind
      CALL unpack(*150,Hir,Dz(dblkor))
!
!     CALCULATE MAGNITUDE OF HIR
!
      IF ( Jtrlrx(5)==2 ) THEN
         dhirmg = Dz(dblkor)
         IF ( Nrowu/=1 ) THEN
            DO j = 2 , Nrowu
               IF ( dabs(Dz(dblkor+j-1))>dabs(dhirmg) ) dhirmg = Dz(dblkor+j-1)
            ENDDO
         ENDIF
      ELSE
         hirmag = Rz(sglkor)
         IF ( Nrowu/=1 ) THEN
            DO j = 2 , Nrowu
               IF ( abs(Rz(sglkor+j-1))>abs(hirmag) ) hirmag = Rz(sglkor+j-1)
            ENDDO
         ENDIF
      ENDIF
!
!     SCALE HIR COLUMN
!
      IF ( Jtrlrx(5)==2 ) THEN
         DO j = 1 , Nrowu
            Dz(dblkor+j-1) = Dz(dblkor+j-1)/dhirmg
         ENDDO
      ELSE
         DO j = 1 , Nrowu
            Rz(sglkor+j-1) = Rz(sglkor+j-1)/hirmag
         ENDDO
      ENDIF
      GOTO 200
!
!     NULL COLUMN
!
 150  IF ( Jtrlrx(5)==2 ) THEN
         DO j = 1 , Nrowu
            Dz(dblkor+j-1) = 0.0D0
         ENDDO
      ELSE
         DO j = 1 , Nrowu
            Rz(sglkor+j-1) = 0.0
         ENDDO
      ENDIF
!
!     PACK HIR COLUMN
!
 200  CALL pack(Dz(dblkor),Hirscr,itrlr1)
   ENDDO
   CALL close(Hirscr,1)
   CALL close(Hir,1)
   CALL wrttrl(itrlr1)
   isub(1) = itrlr1(2)
!
!     SET UP MERGE COLUMN PARTITION VECTOR
!
   itrlr2(1) = Him
   CALL rdtrl(itrlr2)
   i = itrlr1(2) + itrlr2(2)
   isub(2) = itrlr2(2)
   DO j = 1 , i
      Rz(Korbgn+j-1) = 0.0
      IF ( j>isub(1) ) Rz(Korbgn+j-1) = 1.0
   ENDDO
   Typinp = 1
   Typeop = 1
   Irowp = 1
   Nrowp = i
   Incrp = 1
   iform = 7
   CALL makmcb(itrlr2,Rprtn,Nrowp,iform,Typinp)
   CALL gopen(Rprtn,Z(Gbuf1),1)
   CALL pack(Rz(Korbgn),Rprtn,itrlr2)
   CALL close(Rprtn,1)
   CALL wrttrl(itrlr2)
!
!     MERGE FREEBODY, MODAL TRANSFORMATION MATRICES
!
!        **   **   **         **
!        *     *   *     .     *
!        * HIE * = * HIR . HIM *
!        *     *   *     .     *
!        **   **   **         **
!
   IF ( Hie==Him ) THEN
      Hie = iscr8
      Hgh = iscr7
   ENDIF
   itype = 1
   IF ( i/=itrlr2(3) ) itype = 2
   CALL gmmerg(Hie,Hirscr,0,Him,0,Rprtn,0,isub,itype,Z(Korbgn),Korlen)
   CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
!
!     FORM HGH MATRIX
!
!                  **         **
!                  *     .     *
!        **   **   *  I  .  0  *
!        *     *   *     .     *
!        * HGH * = *...........*
!        *     *   *     .     *
!        **   **   * GIB . HIE *
!                  *     .     *
!                  **         **
!
 300  CALL softrl(Oldnam,itmlst(2),itrlr1)
   IF ( itrlr1(1)==1 ) GOTO 99999
!
!     GENERATE IDENTITY MATRIX
!
   CALL softrl(Oldnam,itmlst(1),itrlr1)
   itest = itrlr1(1)
   item = itmlst(1)
   IF ( itest==1 ) THEN
      Typinp = 1
      Typeop = itrlr1(5)
      Irowp = 1
      Nrowp = itrlr1(2)
      Incrp = 1
      iform = 8
      ii = itrlr1(2)
      CALL makmcb(itrlr1,Ident,Nrowp,iform,Typeop)
      CALL gopen(Ident,Z(Gbuf1),1)
      DO i = 1 , ii
         DO j = 1 , ii
            Rz(Korbgn+j-1) = 0.0
            IF ( i==j ) Rz(Korbgn+j-1) = 1.0
         ENDDO
         CALL pack(Rz(Korbgn),Ident,itrlr1)
      ENDDO
      CALL close(Ident,1)
      CALL wrttrl(itrlr1)
!
!     SET UP MERGE ROW PARTITION VECTOR
!
      itrlr1(1) = Hie
      CALL rdtrl(itrlr1)
      iter = itrlr1(2)
      Nrowp = ii + iter
      DO i = 1 , Nrowp
         Rz(Korbgn+i-1) = 0.0
         IF ( i>ii ) Rz(Korbgn+i-1) = 1.0
      ENDDO
      Typinp = 1
      Typeop = 1
      Incrp = 1
      iform = 7
      CALL makmcb(itrlr2,Rprtn,Nrowp,iform,Typinp)
      CALL gopen(Rprtn,Z(Gbuf1),1)
      CALL pack(Rz(Korbgn),Rprtn,itrlr2)
      CALL close(Rprtn,1)
      CALL wrttrl(itrlr2)
      nrows = Nrowp
!
!     SET UP MERGE COLUMN PARTITION VECTOR
!
      item = itmlst(3)
      CALL mtrxi(Cprtn,Oldnam,item,0,itest)
      IF ( itest==1 ) THEN
!
!     SET UP GIB MATRIX
!
         CALL mtrxi(Gib,Oldnam,itmlst(1),0,itest)
         item = itmlst(1)
         IF ( itest==1 ) THEN
!
!     MERGE ALL STRUCTURAL REDUCTION TRANSFORMATION MATRICES
!
            isub(1) = ii
            isub(2) = iter
            isub(3) = itrlr1(3)
            isub(4) = ii
            itype = 1
            IF ( nrows/=Nrowp ) itype = 2
            CALL gmmerg(Hgh,Gib,Ident,Hie,0,Rprtn,Cprtn,isub,itype,Z(Korbgn),Korlen)
!
!     SAVE HGH ON SOF AS HORG MATRIX
!
            CALL mtrxo(Hgh,Oldnam,itmlst(2),0,itest)
            item = itmlst(2)
            IF ( itest==3 ) GOTO 99999
         ENDIF
      ENDIF
   ENDIF
!
!     PROCESS MODULE FATAL ERRORS
!
 400  IF ( itest==2 ) THEN
      imsg = -11
   ELSEIF ( itest==3 ) THEN
      imsg = -1
      CALL smsg(imsg,item,Oldnam)
      GOTO 99999
   ELSEIF ( itest==4 ) THEN
      imsg = -2
      CALL smsg(imsg,item,Oldnam)
      GOTO 99999
   ELSEIF ( itest==5 ) THEN
      imsg = -3
      CALL smsg(imsg,item,Oldnam)
      GOTO 99999
   ELSEIF ( itest==6 ) THEN
      imsg = -10
   ELSE
      imsg = -9
   ENDIF
   Dry = -2
   CALL smsg1(imsg,item,Oldnam,modnam)
99999 RETURN
END SUBROUTINE mred2f
