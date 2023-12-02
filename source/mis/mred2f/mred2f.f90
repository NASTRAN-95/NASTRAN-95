!*==mred2f.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2f
USE C_BITPOS
USE C_BLANK
USE C_FBSX
USE C_MPYADX
USE C_PACKX
USE C_PATX
USE C_SYSTEM
USE C_UNPAKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: cprtn , dblkor , dmr , far , fir , gib , hgh , hie , him , hir , hirscr , i , ident , iform , ii , imsg , iprc ,      &
            & item , iter , itest , ityp , itype , j , lii , maa , nrows , rprtn , sglkor , usetmr , zero
   REAL(REAL64) :: dhirmg
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , SAVE :: farind , iscr7 , iscr8
   REAL :: hirmag , pprtn , prec
   INTEGER , DIMENSION(4) :: isub
   INTEGER , DIMENSION(4) , SAVE :: itmlst
   INTEGER , DIMENSION(7) :: itrlr1 , itrlr2
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL , DIMENSION(1) :: rz
   EXTERNAL calcv , close , fbs , gmmerg , gmprtn , gopen , makmcb , mpyad , mtrxi , mtrxo , pack , rdtrl , smsg , smsg1 , sofcls , &
          & sofopn , softrl , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
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
   !>>>>EQUIVALENCE (Usetmr,Infile(5)) , (Maa,Infile(7)) , (Dmr,Infile(11)) , (Rprtn,Iscr(9)) , (Ident,Iscr(5)) , (Cprtn,Iscr(10)) ,     &
!>>>>    & (Pprtn,Iscr(4)) , (Rz(1),Z(1)) , (Dz(1),Z(1)) , (Gib,Iscr(4)) , (Lii,Iscr11) , (Hirscr,Iscr(5)) , (Hgh,Iscr(8)) ,             &
!>>>>    & (Zero,Iscr(6)) , (Him,Iscr(8)) , (Hie,Iscr(7)) , (Hir,Iscr(9)) , (Far,Iscr(9)) , (Fir,Iscr(10))
   DATA modnam/4HMRED , 4H2F  /
   DATA farind , iscr7 , iscr8/6 , 307 , 308/
   DATA itmlst/4HGIMS , 4HHORG , 4HUPRT , 4HLMTX/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     TEST FREEBODY MODES CALCULATION FLAG
!
         IF ( Dry==-2 ) RETURN
         itrlr2(1) = dmr
         CALL rdtrl(itrlr2)
         IF ( itrlr2(1)<0 ) THEN
!
!     FREEBODY MODES NOT REQUESTED
!
            hie = him
            IF ( hie==iscr7 ) hgh = iscr8
            IF ( hie==iscr8 ) hgh = iscr7
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
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
            itrlr1(1) = maa
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
            CALL makmcb(Itrlrd,far,itrlr1(3),iform,itype)
            T = 0
            Signab = 1
            Signc = 1
            prec = 0
            Scr = Iscr(4)
            dblkor = 1 + Korbgn/2
            Nzmpy = Lstzwd - 2*dblkor - 1
            CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
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
            Fuset = usetmr
            CALL calcv(pprtn,Un,Ui,Ub,Z(Korbgn))
            CALL gmprtn(far,fir,0,0,0,0,pprtn,Nsub(1),Nsub(2),Z(Korbgn),Korlen)
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
                  Jtrlrl(1) = lii
                  CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
                  CALL mtrxi(lii,Oldnam,item,0,itest)
                  IF ( itest/=1 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL sofcls
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            Jtrlrl(1) = lii
            CALL rdtrl(Jtrlrl)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         Jtrlrb(1) = fir
         CALL rdtrl(Jtrlrb)
         iform = 2
         iprc = 1
         ityp = 0
         IF ( Jtrlrl(5)==2 .OR. Jtrlrl(5)==4 ) iprc = 2
         IF ( Jtrlrb(5)==2 .OR. Jtrlrb(5)==4 ) iprc = 2
         IF ( Jtrlrl(5)>=3 ) ityp = 2
         IF ( Jtrlrb(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(Jtrlrx,hir,Jtrlrb(3),iform,itype)
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
         CALL gopen(hir,Z(Gbuf1),0)
         iform = Jtrlrx(4)
         CALL makmcb(itrlr1,hirscr,Jtrlrx(3),iform,Jtrlrx(5))
         CALL gopen(hirscr,Z(Gbuf2),1)
         sglkor = 2*dblkor - 1
         DO i = 1 , farind
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  CALL unpack(*2,hir,dz(dblkor))
!
!     CALCULATE MAGNITUDE OF HIR
!
                  IF ( Jtrlrx(5)==2 ) THEN
                     dhirmg = dz(dblkor)
                     IF ( Nrowu/=1 ) THEN
                        DO j = 2 , Nrowu
                           IF ( dabs(dz(dblkor+j-1))>dabs(dhirmg) ) dhirmg = dz(dblkor+j-1)
                        ENDDO
                     ENDIF
                  ELSE
                     hirmag = rz(sglkor)
                     IF ( Nrowu/=1 ) THEN
                        DO j = 2 , Nrowu
                           IF ( abs(rz(sglkor+j-1))>abs(hirmag) ) hirmag = rz(sglkor+j-1)
                        ENDDO
                     ENDIF
                  ENDIF
!
!     SCALE HIR COLUMN
!
                  IF ( Jtrlrx(5)==2 ) THEN
                     DO j = 1 , Nrowu
                        dz(dblkor+j-1) = dz(dblkor+j-1)/dhirmg
                     ENDDO
                  ELSE
                     DO j = 1 , Nrowu
                        rz(sglkor+j-1) = rz(sglkor+j-1)/hirmag
                     ENDDO
                  ENDIF
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
!
!     NULL COLUMN
!
 2                IF ( Jtrlrx(5)==2 ) THEN
                     DO j = 1 , Nrowu
                        dz(dblkor+j-1) = 0.0D0
                     ENDDO
                  ELSE
                     DO j = 1 , Nrowu
                        rz(sglkor+j-1) = 0.0
                     ENDDO
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
!
!     PACK HIR COLUMN
!
                  CALL pack(dz(dblkor),hirscr,itrlr1)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         CALL close(hirscr,1)
         CALL close(hir,1)
         CALL wrttrl(itrlr1)
         isub(1) = itrlr1(2)
!
!     SET UP MERGE COLUMN PARTITION VECTOR
!
         itrlr2(1) = him
         CALL rdtrl(itrlr2)
         i = itrlr1(2) + itrlr2(2)
         isub(2) = itrlr2(2)
         DO j = 1 , i
            rz(Korbgn+j-1) = 0.0
            IF ( j>isub(1) ) rz(Korbgn+j-1) = 1.0
         ENDDO
         Typinp = 1
         Typeop = 1
         Irowp = 1
         Nrowp = i
         Incrp = 1
         iform = 7
         CALL makmcb(itrlr2,rprtn,Nrowp,iform,Typinp)
         CALL gopen(rprtn,Z(Gbuf1),1)
         CALL pack(rz(Korbgn),rprtn,itrlr2)
         CALL close(rprtn,1)
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
         IF ( hie==him ) THEN
            hie = iscr8
            hgh = iscr7
         ENDIF
         itype = 1
         IF ( i/=itrlr2(3) ) itype = 2
         CALL gmmerg(hie,hirscr,0,him,0,rprtn,0,isub,itype,Z(Korbgn),Korlen)
         CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
         spag_nextblock_1 = 3
      CASE (3)
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
         CALL softrl(Oldnam,itmlst(2),itrlr1)
         IF ( itrlr1(1)==1 ) RETURN
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
            CALL makmcb(itrlr1,ident,Nrowp,iform,Typeop)
            CALL gopen(ident,Z(Gbuf1),1)
            DO i = 1 , ii
               DO j = 1 , ii
                  rz(Korbgn+j-1) = 0.0
                  IF ( i==j ) rz(Korbgn+j-1) = 1.0
               ENDDO
               CALL pack(rz(Korbgn),ident,itrlr1)
            ENDDO
            CALL close(ident,1)
            CALL wrttrl(itrlr1)
!
!     SET UP MERGE ROW PARTITION VECTOR
!
            itrlr1(1) = hie
            CALL rdtrl(itrlr1)
            iter = itrlr1(2)
            Nrowp = ii + iter
            DO i = 1 , Nrowp
               rz(Korbgn+i-1) = 0.0
               IF ( i>ii ) rz(Korbgn+i-1) = 1.0
            ENDDO
            Typinp = 1
            Typeop = 1
            Incrp = 1
            iform = 7
            CALL makmcb(itrlr2,rprtn,Nrowp,iform,Typinp)
            CALL gopen(rprtn,Z(Gbuf1),1)
            CALL pack(rz(Korbgn),rprtn,itrlr2)
            CALL close(rprtn,1)
            CALL wrttrl(itrlr2)
            nrows = Nrowp
!
!     SET UP MERGE COLUMN PARTITION VECTOR
!
            item = itmlst(3)
            CALL mtrxi(cprtn,Oldnam,item,0,itest)
            IF ( itest==1 ) THEN
!
!     SET UP GIB MATRIX
!
               CALL mtrxi(gib,Oldnam,itmlst(1),0,itest)
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
                  CALL gmmerg(hgh,gib,ident,hie,0,rprtn,cprtn,isub,itype,Z(Korbgn),Korlen)
!
!     SAVE HGH ON SOF AS HORG MATRIX
!
                  CALL mtrxo(hgh,Oldnam,itmlst(2),0,itest)
                  item = itmlst(2)
                  IF ( itest==3 ) RETURN
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     PROCESS MODULE FATAL ERRORS
!
         IF ( itest==2 ) THEN
            imsg = -11
         ELSEIF ( itest==3 ) THEN
            imsg = -1
            CALL smsg(imsg,item,Oldnam)
            RETURN
         ELSEIF ( itest==4 ) THEN
            imsg = -2
            CALL smsg(imsg,item,Oldnam)
            RETURN
         ELSEIF ( itest==5 ) THEN
            imsg = -3
            CALL smsg(imsg,item,Oldnam)
            RETURN
         ELSEIF ( itest==6 ) THEN
            imsg = -10
         ELSE
            imsg = -9
         ENDIF
         Dry = -2
         CALL smsg1(imsg,item,Oldnam,modnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mred2f
