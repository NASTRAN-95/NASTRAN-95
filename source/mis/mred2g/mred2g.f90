!*==mred2g.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2g(Kode)
   USE c_bitpos
   USE c_blank
   USE c_mpy3tl
   USE c_mpyadx
   USE c_packx
   USE c_patx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Kode
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blanks , mred2 , papp
   INTEGER :: cprtn , dblkor , eqst , gib , hgh , hie , i , iform , imsg , iprc , item , itest , ityp , itype , j , kbarbb ,        &
            & kbarow , kbb , kcol , kee , keecol , keerow , khh , kib , kii , numb , paa , pove , rprtn , uprt , usetmr , zerobe ,  &
            & zeroeb
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(4) :: isub
   INTEGER , DIMENSION(11) , SAVE :: itmlst
   INTEGER , DIMENSION(2) :: itmnam
   INTEGER , DIMENSION(7) :: itrlr1 , itrlr2 , itrlr3
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL , DIMENSION(1) :: rz
   EXTERNAL calcv , close , gmmerg , gmprtn , gopen , makmcb , mpy3dr , mpyad , mtrxi , mtrxo , pack , rdtrl , setlvl , smsg ,      &
          & smsg1 , sofcls , sofopn , softrl , wrttrl
!
! End of declarations rewritten by SPAG
!
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
   !>>>>EQUIVALENCE (Eqst,Infile(5)) , (Usetmr,Infile(5)) , (Paa,Infile(10)) , (Khh,Otfile(1)) , (Pove,Otfile(6))
   !>>>>EQUIVALENCE (Zerobe,Iscr(1)) , (Uprt,Iscr(1)) , (Kib,Iscr(2)) , (Zeroeb,Iscr(3)) , (Kii,Iscr(3)) , (Kbb,Iscr(1)) , (Gib,Iscr(4)) &
!>>>>    & , (Kbarbb,Iscr(5)) , (Kee,Iscr(6)) , (Hie,Iscr(7)) , (Hgh,Iscr(8)) , (Rprtn,Iscr(2)) , (Cprtn,Iscr(4)) , (Rz(1),Z(1)) ,       &
!>>>>    & (Dz(1),Z(1))
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
   IF ( dry==-2 ) RETURN
   IF ( Kode/=1 .AND. Kode/=3 ) THEN
!
!     SET UP NEW SUBSTRUCTURE
!
      IF ( .NOT.(bounds .OR. modes) ) THEN
         numb = 1
         CALL setlvl(newnam,numb,oldnam,itest,mred2)
         IF ( itest==8 ) THEN
            WRITE (iprntr,99001) ufm
99001       FORMAT (A23,' 6518, ONE OF THE COMPONENT SUBSTRUCTURES HAS BEEN ','USED IN A PREVIOUS COMBINE OR REDUCE.')
            dry = -2
            RETURN
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
         itrlr1(1) = kbb
         CALL rdtrl(itrlr1)
         item = itmlst(8)
         itmnam(1) = oldnam(1)
         itmnam(2) = oldnam(2)
         CALL softrl(oldnam,item,itrlr2)
         itest = itrlr2(1)
         IF ( itest/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         CALL mtrxi(gib,oldnam,item,0,itest)
         IF ( itest/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         CALL sofcls
         itrlr2(1) = gib
         CALL rdtrl(itrlr2)
         itrlr3(1) = kib
         CALL rdtrl(itrlr3)
         DO i = 1 , 7
            itrlra(i) = itrlr2(i)
            itrlrb(i) = itrlr3(i)
            itrlrc(i) = itrlr1(i)
         ENDDO
         iform = 6
         iprc = 1
         ityp = 0
         IF ( (itrlra(5)==2) .OR. (itrlra(5)==4) ) iprc = 2
         IF ( (itrlrb(5)==2) .OR. (itrlrb(5)==4) ) iprc = 2
         IF ( (itrlrc(5)==2) .OR. (itrlrc(5)==4) ) iprc = 2
         IF ( itrlra(5)>=3 ) ityp = 2
         IF ( itrlrb(5)>=3 ) ityp = 2
         IF ( itrlrc(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(itrlrd,kbarbb,itrlr1(3),iform,itype)
         t = 1
         signab = 1
         signc = 1
         prec = 0
         scr = iscr(9)
         dblkor = korbgn/2 + 1
         nz = lstzwd - (2*dblkor-1)
         CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
         CALL wrttrl(itrlrd)
         kbarow = itrlrd(3)
         kcol = itrlrd(2)
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
         itrlr1(1) = hie
         itrlr2(1) = kii
         CALL rdtrl(itrlr1)
         CALL rdtrl(itrlr2)
         DO i = 1 , 7
            jtrlra(i) = itrlr1(i)
            jtrlrb(i) = itrlr2(i)
            jtrlre(i) = 0
         ENDDO
         iprc = 1
         ityp = 0
         IF ( jtrlra(5)==2 .OR. jtrlra(5)==4 ) iprc = 2
         IF ( jtrlrb(5)==2 .OR. jtrlrb(5)==4 ) iprc = 2
         IF ( jtrlra(5)>=3 .OR. jtrlrb(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(jtrlrc,kee,itrlr1(2),iform,itype)
         jscr(1) = iscr(9)
         jscr(2) = iscr(2)
         jscr(3) = iscr(1)
         lkore = nz
         icode = 0
         prec3 = 0
         CALL mpy3dr(dz(dblkor))
         CALL wrttrl(jtrlrc)
         keerow = jtrlrc(3)
         keecol = jtrlrc(2)
!
!     GENERATE MERGE PARTITION VECTOR
!
         nrow = kcol + keecol
         DO i = 1 , nrow
            rz(korbgn+i-1) = 0.0
            IF ( i>kcol ) rz(korbgn+i-1) = 1.0
         ENDDO
         typin = 1
         typout = 1
         irow = 1
         incr = 1
         iform = 7
         CALL makmcb(itrlr1,rprtn,nrow,iform,typin)
         CALL gopen(rprtn,z(gbuf1),1)
         CALL pack(rz(korbgn),rprtn,itrlr1)
         CALL close(rprtn,1)
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
         CALL gmmerg(khh,kbarbb,0,0,kee,rprtn,rprtn,isub,iform,z(korbgn),korlen)
!
!     STORE KHH AS KMTX ON SOF
!
         CALL sofopn(z(sbuf1),z(sbuf2),z(sbuf3))
         itmnam(1) = newnam(1)
         itmnam(2) = newnam(2)
         CALL mtrxo(khh,newnam,itmlst(1),0,itest)
         item = itmlst(1)
         IF ( itest/=3 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         CALL spag_block_1
         RETURN
      ENDIF
   ENDIF
!
!     LOCATE HGH MATRIX
!
   CALL mtrxi(hgh,oldnam,itmlst(9),0,itest)
   item = itmlst(9)
   itmnam(1) = oldnam(1)
   itmnam(2) = oldnam(2)
   IF ( itest/=1 ) THEN
      CALL spag_block_2
      RETURN
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      Signab = 1
      Signc = 1
      Scr = iscr(1)
      Dblkor = Korbgn/2 + 1
      lcore = Lstzwd - (2*Dblkor-1)
!
!     GENERATE MATRICES REQUESTED
!     I = 2, GENERATE MHH MATRIX
!     I = 3, GENERATE BHH MATRIX
!     I = 4, GENERATE K4HH MATRIX
!     I = 5, GENERATE PHH MATRIX
!
      DO I = 2 , 5
         Itrlr1(1) = infile(I+5)
         CALL rdtrl(Itrlr1)
         IF ( Itrlr1(1)>=0 ) THEN
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
            Itrlr2(1) = Hgh
            CALL rdtrl(Itrlr2)
            Item = Itmlst(I)
            IF ( I==5 .AND. popt==Papp ) Item = Itmlst(6)
            DO J = 1 , 7
               jtrlra(J) = Itrlr2(J)
               jtrlrb(J) = Itrlr1(J)
               jtrlre(J) = 0
            ENDDO
            Iform = 6
            Iprc = 1
            Ityp = 0
            IF ( jtrlra(5)==2 .OR. jtrlra(5)==4 ) Iprc = 2
            IF ( jtrlrb(5)==2 .OR. jtrlrb(5)==4 ) Iprc = 2
            IF ( jtrlra(5)>=3 .OR. jtrlrb(5)>=3 ) Ityp = 2
            Itype = Iprc + Ityp
            CALL makmcb(Jtrlrc,otfile(I),Itrlr2(2),Iform,Itype)
            jscr(1) = iscr(9)
            jscr(2) = iscr(2)
            jscr(3) = iscr(1)
            Icode = 0
            IF ( I==5 ) Icode = 1
            Prec3 = 0
            CALL mpy3dr(Dz(Dblkor))
            CALL wrttrl(Jtrlrc)
!
!     STORE MATRIX ON SOF
!     I = 2, STORE MHH AS MMTX
!     I = 3, STORE BHH AS BMTX
!     I = 4, STORE K4HH AS K4MX
!     I = 5, STORE PHH AS PVEC OR PAPP
!
            CALL sofopn(z(Sbuf1),z(Sbuf2),z(Sbuf3))
            Itmnam(1) = Newnam(1)
            Itmnam(2) = Newnam(2)
            CALL mtrxo(otfile(I),Newnam,Item,0,Itest)
            IF ( Itest/=3 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
         ENDIF
      ENDDO
!
!     TEST FOR LOAD PROCESSING
!
      IF ( popt==Blanks ) RETURN
      IF ( .NOT.ponly ) THEN
!
!     PARTITION PAA VECTOR
!
         lcore = Korlen
         fuset = Usetmr
         CALL calcv(Uprt,un,ui,ub,z(Korbgn))
      ELSE
         Itrlr1(1) = Eqst
         CALL rdtrl(Itrlr1)
         nsub(1) = Itrlr1(6)
         nsub(2) = Itrlr1(7)
         Item = Itmlst(11)
         Itmnam(1) = Oldnam(1)
         Itmnam(2) = Oldnam(2)
         CALL mtrxi(Uprt,Oldnam,Item,0,Itest)
         IF ( Itest/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
      ENDIF
      CALL gmprtn(Paa,Pove,0,0,0,0,Uprt,nsub(1),nsub(2),z(Korbgn),Korlen)
!
!     SAVE POVE AS POVE OR POAP ON SOF
!
      IF ( Modes ) RETURN
      Item = Itmlst(7)
      IF ( popt==Papp ) Item = Itmlst(10)
      CALL mtrxo(Pove,Oldnam,Item,0,Itest)
      IF ( Itest==3 ) RETURN
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!     PROCESS MODULE ERRORS
!
      IF ( Itest==2 ) THEN
         Imsg = -11
      ELSEIF ( Itest==3 ) THEN
         Imsg = -1
         CALL smsg(Imsg,Item,Itmnam)
         RETURN
      ELSEIF ( Itest==4 ) THEN
         Imsg = -2
         CALL smsg(Imsg,Item,Itmnam)
         RETURN
      ELSEIF ( Itest==5 ) THEN
         Imsg = -3
         CALL smsg(Imsg,Item,Itmnam)
         RETURN
      ELSEIF ( Itest==6 ) THEN
         Imsg = -10
      ELSE
         Imsg = -9
      ENDIF
      Dry = -2
      CALL smsg1(Imsg,Item,Itmnam,Modnam)
   END SUBROUTINE spag_block_2
END SUBROUTINE mred2g
