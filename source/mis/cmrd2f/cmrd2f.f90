!*==cmrd2f.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmrd2f(Kode)
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
   INTEGER , SAVE :: blanks , cmred2 , papp
   INTEGER :: dblkor , eqst , gib , gibbar , hgh , hghbar , him , himbar , i , iform , imsg , iprc , item , itest , ityp , itype ,  &
            & j , kbarbb , kbarow , kbb , kcol , khh , kib , kii , kmm , kmmcol , kmmrow , numb , paa , pove , uprt , usetmr
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(4) :: isub
   INTEGER , DIMENSION(12) , SAVE :: itmlst
   INTEGER , DIMENSION(2) :: itmnam
   INTEGER , DIMENSION(7) :: itrlr1 , itrlr2 , itrlr3
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL :: rprtn
   REAL , DIMENSION(1) :: rz
   EXTERNAL calcv , close , gmmerg , gmprtn , gopen , makmcb , mpyad , mtrxi , mtrxo , pack , rdtrl , setlvl , smsg , sofcls ,      &
          & sofopn , softrl , wrttrl
!
! End of declarations rewritten by SPAG
!
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
   !>>>>EQUIVALENCE (Eqst,Infile(5)) , (Usetmr,Infile(6)) , (Paa,Infile(11)) , (Khh,Otfile(1)) , (Pove,Otfile(6)) , (Kbb,Iscr(1)) ,      &
!>>>>    & (Kib,Iscr(2)) , (Kii,Iscr(4)) , (Him,Iscr(10)) , (Uprt,Iscr(1)) , (Himbar,Iscr(8)) , (Kbarbb,Iscr(5)) , (Kmm,Iscr(6)) ,       &
!>>>>    & (Gib,Iscr(3)) , (Gibbar,Iscr(11)) , (Hghbar,Iscr(9)) , (Hgh,Iscr(8)) , (Rprtn,Iscr(1)) , (Rz(1),Z(1)) , (Dz(1),Z(1))
   DATA modnam/4HCMRD , 4H2F  / , papp/4HPAPP/ , blanks/4H    /
   DATA cmred2/26/
   DATA itmlst/4HKMTX , 4HHORG , 4HHLFT , 4HMMTX , 4HBMTX , 4HK4MX , 4HPVEC , 4HPAPP , 4HPOVE , 4HGIMS , 4HPOAP , 4HUPRT/
!
!     SELECT OPERATION MODE
!
   IF ( dry==-2 ) RETURN
   IF ( .NOT.(ponly .OR. dry==0) ) THEN
!
!     SET UP NEW SUBSTRUCTURE
!
      IF ( .NOT.(modes) ) THEN
         numb = 1
         CALL setlvl(newnam,numb,oldnam,itest,cmred2)
         IF ( itest==8 ) THEN
!
            WRITE (iprntr,99001) ufm
99001       FORMAT (A23,' 6518, ONE OF THE COMPONENT SUBSTRUCTURES HAS BEEN ','USED IN A PREVIOUS COMBINE OR REDUCE.')
            dry = -2
            RETURN
         ENDIF
      ENDIF
!
!     CHECK FOR STIFFNESS MATRIX GENERATION
!
      itrlr1(1) = khh
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
         itrlr1(1) = kbb
         CALL rdtrl(itrlr1)
         IF ( symtry ) THEN
            item = itmlst(10)
            CALL softrl(oldnam,item,itrlr2)
            itest = itrlr2(1)
            itmnam(1) = oldnam(1)
            itmnam(2) = oldnam(2)
            IF ( itest/=1 ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
            CALL mtrxi(gib,oldnam,item,0,itest)
            IF ( itest/=1 ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
            itrlr2(1) = gib
         ELSE
            itrlr2(1) = gibbar
            CALL rdtrl(itrlr2)
         ENDIF
         itrlr3(1) = kib
         CALL rdtrl(itrlr3)
         DO i = 1 , 7
            itrlra(i) = itrlr2(i)
            itrlrb(i) = itrlr3(i)
            itrlrc(i) = itrlr1(i)
         ENDDO
         iform = 1
         iprc = 1
         ityp = 0
         IF ( itrlra(5)==2 .OR. itrlra(5)==4 ) iprc = 2
         IF ( itrlrb(5)==2 .OR. itrlrb(5)==4 ) iprc = 2
         IF ( itrlrc(5)==2 .OR. itrlrc(5)==4 ) iprc = 2
         IF ( itrlra(5)>=3 ) ityp = 2
         IF ( itrlrb(5)>=3 ) ityp = 2
         IF ( itrlrc(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(itrlrd,kbarbb,itrlr1(3),iform,itype)
         t = 1
         signab = 1
         signc = 1
         prec = 0
         scr = iscr(7)
         scr = iscr(1)
         CALL sofcls
         dblkor = korbgn/2 + 1
         nz = lstzwd - (2*dblkor-1)
         CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
         CALL wrttrl(itrlrd)
         kbarow = itrlrd(3)
         kcol = itrlrd(2)
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
         itrlr1(1) = kii
         itrlr2(1) = him
         CALL rdtrl(itrlr1)
         CALL rdtrl(itrlr2)
         DO i = 1 , 7
            itrlra(i) = itrlr1(i)
            itrlrb(i) = itrlr2(i)
            itrlrc(i) = 0
         ENDDO
         iform = 2
         iprc = 1
         ityp = 0
         IF ( itrlra(5)==2 .OR. itrlra(5)==4 ) iprc = 2
         IF ( itrlrb(5)==2 .OR. itrlrb(5)==4 ) iprc = 2
         IF ( itrlra(5)>=3 ) ityp = 2
         IF ( itrlrb(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(itrlrd,iscr(2),itrlr2(3),iform,itype)
         prec = 0
         t = 0
         signab = 1
         signc = 1
         scr = iscr(1)
         CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
         CALL wrttrl(itrlrd)
         itrlr1(1) = him
         IF ( .NOT.symtry ) itrlr1(1) = himbar
         CALL rdtrl(itrlr1)
         DO i = 1 , 7
            itrlra(i) = itrlr1(i)
            itrlrb(i) = itrlrd(i)
         ENDDO
         iform = 1
         iprc = 1
         ityp = 0
         IF ( itrlra(5)==2 .OR. itrlra(5)==4 ) iprc = 2
         IF ( itrlrb(5)==2 .OR. itrlrb(5)==4 ) iprc = 2
         IF ( itrlra(5)>=3 ) ityp = 2
         IF ( itrlrb(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(itrlrd,kmm,itrlr1(2),iform,itype)
         t = 1
         prec = 0
         CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
         CALL wrttrl(itrlrd)
         kmmrow = itrlrd(3)
         kmmcol = itrlrd(2)
!
!     GENERATE MERGE PARTITION VECTOR
!
         nrow = kcol + kmmcol
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
!        **   **   *   0    . KMM *
!                  *        .     *
!                  **            **
!
         isub(1) = kcol
         isub(2) = kmmcol
         isub(3) = kbarow
         isub(4) = kmmrow
         itype = 1
         CALL gmmerg(khh,kbarbb,0,0,kmm,rprtn,rprtn,isub,itype,z(korbgn),korlen)
!
!     STORE KHH AS KMTX ON SOF
!
         CALL sofopn(z(sbuf1),z(sbuf2),z(sbuf3))
         CALL mtrxo(khh,newnam,itmlst(1),0,itest)
         item = itmlst(1)
         itmnam(1) = newnam(1)
         itmnam(2) = newnam(2)
         IF ( itest/=3 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
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
   itmnam(1) = oldnam(1)
   itmnam(2) = oldnam(2)
   CALL mtrxi(hgh,oldnam,item,0,itest)
   IF ( itest==1 ) THEN
      IF ( .NOT.(Kode>1 .OR. symtry) ) THEN
         item = itmlst(3)
         CALL mtrxi(hghbar,oldnam,item,0,itest)
         IF ( itest/=1 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
      ENDIF
      signab = 1
      signc = 1
      scr = iscr(1)
      dblkor = korbgn/2 + 1
      nz = lstzwd - (2*dblkor-1)
      itmnam(1) = newnam(1)
      itmnam(2) = newnam(2)
!
!     GENERATE MATRICES REQUESTED
!        I .EQ. 2, GENERATE MHH MATRIX
!        I .EQ. 3, GENERATE BHH MATRIX
!        I .EQ. 4, GENERATE K4HH MATRIX
!        I .EQ. 5, GENERATE PHH MATRIX
!
      DO i = 2 , 5
         itrlr1(1) = infile(i+6)
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
            itrlr2(1) = hgh
            CALL rdtrl(itrlr2)
            IF ( i==5 ) THEN
               DO j = 1 , 7
                  itrlrd(j) = itrlr1(j)
               ENDDO
               item = itmlst(7)
               IF ( popt==papp ) item = itmlst(8)
            ELSE
               DO j = 1 , 7
                  itrlra(j) = itrlr1(j)
                  itrlrb(j) = itrlr2(j)
                  itrlrc(j) = 0
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
               CALL makmcb(itrlrd,iscr(2),itrlr1(3),iform,itype)
               prec = 0
               t = 0
               signab = 1
               signc = 1
               scr = iscr(1)
               CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
               CALL wrttrl(itrlrd)
               item = itmlst(i+2)
            ENDIF
            itrlr2(1) = hgh
            IF ( .NOT.symtry ) itrlr2(1) = hghbar
            CALL rdtrl(itrlr2)
            DO j = 1 , 7
               itrlra(j) = itrlr2(j)
               itrlrb(j) = itrlrd(j)
            ENDDO
            iform = 1
            iprc = 1
            ityp = 0
            IF ( itrlrd(5)==2 .OR. itrlrd(5)==4 ) iprc = 2
            IF ( itrlr2(5)==2 .OR. itrlr2(5)==4 ) iprc = 2
            IF ( itrlrd(5)>=3 ) ityp = 2
            IF ( itrlr2(5)>=3 ) ityp = 2
            itype = iprc + ityp
            CALL makmcb(itrlrd,otfile(i),itrlr2(2),iform,itype)
            t = 1
            prec = 0
            CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
            CALL wrttrl(itrlrd)
!
!     STORE MATRIX ON SOF
!        I .EQ. 2, STORE MHH AS MMTX
!        I .EQ. 3, STORE BHH AS BMTX
!        I .EQ. 4, STORE K4HH AS K4MX
!        I .EQ. 5, STORE PHH AS PVEC OR PAPP
!
            CALL sofopn(z(sbuf1),z(sbuf2),z(sbuf3))
            CALL mtrxo(otfile(i),newnam,item,0,itest)
            IF ( itest/=3 ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
         ENDIF
      ENDDO
!
!     TEST FOR LOAD PROCESSING
!
      IF ( popt/=blanks ) THEN
         itmnam(1) = oldnam(1)
         itmnam(2) = oldnam(2)
         IF ( .NOT.ponly ) THEN
!
!     PARTITION PAA VECTOR
!
            lcore = korlen
            fuset = usetmr
            CALL calcv(uprt,un,ui,ub,z(korbgn))
         ELSE
            itrlr1(1) = eqst
            CALL rdtrl(itrlr1)
            nsub(1) = itrlr1(6)
            nsub(2) = itrlr1(7)
            item = itmlst(12)
            CALL mtrxi(uprt,oldnam,item,0,itest)
            IF ( itest/=1 ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
         ENDIF
         CALL gmprtn(paa,pove,0,0,0,0,uprt,nsub(1),nsub(2),z(korbgn),korlen)
!
!     SAVE POVE AS POVE OR POAP ON SOF
!
         IF ( .NOT.(modes) ) THEN
            item = itmlst(9)
            IF ( popt==papp ) item = itmlst(11)
            CALL mtrxo(pove,oldnam,item,0,itest)
            IF ( itest/=3 ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
         ENDIF
      ENDIF
      RETURN
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     PROCESS MODULE ERRORS
!
      IF ( Itest==4 ) THEN
!
         Imsg = -2
      ELSEIF ( Itest==5 ) THEN
         Imsg = -3
      ELSEIF ( Itest==6 ) THEN
!
         WRITE (Iprntr,99001) Ufm , Modnam , Item , Itmnam
99001    FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',' OF SOF ITEM ',A4,', SUBSTRUCTURE ',2A4,', IS PURGED.')
         Dry = -2
         RETURN
      ELSE
         WRITE (Iprntr,99002) Ufm , Modnam , Item , Itmnam
!
99002    FORMAT (A23,' 6211, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
         Dry = -2
         RETURN
      ENDIF
      CALL smsg(Imsg,Item,Itmnam)
   END SUBROUTINE spag_block_1
!
END SUBROUTINE cmrd2f
