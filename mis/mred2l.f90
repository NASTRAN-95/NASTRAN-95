
SUBROUTINE mred2l(Nuf,N2,Nus,Ufbits)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cprtn , Gs , Phi12i , Phigs , Phis12 , Phiss2 , Rprtn , Rz(1) , Z(1)
   INTEGER Dry , Gbuf1 , Ident , Idum1 , Idum2 , Idum3(2) , Idum4(14) , Idum5(2) , Incr , Infile(12) , Irow , Iscr(10) , Itrlra(7) ,&
         & Itrlrb(7) , Itrlrc(7) , Itrlrd(7) , Korbgn , Korlen , Lstzwd , Modpts , Nrow , Nz , Otfile(6) , Phiss , Phiss1 , Prec ,  &
         & Sbuf1 , Sbuf2 , Sbuf3 , Scr , Signab , Signc , T , Typin , Typout
   DOUBLE PRECISION Dz(1)
   COMMON /blank / Idum1 , Dry , Idum2 , Gbuf1 , Idum3 , Sbuf1 , Sbuf2 , Sbuf3 , Infile , Otfile , Iscr , Korlen , Korbgn , Idum4 , &
                 & Modpts , Idum5 , Lstzwd
   COMMON /mpyadx/ Itrlra , Itrlrb , Itrlrc , Itrlrd , Nz , T , Signab , Signc , Prec , Scr
   COMMON /packx / Typin , Typout , Irow , Nrow , Incr
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER N2 , Nuf , Nus , Ufbits
!
! Local variable declarations
!
   INTEGER andf
   REAL b , determ , hkpg , phigsh , phissi , usetmr
   INTEGER dblkor , i , icol , icore , ifile , iform , imsg , invert , ising , isub(4) , itest , itrlr1(7) , itrlr2(7) , itype , j ,&
         & kolumn , kore , modnam(2) , nwdsrd , rows , sglkor
   EXTERNAL andf
!
! End of declarations
!
!
!     THIS SUBROUTINE PREFORMS PRELIMINARY CALCULATIONS AND MERGES OF
!     THE HK MATRIX FOR THE MRED2 MODULE.
!
   EQUIVALENCE (Gs,Iscr(7)) , (Phiss1,Iscr(8)) , (Phiss2,Iscr(9)) , (Ident,Iscr(5)) , (Phiss,Iscr(6)) , (Phigs,Iscr(2)) ,           &
    & (Phis12,Iscr(2)) , (Phi12i,Iscr(8)) , (Rprtn,Iscr(5)) , (Cprtn,Iscr(10)) , (Rz(1),Z(1)) , (Dz(1),Z(1))
   DATA modnam/4HMRED , 4H2L  /
!
!                  -1
!     COMPUTE PHISS1
!
   IF ( Dry==-2 ) RETURN
   CALL sofcls
   ifile = Phiss1
   itrlr1(1) = Phiss1
   CALL rdtrl(itrlr1)
   CALL gopen(Phiss1,Z(Gbuf1),0)
   kolumn = itrlr1(2)
   rows = itrlr1(3)
   itest = kolumn*rows
   IF ( (Korbgn+itest+(3*kolumn))>=Korlen ) THEN
      imsg = -8
      ifile = 0
      GOTO 300
   ELSE
      kore = 0
      dblkor = (Korbgn/2) + 1
      sglkor = (2*dblkor) - 1
      IF ( itrlr1(5)==2 ) THEN
         DO i = 1 , kolumn
            CALL read(*100,*200,Phiss1,Dz(dblkor+kore),rows,0,nwdsrd)
            kore = kore + rows
         ENDDO
         icore = dblkor + itest
      ELSE
         DO i = 1 , kolumn
            CALL read(*100,*200,Phiss1,Z(sglkor+kore),rows,0,nwdsrd)
            kore = kore + rows
         ENDDO
         icore = ((sglkor+itest)/2) + 1
      ENDIF
      CALL close(Phiss1,1)
      invert = 0
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
      ising = -1
      CALL invers(rows,Dz(dblkor),kolumn,b,invert,determ,ising,Dz(icore))
      IF ( ising==2 ) THEN
         imsg = -37
         ifile = 0
         GOTO 300
      ELSE
         kore = 0
         Incr = 1
         Typin = 1
         Typout = 1
         Irow = 1
         Nrow = kolumn
         CALL makmcb(itrlr2,Phiss1,Nrow,itrlr1(4),itrlr1(5))
         CALL gopen(Phiss1,Z(Gbuf1),1)
         DO i = 1 , rows
            IF ( itrlr1(5)==2 ) THEN
               CALL pack(Dz(dblkor+kore),Phiss1,itrlr2)
            ELSE
               CALL pack(Rz(sglkor+kore),Phiss1,itrlr2)
            ENDIF
            kore = kore + rows
         ENDDO
         CALL close(Phiss1,1)
!
!     COMPUTE PHIGS
!
!                               -1
!        **     **    **      **  **     ** **  **
!        *       *    *        *  *       * *    *
!        * PHIGS * = -* PHISS1 *  * PHISS * * GS *
!        *       *    *        *  *       * *    *
!        **     **    **      **  **     ** **  **
!
         itrlr1(1) = Phiss1
         itrlr2(1) = Phiss
         CALL rdtrl(itrlr1)
         CALL rdtrl(itrlr2)
         icol = itrlr2(3)
         DO i = 1 , 7
            Itrlra(i) = itrlr1(i)
            Itrlrb(i) = itrlr2(i)
            Itrlrc(i) = 0
         ENDDO
         CALL makmcb(Itrlrd,phissi,itrlr2(3),itrlr2(4),itrlr2(5))
         T = 0
         Signab = -1
         Signc = 1
         Prec = 0
         Scr = Iscr(10)
         Nz = Lstzwd - ((2*dblkor)-1)
         CALL mpyad(Dz(dblkor),Dz(dblkor),Dz(dblkor))
         CALL wrttrl(Itrlrd)
         itrlr1(1) = Gs
         CALL rdtrl(itrlr1)
         DO i = 1 , 7
            Itrlra(i) = Itrlrd(i)
            Itrlrb(i) = itrlr1(i)
         ENDDO
         CALL makmcb(Itrlrd,Phigs,itrlr1(3),itrlr1(4),itrlr1(5))
         Signab = 1
         CALL mpyad(Dz(dblkor),Dz(dblkor),Dz(dblkor))
         CALL wrttrl(Itrlrd)
!
!     FORM HKPG MATRIX
!
!                   **               **
!                   *       .         *
!                   *       .      -1 *
!        **    **   * PHIGS . PHISS   *
!        *      *   *       .         *
!        * HKPG * = *.................*
!        *      *   *       .         *
!        **    **   *   0   .    0    *
!                   *       .         *
!                   **               **
!
         Nrow = Nuf + N2
         DO i = 1 , Nrow
            Rz(Korbgn+i-1) = 0.0
            IF ( i>N2 ) Rz(Korbgn+i-1) = 1.0
         ENDDO
         Typin = 1
         Typout = 1
         Irow = 1
         Incr = 1
         iform = 7
         CALL makmcb(itrlr1,Cprtn,Nrow,iform,Typin)
         CALL gopen(Cprtn,Z(Gbuf1),1)
         CALL pack(Z(Korbgn),Cprtn,itrlr1)
         CALL close(Cprtn,1)
         CALL wrttrl(itrlr1)
         Nrow = Nus + Nuf
         ifile = usetmr
         CALL gopen(usetmr,Z(Gbuf1),0)
         DO i = 1 , Nrow
            CALL read(*100,*200,usetmr,Z(Korbgn),1,0,nwdsrd)
            Rz(Korbgn+i) = 0.0
            IF ( andf(Z(Korbgn),Ufbits)/=0 ) Rz(Korbgn+i) = 1.0
         ENDDO
         CALL close(usetmr,1)
         Nrow = rows
         rows = Nuf + N2
         CALL makmcb(itrlr2,Rprtn,Nrow,iform,Typin)
         CALL gopen(Rprtn,Z(Gbuf1),1)
         CALL pack(Z(Korbgn+1),Rprtn,itrlr2)
         CALL close(Rprtn,1)
         CALL wrttrl(itrlr2)
         isub(1) = Nuf
         isub(2) = N2
         isub(3) = Nus
         isub(4) = N2
         itype = 2
         CALL gmmerg(hkpg,phigsh,0,Phiss1,0,Rprtn,Cprtn,isub,itype,Z(Korbgn),Korlen)
!
!     COMPUTE PHIS12
!
!                                -1
!        **      **    **      **  **      **
!        *        *    *        *  *        *
!        * PHIS12 * = -* PHISS1 *  * PHISS2 *
!        *        *    *        *  *        *
!        **      **    **      **  **      **
!
         itrlr1(1) = Phiss1
         itrlr2(1) = Phiss2
         CALL rdtrl(itrlr1)
         CALL rdtrl(itrlr2)
         Modpts = itrlr1(3) + itrlr2(3)
         DO i = 1 , 7
            Itrlra(i) = itrlr1(i)
            Itrlrb(i) = itrlr2(i)
         ENDDO
         CALL makmcb(Itrlrd,Phis12,itrlr2(3),itrlr2(4),itrlr2(5))
         Signab = -1
         CALL mpyad(Dz(dblkor),Dz(dblkor),Dz(dblkor))
         CALL wrttrl(Itrlrd)
!
!     GENERATE IDENTITY MATRIX
!
         Nrow = Itrlrd(3)
         CALL makmcb(itrlr1,Ident,Nrow,Itrlrd(4),Itrlrd(5))
         CALL gopen(Ident,Z(Gbuf1),1)
         DO i = 1 , Nrow
            DO j = 1 , Nrow
               Rz(Korbgn+j-1) = 0.0
               IF ( j==i ) Rz(Korbgn+j-1) = 1.0
            ENDDO
            CALL pack(Z(Korbgn),Ident,itrlr1)
         ENDDO
         CALL close(Ident,1)
         CALL wrttrl(itrlr1)
!
!     GENERATE PHI12I MATRIX
!
!                     **      **
!                     *        *
!        **      **   * PHIS12 *
!        *        *   *        *
!        * PHI12I * = *........*
!        *        *   *        *
!        **      **   *   I    *
!                     *        *
!                     **      **
!
         itrlr1(1) = Phis12
         CALL rdtrl(itrlr1)
         isub(3) = itrlr1(3)
         isub(4) = Nrow
         Nrow = itrlr1(2) + Nrow
         DO i = 1 , Nrow
            Rz(Korbgn+i-1) = 0.0
            IF ( i>itrlr1(2) ) Rz(Korbgn+i-1) = 1.0
         ENDDO
         Incr = 1
         CALL makmcb(itrlr2,Cprtn,Nrow,iform,Typin)
         CALL gopen(Cprtn,Z(Gbuf1),1)
         CALL pack(Z(Korbgn),Rprtn,itrlr2)
         CALL close(Cprtn,1)
         CALL wrttrl(itrlr2)
         CALL gmmerg(Phi12i,Phis12,Ident,0,0,0,Cprtn,isub,itype,Z(Korbgn),Korlen)
         CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
         RETURN
      ENDIF
   ENDIF
!
!     PROCESS SYSTEM FATAL ERRORS
!
 100  imsg = -2
   GOTO 300
 200  imsg = -3
 300  CALL sofcls
   CALL mesage(imsg,ifile,modnam)
END SUBROUTINE mred2l
