!*==mred2l.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2l(Nuf,N2,Nus,Ufbits)
USE C_BLANK
USE C_MPYADX
USE C_PACKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nuf
   INTEGER :: N2
   INTEGER :: Nus
   INTEGER :: Ufbits
!
! Local variable declarations rewritten by SPAG
!
   REAL :: b , cprtn , determ , gs , hkpg , phi12i , phigs , phigsh , phis12 , phiss2 , phissi , rprtn , usetmr
   INTEGER :: dblkor , i , icol , icore , ident , ifile , iform , imsg , invert , ising , itest , itype , j , kolumn , kore ,       &
            & nwdsrd , phiss , phiss1 , rows , sglkor
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(4) :: isub
   INTEGER , DIMENSION(7) :: itrlr1 , itrlr2
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL , DIMENSION(1) :: rz
   EXTERNAL andf , close , gmmerg , gopen , invers , makmcb , mesage , mpyad , pack , rdtrl , read , sofcls , sofopn , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE PREFORMS PRELIMINARY CALCULATIONS AND MERGES OF
!     THE HK MATRIX FOR THE MRED2 MODULE.
!
   !>>>>EQUIVALENCE (Gs,Iscr(7)) , (Phiss1,Iscr(8)) , (Phiss2,Iscr(9)) , (Ident,Iscr(5)) , (Phiss,Iscr(6)) , (Phigs,Iscr(2)) ,           &
!>>>>    & (Phis12,Iscr(2)) , (Phi12i,Iscr(8)) , (Rprtn,Iscr(5)) , (Cprtn,Iscr(10)) , (Rz(1),Z(1)) , (Dz(1),Z(1))
   DATA modnam/4HMRED , 4H2L  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!                  -1
!     COMPUTE PHISS1
!
         IF ( Dry==-2 ) RETURN
         CALL sofcls
         ifile = phiss1
         itrlr1(1) = phiss1
         CALL rdtrl(itrlr1)
         CALL gopen(phiss1,Z(Gbuf1),0)
         kolumn = itrlr1(2)
         rows = itrlr1(3)
         itest = kolumn*rows
         IF ( (Korbgn+itest+(3*kolumn))>=Korlen ) THEN
            imsg = -8
            ifile = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            kore = 0
            dblkor = (Korbgn/2) + 1
            sglkor = (2*dblkor) - 1
            IF ( itrlr1(5)==2 ) THEN
               DO i = 1 , kolumn
                  CALL read(*20,*40,phiss1,dz(dblkor+kore),rows,0,nwdsrd)
                  kore = kore + rows
               ENDDO
               icore = dblkor + itest
            ELSE
               DO i = 1 , kolumn
                  CALL read(*20,*40,phiss1,Z(sglkor+kore),rows,0,nwdsrd)
                  kore = kore + rows
               ENDDO
               icore = ((sglkor+itest)/2) + 1
            ENDIF
            CALL close(phiss1,1)
            invert = 0
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
            ising = -1
            CALL invers(rows,dz(dblkor),kolumn,b,invert,determ,ising,dz(icore))
            IF ( ising==2 ) THEN
               imsg = -37
               ifile = 0
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
               kore = 0
               Incr = 1
               Typin = 1
               Typout = 1
               Irow = 1
               Nrow = kolumn
               CALL makmcb(itrlr2,phiss1,Nrow,itrlr1(4),itrlr1(5))
               CALL gopen(phiss1,Z(Gbuf1),1)
               DO i = 1 , rows
                  IF ( itrlr1(5)==2 ) THEN
                     CALL pack(dz(dblkor+kore),phiss1,itrlr2)
                  ELSE
                     CALL pack(rz(sglkor+kore),phiss1,itrlr2)
                  ENDIF
                  kore = kore + rows
               ENDDO
               CALL close(phiss1,1)
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
               itrlr1(1) = phiss1
               itrlr2(1) = phiss
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
               CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
               CALL wrttrl(Itrlrd)
               itrlr1(1) = gs
               CALL rdtrl(itrlr1)
               DO i = 1 , 7
                  Itrlra(i) = Itrlrd(i)
                  Itrlrb(i) = itrlr1(i)
               ENDDO
               CALL makmcb(Itrlrd,phigs,itrlr1(3),itrlr1(4),itrlr1(5))
               Signab = 1
               CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
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
                  rz(Korbgn+i-1) = 0.0
                  IF ( i>N2 ) rz(Korbgn+i-1) = 1.0
               ENDDO
               Typin = 1
               Typout = 1
               Irow = 1
               Incr = 1
               iform = 7
               CALL makmcb(itrlr1,cprtn,Nrow,iform,Typin)
               CALL gopen(cprtn,Z(Gbuf1),1)
               CALL pack(Z(Korbgn),cprtn,itrlr1)
               CALL close(cprtn,1)
               CALL wrttrl(itrlr1)
               Nrow = Nus + Nuf
               ifile = usetmr
               CALL gopen(usetmr,Z(Gbuf1),0)
               DO i = 1 , Nrow
                  CALL read(*20,*40,usetmr,Z(Korbgn),1,0,nwdsrd)
                  rz(Korbgn+i) = 0.0
                  IF ( andf(Z(Korbgn),Ufbits)/=0 ) rz(Korbgn+i) = 1.0
               ENDDO
               CALL close(usetmr,1)
               Nrow = rows
               rows = Nuf + N2
               CALL makmcb(itrlr2,rprtn,Nrow,iform,Typin)
               CALL gopen(rprtn,Z(Gbuf1),1)
               CALL pack(Z(Korbgn+1),rprtn,itrlr2)
               CALL close(rprtn,1)
               CALL wrttrl(itrlr2)
               isub(1) = Nuf
               isub(2) = N2
               isub(3) = Nus
               isub(4) = N2
               itype = 2
               CALL gmmerg(hkpg,phigsh,0,phiss1,0,rprtn,cprtn,isub,itype,Z(Korbgn),Korlen)
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
               itrlr1(1) = phiss1
               itrlr2(1) = phiss2
               CALL rdtrl(itrlr1)
               CALL rdtrl(itrlr2)
               Modpts = itrlr1(3) + itrlr2(3)
               DO i = 1 , 7
                  Itrlra(i) = itrlr1(i)
                  Itrlrb(i) = itrlr2(i)
               ENDDO
               CALL makmcb(Itrlrd,phis12,itrlr2(3),itrlr2(4),itrlr2(5))
               Signab = -1
               CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
               CALL wrttrl(Itrlrd)
!
!     GENERATE IDENTITY MATRIX
!
               Nrow = Itrlrd(3)
               CALL makmcb(itrlr1,ident,Nrow,Itrlrd(4),Itrlrd(5))
               CALL gopen(ident,Z(Gbuf1),1)
               DO i = 1 , Nrow
                  DO j = 1 , Nrow
                     rz(Korbgn+j-1) = 0.0
                     IF ( j==i ) rz(Korbgn+j-1) = 1.0
                  ENDDO
                  CALL pack(Z(Korbgn),ident,itrlr1)
               ENDDO
               CALL close(ident,1)
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
               itrlr1(1) = phis12
               CALL rdtrl(itrlr1)
               isub(3) = itrlr1(3)
               isub(4) = Nrow
               Nrow = itrlr1(2) + Nrow
               DO i = 1 , Nrow
                  rz(Korbgn+i-1) = 0.0
                  IF ( i>itrlr1(2) ) rz(Korbgn+i-1) = 1.0
               ENDDO
               Incr = 1
               CALL makmcb(itrlr2,cprtn,Nrow,iform,Typin)
               CALL gopen(cprtn,Z(Gbuf1),1)
               CALL pack(Z(Korbgn),rprtn,itrlr2)
               CALL close(cprtn,1)
               CALL wrttrl(itrlr2)
               CALL gmmerg(phi12i,phis12,ident,0,0,0,cprtn,isub,itype,Z(Korbgn),Korlen)
               CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
               RETURN
            ENDIF
         ENDIF
!
!     PROCESS SYSTEM FATAL ERRORS
!
 20      imsg = -2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      imsg = -3
         spag_nextblock_1 = 2
      CASE (2)
         CALL sofcls
         CALL mesage(imsg,ifile,modnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mred2l
