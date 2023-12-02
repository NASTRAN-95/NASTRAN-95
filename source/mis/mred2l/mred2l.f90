!*==mred2l.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2l(Nuf,N2,Nus,Ufbits)
   USE c_blank
   USE c_mpyadx
   USE c_packx
   USE c_zzzzzz
   USE iso_fortran_env
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
         IF ( dry==-2 ) RETURN
         CALL sofcls
         ifile = phiss1
         itrlr1(1) = phiss1
         CALL rdtrl(itrlr1)
         CALL gopen(phiss1,z(gbuf1),0)
         kolumn = itrlr1(2)
         rows = itrlr1(3)
         itest = kolumn*rows
         IF ( (korbgn+itest+(3*kolumn))>=korlen ) THEN
            imsg = -8
            ifile = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            kore = 0
            dblkor = (korbgn/2) + 1
            sglkor = (2*dblkor) - 1
            IF ( itrlr1(5)==2 ) THEN
               DO i = 1 , kolumn
                  CALL read(*20,*40,phiss1,dz(dblkor+kore),rows,0,nwdsrd)
                  kore = kore + rows
               ENDDO
               icore = dblkor + itest
            ELSE
               DO i = 1 , kolumn
                  CALL read(*20,*40,phiss1,z(sglkor+kore),rows,0,nwdsrd)
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
               incr = 1
               typin = 1
               typout = 1
               irow = 1
               nrow = kolumn
               CALL makmcb(itrlr2,phiss1,nrow,itrlr1(4),itrlr1(5))
               CALL gopen(phiss1,z(gbuf1),1)
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
                  itrlra(i) = itrlr1(i)
                  itrlrb(i) = itrlr2(i)
                  itrlrc(i) = 0
               ENDDO
               CALL makmcb(itrlrd,phissi,itrlr2(3),itrlr2(4),itrlr2(5))
               t = 0
               signab = -1
               signc = 1
               prec = 0
               scr = iscr(10)
               nz = lstzwd - ((2*dblkor)-1)
               CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
               CALL wrttrl(itrlrd)
               itrlr1(1) = gs
               CALL rdtrl(itrlr1)
               DO i = 1 , 7
                  itrlra(i) = itrlrd(i)
                  itrlrb(i) = itrlr1(i)
               ENDDO
               CALL makmcb(itrlrd,phigs,itrlr1(3),itrlr1(4),itrlr1(5))
               signab = 1
               CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
               CALL wrttrl(itrlrd)
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
               nrow = Nuf + N2
               DO i = 1 , nrow
                  rz(korbgn+i-1) = 0.0
                  IF ( i>N2 ) rz(korbgn+i-1) = 1.0
               ENDDO
               typin = 1
               typout = 1
               irow = 1
               incr = 1
               iform = 7
               CALL makmcb(itrlr1,cprtn,nrow,iform,typin)
               CALL gopen(cprtn,z(gbuf1),1)
               CALL pack(z(korbgn),cprtn,itrlr1)
               CALL close(cprtn,1)
               CALL wrttrl(itrlr1)
               nrow = Nus + Nuf
               ifile = usetmr
               CALL gopen(usetmr,z(gbuf1),0)
               DO i = 1 , nrow
                  CALL read(*20,*40,usetmr,z(korbgn),1,0,nwdsrd)
                  rz(korbgn+i) = 0.0
                  IF ( andf(z(korbgn),Ufbits)/=0 ) rz(korbgn+i) = 1.0
               ENDDO
               CALL close(usetmr,1)
               nrow = rows
               rows = Nuf + N2
               CALL makmcb(itrlr2,rprtn,nrow,iform,typin)
               CALL gopen(rprtn,z(gbuf1),1)
               CALL pack(z(korbgn+1),rprtn,itrlr2)
               CALL close(rprtn,1)
               CALL wrttrl(itrlr2)
               isub(1) = Nuf
               isub(2) = N2
               isub(3) = Nus
               isub(4) = N2
               itype = 2
               CALL gmmerg(hkpg,phigsh,0,phiss1,0,rprtn,cprtn,isub,itype,z(korbgn),korlen)
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
               modpts = itrlr1(3) + itrlr2(3)
               DO i = 1 , 7
                  itrlra(i) = itrlr1(i)
                  itrlrb(i) = itrlr2(i)
               ENDDO
               CALL makmcb(itrlrd,phis12,itrlr2(3),itrlr2(4),itrlr2(5))
               signab = -1
               CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
               CALL wrttrl(itrlrd)
!
!     GENERATE IDENTITY MATRIX
!
               nrow = itrlrd(3)
               CALL makmcb(itrlr1,ident,nrow,itrlrd(4),itrlrd(5))
               CALL gopen(ident,z(gbuf1),1)
               DO i = 1 , nrow
                  DO j = 1 , nrow
                     rz(korbgn+j-1) = 0.0
                     IF ( j==i ) rz(korbgn+j-1) = 1.0
                  ENDDO
                  CALL pack(z(korbgn),ident,itrlr1)
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
               isub(4) = nrow
               nrow = itrlr1(2) + nrow
               DO i = 1 , nrow
                  rz(korbgn+i-1) = 0.0
                  IF ( i>itrlr1(2) ) rz(korbgn+i-1) = 1.0
               ENDDO
               incr = 1
               CALL makmcb(itrlr2,cprtn,nrow,iform,typin)
               CALL gopen(cprtn,z(gbuf1),1)
               CALL pack(z(korbgn),rprtn,itrlr2)
               CALL close(cprtn,1)
               CALL wrttrl(itrlr2)
               CALL gmmerg(phi12i,phis12,ident,0,0,0,cprtn,isub,itype,z(korbgn),korlen)
               CALL sofopn(z(sbuf1),z(sbuf2),z(sbuf3))
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
