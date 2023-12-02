!*==mred2o.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2o(Nus)
   USE c_blank
   USE c_mpy3tl
   USE c_packx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nus
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(11) :: block
   INTEGER :: dblkor , gs , gszero , hk , hm , i , ifile , iform , imsg , iop , iprc , ityp , itype , j , kmw2 , lamamr , m ,       &
            & nwdsrd , rprtn , typea , typeb
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(4) :: isub
   INTEGER , DIMENSION(7) :: itrlr1 , itrlr2
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL , DIMENSION(1) :: rz
   EXTERNAL close , fwdrec , gmmerg , gopen , makmcb , mesage , mpy3dr , pack , rdtrl , read , sofcls , sofopn , ssg2c , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE FORMS THE M MATRIX FOR THE MRED2 MODULE.
!
   !>>>>EQUIVALENCE (Lamamr,Infile(2)) , (Gs,Iscr(7)) , (Dz(1),Z(1)) , (Hk,Iscr(2)) , (Kmw2,Iscr(5)) , (Hm,Iscr(9)) , (Gszero,Iscr(10)) ,&
!>>>>    & (M,Iscr(10)) , (Rprtn,Iscr(8)) , (Rz(1),Z(1)) , (typea,block(1)) , (typeb,block(7))
   DATA modnam/4HMRED , 4H2O  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     FORM HM MATRIX
!
!        **  **   **  **   **          **
!        *    *   *    *   *    .   .   *
!        * HM * = * HK * + * GS . 0 . 0 *
!        *    *   *    *   *    .   .   *
!        **  **   **  **   **          **
!
         IF ( dry==-2 ) RETURN
         IF ( Nus==0 ) THEN
!
!     IF NO US POINTS
!
!        **  **   **  **
!        *    *   *    *
!        * HM * = * HK *
!        *    *   *    *
!        **  **   **  **
!
            hm = hk
            CALL sofcls
         ELSE
!
!     GENERATE ROW PARTITION VECTOR
!
            itrlr1(1) = hk
            CALL rdtrl(itrlr1)
            itrlr2(1) = gs
            CALL rdtrl(itrlr2)
            typin = 1
            typout = 1
            irow = 1
            nrow = itrlr1(2)
            incr = 1
            DO i = 1 , nrow
               rz(korbgn+i-1) = 0.0
               IF ( i>itrlr2(2) ) rz(korbgn+i-1) = 1.0
            ENDDO
            iform = 7
            CALL makmcb(itrlr2,rprtn,nrow,iform,typin)
            CALL gopen(rprtn,z(gbuf1),1)
            CALL pack(z(korbgn),rprtn,itrlr2)
            CALL close(rprtn,1)
            CALL wrttrl(itrlr2)
!
!     MERGE GS, ZERO MATRICES
!
            isub(1) = itrlr2(2)
            isub(2) = itrlr1(2) - itrlr2(2)
            CALL gmmerg(gszero,gs,0,0,0,rprtn,0,isub,itrlr2(5),z(korbgn),korlen)
!
!     FORM HM MATRIX
!
            itrlr2(1) = gszero
            CALL rdtrl(itrlr2)
            DO i = 1 , 11
               block(i) = 0.0
            ENDDO
            block(2) = 1.0
            block(8) = 1.0
            typea = itrlr1(5)
            typeb = itrlr2(5)
            iop = 1
            CALL sofcls
            CALL ssg2c(hk,gszero,hm,iop,block)
         ENDIF
!
!     FORM KMW2 = M  MATRIX
!                  I
!
         ifile = lamamr
         CALL gopen(lamamr,z(gbuf1),0)
         CALL fwdrec(*40,lamamr)
         iform = 3
         itype = 1
         CALL makmcb(itrlr1,kmw2,nmodes,iform,itype)
         typin = 1
         typout = 1
         irow = 1
         nrow = nmodes
         incr = 1
         CALL gopen(kmw2,z(gbuf2),1)
         DO i = 1 , nmodes
            CALL read(*20,*40,lamamr,z(korbgn),7,0,nwdsrd)
            DO j = 1 , nmodes
               rz(korbgn+7+j-1) = 0.0
               IF ( j==i ) rz(korbgn+7+j-1) = rz(korbgn+5)
            ENDDO
            CALL pack(z(korbgn+7),kmw2,itrlr1)
         ENDDO
         CALL close(lamamr,1)
         CALL close(kmw2,1)
         CALL wrttrl(itrlr1)
!
!     FORM M MATRIX
!
!                      T
!                **  ** **     ** **  **
!        ** **   *    * * .     * *    *
!        *   *   *    * *  .    * *    *
!        * M * = * HM * *   M   * * HM *     WHERE M = M
!        *   *   *    * *    .  * *    *                I
!        ** **   *    * *     . * *    *
!                **  ** **     ** **  **
!
         itrlr1(1) = hm
         itrlr2(1) = kmw2
         CALL rdtrl(itrlr1)
         CALL rdtrl(itrlr2)
         DO i = 1 , 7
            itrlra(i) = itrlr1(i)
            itrlrb(i) = itrlr2(i)
            itrlre(i) = 0
         ENDDO
         iprc = 1
         ityp = 0
         IF ( (itrlra(5)==2) .OR. (itrlra(5)==4) ) iprc = 2
         IF ( (itrlrb(5)==2) .OR. (itrlrb(5)==4) ) iprc = 2
         IF ( itrlra(5)>=3 ) ityp = 2
         IF ( itrlrb(5)>=3 ) ityp = 2
         itype = iprc + ityp
         iform = 6
         CALL makmcb(itrlrc,m,itrlr1(3),iform,itype)
         jscr(1) = iscr(7)
         jscr(2) = iscr(8)
         jscr(3) = iscr(6)
         icode = 0
         prec = 0
         dblkor = (korbgn/2) + 1
         lkore = lstzwd - (2*dblkor-1)
         CALL mpy3dr(dz(dblkor))
         CALL wrttrl(itrlrc)
         CALL sofopn(z(sbuf1),z(sbuf2),z(sbuf3))
         RETURN
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
END SUBROUTINE mred2o
