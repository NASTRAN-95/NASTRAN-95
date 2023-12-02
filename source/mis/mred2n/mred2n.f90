!*==mred2n.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2n
   USE c_blank
   USE c_condas
   USE c_mpy3tl
   USE c_packx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: dblkor , hk , i , ifile , iform , imsg , iprc , ityp , itype , j , k , kmw2 , lamamr , nwdsrd
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(7) :: itrlr1 , itrlr2
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL , DIMENSION(1) :: rz
   EXTERNAL close , fwdrec , gopen , makmcb , mesage , mpy3dr , pack , rdtrl , read , sofcls , sofopn , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE CALCULATES THE K MATRIX FOR THE MRED2 MODULE.
!
   !>>>>EQUIVALENCE (Lamamr,Infile(2)) , (Rz(1),Z(1)) , (Dz(1),Z(1)) , (Hk,Iscr(2)) , (Kmw2,Iscr(5)) , (K,Iscr(3))
   DATA modnam/4HMRED , 4H2N  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!                    2
!     FORM KMW2 = M W  MATRIX
!                  I I
!
         IF ( dry==-2 ) RETURN
         IF ( korbgn+7+nmodes>=korlen ) THEN
            imsg = -8
            ifile = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
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
                  IF ( j==i ) rz(korbgn+7+j-1) = forpi2*rz(korbgn+5)*(rz(korbgn+4)**2)
               ENDDO
               CALL pack(z(korbgn+7),kmw2,itrlr1)
            ENDDO
            CALL close(lamamr,1)
            CALL close(kmw2,1)
            CALL wrttrl(itrlr1)
!
!     FORM K MATRIX
!
!                      T
!                       **      **
!        ** **   **  ** * .    0 * **  **
!        *   *   *    * *  .     * *    *                  2
!        * K * = * HK * *   K    * * HK *     WHERE K = M W
!        *   *   *    * *    .   * *    *                I I
!        ** **   **  ** * 0   .  * **  **
!                       **      **
!
            itrlr2(1) = hk
            CALL rdtrl(itrlr2)
            DO i = 1 , 7
               itrlra(i) = itrlr2(i)
               itrlrb(i) = itrlr1(i)
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
            CALL makmcb(itrlrc,k,itrlr2(3),iform,itype)
            jscr(1) = iscr(8)
            jscr(2) = iscr(6)
            jscr(3) = iscr(2)
            icode = 0
            prec = 0
            dblkor = (korbgn/2) + 1
            lkore = lstzwd - (2*dblkor-1)
            CALL sofcls
            CALL mpy3dr(dz(dblkor))
            CALL wrttrl(itrlrc)
            CALL sofopn(z(sbuf1),z(sbuf2),z(sbuf3))
            RETURN
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
END SUBROUTINE mred2n
