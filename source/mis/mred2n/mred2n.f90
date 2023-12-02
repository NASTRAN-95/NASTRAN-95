!*==mred2n.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2n
USE C_BLANK
USE C_CONDAS
USE C_MPY3TL
USE C_PACKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
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
         IF ( Dry==-2 ) RETURN
         IF ( Korbgn+7+Nmodes>=Korlen ) THEN
            imsg = -8
            ifile = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            ifile = lamamr
            CALL gopen(lamamr,Z(Gbuf1),0)
            CALL fwdrec(*40,lamamr)
            iform = 3
            itype = 1
            CALL makmcb(itrlr1,kmw2,Nmodes,iform,itype)
            Typin = 1
            Typout = 1
            Irow = 1
            Nrow = Nmodes
            Incr = 1
            CALL gopen(kmw2,Z(Gbuf2),1)
            DO i = 1 , Nmodes
               CALL read(*20,*40,lamamr,Z(Korbgn),7,0,nwdsrd)
               DO j = 1 , Nmodes
                  rz(Korbgn+7+j-1) = 0.0
                  IF ( j==i ) rz(Korbgn+7+j-1) = Forpi2*rz(Korbgn+5)*(rz(Korbgn+4)**2)
               ENDDO
               CALL pack(Z(Korbgn+7),kmw2,itrlr1)
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
               Itrlra(i) = itrlr2(i)
               Itrlrb(i) = itrlr1(i)
               Itrlre(i) = 0
            ENDDO
            iprc = 1
            ityp = 0
            IF ( (Itrlra(5)==2) .OR. (Itrlra(5)==4) ) iprc = 2
            IF ( (Itrlrb(5)==2) .OR. (Itrlrb(5)==4) ) iprc = 2
            IF ( Itrlra(5)>=3 ) ityp = 2
            IF ( Itrlrb(5)>=3 ) ityp = 2
            itype = iprc + ityp
            iform = 6
            CALL makmcb(Itrlrc,k,itrlr2(3),iform,itype)
            Jscr(1) = Iscr(8)
            Jscr(2) = Iscr(6)
            Jscr(3) = Iscr(2)
            Icode = 0
            Prec = 0
            dblkor = (Korbgn/2) + 1
            Lkore = Lstzwd - (2*dblkor-1)
            CALL sofcls
            CALL mpy3dr(dz(dblkor))
            CALL wrttrl(Itrlrc)
            CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
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
