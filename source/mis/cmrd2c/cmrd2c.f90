!*==cmrd2c.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmrd2c(Iter)
USE C_BLANK
USE C_CDCMPX
USE C_FBSX
USE C_GFBSX
USE C_SFACT
USE C_SYSTEM
USE C_TRNSPX
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iter
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: dblkor , gib , him , i , ifile , imsg , iprc , item , itest , ityp , itype , j , kbi , kib , kii , lii , uii
   REAL(REAL64) , DIMENSION(1) :: dz
   REAL :: gibbar
   INTEGER , DIMENSION(3) , SAVE :: itmlst
   INTEGER , DIMENSION(7) :: itrlr
   INTEGER , SAVE :: lower , upper
   INTEGER , DIMENSION(2) , SAVE :: modnam
   LOGICAL :: restor , symtry
   EXTERNAL cdcomp , fbs , gfbs , makmcb , mesage , mtrxi , mtrxo , rdtrl , sdcomp , smsg , sofcls , sofopn , trnsp , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE PERFORMS THE GUYAN REDUCTION ON THE STRUCTURE
!     POINTS FOR THE CMRED2 MODULE.
!
!     INPUT  DATA
!     GINO - KII    - KII PARTITION MATRIX
!            KIB    - KIB KIB PARTITION MATRIX
!     SOF  - GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS OF
!                      ORIGINAL SUBSTRUCTURE
!
!     OUTPUT DATA
!     SOF  - LMTX   - LII PARTITION MATRIX
!            GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS OF
!                    ORIGINAL SUBSTRUCTURE
!
!     PARAMETERS
!     INPUT- GBUF   - GINO BUFFER
!            ISCR   - SCRATCH FILE NUMBER ARRAY
!            KORLEN - LENGTH OF OPEN CORE
!            KORBGN - BEGINNING ADDRESS OF OPEN CORE
!            OLDNAM - NAME OF SUBSTRUCTURE BEGING REDUCED
!            RSAVE  - DECOMPOSITION SAVE FLAG
!     OTHERS-KII    - KII PARTITION MATRIX FILE NUMBER
!            LII    - LII PARTITION MATRIX FILE NUMBER
!            SYMTRY - KII SYMMETRY FLAG
!
   !>>>>EQUIVALENCE (Kib,Iscr(2)) , (Kbi,Iscr(3)) , (Kii,Iscr(4)) , (Lii,Iscr(8)) , (Uii,Iscr(9)) , (Him,Iscr(10)) , (Gib,Iscr(11)) ,    &
!>>>>    & (Dz(1),Z(1))
   DATA modnam/4HCMRD , 4H2C  /
   DATA lower , upper/4 , 5/
   DATA itmlst/4HLMTX , 4HGIMS , 4HHORG/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     PREFORM GUYAN REDUCTION
!
         IF ( Dry==-2 ) RETURN
         restor = .FALSE.
!
!     TRANSPOSE KII, KBI
!
         IF ( Iter==1 ) THEN
!
!     DECOMPOSE INTERIOR STIFFNESS MATRIX
!        (SYMMETRIC)
!
!                                 T
!        **   **   **   ** **   **
!        *     *   *     * *     *
!        * KII * = * LII * * LII *
!        *     *   *     * *     *
!        **   **   **   ** **   **
!
            CALL sofcls
            Kiit(1) = kii
            CALL rdtrl(Kiit)
            IF ( Kiit(4)/=6 ) THEN
!
!     DECOMPOSE INTERIOR STIFFNESS MATRIX
!        (UNSYMMETRIC)
!
!        **   **   **   ** **   **
!        *     *   *     * *     *
!        * KII * = * LII * * UII *
!        *     *   *     * *     *
!        **   **   **   ** **   **
!
               symtry = .FALSE.
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSE
               symtry = .TRUE.
               iprc = 1
               ityp = 0
               IF ( Kiit(5)==2 .OR. Kiit(5)==4 ) iprc = 2
               IF ( Kiit(5)>=3 ) ityp = 2
               itype = iprc + ityp
               CALL makmcb(Liit,lii,Kiit(3),lower,itype)
               Iscrq(1) = Iscr(5)
               Iscra = Iscr(6)
               Iscrb = Iscr(7)
               Iscrc = Iscr(9)
               Chlsky = 0
               Power = 1
               dblkor = (Korbgn/2) + 1
               Nzsf = Lstzwd - ((2*dblkor)-1)
               CALL sdcomp(*20,dz(dblkor),dz(dblkor),dz(dblkor))
               CALL wrttrl(Liit)
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( symtry ) THEN
!
!     KII SYMMETRIC, GIBBAR = GIB
!
            item = itmlst(2)
            CALL mtrxi(gibbar,Oldnam,item,0,itest)
            IF ( itest/=1 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            RETURN
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         dblkor = (Korbgn/2) + 1
         Lcore = Lstzwd - ((2*dblkor)-1)
         Nscrth = 5
         DO i = 1 , Nscrth
            Iscrth(i) = Iscr(4+i)
         ENDDO
         DO i = 1 , 2
            itrlr(1) = kii
            IF ( i==2 ) itrlr(1) = kbi
            CALL rdtrl(itrlr)
            DO j = 1 , 7
               Atrlr(j) = itrlr(j)
               Attrlr(j) = itrlr(j)
            ENDDO
            Attrlr(2) = itrlr(3)
            Attrlr(3) = itrlr(2)
            CALL trnsp(dz(dblkor))
            CALL wrttrl(Attrlr)
         ENDDO
         IF ( restor ) CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
         IF ( restor ) RETURN
         restor = .TRUE.
         CALL sofcls
         spag_nextblock_1 = 3
      CASE (3)
         Kiitc(1) = kii
         CALL rdtrl(Kiitc)
         ityp = 0
         iprc = 1
         IF ( Kiitc(5)==2 .OR. Kiitc(5)==4 ) iprc = 2
         IF ( Kiitc(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(Liitc,lii,Kiitc(3),lower,itype)
         CALL makmcb(Uiitc,uii,Kiitc(3),upper,itype)
         Scr(1) = Iscr(5)
         Scr(2) = Iscr(6)
         Scr(3) = Iscr(7)
         B = 0
         Bbar = 0
         dblkor = (Korbgn/2) + 1
         Nx = Lstzwd - ((2*dblkor)-1)
         CALL cdcomp(*40,dz(dblkor),dz(dblkor),dz(dblkor))
         CALL wrttrl(Liitc)
         CALL wrttrl(Uiitc)
         spag_nextblock_1 = 4
      CASE (4)
!
!     SAVE LII AS LMTX ON SOF
!
         IF ( .NOT.(Iter==2 .OR. .NOT.Rsave) ) THEN
            CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
            ifile = lii
            CALL mtrxo(lii,Oldnam,itmlst(1),0,itest)
            item = itmlst(1)
            IF ( itest/=3 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL sofcls
         ENDIF
!
!     SOLVE STRUCTURE REDUCTION TRANSFORMATION MATRIX
!        (SYMMETRIC)
!
!                       T
!        **   ** **   ** **   **    **   **
!        *     * *     * *     *    *     *
!        * LII * * LII * * GIB * = -* KIB *
!        *     * *     * *     *    *     *
!        **   ** **   ** **   **    **   **
!
         IF ( .NOT.symtry ) THEN
!
!     SOLVE STRUCTURE REDUCTION TRANSFORMATION MATRIX
!        (UNSYMMETRIC)
!
!        **   ** **   ** **   **    **   **
!        *     * *     * *     *    *     *
!        * LII * * UII * * GIB * = -* KIB *
!        *     * *     * *     *    *     *
!        **   ** **   ** **   **    **   **
!
            Kigfbs(1) = kib
            IF ( Iter==2 ) Kigfbs(1) = kbi
            CALL rdtrl(Kigfbs)
            DO i = 1 , 7
               Ligfbs(i) = Liitc(i)
               Ugfbs(i) = Uiitc(i)
            ENDDO
            iprc = 1
            ityp = 0
            IF ( Kigfbs(5)==2 .OR. Kigfbs(5)==4 ) iprc = 2
            IF ( Liitc(5)==2 .OR. Liitc(5)==4 ) iprc = 2
            IF ( Uiitc(5)==2 .OR. Uiitc(5)==4 ) iprc = 2
            IF ( Kigfbs(5)>=3 ) ityp = 2
            IF ( Liitc(5)>=3 ) ityp = 2
            IF ( Uiitc(5)>=3 ) ityp = 2
            itype = iprc + ityp
            CALL makmcb(Gibfbs,gib,Kigfbs(3),Kigfbs(4),itype)
            Nzgfbs = Lstzwd - ((2*dblkor)-1)
            Prec1 = iprc
            Isign = -1
            CALL gfbs(dz(dblkor),dz(dblkor))
            CALL wrttrl(Gibfbs)
         ELSE
            Kibt(1) = kib
            IF ( Iter==2 ) Kibt(1) = kbi
            CALL rdtrl(Kibt)
            DO i = 1 , 7
               Liifbs(i) = Liit(i)
            ENDDO
            iprc = 1
            ityp = 0
            IF ( Kibt(5)==2 .OR. Kibt(5)==4 ) iprc = 2
            IF ( Liit(5)==2 .OR. Liit(5)==4 ) iprc = 2
            IF ( Kibt(5)>=3 ) ityp = 2
            IF ( Liit(5)>=3 ) ityp = 2
            itype = iprc + ityp
            CALL makmcb(Gibt,gib,Kibt(3),Kibt(4),itype)
            Nzfbs = Lstzwd - ((2*dblkor)-1)
            Prec = Kibt(5) - 2
            Sign = -1
            CALL fbs(dz(dblkor),dz(dblkor))
            CALL wrttrl(Gibt)
         ENDIF
!
!     SAVE GIB AS GIMS ON SOF
!
         IF ( restor ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Iter==2 ) RETURN
         CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
         ifile = gib
         CALL mtrxo(gib,Oldnam,itmlst(2),0,itest)
         item = itmlst(2)
         IF ( itest/=3 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         RETURN
!
!     PROCESS SYSTEM FATAL ERRORS
!
 20      WRITE (Iprntr,99001) Uwm , Oldnam
99001    FORMAT (A25,' 6311, SDCOMP DECOMPOSITION FAILED ON KII MATRIX ','FOR SUBSTRUCTURE ',2A4)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 40      WRITE (Iprntr,99002) Uwm , Oldnam
99002    FORMAT (A23,' 6635, CDCOMP DECOMPOSITION FAILED ON KII MATRIX ','FOR SUBSTRUCTURE ',2A4)
         spag_nextblock_1 = 5
      CASE (5)
         imsg = -37
         ifile = 0
         CALL sofcls
         CALL mesage(imsg,ifile,modnam)
         RETURN
      CASE (6)
!
!     PROCESS MODULE FATAL ERRORS
!
         IF ( itest==2 ) THEN
!
            WRITE (Iprntr,99003) Ufm , modnam , item , Oldnam
99003       FORMAT (A23,' 6215, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' PSEUDO-EXISTS ONLY.')
            Dry = -2
         ELSEIF ( itest==3 ) THEN
!
            imsg = -1
            CALL smsg(imsg,item,Oldnam)
         ELSEIF ( itest==4 ) THEN
            imsg = -2
            CALL smsg(imsg,item,Oldnam)
         ELSEIF ( itest==5 ) THEN
            imsg = -3
            CALL smsg(imsg,item,Oldnam)
         ELSEIF ( itest==6 ) THEN
!
            WRITE (Iprntr,99004) Ufm , modnam , item , Oldnam
99004       FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',' OF SOF ITEM ',A4,', SUBSTRUCTURE ',2A4,             &
                   &', IS PURGED.')
            Dry = -2
         ELSE
            WRITE (Iprntr,99005) Ufm , modnam , item , Oldnam
!
99005       FORMAT (A23,' 6211, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
            Dry = -2
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE cmrd2c
