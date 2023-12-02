!*==mred2b.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2b
USE C_BLANK
USE C_FBSX
USE C_SFACT
USE C_SYSTEM
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: dblkor , dmr , gib , i , ifile , imsg , item , itest , kib , kii , lii , modsam
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(2) , SAVE :: itmlst , modnam
   INTEGER , DIMENSION(7) :: itrlr
   INTEGER , SAVE :: lower
   EXTERNAL fbs , makmcb , mesage , mtrxo , rdtrl , sdcomp , smsg , smsg1 , sofcls , sofopn , softrl , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE PERFORMS THE GUYAN REDUCTION ON THE STRUCTURE
!     POINTS FOR THE MRED2 MODULE.
!
!     INPUT DATA
!     GINO   - KII    - KII PARTITION MATRIX
!     SOF    - GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS OF
!                       ORIGINAL SUBSTRUCTURE
!
!     OUTPUT DATA
!     GINO   - LII    - LII PARTITION MATRIX
!     SOF    - LMTX   - LII PARTITION MATRIX
!              GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS OF
!                       ORIGINAL SUBSTRUCTURE
!
!     PARAMETERS
!     INPUT  - GBUF   - GINO BUFFER
!              ISCR   - SCRATCH FILE NUMBER ARRAY
!              KORLEN - LENGTH OF OPEN CORE
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              OLDNAM - NAME OF SUBSTRUCTURE BEGING REDUCED
!              BOUNDS - OLDBOUNDS OPTION FLAG
!              RSAVE  - DECOMPOSITION SAVE FLAG
!     OTHERS - KIB    - KIB PARTITION MATRIX FILE NUMBER
!              KII    - KII PARTITION MATRIX FILE NUMBER
!              LII    - LII PARTITION MATRIX FILE NUMBER (ISCR11)
!
   !>>>>EQUIVALENCE (Dmr,Infile(11)) , (Gib,Iscr(6)) , (Dz(1),Z(1)) , (Kib,Iscr(2)) , (Kii,Iscr(3)) , (Lii,Iscr11)
   DATA modnam/4HMRED , 4H2B  /
   DATA lower/4/
   DATA itmlst/4HLMTX , 4HGIMS/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     TEST FOR GUYAN REDUCTION
!
         IF ( Dry==-2 ) RETURN
         IF ( Bounds ) THEN
            itrlr(1) = dmr
            CALL rdtrl(itrlr)
            IF ( itrlr(1)<0 ) RETURN
            item = itmlst(1)
            CALL softrl(Oldnam,item,itrlr)
            IF ( itrlr(1)==1 ) RETURN
         ENDIF
!
!     DECOMPOSE INTERIOR STIFFNESS MATRIX
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
         CALL makmcb(Liit,lii,Kiit(3),lower,Kiit(5))
         Iscrq(1) = Iscr(6)
         Iscra = Iscr(7)
         Iscrb = Iscr(8)
         Iscrc = Iscr(9)
         Power = 1
         Chlsky = 0
         dblkor = 1 + Korbgn/2
         Nzsf = Lstzwd - 2*dblkor - 1
         CALL sdcomp(*20,dz(dblkor),dz(dblkor),dz(dblkor))
         CALL wrttrl(Liit)
!
!     SAVE LII AS LMTX ON SOF
!
         IF ( Rsave ) THEN
            CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
            ifile = lii
            item = itmlst(1)
            CALL mtrxo(lii,Oldnam,item,0,itest)
            IF ( itest/=3 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Bounds ) RETURN
            CALL sofcls
         ENDIF
!
!     SOLVE STRUCTURE REDUCTION TRANSFORMATION MATRIX
!
!                       T
!        **   ** **   ** **   **    **   **
!        *     * *     * *     *    *     *
!        * LII * * LII * * GIB * = -* KIB *
!        *     * *     * *     *    *     *
!        **   ** **   ** **   **    **   **
!
         IF ( Bounds ) THEN
            CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
            RETURN
         ELSE
            Kibt(1) = kib
            CALL rdtrl(Kibt)
            DO i = 1 , 7
               Liifbs(i) = Liit(i)
            ENDDO
            CALL makmcb(Gibt,gib,Kibt(3),Kibt(4),Kibt(5))
            Nzfbs = Lstzwd - 2*dblkor
            Prec = Kibt(5)
            Sign = -1
            CALL fbs(dz(dblkor),dz(dblkor))
            CALL wrttrl(Gibt)
!
!     SAVE GIB AS GIMS ON SOF
!
            CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
            ifile = gib
            item = itmlst(2)
            CALL mtrxo(gib,Oldnam,item,0,itest)
            IF ( itest/=3 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            RETURN
         ENDIF
!
!     PROCESS SYSTEM FATAL ERRORS
!
 20      WRITE (Iprntr,99001) Swm , Oldnam
99001    FORMAT (A27,' 6311, SDCOMP DECOMPOSITION FAILED ON KII MATRIX ','FOR SUBSTRUCTURE ',2A4)
         imsg = -37
         ifile = 0
         CALL mesage(imsg,ifile,modnam)
         RETURN
      CASE (2)
!
!     PROCESS MODULE FATAL ERRORS
!
         IF ( itest==4 ) THEN
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
         CALL smsg1(imsg,item,Oldnam,modsam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE mred2b
