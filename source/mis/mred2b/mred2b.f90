!*==mred2b.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2b
   USE c_blank
   USE c_fbsx
   USE c_sfact
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
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
         IF ( dry==-2 ) RETURN
         IF ( bounds ) THEN
            itrlr(1) = dmr
            CALL rdtrl(itrlr)
            IF ( itrlr(1)<0 ) RETURN
            item = itmlst(1)
            CALL softrl(oldnam,item,itrlr)
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
         kiit(1) = kii
         CALL rdtrl(kiit)
         CALL makmcb(liit,lii,kiit(3),lower,kiit(5))
         iscrq(1) = iscr(6)
         iscra = iscr(7)
         iscrb = iscr(8)
         iscrc = iscr(9)
         power = 1
         chlsky = 0
         dblkor = 1 + korbgn/2
         nzsf = lstzwd - 2*dblkor - 1
         CALL sdcomp(*20,dz(dblkor),dz(dblkor),dz(dblkor))
         CALL wrttrl(liit)
!
!     SAVE LII AS LMTX ON SOF
!
         IF ( rsave ) THEN
            CALL sofopn(z(sbuf1),z(sbuf2),z(sbuf3))
            ifile = lii
            item = itmlst(1)
            CALL mtrxo(lii,oldnam,item,0,itest)
            IF ( itest/=3 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( bounds ) RETURN
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
         IF ( bounds ) THEN
            CALL sofopn(z(sbuf1),z(sbuf2),z(sbuf3))
            RETURN
         ELSE
            kibt(1) = kib
            CALL rdtrl(kibt)
            DO i = 1 , 7
               liifbs(i) = liit(i)
            ENDDO
            CALL makmcb(gibt,gib,kibt(3),kibt(4),kibt(5))
            nzfbs = lstzwd - 2*dblkor
            prec = kibt(5)
            sign = -1
            CALL fbs(dz(dblkor),dz(dblkor))
            CALL wrttrl(gibt)
!
!     SAVE GIB AS GIMS ON SOF
!
            CALL sofopn(z(sbuf1),z(sbuf2),z(sbuf3))
            ifile = gib
            item = itmlst(2)
            CALL mtrxo(gib,oldnam,item,0,itest)
            IF ( itest/=3 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            RETURN
         ENDIF
!
!     PROCESS SYSTEM FATAL ERRORS
!
 20      WRITE (iprntr,99001) swm , oldnam
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
            CALL smsg(imsg,item,oldnam)
            RETURN
         ELSEIF ( itest==5 ) THEN
            imsg = -3
            CALL smsg(imsg,item,oldnam)
            RETURN
         ELSEIF ( itest==6 ) THEN
            imsg = -10
         ELSE
            imsg = -9
         ENDIF
         dry = -2
         CALL smsg1(imsg,item,oldnam,modsam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE mred2b
