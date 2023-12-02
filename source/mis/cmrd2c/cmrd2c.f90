!*==cmrd2c.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmrd2c(Iter)
   USE c_blank
   USE c_cdcmpx
   USE c_fbsx
   USE c_gfbsx
   USE c_sfact
   USE c_system
   USE c_trnspx
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
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
         IF ( dry==-2 ) RETURN
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
            kiit(1) = kii
            CALL rdtrl(kiit)
            IF ( kiit(4)/=6 ) THEN
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
            ELSE
               symtry = .TRUE.
               iprc = 1
               ityp = 0
               IF ( kiit(5)==2 .OR. kiit(5)==4 ) iprc = 2
               IF ( kiit(5)>=3 ) ityp = 2
               itype = iprc + ityp
               CALL makmcb(liit,lii,kiit(3),lower,itype)
               iscrq(1) = iscr(5)
               iscra = iscr(6)
               iscrb = iscr(7)
               iscrc = iscr(9)
               chlsky = 0
               power = 1
               dblkor = (korbgn/2) + 1
               nzsf = lstzwd - ((2*dblkor)-1)
               CALL sdcomp(*20,dz(dblkor),dz(dblkor),dz(dblkor))
               CALL wrttrl(liit)
               spag_nextblock_1 = 4
            ENDIF
            CYCLE
         ELSEIF ( symtry ) THEN
!
!     KII SYMMETRIC, GIBBAR = GIB
!
            item = itmlst(2)
            CALL mtrxi(gibbar,oldnam,item,0,itest)
            IF ( itest/=1 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            RETURN
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         dblkor = (korbgn/2) + 1
         lcore = lstzwd - ((2*dblkor)-1)
         nscrth = 5
         DO i = 1 , nscrth
            iscrth(i) = iscr(4+i)
         ENDDO
         DO i = 1 , 2
            itrlr(1) = kii
            IF ( i==2 ) itrlr(1) = kbi
            CALL rdtrl(itrlr)
            DO j = 1 , 7
               atrlr(j) = itrlr(j)
               attrlr(j) = itrlr(j)
            ENDDO
            attrlr(2) = itrlr(3)
            attrlr(3) = itrlr(2)
            CALL trnsp(dz(dblkor))
            CALL wrttrl(attrlr)
         ENDDO
         IF ( restor ) CALL sofopn(z(sbuf1),z(sbuf2),z(sbuf3))
         IF ( restor ) RETURN
         restor = .TRUE.
         CALL sofcls
         spag_nextblock_1 = 3
      CASE (3)
         kiitc(1) = kii
         CALL rdtrl(kiitc)
         ityp = 0
         iprc = 1
         IF ( kiitc(5)==2 .OR. kiitc(5)==4 ) iprc = 2
         IF ( kiitc(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(liitc,lii,kiitc(3),lower,itype)
         CALL makmcb(uiitc,uii,kiitc(3),upper,itype)
         scr(1) = iscr(5)
         scr(2) = iscr(6)
         scr(3) = iscr(7)
         b = 0
         bbar = 0
         dblkor = (korbgn/2) + 1
         nx = lstzwd - ((2*dblkor)-1)
         CALL cdcomp(*40,dz(dblkor),dz(dblkor),dz(dblkor))
         CALL wrttrl(liitc)
         CALL wrttrl(uiitc)
         spag_nextblock_1 = 4
      CASE (4)
!
!     SAVE LII AS LMTX ON SOF
!
         IF ( .NOT.(Iter==2 .OR. .NOT.rsave) ) THEN
            CALL sofopn(z(sbuf1),z(sbuf2),z(sbuf3))
            ifile = lii
            CALL mtrxo(lii,oldnam,itmlst(1),0,itest)
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
            kigfbs(1) = kib
            IF ( Iter==2 ) kigfbs(1) = kbi
            CALL rdtrl(kigfbs)
            DO i = 1 , 7
               ligfbs(i) = liitc(i)
               ugfbs(i) = uiitc(i)
            ENDDO
            iprc = 1
            ityp = 0
            IF ( kigfbs(5)==2 .OR. kigfbs(5)==4 ) iprc = 2
            IF ( liitc(5)==2 .OR. liitc(5)==4 ) iprc = 2
            IF ( uiitc(5)==2 .OR. uiitc(5)==4 ) iprc = 2
            IF ( kigfbs(5)>=3 ) ityp = 2
            IF ( liitc(5)>=3 ) ityp = 2
            IF ( uiitc(5)>=3 ) ityp = 2
            itype = iprc + ityp
            CALL makmcb(gibfbs,gib,kigfbs(3),kigfbs(4),itype)
            nzgfbs = lstzwd - ((2*dblkor)-1)
            prec1 = iprc
            isign = -1
            CALL gfbs(dz(dblkor),dz(dblkor))
            CALL wrttrl(gibfbs)
         ELSE
            kibt(1) = kib
            IF ( Iter==2 ) kibt(1) = kbi
            CALL rdtrl(kibt)
            DO i = 1 , 7
               liifbs(i) = liit(i)
            ENDDO
            iprc = 1
            ityp = 0
            IF ( kibt(5)==2 .OR. kibt(5)==4 ) iprc = 2
            IF ( liit(5)==2 .OR. liit(5)==4 ) iprc = 2
            IF ( kibt(5)>=3 ) ityp = 2
            IF ( liit(5)>=3 ) ityp = 2
            itype = iprc + ityp
            CALL makmcb(gibt,gib,kibt(3),kibt(4),itype)
            nzfbs = lstzwd - ((2*dblkor)-1)
            prec = kibt(5) - 2
            sign = -1
            CALL fbs(dz(dblkor),dz(dblkor))
            CALL wrttrl(gibt)
         ENDIF
!
!     SAVE GIB AS GIMS ON SOF
!
         IF ( restor ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Iter==2 ) RETURN
         CALL sofopn(z(sbuf1),z(sbuf2),z(sbuf3))
         ifile = gib
         CALL mtrxo(gib,oldnam,itmlst(2),0,itest)
         item = itmlst(2)
         IF ( itest/=3 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         RETURN
!
!     PROCESS SYSTEM FATAL ERRORS
!
 20      WRITE (iprntr,99001) uwm , oldnam
99001    FORMAT (A25,' 6311, SDCOMP DECOMPOSITION FAILED ON KII MATRIX ','FOR SUBSTRUCTURE ',2A4)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 40      WRITE (iprntr,99002) uwm , oldnam
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
            WRITE (iprntr,99003) ufm , modnam , item , oldnam
99003       FORMAT (A23,' 6215, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' PSEUDO-EXISTS ONLY.')
            dry = -2
         ELSEIF ( itest==3 ) THEN
!
            imsg = -1
            CALL smsg(imsg,item,oldnam)
         ELSEIF ( itest==4 ) THEN
            imsg = -2
            CALL smsg(imsg,item,oldnam)
         ELSEIF ( itest==5 ) THEN
            imsg = -3
            CALL smsg(imsg,item,oldnam)
         ELSEIF ( itest==6 ) THEN
!
            WRITE (iprntr,99004) ufm , modnam , item , oldnam
99004       FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',' OF SOF ITEM ',A4,', SUBSTRUCTURE ',2A4,             &
                   &', IS PURGED.')
            dry = -2
         ELSE
            WRITE (iprntr,99005) ufm , modnam , item , oldnam
!
99005       FORMAT (A23,' 6211, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
            dry = -2
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE cmrd2c
