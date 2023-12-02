!*==mred2d.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2d
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_PACKX
   USE C_SYSTEM
   USE C_TWO
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: bbzero , cprtn , i , ifile , iform , ii , imsg , iop , item , itest , itype , j , jrow , k , kbb , khh , km , kmbb ,  &
            & kmhh , kolmns , kolumn , lafnb , luset , m , mbb , mhh , n2 , nuf , nus , nwdsrd , paa , phh , rprtn , snb , typea ,  &
            & typeb , usetmr , zero
   REAL , DIMENSION(11) :: block
   INTEGER , DIMENSION(4) :: isub
   INTEGER , DIMENSION(6) , SAVE :: itmlst
   INTEGER , DIMENSION(2) :: itmnam
   INTEGER , DIMENSION(7) :: itrlr , itrlr1 , itrlr2
   INTEGER , DIMENSION(2) , SAVE :: modnam
   INTEGER , SAVE :: mred2 , papp
   REAL , DIMENSION(1) :: rz
   REAL :: ufbits , zero1
   EXTERNAL close , gmmerg , gopen , makmcb , mesage , mred2i , mred2j , mred2l , mred2m , mred2n , mred2o , mred2p , mtrxo , pack ,&
          & rdtrl , read , setlvl , smsg , smsg1 , sofcls , sofopn , ssg2c , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE CALCULATES THE MODAL MASS AND STIFFNESS MATRICES
!     IF USERMODE = TYPE2 FOR THE MRED2 MODULE.
!
!     INPUT DATA
!     GINO - USETMR  - USET TABLE FOR REDUCED SUBSTRUCTURE
!            LAMAMR  - EIGENVALUE TABLE FOR SUBSTRUCTURE BEING REDUCED
!            PHISS   - EIGENVECTORS FOR SUBSTRUCTURE BEING REDUCED
!            QSM     - MODEL REACTION MATRIX
!            PAA     - SUBSTRUCTURE LOAD MATRIX
!
!     OUTPUT DATA
!     GINO - KHH     - REDUCED STIFFNESS MATRIX
!            MHH     - REDUCED MASS MATRIX
!            PHH     - REDUCED LOAD MATRIX
!     SOF  - HORG    - H TRANSFORMATION MATRIX
!            KMTX    - STIFFNESS MATRIX FOR REDUCED SUBSTRUCTURE
!            MMTX    - MASS MATRIX FOR REDUCED SUBSTRUCTURE
!            PVEC    - LOAD MATRIX FOR REDUCED SUBSTRUCTURE
!            PAPP    - APPENDED LOAD MATRIX FOR REDUCED SUBSTRUCTURE
!            POVE    - INTERNAL POINT LOADS FOR ORIGINAL SUBSTRUCTURE
!            POAP    - INTERNAL POINTS APPENDED LOADS FOR ORIGINAL
!                       SUBSTRUCTURE
!
!     PARAMETERS
!     INPUT - DRY    - MODULE OPERATION FALG
!             GBUF   - GINO BUFFERS
!             INFILE - INPUT FILE NUMBERS
!             OTFILE - OUTPUT FILE NUMBERS
!             ISCR   - SCRATCH FILE NUMBERS
!             KORLEN - LENGTH OF OPEN CORE
!             KORBGN - BEGINNING ADDRESS OF OPEN CORE
!             OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
!             NEWNAM - NAME OF REDUCED SUBSTRUCTURE
!             USRMOD - USERMODE FLAG
!
   !>>>>EQUIVALENCE (Usetmr,Infile(5)) , (Kbb,Infile(6)) , (Mbb,Infile(7)) , (Paa,Infile(10)) , (Khh,Otfile(1)) , (Mhh,Otfile(2)) ,      &
!>>>>    & (Phh,Otfile(5)) , (Rprtn,Iscr(8)) , (Cprtn,Iscr(8)) , (K,Iscr(3)) , (Bbzero,Iscr(9)) , (M,Iscr(10)) , (Zero,Iscr(3)) ,        &
!>>>>    & (Rz(1),Z(1)) , (typea,block(1)) , (typeb,block(7))
   DATA modnam/4HMRED , 4H2D  /
   DATA papp/4HPAPP/
   DATA mred2/27/
   DATA itmlst/4HKMTX , 4HMMTX , 4HPVEC , 4HPAPP , 4HPOVE , 4HPOAP/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CHECK USERMODE OPTION FLAG
!
         IF ( Dry==-2 ) RETURN
!
!     COUNT NUMBER OF FREE, FIXED POINTS WITHIN BOUNDARY SET
!
         itrlr(1) = usetmr
         CALL rdtrl(itrlr)
         ifile = usetmr
         IF ( itrlr(1)<0 ) GOTO 40
         luset = itrlr(3)
         IF ( (Korbgn+luset)>=Korlen ) THEN
            imsg = -8
            ifile = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL gopen(usetmr,Z(Gbuf1),0)
            CALL read(*20,*40,usetmr,Z(Korbgn),luset,0,nwdsrd)
            CALL close(usetmr,1)
            nuf = 0
            nus = 0
            snb = Itwo(Us) + Itwo(Un) + Itwo(Ub)
            lafnb = Itwo(Ul) + Itwo(Ua) + Itwo(Uf) + Itwo(Un) + Itwo(Ub)
            DO i = 1 , luset
               IF ( Z(Korbgn+i-1)==lafnb ) nuf = nuf + 1
               IF ( Z(Korbgn+i-1)==snb ) nus = nus + 1
            ENDDO
!
!     IF FIXED SET, COMPUTE GS MATRIX
!
            IF ( nus/=0 ) CALL mred2i(1,0,0)
!
!     IF FREE SET, PARTITION PHISS
!
            IF ( nuf/=0 ) THEN
               CALL mred2j(nuf,n2)
!
!     FORM HK MATRIX
!
               CALL mred2l(nuf,n2,nus,ufbits)
            ENDIF
            CALL mred2m(nuf,n2,nus)
!
!     COMPUTE K MATRIX
!
            CALL mred2n
!
!     COMPUTE HM MATRIX
!
            CALL mred2o(nus)
!
!     OUTPUT HORG
!
            CALL mred2p(nus,nuf,n2)
!
!     PROCESS STIFFNESS, MASS MATRICES
!     II = 1, PROCESS STIFFNESS MATRIX
!     II = 2, PROCESS MASS MATRIX
!
            IF ( Dry==-2 ) RETURN
            CALL setlvl(Newnam,1,Oldnam,itest,mred2)
            IF ( itest==8 ) THEN
               WRITE (Iprntr,99001) Ufm
99001          FORMAT (A23,' 6518, ONE OF THE COMPONENT SUBSTRUCTURES HAS BEEN ','USED IN A PREVIOUS COMBINE OR REDUCE.')
               Dry = -2
               RETURN
            ELSE
               DO ii = 1 , 2
                  itrlr1(1) = kbb
                  km = k
                  kmhh = khh
                  IF ( ii/=1 ) THEN
                     itrlr1(1) = mbb
                     km = m
                     kmhh = mhh
                  ENDIF
                  kmbb = itrlr1(1)
                  CALL rdtrl(itrlr1)
                  IF ( itrlr1(1)<0 ) THEN
!
!     NO BB MATRIX PARTITION
!
                     kmhh = km
                  ELSE
                     CALL sofcls
!
!     FORM MERGE VECTOR
!
                     jrow = itrlr1(3)
                     kolumn = itrlr1(2)
                     itrlr2(1) = km
                     CALL rdtrl(itrlr2)
                     Nrow = itrlr2(3)
                     kolmns = itrlr2(2)
                     DO i = 1 , Nrow
                        rz(Korbgn+i-1) = 0.0
                        IF ( i>jrow ) rz(Korbgn+i-1) = 1.0
                     ENDDO
                     iform = 7
                     Typin = 1
                     Typout = 1
                     Irow = 1
                     Incr = 1
                     CALL makmcb(itrlr1,rprtn,Nrow,iform,Typin)
                     CALL gopen(rprtn,Z(Gbuf1),1)
                     CALL pack(Z(Korbgn),rprtn,itrlr1)
                     CALL close(rprtn,1)
                     CALL wrttrl(itrlr1)
!
!     MERGE (K,M)BB MATRIX WITH ZERO MATRICES
!
                     isub(1) = kolumn
                     isub(2) = kolmns - kolumn
                     isub(3) = jrow
                     isub(4) = Nrow - jrow
                     itype = 1
                     CALL gmmerg(bbzero,kmbb,0,0,0,rprtn,rprtn,isub,itype,Z(Korbgn),Korlen)
!
!     FORM STIFFNESS, MASS MATRICES
!
!                                  **           **
!                                  *         .   *
!        **       **   **     **   * (K,M)BB . 0 *
!        *         *   *       *   *         .   *
!        * (K,M)HH * = * (K,M) * + *.............*
!        *         *   *       *   *         .   *
!        **       **   **     **   *    0    . 0 *
!                                  *         .   *
!                                  **           **
                     DO i = 1 , 11
                        block(i) = 0.0
                     ENDDO
                     block(2) = 1.0
                     block(8) = 1.0
                     typea = itrlr2(5)
                     typeb = itrlr1(5)
                     iop = 1
                     CALL ssg2c(km,bbzero,kmhh,iop,block)
                     CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
                  ENDIF
!
!     STORE MATRIX ON SOF
!     II = 1, STORE KHH AS KMTX
!     II = 2, STORE MHH AS MMTX
!
                  item = itmlst(ii)
                  itmnam(1) = Newnam(1)
                  itmnam(2) = Newnam(2)
                  CALL mtrxo(kmhh,Newnam,item,0,itest)
                  IF ( itest/=3 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
!
!     PROCESS LOAD DATA
!
               itrlr1(1) = paa
               CALL rdtrl(itrlr1)
               IF ( itrlr1(1)<0 ) RETURN
!
!     EXPAND PAA FOR MODAL DOF
!
!                  **   **
!                  *     *
!        **   **   * PAA *
!        *     *   *     *
!        * PHH * = *.....*
!        *     *   *     *
!        **   **   *  0  *
!                  *     *
!                  **   **
!
               Nrow = itrlr1(3) + n2
               IF ( n2==0 ) Nrow = Nrow + (Nmodes-nuf)
               iform = 7
               Typin = 1
               Typout = 1
               Irow = 1
               Incr = 1
               CALL makmcb(itrlr2,cprtn,Nrow,iform,Typin)
               DO i = 1 , Nrow
                  rz(Korbgn+i-1) = 0.0
                  IF ( i>itrlr1(3) ) rz(Korbgn+i-1) = 1.0
               ENDDO
               CALL gopen(cprtn,Z(Gbuf1),1)
               CALL pack(Z(Korbgn),cprtn,itrlr2)
               CALL close(cprtn,1)
               CALL wrttrl(itrlr2)
!
!     MERGE PAA WITH ZERO MATRIX
!
               isub(3) = itrlr1(3)
               isub(4) = n2
               IF ( n2==0 ) isub(4) = Nmodes - nuf
               itype = 1
               CALL gmmerg(phh,paa,0,0,0,0,cprtn,isub,itype,Z(Korbgn),Korlen)
!
!     SAVE PHH AS PVEC OR PAPP ON SOF
!
               item = itmlst(3)
               IF ( Popt==papp ) item = itmlst(4)
               itmnam(1) = Newnam(1)
               itmnam(2) = Newnam(2)
               CALL mtrxo(phh,Newnam,item,0,itest)
               IF ( itest/=3 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     STORE NULL MATRIX AS POVE OR POAP ON SOF
!
               iform = 2
               kolmns = itrlr1(2)
               Nrow = n2
               IF ( n2==0 ) Nrow = Nmodes - nuf
               CALL makmcb(itrlr2,zero,Nrow,iform,Typin)
               CALL gopen(zero,Z(Gbuf1),1)
               DO i = 1 , kolmns
                  DO j = 1 , Nrow
                     rz(Korbgn+j-1) = 0.0
                  ENDDO
                  CALL pack(Z(Korbgn),zero,itrlr2)
               ENDDO
               CALL close(zero,1)
               CALL wrttrl(itrlr2)
               item = itmlst(5)
               IF ( Popt==papp ) item = itmlst(6)
               itmnam(1) = Oldnam(1)
               itmnam(2) = Oldnam(2)
               CALL mtrxo(zero1,Oldnam,item,0,itest)
               IF ( itest/=3 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
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
         RETURN
      CASE (3)
!
!     PROCESS MODULE FATAL ERRORS
!
         IF ( itest==2 ) THEN
            imsg = -11
         ELSEIF ( itest==3 ) THEN
            imsg = -1
            CALL smsg(imsg,item,itmnam)
            RETURN
         ELSEIF ( itest==4 ) THEN
            imsg = -2
            CALL smsg(imsg,item,itmnam)
            RETURN
         ELSEIF ( itest==5 ) THEN
            imsg = -3
            CALL smsg(imsg,item,itmnam)
            RETURN
         ELSEIF ( itest==6 ) THEN
            imsg = -10
         ELSE
            imsg = -9
         ENDIF
         Dry = -2
         CALL smsg1(imsg,item,itmnam,modnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mred2d
