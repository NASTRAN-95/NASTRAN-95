
SUBROUTINE mred2d
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bbzero , Cprtn , Dry , Gbuf1 , Idum1 , Idum2(5) , Idum3(2) , Idum4(4) , Idum5(5) , Idum6(10) , Idum7 , Incr , Infile(12) &
         & , Iprntr , Irow , Iscr(10) , Itwo(32) , K , Kbb , Khh , Korbgn , Korlen , M , Mbb , Mhh , Newnam(2) , Nmodes , Nrow ,    &
         & Oldnam(2) , Otfile(6) , Paa , Phh , Popt , Rprtn , Sbuf1 , Sbuf2 , Sbuf3 , Typin , Typout , Ua , Ub , Uf , Ul , Un , Us ,&
         & Usetmr , Usrmod , Z(1) , Zero
   REAL Rz(1)
   CHARACTER*23 Ufm
   COMMON /bitpos/ Idum5 , Ul , Ua , Uf , Us , Un , Idum6 , Ub
   COMMON /blank / Idum1 , Dry , Popt , Gbuf1 , Idum3 , Sbuf1 , Sbuf2 , Sbuf3 , Infile , Otfile , Iscr , Korlen , Korbgn , Oldnam , &
                 & Newnam , Idum4 , Usrmod , Idum2 , Nmodes
   COMMON /packx / Typin , Typout , Irow , Nrow , Incr
   COMMON /system/ Idum7 , Iprntr
   COMMON /two   / Itwo
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   REAL block(11) , ufbits , zero1
   INTEGER i , ifile , iform , ii , imsg , iop , isub(4) , item , itest , itmlst(6) , itmnam(2) , itrlr(7) , itrlr1(7) , itrlr2(7) ,&
         & itype , j , jrow , km , kmbb , kmhh , kolmns , kolumn , lafnb , luset , modnam(2) , mred2 , n2 , nuf , nus , nwdsrd ,    &
         & papp , snb , typea , typeb
!
! End of declarations
!
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
   EQUIVALENCE (Usetmr,Infile(5)) , (Kbb,Infile(6)) , (Mbb,Infile(7)) , (Paa,Infile(10)) , (Khh,Otfile(1)) , (Mhh,Otfile(2)) ,      &
    & (Phh,Otfile(5)) , (Rprtn,Iscr(8)) , (Cprtn,Iscr(8)) , (K,Iscr(3)) , (Bbzero,Iscr(9)) , (M,Iscr(10)) , (Zero,Iscr(3)) ,        &
    & (Rz(1),Z(1)) , (typea,block(1)) , (typeb,block(7))
   DATA modnam/4HMRED , 4H2D  /
   DATA papp/4HPAPP/
   DATA mred2/27/
   DATA itmlst/4HKMTX , 4HMMTX , 4HPVEC , 4HPAPP , 4HPOVE , 4HPOAP/
!
!     CHECK USERMODE OPTION FLAG
!
   IF ( Dry==-2 ) GOTO 99999
!
!     COUNT NUMBER OF FREE, FIXED POINTS WITHIN BOUNDARY SET
!
   itrlr(1) = Usetmr
   CALL rdtrl(itrlr)
   ifile = Usetmr
   IF ( itrlr(1)<0 ) GOTO 200
   luset = itrlr(3)
   IF ( (Korbgn+luset)>=Korlen ) THEN
      imsg = -8
      ifile = 0
      GOTO 300
   ELSE
      CALL gopen(Usetmr,Z(Gbuf1),0)
      CALL read(*100,*200,Usetmr,Z(Korbgn),luset,0,nwdsrd)
      CALL close(Usetmr,1)
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
      IF ( Dry==-2 ) GOTO 99999
      CALL setlvl(Newnam,1,Oldnam,itest,mred2)
      IF ( itest==8 ) THEN
         WRITE (Iprntr,99001) Ufm
99001    FORMAT (A23,' 6518, ONE OF THE COMPONENT SUBSTRUCTURES HAS BEEN ','USED IN A PREVIOUS COMBINE OR REDUCE.')
         Dry = -2
         GOTO 99999
      ELSE
         DO ii = 1 , 2
            itrlr1(1) = Kbb
            km = K
            kmhh = Khh
            IF ( ii/=1 ) THEN
               itrlr1(1) = Mbb
               km = M
               kmhh = Mhh
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
                  Rz(Korbgn+i-1) = 0.0
                  IF ( i>jrow ) Rz(Korbgn+i-1) = 1.0
               ENDDO
               iform = 7
               Typin = 1
               Typout = 1
               Irow = 1
               Incr = 1
               CALL makmcb(itrlr1,Rprtn,Nrow,iform,Typin)
               CALL gopen(Rprtn,Z(Gbuf1),1)
               CALL pack(Z(Korbgn),Rprtn,itrlr1)
               CALL close(Rprtn,1)
               CALL wrttrl(itrlr1)
!
!     MERGE (K,M)BB MATRIX WITH ZERO MATRICES
!
               isub(1) = kolumn
               isub(2) = kolmns - kolumn
               isub(3) = jrow
               isub(4) = Nrow - jrow
               itype = 1
               CALL gmmerg(Bbzero,kmbb,0,0,0,Rprtn,Rprtn,isub,itype,Z(Korbgn),Korlen)
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
               CALL ssg2c(km,Bbzero,kmhh,iop,block)
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
            IF ( itest/=3 ) GOTO 400
         ENDDO
!
!     PROCESS LOAD DATA
!
         itrlr1(1) = Paa
         CALL rdtrl(itrlr1)
         IF ( itrlr1(1)<0 ) GOTO 99999
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
         CALL makmcb(itrlr2,Cprtn,Nrow,iform,Typin)
         DO i = 1 , Nrow
            Rz(Korbgn+i-1) = 0.0
            IF ( i>itrlr1(3) ) Rz(Korbgn+i-1) = 1.0
         ENDDO
         CALL gopen(Cprtn,Z(Gbuf1),1)
         CALL pack(Z(Korbgn),Cprtn,itrlr2)
         CALL close(Cprtn,1)
         CALL wrttrl(itrlr2)
!
!     MERGE PAA WITH ZERO MATRIX
!
         isub(3) = itrlr1(3)
         isub(4) = n2
         IF ( n2==0 ) isub(4) = Nmodes - nuf
         itype = 1
         CALL gmmerg(Phh,Paa,0,0,0,0,Cprtn,isub,itype,Z(Korbgn),Korlen)
!
!     SAVE PHH AS PVEC OR PAPP ON SOF
!
         item = itmlst(3)
         IF ( Popt==papp ) item = itmlst(4)
         itmnam(1) = Newnam(1)
         itmnam(2) = Newnam(2)
         CALL mtrxo(Phh,Newnam,item,0,itest)
         IF ( itest/=3 ) GOTO 400
!
!     STORE NULL MATRIX AS POVE OR POAP ON SOF
!
         iform = 2
         kolmns = itrlr1(2)
         Nrow = n2
         IF ( n2==0 ) Nrow = Nmodes - nuf
         CALL makmcb(itrlr2,Zero,Nrow,iform,Typin)
         CALL gopen(Zero,Z(Gbuf1),1)
         DO i = 1 , kolmns
            DO j = 1 , Nrow
               Rz(Korbgn+j-1) = 0.0
            ENDDO
            CALL pack(Z(Korbgn),Zero,itrlr2)
         ENDDO
         CALL close(Zero,1)
         CALL wrttrl(itrlr2)
         item = itmlst(5)
         IF ( Popt==papp ) item = itmlst(6)
         itmnam(1) = Oldnam(1)
         itmnam(2) = Oldnam(2)
         CALL mtrxo(zero1,Oldnam,item,0,itest)
         IF ( itest==3 ) GOTO 99999
         GOTO 400
      ENDIF
   ENDIF
!
!     PROCESS SYSTEM FATAL ERRORS
!
 100  imsg = -2
   GOTO 300
 200  imsg = -3
 300  CALL sofcls
   CALL mesage(imsg,ifile,modnam)
   GOTO 99999
!
!     PROCESS MODULE FATAL ERRORS
!
 400  IF ( itest==2 ) THEN
      imsg = -11
   ELSEIF ( itest==3 ) THEN
      imsg = -1
      CALL smsg(imsg,item,itmnam)
      GOTO 99999
   ELSEIF ( itest==4 ) THEN
      imsg = -2
      CALL smsg(imsg,item,itmnam)
      GOTO 99999
   ELSEIF ( itest==5 ) THEN
      imsg = -3
      CALL smsg(imsg,item,itmnam)
      GOTO 99999
   ELSEIF ( itest==6 ) THEN
      imsg = -10
   ELSE
      imsg = -9
   ENDIF
   Dry = -2
   CALL smsg1(imsg,item,itmnam,modnam)
99999 RETURN
END SUBROUTINE mred2d
