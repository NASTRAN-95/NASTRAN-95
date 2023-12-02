!*==mpyq.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpyq(Z)
!
!     MPYQ IS CALLED ONCE PER EXECUTION OF MPYAD. IT PERFORMS GENERAL
!     INITIALIZATION FOR EACH OF THE ENTRY POINTS
!     I.E.  SETTING UP MPYAD GO-TO-BRANCHES, ARITH AND BPICK FOR METHOD
!           1, AND ARITH2, APICK2 AND BPICK2 FOR METHOD 2
!
!     ENTRY POINTS -
!           MPY1V   (PERFORMS THE INNER LOOP FOR MPYAD, METHOD 1,
!                    TRANSPOSE AND NON-TRANSPOSE. IT IS CALLED ONCE FOR
!                    EACH COLUMNN OF THE A MATRIX)
!           MPY2NV  (PERFORMS THE INNER LOOP FOR THE NON-TRANSPOSE CASE
!                    OF METHOD 2. IT IS CALLED ONCE FOR EACH COUMN OF
!                    THE B MATRIX)
!           MPY2TV  (SAME AS MPY2NV, EXECPT IT IS FOR THE TRANSPOSE
!                    CASE)
!           MPY3T   (PERFORMS THE INNER LOOP FOR THE TRANSPOSE CASE OF
!                    METHOD 3. IT IS CALLED ONCE FOR EACH COLUMN OF THE
!                    B MATRIX)
!     (WHERE V STANDS FOR VAX VERSION, AND T IS THE TRANSPOSE FLAG)
!
!     THE MPYi ROUTINES PERFORM THE MATRIX MULTIPLICATION AND ADDITION
!     FOR THE MPYAD INNER LOOPS
!
!           (+/-)A * B (+/-)C = D   OR (+/-)A(T) * B (+/-)C = D
!
!
!     LAST REVISED BY G.CHAN/UNISYS  1/91
!     (1) MPY3T WAS PREVIOUSLY A .MDS SUBROUTINE. IT IS NOW AN ENTRY
!         POINT IN THIS MPYQ ROUTINE
!         (MPY3T IS AN ENTRY POINT IN MPYQ, IN IBM AND CDC VERSIONS)
!     (2) TO IMPROVE MPYAD INNER LOOP LOGIC FOR THE COMMON CASES
!
!
   USE c_mpyadx
   USE c_mpyadz
   USE c_mpyqt4
   USE c_names
   USE c_packx
   USE c_system
   USE c_type
   USE c_unpakx
   USE c_zblpkx
   USE c_zntpkx
   USE c_zzzzzz
   USE C_MPYADX
   USE C_MPYADZ
   USE C_MPYQT4
   USE C_NAMES
   USE C_PACKX
   USE C_SYSTEM
   USE C_TYPE
   USE C_UNPAKX
   USE C_ZBLPKX
   USE C_ZNTPKX
   USE C_ZZZZZZ
   IMPLICIT NONE
   REAL A(4) , Bbs(17000) , D(4)
   INTEGER Acol , Acol1 , Acoln , Acore , All4 , Apoint , arow , arow1 , arown , b3flag , Bcol , Block(20) , Cls , Clsrew , Crow ,  &
         & Drow , Eol , Eor , Filea(7) , Fileb(7) , Filec(7) , Filed(7) , Firstl , flag , form , I , Incr1 , Incr2 , Incra , ipass ,&
         & j3 , Jb , jb31 , jb3n , Jmax , Jmax1x , Jump4 , Ksystm(65) , Ll , Lll , m , mout , n , Na , Nb , nbrstr , Nbx , Nd ,     &
         & Ndx , nterm3 , Nwda , Nwdb , Nwdd , Nwds(4) , Nz , One1 , One2 , point , Pp1 , Pp2 , Prc(2) , Prca , Prec , Prec1 ,      &
         & Prec4 , q , r , Rc(4) , Rca , Rcb , Rcd , Rd , Rdrew , row , Scrtch , Signab , Signc , sysbuf , T , Time
   DOUBLE PRECISION ad(2) , bbd(1) , dd(2)
   INTEGER typd , Typd1 , type , typea , typeb , Typebd , typec , Typed , Wrt , Wrtrew
   COMMON /mpyadx/ Filea , Fileb , Filec , Filed , Nz , T , Signab , Signc , Prec1 , Scrtch , Time
   COMMON /mpyadz/ Rcb , Rcd , Ll , Lll , Jb , Nbx , Ndx , Jmax1x , Acol , Acol1 , Acoln , Acore , Apoint , Bcol , Crow , Firstl ,  &
                 & Na , Nb , Nd , Nwda , Nwdb , Nwdd , Prec , Jmax , Incra , Block
   COMMON /mpyqt4/ Rca , Prca , All4 , Jump4 , Prec4
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /packx / Typed , Typd1 , One1 , Pp1 , Incr1
   COMMON /system/ Ksystm
   COMMON /type  / Prc , Nwds , Rc
   COMMON /unpakx/ Typebd , One2 , Pp2 , Incr2
   COMMON /zblpkx/ D , Drow
   COMMON /zntpkx/ A , I , Eol , Eor
   COMMON /zzzzzz/ Bbs
   DOUBLE PRECISION Aad(1) , Ddd(1) , Zd(1)
   REAL Aas(1) , Dds(1) , Z(1)
   INTEGER Zz(1)
   REAL aa(4) , aaa , b(4) , bbb , bsi , bsr
   DOUBLE PRECISION add(2) , bd(2) , bdi , bdr
   INTEGER all , apick2 , arith , arith2 , bpick , bpick2 , buf1 , fa3 , i1 , in , inca , init , irow , j , jb3 , k , k1 , k2 , k3 ,&
         & l , mask6f , moda , modb , nbr , nbr1 , nrow , nzz
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Mout) , (Ksystm(58),Ipass)
   !>>>>EQUIVALENCE (A(1),Ad(1)) , (b(1),bd(1)) , (D(1),Dd(1)) , (Filea(2),M) , (Filea(3),N) , (Filea(5),Typea) , (Fileb(2),Q) ,         &
!>>>>    & (Fileb(3),R) , (Fileb(5),Typeb) , (Filec(5),Typec) , (Filed(5),Typd) , (nzz,buf1) , (Acoln,Arown) , (aa(1),add(1)) ,          &
!>>>>    & (Acol1,Arow1) , (Acol,Arow) , (Bbs(1),Bbd(1))
   !>>>>EQUIVALENCE (Block(2),Type) , (Block(3),Form) , (Block(4),Row,J3) , (Block(5),Point) , (Block(6),Nbrstr) , (Block(8),Flag)
   !>>>>EQUIVALENCE (Block(5),Jb31) , (Block(6),Nterm3) , (Block(7),Jb3n) , (Block(8),B3flag)
!     DATA    MASK6F / X'00FFFFFF'  /
   DATA mask6f/16777215/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     MASK6F= '00FFFFFF'X (OR X'00FFFFFF') = 16777215
!     RCB   = 1 IF B IS REAL,2 IF B IS COMPLEX
!     NWDB  = NUMBER OF WORDS PER ELEMENT OF B
!     NBX   = NUMBER OF ELEMENTS PER COLUMN OF B
!     NB    = NUMBER OF WORDS PER COLUMN OF B
!     NDX   = NUMBER OF ELEMENTS PER COLUMN OF C AND D
!     ND    = NUMBER OF WORDS PER COLUMN OF C AND D
!     NZZ   = BUF1 = POINTER TO FIRST GINO BUFFER
!     BUF2  = POINTER TO SECOND GINO BUFFER
!     BUF3  = POINTER TO THIRD GINO BUFFER
!     JJ    = MAX. NO. OF COLUMNS OF B AND D THAT MAY BE HELD IN CORE
!     MPASS1= NUMBER OF PASSES REQUIRED FOR METHOD ONE
!     JZB   = POINTER TO FIRST ELEMENT OF B FOR SP REFERENCE
!     JZDB  = POINTER TO FIRST ELEMENT OF B FOR DP REFERENCE
!     JB    = POINTER TO FIRST ELEMENT OF B FOR PRECISION OF PROBLEM
!     NWDA  = NUMBER OF WORDS PER ELEMENT OF A
!     NWDD  = NUMBER OF WORDS PER ELEMENT OF D
!     ACORE = POINTER TO FIRST WORD FOR STORAGE OF PACKED COLUMNS
!             OF A MATRIX FOR METHOD TWO
!*****
         Rca = Rc(typea)
         modb = mod(typeb,2)
         moda = mod(typea,2)
         Prca = Prc(typea)
         fa3 = Filea(3)
!
!     IF DIAG 43 IS ON, SKIP ALL (1991) SPEED IMPROVEMENT LOGIC
!     (THIS IS ONLY TEMPORARY)
!
         all = 0
         IF ( typea==typeb .AND. typea==Typed ) all = typea
         All4 = all
         IF ( All4==0 ) All4 = 5
         IF ( Typed>=3 .AND. typea<=2 .AND. typeb<=2 ) All4 = 6
         CALL sswtch(43,j)
         IF ( j==1 ) all = 0
         Jump4 = typeb + (typea-1)*4
         Prec4 = Prec1 - 1
!
!     RCA, PRCA, ALL4 AND JUMP4 ARE USED IN MPY4T
!
         IF ( Typed==2 ) THEN
!
!     REAL DOUBLE PRECISION
!
            ASSIGN 18 TO arith
            ASSIGN 6 TO bpick
            IF ( modb==1 ) ASSIGN 10 TO bpick
            IF ( T/=0 ) THEN
!
               ASSIGN 500 TO arith2
               ASSIGN 240 TO bpick2
               IF ( modb==1 ) ASSIGN 280 TO bpick2
               ASSIGN 380 TO apick2
               IF ( moda==1 ) ASSIGN 420 TO apick2
            ELSE
!
               ASSIGN 178 TO arith2
               ASSIGN 80 TO bpick2
               IF ( modb==1 ) ASSIGN 120 TO bpick2
               ASSIGN 166 TO apick2
               IF ( moda==1 ) ASSIGN 170 TO apick2
            ENDIF
         ELSEIF ( Typed==3 ) THEN
!
!     COMPLEX SINGLE PRECISION
!
            ASSIGN 20 TO arith
            IF ( typeb==3 ) THEN
               ASSIGN 4 TO bpick
            ELSEIF ( typeb==4 ) THEN
               ASSIGN 14 TO bpick
            ELSE
               ASSIGN 2 TO bpick
            ENDIF
            IF ( T/=0 ) THEN
               ASSIGN 520 TO arith2
!
               IF ( typeb==3 ) THEN
                  ASSIGN 220 TO bpick2
               ELSEIF ( typeb==4 ) THEN
                  ASSIGN 320 TO bpick2
               ELSE
                  ASSIGN 200 TO bpick2
               ENDIF
               IF ( typea==3 ) THEN
                  ASSIGN 360 TO apick2
               ELSEIF ( typea==4 ) THEN
                  ASSIGN 460 TO apick2
               ELSE
                  ASSIGN 340 TO apick2
               ENDIF
            ELSE
!
               ASSIGN 180 TO arith2
               IF ( typeb==3 ) THEN
                  ASSIGN 60 TO bpick2
               ELSEIF ( typeb==4 ) THEN
                  ASSIGN 160 TO bpick2
               ELSE
                  ASSIGN 40 TO bpick2
               ENDIF
               IF ( typea==3 ) THEN
                  ASSIGN 164 TO apick2
               ELSEIF ( typea==4 ) THEN
                  ASSIGN 174 TO apick2
               ELSE
                  ASSIGN 162 TO apick2
               ENDIF
            ENDIF
         ELSEIF ( Typed==4 ) THEN
!
!     COMPLEX DOUBLE PRECISION
!
            ASSIGN 22 TO arith
            IF ( typeb==2 ) THEN
               ASSIGN 6 TO bpick
            ELSEIF ( typeb==3 ) THEN
               ASSIGN 12 TO bpick
            ELSEIF ( typeb==4 ) THEN
               ASSIGN 8 TO bpick
            ELSE
               ASSIGN 10 TO bpick
            ENDIF
            IF ( T/=0 ) THEN
!
               ASSIGN 540 TO arith2
               IF ( typeb==2 ) THEN
                  ASSIGN 240 TO bpick2
               ELSEIF ( typeb==3 ) THEN
                  ASSIGN 300 TO bpick2
               ELSEIF ( typeb==4 ) THEN
                  ASSIGN 260 TO bpick2
               ELSE
                  ASSIGN 280 TO bpick2
               ENDIF
               IF ( typea==1 ) THEN
                  IF ( moda==1 ) ASSIGN 420 TO apick2
               ELSEIF ( typea==3 ) THEN
                  ASSIGN 440 TO apick2
               ELSEIF ( typea==4 ) THEN
                  ASSIGN 400 TO apick2
               ELSE
                  ASSIGN 380 TO apick2
               ENDIF
            ELSE
!
               ASSIGN 182 TO arith2
               IF ( typeb==2 ) THEN
                  ASSIGN 80 TO bpick2
               ELSEIF ( typeb==3 ) THEN
                  ASSIGN 140 TO bpick2
               ELSEIF ( typeb==4 ) THEN
                  ASSIGN 100 TO bpick2
               ELSE
                  ASSIGN 120 TO bpick2
               ENDIF
               IF ( typea==1 ) THEN
                  IF ( moda==1 ) ASSIGN 170 TO apick2
               ELSEIF ( typea==3 ) THEN
                  ASSIGN 172 TO apick2
               ELSEIF ( typea==4 ) THEN
                  ASSIGN 168 TO apick2
               ELSE
                  ASSIGN 166 TO apick2
               ENDIF
            ENDIF
         ELSE
!
!     REAL SINGLE PRECISION
!
            ASSIGN 16 TO arith
            ASSIGN 2 TO bpick
            IF ( T/=0 ) THEN
!
               ASSIGN 480 TO arith2
               ASSIGN 200 TO bpick2
               ASSIGN 340 TO apick2
            ELSE
!
               ASSIGN 176 TO arith2
               ASSIGN 40 TO bpick2
               ASSIGN 162 TO apick2
            ENDIF
         ENDIF
!
!     MPYQ INITIALIZATION DONE
!
         RETURN
!
!
         ENTRY mpy1v(Zz,Z,Zd)
!     =====================
!
!     METHOD 1  (TRANSPOSE AND NON-TRANSPOSE)
!
         b(2) = 0.
         bd(2) = 0.D0
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            CALL zntpki
            i1 = I - 1
            IF ( T/=0 ) THEN
               k1 = i1*Rcb + Jb
               k2 = Lll
            ELSE
               k1 = Ll
               k2 = i1*Rcd + 1
            ENDIF
            k3 = k2 + Jmax1x
            IF ( all/=0 ) THEN
               IF ( all==1 ) THEN
!
!     COMMON CASES (TYPEA=TYPEB=TYPED=PREC)
!
!     PREC=1, ARITH(840) AND BPICK(750)
!     PREC=2, ARITH(850) AND BPICK(770)
!     PREC=3, ARITH(860) AND BPICK(760)
!     PREC=4, ARITH(870) AND BPICK(780)
!
                  DO k = k2 , k3 , Ndx
                     Z(k) = Z(k) + A(1)*Z(k1)
                     k1 = k1 + Nbx
                  ENDDO
                  IF ( Eol==0 ) CYCLE
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( all==2 ) THEN
                  DO k = k2 , k3 , Ndx
                     Zd(k) = Zd(k) + ad(1)*Zd(k1)
                     k1 = k1 + Nbx
                  ENDDO
                  IF ( Eol==0 ) CYCLE
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( all==3 ) THEN
                  DO k = k2 , k3 , Ndx
                     Z(k) = Z(k) + A(1)*Z(k1) - A(2)*Z(k1+1)
                     Z(k+1) = Z(k+1) + A(1)*Z(k1+1) + A(2)*Z(k1)
                     k1 = k1 + Nbx
                  ENDDO
                  IF ( Eol==0 ) CYCLE
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( all==4 ) THEN
                  DO k = k2 , k3 , Ndx
                     Zd(k) = Zd(k) + ad(1)*Zd(k1) - ad(2)*Zd(k1+1)
                     Zd(k+1) = Zd(k+1) + ad(1)*Zd(k1+1) + ad(2)*Zd(k1)
                     k1 = k1 + Nbx
                  ENDDO
                  IF ( Eol==0 ) CYCLE
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
         DO k = k2 , k3 , Ndx
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  j = k1
                  GOTO bpick
 2                IF ( Z(j)==0.0 ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  b(1) = Z(j)
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 4                IF ( Z(j)==0.0 .AND. Z(j+1)==0.0 ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  b(1) = Z(j)
                  b(2) = Z(j+1)
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 6                IF ( Zd(j)==0.D0 ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  bd(1) = Zd(j)
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 8                IF ( Zd(j)==0.D0 .AND. Zd(j+1)==0.D0 ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  bd(1) = Zd(j)
                  bd(2) = Zd(j+1)
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 10               IF ( Z(j)==0.0 ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  bd(1) = Z(j)
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 12               IF ( Z(j)==0.0 .AND. Z(j+1)==0.0 ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  bd(1) = Z(j)
                  bd(2) = Z(j+1)
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 14               IF ( Zd(j)==0.D0 .AND. Zd(j+1)==0.D0 ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  b(1) = Zd(j)
                  b(2) = Zd(j+1)
                  spag_nextblock_2 = 2
               CASE (2)
!
                  GOTO arith
 16               Z(k) = Z(k) + A(1)*b(1)
                  spag_nextblock_2 = 3
                  CYCLE SPAG_DispatchLoop_2
 18               Zd(k) = Zd(k) + ad(1)*bd(1)
                  spag_nextblock_2 = 3
                  CYCLE SPAG_DispatchLoop_2
 20               Z(k) = Z(k) + A(1)*b(1) - A(2)*b(2)
                  Z(k+1) = Z(k+1) + A(1)*b(2) + A(2)*b(1)
                  spag_nextblock_2 = 3
                  CYCLE SPAG_DispatchLoop_2
 22               Zd(k) = Zd(k) + ad(1)*bd(1) - ad(2)*bd(2)
                  Zd(k+1) = Zd(k+1) + ad(1)*bd(2) + ad(2)*bd(1)
                  spag_nextblock_2 = 3
               CASE (3)
                  k1 = k1 + Nbx
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         IF ( Eol==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         RETURN
!
!
         ENTRY mpy2nv(Zz,Z,Zd)
!     ======================
!
!     METHOD 2 NON-TRANSPOSE CASE
!
         b(2) = 0.
         bd(2) = 0.D0
         aa(2) = 0.
         add(2) = 0.D0
         l = Firstl
         Acol = Acol1
         spag_nextblock_1 = 4
      CASE (4)
         CALL zntpki
         IF ( I<Acol1 .OR. I>Acoln .OR. I<Acol ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         l = l - 2*(I-Acol)
         Acol = I
         Apoint = Zz(l)
         IF ( Apoint==0 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nbr = Zz(l-1)
         IF ( all/=0 ) THEN
            IF ( all==1 ) THEN
               DO
!
!     COMMON CASES (TYPEA=TYPEB=TYPED=PREC)
!
!     PREC=1, ARITH2(1210), APICK2(1100) AND BPICK2(1010)
!     PREC=2, ARITH2(1220), APICK2(1120) AND BPICK2(1030)
!     PREC=3, ARITH2(1230), APICK2(1110) AND BPICK2(1020)
!     PREC=4, ARITH2(1620), APICK2(1510) AND BPICK2(1410)
!
                  nbrstr = Zz(Apoint+1)
                  init = Zz(Apoint)
                  Apoint = Apoint + 2
                  j = Apoint
                  IF ( Prca==2 ) j = j/2 + 1
                  Apoint = Apoint + nbrstr*Nwda
                  irow = init*Rcd - Rcd + 1
                  nrow = irow + nbrstr*Rcd - 1
                  DO k = irow , nrow , Rcd
                     Z(k) = Z(k) + Z(j)*A(1)
                     j = j + Rca
                  ENDDO
                  nbr = nbr - 1
                  IF ( nbr<=0 ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ELSEIF ( all==2 ) THEN
               DO
!
                  nbrstr = Zz(Apoint+1)
                  init = Zz(Apoint)
                  Apoint = Apoint + 2
                  j = Apoint
                  IF ( Prca==2 ) j = j/2 + 1
                  Apoint = Apoint + nbrstr*Nwda
                  irow = init*Rcd - Rcd + 1
                  nrow = irow + nbrstr*Rcd - 1
                  DO k = irow , nrow , Rcd
                     Zd(k) = Zd(k) + Zd(j)*ad(1)
                     j = j + Rca
                  ENDDO
                  nbr = nbr - 1
                  IF ( nbr<=0 ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ELSEIF ( all==3 ) THEN
               DO
!
                  nbrstr = Zz(Apoint+1)
                  init = Zz(Apoint)
                  Apoint = Apoint + 2
                  j = Apoint
                  IF ( Prca==2 ) j = j/2 + 1
                  Apoint = Apoint + nbrstr*Nwda
                  irow = init*Rcd - Rcd + 1
                  nrow = irow + nbrstr*Rcd - 1
                  DO k = irow , nrow , Rcd
                     Z(k) = Z(k) + Z(j)*A(1) - Z(j+1)*A(2)
                     Z(k+1) = Z(k+1) + Z(j)*A(2) + Z(j+1)*A(1)
                     j = j + Rca
                  ENDDO
                  nbr = nbr - 1
                  IF ( nbr<=0 ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ELSEIF ( all==4 ) THEN
               DO
!
                  nbrstr = Zz(Apoint+1)
                  init = Zz(Apoint)
                  Apoint = Apoint + 2
                  j = Apoint
                  IF ( Prca==2 ) j = j/2 + 1
                  Apoint = Apoint + nbrstr*Nwda
                  irow = init*Rcd - Rcd + 1
                  nrow = irow + nbrstr*Rcd - 1
                  DO k = irow , nrow , Rcd
                     Zd(k) = Zd(k) + Zd(j)*ad(1) - Zd(j+1)*ad(2)
                     Zd(k+1) = Zd(k+1) + Zd(j)*ad(2) + Zd(j+1)*ad(1)
                     j = j + Rca
                  ENDDO
                  nbr = nbr - 1
                  IF ( nbr<=0 ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
         GOTO bpick2
 40      b(1) = A(1)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 60      b(1) = A(1)
         b(2) = A(2)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 80      bd(1) = ad(1)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 100     bd(1) = ad(1)
         bd(2) = ad(2)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 120     bd(1) = A(1)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 140     bd(1) = A(1)
         bd(2) = A(2)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 160     b(1) = ad(1)
         b(2) = ad(2)
         spag_nextblock_1 = 5
      CASE (5)
         SPAG_Loop_1_2: DO
!
            nbrstr = Zz(Apoint+1)
            init = Zz(Apoint)
            Apoint = Apoint + 2
            j = Apoint
            IF ( Prca==2 ) j = j/2 + 1
            Apoint = Apoint + nbrstr*Nwda
            irow = init*Rcd - Rcd + 1
            nrow = irow + nbrstr*Rcd - 1
            DO k = irow , nrow , Rcd
               spag_nextblock_3 = 1
               SPAG_DispatchLoop_3: DO
                  SELECT CASE (spag_nextblock_3)
                  CASE (1)
                     GOTO apick2
 162                 aa(1) = Z(j)
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
 164                 aa(1) = Z(j)
                     aa(2) = Z(j+1)
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
 166                 add(1) = Zd(j)
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
 168                 add(1) = Zd(j)
                     add(2) = Zd(j+1)
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
 170                 add(1) = Z(j)
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
 172                 add(1) = Z(j)
                     add(2) = Z(j+1)
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
 174                 aa(1) = Zd(j)
                     aa(2) = Zd(j+1)
                     spag_nextblock_3 = 2
                  CASE (2)
!
                     GOTO arith2
 176                 Z(k) = Z(k) + aa(1)*b(1)
                     spag_nextblock_3 = 3
                     CYCLE SPAG_DispatchLoop_3
 178                 Zd(k) = Zd(k) + add(1)*bd(1)
                     spag_nextblock_3 = 3
                     CYCLE SPAG_DispatchLoop_3
 180                 Z(k) = Z(k) + aa(1)*b(1) - aa(2)*b(2)
                     Z(k+1) = Z(k+1) + aa(1)*b(2) + aa(2)*b(1)
                     spag_nextblock_3 = 3
                     CYCLE SPAG_DispatchLoop_3
 182                 Zd(k) = Zd(k) + add(1)*bd(1) - add(2)*bd(2)
                     Zd(k+1) = Zd(k+1) + add(1)*bd(2) + add(2)*bd(1)
                     spag_nextblock_3 = 3
                  CASE (3)
                     j = j + Rca
                     EXIT SPAG_DispatchLoop_3
                  END SELECT
               ENDDO SPAG_DispatchLoop_3
            ENDDO
            nbr = nbr - 1
            IF ( nbr<=0 ) EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 6
      CASE (6)
!
         l = l - 2
         Acol = Acol + 1
         spag_nextblock_1 = 7
      CASE (7)
         IF ( Eol==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         RETURN
!
!
         ENTRY mpy2tv(Zz,Z,Zd)
!     ======================
!
!     METHOD 2 - TRANSPOSE CASE
!
!     COMMENTS FROM G.CHAN/UNISYS      1/91
!     OBSERVE THAT THERE IS NO DO-LOOP IN THIS MPY2TV LOGIC. IT IS
!     THEREFORE CONCLUDED THAT THE TRANSPOSE CASE WOULD TAKE MUCH MORE
!     TIME THAN THE NON-TRANSPOSE CASE
!
         b(2) = 0.
         bd(2) = 0.D0
         aa(2) = 0.
         add(2) = 0.D0
         dd(1) = 0.D0
         dd(2) = 0.D0
         l = Firstl
         Apoint = Zz(l)
         arow = arow1
         IF ( Crow==mask6f ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (8)
         IF ( Eol/=0 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         CALL zntpki
         Crow = I
         spag_nextblock_1 = 10
      CASE (10)
         dd(1) = ad(1)
         dd(2) = ad(2)
         IF ( Crow<arow ) THEN
            Drow = Crow
            CALL zblpki
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Crow==arow ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
         dd(1) = 0.D0
         dd(2) = 0.D0
         IF ( Apoint==0 ) THEN
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
         Drow = arow
         IF ( Apoint==0 ) THEN
            CALL zblpki
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ELSE
            nbrstr = Zz(l-1)
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
         nbr = Zz(Apoint+1)
         nbr1 = nbr
         init = Zz(Apoint)
         Apoint = Apoint + 2
         j = Apoint
         IF ( Prca>1 ) j = j/2 + 1
         Apoint = Apoint + nbr*Nwda
         k = (init-1)*Rcb + 1
         spag_nextblock_1 = 14
      CASE (14)
         IF ( all/=0 ) THEN
            IF ( all==1 ) THEN
               DO
!
!     COMMON CASES (TYPEA=TYPEB=TYPED=PREC)
!
!     PREC=1, ARITH2(1600), APICK2(1500) AND BPICK2(1400)
!     PREC=2, ARITH2(1610), APICK2(1520) AND BPICK2(1420)
!     PREC=3, ARITH2(1620), APICK2(1510) AND BPICK2(1410)
!     PREC=4, ARITH2(1630), APICK2(1530) AND BPICK2(1430)
!
                  D(1) = D(1) + Z(j)*Z(k)
                  j = j + Rca
                  k = k + Rcb
                  nbr = nbr - 1
                  IF ( nbr<=0 ) THEN
                     spag_nextblock_1 = 18
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ELSEIF ( all==2 ) THEN
               DO
!
                  dd(1) = dd(1) + Zd(j)*Zd(k)
                  j = j + Rca
                  k = k + Rcb
                  nbr = nbr - 1
                  IF ( nbr<=0 ) THEN
                     spag_nextblock_1 = 18
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ELSEIF ( all==3 ) THEN
               DO
!
                  D(1) = D(1) + Z(j)*Z(k) - Z(j+1)*Z(k+1)
                  D(2) = D(2) + Z(j)*Z(k+1) + Z(j+1)*Z(k)
                  j = j + Rca
                  k = k + Rcb
                  nbr = nbr - 1
                  IF ( nbr<=0 ) THEN
                     spag_nextblock_1 = 18
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ELSEIF ( all==4 ) THEN
               DO
!
                  dd(1) = dd(1) + Zd(j)*Zd(k) - Zd(j+1)*Zd(k+1)
                  dd(2) = dd(2) + Zd(j)*Zd(k+1) + Zd(j+1)*Zd(k)
                  j = j + Rca
                  k = k + Rcb
                  nbr = nbr - 1
                  IF ( nbr<=0 ) THEN
                     spag_nextblock_1 = 18
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
         GOTO bpick2
 200     b(1) = Z(k)
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 220     b(1) = Z(k)
         b(2) = Z(k+1)
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 240     bd(1) = Zd(k)
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 260     bd(1) = Zd(k)
         bd(2) = Zd(k+1)
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 280     bd(1) = Z(k)
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 300     bd(1) = Z(k)
         bd(2) = Z(k+1)
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 320     b(1) = Zd(k)
         b(2) = Zd(k+1)
         spag_nextblock_1 = 15
      CASE (15)
!
         GOTO apick2
 340     aa(1) = Z(j)
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 360     aa(1) = Z(j)
         aa(2) = Z(j+1)
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 380     add(1) = Zd(j)
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 400     add(1) = Zd(j)
         add(2) = Zd(j+1)
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 420     add(1) = Z(j)
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 440     add(1) = Z(j)
         add(2) = Z(j+1)
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 460     aa(1) = Z(j)
         aa(2) = Z(j+2)
         spag_nextblock_1 = 16
      CASE (16)
!
         GOTO arith2
 480     D(1) = D(1) + aa(1)*b(1)
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
 500     dd(1) = dd(1) + add(1)*bd(1)
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
 520     D(1) = D(1) + aa(1)*b(1) - aa(2)*b(2)
         D(2) = D(2) + aa(1)*b(2) + aa(2)*b(1)
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
 540     dd(1) = dd(1) + add(1)*bd(1) - add(2)*bd(2)
         dd(2) = dd(2) + add(1)*bd(2) + add(2)*bd(1)
         spag_nextblock_1 = 17
      CASE (17)
!
         j = j + Rca
         k = k + Rcb
         nbr = nbr - 1
         IF ( nbr>0 ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 18
      CASE (18)
         nbrstr = nbrstr - 1
         IF ( nbrstr>0 ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL zblpki
         spag_nextblock_1 = 19
      CASE (19)
         l = l - 2
         arow = arow + 1
         IF ( arow<=arown ) THEN
            Apoint = Zz(l)
            IF ( Crow<arow ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Crow/=arow ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ELSE
            RETURN
         ENDIF
!
!
         ENTRY mpy3t(*,Aas,Aad,Dds,Ddd)
!     ===============================
!
!     METHOD 3 (TRANSPOSE ONLY)
!
         b3flag = -1
         CALL getstr(*560,Block)
!IBMNB 6/93
         IF ( Block(2)/=typeb ) THEN
            typeb = Block(2)
            Rcb = Rc(typeb)
            all = 0
         ENDIF
!IBMNE 6/93
         IF ( all/=0 ) THEN
            IF ( all==1 ) THEN
               DO
!
!     COMMON CASE (TYPEA=TYPEB=TYPED=PREC=1)
!
                  jb3n = jb31 + nterm3 - 1
                  DO jb3 = jb31 , jb3n
                     k = j3
                     DO I = arow1 , arown
                        Dds(I) = Dds(I) + Aas(k)*Bbs(jb3)
                        k = k + Na
                     ENDDO
                     j3 = j3 + 1
                  ENDDO
                  CALL endget(Block)
                  CALL getstr(*99999,Block)
               ENDDO
            ELSEIF ( all==2 ) THEN
               DO
!
!     COMMON CASE (TYPEA=TYPEB=TYPED=PREC=2)
!
                  jb3n = jb31 + nterm3 - 1
                  DO jb3 = jb31 , jb3n
                     k = j3
                     DO I = arow1 , arown
                        Ddd(I) = Ddd(I) + Aad(k)*bbd(jb3)
                        k = k + fa3
                     ENDDO
                     j3 = j3 + 1
                  ENDDO
                  CALL endget(Block)
                  CALL getstr(*99999,Block)
               ENDDO
            ELSEIF ( all==3 ) THEN
!
!     COMMON CASE (TYPEA=TYPEB=TYPED=PREC=3)
!
               i1 = 2*arow1 - 1
               in = 2*arown - 1
               DO
                  j3 = 2*j3 - 1
                  jb3n = jb31 + Rcb*nterm3 - Rcb
                  DO jb3 = jb31 , jb3n , Rcb
                     k = j3
                     DO I = i1 , in , 2
                        Dds(I) = Dds(I) + Aas(k)*Bbs(jb3) - Aas(k+1)*Bbs(jb3+1)
                        Dds(I+1) = Dds(I+1) + Aas(k)*Bbs(jb3+1) + Aas(k+1)*Bbs(jb3)
                        k = k + Na
                     ENDDO
                     j3 = j3 + Rca
                  ENDDO
                  CALL endget(Block)
                  CALL getstr(*99999,Block)
               ENDDO
            ELSEIF ( all==4 ) THEN
!
!     COMMON CASE (TYPEA=TYPEB=TYPED=PREC=4)
!
               inca = Rca*fa3
               i1 = 2*arow1 - 1
               in = 2*arown - 1
               DO
                  j3 = 2*j3 - 1
                  jb3n = jb31 + Rcb*nterm3 - Rcb
                  DO jb3 = jb31 , jb3n , Rcb
                     k = j3
                     DO I = i1 , in , 2
                        Ddd(I) = Ddd(I) + Aad(k)*bbd(jb3) - Aad(k+1)*bbd(jb3+1)
                        Ddd(I+1) = Ddd(I+1) + Aad(k)*bbd(jb3+1) + Aad(k+1)*bbd(jb3)
                        k = k + inca
                     ENDDO
                     j3 = j3 + Rca
                  ENDDO
                  CALL endget(Block)
                  CALL getstr(*99999,Block)
               ENDDO
            ENDIF
         ENDIF
         IF ( Typed==2 ) THEN
!
!     PERFORM ARITHMETIC IN REAL DOUBLE PRECISION
!
            k1 = 2*(Prca-1) + Prc(typeb)
            DO
               jb3n = jb31 + nterm3 - 1
               DO jb3 = jb31 , jb3n
                  k = j3
                  IF ( k1==2 ) THEN
                     DO I = arow1 , arown
                        Ddd(I) = Ddd(I) + Aas(k)*bbd(jb3)
                        k = k + fa3
                     ENDDO
                  ELSEIF ( k1==3 ) THEN
                     DO I = arow1 , arown
                        Ddd(I) = Ddd(I) + Aad(k)*Bbs(jb3)
                        k = k + fa3
                     ENDDO
                  ELSEIF ( k1==4 ) THEN
                     DO I = arow1 , arown
                        Ddd(I) = Ddd(I) + Aad(k)*bbd(jb3)
                        k = k + fa3
                     ENDDO
                  ELSE
                     DO I = arow1 , arown
                        Ddd(I) = Ddd(I) + Aas(k)*Bbs(jb3)
                        k = k + fa3
                     ENDDO
                  ENDIF
                  j3 = j3 + 1
               ENDDO
               CALL endget(Block)
               CALL getstr(*99999,Block)
            ENDDO
         ELSEIF ( Typed==3 ) THEN
!
!     PERFORM ARITHMETIC IN COMPLEX SINGLE PRECISION
!
            bsi = 0.
            i1 = 2*arow1 - 1
            in = 2*arown - 1
            DO
               IF ( Rca==2 ) j3 = 2*j3 - 1
               jb3n = jb31 + Rcb*nterm3 - Rcb
               DO jb3 = jb31 , jb3n , Rcb
                  bsr = Bbs(jb3)
                  IF ( Rcb==2 ) bsi = Bbs(jb3+1)
                  k = j3
                  IF ( Rca==2 ) THEN
                     DO I = i1 , in , 2
                        Dds(I) = Dds(I) + Aas(k)*bsr - Aas(k+1)*bsi
                        Dds(I+1) = Dds(I+1) + Aas(k)*bsi + Aas(k+1)*bsr
                        k = k + Na
                     ENDDO
                  ELSE
                     DO I = i1 , in , 2
                        Dds(I) = Dds(I) + Aas(k)*bsr
                        Dds(I+1) = Dds(I+1) + Aas(k)*bsi
                        k = k + Na
                     ENDDO
                  ENDIF
                  j3 = j3 + Rca
               ENDDO
               CALL endget(Block)
               CALL getstr(*99999,Block)
            ENDDO
         ELSEIF ( Typed==4 ) THEN
!
!     PERFORM ARITHMETIC IN COMPLEX DOUBLE PRECISION
!
            bdi = 0.
            inca = Rca*fa3
            i1 = 2*arow1 - 1
            in = 2*arown - 1
            DO
               IF ( Rca==2 ) j3 = 2*j3 - 1
               jb3n = jb31 + Rcb*nterm3 - Rcb
               DO jb3 = jb31 , jb3n , Rcb
                  k = j3
                  IF ( typeb==2 ) THEN
                     bdr = bbd(jb3)
                  ELSEIF ( typeb==3 ) THEN
                     bdr = Bbs(jb3)
                     bdi = Bbs(jb3+1)
                  ELSEIF ( typeb==4 ) THEN
                     bdr = bbd(jb3)
                     bdi = bbd(jb3+1)
                  ELSE
                     bdr = Bbs(jb3)
                  ENDIF
                  IF ( typea==2 ) THEN
                     DO I = i1 , in , 2
                        Ddd(I) = Ddd(I) + Aad(k)*bdr
                        Ddd(I+1) = Ddd(I+1) + Aad(k)*bdi
                        k = k + inca
                     ENDDO
                  ELSEIF ( typea==3 ) THEN
                     DO I = i1 , in , 2
                        Ddd(I) = Ddd(I) + Aas(k)*bdr - Aas(k+1)*bdi
                        Ddd(I+1) = Ddd(I+1) + Aas(k)*bdi + Aas(k+1)*bdr
                        k = k + inca
                     ENDDO
                  ELSEIF ( typea==4 ) THEN
                     DO I = i1 , in , 2
                        Ddd(I) = Ddd(I) + Aad(k)*bdr - Aad(k+1)*bdi
                        Ddd(I+1) = Ddd(I+1) + Aad(k)*bdi + Aad(k+1)*bdr
                        k = k + inca
                     ENDDO
                  ELSE
                     DO I = i1 , in , 2
                        Ddd(I) = Ddd(I) + Aas(k)*bdr
                        Ddd(I+1) = Ddd(I+1) + Aas(k)*bdi
                        k = k + inca
                     ENDDO
                  ENDIF
                  j3 = j3 + Rca
               ENDDO
               CALL endget(Block)
               CALL getstr(*99999,Block)
            ENDDO
         ELSE
            DO
!
!     PERFORM ARITHMETIC IN REAL SINGLE PRECISION
!
               jb3n = jb31 + nterm3 - 1
               DO jb3 = jb31 , jb3n
                  k = j3
                  bbb = Bbs(jb3)
                  IF ( Block(2)==2 ) bbb = bbd(jb3)
                  IF ( typea/=2 ) THEN
                     DO I = arow1 , arown
                        Dds(I) = Dds(I) + Aas(k)*bbb
                        k = k + Na
                     ENDDO
                  ELSE
                     DO I = arow1 , arown
                        aaa = Aad(k)
                        Dds(I) = Dds(I) + aaa*bbb
                        k = k + Na
                     ENDDO
                  ENDIF
                  j3 = j3 + 1
               ENDDO
               CALL endget(Block)
               CALL getstr(*99999,Block)
            ENDDO
         ENDIF
!
 560     RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99999 END SUBROUTINE mpyq
