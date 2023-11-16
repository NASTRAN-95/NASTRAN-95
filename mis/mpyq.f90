
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
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(4) , Bbs(17000) , D(4)
   INTEGER Acol , Acol1 , Acoln , Acore , All4 , Apoint , Arow , Arow1 , Arown , B3flag , Bcol , Block(20) , Cls , Clsrew , Crow ,  &
         & Drow , Eol , Eor , Filea(7) , Fileb(7) , Filec(7) , Filed(7) , Firstl , Flag , Form , I , Incr1 , Incr2 , Incra , Ipass ,&
         & J3 , Jb , Jb31 , Jb3n , Jmax , Jmax1x , Jump4 , Ksystm(65) , Ll , Lll , M , Mout , N , Na , Nb , Nbrstr , Nbx , Nd ,     &
         & Ndx , Nterm3 , Nwda , Nwdb , Nwdd , Nwds(4) , Nz , One1 , One2 , Point , Pp1 , Pp2 , Prc(2) , Prca , Prec , Prec1 ,      &
         & Prec4 , Q , R , Rc(4) , Rca , Rcb , Rcd , Rd , Rdrew , Row , Scrtch , Signab , Signc , Sysbuf , T , Time
   DOUBLE PRECISION Ad(2) , Bbd(1) , Dd(2)
   INTEGER Typd , Typd1 , Type , Typea , Typeb , Typebd , Typec , Typed , Wrt , Wrtrew
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
!
! Dummy argument declarations
!
   DOUBLE PRECISION Aad(1) , Ddd(1) , Zd(1)
   REAL Aas(1) , Dds(1) , Z(1)
   INTEGER Zz(1)
!
! Local variable declarations
!
   REAL aa(4) , aaa , b(4) , bbb , bsi , bsr
   DOUBLE PRECISION add(2) , bd(2) , bdi , bdr
   INTEGER all , apick2 , arith , arith2 , bpick , bpick2 , buf1 , fa3 , i1 , in , inca , init , irow , j , jb3 , k , k1 , k2 , k3 ,&
         & l , mask6f , moda , modb , nbr , nbr1 , nrow , nzz
!
! End of declarations
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Mout) , (Ksystm(58),Ipass)
   EQUIVALENCE (A(1),Ad(1)) , (b(1),bd(1)) , (D(1),Dd(1)) , (Filea(2),M) , (Filea(3),N) , (Filea(5),Typea) , (Fileb(2),Q) ,         &
    & (Fileb(3),R) , (Fileb(5),Typeb) , (Filec(5),Typec) , (Filed(5),Typd) , (nzz,buf1) , (Acoln,Arown) , (aa(1),add(1)) ,          &
    & (Acol1,Arow1) , (Acol,Arow) , (Bbs(1),Bbd(1))
   EQUIVALENCE (Block(2),Type) , (Block(3),Form) , (Block(4),Row,J3) , (Block(5),Point) , (Block(6),Nbrstr) , (Block(8),Flag)
   EQUIVALENCE (Block(5),Jb31) , (Block(6),Nterm3) , (Block(7),Jb3n) , (Block(8),B3flag)
!     DATA    MASK6F / X'00FFFFFF'  /
   DATA mask6f/16777215/
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
   Rca = Rc(Typea)
   modb = mod(Typeb,2)
   moda = mod(Typea,2)
   Prca = Prc(Typea)
   fa3 = Filea(3)
!
!     IF DIAG 43 IS ON, SKIP ALL (1991) SPEED IMPROVEMENT LOGIC
!     (THIS IS ONLY TEMPORARY)
!
   all = 0
   IF ( Typea==Typeb .AND. Typea==Typed ) all = Typea
   All4 = all
   IF ( All4==0 ) All4 = 5
   IF ( Typed>=3 .AND. Typea<=2 .AND. Typeb<=2 ) All4 = 6
   CALL sswtch(43,j)
   IF ( j==1 ) all = 0
   Jump4 = Typeb + (Typea-1)*4
   Prec4 = Prec1 - 1
!
!     RCA, PRCA, ALL4 AND JUMP4 ARE USED IN MPY4T
!
   IF ( Typed==2 ) THEN
!
!     REAL DOUBLE PRECISION
!
      ASSIGN 600 TO arith
      ASSIGN 250 TO bpick
      IF ( modb==1 ) ASSIGN 350 TO bpick
      IF ( T/=0 ) THEN
!
         ASSIGN 4600 TO arith2
         ASSIGN 3100 TO bpick2
         IF ( modb==1 ) ASSIGN 3300 TO bpick2
         ASSIGN 3900 TO apick2
         IF ( moda==1 ) ASSIGN 4100 TO apick2
      ELSE
!
         ASSIGN 1900 TO arith2
         ASSIGN 1200 TO bpick2
         IF ( modb==1 ) ASSIGN 1400 TO bpick2
         ASSIGN 1760 TO apick2
         IF ( moda==1 ) ASSIGN 1800 TO apick2
      ENDIF
   ELSEIF ( Typed==3 ) THEN
!
!     COMPLEX SINGLE PRECISION
!
      ASSIGN 650 TO arith
      IF ( Typeb==3 ) THEN
         ASSIGN 200 TO bpick
      ELSEIF ( Typeb==4 ) THEN
         ASSIGN 450 TO bpick
      ELSE
         ASSIGN 150 TO bpick
      ENDIF
      IF ( T/=0 ) THEN
         ASSIGN 4700 TO arith2
!
         IF ( Typeb==3 ) THEN
            ASSIGN 3000 TO bpick2
         ELSEIF ( Typeb==4 ) THEN
            ASSIGN 3500 TO bpick2
         ELSE
            ASSIGN 2900 TO bpick2
         ENDIF
         IF ( Typea==3 ) THEN
            ASSIGN 3800 TO apick2
         ELSEIF ( Typea==4 ) THEN
            ASSIGN 4300 TO apick2
         ELSE
            ASSIGN 3700 TO apick2
         ENDIF
      ELSE
!
         ASSIGN 1920 TO arith2
         IF ( Typeb==3 ) THEN
            ASSIGN 1100 TO bpick2
         ELSEIF ( Typeb==4 ) THEN
            ASSIGN 1600 TO bpick2
         ELSE
            ASSIGN 1000 TO bpick2
         ENDIF
         IF ( Typea==3 ) THEN
            ASSIGN 1740 TO apick2
         ELSEIF ( Typea==4 ) THEN
            ASSIGN 1840 TO apick2
         ELSE
            ASSIGN 1720 TO apick2
         ENDIF
      ENDIF
   ELSEIF ( Typed==4 ) THEN
!
!     COMPLEX DOUBLE PRECISION
!
      ASSIGN 700 TO arith
      IF ( Typeb==2 ) THEN
         ASSIGN 250 TO bpick
      ELSEIF ( Typeb==3 ) THEN
         ASSIGN 400 TO bpick
      ELSEIF ( Typeb==4 ) THEN
         ASSIGN 300 TO bpick
      ELSE
         ASSIGN 350 TO bpick
      ENDIF
      IF ( T/=0 ) THEN
!
         ASSIGN 4800 TO arith2
         IF ( Typeb==2 ) THEN
            ASSIGN 3100 TO bpick2
         ELSEIF ( Typeb==3 ) THEN
            ASSIGN 3400 TO bpick2
         ELSEIF ( Typeb==4 ) THEN
            ASSIGN 3200 TO bpick2
         ELSE
            ASSIGN 3300 TO bpick2
         ENDIF
         IF ( Typea==1 ) THEN
            IF ( moda==1 ) ASSIGN 4100 TO apick2
         ELSEIF ( Typea==3 ) THEN
            ASSIGN 4200 TO apick2
         ELSEIF ( Typea==4 ) THEN
            ASSIGN 4000 TO apick2
         ELSE
            ASSIGN 3900 TO apick2
         ENDIF
      ELSE
!
         ASSIGN 1940 TO arith2
         IF ( Typeb==2 ) THEN
            ASSIGN 1200 TO bpick2
         ELSEIF ( Typeb==3 ) THEN
            ASSIGN 1500 TO bpick2
         ELSEIF ( Typeb==4 ) THEN
            ASSIGN 1300 TO bpick2
         ELSE
            ASSIGN 1400 TO bpick2
         ENDIF
         IF ( Typea==1 ) THEN
            IF ( moda==1 ) ASSIGN 1800 TO apick2
         ELSEIF ( Typea==3 ) THEN
            ASSIGN 1820 TO apick2
         ELSEIF ( Typea==4 ) THEN
            ASSIGN 1780 TO apick2
         ELSE
            ASSIGN 1760 TO apick2
         ENDIF
      ENDIF
   ELSE
!
!     REAL SINGLE PRECISION
!
      ASSIGN 550 TO arith
      ASSIGN 150 TO bpick
      IF ( T/=0 ) THEN
!
         ASSIGN 4500 TO arith2
         ASSIGN 2900 TO bpick2
         ASSIGN 3700 TO apick2
      ELSE
!
         ASSIGN 1880 TO arith2
         ASSIGN 1000 TO bpick2
         ASSIGN 1720 TO apick2
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
 100  DO
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
            GOTO 800
         ELSEIF ( all==2 ) THEN
            DO k = k2 , k3 , Ndx
               Zd(k) = Zd(k) + Ad(1)*Zd(k1)
               k1 = k1 + Nbx
            ENDDO
            IF ( Eol==0 ) CYCLE
            GOTO 800
         ELSEIF ( all==3 ) THEN
            DO k = k2 , k3 , Ndx
               Z(k) = Z(k) + A(1)*Z(k1) - A(2)*Z(k1+1)
               Z(k+1) = Z(k+1) + A(1)*Z(k1+1) + A(2)*Z(k1)
               k1 = k1 + Nbx
            ENDDO
            IF ( Eol==0 ) CYCLE
            GOTO 800
         ELSEIF ( all==4 ) THEN
            DO k = k2 , k3 , Ndx
               Zd(k) = Zd(k) + Ad(1)*Zd(k1) - Ad(2)*Zd(k1+1)
               Zd(k+1) = Zd(k+1) + Ad(1)*Zd(k1+1) + Ad(2)*Zd(k1)
               k1 = k1 + Nbx
            ENDDO
            IF ( Eol==0 ) CYCLE
            GOTO 800
         ENDIF
      ENDIF
      EXIT
   ENDDO
   DO k = k2 , k3 , Ndx
      j = k1
      GOTO bpick
 150  IF ( Z(j)==0.0 ) GOTO 750
      b(1) = Z(j)
      GOTO 500
 200  IF ( Z(j)==0.0 .AND. Z(j+1)==0.0 ) GOTO 750
      b(1) = Z(j)
      b(2) = Z(j+1)
      GOTO 500
 250  IF ( Zd(j)==0.D0 ) GOTO 750
      bd(1) = Zd(j)
      GOTO 500
 300  IF ( Zd(j)==0.D0 .AND. Zd(j+1)==0.D0 ) GOTO 750
      bd(1) = Zd(j)
      bd(2) = Zd(j+1)
      GOTO 500
 350  IF ( Z(j)==0.0 ) GOTO 750
      bd(1) = Z(j)
      GOTO 500
 400  IF ( Z(j)==0.0 .AND. Z(j+1)==0.0 ) GOTO 750
      bd(1) = Z(j)
      bd(2) = Z(j+1)
      GOTO 500
 450  IF ( Zd(j)==0.D0 .AND. Zd(j+1)==0.D0 ) GOTO 750
      b(1) = Zd(j)
      b(2) = Zd(j+1)
!
 500  GOTO arith
 550  Z(k) = Z(k) + A(1)*b(1)
      GOTO 750
 600  Zd(k) = Zd(k) + Ad(1)*bd(1)
      GOTO 750
 650  Z(k) = Z(k) + A(1)*b(1) - A(2)*b(2)
      Z(k+1) = Z(k+1) + A(1)*b(2) + A(2)*b(1)
      GOTO 750
 700  Zd(k) = Zd(k) + Ad(1)*bd(1) - Ad(2)*bd(2)
      Zd(k+1) = Zd(k+1) + Ad(1)*bd(2) + Ad(2)*bd(1)
 750  k1 = k1 + Nbx
   ENDDO
   IF ( Eol==0 ) GOTO 100
 800  RETURN
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
 900  CALL zntpki
   IF ( I<Acol1 .OR. I>Acoln .OR. I<Acol ) GOTO 2100
   l = l - 2*(I-Acol)
   Acol = I
   Apoint = Zz(l)
   IF ( Apoint==0 ) GOTO 2000
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
            Nbrstr = Zz(Apoint+1)
            init = Zz(Apoint)
            Apoint = Apoint + 2
            j = Apoint
            IF ( Prca==2 ) j = j/2 + 1
            Apoint = Apoint + Nbrstr*Nwda
            irow = init*Rcd - Rcd + 1
            nrow = irow + Nbrstr*Rcd - 1
            DO k = irow , nrow , Rcd
               Z(k) = Z(k) + Z(j)*A(1)
               j = j + Rca
            ENDDO
            nbr = nbr - 1
            IF ( nbr<=0 ) GOTO 2000
         ENDDO
      ELSEIF ( all==2 ) THEN
         DO
!
            Nbrstr = Zz(Apoint+1)
            init = Zz(Apoint)
            Apoint = Apoint + 2
            j = Apoint
            IF ( Prca==2 ) j = j/2 + 1
            Apoint = Apoint + Nbrstr*Nwda
            irow = init*Rcd - Rcd + 1
            nrow = irow + Nbrstr*Rcd - 1
            DO k = irow , nrow , Rcd
               Zd(k) = Zd(k) + Zd(j)*Ad(1)
               j = j + Rca
            ENDDO
            nbr = nbr - 1
            IF ( nbr<=0 ) GOTO 2000
         ENDDO
      ELSEIF ( all==3 ) THEN
         DO
!
            Nbrstr = Zz(Apoint+1)
            init = Zz(Apoint)
            Apoint = Apoint + 2
            j = Apoint
            IF ( Prca==2 ) j = j/2 + 1
            Apoint = Apoint + Nbrstr*Nwda
            irow = init*Rcd - Rcd + 1
            nrow = irow + Nbrstr*Rcd - 1
            DO k = irow , nrow , Rcd
               Z(k) = Z(k) + Z(j)*A(1) - Z(j+1)*A(2)
               Z(k+1) = Z(k+1) + Z(j)*A(2) + Z(j+1)*A(1)
               j = j + Rca
            ENDDO
            nbr = nbr - 1
            IF ( nbr<=0 ) GOTO 2000
         ENDDO
      ELSEIF ( all==4 ) THEN
         DO
!
            Nbrstr = Zz(Apoint+1)
            init = Zz(Apoint)
            Apoint = Apoint + 2
            j = Apoint
            IF ( Prca==2 ) j = j/2 + 1
            Apoint = Apoint + Nbrstr*Nwda
            irow = init*Rcd - Rcd + 1
            nrow = irow + Nbrstr*Rcd - 1
            DO k = irow , nrow , Rcd
               Zd(k) = Zd(k) + Zd(j)*Ad(1) - Zd(j+1)*Ad(2)
               Zd(k+1) = Zd(k+1) + Zd(j)*Ad(2) + Zd(j+1)*Ad(1)
               j = j + Rca
            ENDDO
            nbr = nbr - 1
            IF ( nbr<=0 ) GOTO 2000
         ENDDO
      ENDIF
   ENDIF
   GOTO bpick2
 1000 b(1) = A(1)
   GOTO 1700
 1100 b(1) = A(1)
   b(2) = A(2)
   GOTO 1700
 1200 bd(1) = Ad(1)
   GOTO 1700
 1300 bd(1) = Ad(1)
   bd(2) = Ad(2)
   GOTO 1700
 1400 bd(1) = A(1)
   GOTO 1700
 1500 bd(1) = A(1)
   bd(2) = A(2)
   GOTO 1700
 1600 b(1) = Ad(1)
   b(2) = Ad(2)
 1700 DO
!
      Nbrstr = Zz(Apoint+1)
      init = Zz(Apoint)
      Apoint = Apoint + 2
      j = Apoint
      IF ( Prca==2 ) j = j/2 + 1
      Apoint = Apoint + Nbrstr*Nwda
      irow = init*Rcd - Rcd + 1
      nrow = irow + Nbrstr*Rcd - 1
      DO k = irow , nrow , Rcd
         GOTO apick2
 1720    aa(1) = Z(j)
         GOTO 1860
 1740    aa(1) = Z(j)
         aa(2) = Z(j+1)
         GOTO 1860
 1760    add(1) = Zd(j)
         GOTO 1860
 1780    add(1) = Zd(j)
         add(2) = Zd(j+1)
         GOTO 1860
 1800    add(1) = Z(j)
         GOTO 1860
 1820    add(1) = Z(j)
         add(2) = Z(j+1)
         GOTO 1860
 1840    aa(1) = Zd(j)
         aa(2) = Zd(j+1)
!
 1860    GOTO arith2
 1880    Z(k) = Z(k) + aa(1)*b(1)
         GOTO 1960
 1900    Zd(k) = Zd(k) + add(1)*bd(1)
         GOTO 1960
 1920    Z(k) = Z(k) + aa(1)*b(1) - aa(2)*b(2)
         Z(k+1) = Z(k+1) + aa(1)*b(2) + aa(2)*b(1)
         GOTO 1960
 1940    Zd(k) = Zd(k) + add(1)*bd(1) - add(2)*bd(2)
         Zd(k+1) = Zd(k+1) + add(1)*bd(2) + add(2)*bd(1)
 1960    j = j + Rca
      ENDDO
      nbr = nbr - 1
      IF ( nbr<=0 ) EXIT
   ENDDO
!
 2000 l = l - 2
   Acol = Acol + 1
 2100 IF ( Eol==0 ) GOTO 900
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
   Dd(1) = 0.D0
   Dd(2) = 0.D0
   l = Firstl
   Apoint = Zz(l)
   Arow = Arow1
   IF ( Crow/=mask6f ) GOTO 2300
   GOTO 2500
 2200 IF ( Eol/=0 ) GOTO 2500
 2300 CALL zntpki
   Crow = I
 2400 Dd(1) = Ad(1)
   Dd(2) = Ad(2)
   IF ( Crow<Arow ) THEN
      Drow = Crow
      CALL zblpki
      GOTO 2200
   ELSEIF ( Crow==Arow ) THEN
      GOTO 2600
   ENDIF
 2500 Dd(1) = 0.D0
   Dd(2) = 0.D0
   IF ( Apoint==0 ) GOTO 5100
 2600 Drow = Arow
   IF ( Apoint==0 ) THEN
      CALL zblpki
      GOTO 5100
   ELSE
      Nbrstr = Zz(l-1)
   ENDIF
 2700 nbr = Zz(Apoint+1)
   nbr1 = nbr
   init = Zz(Apoint)
   Apoint = Apoint + 2
   j = Apoint
   IF ( Prca>1 ) j = j/2 + 1
   Apoint = Apoint + nbr*Nwda
   k = (init-1)*Rcb + 1
 2800 IF ( all/=0 ) THEN
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
            IF ( nbr<=0 ) GOTO 5000
         ENDDO
      ELSEIF ( all==2 ) THEN
         DO
!
            Dd(1) = Dd(1) + Zd(j)*Zd(k)
            j = j + Rca
            k = k + Rcb
            nbr = nbr - 1
            IF ( nbr<=0 ) GOTO 5000
         ENDDO
      ELSEIF ( all==3 ) THEN
         DO
!
            D(1) = D(1) + Z(j)*Z(k) - Z(j+1)*Z(k+1)
            D(2) = D(2) + Z(j)*Z(k+1) + Z(j+1)*Z(k)
            j = j + Rca
            k = k + Rcb
            nbr = nbr - 1
            IF ( nbr<=0 ) GOTO 5000
         ENDDO
      ELSEIF ( all==4 ) THEN
         DO
!
            Dd(1) = Dd(1) + Zd(j)*Zd(k) - Zd(j+1)*Zd(k+1)
            Dd(2) = Dd(2) + Zd(j)*Zd(k+1) + Zd(j+1)*Zd(k)
            j = j + Rca
            k = k + Rcb
            nbr = nbr - 1
            IF ( nbr<=0 ) GOTO 5000
         ENDDO
      ENDIF
   ENDIF
   GOTO bpick2
 2900 b(1) = Z(k)
   GOTO 3600
 3000 b(1) = Z(k)
   b(2) = Z(k+1)
   GOTO 3600
 3100 bd(1) = Zd(k)
   GOTO 3600
 3200 bd(1) = Zd(k)
   bd(2) = Zd(k+1)
   GOTO 3600
 3300 bd(1) = Z(k)
   GOTO 3600
 3400 bd(1) = Z(k)
   bd(2) = Z(k+1)
   GOTO 3600
 3500 b(1) = Zd(k)
   b(2) = Zd(k+1)
!
 3600 GOTO apick2
 3700 aa(1) = Z(j)
   GOTO 4400
 3800 aa(1) = Z(j)
   aa(2) = Z(j+1)
   GOTO 4400
 3900 add(1) = Zd(j)
   GOTO 4400
 4000 add(1) = Zd(j)
   add(2) = Zd(j+1)
   GOTO 4400
 4100 add(1) = Z(j)
   GOTO 4400
 4200 add(1) = Z(j)
   add(2) = Z(j+1)
   GOTO 4400
 4300 aa(1) = Z(j)
   aa(2) = Z(j+2)
!
 4400 GOTO arith2
 4500 D(1) = D(1) + aa(1)*b(1)
   GOTO 4900
 4600 Dd(1) = Dd(1) + add(1)*bd(1)
   GOTO 4900
 4700 D(1) = D(1) + aa(1)*b(1) - aa(2)*b(2)
   D(2) = D(2) + aa(1)*b(2) + aa(2)*b(1)
   GOTO 4900
 4800 Dd(1) = Dd(1) + add(1)*bd(1) - add(2)*bd(2)
   Dd(2) = Dd(2) + add(1)*bd(2) + add(2)*bd(1)
!
 4900 j = j + Rca
   k = k + Rcb
   nbr = nbr - 1
   IF ( nbr>0 ) GOTO 2800
 5000 Nbrstr = Nbrstr - 1
   IF ( Nbrstr>0 ) GOTO 2700
   CALL zblpki
 5100 l = l - 2
   Arow = Arow + 1
   IF ( Arow<=Arown ) THEN
      Apoint = Zz(l)
      IF ( Crow<Arow ) GOTO 2200
      IF ( Crow==Arow ) GOTO 2400
      GOTO 2500
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
   B3flag = -1
   CALL getstr(*5200,Block)
!IBMNB 6/93
   IF ( Block(2)/=Typeb ) THEN
      Typeb = Block(2)
      Rcb = Rc(Typeb)
      all = 0
   ENDIF
!IBMNE 6/93
   IF ( all/=0 ) THEN
      IF ( all==1 ) THEN
         DO
!
!     COMMON CASE (TYPEA=TYPEB=TYPED=PREC=1)
!
            Jb3n = Jb31 + Nterm3 - 1
            DO jb3 = Jb31 , Jb3n
               k = J3
               DO I = Arow1 , Arown
                  Dds(I) = Dds(I) + Aas(k)*Bbs(jb3)
                  k = k + Na
               ENDDO
               J3 = J3 + 1
            ENDDO
            CALL endget(Block)
            CALL getstr(*99999,Block)
         ENDDO
      ELSEIF ( all==2 ) THEN
         DO
!
!     COMMON CASE (TYPEA=TYPEB=TYPED=PREC=2)
!
            Jb3n = Jb31 + Nterm3 - 1
            DO jb3 = Jb31 , Jb3n
               k = J3
               DO I = Arow1 , Arown
                  Ddd(I) = Ddd(I) + Aad(k)*Bbd(jb3)
                  k = k + fa3
               ENDDO
               J3 = J3 + 1
            ENDDO
            CALL endget(Block)
            CALL getstr(*99999,Block)
         ENDDO
      ELSEIF ( all==3 ) THEN
!
!     COMMON CASE (TYPEA=TYPEB=TYPED=PREC=3)
!
         i1 = 2*Arow1 - 1
         in = 2*Arown - 1
         DO
            J3 = 2*J3 - 1
            Jb3n = Jb31 + Rcb*Nterm3 - Rcb
            DO jb3 = Jb31 , Jb3n , Rcb
               k = J3
               DO I = i1 , in , 2
                  Dds(I) = Dds(I) + Aas(k)*Bbs(jb3) - Aas(k+1)*Bbs(jb3+1)
                  Dds(I+1) = Dds(I+1) + Aas(k)*Bbs(jb3+1) + Aas(k+1)*Bbs(jb3)
                  k = k + Na
               ENDDO
               J3 = J3 + Rca
            ENDDO
            CALL endget(Block)
            CALL getstr(*99999,Block)
         ENDDO
      ELSEIF ( all==4 ) THEN
!
!     COMMON CASE (TYPEA=TYPEB=TYPED=PREC=4)
!
         inca = Rca*fa3
         i1 = 2*Arow1 - 1
         in = 2*Arown - 1
         DO
            J3 = 2*J3 - 1
            Jb3n = Jb31 + Rcb*Nterm3 - Rcb
            DO jb3 = Jb31 , Jb3n , Rcb
               k = J3
               DO I = i1 , in , 2
                  Ddd(I) = Ddd(I) + Aad(k)*Bbd(jb3) - Aad(k+1)*Bbd(jb3+1)
                  Ddd(I+1) = Ddd(I+1) + Aad(k)*Bbd(jb3+1) + Aad(k+1)*Bbd(jb3)
                  k = k + inca
               ENDDO
               J3 = J3 + Rca
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
      k1 = 2*(Prca-1) + Prc(Typeb)
      DO
         Jb3n = Jb31 + Nterm3 - 1
         DO jb3 = Jb31 , Jb3n
            k = J3
            IF ( k1==2 ) THEN
               DO I = Arow1 , Arown
                  Ddd(I) = Ddd(I) + Aas(k)*Bbd(jb3)
                  k = k + fa3
               ENDDO
            ELSEIF ( k1==3 ) THEN
               DO I = Arow1 , Arown
                  Ddd(I) = Ddd(I) + Aad(k)*Bbs(jb3)
                  k = k + fa3
               ENDDO
            ELSEIF ( k1==4 ) THEN
               DO I = Arow1 , Arown
                  Ddd(I) = Ddd(I) + Aad(k)*Bbd(jb3)
                  k = k + fa3
               ENDDO
            ELSE
               DO I = Arow1 , Arown
                  Ddd(I) = Ddd(I) + Aas(k)*Bbs(jb3)
                  k = k + fa3
               ENDDO
            ENDIF
            J3 = J3 + 1
         ENDDO
         CALL endget(Block)
         CALL getstr(*99999,Block)
      ENDDO
   ELSEIF ( Typed==3 ) THEN
!
!     PERFORM ARITHMETIC IN COMPLEX SINGLE PRECISION
!
      bsi = 0.
      i1 = 2*Arow1 - 1
      in = 2*Arown - 1
      DO
         IF ( Rca==2 ) J3 = 2*J3 - 1
         Jb3n = Jb31 + Rcb*Nterm3 - Rcb
         DO jb3 = Jb31 , Jb3n , Rcb
            bsr = Bbs(jb3)
            IF ( Rcb==2 ) bsi = Bbs(jb3+1)
            k = J3
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
            J3 = J3 + Rca
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
      i1 = 2*Arow1 - 1
      in = 2*Arown - 1
      DO
         IF ( Rca==2 ) J3 = 2*J3 - 1
         Jb3n = Jb31 + Rcb*Nterm3 - Rcb
         DO jb3 = Jb31 , Jb3n , Rcb
            k = J3
            IF ( Typeb==2 ) THEN
               bdr = Bbd(jb3)
            ELSEIF ( Typeb==3 ) THEN
               bdr = Bbs(jb3)
               bdi = Bbs(jb3+1)
            ELSEIF ( Typeb==4 ) THEN
               bdr = Bbd(jb3)
               bdi = Bbd(jb3+1)
            ELSE
               bdr = Bbs(jb3)
            ENDIF
            IF ( Typea==2 ) THEN
               DO I = i1 , in , 2
                  Ddd(I) = Ddd(I) + Aad(k)*bdr
                  Ddd(I+1) = Ddd(I+1) + Aad(k)*bdi
                  k = k + inca
               ENDDO
            ELSEIF ( Typea==3 ) THEN
               DO I = i1 , in , 2
                  Ddd(I) = Ddd(I) + Aas(k)*bdr - Aas(k+1)*bdi
                  Ddd(I+1) = Ddd(I+1) + Aas(k)*bdi + Aas(k+1)*bdr
                  k = k + inca
               ENDDO
            ELSEIF ( Typea==4 ) THEN
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
            J3 = J3 + Rca
         ENDDO
         CALL endget(Block)
         CALL getstr(*99999,Block)
      ENDDO
   ELSE
      DO
!
!     PERFORM ARITHMETIC IN REAL SINGLE PRECISION
!
         Jb3n = Jb31 + Nterm3 - 1
         DO jb3 = Jb31 , Jb3n
            k = J3
            bbb = Bbs(jb3)
            IF ( Block(2)==2 ) bbb = Bbd(jb3)
            IF ( Typea/=2 ) THEN
               DO I = Arow1 , Arown
                  Dds(I) = Dds(I) + Aas(k)*bbb
                  k = k + Na
               ENDDO
            ELSE
               DO I = Arow1 , Arown
                  aaa = Aad(k)
                  Dds(I) = Dds(I) + aaa*bbb
                  k = k + Na
               ENDDO
            ENDIF
            J3 = J3 + 1
         ENDDO
         CALL endget(Block)
         CALL getstr(*99999,Block)
      ENDDO
   ENDIF
!
 5200 RETURN 1
99999 RETURN
END SUBROUTINE mpyq
