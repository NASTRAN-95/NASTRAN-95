
SUBROUTINE mpydri(A,Da,B,Db,C,Dc)
!
!     SPECIAL MPYAD PERFORMS THE MATRIX OPERATION
!        (+/-)A   *B (+/-)C = D   OR
!        (+/-)A(T)*B (+/-)C = D
!
!     WHERE A, OR B IS , OR BOTH ARE, DIAGONAL, ROW VECTOR, OR IDENTITY
!     MATRIX.  MATRIX C CAN BE PURGED
!
!     THIS ROUITNE DOES NOT HANDEL A-TRANSPOSE, WHILE B IS DIAGNOL, ROW
!     VECTOR, OR IDENTIY MASTRIX. ONLY EXCEPTION IS A IS TRULY (Nx1).
!
!     NOTE -
!     1. IN NASTRAN GINO, THE TRAILER 2ND AND 3RD WORDS FOR A ROW-VECTOR
!        IS (1xM), AND THE DIAGONAL MATRIX IS ALSO (1xM)
!     2. THE ROW-VECTOR AND DIAGONAL MATRIX ARE PACKED IN ONE RECORD.
!        AND THUS, THEY REQUIRE SPECIAL ATTENTION DEALING WITH THE FILEB
!        WHILE FILEA IS ALREADY A ROW-VECTOR, OR A DIAGONAL MATRIX
!
!     WRITTEN BY G.CHAN/UNISYS,  1/92
!     LAST MODIFIED FOR SPECIAL CASES THAT INVOLVE B MATRIX IS ALSO
!     A DIAGONAL MATRIX OR A ROW-VECOTR,  2/93                 ----
!
   IMPLICIT NONE
   INTEGER Clsrew , Cold , Fa , Fb , Fc , Fd , Filea(7) , Fileb(7) , Filec(7) , Filed(7) , Forma , Formb , Formc , Formd , Incrp ,  &
         & Incru , Ip , Iscr , Iu , Jp , Ju , Lcore , Namea(7) , Nameat(7) , Nout , Nscr , Nzz , Prc(2) , Prec , Rd , Rdrew , Scr , &
         & Signab , Signc , Sysbuf , T , Typea , Typeb , Typec , Typed , Typep , Typeu , Typout , Words(4) , Wrt , Wrtrew
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /mpyadx/ Filea , Fileb , Filec , Filed , Nzz , T , Signab , Signc , Prec , Scr
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /packx / Typep , Typout , Ip , Jp , Incrp
   COMMON /system/ Sysbuf , Nout
   COMMON /trnspx/ Namea , Nameat , Lcore , Nscr , Iscr
   COMMON /type  / Prc , Words
   COMMON /unpakx/ Typeu , Iu , Ju , Incru
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   REAL A(1) , B(1) , C(1)
   DOUBLE PRECISION Da(1) , Db(1) , Dc(1)
   INTEGER ad(7) , buf1 , buf2 , buf3 , cola , colb , colb2 , colb4 , colc , diagnl , file , i , ident , j , je , k , kx , name(2) ,&
         & nwds , nz , rowa , rowa2 , rowb , rowb2 , rowc , rowd , rowd2 , rowvec , sd(7)
   !>>>>EQUIVALENCE (Filea(1),Fa) , (Filea(4),Forma) , (Filea(5),Typea) , (Fileb(1),Fb) , (Fileb(4),Formb) , (Fileb(5),Typeb) ,          &
!>>>>    & (Filec(1),Fc) , (Filec(4),Formc) , (Filec(5),Typec) , (Filed(1),Fd) , (Filed(2),Cold) , (Filed(4),Formd) , (Filed(5),Typed)
   DATA name/4HMPYA , 4HDRI / , diagnl , rowvec , ident/3 , 7 , 8/
!
!     MOVE TRUE ROWS AND COLUMNS INTO ROWA/B/C AND COLA/B/C
!
   cola = Filea(2)
   rowa = Filea(3)
   colb = Fileb(2)
   rowb = Fileb(3)
   colc = Filec(2)
   rowc = Filec(3)
   IF ( Forma==diagnl .OR. Forma==rowvec ) cola = rowa
   IF ( Forma==rowvec ) rowa = 1
   IF ( Formb==diagnl .OR. Formb==rowvec ) colb = rowb
   IF ( Formb==rowvec ) rowb = 1
   IF ( Formc==diagnl .OR. Formc==rowvec ) colc = rowc
   IF ( Formc==rowvec ) rowc = 1
!
   IF ( Signab==0 .AND. Fc==0 ) GOTO 99999
   IF ( Signab==0 .AND. Fc/=0 ) THEN
!
!     NULL MATRIX PRODUCT A*B, COPY FILEC TO FILED
!
      file = Fc
      CALL open(*900,Fc,A(buf1),Rdrew)
      file = Fd
      CALL open(*900,Fd,A(buf2),Wrtrew)
      CALL cpyfil(Fc,Fd,A(1),nz,k)
      CALL close(Fc,Clsrew)
      CALL close(Fd,Clsrew)
      DO i = 2 , 7
         Filed(i) = Filec(i)
      ENDDO
      GOTO 99999
   ELSE
      buf1 = Nzz - Sysbuf
      buf2 = buf1 - Sysbuf
      buf3 = buf2 - Sysbuf
      Cold = 0
      rowd = rowa
      IF ( T==1 ) rowd = cola
      IF ( Prec==1 .AND. (Typed==2 .OR. Typed==4) ) Typed = Typed - 1
      Typout = Typed
      nwds = Words(Typed)
      rowa2 = rowa*nwds
      rowb2 = rowb*nwds
      rowd2 = rowd*nwds
      colb2 = colb*2
      nz = buf3 - 1
      sd(1) = Scr
      IF ( Fc==0 ) THEN
         sd(1) = Fd
         nz = buf2 - 1
      ENDIF
      CALL makmcb(sd,sd,rowd,Formd,Typed)
!
!     REMEMBER, ONLY FILEA CAN HAVE TRANSPOSE, NOT FILEB.
!     IF FILEA IS DIAGONAL, ROW VECTOR, OR IDENTITY MATRIX, THE ACTUAL
!     TRANSPOSE OF FILEA HAS NEVER TAKEN PLACE.
!
!     FA, FB, FC AND FD ARE FILEA, FILEB, FILEC AND FILED RESPECTIVELY.
!     AD(1) IS EITHER FILEA OR FILED, AND
!     SD(1) IS EITHER SCRATCH FILE OR FILED
!
      IF ( T==1 ) THEN
         DO i = 1 , 7
            ad(i) = Filed(i)
         ENDDO
      ELSE
         DO i = 1 , 7
            ad(i) = Filea(i)
         ENDDO
      ENDIF
      Ip = 1
      Jp = rowd
      Incrp = 1
      Iu = 1
      Incru = 1
      IF ( Fa>0 ) THEN
         file = Fa
         CALL open(*900,Fa,A(buf1),Rdrew)
         CALL fwdrec(*1000,Fa)
      ENDIF
      IF ( Fb>0 ) THEN
         file = Fb
         CALL open(*900,Fb,A(buf2),Rdrew)
         CALL fwdrec(*1000,Fb)
      ENDIF
!
      IF ( Fa>0 ) THEN
         IF ( Forma==diagnl ) THEN
!
!                                         D   G   J   M
!     FILEA IS                            E   H   K   N
!     DIAGONAL -                          F   I   L   O
!                      a      a  0  0    aD  aG  aJ  aM
!                      b      0  b  0    bE  bH  bK  bN
!                      c ==>  0  0  c    cF  cI  cL  cO
!
!     SPECIAL CASE NEEDS TO BE CONSIDERED -
!     FILEB IS ALSO A DIAGONAL MATRIX. (FILEB CANNOT BE A ROW VECTOR)
!
            file = Fa
            Ju = rowa
            Typeu = Typed*Signab
            CALL unpack(*1200,Fa,A)
            CALL close(Fa,Clsrew)
            CALL gopen(sd,A(buf1),Wrtrew)
            file = Fb
            Ju = rowb
            Typeu = Typed
            IF ( Formb==diagnl ) THEN
!
!     SPECIAL CASE - FILEB IS ALSO A DIAGONAL MATRIX
!
               CALL unpack(*1200,Fb,B)
               IF ( Typeb>=3 ) THEN
!
                  DO j = 1 , rowb2
                     C(j) = 0.0
                  ENDDO
                  DO j = 1 , rowb2 , 2
                     IF ( Typeb==4 ) THEN
                        Dc(j) = Da(j)*Db(j) - Da(j+1)*Db(j+1)
                        Dc(j+1) = Da(j)*Db(j+1) + Da(j+1)*Db(j)
                        CALL pack(C,sd,sd)
                        Dc(j) = 0.0D+0
                        Dc(j+1) = 0.0D+0
                     ELSE
                        C(j) = A(j)*B(j) - A(j+1)*B(j+1)
                        C(j+1) = A(j)*B(j+1) + A(j+1)*B(j)
                        CALL pack(C,sd,sd)
                        C(j) = 0.0
                        C(j+1) = 0.0
                     ENDIF
                  ENDDO
               ELSE
                  DO j = 1 , rowb
                     C(j) = 0.0
                  ENDDO
                  DO j = 1 , rowb
                     IF ( Typeb==1 ) C(j) = A(j)*B(j)
                     IF ( Typeb==2 ) Dc(j) = Da(j)*Db(j)
                     CALL pack(C,sd,sd)
                     C(j) = 0.0
                     IF ( Typeb==2 ) Dc(j) = 0.0D+0
                  ENDDO
               ENDIF
            ELSE
               DO i = 1 , colb
                  CALL unpack(*1200,Fb,B)
                  IF ( Typeb==2 ) THEN
                     DO j = 1 , rowb
                        Dc(j) = Da(j)*Db(j)
                     ENDDO
                  ELSEIF ( Typeb==3 ) THEN
                     DO j = 1 , rowb2 , 2
                        C(j) = A(j)*B(j) - A(j+1)*B(j+1)
                        C(j+1) = A(j)*B(j+1) + A(j+1)*B(j)
                     ENDDO
                  ELSEIF ( Typeb==4 ) THEN
                     DO j = 1 , rowb2 , 2
                        Dc(j) = Da(j)*Db(j) - Da(j+1)*Db(j+1)
                        Dc(j+1) = Da(j)*Db(j+1) + Da(j+1)*Db(j)
                     ENDDO
                  ELSE
                     DO j = 1 , rowb
                        C(j) = A(j)*B(j)
                     ENDDO
                  ENDIF
                  CALL pack(C,sd,sd)
               ENDDO
            ENDIF
!
            CALL close(Fb,Clsrew)
            CALL close(sd,Clsrew)
            GOTO 600
         ELSEIF ( Forma==rowvec ) THEN
!                                         E       I      M
!     FILEA IS A ROW     a                F       J      N
!     VECTOR -           b                G       K      O
!     RESULT IN FILED,   c                H       L      P
!     A (Nx1) RECT.      d ==> a b c d  aE+bF+  aI+bJ+  aM+bN+
!     MATRIX or A ROW-                  cG+dH   cK+dL   cO+dP
!     VECTOR
!
!     SPECIAL CASE NEEDS TO BE CONSIDERED -
!     FILEB IS A DIAGONAL MATRIX. (FILEB CANNOT BE A ROW VECTOR)
!
!
!     TRANSPOSE OF FILEA,                 E       F       G
!     A ROW VECTOR -               a     aE      aF      aG
!                                  b     bE      bF      bG
!                                  c     cE      cF      cG
!                                  d     dE      dF      dG
!
!     SPECIAL CASES NEED TO BE CONSIDERED -
!     FILEB MUST BE A (Nx1) RECTANGULAR MATRIX, OR A ROW VECTOR
!
            file = Fa
            Ju = rowa
            Typeu = Typed*Signab
            CALL unpack(*1200,Fa,A)
            CALL close(Fa,Clsrew)
            CALL gopen(sd,A(buf1),Wrtrew)
            file = Fb
            Typeu = Typed
            IF ( T/=1 ) THEN
!
!     FILEA IS A ROW-VECTOR, RESULT IS ALSO A ROW-VECTOR, OR A
!     (Nx1) RECTANGULAR MATRIX
!
               Ju = rowb
               IF ( Formb==diagnl ) THEN
!
!     SPECIAL CASE - FILEB IS A DIAGONAL MATRIX.
!
                  CALL unpack(*1200,Fb,B)
                  IF ( Typeb==2 ) THEN
                     DO j = 1 , colb
                        Dc(j) = Da(j)*Db(j)
                     ENDDO
                  ELSEIF ( Typeb==3 ) THEN
                     GOTO 100
                  ELSEIF ( Typeb==4 ) THEN
                     GOTO 200
                  ELSE
                     DO j = 1 , colb
                        C(j) = A(j)*B(j)
                     ENDDO
                  ENDIF
                  GOTO 300
               ELSE
                  IF ( rowb/=rowa ) GOTO 1100
                  colb4 = colb*4
                  DO j = 1 , colb4
                     C(j) = 0.0
                  ENDDO
                  DO j = 1 , colb
                     CALL unpack(*100,Fb,B)
                     IF ( Typeb==2 ) THEN
                        DO k = 1 , rowb
                           Dc(j) = Dc(j) + Da(k)*Db(k)
                        ENDDO
                     ELSEIF ( Typeb==3 ) THEN
                        DO k = 1 , rowb2 , 2
                           C(j) = C(j) + A(k)*B(k) - A(k+1)*B(k+1)
                           C(j+1) = C(j+1) + A(k)*B(k+1) + A(k+1)*B(k)
                        ENDDO
                     ELSEIF ( Typeb==4 ) THEN
                        DO k = 1 , rowb , 2
                           Dc(j) = Dc(j) + Da(k)*Db(k) - Da(k+1)*Db(k+1)
                           Dc(j+1) = Dc(j+1) + Da(k)*Db(k+1) + Da(k+1)*Db(k)
                        ENDDO
                     ELSE
                        DO k = 1 , rowb
                           C(j) = C(j) + A(k)*B(k)
                        ENDDO
                     ENDIF
                  ENDDO
                  GOTO 200
               ENDIF
!
!     FILEA (A ROW VECTOR) TRANSFPOSE
!
            ELSEIF ( Formb==rowvec ) THEN
!
!     SPECAIL CASE - FILE B IS A ROW VECTOR
!
               IF ( rowb/=1 ) GOTO 1100
               Ju = colb
               CALL unpack(*1100,Fb,B(1))
               CALL close(Fb,Clsrew)
               GOTO 500
            ELSE
               IF ( rowb/=1 ) GOTO 1100
               Iu = 0
               j = 1
               DO i = 1 , rowb
                  CALL unpack(*2,Fb,B(j))
                  IF ( Iu==i ) GOTO 4
                  GOTO 1100
 2                je = j + nwds
                  DO k = j , je
                     B(k) = 0.0
                  ENDDO
 4                j = j + nwds
               ENDDO
               CALL close(Fb,Clsrew)
               Iu = 1
               GOTO 500
            ENDIF
         ELSEIF ( Forma==ident ) THEN
!
!     FILEA IS IDENTITY -
!
!     SPECIAL CASEs NEED TO BE CONSIDERED -
!     SIGNAB IS NEGATIVE, OR FILEB IS A DIAGONAL MATRIX
!     (FILEB CANNOT BE A ROW-VECTOR)
!
            CALL close(Fa,Clsrew)
            IF ( Formb==diagnl .OR. Signab<0 ) THEN
!
!     SPECIAL CASE - FILEB IS A DIAGONAL MATRIX
!                    OR SIGNAB IS NEGATIVE
!
               CALL gopen(sd,A(buf1),Wrtrew)
               Ju = rowb
               file = Fb
               Typeu = Typed*Signab
               IF ( Formb/=diagnl ) THEN
!
!     SPECIAL CASE - SIGNAB IS NEGATIVE
!
                  file = Fb
                  DO i = 1 , colb
                     CALL unpack(*1200,Fb,B)
                     CALL pack(B,sd,sd)
                  ENDDO
                  CALL close(sd,Clsrew)
                  CALL close(Fb,Clsrew)
                  IF ( Fc==0 ) GOTO 700
                  GOTO 600
               ELSE
                  CALL unpack(*1200,Fb,B)
                  CALL close(Fb,Clsrew)
                  j = 1
                  DO i = 1 , rowa
                     Ip = i
                     Jp = i
                     CALL pack(B(j),sd,sd)
                     j = j + nwds
                  ENDDO
                  CALL close(sd,Clsrew)
                  IF ( Fc==0 ) GOTO 700
                  GOTO 600
               ENDIF
            ELSE
               file = sd(1)
               CALL open(*900,Fa,A(buf1),Wrtrew)
               CALL rewind(Fb)
               CALL cpyfil(Fb,sd,A(1),nz,k)
               CALL close(Fb,Clsrew)
               CALL close(sd,Clsrew)
               IF ( Fc==0 ) THEN
                  DO i = 2 , 7
                     Filed(i) = Fileb(i)
                  ENDDO
                  GOTO 99999
               ELSE
                  DO i = 2 , 7
                     sd(i) = Fileb(i)
                  ENDDO
                  GOTO 600
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF ( T==1 ) THEN
!
!     ERROR
!
         WRITE (Nout,99001) Sfm
99001    FORMAT (A25,'. MPYDRI DOES NOT HANDLE A-TRANSPOSE. SHOULD NOT BE',' CALLED BY MPYAD')
         GOTO 1300
      ELSEIF ( Formb==diagnl ) THEN
!
!     FILEA IS A COLUMN MATRIX -
!     i.e. A (1,N) RECTANGULAR MATRIX OR A (Nx1) TRANSPOSE
!
!     FILEB MUST BE A (Nx1) RECTANGULAR MATRIX
!
!     CURRENTLY THIS CASE IS HANDLED IN MPYAD SUBROUINTE
!
!     HOWEVER, IF FILEB IS A ROW VECTOR,  IT IS HANDLED IN 600
!     IF FILEA IS A ROW VECTOR TRANSPOSE, IT IS HANDLED IN 200/350
!
! 440 CONTINUE
!
!                                         X   0   0      X
!     FILEB IS DIAGONAL -                 0   Y   0      Y
!                                         0   0   Z <==  Z
!                             a  e  i    aX  eY  iZ
!                             b  f  j    bX  fY  jZ
!                             c  g  k    cX  gY  kZ
!                             d  h  l    dX  hY  lZ
!
         file = Fb
         Ju = colb
         Typeu = Typed*Signab
         CALL unpack(*1200,Fb,B)
         CALL close(Fb,Clsrew)
         CALL gopen(sd,A(buf2),Wrtrew)
         file = Fa
         Ju = rowa
         Typeu = Typed
         DO i = 1 , cola
            CALL unpack(*1200,Fa,A)
            IF ( Typeb==2 ) THEN
               DO j = 1 , rowa
                  Dc(j) = Da(j)*Db(i)
               ENDDO
            ELSEIF ( Typeb==3 ) THEN
               DO j = 1 , rowa2 , 2
                  C(j) = A(j)*B(j) - A(j+1)*B(j+1)
                  C(j+1) = A(j)*B(j+1) + A(j+1)*B(j)
               ENDDO
            ELSEIF ( Typeb==4 ) THEN
               DO j = 1 , rowa2 , 2
                  Dc(j) = Da(j)*Db(j) - Da(j+1)*Db(j+1)
                  Dc(j+1) = Da(j)*Db(j+1) + Da(j+1)*Db(j)
               ENDDO
            ELSE
               DO j = 1 , rowa
                  C(j) = A(j)*B(i)
               ENDDO
            ENDIF
            CALL pack(C,sd,sd)
         ENDDO
         CALL close(ad,Clsrew)
         CALL close(sd,Clsrew)
         GOTO 600
      ELSEIF ( Formb==rowvec ) THEN
!
!     FILEB IS A ROW VECTOR -                            E
!                                                        F
!     NOTE - FILEA MUST BE A               E   F   G <== G
!     ONE-COLUMN MATRIX.             a    aE  aF  aG
!     i.e. A(1xN) OR                 b    bE  bF  bG
!          A(Nx1) TRNASPOSE          c    cE  cF  cG
!                                    d    dE  dF  dG
!     WE ALREADY HANDLED THE CASE
!     WHERE FILEA IS A ROW-VECTOR TRANSPOSE IN 200
!
         file = Fb
         Ju = colb
         Typeu = Typed*Signab
         IF ( T==1 ) THEN
            IF ( rowa/=1 ) GOTO 1100
            j = cola*nwds
            DO i = 1 , j
               B(i) = 0.0
            ENDDO
            j = 1
            DO i = 1 , cola
               CALL unpack(*5,Fb,B(j))
 5             j = j + nwds
            ENDDO
            GOTO 400
         ELSE
            IF ( cola/=1 ) GOTO 1100
            CALL unpack(*1200,Fb,B)
            GOTO 400
         ENDIF
      ELSEIF ( Formb==ident ) THEN
!
!     FILEB IS IDENTITY -
!
!     SPECIAL CASE NEEDS TO BE CONSIDERED -
!     NEGATIVE SIGNAB
!
         CALL close(Fb,Clsrew)
         file = sd(1)
         CALL open(*900,sd,A(buf2),Wrtrew)
         IF ( Signab<0 ) THEN
!
            Typeu = Typed*Signab
            Ju = rowa
            file = Fa
            DO i = 1 , cola
               CALL unpack(*1200,Fa,A)
               CALL pack(A,sd,sd)
            ENDDO
         ELSE
            CALL rewind(Fa)
            CALL cpyfil(Fa,sd,A(1),nz,k)
         ENDIF
         CALL close(Fa,Clsrew)
         CALL close(sd,Clsrew)
         IF ( Fc==0 ) GOTO 700
         GOTO 600
      ELSE
         file = 0
         GOTO 1300
      ENDIF
   ENDIF
 100  DO j = 1 , colb2 , 2
      C(j) = A(j)*B(j) - A(j+1)*B(j+1)
      C(j+1) = A(j)*B(j+1) + A(j+1)*B(j)
   ENDDO
   GOTO 300
 200  DO j = 1 , colb2 , 2
      Dc(j) = Da(j)*Db(j) - Da(j+1)*Db(j+1)
      Dc(j+1) = Da(j)*Db(j+1) + Da(j+1)*Db(j)
   ENDDO
!
 300  CALL close(Fb,Clsrew)
   IF ( Fc/=0 ) THEN
      file = Fc
      Typeu = Typec*Signc
      CALL gopen(Fc,A(buf2),Rdrew)
      IF ( Formc/=rowvec ) THEN
         Ip = 1
         Jp = 1
         DO j = 1 , colc
            CALL unpack(*310,Fc,A(j*nwds-1))
            CYCLE
 310        A(j*nwds-1) = 0.
            A(j*nwds) = 0.
         ENDDO
      ELSE
         CALL unpack(*1200,Fc,A(1))
      ENDIF
!
      CALL close(Fc,Clsrew)
      IF ( Typed==2 ) THEN
         DO j = 1 , rowd2
            Dc(j) = Dc(j) + Da(j)
         ENDDO
      ELSE
         DO j = 1 , rowd2
            C(j) = C(j) + A(j)
         ENDDO
      ENDIF
   ENDIF
!
   CALL pack(C,sd,sd)
   Formd = rowvec
   GOTO 800
 400  CALL close(Fb,Clsrew)
   file = Fa
   Ju = rowa
   Typeu = Typed
   CALL unpack(*1200,Fa,A)
   CALL close(ad,Clsrew)
   CALL gopen(Fd,A(buf1),Wrtrew)
 500  DO j = 1 , colb
      IF ( Typea==2 ) THEN
         DO i = 1 , rowa
            Da(i) = Da(i)*Db(j)
         ENDDO
      ELSEIF ( Typea==3 ) THEN
         DO i = 1 , rowa2 , 2
            C(i) = A(i)*B(j) - A(i+1)*B(j+1)
            C(i+1) = A(i)*B(j+1) + A(i+1)*B(j)
         ENDDO
      ELSEIF ( Typea==4 ) THEN
         DO i = 1 , rowa2 , 2
            Dc(i) = Da(i)*Db(j) - Da(i+1)*Db(j+1)
            Dc(i+1) = Da(i)*Db(j+1) + Da(i+1)*Db(j)
            kx = kx + nwds
         ENDDO
      ELSE
         DO i = 1 , rowa
            C(i) = A(i)*B(j)
         ENDDO
      ENDIF
      CALL pack(C,Fd,Filed)
   ENDDO
   CALL close(Fd,Clsrew)
!
!     ADD PRODUCT OF A*B TO C
!
 600  IF ( Fc/=0 ) THEN
      CALL gopen(Fd,A(buf3),Wrtrew)
      file = Fc
      CALL open(*900,Fc,A(buf2),Rdrew)
      CALL fwdrec(*1000,Fc)
      file = sd(1)
      CALL open(*900,sd,A(buf1),Rdrew)
      CALL fwdrec(*1000,sd)
      Ju = rowc
      Typep = Typed
      DO i = 1 , colc
         Typeu = Typed*Signc
         CALL unpack(*620,Fc,C)
         GOTO 640
 620     DO j = 1 , rowd2
            C(j) = 0.0
         ENDDO
 640     Typeu = Typed
         CALL unpack(*660,sd,B)
         GOTO 680
 660     DO j = 1 , rowd2
            B(j) = 0.0
         ENDDO
 680     IF ( Typed==2 .OR. Typed==4 ) THEN
            DO j = 1 , rowd2
               Da(j) = Db(j) + Dc(j)
            ENDDO
         ELSE
            DO j = 1 , rowd2
               A(j) = B(j) + C(j)
            ENDDO
         ENDIF
         CALL pack(A,Fd,Filed)
      ENDDO
      CALL close(Fc,Clsrew)
      CALL close(sd,Clsrew)
   ENDIF
!
 700  IF ( Cold==0 ) THEN
      DO i = 2 , 7
         Filed(i) = sd(i)
      ENDDO
   ENDIF
 800  CALL close(Fd,Clsrew)
   CALL wrttrl(Filed)
   GOTO 99999
 900  j = -1
   GOTO 1400
 1000 j = -2
   GOTO 1400
 1100 WRITE (Nout,99002) Ufm
99002 FORMAT (A23,' FROM MPYAD/MPYDRI.  FILES NOT COMPATIBLE')
   GOTO 1300
 1200 WRITE (Nout,99003) Ufm
99003 FORMAT (A23,' FROM MPYAD/MPYDRI.  NULL COLUMN ENCOUNTERED DURING',' MATRIX UNPACK')
 1300 j = -37
 1400 CALL mesage(j,file,name)
!
99999 RETURN
END SUBROUTINE mpydri