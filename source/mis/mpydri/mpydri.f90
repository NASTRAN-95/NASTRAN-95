!*==mpydri.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
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
USE C_MPYADX
USE C_NAMES
USE C_PACKX
USE C_SYSTEM
USE C_TRNSPX
USE C_TYPE
USE C_UNPAKX
USE C_XMSSG
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: A
   REAL(REAL64) , DIMENSION(1) :: Da
   REAL , DIMENSION(1) :: B
   REAL(REAL64) , DIMENSION(1) :: Db
   REAL , DIMENSION(1) :: C
   REAL(REAL64) , DIMENSION(1) :: Dc
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: ad , sd
   INTEGER :: buf1 , buf2 , buf3 , cola , colb , colb2 , colb4 , colc , cold , fa , fb , fc , fd , file , forma , formb , formc ,   &
            & formd , i , j , je , k , kx , nwds , nz , rowa , rowa2 , rowb , rowb2 , rowc , rowd , rowd2 , typea , typeb , typec , &
            & typed
   INTEGER , SAVE :: diagnl , ident , rowvec
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , cpyfil , fwdrec , gopen , makmcb , mesage , open , pack , rewind , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   !>>>>EQUIVALENCE (Filea(1),Fa) , (Filea(4),Forma) , (Filea(5),Typea) , (Fileb(1),Fb) , (Fileb(4),Formb) , (Fileb(5),Typeb) ,          &
!>>>>    & (Filec(1),Fc) , (Filec(4),Formc) , (Filec(5),Typec) , (Filed(1),Fd) , (Filed(2),Cold) , (Filed(4),Formd) , (Filed(5),Typed)
   DATA name/4HMPYA , 4HDRI / , diagnl , rowvec , ident/3 , 7 , 8/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     MOVE TRUE ROWS AND COLUMNS INTO ROWA/B/C AND COLA/B/C
!
         cola = Filea(2)
         rowa = Filea(3)
         colb = Fileb(2)
         rowb = Fileb(3)
         colc = Filec(2)
         rowc = Filec(3)
         IF ( forma==diagnl .OR. forma==rowvec ) cola = rowa
         IF ( forma==rowvec ) rowa = 1
         IF ( formb==diagnl .OR. formb==rowvec ) colb = rowb
         IF ( formb==rowvec ) rowb = 1
         IF ( formc==diagnl .OR. formc==rowvec ) colc = rowc
         IF ( formc==rowvec ) rowc = 1
!
         IF ( Signab==0 .AND. fc==0 ) RETURN
         IF ( Signab==0 .AND. fc/=0 ) THEN
!
!     NULL MATRIX PRODUCT A*B, COPY FILEC TO FILED
!
            file = fc
            CALL open(*40,fc,A(buf1),Rdrew)
            file = fd
            CALL open(*40,fd,A(buf2),Wrtrew)
            CALL cpyfil(fc,fd,A(1),nz,k)
            CALL close(fc,Clsrew)
            CALL close(fd,Clsrew)
            DO i = 2 , 7
               Filed(i) = Filec(i)
            ENDDO
            RETURN
         ELSE
            buf1 = Nzz - Sysbuf
            buf2 = buf1 - Sysbuf
            buf3 = buf2 - Sysbuf
            cold = 0
            rowd = rowa
            IF ( T==1 ) rowd = cola
            IF ( Prec==1 .AND. (typed==2 .OR. typed==4) ) typed = typed - 1
            Typout = typed
            nwds = Words(typed)
            rowa2 = rowa*nwds
            rowb2 = rowb*nwds
            rowd2 = rowd*nwds
            colb2 = colb*2
            nz = buf3 - 1
            sd(1) = Scr
            IF ( fc==0 ) THEN
               sd(1) = fd
               nz = buf2 - 1
            ENDIF
            CALL makmcb(sd,sd,rowd,formd,typed)
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
            IF ( fa>0 ) THEN
               file = fa
               CALL open(*40,fa,A(buf1),Rdrew)
               CALL fwdrec(*60,fa)
            ENDIF
            IF ( fb>0 ) THEN
               file = fb
               CALL open(*40,fb,A(buf2),Rdrew)
               CALL fwdrec(*60,fb)
            ENDIF
!
            IF ( fa>0 ) THEN
               IF ( forma==diagnl ) THEN
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
                  file = fa
                  Ju = rowa
                  Typeu = typed*Signab
                  CALL unpack(*100,fa,A)
                  CALL close(fa,Clsrew)
                  CALL gopen(sd,A(buf1),Wrtrew)
                  file = fb
                  Ju = rowb
                  Typeu = typed
                  IF ( formb==diagnl ) THEN
!
!     SPECIAL CASE - FILEB IS ALSO A DIAGONAL MATRIX
!
                     CALL unpack(*100,fb,B)
                     IF ( typeb>=3 ) THEN
!
                        DO j = 1 , rowb2
                           C(j) = 0.0
                        ENDDO
                        DO j = 1 , rowb2 , 2
                           IF ( typeb==4 ) THEN
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
                           IF ( typeb==1 ) C(j) = A(j)*B(j)
                           IF ( typeb==2 ) Dc(j) = Da(j)*Db(j)
                           CALL pack(C,sd,sd)
                           C(j) = 0.0
                           IF ( typeb==2 ) Dc(j) = 0.0D+0
                        ENDDO
                     ENDIF
                  ELSE
                     DO i = 1 , colb
                        CALL unpack(*100,fb,B)
                        IF ( typeb==2 ) THEN
                           DO j = 1 , rowb
                              Dc(j) = Da(j)*Db(j)
                           ENDDO
                        ELSEIF ( typeb==3 ) THEN
                           DO j = 1 , rowb2 , 2
                              C(j) = A(j)*B(j) - A(j+1)*B(j+1)
                              C(j+1) = A(j)*B(j+1) + A(j+1)*B(j)
                           ENDDO
                        ELSEIF ( typeb==4 ) THEN
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
                  CALL close(fb,Clsrew)
                  CALL close(sd,Clsrew)
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( forma==rowvec ) THEN
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
                  file = fa
                  Ju = rowa
                  Typeu = typed*Signab
                  CALL unpack(*100,fa,A)
                  CALL close(fa,Clsrew)
                  CALL gopen(sd,A(buf1),Wrtrew)
                  file = fb
                  Typeu = typed
                  IF ( T/=1 ) THEN
!
!     FILEA IS A ROW-VECTOR, RESULT IS ALSO A ROW-VECTOR, OR A
!     (Nx1) RECTANGULAR MATRIX
!
                     Ju = rowb
                     IF ( formb==diagnl ) THEN
!
!     SPECIAL CASE - FILEB IS A DIAGONAL MATRIX.
!
                        CALL unpack(*100,fb,B)
                        IF ( typeb==2 ) THEN
                           DO j = 1 , colb
                              Dc(j) = Da(j)*Db(j)
                           ENDDO
                        ELSEIF ( typeb==3 ) THEN
                           GOTO 20
                        ELSEIF ( typeb==4 ) THEN
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
                        ELSE
                           DO j = 1 , colb
                              C(j) = A(j)*B(j)
                           ENDDO
                        ENDIF
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        IF ( rowb/=rowa ) GOTO 80
                        colb4 = colb*4
                        DO j = 1 , colb4
                           C(j) = 0.0
                        ENDDO
                        DO j = 1 , colb
                           CALL unpack(*20,fb,B)
                           IF ( typeb==2 ) THEN
                              DO k = 1 , rowb
                                 Dc(j) = Dc(j) + Da(k)*Db(k)
                              ENDDO
                           ELSEIF ( typeb==3 ) THEN
                              DO k = 1 , rowb2 , 2
                                 C(j) = C(j) + A(k)*B(k) - A(k+1)*B(k+1)
                                 C(j+1) = C(j+1) + A(k)*B(k+1) + A(k+1)*B(k)
                              ENDDO
                           ELSEIF ( typeb==4 ) THEN
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
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
!     FILEA (A ROW VECTOR) TRANSFPOSE
!
                  ELSEIF ( formb==rowvec ) THEN
!
!     SPECAIL CASE - FILE B IS A ROW VECTOR
!
                     IF ( rowb/=1 ) GOTO 80
                     Ju = colb
                     CALL unpack(*80,fb,B(1))
                     CALL close(fb,Clsrew)
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     IF ( rowb/=1 ) GOTO 80
                     Iu = 0
                     j = 1
                     DO i = 1 , rowb
                        spag_nextblock_2 = 1
                        SPAG_DispatchLoop_2: DO
                           SELECT CASE (spag_nextblock_2)
                           CASE (1)
                              CALL unpack(*2,fb,B(j))
                              IF ( Iu/=i ) GOTO 80
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
 2                            je = j + nwds
                              DO k = j , je
                                 B(k) = 0.0
                              ENDDO
                              spag_nextblock_2 = 2
                           CASE (2)
                              j = j + nwds
                              EXIT SPAG_DispatchLoop_2
                           END SELECT
                        ENDDO SPAG_DispatchLoop_2
                     ENDDO
                     CALL close(fb,Clsrew)
                     Iu = 1
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSEIF ( forma==ident ) THEN
!
!     FILEA IS IDENTITY -
!
!     SPECIAL CASEs NEED TO BE CONSIDERED -
!     SIGNAB IS NEGATIVE, OR FILEB IS A DIAGONAL MATRIX
!     (FILEB CANNOT BE A ROW-VECTOR)
!
                  CALL close(fa,Clsrew)
                  IF ( formb==diagnl .OR. Signab<0 ) THEN
!
!     SPECIAL CASE - FILEB IS A DIAGONAL MATRIX
!                    OR SIGNAB IS NEGATIVE
!
                     CALL gopen(sd,A(buf1),Wrtrew)
                     Ju = rowb
                     file = fb
                     Typeu = typed*Signab
                     IF ( formb/=diagnl ) THEN
!
!     SPECIAL CASE - SIGNAB IS NEGATIVE
!
                        file = fb
                        DO i = 1 , colb
                           CALL unpack(*100,fb,B)
                           CALL pack(B,sd,sd)
                        ENDDO
                        CALL close(sd,Clsrew)
                        CALL close(fb,Clsrew)
                        IF ( fc/=0 ) THEN
                           spag_nextblock_1 = 6
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        CALL unpack(*100,fb,B)
                        CALL close(fb,Clsrew)
                        j = 1
                        DO i = 1 , rowa
                           Ip = i
                           Jp = i
                           CALL pack(B(j),sd,sd)
                           j = j + nwds
                        ENDDO
                        CALL close(sd,Clsrew)
                        IF ( fc/=0 ) THEN
                           spag_nextblock_1 = 6
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ELSE
                     file = sd(1)
                     CALL open(*40,fa,A(buf1),Wrtrew)
                     CALL rewind(fb)
                     CALL cpyfil(fb,sd,A(1),nz,k)
                     CALL close(fb,Clsrew)
                     CALL close(sd,Clsrew)
                     IF ( fc==0 ) THEN
                        DO i = 2 , 7
                           Filed(i) = Fileb(i)
                        ENDDO
                        RETURN
                     ELSE
                        DO i = 2 , 7
                           sd(i) = Fileb(i)
                        ENDDO
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
            IF ( T==1 ) THEN
!
!     ERROR
!
               WRITE (Nout,99001) Sfm
99001          FORMAT (A25,'. MPYDRI DOES NOT HANDLE A-TRANSPOSE. SHOULD NOT BE',' CALLED BY MPYAD')
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( formb==diagnl ) THEN
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
               file = fb
               Ju = colb
               Typeu = typed*Signab
               CALL unpack(*100,fb,B)
               CALL close(fb,Clsrew)
               CALL gopen(sd,A(buf2),Wrtrew)
               file = fa
               Ju = rowa
               Typeu = typed
               DO i = 1 , cola
                  CALL unpack(*100,fa,A)
                  IF ( typeb==2 ) THEN
                     DO j = 1 , rowa
                        Dc(j) = Da(j)*Db(i)
                     ENDDO
                  ELSEIF ( typeb==3 ) THEN
                     DO j = 1 , rowa2 , 2
                        C(j) = A(j)*B(j) - A(j+1)*B(j+1)
                        C(j+1) = A(j)*B(j+1) + A(j+1)*B(j)
                     ENDDO
                  ELSEIF ( typeb==4 ) THEN
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
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( formb==rowvec ) THEN
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
               file = fb
               Ju = colb
               Typeu = typed*Signab
               IF ( T==1 ) THEN
                  IF ( rowa/=1 ) GOTO 80
                  j = cola*nwds
                  DO i = 1 , j
                     B(i) = 0.0
                  ENDDO
                  j = 1
                  DO i = 1 , cola
                     CALL unpack(*4,fb,B(j))
 4                   j = j + nwds
                  ENDDO
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  IF ( cola/=1 ) GOTO 80
                  CALL unpack(*100,fb,B)
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( formb==ident ) THEN
!
!     FILEB IS IDENTITY -
!
!     SPECIAL CASE NEEDS TO BE CONSIDERED -
!     NEGATIVE SIGNAB
!
               CALL close(fb,Clsrew)
               file = sd(1)
               CALL open(*40,sd,A(buf2),Wrtrew)
               IF ( Signab<0 ) THEN
!
                  Typeu = typed*Signab
                  Ju = rowa
                  file = fa
                  DO i = 1 , cola
                     CALL unpack(*100,fa,A)
                     CALL pack(A,sd,sd)
                  ENDDO
               ELSE
                  CALL rewind(fa)
                  CALL cpyfil(fa,sd,A(1),nz,k)
               ENDIF
               CALL close(fa,Clsrew)
               CALL close(sd,Clsrew)
               IF ( fc/=0 ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
               file = 0
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
 20      DO j = 1 , colb2 , 2
            C(j) = A(j)*B(j) - A(j+1)*B(j+1)
            C(j+1) = A(j)*B(j+1) + A(j+1)*B(j)
         ENDDO
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         DO j = 1 , colb2 , 2
            Dc(j) = Da(j)*Db(j) - Da(j+1)*Db(j+1)
            Dc(j+1) = Da(j)*Db(j+1) + Da(j+1)*Db(j)
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
!
         CALL close(fb,Clsrew)
         IF ( fc/=0 ) THEN
            file = fc
            Typeu = typec*Signc
            CALL gopen(fc,A(buf2),Rdrew)
            IF ( formc/=rowvec ) THEN
               Ip = 1
               Jp = 1
               DO j = 1 , colc
                  CALL unpack(*22,fc,A(j*nwds-1))
                  CYCLE
 22               A(j*nwds-1) = 0.
                  A(j*nwds) = 0.
               ENDDO
            ELSE
               CALL unpack(*100,fc,A(1))
            ENDIF
!
            CALL close(fc,Clsrew)
            IF ( typed==2 ) THEN
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
         formd = rowvec
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         CALL close(fb,Clsrew)
         file = fa
         Ju = rowa
         Typeu = typed
         CALL unpack(*100,fa,A)
         CALL close(ad,Clsrew)
         CALL gopen(fd,A(buf1),Wrtrew)
         spag_nextblock_1 = 5
      CASE (5)
         DO j = 1 , colb
            IF ( typea==2 ) THEN
               DO i = 1 , rowa
                  Da(i) = Da(i)*Db(j)
               ENDDO
            ELSEIF ( typea==3 ) THEN
               DO i = 1 , rowa2 , 2
                  C(i) = A(i)*B(j) - A(i+1)*B(j+1)
                  C(i+1) = A(i)*B(j+1) + A(i+1)*B(j)
               ENDDO
            ELSEIF ( typea==4 ) THEN
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
            CALL pack(C,fd,Filed)
         ENDDO
         CALL close(fd,Clsrew)
         spag_nextblock_1 = 6
      CASE (6)
!
!     ADD PRODUCT OF A*B TO C
!
         IF ( fc/=0 ) THEN
            CALL gopen(fd,A(buf3),Wrtrew)
            file = fc
            CALL open(*40,fc,A(buf2),Rdrew)
            CALL fwdrec(*60,fc)
            file = sd(1)
            CALL open(*40,sd,A(buf1),Rdrew)
            CALL fwdrec(*60,sd)
            Ju = rowc
            Typep = typed
            DO i = 1 , colc
               spag_nextblock_3 = 1
               SPAG_DispatchLoop_3: DO
                  SELECT CASE (spag_nextblock_3)
                  CASE (1)
                     Typeu = typed*Signc
                     CALL unpack(*24,fc,C)
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
 24                  DO j = 1 , rowd2
                        C(j) = 0.0
                     ENDDO
                     spag_nextblock_3 = 2
                  CASE (2)
                     Typeu = typed
                     CALL unpack(*26,sd,B)
                     spag_nextblock_3 = 3
                     CYCLE SPAG_DispatchLoop_3
 26                  DO j = 1 , rowd2
                        B(j) = 0.0
                     ENDDO
                     spag_nextblock_3 = 3
                  CASE (3)
                     IF ( typed==2 .OR. typed==4 ) THEN
                        DO j = 1 , rowd2
                           Da(j) = Db(j) + Dc(j)
                        ENDDO
                     ELSE
                        DO j = 1 , rowd2
                           A(j) = B(j) + C(j)
                        ENDDO
                     ENDIF
                     CALL pack(A,fd,Filed)
                     EXIT SPAG_DispatchLoop_3
                  END SELECT
               ENDDO SPAG_DispatchLoop_3
            ENDDO
            CALL close(fc,Clsrew)
            CALL close(sd,Clsrew)
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
         IF ( cold==0 ) THEN
            DO i = 2 , 7
               Filed(i) = sd(i)
            ENDDO
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         CALL close(fd,Clsrew)
         CALL wrttrl(Filed)
         RETURN
 40      j = -1
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 60      j = -2
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 80      WRITE (Nout,99002) Ufm
99002    FORMAT (A23,' FROM MPYAD/MPYDRI.  FILES NOT COMPATIBLE')
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 100     WRITE (Nout,99003) Ufm
99003    FORMAT (A23,' FROM MPYAD/MPYDRI.  NULL COLUMN ENCOUNTERED DURING',' MATRIX UNPACK')
         spag_nextblock_1 = 9
      CASE (9)
         j = -37
         spag_nextblock_1 = 10
      CASE (10)
         CALL mesage(j,file,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE mpydri
