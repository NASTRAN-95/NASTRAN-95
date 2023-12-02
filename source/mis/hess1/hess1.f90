!*==hess1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE hess1(Kdd,Mdd,Lamd,Phid,Oeigs,Nfound,Nvecd,Bdd,Scr1,Scr2,Scr3,Scr4,Scr5,Scr6,Scr7,Eed,Method)
USE C_CDCMPX
USE C_OUTPUT
USE C_SYSTEM
USE C_UNPAKX
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Kdd
   INTEGER :: Mdd
   INTEGER :: Lamd
   INTEGER :: Phid
   INTEGER :: Oeigs
   INTEGER :: Nfound
   INTEGER :: Nvecd
   INTEGER :: Bdd
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
   INTEGER :: Scr4
   INTEGER :: Scr5
   INTEGER :: Scr6
   INTEGER :: Scr7
   INTEGER :: Eed
   INTEGER :: Method
!
! Local variable declarations rewritten by SPAG
!
   REAL :: alph1 , alph2 , epsi , w1 , w2
   INTEGER :: amat , file , i , ia , ibdd , ibuf1 , id , iflag , ih , ihl , il , im , imat1 , imat2 , inorm , int , inth , iopt ,   &
            & iopt1 , ip1 , iprec , isil , iv , j , k , m , mout , ncount , nout , nrow , nz , sysbuf
   COMPLEX , DIMENSION(1) :: cz
   REAL(REAL64) :: d1 , d2 , d3 , d4 , d5
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(2) , SAVE :: eigc , name
   INTEGER , DIMENSION(10) , SAVE :: ihead
   INTEGER , DIMENSION(8) :: iz
   INTEGER , SAVE :: iz0 , poin
   INTEGER , DIMENSION(7) :: mcb
   REAL(REAL64) , DIMENSION(2) :: temp
   COMPLEX :: tz
   EXTERNAL allmat , cfactr , cfbsor , close , fread , gopen , hess2 , korsz , locate , merged , mesage , open , preloc , rdtrl ,   &
          & unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     SUBROUTINE HESS1 TRANSFORMS THE  PROBLEM
!         PSQ M  + P B  + K   INTO   PSQ I  + MINV K
!
!     THREE CASES ARE AVAILABLE
!         1    BDB = 0   MDD  NOT IDENTITY
!              AMAT=     MINVERSE K  (MINUS ADDED IN CORE)
!                        OUTPUT   P = CSQRT COMPUTED  PS
!                        OUTPUT VEC = COMPUTED VECTOR
!
!         2    BDD = 0   MDD  IDENTITY
!              AMAT=     KDD
!                        OUTPUT AS IN CASE 1
!
!         3    BDD NOT  ZERO   MDD  NOT  IDENTITY
!              AMAT=  1    1    1
!                     1  0 1-I  1
!                     1----------
!                     1 -1 1 -1 1
!                     1M  K1M  B1
!                     1    1    1
!                     OUTPUT  P   = COMPUTED  P
!                     OUTPUT  VEC = FIRST HALF OF COMPUTED VECTOR
!
!     CORE  LAYOUT (FOR ALLMAT) IS AS FOLLOWS)
!
!     CONTENTS                SIZE              POINTER   TYPE  NAME
!     --------                ----              -------   ----  ----
!     INPUT MATRIX--VECTORS   2*NROW*NROW        IA       COMP  A
!     EIGENVALUES             2*NROW             IL       COMP  LAMBDA
!     H MATRIX                2*NROW*NROW        IH       COMP  H
!     HL MATRIX               2*NROW*NROW        IHL      COMP  HL
!     VECTOR STORAGE          2*NROW             IV       COMP  VEC
!     MULTPLIERS              2*NROW             IM       COMP  MULT
!     INTH                    NROW               INTH     INT   INTH
!     INT                     NROW               INT      LOG   INT
!
!     BUFFER                  SYSBUF             IBUF1    INT   BUFFER
!
!
!     VARIABLE  DEFINITION
!
!     ID   0  MEANS  IDENTY MASS MATRIX
!     IBDD 0  MEANS  NULL B MATRIX
!     AMAT    FINAL  A MATRIX GINO NAME
!     NROW    ORDER  OF PROBLEM
!
!
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Mout) , (Ksystm(55),Iprec) , (Z(1),Dz(1),Iz(1),Cz(1))
   DATA name/4HHESS , 4H1   /
   DATA ihead/0 , 1009 , 4 , 7*0/
   DATA eigc , poin/207 , 2 , 4HPOIN/
   DATA iz0/0/
!
!     DETERMINE  IF MASS MATRIX IS IDENTITY
!
   mcb(1) = Mdd
   CALL rdtrl(mcb)
   id = 0
   IF ( mcb(4)==8 ) id = 1
   nrow = mcb(2)
   amat = Kdd
   IF ( id==0 ) THEN
!
!     DECOMPOSE  MASS MATRIX
!
      Ib = 0
      CALL cfactr(Mdd,Scr1,Scr2,Scr3,Scr4,Scr5,iopt)
!
!     SOLVE FOR AMATRIX
!
      CALL cfbsor(Scr1,Scr2,Kdd,Scr3,iopt)
!
!     DETERMINE IF  B MATRIX IS NULL
!
      amat = Scr3
   ENDIF
   ibdd = 0
   mcb(1) = Bdd
   CALL rdtrl(mcb)
   IF ( mcb(1)>0 .AND. mcb(6)/=0 ) THEN
!
!     FORM  M-1  B
!
      ibdd = 1
      imat1 = Bdd
      imat2 = Kdd
      IF ( id==0 ) THEN
!
!     - AS OF APRIL 1985 -
!     THE UPPER AND LOWER TRIANGULAR MATRICES IN SCR1 AND SCR2 WERE
!     MYSTERIOUSLY DESTROYED HERE. MUST CALL CFACTR TO RE-GENERATE THEM
!
!     - AS OF JUNE 1991 -
!     TRY WITHOUT 2ND CALL TO CFACTR, AND MAKE SURE SCR1 AND SCR2 ARE
!     STILL GINO UNITS 301 AND 302
!
         Ib = 0
         CALL cfactr(Mdd,Scr1,Scr2,Scr3,Scr4,Scr5,iopt)
!
         CALL cfbsor(Scr1,Scr2,Bdd,Scr4,iopt)
         imat1 = Scr4
         imat2 = Scr3
      ENDIF
      CALL hess2(nrow,Scr5,Scr6)
!
!     IDENTITY ON SCR5  MERGE VECTOR ON SCR6
!
      CALL merged(0,Scr5,imat2,imat1,Scr7,Scr6,Scr6,0,0)
      amat = Scr7
      nrow = 2*nrow
   ENDIF
!
!     ALLOCATE  CORE FOR  ALLMAT
!
   ia = 1
   il = ia + 2*nrow*nrow
   ih = il + 2*nrow
   ihl = ih + 2*nrow*nrow
   iv = ihl + 2*nrow*nrow
   im = iv + 2*nrow
   inth = im + 2*nrow
   int = inth + nrow
   nz = korsz(iz)
   ibuf1 = nz - sysbuf + 1
   IF ( ih+sysbuf>nz ) CALL mesage(-8,0,name)
!
!     PROCESS EIGC CARD
!
   file = Eed
   CALL preloc(*100,iz(ibuf1-1),Eed)
   CALL locate(*100,iz(ibuf1-1),eigc,iflag)
   DO
      CALL fread(Eed,iz,10,0)
      IF ( Method==iz(1) .OR. Method==-1 ) THEN
!
!     EIGC  CARD  FOUND
!
         inorm = 0
         IF ( iz(4)/=poin ) inorm = 1
         isil = iz(6)
         epsi = 1.0E-6
         IF ( Z(iz0+8)/=0.0 ) epsi = Z(iz0+8)
!
!     PROCESS  REGION  DEFINITION
!
         CALL fread(Eed,iz,7,0)
         alph1 = Z(1)
         alph2 = Z(iz0+3)
         w1 = Z(iz0+2)
         w2 = Z(iz0+4)
         Nvecd = iz(7)
         IF ( Nvecd<=0 ) THEN
!
!     ---- SET DEFAULT TO ONE SOLUTION VECTOR ----
!
            Nvecd = 1
            WRITE (mout,99001) Uwm
99001       FORMAT (A25,' 2357, ONE VECTOR (DEFAULT) WILL BE COMPUTED IN THE',' COMPLEX REGION.')
         ENDIF
         CALL close(Eed,1)
         Nvecd = max0(Nvecd,1)
!
!     BRING IN  TERMS OF MATRIX
!
         CALL gopen(amat,iz(ibuf1),0)
         Itc = -3
         Ii = 1
         Jj = nrow
         Incr = 1
         DO i = ia , il
            Z(i) = 0.0
         ENDDO
         j = ia
         DO i = 1 , nrow
            CALL unpack(*10,amat,Z(j))
 10         j = j + 2*nrow
         ENDDO
         CALL close(amat,1)
!
!     DO IT
!
         ncount = Nvecd
         CALL allmat(Z(ia),Z(il),Z(ih),Z(ihl),Z(iv),Z(im),Z(inth),Z(int),nrow,ncount,iopt1)
         Nfound = ncount/iprec
         file = Lamd
         CALL open(*100,Lamd,iz(ibuf1),1)
         DO i = 1 , nrow
            j = ia + nrow*nrow + i - 1
            IF ( ibdd/=0 ) THEN
!
!     NON-ZERO  B
!
               temp(1) = real(cz(j))
               temp(2) = aimag(cz(j))
            ELSE
!
!     PUT OUT COMPLEX SQUARE ROOT
!
               tz = csqrt(cz(j))
               IF ( aimag(tz)<0.0 ) tz = -tz
               temp(1) = real(tz)
               temp(2) = aimag(tz)
            ENDIF
            CALL write(Lamd,temp,4,1)
         ENDDO
         CALL close(Lamd,1)
!
!     PUT OUT  EIGENVECTORS
!
         file = Phid
         CALL open(*100,Phid,iz(ibuf1),1)
         j = nrow*nrow + nrow
         k = ia - 1
         nout = nrow*2
         IF ( ibdd/=0 ) nout = nout/2
         DO m = 1 , Nvecd
            d1 = 0.0
            DO i = 1 , nout , 2
               Ii = j + i
               Jj = k + i
               dz(Ii) = Z(Jj)
               dz(Ii+1) = Z(Jj+1)
               d2 = dz(Ii)*dz(Ii) + dz(Ii+1)*dz(Ii+1)
               IF ( d2>=d1 ) THEN
                  d3 = dz(Ii)
                  d4 = dz(Ii+1)
                  d1 = d2
               ENDIF
            ENDDO
            IF ( inorm==0 ) THEN
               Jj = 2*isil + j
               d2 = dz(Jj)*dz(Jj) + dz(Jj-1)*dz(Jj-1)
               IF ( d2/=0.0D0 .AND. d1/d2<=1.0D6 ) THEN
                  d3 = dz(Jj-1)
                  d4 = dz(Jj)
                  d1 = d2
               ENDIF
            ENDIF
            DO i = 1 , nout , 2
               Jj = j + i
               d5 = (dz(Jj)*d3+dz(Jj+1)*d4)/d1
               dz(Jj+1) = (d3*dz(Jj+1)-d4*dz(Jj))/d1
               dz(Jj) = d5
            ENDDO
            CALL write(Phid,dz(j+1),nout*2,1)
            k = k + nrow*2
         ENDDO
         CALL close(Phid,1)
!
!     PUT OUT OEIGS
!
         CALL gopen(Oeigs,iz(ibuf1),1)
         CALL write(Oeigs,ihead,10,0)
         iz(1) = Nfound
         iz(2) = Nvecd
         iz(3) = 0
         iz(4) = 0
         iz(5) = 0
         iz(6) = 0
         iz(7) = 0
         iz(8) = 1
         CALL write(Oeigs,iz,40,0)
         CALL write(Oeigs,Head,96,1)
         CALL close(Oeigs,1)
         mcb(1) = Oeigs
         mcb(2) = Nfound
         mcb(3) = Nvecd
         CALL wrttrl(mcb)
         RETURN
      ELSE
         SPAG_Loop_2_1: DO
!
!     SKIP REMAINDER OF EIGC CARD
!
            CALL fread(Eed,iz,7,0)
            IF ( iz(6)==-1 ) EXIT SPAG_Loop_2_1
         ENDDO SPAG_Loop_2_1
      ENDIF
   ENDDO
!
!     ERROR MESSAGES
!
 100  ip1 = -1
   CALL mesage(ip1,file,name)
!
END SUBROUTINE hess1
