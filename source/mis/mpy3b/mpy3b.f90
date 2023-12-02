!*==mpy3b.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpy3b(Z,Iz,Dz)
USE C_MPY3CP
USE C_MPY3TL
USE C_UNPAKX
USE C_ZNTPKX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Z
   INTEGER , DIMENSION(1) :: Iz
   REAL(REAL64) , DIMENSION(1) :: Dz
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: da
   INTEGER :: file , iakj , ib , ibcid , ibcols , ibntu , iktbp , kbc , kbn , kj , kk , kkb , kkk , kt , l , nerr , preck , precn
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL filpos , fwdrec , intpk , mesage , mpy3nu , mpy3p , unpack , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!*****
!    PROCESSES A AND PERFORMS FIRST PART OF PRODUCT.
!*****
!
!
!
!
!
!
!
!
!
!
!
! FILES
! SUBROUTINE CALL PARAMETERS
! UNPACK
! TERMWISE MATRIX READ
!
!
!
   !>>>>EQUIVALENCE (A(1),Da)
! OPEN CORE POINTERS
   !>>>>EQUIVALENCE (Ibcols,Zpntrs(11)) , (Ibcid,Zpntrs(13)) , (Ibntu,Zpntrs(15)) , (Iktbp,Zpntrs(17)) , (Iakj,Zpntrs(21))
!
!
!
   DATA name/4HMPY3 , 4HB   /
!*****
!    INITIALIZATION.
!*****
   file = Scr1
   Utyp = Prec
   Urow1 = 1
   Urown = N
   Uincr = 1
   precn = Prec*N
!*****
!    READ AND STORE COLUMN OF A.
!*****
   K = 0
   kt = iktbp - 1
   IF ( Prec==2 ) THEN
! DOUBLE PRECISION CASE
      kj = (iakj-1)/2
      CALL intpk(*100,Filea,0,2,0)
      SPAG_Loop_1_1: DO
         CALL zntpki
         K = K + 1
         kt = kt + 1
         Iz(kt) = Irow
         kj = kj + 1
         Dz(kj) = da
         IF ( Eol==1 ) EXIT SPAG_Loop_1_1
      ENDDO SPAG_Loop_1_1
   ELSE
! SINGLE PRECISION CASE
      kj = iakj - 1
      CALL intpk(*100,Filea,0,1,0)
      SPAG_Loop_1_2: DO
         CALL zntpki
         K = K + 1
         kt = kt + 1
         Iz(kt) = Irow
         kj = kj + 1
         Z(kj) = A(1)
         IF ( Eol==1 ) EXIT SPAG_Loop_1_2
      ENDDO SPAG_Loop_1_2
   ENDIF
   IF ( First1 ) THEN
!*****
!    READ COLUMNS OF B INTO CORE.
!*****
      First1 = .FALSE.
      IF ( K>Nk ) THEN
         K2 = Nk
      ELSE
         K2 = K
      ENDIF
      kt = iktbp - 1
      Kb = ibcols - precn
      kbc = ibcid - 1
      DO kk = 1 , K2
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
               kt = kt + 1
               kkk = Iz(kt)
               CALL filpos(Scr1,Iz(kkk))
               Kb = Kb + precn
               CALL unpack(*5,Scr1,Z(Kb))
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
 5             ib = Kb - 1
               DO l = 1 , precn
                  ib = ib + 1
                  Z(ib) = 0.
               ENDDO
               spag_nextblock_1 = 2
            CASE (2)
               kbc = kbc + 1
               Iz(kbc) = kkk
               EXIT SPAG_DispatchLoop_1
            END SELECT
         ENDDO SPAG_DispatchLoop_1
      ENDDO
   ENDIF
!*****
!    BEGIN CALCULATING MATRIX PRODUCT.
!*****
   kt = iktbp - 1
   Kcount = 0
   preck = Prec*K
   DO Ka = 1 , K
      spag_nextblock_2 = 1
      SPAG_DispatchLoop_2: DO
         SELECT CASE (spag_nextblock_2)
         CASE (1)
            kt = kt + 1
            kbc = ibcid - 1
            DO Kb = 1 , K2
               kbc = kbc + 1
               IF ( Iz(kt)==Iz(kbc) ) THEN
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
               ENDIF
            ENDDO
            CYCLE
         CASE (2)
            kkb = Kb
            CALL mpy3p(Z,Z,Z)
            Iz(kt) = 0
            Kcount = Kcount + 1
            IF ( .NOT.(First2 .OR. Icore==1) ) THEN
               I = Iz(kbc)
               CALL mpy3nu(Z)
               kbn = ibntu + kkb - 1
               Iz(kbn) = Ntbu
            ENDIF
            EXIT SPAG_DispatchLoop_2
         END SELECT
      ENDDO SPAG_DispatchLoop_2
   ENDDO
!*****
!    SET RETURN FLAG.
!*****
   IF ( Kcount/=K ) THEN
      Iflag = 1
      RETURN
   ENDIF
 100  Iflag = 0
   IF ( .NOT.(Icore/=1 .OR. First2) ) THEN
      IF ( J/=M ) THEN
         file = Scr2
         CALL fwdrec(*200,Scr2)
      ENDIF
   ENDIF
   RETURN
!*****
!    ERROR MESSAGES.
!*****
 200  nerr = -2
   CALL mesage(nerr,file,name)
!
END SUBROUTINE mpy3b
