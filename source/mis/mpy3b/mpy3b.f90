!*==mpy3b.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpy3b(Z,Iz,Dz)
   USE c_mpy3cp
   USE c_mpy3tl
   USE c_unpakx
   USE c_zntpkx
   USE iso_fortran_env
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
   file = scr1
   utyp = prec
   urow1 = 1
   urown = n
   uincr = 1
   precn = prec*n
!*****
!    READ AND STORE COLUMN OF A.
!*****
   k = 0
   kt = iktbp - 1
   IF ( prec==2 ) THEN
! DOUBLE PRECISION CASE
      kj = (iakj-1)/2
      CALL intpk(*100,filea,0,2,0)
      SPAG_Loop_1_1: DO
         CALL zntpki
         k = k + 1
         kt = kt + 1
         Iz(kt) = irow
         kj = kj + 1
         Dz(kj) = da
         IF ( eol==1 ) EXIT SPAG_Loop_1_1
      ENDDO SPAG_Loop_1_1
   ELSE
! SINGLE PRECISION CASE
      kj = iakj - 1
      CALL intpk(*100,filea,0,1,0)
      SPAG_Loop_1_2: DO
         CALL zntpki
         k = k + 1
         kt = kt + 1
         Iz(kt) = irow
         kj = kj + 1
         Z(kj) = a(1)
         IF ( eol==1 ) EXIT SPAG_Loop_1_2
      ENDDO SPAG_Loop_1_2
   ENDIF
   IF ( first1 ) THEN
!*****
!    READ COLUMNS OF B INTO CORE.
!*****
      first1 = .FALSE.
      IF ( k>nk ) THEN
         k2 = nk
      ELSE
         k2 = k
      ENDIF
      kt = iktbp - 1
      kb = ibcols - precn
      kbc = ibcid - 1
      DO kk = 1 , k2
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
               kt = kt + 1
               kkk = Iz(kt)
               CALL filpos(scr1,Iz(kkk))
               kb = kb + precn
               CALL unpack(*5,scr1,Z(kb))
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
 5             ib = kb - 1
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
   kcount = 0
   preck = prec*k
   DO ka = 1 , k
      spag_nextblock_2 = 1
      SPAG_DispatchLoop_2: DO
         SELECT CASE (spag_nextblock_2)
         CASE (1)
            kt = kt + 1
            kbc = ibcid - 1
            SPAG_Loop_3_1: DO kb = 1 , k2
               kbc = kbc + 1
               IF ( Iz(kt)==Iz(kbc) ) THEN
                  spag_nextblock_2 = 2
                  EXIT SPAG_Loop_3_1
               ENDIF
            ENDDO SPAG_Loop_3_1
         CASE (2)
            kkb = kb
            CALL mpy3p(Z,Z,Z)
            Iz(kt) = 0
            kcount = kcount + 1
            IF ( .NOT.(first2 .OR. icore==1) ) THEN
               i = Iz(kbc)
               CALL mpy3nu(Z)
               kbn = ibntu + kkb - 1
               Iz(kbn) = ntbu
            ENDIF
            EXIT SPAG_DispatchLoop_2
         END SELECT
      ENDDO SPAG_DispatchLoop_2
   ENDDO
!*****
!    SET RETURN FLAG.
!*****
   IF ( kcount/=k ) THEN
      iflag = 1
      RETURN
   ENDIF
 100  iflag = 0
   IF ( .NOT.(icore/=1 .OR. first2) ) THEN
      IF ( j/=m ) THEN
         file = scr2
         CALL fwdrec(*200,scr2)
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
