
SUBROUTINE mpy3b(Z,Iz,Dz)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(2) , Buf1 , Buf2 , Buf3 , Buf4 , Code , Dum(2) , Dumcp(2) , E , Fileb(7) , Filec(7) , Filee(7) , Scr , Scr3(7)
   DOUBLE PRECISION Da
   INTEGER Eol , Eor , Filea(7) , I , Iakj , Ibcid , Ibcols , Ibntu , Icore , Iflag , Iktbp , Irow , Itrl , J , K , K2 , Ka , Kb ,  &
         & Kcount , Laend , Lcore , Lkore , M , N , Ncb , Nk , Ntbu , Prec , Scr1 , Scr2 , Uincr , Urow1 , Urown , Utyp , Zpntrs(22)
   LOGICAL First1 , First2
   COMMON /mpy3cp/ Itrl , Icore , N , Ncb , M , Nk , Dumcp , Zpntrs , Laend , First1 , First2 , K , K2 , Kcount , Iflag , Ka , Kb , &
                 & J , I , Ntbu
   COMMON /mpy3tl/ Filea , Fileb , Filee , Filec , Scr1 , Scr2 , Scr , Lkore , Code , Prec , Lcore , Scr3 , Buf1 , Buf2 , Buf3 ,    &
                 & Buf4 , E
   COMMON /unpakx/ Utyp , Urow1 , Urown , Uincr
   COMMON /zntpkx/ A , Dum , Irow , Eol , Eor
!
! Dummy argument declarations
!
   DOUBLE PRECISION Dz(1)
   INTEGER Iz(1)
   REAL Z(1)
!
! Local variable declarations
!
   INTEGER file , ib , kbc , kbn , kj , kk , kkb , kkk , kt , l , name(2) , nerr , preck , precn
!
! End of declarations
!
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
   EQUIVALENCE (A(1),Da)
! OPEN CORE POINTERS
   EQUIVALENCE (Ibcols,Zpntrs(11)) , (Ibcid,Zpntrs(13)) , (Ibntu,Zpntrs(15)) , (Iktbp,Zpntrs(17)) , (Iakj,Zpntrs(21))
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
   kt = Iktbp - 1
   IF ( Prec==2 ) THEN
! DOUBLE PRECISION CASE
      kj = (Iakj-1)/2
      CALL intpk(*100,Filea,0,2,0)
      DO
         CALL zntpki
         K = K + 1
         kt = kt + 1
         Iz(kt) = Irow
         kj = kj + 1
         Dz(kj) = Da
         IF ( Eol==1 ) EXIT
      ENDDO
   ELSE
! SINGLE PRECISION CASE
      kj = Iakj - 1
      CALL intpk(*100,Filea,0,1,0)
      DO
         CALL zntpki
         K = K + 1
         kt = kt + 1
         Iz(kt) = Irow
         kj = kj + 1
         Z(kj) = A(1)
         IF ( Eol==1 ) EXIT
      ENDDO
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
      kt = Iktbp - 1
      Kb = Ibcols - precn
      kbc = Ibcid - 1
      DO kk = 1 , K2
         kt = kt + 1
         kkk = Iz(kt)
         CALL filpos(Scr1,Iz(kkk))
         Kb = Kb + precn
         CALL unpack(*20,Scr1,Z(Kb))
         GOTO 40
 20      ib = Kb - 1
         DO l = 1 , precn
            ib = ib + 1
            Z(ib) = 0.
         ENDDO
 40      kbc = kbc + 1
         Iz(kbc) = kkk
      ENDDO
   ENDIF
!*****
!    BEGIN CALCULATING MATRIX PRODUCT.
!*****
   kt = Iktbp - 1
   Kcount = 0
   preck = Prec*K
   DO Ka = 1 , K
      kt = kt + 1
      kbc = Ibcid - 1
      DO Kb = 1 , K2
         kbc = kbc + 1
         IF ( Iz(kt)==Iz(kbc) ) GOTO 50
      ENDDO
      CYCLE
 50   kkb = Kb
      CALL mpy3p(Z,Z,Z)
      Iz(kt) = 0
      Kcount = Kcount + 1
      IF ( .NOT.(First2 .OR. Icore==1) ) THEN
         I = Iz(kbc)
         CALL mpy3nu(Z)
         kbn = Ibntu + kkb - 1
         Iz(kbn) = Ntbu
      ENDIF
   ENDDO
!*****
!    SET RETURN FLAG.
!*****
   IF ( Kcount/=K ) THEN
      Iflag = 1
      GOTO 99999
   ENDIF
 100  Iflag = 0
   IF ( .NOT.(Icore/=1 .OR. First2) ) THEN
      IF ( J/=M ) THEN
         file = Scr2
         CALL fwdrec(*200,Scr2)
      ENDIF
   ENDIF
   GOTO 99999
!*****
!    ERROR MESSAGES.
!*****
 200  nerr = -2
   CALL mesage(nerr,file,name)
!
99999 RETURN
END SUBROUTINE mpy3b
