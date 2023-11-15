
SUBROUTINE mpy3c(Z,Iz,Dz)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Buf1 , Buf2 , Buf3 , Buf4 , Code , Dum1(2) , Dum2(2) , E , Filea(7) , Fileb(7) , Filec(7) , Filee(7) , Scr , Scr3(7)
   LOGICAL First2
   INTEGER Iantu , Ibcid , Ibcols , Ibntu , Icore , Iflag , Iktbp , Itrl , J , K , K2 , Ka , Kcount , Lcore , Lkore , Ltac , Ltbc , &
         & M , N , Ncb , Nk , Ntbu , Prec , Scr1 , Scr2 , Uincr , Urow1 , Urown , Utyp , Zpntrs(22)
   COMMON /mpy3cp/ Itrl , Icore , N , Ncb , M , Nk , Dum1 , Zpntrs , Dum2 , First2 , K , K2 , Kcount , Iflag , Ka , Ltbc , J ,      &
                 & Ltac , Ntbu
   COMMON /mpy3tl/ Filea , Fileb , Filee , Filec , Scr1 , Scr2 , Scr , Lkore , Code , Prec , Lcore , Scr3 , Buf1 , Buf2 , Buf3 ,    &
                 & Buf4 , E
   COMMON /unpakx/ Utyp , Urow1 , Urown , Uincr
!
! Dummy argument declarations
!
   REAL Dz
   INTEGER Iz(1)
   REAL Z(1)
!
! Local variable declarations
!
   INTEGER file , i , ia , ib , ik , kb , kk , l , lta , ltb , precn
!
! End of declarations
!
!*****
!    PERFORMS MULTIPLICATION AND SUMMATION FOR REMAINING TERMS OF COLUMN
!    OF A.
!*****
!
!
!
!
!
!
!     DIMENSION NAME(2)
!
!
! FILES
! SUBROUTINE CALL PARAMETERS
! UNPACK
!
!
!
   EQUIVALENCE (Ibcols,Zpntrs(11)) , (Ibcid,Zpntrs(13)) , (Ibntu,Zpntrs(15)) , (Iktbp,Zpntrs(17)) , (Iantu,Zpntrs(19))
!
!
!
!     DATA NAME / 4HMPY3,4HC    /
!*****
!    INITIALIZATION.
!*****
   Utyp = Prec
   Urow1 = 1
   Urown = N
   Uincr = 1
   precn = Prec*N
   file = Scr1
!*****
!    TEST TO SEE IF LESS THAN NK COLUMNS OF B IN CORE.
!*****
   IF ( First2 ) THEN
!*****
!    LESS THAN NK COLUMNS OF B IN CORE.
!*****
      K2 = K2 + 1
      Ltbc = K2
      kk = Iktbp - 1
      DO Ka = 1 , K
         kk = kk + 1
         IF ( Iz(kk)/=0 ) THEN
            Ltac = Iz(kk)
            EXIT
         ENDIF
      ENDDO
   ELSE
!*****
!    DETERMINE WHICH COLUMN OF B TO BE PUT INTO CORE.
!*****
      lta = 0
      ia = Iantu - 1
      DO i = 1 , K
         ia = ia + 1
         IF ( lta<Iz(ia) ) THEN
            lta = Iz(ia)
            ik = Iktbp + i - 1
            Ltac = Iz(ik)
            Ka = i
         ENDIF
      ENDDO
!*****
!    DETERMINE WHICH COLUMN OF B TO BE REPLACED.
!*****
      ltb = 0
      ib = Ibntu - 1
      DO i = 1 , Nk
         ib = ib + 1
         IF ( ltb<Iz(ib) ) THEN
            ltb = Iz(ib)
            Ltbc = i
         ENDIF
      ENDDO
   ENDIF
!*****
!    ADD OR REPLACE COLUMN OF B INTO CORE.
!*****
   CALL filpos(Scr1,Iz(Ltac))
   kk = Ibcols + precn*(Ltbc-1)
   CALL unpack(*100,Scr1,Z(kk))
   GOTO 200
 100  ik = kk - 1
   DO l = 1 , precn
      ik = ik + 1
      Z(ik) = 0.
   ENDDO
 200  IF ( .NOT.(First2) ) THEN
      IF ( Icore/=1 ) THEN
         CALL mpy3nu(Z)
         kk = Ibntu + Ltbc - 1
         Iz(kk) = Ntbu
      ENDIF
      kk = Iantu + Ka - 1
      Iz(kk) = 0
   ENDIF
   kk = Ibcid + Ltbc - 1
   Iz(kk) = Ltac
   kb = Ltbc
!*****
!    PERFORM COMPUTATION.
!*****
   CALL mpy3p(Z,Z,Z)
   Ltbc = kb
   kk = Iktbp + Ka - 1
   Iz(kk) = 0
   Kcount = Kcount + 1
END SUBROUTINE mpy3c
