
SUBROUTINE ddcomp
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION Cdet(2) , Cmndia , Ddet , Dmndia , Mindia , Minds , Sdet , Sdetc
   INTEGER Chlsky , Ia(7) , Ib , Ichlk , Ifila(7) , Ifill(7) , Ifilu(7) , Il(7) , Ipow , Iscr1 , Iscr2 , Iscr3 , Isym , Iu(7) ,     &
         & Ja(7) , Jb , Jl(7) , Jpow , Jscr1 , Jscr2 , Jscr3 , Ju(7) , Knames(19) , Kpow , Kscr1 , Kscr2 , Kscr3 , Ksystm(65) ,     &
         & Lower , Nz , Nzz , Nzzz , Outpt , Power , Rect , Sing , Sqr , Sym , Upper
   REAL Det(2) , Z(1) , Zz(1) , Zzz(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Isym , Chlsky , Mindia , Det , Power , Sing
   COMMON /cdcmpx/ Ja , Jl , Ju , Jscr1 , Jscr2 , Jscr3 , Cdet , Jpow , Nzzz , Cmndia , Jb
   COMMON /dcompx/ Ia , Il , Iu , Iscr1 , Iscr2 , Iscr3 , Ddet , Ipow , Nzz , Dmndia , Ib
   COMMON /names / Knames
   COMMON /sfact / Ifila , Ifill , Ifilu , Kscr1 , Kscr2 , Nz , Sdet , Sdetc , Kpow , Kscr3 , Minds , Ichlk
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER iform , kaa , lll , lscr1 , lscr2 , lscr3 , lscr4 , name(2) , ull
   INTEGER korsz
!
! End of declarations
!
!
!     DDCOMP IS THE DMAP DRIVER FOR DECOMP
!
!     DECOMP    KAA/LLL,ULL/SYM/CHLSKY/MINDIA/DET/POWER/SING $
!
!        SYM    =  1 - USE SYMMETRIC DECOMPOSITION
!                  0 - CHOOSE WHICH DECOMPOSITION BASED ON INPUT MATRIX
!                 -1 - USE UNSYMETRIC DECOMPOSITION
!        CHLSKY =  1 USE CHOLESKY DECOMPOSITION LLL = C
!        DET    =  DETERMINANT OF KAA
!        POWER  =  SCALE FACTOR FOR DET
!        MINDIA =  MINIMUM DIAGONAL OF ULL
!        SING   = -1 SINGULAR MATRIX
!
   EQUIVALENCE (Zz(1),Z(1))
   EQUIVALENCE (Zzz(1),Z(1))
   EQUIVALENCE (Ksystm(2),Outpt) , (Knames(12),Sqr) , (Knames(13),Rect) , (Knames(17),Sym) , (Knames(16),Upper) , (Knames(15),Lower)
   DATA kaa , lll , ull , lscr1 , lscr2 , lscr3 , lscr4/101 , 201 , 202 , 301 , 302 , 303 , 304/
   DATA name/4HDDCO , 4HMP  /
!
   Sing = 0
   Ja(1) = kaa
   CALL rdtrl(Ja)
   iform = Ja(4)
   IF ( Isym<0 ) THEN
      IF ( iform==Sym ) WRITE (Outpt,99001) Swm , name
99001 FORMAT (A27,' 2340, MODULE ',2A4,' HAS BEEN REQUESTED TO DO ','UNSYMMETRIC DECOMPOSITION OF A SYMMETRIC MATRIX')
      iform = Rect
      IF ( Ja(2)==Ja(3) ) iform = Sqr
   ELSEIF ( Isym/=0 ) THEN
      IF ( Ja(2)==Ja(3) .AND. iform/=Sym ) WRITE (Outpt,99002) Swm , name
99002 FORMAT (A27,' 2341, MODULE ',2A4,'HAS BEEN FURNISHED A SQUARE ','MATRIX MARKED UNSYMMETRIC FOR SYMMETRIC DECOMPOSITION.')
      iform = Sym
   ENDIF
   Isym = -1
   IF ( iform==Sym ) Isym = 1
   Ja(4) = iform
   IF ( Isym>=0 ) THEN
!
!     SET UP CALL TO SDCOMP
!
      Ifila(1) = kaa
      CALL rdtrl(Ifila)
      Ifill(1) = lll
      Ifilu(1) = lscr4
      Kscr1 = lscr1
      Kscr2 = lscr2
      Kscr3 = lscr3
      Nz = korsz(Z)
      Ifill(5) = Ifila(5)
      Ichlk = Chlsky
      CALL sdcomp(*100,Z,Z,Z)
      Det(1) = Sdet
      Det(2) = Sdetc
      Mindia = Minds
      Power = Kpow
      Ifill(2) = Ifila(2)
      Ifill(3) = Ifila(3)
      Ifill(4) = Lower
      CALL wrttrl(Ifill)
      RETURN
!
!     SET UP CALL TO DECOMP
!
   ELSEIF ( Ja(5)>2 ) THEN
!
!     SET UP CALL TO CDCOMP
!
      Jl(1) = lll
      Ju(1) = ull
      Jscr1 = lscr1
      Jscr2 = lscr2
      Jscr3 = lscr3
      Nzzz = korsz(Zzz)
      Jl(5) = 4
      Jb = 0
      CALL cdcomp(*100,Zzz,Zzz,Zzz)
      Ju(5) = 4
      Jl(4) = Lower
      Ju(4) = Upper
      Jl(3) = Jl(2)
      Ju(3) = Ju(2)
      Det(1) = Cdet(1)
      Det(2) = Cdet(2)
      Mindia = Cmndia
      Power = Jpow
      CALL wrttrl(Jl)
      CALL wrttrl(Ju)
      RETURN
   ELSE
      Ia(1) = kaa
      CALL rdtrl(Ia)
      Il(1) = lll
      Iu(1) = ull
      Nzz = korsz(Zz)
      Iscr1 = lscr1
      Iscr2 = lscr2
      Iscr3 = lscr3
      Ib = 0
      Il(5) = 2
      CALL decomp(*100,Zz,Zz,Zz)
      Iu(5) = 2
      Il(4) = Lower
      Iu(4) = Upper
      Il(3) = Il(2)
      Iu(3) = Iu(2)
      Det(1) = Ddet
      Det(2) = 0.0
      Power = Ipow
      Mindia = Dmndia
      CALL wrttrl(Iu)
      CALL wrttrl(Il)
      RETURN
   ENDIF
!
 100  Sing = -1
   Det(1) = 0.0
   Det(2) = 0.0
   Power = 0
   Mindia = 0.0
   CALL fname(kaa,Ja(1))
   WRITE (Outpt,99003) Uim , Ja(1) , Ja(2)
99003 FORMAT (A29,' FORM DECOMP MODULE. MATRIX ',2A4,' IS SINGULAR')
END SUBROUTINE ddcomp
