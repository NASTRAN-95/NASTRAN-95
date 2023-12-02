!*==ddcomp.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddcomp
USE C_BLANK
USE C_CDCMPX
USE C_DCOMPX
USE C_NAMES
USE C_SFACT
USE C_SYSTEM
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: iform , lower , outpt , rect , sqr , sym , upper
   INTEGER , SAVE :: kaa , lll , lscr1 , lscr2 , lscr3 , lscr4 , ull
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(1) :: zz , zzz
   EXTERNAL cdcomp , decomp , fname , korsz , rdtrl , sdcomp , wrttrl
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Zz(1),Z(1))
   !>>>>EQUIVALENCE (Zzz(1),Z(1))
   !>>>>EQUIVALENCE (Ksystm(2),Outpt) , (Knames(12),Sqr) , (Knames(13),Rect) , (Knames(17),Sym) , (Knames(16),Upper) , (Knames(15),Lower)
   DATA kaa , lll , ull , lscr1 , lscr2 , lscr3 , lscr4/101 , 201 , 202 , 301 , 302 , 303 , 304/
   DATA name/4HDDCO , 4HMP  /
!
   Sing = 0
   Ja(1) = kaa
   CALL rdtrl(Ja)
   iform = Ja(4)
   IF ( Isym<0 ) THEN
      IF ( iform==sym ) WRITE (outpt,99001) Swm , name
99001 FORMAT (A27,' 2340, MODULE ',2A4,' HAS BEEN REQUESTED TO DO ','UNSYMMETRIC DECOMPOSITION OF A SYMMETRIC MATRIX')
      iform = rect
      IF ( Ja(2)==Ja(3) ) iform = sqr
   ELSEIF ( Isym/=0 ) THEN
      IF ( Ja(2)==Ja(3) .AND. iform/=sym ) WRITE (outpt,99002) Swm , name
99002 FORMAT (A27,' 2341, MODULE ',2A4,'HAS BEEN FURNISHED A SQUARE ','MATRIX MARKED UNSYMMETRIC FOR SYMMETRIC DECOMPOSITION.')
      iform = sym
   ENDIF
   Isym = -1
   IF ( iform==sym ) Isym = 1
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
      Ifill(4) = lower
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
      Nzzz = korsz(zzz)
      Jl(5) = 4
      Jb = 0
      CALL cdcomp(*100,zzz,zzz,zzz)
      Ju(5) = 4
      Jl(4) = lower
      Ju(4) = upper
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
      Nzz = korsz(zz)
      Iscr1 = lscr1
      Iscr2 = lscr2
      Iscr3 = lscr3
      Ib = 0
      Il(5) = 2
      CALL decomp(*100,zz,zz,zz)
      Iu(5) = 2
      Il(4) = lower
      Iu(4) = upper
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
   WRITE (outpt,99003) Uim , Ja(1) , Ja(2)
99003 FORMAT (A29,' FORM DECOMP MODULE. MATRIX ',2A4,' IS SINGULAR')
END SUBROUTINE ddcomp
