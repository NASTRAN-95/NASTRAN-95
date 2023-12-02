!*==ddcomp.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddcomp
   USE c_blank
   USE c_cdcmpx
   USE c_dcompx
   USE c_names
   USE c_sfact
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
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
   sing = 0
   ja(1) = kaa
   CALL rdtrl(ja)
   iform = ja(4)
   IF ( isym<0 ) THEN
      IF ( iform==sym ) WRITE (outpt,99001) swm , name
99001 FORMAT (A27,' 2340, MODULE ',2A4,' HAS BEEN REQUESTED TO DO ','UNSYMMETRIC DECOMPOSITION OF A SYMMETRIC MATRIX')
      iform = rect
      IF ( ja(2)==ja(3) ) iform = sqr
   ELSEIF ( isym/=0 ) THEN
      IF ( ja(2)==ja(3) .AND. iform/=sym ) WRITE (outpt,99002) swm , name
99002 FORMAT (A27,' 2341, MODULE ',2A4,'HAS BEEN FURNISHED A SQUARE ','MATRIX MARKED UNSYMMETRIC FOR SYMMETRIC DECOMPOSITION.')
      iform = sym
   ENDIF
   isym = -1
   IF ( iform==sym ) isym = 1
   ja(4) = iform
   IF ( isym>=0 ) THEN
!
!     SET UP CALL TO SDCOMP
!
      ifila(1) = kaa
      CALL rdtrl(ifila)
      ifill(1) = lll
      ifilu(1) = lscr4
      kscr1 = lscr1
      kscr2 = lscr2
      kscr3 = lscr3
      nz = korsz(z)
      ifill(5) = ifila(5)
      ichlk = chlsky
      CALL sdcomp(*100,z,z,z)
      det(1) = sdet
      det(2) = sdetc
      mindia = minds
      power = kpow
      ifill(2) = ifila(2)
      ifill(3) = ifila(3)
      ifill(4) = lower
      CALL wrttrl(ifill)
      RETURN
!
!     SET UP CALL TO DECOMP
!
   ELSEIF ( ja(5)>2 ) THEN
!
!     SET UP CALL TO CDCOMP
!
      jl(1) = lll
      ju(1) = ull
      jscr1 = lscr1
      jscr2 = lscr2
      jscr3 = lscr3
      nzzz = korsz(zzz)
      jl(5) = 4
      jb = 0
      CALL cdcomp(*100,zzz,zzz,zzz)
      ju(5) = 4
      jl(4) = lower
      ju(4) = upper
      jl(3) = jl(2)
      ju(3) = ju(2)
      det(1) = cdet(1)
      det(2) = cdet(2)
      mindia = cmndia
      power = jpow
      CALL wrttrl(jl)
      CALL wrttrl(ju)
      RETURN
   ELSE
      ia(1) = kaa
      CALL rdtrl(ia)
      il(1) = lll
      iu(1) = ull
      nzz = korsz(zz)
      iscr1 = lscr1
      iscr2 = lscr2
      iscr3 = lscr3
      ib = 0
      il(5) = 2
      CALL decomp(*100,zz,zz,zz)
      iu(5) = 2
      il(4) = lower
      iu(4) = upper
      il(3) = il(2)
      iu(3) = iu(2)
      det(1) = ddet
      det(2) = 0.0
      power = ipow
      mindia = dmndia
      CALL wrttrl(iu)
      CALL wrttrl(il)
      RETURN
   ENDIF
!
 100  sing = -1
   det(1) = 0.0
   det(2) = 0.0
   power = 0
   mindia = 0.0
   CALL fname(kaa,ja(1))
   WRITE (outpt,99003) uim , ja(1) , ja(2)
99003 FORMAT (A29,' FORM DECOMP MODULE. MATRIX ',2A4,' IS SINGULAR')
END SUBROUTINE ddcomp
