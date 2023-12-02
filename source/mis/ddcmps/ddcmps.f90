!*==ddcmps.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddcmps
   USE c_blank
   USE c_cdcmpx
   USE c_dcompx
   USE c_names
   USE c_sdcq
   USE c_sfact
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iform , nbufsz , outpt , rect , sqr , sym
   INTEGER , SAVE :: kaa , lgpl , lll , lscr1 , lscr2 , lscr3 , lscr4 , lscr5 , lscr6 , lsil , luset , ull
   INTEGER , DIMENSION(2) , SAVE :: nam , name
   REAL , DIMENSION(1) :: zm , zz , zzz , zzzz
   EXTERNAL cdcomp , decomp , korsz , mesage , page2 , rdtrl , sdcmm , sdcmps , sdcomp , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     DDCMPS IS THE DMAP DRIVER FOR SDCMPS
!
!     SDCMPS   USET,GPL,SIL,KAA/LLL,ULL/SYM=0/DIAGCK=0/DIAGET=20/
!              PDEFCK=0/SING=0/SET=L/CHLSKY=0/DET=0.0D0/MINDIA=0.0D0/
!              POWER=0/SUBNAM=NONE
!
!     SYM      =  1 - USE SYMMETRIC DECOMPOSITION
!                 0 - CHOOSE WHICH DECOMPOSITION BASED ON INPUT MATRIX
!                -1 - USE UNSYMETRIC DECOMPOSITION
!     DIAGCK   =  DIAGONAL SINGULARITY CHECK              (SDCMPS)
!                 - = NO CHECK                            (SDCMPS)
!                 0 = NONFATAL                            (SDCMPS)
!                 + = MAX ALLOWED FATAL                   (SDCMPS)
!     DIAGET   =  DIAGONAL SINGULARITY ERROR TOLERANCE.   (SDCMPS)
!     PDEFCK   =  POSITIVE DEFINATE CHECK                 (SDCMPS)
!                 - = NO CHECK                            (SDCMPS)
!                 0 = NONFATAL                            (SDCMPS)
!                 + = MAX ALLOWED FATAL                   (SDCMPS)
!     SING     =  SINGULARITY OUTPUT FLAG
!                 1 = OK
!                 0 = NONCONSERVATIVE OR ES FAILURE
!                -1 = SINGULAR OR LIMITS EXCEEDED
!     SET      =  SET MATRIX BELONGS TO                   (SDCMPS)
!     CHLSKY   =  1 USE CHOLESKY DECOMPOSITION LLL = C
!     DET      =  DETERMINANT OF KAA
!     MINDIA   =  MINIMUM DIAGONAL OF ULL
!     POWER    =  SCALE FACTOR FOR DET
!     SUBNAM   =  SUBSTRUCTURE NAME                       (SDCMPS)
!
   !>>>>EQUIVALENCE (Zz(1),Z(1))
   !>>>>EQUIVALENCE (Zzz(1),Z(1))
   !>>>>EQUIVALENCE (Zzzz(1),Z(1))
   !>>>>EQUIVALENCE (Zm(1),Z(1))
   !>>>>EQUIVALENCE (Ksystm(1),Nbufsz) , (Ksystm(2),Outpt) , (Knames(12),Sqr) , (Knames(13),Rect) , (Knames(17),Sym)
   DATA luset , lgpl , lsil , kaa , lll , ull , lscr1 , lscr2 , lscr3/101 , 102 , 103 , 104 , 201 , 202 , 301 , 302 , 303/
   DATA lscr4 , lscr5 , lscr6/304 , 305 , 306/
   DATA name/4HDDCM , 4HPS  /
   DATA nam/4HSDCM , 4HPS  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     NOTE SYM DECOMP DOES NOT OUTPUT  ULL
!
!
         opnscr = .FALSE.
         first = .TRUE.
         sing = 1
         ja(1) = kaa
         CALL rdtrl(ja)
         IF ( ja(1)<0 ) THEN
!
!     ERROR  MESSAGES
!
!     PURGED INPUT
!
            parm(1) = -1
            parm(2) = kaa
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            iform = ja(4)
            IF ( isym<0 ) THEN
               IF ( iform==sym ) THEN
                  CALL page2(2)
                  WRITE (outpt,99001) swm , nam
99001             FORMAT (A27,' 2340, MODULE ',2A4,' HAS BEEN REQUESTED TO DO ','UNSYMETRIC DECOMPOSITION OF A SYMETRIC MATRIX')
               ENDIF
               iform = rect
               IF ( ja(2)==ja(3) ) iform = sqr
            ELSEIF ( isym/=0 ) THEN
!
               IF ( iform/=sym ) THEN
                  CALL page2(2)
                  WRITE (outpt,99002) swm , nam
99002             FORMAT (A27,' 2341, MODULE ',2A4,' HAS BEEN FURNISHED A SQUARE ',                                                 &
                         &'MATRIX MARKED UNSYMETRIC FOR SYMETRIC DECOMPOSITION.')
                  iform = sym
               ENDIF
            ENDIF
            isym = -1
            IF ( iform==sym ) isym = 1
            ja(4) = iform
            i = 0
            IF ( ja(2)/=ja(3) ) THEN
               CALL page2(2)
               i = 1
               WRITE (outpt,99003) swm , nam
99003          FORMAT (A27,' 2375, MODULE ',2A4,' HAS BEEN REQUESTED TO ','DECOMPOSE A RECTANGULAR MATRIX')
            ENDIF
            IF ( isym<0 ) THEN
!
!     SET UP CALL TO DECOMP
!
               IF ( ja(5)>2 ) THEN
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
                  CALL cdcomp(*20,zzz,zzz,zzz)
                  ju(5) = 4
                  jl(4) = 4
                  ju(4) = 5
                  jl(3) = jl(2)
                  ju(3) = ju(2)
                  det(1) = cdet(1)
                  det(2) = cdet(2)
                  mindia = cmndia
                  power = jpow
                  CALL wrttrl(jl)
                  CALL wrttrl(ju)
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
                  CALL decomp(*20,zz,zz,zz)
                  iu(5) = 2
                  il(4) = 4
                  iu(4) = 5
                  il(3) = il(2)
                  iu(3) = iu(2)
                  det(1) = ddet
                  det(2) = 0.0
                  power = ipow
                  mindia = dmndia
                  CALL wrttrl(iu)
                  CALL wrttrl(il)
               ENDIF
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
!
!     SET UP CALL TO SDCOMP
!
            ELSEIF ( i/=0 ) THEN
!
!     NUMBER ROWS.NE.COLUMNS
!
               parm(1) = -16
               parm(2) = kaa
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSE
               ifila(1) = kaa
               CALL rdtrl(ifila)
               ifill(1) = lll
               ifilu(1) = lscr4
               kscr1 = lscr1
               kscr2 = lscr2
               kscr3 = lscr3
               ifill(5) = ifila(5)
               ichlk = chlsky
               IF ( ifila(5)<=2 ) THEN
                  nz = korsz(zzzz)
                  iscmsg = lscr5
                  iscdia = lscr6
                  kpdfck = pdefck
                  kdgck = diagck
                  kdget = diaget
                  CALL sdcmps(zzzz,zzzz,zzzz)
                  IF ( nerr(1)+nerr(2)==0 ) THEN
                     IF ( parm(1)/=0 ) CALL mesage(parm(1),parm(2),parm(3))
                  ELSE
                     buf6 = korsz(zm) - 2*nbufsz + 1
                     IF ( buf6+nbufsz<=0 ) THEN
!
!     INSUFFICIENT CORE
!
                        parm(1) = -8
                        parm(2) = -buf6 - nbufsz
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        CALL sdcmm(zm,set(1),ifila(2),ifila(1),luset,lgpl,lsil,subnam)
                        sing = 0
!
!     ONLY ES CHECK AND NONCONSERVATIVE COLUMN CAN EXIT WITH SING = 1
!     OR IF USER DESIRES TO CONTINUE
!
                        IF ( noglev>0 ) sing = -1
                        IF ( parm(1)/=0 ) CALL mesage(parm(1),parm(2),parm(3))
                     ENDIF
                  ENDIF
               ELSE
                  nz = korsz(z)
                  CALL sdcomp(*20,z,z,z)
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         det(1) = sdet
         det(2) = sdetc
         mindia = minds
         power = kpow
         ifill(2) = ifila(2)
         ifill(3) = ifila(3)
         ifill(4) = 4
         IF ( sing>=0 ) CALL wrttrl(ifill)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
 20      sing = -1
         det(1) = 0.0
         det(2) = 0.0
         power = 0
         mindia = 0.0
         spag_nextblock_1 = 3
      CASE (3)
         RETURN
      CASE (4)
         parm(3) = name(1)
         parm(4) = name(2)
         IF ( parm(1)/=0 ) CALL mesage(parm(1),parm(2),parm(3))
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ddcmps
