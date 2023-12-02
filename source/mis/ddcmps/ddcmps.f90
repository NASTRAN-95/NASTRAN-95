!*==ddcmps.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddcmps
USE C_BLANK
USE C_CDCMPX
USE C_DCOMPX
USE C_NAMES
USE C_SDCQ
USE C_SFACT
USE C_SYSTEM
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
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
         Opnscr = .FALSE.
         First = .TRUE.
         Sing = 1
         Ja(1) = kaa
         CALL rdtrl(Ja)
         IF ( Ja(1)<0 ) THEN
!
!     ERROR  MESSAGES
!
!     PURGED INPUT
!
            Parm(1) = -1
            Parm(2) = kaa
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            iform = Ja(4)
            IF ( Isym<0 ) THEN
               IF ( iform==sym ) THEN
                  CALL page2(2)
                  WRITE (outpt,99001) Swm , nam
99001             FORMAT (A27,' 2340, MODULE ',2A4,' HAS BEEN REQUESTED TO DO ','UNSYMETRIC DECOMPOSITION OF A SYMETRIC MATRIX')
               ENDIF
               iform = rect
               IF ( Ja(2)==Ja(3) ) iform = sqr
            ELSEIF ( Isym/=0 ) THEN
!
               IF ( iform/=sym ) THEN
                  CALL page2(2)
                  WRITE (outpt,99002) Swm , nam
99002             FORMAT (A27,' 2341, MODULE ',2A4,' HAS BEEN FURNISHED A SQUARE ',                                                 &
                         &'MATRIX MARKED UNSYMETRIC FOR SYMETRIC DECOMPOSITION.')
                  iform = sym
               ENDIF
            ENDIF
            Isym = -1
            IF ( iform==sym ) Isym = 1
            Ja(4) = iform
            i = 0
            IF ( Ja(2)/=Ja(3) ) THEN
               CALL page2(2)
               i = 1
               WRITE (outpt,99003) Swm , nam
99003          FORMAT (A27,' 2375, MODULE ',2A4,' HAS BEEN REQUESTED TO ','DECOMPOSE A RECTANGULAR MATRIX')
            ENDIF
            IF ( Isym<0 ) THEN
!
!     SET UP CALL TO DECOMP
!
               IF ( Ja(5)>2 ) THEN
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
                  CALL cdcomp(*20,zzz,zzz,zzz)
                  Ju(5) = 4
                  Jl(4) = 4
                  Ju(4) = 5
                  Jl(3) = Jl(2)
                  Ju(3) = Ju(2)
                  Det(1) = Cdet(1)
                  Det(2) = Cdet(2)
                  Mindia = Cmndia
                  Power = Jpow
                  CALL wrttrl(Jl)
                  CALL wrttrl(Ju)
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
                  CALL decomp(*20,zz,zz,zz)
                  Iu(5) = 2
                  Il(4) = 4
                  Iu(4) = 5
                  Il(3) = Il(2)
                  Iu(3) = Iu(2)
                  Det(1) = Ddet
                  Det(2) = 0.0
                  Power = Ipow
                  Mindia = Dmndia
                  CALL wrttrl(Iu)
                  CALL wrttrl(Il)
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
               Parm(1) = -16
               Parm(2) = kaa
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSE
               Ifila(1) = kaa
               CALL rdtrl(Ifila)
               Ifill(1) = lll
               Ifilu(1) = lscr4
               Kscr1 = lscr1
               Kscr2 = lscr2
               Kscr3 = lscr3
               Ifill(5) = Ifila(5)
               Ichlk = Chlsky
               IF ( Ifila(5)<=2 ) THEN
                  Nz = korsz(zzzz)
                  Iscmsg = lscr5
                  Iscdia = lscr6
                  Kpdfck = Pdefck
                  Kdgck = Diagck
                  Kdget = Diaget
                  CALL sdcmps(zzzz,zzzz,zzzz)
                  IF ( Nerr(1)+Nerr(2)==0 ) THEN
                     IF ( Parm(1)/=0 ) CALL mesage(Parm(1),Parm(2),Parm(3))
                  ELSE
                     Buf6 = korsz(zm) - 2*nbufsz + 1
                     IF ( Buf6+nbufsz<=0 ) THEN
!
!     INSUFFICIENT CORE
!
                        Parm(1) = -8
                        Parm(2) = -Buf6 - nbufsz
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        CALL sdcmm(zm,Set(1),Ifila(2),Ifila(1),luset,lgpl,lsil,Subnam)
                        Sing = 0
!
!     ONLY ES CHECK AND NONCONSERVATIVE COLUMN CAN EXIT WITH SING = 1
!     OR IF USER DESIRES TO CONTINUE
!
                        IF ( Noglev>0 ) Sing = -1
                        IF ( Parm(1)/=0 ) CALL mesage(Parm(1),Parm(2),Parm(3))
                     ENDIF
                  ENDIF
               ELSE
                  Nz = korsz(Z)
                  CALL sdcomp(*20,Z,Z,Z)
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         Det(1) = Sdet
         Det(2) = Sdetc
         Mindia = Minds
         Power = Kpow
         Ifill(2) = Ifila(2)
         Ifill(3) = Ifila(3)
         Ifill(4) = 4
         IF ( Sing>=0 ) CALL wrttrl(Ifill)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
 20      Sing = -1
         Det(1) = 0.0
         Det(2) = 0.0
         Power = 0
         Mindia = 0.0
         spag_nextblock_1 = 3
      CASE (3)
         RETURN
      CASE (4)
         Parm(3) = name(1)
         Parm(4) = name(2)
         IF ( Parm(1)/=0 ) CALL mesage(Parm(1),Parm(2),Parm(3))
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ddcmps
