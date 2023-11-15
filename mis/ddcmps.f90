
SUBROUTINE ddcmps
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf6 , Chlsky , Diagck , Diaget , Ia(7) , Ib , Ichlk , Ifila(7) , Ifill(7) , Ifilu(7) , Il(7) , Ipow , Iscdia , Iscmsg , &
         & Iscr1 , Iscr2 , Iscr3 , Istscr , Isym , Iu(7) , Ja(7) , Jb , Jl(7) , Jpow , Jscr1 , Jscr2 , Jscr3 , Ju(7) , Kdgck ,      &
         & Kdget , Knames(19) , Kpdfck , Kpow , Kprec , Kscr1 , Kscr2 , Kscr3 , Ksystm(69) , Nbufsz , Nerr(2) , Noglev , Nz , Nzz , &
         & Nzzz , Outpt , Parm(4) , Pdefck , Power , Rect , Set(2) , Sing , Sqr , Sym
   DOUBLE PRECISION Cdet(2) , Cmndia , Ddet , Dmndia , Mindia , Minds , Sdet , Sdetc
   REAL Det(2) , Subnam(2) , Z(1) , Zm(1) , Zz(1) , Zzz(1) , Zzzz(1)
   LOGICAL First , Opnscr
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Isym , Diagck , Diaget , Pdefck , Sing , Set , Chlsky , Det , Mindia , Power , Subnam
   COMMON /cdcmpx/ Ja , Jl , Ju , Jscr1 , Jscr2 , Jscr3 , Cdet , Jpow , Nzzz , Cmndia , Jb
   COMMON /dcompx/ Ia , Il , Iu , Iscr1 , Iscr2 , Iscr3 , Ddet , Ipow , Nzz , Dmndia , Ib
   COMMON /names / Knames
   COMMON /sdcq  / Nerr , Noglev , Buf6 , Iscmsg , Iscdia , Istscr , Kpdfck , Kdgck , Kdget , Kprec , Parm , Opnscr , First
   COMMON /sfact / Ifila , Ifill , Ifilu , Kscr1 , Kscr2 , Nz , Sdet , Sdetc , Kpow , Kscr3 , Minds , Ichlk
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER i , iform , kaa , lgpl , lll , lscr1 , lscr2 , lscr3 , lscr4 , lscr5 , lscr6 , lsil , luset , nam(2) , name(2) , ull
   INTEGER korsz
!
! End of declarations
!
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
   EQUIVALENCE (Zz(1),Z(1))
   EQUIVALENCE (Zzz(1),Z(1))
   EQUIVALENCE (Zzzz(1),Z(1))
   EQUIVALENCE (Zm(1),Z(1))
   EQUIVALENCE (Ksystm(1),Nbufsz) , (Ksystm(2),Outpt) , (Knames(12),Sqr) , (Knames(13),Rect) , (Knames(17),Sym)
   DATA luset , lgpl , lsil , kaa , lll , ull , lscr1 , lscr2 , lscr3/101 , 102 , 103 , 104 , 201 , 202 , 301 , 302 , 303/
   DATA lscr4 , lscr5 , lscr6/304 , 305 , 306/
   DATA name/4HDDCM , 4HPS  /
   DATA nam/4HSDCM , 4HPS  /
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
      GOTO 400
   ELSE
      iform = Ja(4)
      IF ( Isym<0 ) THEN
         IF ( iform==Sym ) THEN
            CALL page2(2)
            WRITE (Outpt,99001) Swm , nam
99001       FORMAT (A27,' 2340, MODULE ',2A4,' HAS BEEN REQUESTED TO DO ','UNSYMETRIC DECOMPOSITION OF A SYMETRIC MATRIX')
         ENDIF
         iform = Rect
         IF ( Ja(2)==Ja(3) ) iform = Sqr
      ELSEIF ( Isym/=0 ) THEN
!
         IF ( iform/=Sym ) THEN
            CALL page2(2)
            WRITE (Outpt,99002) Swm , nam
99002       FORMAT (A27,' 2341, MODULE ',2A4,' HAS BEEN FURNISHED A SQUARE ','MATRIX MARKED UNSYMETRIC FOR SYMETRIC DECOMPOSITION.')
            iform = Sym
         ENDIF
      ENDIF
      Isym = -1
      IF ( iform==Sym ) Isym = 1
      Ja(4) = iform
      i = 0
      IF ( Ja(2)/=Ja(3) ) THEN
         CALL page2(2)
         i = 1
         WRITE (Outpt,99003) Swm , nam
99003    FORMAT (A27,' 2375, MODULE ',2A4,' HAS BEEN REQUESTED TO ','DECOMPOSE A RECTANGULAR MATRIX')
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
            Nzzz = korsz(Zzz)
            Jl(5) = 4
            Jb = 0
            CALL cdcomp(*200,Zzz,Zzz,Zzz)
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
            Nzz = korsz(Zz)
            Iscr1 = lscr1
            Iscr2 = lscr2
            Iscr3 = lscr3
            Ib = 0
            Il(5) = 2
            CALL decomp(*200,Zz,Zz,Zz)
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
         GOTO 300
!
!     SET UP CALL TO SDCOMP
!
      ELSEIF ( i/=0 ) THEN
!
!     NUMBER ROWS.NE.COLUMNS
!
         Parm(1) = -16
         Parm(2) = kaa
         GOTO 400
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
            Nz = korsz(Zzzz)
            Iscmsg = lscr5
            Iscdia = lscr6
            Kpdfck = Pdefck
            Kdgck = Diagck
            Kdget = Diaget
            CALL sdcmps(Zzzz,Zzzz,Zzzz)
            IF ( Nerr(1)+Nerr(2)==0 ) THEN
               IF ( Parm(1)/=0 ) CALL mesage(Parm(1),Parm(2),Parm(3))
            ELSE
               Buf6 = korsz(Zm) - 2*Nbufsz + 1
               IF ( Buf6+Nbufsz<=0 ) THEN
!
!     INSUFFICIENT CORE
!
                  Parm(1) = -8
                  Parm(2) = -Buf6 - Nbufsz
                  GOTO 400
               ELSE
                  CALL sdcmm(Zm,Set(1),Ifila(2),Ifila(1),luset,lgpl,lsil,Subnam)
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
            CALL sdcomp(*200,Z,Z,Z)
         ENDIF
      ENDIF
   ENDIF
 100  Det(1) = Sdet
   Det(2) = Sdetc
   Mindia = Minds
   Power = Kpow
   Ifill(2) = Ifila(2)
   Ifill(3) = Ifila(3)
   Ifill(4) = 4
   IF ( Sing>=0 ) CALL wrttrl(Ifill)
   GOTO 300
!
 200  Sing = -1
   Det(1) = 0.0
   Det(2) = 0.0
   Power = 0
   Mindia = 0.0
 300  RETURN
 400  Parm(3) = name(1)
   Parm(4) = name(2)
   IF ( Parm(1)/=0 ) CALL mesage(Parm(1),Parm(2),Parm(3))
   GOTO 100
END SUBROUTINE ddcmps
