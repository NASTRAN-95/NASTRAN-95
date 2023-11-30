
SUBROUTINE sdcmq(Key,V1,V,Dv1,Dv,Ic,Z) !HIDESTARS (*,Key,V1,V,Dv1,Dv,Ic,Z)
   IMPLICIT NONE
   INTEGER Buf , Diagck , Filcur , Filscr , Ichly , Iout , Iprec , Isb , Kcl2 , Krd2 , Krr0 , Kwr1 , Kwt3 , Nerr(2) , Noglev ,      &
         & Parm(4) , Pdefck , Stscr
   REAL Diaget , Skpn , Skpsf(32)
   LOGICAL First , Opnscr
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /names / Krd2 , Krr0 , Kwt3 , Kwr1 , Skpn , Kcl2
   COMMON /sdcq  / Nerr , Noglev , Buf , Filscr , Filcur , Stscr , Pdefck , Diagck , Diaget , Iprec , Parm , Opnscr , First
   COMMON /sfact / Skpsf , Ichly
   COMMON /system/ Isb , Iout
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   DOUBLE PRECISION Dv , Dv1
   INTEGER Ic , Key
   REAL V , V1
   INTEGER Z(1)
   INTEGER filerr , i , iv(3) , name(2)
   REAL rv , rv1
!
!     THIS SUBROUTINE CREATES A SCRATCH FILE OF QUEUED SINGULARITY
!     MESSAGES.  EACH MESSAGE IS A GINO RECORD DUE TO POSSIBLE CLOSE
!     WITHOUT REWIND.
!     THE -KEY- IS AS FOLLOWS,
!      1  - NULL COLUMN       - INPUT MATRIX
!      2  - ZERO DIAGONAL     - DECOMPOSED MATRIX.
!      3  - NEGATIVE DIAGONAL - DECOMPOSED MATRIX
!      4  - SINGULARITY TOLERANCE FAILURE - DECOMPOSED MATRIX.
!      5  - UNEXPECTED NULL COLUMN OR END OF COLUMN - ABORT IMMIDIATELY.
!      6  - NONCONSERVATIVE COLUMN  D/A.GT.1.001
!      7  - ZERO DIAGONAL     - INPUT MATRIX.
!     OTHER ARGUMENTS ARE
!      $  - NONSTANDARD RETURN IF DECOMPOSITION IS TO BE ABORTED.
!      Z  - OPEN CORE.  BUFFER LOCATIONS RELATIVE TO Z(1).
!      V  - RSP VALUE OF ENTRY IN ERROR (DV IS DOUBLE PRECISION).
!      V1 - INPUT VALUE OF DIAGONAL (DV1 IS DOUBLE PRECISION VERSION).
!      IC - COLUMN NUMBER IN ERROR.
!    THE ARGUMENTS ARE NOT CHANGED.
!    /SDCQ/ CONTAINS CONSTANT DATA.
!      FILCUR - CURRENT FILE USING BUFFER FOR SCRATCH FILE.  NEGATIVE IF
!               NONE, ZERO IF FILSCR IS TO REMAIN OPEN
!      STSCR  - GINO FILE STATUS FOR REOPENING FILCUR (1=READ,2=WRITE)
!      FILSCR - SCRATCH FILE NAME.
!      BUF    - BUFFER LOCATION RELATIVE TO Z(1).
!      NERR(2)- COUNT OF NUMBER OF ERROR CALLS ( (1)=ES, (2)=PD CHECK)
!      DIAGCK - EXIT FLAG FOR KEY=4 -- 0=NONFATAL, +N = MAX.-MESSAGES
!               WITHOUT ABORTING, -N = IMMEDIATE ABORT
!      IPREC  - 1=RSP, USE V. 2=RDP, USE DV.
!      PDEFCK - EXIT FLAG FOR KEY=3.  0 = NONFATAL IF -V, FATAL AT END
!               OF DECOMP FOR V=0.  +N = MAX-MESSAGES WITHOUT ABORTING.
!               -N = IMMEDIATE ABORT
!      NOGLEV - NOGO CODE.
!             = 0, NO FATAL ERRORS,
!             = 1, ABORT AT END OF DECOMP,
!             = 2, ABORT AT END OF PREPASS
!             = 3, ABORT,NONSTD RET.
!             = 4, INTERNAL ERRORS.  ABORT AT MAJOR CHECK-POINTS.
!-----
   !>>>>EQUIVALENCE (rv1,iv(2)) , (rv,iv(3))
   DATA name/4HSDCM , 2HQ /
!
   IF ( Filcur>0 ) CALL close(Filcur,Kcl2)
   filerr = Filscr
   IF ( .NOT.(Opnscr) ) THEN
      IF ( .NOT.First ) CALL open(*200,Filscr,Z(Buf),Kwt3)
      IF ( First ) CALL open(*200,Filscr,Z(Buf),Kwr1)
      First = .FALSE.
      Opnscr = .TRUE.
   ENDIF
!
   iv(1) = Ic*10 + Key
   IF ( Iprec==1 ) THEN
      rv = V
      rv1 = V1
   ELSE
      rv = Dv
      rv1 = Dv1
   ENDIF
   CALL write(Filscr,iv,3,1)
!
!     CONVERT FILES TO ORIGINAL STATUS
!
   IF ( Filcur/=0 ) THEN
      CALL close(Filscr,Kcl2)
      Opnscr = .FALSE.
      IF ( Filcur>0 ) THEN
         filerr = Filcur
!
!     READ MODE ON CURRENT FILE
!
         IF ( Stscr==1 ) i = Krd2
!
!     WRITE MODE ON CURRENT FILE
!
         IF ( Stscr==2 ) i = Kwt3
         CALL open(*200,Filcur,Z(Buf),i)
      ENDIF
   ENDIF
!
!     DETERMINE ABORT FLAG
!
   IF ( Key==1 ) THEN
!
      Noglev = 2
   ELSEIF ( Key==3 ) THEN
!
!     NEGATIVE DIAGONAL
!
      IF ( Ichly==1 ) THEN
         IF ( Iprec==1 ) V = -V
         IF ( Iprec==2 ) Dv = -Dv
      ENDIF
      IF ( Pdefck/=0 ) Noglev = max0(Noglev,1)
   ELSEIF ( Key==4 .OR. Key==6 .OR. Key==7 ) THEN
!
!     ES SINGULARITY CHECK, DIAG-IN=0.0, NON-CONSERVATIVE MATRIX
!
      Nerr(1) = Nerr(1) + 1
      IF ( Diagck/=0 ) THEN
         Noglev = max0(Noglev,1)
         IF ( Nerr(1)>=Diagck ) Noglev = 3
      ENDIF
      GOTO 100
   ELSEIF ( Key==5 ) THEN
!
!     UNEXPECTED NULL COLUMN
!
      Noglev = 3
   ELSE
!
!     ZERO DIAGONAL - DECOMPOSED MATRIX
!
      Noglev = max0(Noglev,1)
      IF ( Iprec==1 ) V = 1.0
      IF ( Iprec==2 ) Dv = 1.D0
   ENDIF
   Nerr(2) = Nerr(2) + 1
   IF ( Nerr(2)>iabs(Pdefck) .AND. Pdefck/=0 ) Noglev = 3
!
 100  IF ( Noglev==3 ) RETURN 1
   RETURN
!
!     UNABLE TO USE FILES - WRITE GINO NUMBER. ABORT AT MAJOR DECOMP
!     CHCK
!
 200  CALL page2(2)
   WRITE (Iout,99001) Swm , filerr , name , Ic , Key
99001 FORMAT (A27,' 2379, FILE',I8,' COULD NOT BE OPENED IN',A4,A1,'. COLUMN',I8,' SINGULAR, REASON',I3)
   Parm(1) = -37
   Parm(2) = Filscr
   Parm(3) = name(1)
   Parm(4) = name(2)
   Noglev = 4
   GOTO 100
END SUBROUTINE sdcmq