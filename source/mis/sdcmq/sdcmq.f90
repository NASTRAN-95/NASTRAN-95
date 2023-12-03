!*==sdcmq.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdcmq(*,Key,V1,V,Dv1,Dv,Ic,Z)
   USE c_names
   USE c_sdcq
   USE c_sfact
   USE c_system
   USE c_xmssg
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Key
   REAL :: V1
   REAL :: V
   REAL(REAL64) :: Dv1
   REAL(REAL64) :: Dv
   INTEGER :: Ic
   INTEGER , DIMENSION(1) :: Z
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: filerr , i
   INTEGER , DIMENSION(3) :: iv
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: rv , rv1
   EXTERNAL close , open , page2 , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( filcur>0 ) CALL close(filcur,kcl2)
         filerr = filscr
         IF ( .NOT.(opnscr) ) THEN
            IF ( .NOT.first ) CALL open(*20,filscr,Z(buf),kwt3)
            IF ( first ) CALL open(*20,filscr,Z(buf),kwr1)
            first = .FALSE.
            opnscr = .TRUE.
         ENDIF
!
         iv(1) = Ic*10 + Key
         IF ( iprec==1 ) THEN
            rv = V
            rv1 = V1
         ELSE
            rv = Dv
            rv1 = Dv1
         ENDIF
         CALL write(filscr,iv,3,1)
!
!     CONVERT FILES TO ORIGINAL STATUS
!
         IF ( filcur/=0 ) THEN
            CALL close(filscr,kcl2)
            opnscr = .FALSE.
            IF ( filcur>0 ) THEN
               filerr = filcur
!
!     READ MODE ON CURRENT FILE
!
               IF ( stscr==1 ) i = krd2
!
!     WRITE MODE ON CURRENT FILE
!
               IF ( stscr==2 ) i = kwt3
               CALL open(*20,filcur,Z(buf),i)
            ENDIF
         ENDIF
!
!     DETERMINE ABORT FLAG
!
         IF ( Key==1 ) THEN
!
            noglev = 2
         ELSEIF ( Key==3 ) THEN
!
!     NEGATIVE DIAGONAL
!
            IF ( ichly==1 ) THEN
               IF ( iprec==1 ) V = -V
               IF ( iprec==2 ) Dv = -Dv
            ENDIF
            IF ( pdefck/=0 ) noglev = max0(noglev,1)
         ELSEIF ( Key==4 .OR. Key==6 .OR. Key==7 ) THEN
!
!     ES SINGULARITY CHECK, DIAG-IN=0.0, NON-CONSERVATIVE MATRIX
!
            nerr(1) = nerr(1) + 1
            IF ( diagck/=0 ) THEN
               noglev = max0(noglev,1)
               IF ( nerr(1)>=diagck ) noglev = 3
            ENDIF
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Key==5 ) THEN
!
!     UNEXPECTED NULL COLUMN
!
            noglev = 3
         ELSE
!
!     ZERO DIAGONAL - DECOMPOSED MATRIX
!
            noglev = max0(noglev,1)
            IF ( iprec==1 ) V = 1.0
            IF ( iprec==2 ) Dv = 1.D0
         ENDIF
         nerr(2) = nerr(2) + 1
         IF ( nerr(2)>iabs(pdefck) .AND. pdefck/=0 ) noglev = 3
         spag_nextblock_1 = 2
      CASE (2)
!
         IF ( noglev==3 ) RETURN 1
         RETURN
!
!     UNABLE TO USE FILES - WRITE GINO NUMBER. ABORT AT MAJOR DECOMP
!     CHCK
!
 20      CALL page2(2)
         WRITE (iout,99001) swm , filerr , name , Ic , Key
99001    FORMAT (A27,' 2379, FILE',I8,' COULD NOT BE OPENED IN',A4,A1,'. COLUMN',I8,' SINGULAR, REASON',I3)
         parm(1) = -37
         parm(2) = filscr
         parm(3) = name(1)
         parm(4) = name(2)
         noglev = 4
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sdcmq