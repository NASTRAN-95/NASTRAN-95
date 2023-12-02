!*==dummy.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dummy
USE c_machin
USE c_system
USE C_MACHIN
USE C_SYSTEM
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   *0() :: 
   *0() :: 
   *0() :: 
   *0() :: 
   *0() :: 
   *0() :: 
   *0() :: 
   *0() :: 
   *0() :: 
   *0() :: 
   *0() :: 
   *0() :: 
   *0() :: 
   *0() :: 
!
! Local variable declarations rewritten by SPAG
!
   REAL :: Code , O
   INTEGER :: I , I1 , I2 , I3 , I4 , I5 , J , K , Key , L , M
   INTEGER , DIMENSION(1) :: N
   CHARACTER(8) :: name
!
! End of declarations rewritten by SPAG
!
!
!     NOTE:
!     THIS DUMMY.MIS ROUTINE CONTAINS 4 MACHINE VERSIONS (IBM,CDC,VAX,
!     AND UNIVAC). MOVE THIS SUBROUTINE TO THE MDS GROUP AND
!     REPLACE ALL THE 'C+' BY 2 SPACES IF MACHINE IS IBM, OR
!     REPLACE ALL THE 'C-' BY 2 SPACES IF MACHINE IS CDC, OR
!     REPLACE ALL THE 'C=' BY 2 SPACES IF MACHINE IS VAX, AND UNIX, OR
!     REPLACE ALL THE 'C*' BY 2 SPACES IF MACHINE IS UNIVAC
!     REPLACE ALL THE 'C.' BY 2 SPACES IF MACHINE TYPE IS 1, AND 11-20
!
! ****
!     IBM VERSION
!
!     THIS SUBROUTINE PROVIDES ENTRIES FOR THE DUMMY ROUTINES
!     USED BY OTHER COMPUTER MACHINES, AND ARE REFERENCED IN
!     VARIOUS NASTRAN LINKS
!
!     THIS SUBROUTINE INCLUDES ALSO SOME DUMMY ROUTINES NOT YET
!     WRITTEN
!
!     THIS ROUTINE SHOULD BE MOVED TO NASTRAN MACHINE-DEPENDENT
!     SECTION (MDS)
! ****
!
!+    DIMENSION       N(1)
!+    CHARACTER*8     NAME
!
!+    COMMON /MACHIN/ MACH
!+    COMMON /SYSTEM/ ISYSBF, NOUT
!
!+    IF (MACH .EQ. 2) GO TO 250
!+    WRITE  (NOUT,20) MACH
!+ 20 FORMAT (/,' MACH =',I7)
!+    NAME = 'DUMMY'
!+    GO TO 100
!
! ****
!     ROUTINES USED ONLY IN UNIVAC MACHINE
! ****
!
!
!+    ENTRY NTRAN (I,J,K)
!+    NAME = 'NTRAN'
!+    GO TO 100
!
!+    ENTRY CONTIN
!+    NAME = 'CONTIN'
!+    GO TO 100
!
!+    ENTRY FACIL (I,J)
!+    NAME = 'FACIL'
!+    GO TO 100
!
!+    ENTRY FACSF (I)
!+    NAME = 'FACSF'
!+    GO TO 100
!
!+    ENTRY UNVOPN (I)
!+    NAME = 'UNVOPN'
!+    GO TO 100
!
!+    ENTRY UNVCLS (I)
!+    NAME = 'UNVCLS'
!+    GO TO 100
!
!+    ENTRY ADDCRD (I,J)
!+    NAME = 'ADDCRD'
!+    GO TO 100
!
! ****
!     ROUTINES USED BY UNIVAC AND IBM
! ****
!
!     ENTRY RETURN
!     GO TO 250
!
!+    ENTRY MSGUNI
!+    IF (MACH .EQ. 2) GO TO 250
!+    NAME = 'MSGUNI'
!+    GO TO 100
!
!+    ENTRY XEOT (I,J,K,L)
!+    IF (MACH .EQ. 2) GO TO 250
!+    NAME = 'XEOT'
!+    GO TO 100
!
!     ENTRY TPSWIT (I,J,K,L)
!     NAME = 'TPSWIT'
!     GO TO 100
!
! ****
!     ROUTINES USED ONLY IN IBM MACHINE
! ****
!
!     ENTRY UMFTRN (I)
!     NAME = 'UMFTRN'
!     GO TO 100
!
!     ENTRY TAPSWI (I,J,K,L)
!     NAME = 'TAPSWI'
!     GO TO 100
!
!     ENTRY SOFIOI
!     NAME = 'SOFIOI'
!     GO TO 100
!
!     ENTRY SEARCH (I)
!     NAME = 'SEARCH'
!     GO TO 100
!
! ... NEXT THREE ARE SYSTEM ROUTINES THAT OPEN FILE DYNAMICALLY WITHOUT
!     THE USE OF JCL. THESE ROUTINES ARE COMMONLY 'LOCAL INSTALLED'.
!
!     IQADDN CHECKS WHETHER A FILE EXISTS OR NOT
!     QQDCBF BUILDS AN ATTRIBUTE LIST BY DDNAME
!     QQGETF ALLOCATES FILE IN TSO OR BATCH
!
!     ENTRY IQZDDN (I)
!     NAME = 'IQZDDN'
!     GO TO 100
!
!     ENTRY QQDCBF (I,J,K,L,M,N)
!     NAME = 'QQDCBF'
!     GO TO 100
!
!     ENTRY QQGETF (I,J,K)
!     NAME = 'QQGETF'
!     GO TO 100
!
! ****
!     ROUTINE USED ONLY BY IBM AND VAX
! ****
!
!     ENTRY SOFIOF
!     NAME = 'SOFIOF'
!     GO TO 100
!
!     THE FOLLOWING THREE ARE FUNCTIONS FOR QUAD WORD OPERATIONS
!                                           (REAL*16)
!     ENTRY QABS (I)
!     NAME = 'QABS'
!     GO TO 100
!
!     ENTRY SNGLQ (I)
!     NAME = 'SNGLQ'
!     GO TO 100
!
!     ENTRY DBLEQ (I)
!     NAME = 'DBLEQ'
!     GO TO 100
!
!     ENTRY QSQRT (I)
!     NAME = 'QSQRT'
!     GO TO 100
!
!     ENTRY QLOG (I)
!     NAME = 'QLOG'
!     GO TO 100
!
!     ENTRY QEXTD (I)
!     NAME = 'QEXTD'
!     GO TO 100
!
! ****
!     ROUTINE USED BY UNIVAC AND VAX
! ****
!
!+    ENTRY DEFCOR
!+    NAME = 'DEFCOR'
!+    GO TO 100
!
! ****
!     ROUTINES USED BY ALL MACHINES, EXCEPT VAX
! ****
!
!     ENTRY GPERR
!     NAME = 'GPERR'
!     GO TO 100
!
!     ENTRY PDUMP
!     GO TO 250
!
!     ENTRY MPY1
!     NAME = 'MPY1'
!     GO TO 100
!
!     ENTRY MPY2NT
!     NAME = 'MPY2NT'
!     GO TO 100
!
!     ENTRY MPY2T
!     NAME = 'MPY2T'
!     GO TO 100
!
! ****
!     ROUTINES USED ONLY IN CDC MACHINE
! ****
!
!+    ENTRY LINK (I,J,K)
!+    NAME = 'LINK'
!+    GO TO 100
!
!+    ENTRY REMARK (I)
!+    NAME = 'REMARK'
!+    GO TO 100
!
!+    ENTRY CDCBUG (I,J,K,L)
!+    NAME = 'CDCBUG'
!+    GO TO 100
!
!+    ENTRY CDCOPN (I)
!+    NAME = 'CDCOPN'
!+    GO TO 100
!
!+    ENTRY CDCCLS (I)
!+    NAME = 'CDCCLS'
!+    GO TO 100
!
!+    ENTRY CDCKSZ (I)
!+    NAME = 'CDCKSZ'
!+    GO TO 100
!
!+    ENTRY PF (I,J,K)
!+    NAME = 'PF'
!+    GO TO 100
!
!+    ENTRY ISWAP (I)
!+    NAME = 'ISWAP'
!+    GO TO 100
!
! ****
!     ROUTINES USED ONLY IN VAX MACHINE
! ****
!
!+    ENTRY VAXEND
!+    NAME = 'VAXEND'
!+    GO TO 100
!
!+    ENTRY VAXERR (L)
!+    WRITE (NOUT,50) L
!+ 50 FORMAT (/,' *** GINO ERROR AT LOC',I5)
!+    GO TO 220
!
!+    ENTRY VAXSCH
!+    NAME = 'VAXSCH'
!+    GO TO 100
!
!+    ENTRY VAXBRK
!+    NAME = 'VAXBRK'
!+    GO TO 100
!
!+    ENTRY MPY1V (I,J,K)
!+    NAME = 'MPY1V'
!+    GO TO 100
!
!+    ENTRY MPY2NV (I,J,K)
!+    NAME = 'MPY2NV'
!+    GO TO 100
!
!+    ENTRY MPY2TV (I,J,K)
!+    NAME = 'MPY2TV'
!+    GO TO 100
!
! ****
!     ROUTINES THAT PERFORM NO PARTICULAR FUNCTIONS, BUT THEY
!     ARE STILL CALLED BY NASTRAN
! ****
!
!+    ENTRY UNLOAD (I)
!     CALLED BY INPTT1
!+    GO TO 250
!
! ****
!     THE FOLLOWING ROUTINES SEEM TO BE NO LONGER USED IN NASTRAN
! ****
!
!+    ENTRY JIDINT (I)
!+    NAME = 'JIDINT'
!+    GO TO 100
!
!+    ENTRY OPMESG
!+    NAME = 'OPMESG'
!+    GO TO 100
!
!     ENTRY PDUM1,PDUM2,...,PDUM9 HAD BEEN REPLACED BY PDUMI
!     ENTRY QDMM3, SQDM31, AND SQDM32 ARE NOW OBSOLETE
!
!+    ENTRY SEMTRN
!+    NAME = 'SEMTRN'
!+    GO TO 100
!
! ****
!     DUMMY ROUTINES REFERENCED ONLY IN LINK 2, ALL MACHINES
! ****
!
!+    ENTRY PDUMI (*,*,*,I,J,K,L,M,N,O)
!+    NAME = 'PDUMI'
!+    GO TO 100
!
! ****
!     DUMMY ROUTINES REFERENCED ONLY IN LINK 5, ALL MACHINES
! ****
!
!+    ENTRY PLBAR1 (I,J)
!+    NAME = 'PLBAR1'
!+    GO TO 100
!
!+    ENTRY PLOADX
!+    NAME = 'PLOADX'
!+    GO TO 100
!
!+    ENTRY ERRTRC (NAM)
!     ==================
!     ERROR TRACEBACK
!
!+    GO TO 220
!
!+100 WRITE  (NOUT,150) NAME
!+150 FORMAT ('0*** SYSTEM FATAL ERROR  ---  JOB TERMINATED',
!+   1        ' DUE TO CALL TO DUMMY SUBROUTINE.  ENTRY NAME IS ', A8)
!+    GO TO 220
!
! ****
!     TO FORCE A SYSTEM FATAL ERROR FOR TRACEBACK
! ****
!
!+220 WRITE  (NOUT,230)
!+230 FORMAT ('0*** ERROR TRACEBACK IN SYSTEM LOG FILE')
!+    I = 987654321
!+    N(I) = 1
!+250 RETURN
!
!
!     SUBROUTINE DUMMY
!
! ****
!     CDC VERSION
!
!     THIS SUBROUTINE PROVIDES ENTRIES FOR THE DUMMY ROUTINES
!     USED BY OTHER COMPUTER MACHINES, AND ARE REFERENCED IN
!     VARIOUS NASTRAN LINKS
!
!     THIS SUBROUTINE INCLUDES ALSO SOME DUMMY ROUTINES NOT YET
!     WRITTEN
!
!     THIS ROUTINE SHOULD BE MOVED TO NASTRAN MACHINE-DEPENDENT
!     SECTION (MDS)
! ****
!
!-    CHARACTER*8     NAME
!
!-    COMMON /MACHIN/ MACH
!-    COMMON /SYSTEM/ ISYSBF, NOUT
!
!-    IF (MACH .EQ. 4) GO TO 250
!-    WRITE  (NOUT,20) MACH
!- 20 FORMAT (/,' MACH =',I7)
!-    NAME = 'DUMMY'
!-    GO TO 100
!
! ****
!     ROUTINES USED ONLY IN UNIVAC MACHINE
! ****
!
!-    ENTRY NTRAN (I,J,K)
!-    NAME = 'NTRAN'
!-    GO TO 100
!
!-    ENTRY CONTIN
!-    NAME = 'CONTIN'
!-    GO TO 100
!
!-    ENTRY FACIL (I,J)
!-    NAME = 'FACIL'
!-    GO TO 100
!
!-    ENTRY FACSF (I)
!-    NAME = 'FACSF'
!-    GO TO 100
!
!-    ENTRY UNVOPN (I)
!-    NAME = 'UNVOPN'
!-    GO TO 100
!
!-    ENTRY UNVCLS (I)
!-    NAME = 'UNVCLS'
!-    GO TO 100
!
!-    ENTRY ADDCRD (I,J)
!-    NAME = 'ADDCRD'
!-    GO TO 100
!
! ****
!     ROUTINES USED BY UNIVAC AND IBM
! ****
!
!-    ENTRY RETURN
!-    GO TO 250
!
!-    ENTRY MSGUNI
!-    IF (MACH .EQ. 2) GO TO 250
!-    NAME = 'MSGUNI'
!-    GO TO 100
!
!-    ENTRY XEOT (I,J,K,L)
!-    IF (MACH .EQ. 2) GO TO 250
!-    NAME = 'XEOT'
!-    GO TO 100
!
!-    ENTRY TPSWIT (I,J,K,L)
!-    NAME = 'TPSWIT'
!-    GO TO 100
!
! ****
!     ROUTINES USED ONLY IN IBM MACHINE
! ****
!
!-    ENTRY UMFTRN (I)
!-    NAME = 'UMFTRN'
!-    GO TO 100
!
!-    ENTRY TAPWSI (I,J,K,L)
!-    NAME = 'TAPSWI'
!-    GO TO 100
!
!-    ENTRY SEARCH (I)
!-    NAME = 'SEARCH'
!-    GO TO 100
!
!-    ENTRY SOFIOI
!-    NAME = 'SOFIOI'
!-    GO TO 100
!
!-    ENTRY IQZDDN (I)
!-    NAME = 'IQZDDN'
!-    GO TO 100
!
!-    ENTRY QQDCBF (I,J,K,L,M,N)
!-    NAME = 'QQDCBF'
!-    GO TO 100
!
!-    ENTRY QQGETF (I,J,K)
!-    NAME = 'QQGETF'
!-    GO TO 100
!
! ****
!     ROUTINE USED ONLY BY IBM AND VAX
! ****
!
!-    ENTRY SOFIOF
!-    NAME = 'SOFIOF'
!-    GO TO 100
!
!     THE FOLLOWING THREE ARE FUNCTIONS FOR QUAD WORD OPERATIONS
!                                           (REAL*16)
!-    ENTRY QABS (I)
!-    NAME = 'QABS'
!-    GO TO 100
!
!-    ENTRY SNGLQ (I)
!-    NAME = 'SNGLQ'
!-    GO TO 100
!
!-    ENTRY DBLEQ (I)
!-    NAME = 'DBLEQ'
!-    GO TO 100
!
!-    ENTRY QSQRT (I)
!-    NAME = 'QSQRT'
!-    GO TO 100
!
!-    ENTRY QLOG (I)
!-    NAME = 'QLOG'
!-    GO TO 100
!
!-    ENTRY QEXTD (I)
!-    NAME = 'QEXTD'
!-    GO TO 100
!
! ****
!     ROUTINE USED BY UNIVAC AND VAX
! ****
!
!-    ENTRY DEFCOR
!-    NAME = 'DEFCOR'
!-    GO TO 100
!
! ****
!     ROUTINES USEDS BY ALL MACHINES, EXCEPT VAX
! ****
!
!     ENTRY GPERR (I,J)
!     NAME = 'GPERR'
!     GO TO 100
!
!     ENTRY PDUMP
!     NAME = 'PDUMP'
!     GO TO 250
!
!     ENTRY MPY1
!     NAME = 'MPY1'
!     GO TO 100
!
!     ENTRY MPY2NT
!     NAME = 'MPY2NT'
!     GO TO 100
!
!     ENTRY MPY2T
!     NAME = 'MPY2T'
!     GO TO 100
!
! ****
!     ROUTINES USED ONLY IN CDC MACHINE
! ****
!
!     ENTRY LINK (I,J,K)
!     NAME = 'LINK'
!     GO TO 100
!
!     ENTRY REMARK (I)
!     NAME = 'REMARK'
!     GO TO 100
!
!     ENTRY CDCBUG (I,J,K,L)
!     NAME = 'CDCBUG'
!     GO TO 100
!
!     ENTRY CDCOPN (I)
!     NAME = 'CDCOPN'
!     GO TO 100
!
!     ENTRY CDCCLS (I)
!     NAME = 'CDCCLS'
!     GO TO 100
!
!     ENTRY PF (I,J,K)
!     NAME = 'PF'
!     GO TO 100
!
!     ENTRY ISWAP (I)
!     NAME = 'ISWAP'
!     GO TO 100
!
!-    ENTRY CDCKSZ (I)
!-    ENCODE (20,30,A) I
!- 30 FORMAT ('OPEN CORE =',I7,2X)
!-    CALL REMARK (A)
!-    GO TO 250
!
! ****
!     ROUTINES USED ONLY IN VAX MACHINE
! ****
!
!-    ENTRY VAXEND
!-    NAME = 'VAXEND'
!-    GO TO 100
!
!-    ENTRY VAXERR (L)
!-    WRITE  (NOUT,50) L
!- 50 FORMAT (/,' *** GINO ERROR AT LOC',I5)
!-    GO TO 220
!
!-    ENTRY VAXSCH
!-    NAME = 'VAXSCH'
!-    GO TO 100
!
!-    ENTRY VAXBRK
!-    NAME = 'VAXBRK'
!-    GO TO 100
!
!-    ENTRY MPY1V (I,J,K)
!-    NAME = 'MPY1V'
!-    GO TO 100
!
!-    ENTRY MPY2NV (I,J,K)
!-    NAME = 'MPY2NV'
!-    GO TO 100
!
!-    ENTRY MPY2TV (I,J,K)
!-    NAME = 'MPY2TV'
!-    GO TO 100
!
! ****
!     ROUTINES THAT PERFORM NO PARTICULAR FUNCTIONS, BUT THEY
!     ARE STILL CALLED BY NASTRAN
! ****
!
!-    ENTRY UNLOAD (I)
!     CALLED BY INPTT1
!-    GO TO 250
!
! ****
!     THE FOLLOWING ROUTINES SEEM TO BE NO LONGER USED IN NASTRAN
! ****
!
!-    ENTRY JIDINT (I)
!-    NAME = 'JIDINT'
!-    GO TO 100
!
!-    ENTRY OPMESG
!-    NAME = 'OPMESG'
!-    GO TO 100
!
!     ENTRY PDUM1,PDUM2,...,PDUM9 HAD BEEN REPLACED BY PDUMI
!     ENTRY QDMM3, SQDM31, AND SQDM32 ARE NOW OBSOLETE
!
!-    ENTRY SEMTRN
!-    NAME = 'SEMTRN'
!-    GO TO 100
!
! ****
!     DUMMY ROUTINES REFERENCED ONLY IN LINK 2, ALL MACHINES
! ****
!
!-    ENTRY PDUMI (*,*,*,I,J,K,L,M,N,O)
!-    NAME = 'PDUMI'
!-    GO TO 100
!
! ****
!     DUMMY ROUTINES REFERENCED ONLY IN LINK 5, ALL MACHINES
! ****
!
!-    ENTRY PLBAR1 (I,J)
!-    NAME = 'PLBAR1'
!-    GO TO 100
!
!-    ENTRY PLOADX
!-    NAME = 'PLOADX'
!-    GO TO 100
!
!-    ENTRY ERRTRC (NAM)
!     ==================
!     ERROR TRACEBACK
!
!-    GO TO 220
!
!-100 WRITE  (NOUT,150) NAME
!-150 FORMAT ('0*** SYSTEM FATAL ERROR  ---  JOB TERMINATED',
!-   1        ' DUE TO CALL TO DUMMY SUBROUTINE.  ENTRY NAME IS ', A8)
!-    GO TO 220
!
! ****
!     TO FORCE A SYSTEM FATAL ERROR FOR TRACEBACK
! ****
!
!-220 WRITE  (NOUT,230)
!-230 FORMAT ('0*** ERROR TRACEBACK IN SYSTEM LOG FILE')
!-    I =-3
!-    READ (I) J,K,M,N,O
!-250 RETURN
!
!
!     SUBROUTINE DUMMY
!
! ****
!     VAX VERSION  (MODIFIED FOR DEC/ULTRIX)
!
!     THIS SUBROUTINE PROVIDES ENTRIES FOR THE DUMMY ROUTINES
!     USED BY OTHER COMPUTER MACHINES, AND ARE REFERENCED IN
!     VARIOUS NASTRAN LINKS
!
!     THIS SUBROUTINE INCLUDES ALSO SOME DUMMY ROUTINES NOT YET
!     WRITTEN
!
!     THIS ROUTINE SHOULD BE MOVED TO NASTRAN MACHINE-DEPENDENT
!     SECTION (MDS)
! ****
!
!
!
   IF ( Mach==6 ) RETURN
   WRITE (Nout,99001) Mach
99001 FORMAT (/,' MACH =',I7)
   name = 'DUMMY'
   CALL spag_block_1
   RETURN
!
! ****
!     ROUTINES USED ONLY IN UNIVAC MACHINE
! ****
!
   ENTRY zcorsz(I)
   name = 'ZCORSZ'
   CALL spag_block_1
   RETURN
!
   ENTRY mvbits(I1,I2,I3,I4,I5)
   name = 'MVBITS'
   CALL spag_block_1
   RETURN
!
   ENTRY codkey(Code,Key)
   name = 'CODKEY'
   CALL spag_block_1
   RETURN
!
   ENTRY kconeq
   name = 'KCONEQ'
   CALL spag_block_1
   RETURN
!
!hgs      ENTRY FNXTVQ (V1,V2,V3,V4,V5,ZB,I)
!hgs      NAME = 'FNXTVQ'
!hgs      GO TO 100
!
   ENTRY ntran(I,J,K)
   name = 'NTRAN'
   CALL spag_block_1
   RETURN
!
   ENTRY contin
   name = 'CONTIN'
   CALL spag_block_1
   RETURN
!
   ENTRY facil(I,J)
   name = 'FACIL'
   CALL spag_block_1
   RETURN
!
   ENTRY facsf(I)
   name = 'FACSF'
   CALL spag_block_1
   RETURN
!
   ENTRY unvopn(I)
   name = 'UNVOPN'
   CALL spag_block_1
   RETURN
!
   ENTRY unvcls(I)
   name = 'UNVCLS'
   CALL spag_block_1
   RETURN
!
   ENTRY addcrd(I,J)
   name = 'ADDCRD'
   CALL spag_block_1
   RETURN
!
! ****
!     ROUTINES USED BY UNIVAC AND IBM
! ****
!
   ENTRY return
   RETURN
!
   ENTRY msguni
   IF ( Mach==2 ) RETURN
   name = 'MSGUNI'
   CALL spag_block_1
   RETURN
!
   ENTRY xeot(I,J,K,L)
   IF ( Mach==2 ) RETURN
   name = 'XEOT'
   CALL spag_block_1
   RETURN
!
   ENTRY tpswit(I,J,K,L)
   name = 'TPSWIT'
   CALL spag_block_1
   RETURN
!
! ****
!     ROUTINES USED ONLY IN IBM MACHINE
! ****
!
   ENTRY umftrn(I)
   name = 'UMFTRN'
   CALL spag_block_1
   RETURN
!
   ENTRY tapswi(I,J,K,L)
   name = 'TAPSWI'
   CALL spag_block_1
   RETURN
!
   ENTRY search(I)
   name = 'SEARCH'
   CALL spag_block_1
   RETURN
!
   ENTRY sofioi
   name = 'SOFIOI'
   CALL spag_block_1
   RETURN
!
   ENTRY iqzddn(I)
   name = 'IQZDDN'
   CALL spag_block_1
   RETURN
!
   ENTRY qqdcbf(I,J,K,L,M,N)
   name = 'QQDCBF'
   CALL spag_block_1
   RETURN
!
   ENTRY qqgetf(I,J,K)
   name = 'QQGETF'
   CALL spag_block_1
   RETURN
!
! ****
!     ROUTINE USED ONLY BY IBM AND VAX
! ****
!
!     ENTRY SOFIOF
!     NAME = 'SOFIOF'
!     GO TO 100
!
!     THE FOLLOWING THREE ARE FUNCTIONS FOR QUAD WORD OPERATIONS
!                                           (REAL*16)
   ENTRY qabs(I)
   name = 'QABS'
   CALL spag_block_1
   RETURN
!
   ENTRY snglq(I)
   name = 'SNGLQ'
   CALL spag_block_1
   RETURN
!
   ENTRY dbleq(I)
   name = 'DBLEQ'
   CALL spag_block_1
   RETURN
!
   ENTRY qsqrt(I)
   name = 'QSQRT'
   CALL spag_block_1
   RETURN
!
   ENTRY qlog(I)
   name = 'QLOG'
   CALL spag_block_1
   RETURN
!
   ENTRY qextd(I)
   name = 'QEXTD'
   CALL spag_block_1
   RETURN
!
! ****
!     ROUTINE USED BY UNIVAC AND VAX
! ****
!
!     ENTRY DEFCOR
!     NAME = 'DEFCOR'
!     GO TO 100
!
! ****
!     ROUTINES USED BY ALL MACHINES, EXCEPT VAX
! ****
!
   ENTRY gperr(I,J)
   name = 'GPERR'
   CALL spag_block_1
   RETURN
!
   ENTRY pdump
   RETURN
!
   ENTRY mpy1
   name = 'MPY1'
   CALL spag_block_1
   RETURN
!
   ENTRY mpy2nt
   name = 'MPY2NT'
   CALL spag_block_1
   RETURN
!
   ENTRY mpy2t
   name = 'MPY2T'
   CALL spag_block_1
   RETURN
!
! ****
!     ROUTINES USED ONLY IN CDC MACHINE
! ****
!
   ENTRY link(I,J,K)
   name = 'LINK'
   CALL spag_block_1
   RETURN
!
   ENTRY remark(I)
   name = 'REMARK'
   CALL spag_block_1
   RETURN
!
   ENTRY cdcbug(I,J,K,L)
   name = 'CDCBUG'
   CALL spag_block_1
   RETURN
!
   ENTRY cdcopn(I)
   name = 'CDCOPN'
   CALL spag_block_1
   RETURN
!
   ENTRY cdccls(I)
   name = 'CDCCLS'
   CALL spag_block_1
   RETURN
!
   ENTRY cdcksz(I)
   name = 'CDCKSZ'
   CALL spag_block_1
   RETURN
!
   ENTRY pf(I,J,K)
   name = 'PF'
   CALL spag_block_1
   RETURN
!
   ENTRY iswap(I)
   name = 'ISWAP'
   CALL spag_block_1
   RETURN
!
! ****
!     ROUTINES USED ONLY IN VAX MACHINE
! ****
!
   ENTRY vaxend
   name = 'VAXEND'
   CALL spag_block_1
   RETURN
!
   ENTRY vaxerr(L)
   WRITE (Nout,99002) L
99002 FORMAT (/,' *** GINO ERROR AT LOC',I5)
   CALL spag_block_2
   RETURN
!
!     ENTRY VAXSCH
!     NAME = 'VAXSCH'
!     GO TO 100
!
   ENTRY vaxbrk
   name = 'VAXBRK'
   CALL spag_block_1
   RETURN
!
!     ENTRY MPY1V (I,J,K)
!     NAME = 'MPY1V'
!     GO TO 100
!
!     ENTRY MPY2NV (I,J,K)
!     NAME = 'MPY2NV'
!     GO TO 100
!
!     ENTRY MPY2TV (I,J,K)
!     NAME = 'MPY2TV'
!     GO TO 100
!
! ****
!     ROUTINES THAT PERFORM NO PARTICULAR FUNCTIONS, BUT THEY
!     ARE STILL CALLED BY NASTRAN
! ****
!
   ENTRY unload(I)
!     CALLED BY INPTT1
   RETURN
!
! ****
!     THE FOLLOWING ROUTINES SEEM TO BE NO LONGER USED IN NASTRAN
! ****
!
   ENTRY jidint(I)
   name = 'JIDINT'
   CALL spag_block_1
   RETURN
!
   ENTRY opmesg
   name = 'OPMESG'
   CALL spag_block_1
   RETURN
!
!     ENTRY PDUM1,PDUM2,...,PDUM9 HAD BEEN REPLACED BY PDUMI
!     ENTRY QDMM3, SQDM31, AND SQDM32 ARE NOW OBSOLETE
!
   ENTRY semtrn
   name = 'SEMTRN'
   CALL spag_block_1
   RETURN
!
! ****
!     DUMMY ROUTINES REFERENCED ONLY IN LINK 2, ALL MACHINES
! ****
!
   ENTRY pdumi(*,*,*,I,J,K,L,M,N,O)
   name = 'PDUMI'
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      USE C_MACHIN
      USE C_SYSTEM
!
! ****
!     DUMMY ROUTINES REFERENCED ONLY IN LINK 5, ALL MACHINES
! ****
!
!HGS      ENTRY PLBAR1 (I,J)
!HGS      NAME = 'PLBAR1'
!HGS      GO TO 100
!
!HGS      ENTRY PLOADX
!HGS      NAME = 'PLOADX'
!HGS      GO TO 100
!
!WKBD ENTRY ERRTRC (NAM)
!     ==================
!     ERROR TRACEBACK
!
!WKBD GO TO 220
!
      WRITE (Nout,99001) Name
99001 FORMAT ('0*** SYSTEM FATAL ERROR  ---  JOB TERMINATED',' DUE TO CALL TO DUMMY SUBROUTINE.  ENTRY NAME IS ',A8)
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      USE C_MACHIN
      USE C_SYSTEM
!
! ****
!     TO FORCE A SYSTEM FATAL ERROR FOR TRACEBACK (VAX ONLY, NOT UNIX)
! ****
!
      IF ( Mach==5 ) THEN
         WRITE (Nout,99001)
99001    FORMAT ('0*** ERROR TRACEBACK IN SYSTEM LOG FILE')
         I = 987654321
         N(I) = 0
      ENDIF
      STOP
   END SUBROUTINE spag_block_2
!
!
!     SUBROUTINE DUMMY
!
! ****
!     UNIVAC  VERSION
!
!     THIS SUBROUTINE PROVIDES ENTRIES FOR THE DUMMY ROUTINES
!     USED BY OTHER COMPUTER MACHINES, AND ARE REFERENCED IN
!     VARIOUS NASTRAN LINKS
!
!     THIS SUBROUTINE INCLUDES ALSO SOME DUMMY ROUTINES NOT YET
!     WRITTEN
!
!     THIS ROUTINE SHOULD BE MOVED TO NASTRAN MACHINE-DEPENDENT
!     SECTION (MDS)
! ****
!
!*    CHARACTER*8     NAME
!
!*    COMMON /MACHIN/ MACH
!*    COMMON /SYSTEM/ ISYSBF, NOUT
!
!*    IF (MACH .EQ. 3) GO TO 250
!*    WRITE  (NOUT,20) MACH
!* 20 FORMAT (/,' MACH =',I7)
!*    NAME = 'DUMMY'
!*    GO TO 100
!
! ****
!     ROUTINES USED ONLY IN UNIVAC MACHINE
! ****
!
!     ENTRY NTRAN (I,J,K)
!     NAME = 'NTRAN'
!     GO TO 100
!
!     ENTRY CONTIN
!     NAME = 'CONTIN'
!     GO TO 100
!
!     ENTRY FACIL (I,J)
!     NAME = 'FACIL'
!     GO TO 100
!
!     ENTRY FACSF (I)
!     NAME = 'FACSF'
!     GO TO 100
!
!     ENTRY UNVOPN (I)
!     NAME = 'UNVOPN'
!     GO TO 100
!
!     ENTRY UNVCLS (I)
!     NAME = 'UNVCLS'
!     GO TO 100
!
!     ENTRY ADDCRD (I,J)
!     NAME = 'ADDCRD'
!     GO TO 100
!
! ****
!     ROUTINES USED BY UNIVAC AND IBM
! ****
!
!*    ENTRY RETURN
!*    GO TO 250
!
!     ENTRY MSGUNI
!     IF (MACH .EQ. 2) GO TO 250
!     NAME = 'MSGUNI'
!     GO TO 100
!
!     ENTRY XEOT (I,J,K,L)
!     IF (MACH .EQ. 2) GO TO 250
!     NAME = 'XEOT'
!     GO TO 100
!
!     ENTRY TPSWIT (I,J,K,L)
!     NAME = 'TPSWIT'
!     GO TO 100
!
! ****
!     ROUTINES USED ONLY IN IBM MACHINE
! ****
!
!*    ENTRY UMFTRN (I)
!*    NAME = 'UMFTRN'
!*    GO TO 100
!
!*    ENTRY TAPSWI (I,J,K,L)
!*    NAME = 'TAPWSI'
!*    GO TO 100
!
!*    ENTRY SEARCH (I)
!*    NAME = 'SEARCH'
!*    GO TO 100
!
!*    ENTRY SOFIOI
!*    NAME = 'SOFIOI'
!*    GO TO 100
!
!*    ENTRY IQZDDN (I)
!*    NAME = 'IQZDDN'
!*    GO TO 100
!
!*    ENTRY QQDCBF (I,J,K,L,M,N)
!*    NAME = 'QQDCBF'
!*    GO TO 100
!
!*    ENTRY QQGETF (I,J,K)
!*    NAME = 'QQGETF'
!*    GO TO 100
!
! ****
!     ROUTINE USED ONLY BY IBM AND VAX
! ****
!
!*    ENTRY SOFIOF
!*    NAME = 'SOFIOF'
!*    GO TO 100
!
!     THE FOLLOWING THREE ARE FUNCTIONS FOR QUAD WORD OPERATIONS
!                                           (REAL*16)
!*    ENTRY QABS (I)
!*    NAME = 'QABS'
!*    GO TO 100
!
!*    ENTRY SNGLQ (I)
!*    NAME = 'SNGLQ'
!*    GO TO 100
!
!*    ENTRY DBLEQ (I)
!*    NAME = 'DBLEQ'
!*    GO TO 100
!
!*    ENTRY QSQRT (I)
!*    NAME = 'QSQRT'
!*    GO TO 100
!
!*    ENTRY QLOG (I)
!*    NAME = 'QLOG'
!*    GO TO 100
!
!*    ENTRY QEXTD (I)
!*    NAME = 'QEXTD'
!*    GO TO 100
!
! ****
!     ROUTINE USED BY UNIVAC AND VAX
! ****
!
!     ENTRY DEFCOR
!     NAME = 'DEFCOR'
!     GO TO 100
!
! ****
!     ROUTINES USED BY ALL MACHINES, EXCEPT VAX
! ****
!
!     ENTRY GPERR (I,J)
!     NAME = 'GPERR'
!     GO TO 100
!
!     ENTRY PDUMP
!     GO TO 250
!
!     ENTRY MPY1
!     NAME = 'MPY1'
!     GO TO 100
!
!     ENTRY MPY2NT
!     NAME = 'MPY2NT'
!     GO TO 100
!
!     ENTRY MPY2T
!     NAME = 'MPY2T'
!     GO TO 100
!
! ****
!     ROUTINES USED ONLY IN CDC MACHINE
! ****
!
!*    ENTRY LINK (I,J,K)
!*    NAME = 'LINK'
!*    GO TO 100
!
!*    ENTRY REMARK (I)
!*    NAME = 'REMARK'
!*    GO TO 100
!
!*    ENTRY CDCBUG (I,J,K,L)
!*    NAME = 'CDCBUG'
!*    GO TO 100
!
!*    ENTRY CDCOPN (I)
!*    NAME = 'CDCOPN'
!*    GO TO 100
!
!*    ENTRY CDCCLS (I)
!*    NAME = 'CDCCLS'
!*    GO TO 100
!
!*    ENTRY CDCKSZ (I)
!*    NAME = 'CDCKSZ'
!*    GO TO 100
!
!*    ENTRY PF (I,J,K)
!*    NAME = 'PF'
!*    GO TO 100
!
!*    ENTRY ISWAP (I)
!*    NAME = 'ISWAP'
!*    GO TO 100
!
! ****
!     ROUTINES USED ONLY IN VAX MACHINE
! ****
!
!*    ENTRY VAXEND
!*    NAME = 'VAXEND'
!*    GO TO 100
!
!*    ENTRY VAXERR (L)
!*    WRITE  (NOUT,50) L
!* 50 FORMAT (/,' *** GINO ERROR AT LOC',I5)
!*    GO TO 220
!
!*    ENTRY VAXSCH
!*    NAME = 'VAXSCH'
!*    GO TO 100
!
!*    ENTRY VAXBRK
!*    NAME = 'VAXBRK'
!*    GO TO 100
!
!*    ENTRY MPY1V (I,J,K)
!*    NAME = 'MPY1V'
!*    GO TO 100
!
!*    ENTRY MPY2NV (I,J,K)
!*    NAME = 'MPY2NV'
!*    GO TO 100
!
!*    ENTRY MPY2TV (I,J,K)
!*    NAME = 'MPY2TV'
!*    GO TO 100
!
! ****
!     ROUTINES THAT PERFORM NO PARTICULAR FUNCTIONS, BUT THEY
!     ARE STILL CALLED BY NASTRAN
! ****
!
!*    ENTRY UNLOAD (I)
!     CALLED BY INPTT1
!*    GO TO 250
!
! ****
!     THE FOLLOWING ROUTINES SEEM TO BE NO LONGER USED IN NASTRAN
! ****
!
!*    ENTRY JIDINT (I)
!*    NAME = 'JIDINT'
!*    GO TO 100
!
!*    ENTRY OPMESG
!*    NAME = 'OPMESG'
!*    GO TO 100
!
!     ENTRY PDUM1,PDUM2,...,PDUM9 HAD BEEN REPLACED BY PDUMI
!     ENTRY QDMM3, SQDM31, AND SQDM32 ARE NOW OBSOLETE
!
!*    ENTRY SEMTRN
!*    NAME = 'SEMTRN'
!*    GO TO 100
!
! ****
!     DUMMY ROUTINES REFERENCED ONLY IN LINK 2, ALL MACHINES
! ****
!
!*    ENTRY PDUMI (*,*,*,I,J,K,L,M,N,O)
!*    NAME = 'PDUMI'
!*    GO TO 100
!
! ****
!     DUMMY ROUTINES REFERENCED ONLY IN LINK 5, ALL MACHINES
! ****
!
!*    ENTRY PLBAR1 (I,J)
!*    NAME = 'PLBAR1'
!*    GO TO 100
!
!*    ENTRY PLOADX
!*    NAME = 'PLOADX'
!*    GO TO 100
!
!*    ENTRY ERRTRC (NAM)
!     ==================
!     ERROR TRACEBACK
!
!*    GO TO 220
!
!*100 WRITE  (NOUT,150) NAME
!*150 FORMAT ('0*** SYSTEM FATAL ERROR  ---  JOB TERMINATED',
!*   1        ' DUE TO CALL TO DUMMY SUBROUTINE.  ENTRY NAME IS ', A8)
!*    GO TO 220
!
! ****
!     TO FORCE A SYSTEM FATAL ERROR FOR TRACEBACK
! ****
!
!*220 WRITE  (NOUT,230)
!*230 FORMAT ('0*** ERROR TRACEBACK IN SYSTEM LOG FILE')
!*    X =-1.0
!*    X = SQRT(X)
!*250 RETURN
!
!
!     SUBROUTINE DUMMY
!
! ****
!     MACHINES 1, AND 6 THRU 20 VERSION
!
!     THIS SUBROUTINE PROVIDES ENTRIES FOR THE DUMMY ROUTINES
!     USED BY OTHER COMPUTER MACHINES, AND ARE REFERENCED IN
!     VARIOUS NASTRAN LINKS
!
!     THIS SUBROUTINE INCLUDES ALSO SOME DUMMY ROUTINES NOT YET
!     WRITTEN
!
!     THIS ROUTINE SHOULD BE MOVED TO NASTRAN MACHINE-DEPENDENT
!     SECTION (MDS)
! ****
!
!.    DIMENSION       N(1)
!.    CHARACTER*8     NAME
!
!.    COMMON /MACHIN/ MACH
!.    COMMON /SYSTEM/ ISYSBF, NOUT
!
!.    IF (MACH.EQ.1 .AND. MACH.GE.6) GO TO RETURN
!.    WRITE  (NOUT,150) NAME,MACH
!.150 FORMAT ('0*** SYSTEM FATAL ERROR  ---  JOB TERMINATED', /5X,
!.   1       'SUBROUTINE DUMMY FOR MACHINE TYPE',I4,' IS NOT AVAILABLE')
!.    I = 987654321
!.    N(I) = 0
!.    STOP
!
END SUBROUTINE dummy
