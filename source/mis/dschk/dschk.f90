!*==dschk.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dschk
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL :: epsi , value , xlama
   INTEGER :: file , ibuf1 , iexit , ifrst , iretn , nf , ni , ti , tleft , tnow , to
   INTEGER , DIMENSION(1) :: iz
   INTEGER , SAVE :: pgi , pgip1 , scr1 , scr2 , scr3 , ugip1
   EXTERNAL close , gopen , klock , korsz , page2 , ssg2b , tmtogo , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     MODULE TO PERFORM DIFFERENTIAL STIFFNESS CONVERGENCE TESTS
!
!     DSCHK    PGI,PGIP1,UGIP1//EPSIO,DSEPSI,NT,TOUT,TIN,DONE,SHIFT,
!                               COUNT,BETA
!
!     EPSIO    ACCEPTABLE RATIO OF ENERGY ERROR TO TOTAL ERROR(R) INPUT
!     DSEPSI   EPSI(SUB I -1)   (REAL)                            IN/OUT
!     NT       TOTAL NUMBER OF ITERATIONS ALLOWED                 INPUT
!     TOUT     START TIME FOR OUTER LOOP                          INPUT
!     TIN      START TIME FOR INNER LOOP                          INPUT
!     DONE     EXIT FLAG FOR SKIP TO SDR2                         OUTPUT
!     SHIFT    EXIT FLAG FOR SHIFT                                IN/OUT
!     COUNT    CURRENT STEP NUMBER                                IN/OUT
!     BETA     SHIFT DECISION FACTOR (REAL)                       INPUT
!
!     EXIT FLAG VALUES (IEXIT)                                    LOCAL
!          0   NOT SET
!          1   CONVERGED
!          2   DIVERGED
!          3   INSUFFICIENT TIME
!          4   ITERATION LIMIT
!          5   ZERO EPSIO
!          6   ZERO EPSI
!
   !>>>>EQUIVALENCE (Count,Ni) , (Z(1),Iz(1))
   DATA pgi , pgip1 , ugip1 , scr1 , scr2 , scr3/101 , 102 , 103 , 301 , 302 , 303/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         ibuf1 = korsz(iz) - Sysbuf + 1
         iexit = 0
         ifrst = Shift
         Shift = 1
         nf = 1
         CALL klock(tnow)
         ti = tnow - Tin
         CALL tmtogo(tleft)
         to = tnow - tleft
!
!     COMPUTE DSEPSI(I)
!
         CALL ssg2b(ugip1,pgi,0,scr1,1,Iprec,1,scr3)
         CALL ssg2b(ugip1,pgip1,scr1,scr2,1,Iprec,2,scr3)
!
         Ii = 1
         Jj = 1
         Incr = 1
         Ita = 1
         file = scr2
         ASSIGN 20 TO iretn
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     GET DENOMINATOR
!
 20      epsi = value
         file = scr1
         ASSIGN 40 TO iretn
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 40      IF ( value/=0.0 ) THEN
            epsi = abs(epsi/value)
            Count = Count + 1
            IF ( ifrst/=-1 ) THEN
               IF ( epsi==0.0 ) THEN
!
!     AFTER SSG2B, EPSI IS ZERO DUE TO THE FIRST VAULE FROM SCR2 IS ZERO
!     WHILE VALUE FROM SCR1 IS NOT ZERO
!
                  iexit = 6
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  xlama = abs(Dsepsi/epsi)
                  IF ( xlama<=1.0 ) THEN
!
!     DIVERGED
!
                     iexit = 2
                     Done = -1
                     Dsepsi = epsi
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDIF
            Dsepsi = epsi
            IF ( epsi>Epsio ) THEN
!
!     MAKE FIRST TEST
!
               IF ( ifrst==-1 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     NOT FIRST TIME
!
               IF ( Epsio<=0.0 ) THEN
!
!     PARAMETER ERROR, EPSIO HAS NO VALUE
!
                  iexit = 5
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  nf = alog(epsi/Epsio)/alog(xlama)
                  CALL klock(tnow)
                  CALL tmtogo(tleft)
                  ti = tnow - Tin
                  to = tnow - Tout
!
!     CONVERGENT
!
                  IF ( nf>Nt-ni ) THEN
                     IF ( Nt-ni<Beta ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     IF ( ti*nf>to+Beta*ti ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
!
!     CONVERGED
!
         iexit = 1
         Done = -1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         IF ( tleft<3*ti ) THEN
!
!     INSUFFICIENT TIME
!
            iexit = 3
            Done = -1
!
!     WRAP UP FOR NO SHIFT
!
         ELSEIF ( ni>=Nt ) THEN
!
!     USER LIMIT ITERATION NUMBER EXPIRED
!
            iexit = 4
            Done = -1
         ELSE
            Shift = 1
            Done = nf
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
!     SET SHIFT FLAG
!
         Shift = -1
         IF ( tleft<to+Beta*ti ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     WRAP UP FOR SHIFT
!
         Done = nf
         iexit = 0
         spag_nextblock_1 = 4
      CASE (4)
!
!     EXIT FROM MODULE
!
         CALL page2(-9)
         WRITE (Nout,99001) Uim , iexit , Count , Done , Shift , Dsepsi
99001    FORMAT (A29,' 7019, MODULE DSCHK IS EXITING FOR REASON',I4,/5X,'ON ITERATION NUMBER',I7,1H.,/5X,                           &
                &'PARAMETER VALUES ARE AS FOLLOWS',/10X,'DONE   =',I10,/10X,'SHIFT  =',I10,/10X,'DSEPSI =',1P,E14.7)
         IF ( iexit>=5 ) WRITE (Nout,99002) Epsio , epsi
99002    FORMAT (10X,'EPSIO  =',1P,E10.3,/10X,'EPSI   =',1P,E10.3)
         RETURN
      CASE (5)
!
!     INTERNAL ROUTINE TO OBTAIN VALUE FROM MATRIX
!
         CALL gopen(file,iz(ibuf1),0)
         CALL unpack(*60,file,value)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 60      value = 0.0
         spag_nextblock_1 = 6
      CASE (6)
         CALL close(file,1)
         GOTO iretn
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dschk
