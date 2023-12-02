!*==dschk.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dschk
   USE c_blank
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
         ibuf1 = korsz(iz) - sysbuf + 1
         iexit = 0
         ifrst = shift
         shift = 1
         nf = 1
         CALL klock(tnow)
         ti = tnow - tin
         CALL tmtogo(tleft)
         to = tnow - tleft
!
!     COMPUTE DSEPSI(I)
!
         CALL ssg2b(ugip1,pgi,0,scr1,1,iprec,1,scr3)
         CALL ssg2b(ugip1,pgip1,scr1,scr2,1,iprec,2,scr3)
!
         ii = 1
         jj = 1
         incr = 1
         ita = 1
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
            count = count + 1
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
                  xlama = abs(dsepsi/epsi)
                  IF ( xlama<=1.0 ) THEN
!
!     DIVERGED
!
                     iexit = 2
                     done = -1
                     dsepsi = epsi
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDIF
            dsepsi = epsi
            IF ( epsi>epsio ) THEN
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
               IF ( epsio<=0.0 ) THEN
!
!     PARAMETER ERROR, EPSIO HAS NO VALUE
!
                  iexit = 5
                  spag_nextblock_1 = 4
               ELSE
                  nf = alog(epsi/epsio)/alog(xlama)
                  CALL klock(tnow)
                  CALL tmtogo(tleft)
                  ti = tnow - tin
                  to = tnow - tout
!
!     CONVERGENT
!
                  IF ( nf>nt-ni ) THEN
                     IF ( nt-ni<beta ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 3
                  ELSE
                     IF ( ti*nf>to+beta*ti ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 2
                  ENDIF
               ENDIF
               CYCLE
            ENDIF
         ENDIF
!
!     CONVERGED
!
         iexit = 1
         done = -1
         spag_nextblock_1 = 4
      CASE (2)
         IF ( tleft<3*ti ) THEN
!
!     INSUFFICIENT TIME
!
            iexit = 3
            done = -1
!
!     WRAP UP FOR NO SHIFT
!
         ELSEIF ( ni>=nt ) THEN
!
!     USER LIMIT ITERATION NUMBER EXPIRED
!
            iexit = 4
            done = -1
         ELSE
            shift = 1
            done = nf
         ENDIF
         spag_nextblock_1 = 4
      CASE (3)
!
!     SET SHIFT FLAG
!
         shift = -1
         IF ( tleft<to+beta*ti ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     WRAP UP FOR SHIFT
!
         done = nf
         iexit = 0
         spag_nextblock_1 = 4
      CASE (4)
!
!     EXIT FROM MODULE
!
         CALL page2(-9)
         WRITE (nout,99001) uim , iexit , count , done , shift , dsepsi
99001    FORMAT (A29,' 7019, MODULE DSCHK IS EXITING FOR REASON',I4,/5X,'ON ITERATION NUMBER',I7,1H.,/5X,                           &
                &'PARAMETER VALUES ARE AS FOLLOWS',/10X,'DONE   =',I10,/10X,'SHIFT  =',I10,/10X,'DSEPSI =',1P,E14.7)
         IF ( iexit>=5 ) WRITE (nout,99002) epsio , epsi
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
