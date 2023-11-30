
SUBROUTINE dschk
   IMPLICIT NONE
   INTEGER Beta , Count , Done , Ii , Incr , Iprec , Ita , Iz(1) , Jj , Ksystm(52) , Ni , Nout , Nt , Shift , Sysbuf , Tin , Tout
   REAL Dsepsi , Epsio , Z(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Epsio , Dsepsi , Nt , Tout , Tin , Done , Shift , Count , Beta
   COMMON /system/ Sysbuf , Nout , Ksystm , Iprec
   COMMON /unpakx/ Ita , Ii , Jj , Incr
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   REAL epsi , value , xlama
   INTEGER file , ibuf1 , iexit , ifrst , iretn , nf , pgi , pgip1 , scr1 , scr2 , scr3 , ti , tleft , tnow , to , ugip1
   INTEGER korsz
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
   EQUIVALENCE (Count,Ni) , (Z(1),Iz(1))
   DATA pgi , pgip1 , ugip1 , scr1 , scr2 , scr3/101 , 102 , 103 , 301 , 302 , 303/
!
!     INITIALIZE
!
   ibuf1 = korsz(Iz) - Sysbuf + 1
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
   ASSIGN 100 TO iretn
   GOTO 600
!
!     GET DENOMINATOR
!
 100  epsi = value
   file = scr1
   ASSIGN 200 TO iretn
   GOTO 600
 200  IF ( value/=0.0 ) THEN
      epsi = abs(epsi/value)
      Count = Count + 1
      IF ( ifrst/=-1 ) THEN
         IF ( epsi==0.0 ) THEN
!
!     AFTER SSG2B, EPSI IS ZERO DUE TO THE FIRST VAULE FROM SCR2 IS ZERO
!     WHILE VALUE FROM SCR1 IS NOT ZERO
!
            iexit = 6
            GOTO 500
         ELSE
            xlama = abs(Dsepsi/epsi)
            IF ( xlama<=1.0 ) THEN
!
!     DIVERGED
!
               iexit = 2
               Done = -1
               Dsepsi = epsi
               GOTO 500
            ENDIF
         ENDIF
      ENDIF
      Dsepsi = epsi
      IF ( epsi>Epsio ) THEN
!
!     MAKE FIRST TEST
!
         IF ( ifrst==-1 ) GOTO 300
!
!     NOT FIRST TIME
!
         IF ( Epsio<=0.0 ) THEN
!
!     PARAMETER ERROR, EPSIO HAS NO VALUE
!
            iexit = 5
            GOTO 500
         ELSE
            nf = alog(epsi/Epsio)/alog(xlama)
            CALL klock(tnow)
            CALL tmtogo(tleft)
            ti = tnow - Tin
            to = tnow - Tout
!
!     CONVERGENT
!
            IF ( nf>Nt-Ni ) THEN
               IF ( Nt-Ni>=Beta ) GOTO 400
               GOTO 300
            ELSE
               IF ( ti*nf<=to+Beta*ti ) GOTO 300
               GOTO 400
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
!     CONVERGED
!
   iexit = 1
   Done = -1
   GOTO 500
 300  IF ( tleft<3*ti ) THEN
!
!     INSUFFICIENT TIME
!
      iexit = 3
      Done = -1
!
!     WRAP UP FOR NO SHIFT
!
   ELSEIF ( Ni>=Nt ) THEN
!
!     USER LIMIT ITERATION NUMBER EXPIRED
!
      iexit = 4
      Done = -1
   ELSE
      Shift = 1
      Done = nf
   ENDIF
   GOTO 500
!
!     SET SHIFT FLAG
!
 400  Shift = -1
   IF ( tleft<to+Beta*ti ) GOTO 300
!
!     WRAP UP FOR SHIFT
!
   Done = nf
   iexit = 0
!
!     EXIT FROM MODULE
!
 500  CALL page2(-9)
   WRITE (Nout,99001) Uim , iexit , Count , Done , Shift , Dsepsi
99001 FORMAT (A29,' 7019, MODULE DSCHK IS EXITING FOR REASON',I4,/5X,'ON ITERATION NUMBER',I7,1H.,/5X,                              &
             &'PARAMETER VALUES ARE AS FOLLOWS',/10X,'DONE   =',I10,/10X,'SHIFT  =',I10,/10X,'DSEPSI =',1P,E14.7)
   IF ( iexit>=5 ) WRITE (Nout,99002) Epsio , epsi
99002 FORMAT (10X,'EPSIO  =',1P,E10.3,/10X,'EPSI   =',1P,E10.3)
   RETURN
!
!     INTERNAL ROUTINE TO OBTAIN VALUE FROM MATRIX
!
 600  CALL gopen(file,Iz(ibuf1),0)
   CALL unpack(*700,file,value)
   GOTO 800
 700  value = 0.0
 800  CALL close(file,1)
   GOTO iretn
END SUBROUTINE dschk
