!*==shctsd.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE shctsd(Ierr,Elid,Pid,Mid,Tlam,Tmean,Tgrad,Thetae,Ftherm,Epslnt,Icore,Core)
USE C_CONDAD
USE C_MATIN
USE C_SDR2C1
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ierr
   INTEGER :: Elid
   INTEGER :: Pid
   INTEGER , DIMENSION(4) :: Mid
   REAL(REAL64) :: Tlam
   REAL(REAL64) :: Tmean
   REAL(REAL64) :: Tgrad
   REAL(REAL64) :: Thetae
   REAL(REAL64) , DIMENSION(6) :: Ftherm
   REAL(REAL64) , DIMENSION(6) :: Epslnt
   INTEGER , DIMENSION(1) :: Icore
   REAL , DIMENSION(1) :: Core
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(6,6) :: abbd
   REAL(REAL64) , DIMENSION(3) :: alphae , alphal , galpha
   REAL(REAL64) :: c , c2 , delta , deltat , determ , minrt , s , s2 , theta , tsubo , zk , zk1 , zref , zsubi
   REAL(REAL64) , DIMENSION(6) :: dum
   REAL(REAL64) , DIMENSION(9) :: gbar , glay , glayt , transl
   REAL(REAL64) , DIMENSION(25) :: gprop
   INTEGER :: ii , ip , ipc11 , ipc21 , ipoint , ir , ising , itype , k , lamopt , ll , lpc11 , lpcomp , mm , nlay , nn , pidloc
   INTEGER , DIMENSION(6,3) :: indx
   INTEGER , SAVE :: mem , pcomp , pcomp1 , pcomp2 , sym , symmem
   LOGICAL :: nonmem , pcmp , pcmp1 , pcmp2
   REAL(REAL64) , DIMENSION(36) :: stiff
   REAL :: ti
   EXTERNAL gmmatd , inverd , lpropd , mat
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     DOUBLE PRECISION ROUTINE TO EVALUATE THERMAL STRAINS FOR COMPOSITE
!     SHELL ELEMENTS.
!
!     INPUT :
!           ELID   - ELEMENT ID
!           PID    - PROPERTY ID
!           MID    - ARRAY OF LAMINATE MATERIAL ID'S
!           TLAM   - LAMINATE THICKNESS
!           TMEAN  - ELEMENT MEAN TEMPERATURE
!           TGRAD  - THERMAL GRADIENT
!           THETAE - ANGLE FROM ELEMENT X-AXIS TO MATERIAL X-AXIS
!           FTHERM - ARRAY OF THERMAL FORCES CONTAINING THE USER-
!                    DEFINED THERMAL MOMENTS, IF SUPPLIED
!           IPCMPI AND NPCMPI ARE THE STARTING POINT AND THE NUMBER
!           OF WORDS OF PCOMPI DATA IN CORE, AS INPUT BY /SDR2C1/.
!     OUTPUT:
!           EPSLNT - ARRAY OF THERMAL STRAINS FOR THE LAMINATE
!
!
!
!
   DATA pcomp , pcomp1 , pcomp2/0 , 1 , 2/
   DATA sym , mem , symmem/1 , 2 , 3/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         Ierr = 0
         DO ll = 1 , 6
            DO mm = 1 , 6
               abbd(ll,mm) = 0.0D0
            ENDDO
         ENDDO
!
         minrt = Tlam*Tlam*Tlam/12.0D0
         zref = -Tlam/2.0D0
!
         Inflag = 12
         Eltemp = Tmean
!
         itype = -1
         lpcomp = Ipcmp + Npcmp + Npcmp1 + Npcmp2
         pcmp = Npcmp>0
         pcmp1 = Npcmp1>0
         pcmp2 = Npcmp2>0
!
!     ISSUE ERROR IF PCOMPI DATA HAS NOT BEEN READ INTO CORE
!
         IF ( lpcomp==Ipcmp ) THEN
!
            Ierr = 1
            RETURN
         ELSE
!
!     LOCATE PID BY PERFORMING A SEQUENTIAL SEARCH OF THE PCOMPI DATA
!     BLOCK WHICH IS IN CORE.
!
!     SEARCH FOR PID IN PCOMP DATA
!
            IF ( pcmp ) THEN
               ip = Ipcmp
               IF ( Icore(ip)==Pid ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ipc11 = Ipcmp1 - 1
               DO ip = Ipcmp , ipc11
                  IF ( Icore(ip)==-1 .AND. ip<ipc11 ) THEN
                     IF ( Icore(ip+1)==Pid ) GOTO 10
                  ENDIF
               ENDDO
            ENDIF
!
!     SEARCH FOR PID IN PCOMP1 DATA
!
            IF ( pcmp1 ) THEN
               ip = Ipcmp1
               IF ( Icore(ip)==Pid ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ipc21 = Ipcmp2 - 1
               DO ip = Ipcmp1 , ipc21
                  IF ( Icore(ip)==-1 .AND. ip<ipc21 ) THEN
                     IF ( Icore(ip+1)==Pid ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
!
!     SEARCH FOR PID IN PCOMP2 DATA
!
            IF ( .NOT.pcmp2 ) THEN
               Ierr = 1
               RETURN
            ELSE
               ip = Ipcmp2
               IF ( Icore(ip)==Pid ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               lpc11 = lpcomp - 1
               DO ip = Ipcmp2 , lpc11
                  IF ( Icore(ip)==-1 .AND. ip<lpc11 ) THEN
                     IF ( Icore(ip+1)==Pid ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDDO
!
!     PID WAS NOT LOCATED; ISSUE ERROR
!
               Ierr = 1
               RETURN
            ENDIF
!
!     PID WAS LOCATED; DETERMINE TYPE
!
 10         ip = ip + 1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         itype = pcomp
         pidloc = ip
         nlay = Icore(pidloc+1)
         ipoint = pidloc + 8 + 4*nlay
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
         ip = ip + 1
         spag_nextblock_1 = 4
      CASE (4)
         itype = pcomp1
         pidloc = ip
         nlay = Icore(pidloc+1)
         ipoint = pidloc + 8 + nlay
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
!
         ip = ip + 1
         spag_nextblock_1 = 6
      CASE (6)
         itype = pcomp2
         pidloc = ip
         nlay = Icore(pidloc+1)
         ipoint = pidloc + 8 + 2*nlay
         spag_nextblock_1 = 7
      CASE (7)
!
         tsubo = Core(ipoint+24)
         delta = Tmean - tsubo
         lamopt = Icore(pidloc+8)
         nonmem = lamopt/=mem .AND. lamopt/=symmem
!
!     LAMOPT - LAMINATION GENERATION OPTION
!            = ALL     (ALL PLYS, DEFAULT)
!            = SYM     (SYMMETRIC)
!            = MEM     (MEMBRANE ONLY)
!            = SYMMEM  (SYMMETRIC-MEMBRANE)
!
!     CONSTRUCT THE LAMINATE FORCE-STRAIN MATRIX
!
!     EXTENSIONAL
!
         Matid = Mid(1)
         CALL mat(Elid)
         CALL lpropd(gprop)
!
         DO ll = 1 , 3
            ii = 3*(ll-1)
            DO mm = 1 , 3
               abbd(ll,mm) = gprop(mm+ii)*Tlam
            ENDDO
         ENDDO
!
!     BENDING
!
         IF ( nonmem ) THEN
!
            Matid = Mid(2)
            CALL mat(Elid)
            CALL lpropd(gprop)
!
            DO ll = 1 , 3
               ii = 3*(ll-1)
               DO mm = 1 , 3
                  abbd(ll+3,mm+3) = gprop(mm+ii)*minrt
               ENDDO
            ENDDO
!
!     MEMBRANE-BENDING
!
            IF ( lamopt/=sym ) THEN
!
               Matid = Mid(4)
               CALL mat(Elid)
               CALL lpropd(gprop)
!
               DO ll = 1 , 3
                  ii = 3*(ll-1)
                  DO mm = 1 , 3
                     abbd(ll,mm+3) = gprop(mm+ii)*Tlam*Tlam
                     abbd(ll+3,mm) = gprop(mm+ii)*Tlam*Tlam
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
!
!
!     BEGIN THE LOOP OVER LAYERS
!
         zk = zref
         DO k = 1 , nlay
!
!     SET THE LAYER-DEPENDENT VARIABLES
!
            zk1 = zk
            IF ( itype==pcomp ) THEN
               zk = zk1 + Core(pidloc+6+4*k)
               theta = Core(pidloc+7+4*k)
!
            ELSEIF ( itype==pcomp1 ) THEN
               zk = zk1 + Core(pidloc+7)
               theta = Core(pidloc+8+k)
!
            ELSEIF ( itype==pcomp2 ) THEN
               zk = zk1 + Core(pidloc+7+2*k)
               theta = Core(pidloc+8+2*k)
            ENDIF
!
!     LAYER MATERIAL PROPERTIES
!
            DO ir = 1 , 9
               glay(ir) = Core(ipoint+ir)
            ENDDO
!
            DO ir = 1 , 3
               alphal(ir) = Core(ipoint+13+ir)
            ENDDO
!
            ti = zk - zk1
            zsubi = (zk+zk1)/2.0D0
            deltat = delta + zsubi*Tgrad
!
!     TRANSFORM THE LAYER MATERIAL PROPERTIES FROM THE FIBER SYSTEM TO
!     THE ELEMENT SYSTEM
!
            theta = theta*Degrad + Thetae
            c = dcos(theta)
            c2 = c*c
            s = dsin(theta)
            s2 = s*s
!
            transl(1) = c2
            transl(2) = s2
            transl(3) = c*s
            transl(4) = s2
            transl(5) = c2
            transl(6) = -c*s
            transl(7) = -2.0D0*c*s
            transl(8) = 2.0D0*c*s
            transl(9) = c2 - s2
!
!                _            T
!     CALCULATE [G] = [TRANSL] [GLAY][TRANSL]
!
            CALL gmmatd(glay(1),3,3,0,transl(1),3,3,0,glayt(1))
            CALL gmmatd(transl(1),3,3,1,glayt(1),3,3,0,gbar(1))
!
!     CALCULATE [ALPHAE] = [TRANSL]X[ALPHA]
!     MODIFY [TRANSL] FOR TRANSFORMATIONS OF ALPHAS
!
            transl(3) = -transl(3)
            transl(6) = -transl(6)
            transl(7) = -transl(7)
            transl(8) = -transl(8)
!
            CALL gmmatd(transl(1),3,3,0,alphal(1),3,1,0,alphae(1))
!
!
!     CALCULATE THERMAL FORCES AND MOMENTS
!
            CALL gmmatd(gbar(1),3,3,0,alphae(1),3,1,0,galpha(1))
!
            DO ir = 1 , 3
               Ftherm(ir) = Ftherm(ir) + galpha(ir)*deltat*(zk-zk1)
               IF ( nonmem ) Ftherm(ir+3) = Ftherm(ir+3) - galpha(ir)*deltat*(zk*zk-zk1*zk1)/2.0D0
            ENDDO
!
!     CALCULATE CONTRIBUTION FROM SYMMETRIC LAYERS
!
            IF ( lamopt==sym .OR. lamopt==symmem ) THEN
               deltat = delta - zsubi*Tgrad
!
               DO ir = 1 , 3
                  Ftherm(ir) = Ftherm(ir) + galpha(ir)*deltat*(zk-zk1)
                  IF ( nonmem ) Ftherm(ir+3) = Ftherm(ir+3) - galpha(ir)*deltat*(zk1*zk1-zk*zk)/2.0D0
               ENDDO
            ENDIF
            IF ( itype==pcomp ) ipoint = ipoint + 27
!
         ENDDO
!
!
!     END OF LOOP OVER THE LAYERS
!
!     COMPUTE THERMAL STRAIN VECTOR
!
!                      -1
!     {EPSLNT} = [ABBD]  {FTHERM}
!
         ising = -1
         CALL inverd(6,abbd,6,dum,0,determ,ising,indx)
!
         DO ll = 1 , 6
            nn = 6*(ll-1)
            DO mm = 1 , 6
               stiff(nn+mm) = abbd(ll,mm)
            ENDDO
         ENDDO
!
         CALL gmmatd(stiff(1),6,6,0,Ftherm(1),6,1,0,Epslnt(1))
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE shctsd
