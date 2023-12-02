!*==curvit.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE curvit(Indep,Ni,Dep,Nd,Ifile,Z,Iz,Lz,Mclose,Toler,Mcsid,Xscale,Yscale)
   IMPLICIT NONE
   USE C_NAMES
   USE C_SYSTEM
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(2,1) :: Indep
   INTEGER :: Ni
   REAL , DIMENSION(2,1) :: Dep
   INTEGER :: Nd
   INTEGER :: Ifile
   REAL , DIMENSION(1) :: Z
   INTEGER , DIMENSION(1) :: Iz
   INTEGER :: Lz
   INTEGER :: Mclose
   REAL :: Toler
   INTEGER :: Mcsid
   REAL :: Xscale
   REAL :: Yscale
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: eor , noeor
   REAL :: fmax , fmin , tol , x , y
   INTEGER :: i , ibuf , icrq , idx , ilist , ipts , ising , ixy , j , jxy , jz , k , k1 , k2 , n , nclose , nlist , nxy
   INTEGER , DIMENSION(2) :: itemp
   INTEGER , DIMENSION(2) , SAVE :: subr
   EXTERNAL close , gopen , mesage , page2 , ssplin , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     PERFORMS LOCAL INTERPOLATION
!
!     INDEP  = X,Y COORDINATES OF INDEPENDENT ELEMENT CENTERS (2 X NI)
!     DEP    = X,Y COORDINATES OF DEPENDENT GRID POINTS  (2 X ND)
!     IFILE  = FILE TO WRITE SPECIAL FORM ROWS OF G-MATRIX
!     Z      = REAL AREA OF CORE, LENGTH = LZ.
!     IZ     = EQUIVALENT INTEGER AREA OF CORE, LENGTH = LZ.
!     MCLOSE = NUMBER OF CLOSEST INDEPENDENT POINTS TO USE
!     TOLER  = PERCENT OF DISTANCE FROM A DEPENDENT POINT TO
!              INDEPENDENT POINT NUMBER -NCLOSE- POINTS FURTHER OUT ARE
!              ALLOWED TO BE SUCH AS TO BE INCLUDED IN A LOCAL
!              INTERPOLATION.
!
   DATA subr/4HCURV , 4HIT  / , eor , noeor/1 , 0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nclose = min0(Mclose,Ni)
         IF ( nclose<=2 ) nclose = Ni
!
!     COMPUTE TOLERANCE MULTIPLIER WITH RESPECT TO SQUARES.
!     TOLERANCE IS IN PERCENT OF DISTANCE TO POINT NUMBER -NCLOSE- IN
!     FINAL LIST
!
         tol = (1.0+Toler/100.0)**2
!
!     THUS IF DISTANCE FROM THE DEPENDENT POINT TO INDEPENDENT POINT
!     NUMBER -NCLOSE- = LSQ, ADDITIONAL INDEPENDENT POINTS WILL BE
!     INCLUDED IF THE SQUARE OF THEIR DISTANCE TO THE DEPENDENT POINT
!     IS .LE. TOL TIMES LSQ.
!
!
!     ALLOCATE BUFFER FOR -IFILE- AND OPEN -IFILE-.
!
         ibuf = Lz - Sysbuf
         jz = ibuf - 1
         icrq = -jz
         IF ( jz>0 ) THEN
            CALL gopen(Ifile,Iz(ibuf),1)
!
!     EACH ROW OF G-MATRIX WILL BE WRITTEN AS A LOGICAL RECORD
!     WITH PAIRS OF
!                 1- INDEPENDENT POINT INDEX
!                 2- G VALUE
!
!
!     SHORT CUT WILL BE TAKEN IF ALL INDEPENDENT POINTS ARE TO BE USED
!     FOR INTERPOLATION AT EACH DEPENDENT POINT.
!
            IF ( nclose==Ni ) THEN
!
!     CHECK FOR SUFFICIENT CORE FOR SHORT CUT.
!
               n = Ni + 3
               n = n**2 + 3*n + Ni*Nd + n*Nd
               IF ( n<=jz ) THEN
!
!     CALL SSPLIN AND GET G-MATRIX STORED BY ROWS.
!
                  CALL ssplin(Ni,Indep(1,1),Nd,Dep(1,1),0,0,0,1,0,Z(1),jz,ising)
                  IF ( ising/=2 ) THEN
!
!     OUTPUT ROWS OF G-MATRIX WITH INDEXES.
!
                     k = 0
                     DO i = 1 , Nd
                        DO j = 1 , Ni
                           k = k + 1
                           itemp(1) = j
                           itemp(2) = Iz(k)
                           CALL write(Ifile,itemp(1),2,noeor)
                        ENDDO
                        CALL write(Ifile,0,0,eor)
                     ENDDO
                  ELSE
                     n = 0
                     WRITE (Ioutpt,99001) Uwm , n , Mcsid
!
!     OUTPUT NULL ROW FOR EACH DEPENDENT POINT.
!
                     DO i = 1 , Nd
                        CALL write(Ifile,0,0,eor)
                     ENDDO
                  ENDIF
                  GOTO 10
               ENDIF
            ENDIF
!
!     MASTER LOOP ON DEPENDENT POINTS. EACH DEPENDENT POINT RESULTS IN
!     A VARIABLE LENGTH ROW OF G-MATRIX DEPENDING ON HOW MANY
!     INDEPENDENT POINTS ARE SELECTED FOR USE. (AT LEAST 3 MUST BE USED)
!
            DO i = 1 , Nd
!
!     LIST OF DISTANCE SQUARES OF ALL INDEPENDENT POINTS TO
!     CURRENT DEPENDENT POINT IS FORMED.
!
!     SELECTION OF THE -NCLOSE- SMALLEST VALUES IS THEN MADE.
!
!     THEN ANY OTHER INDEPENDENT POINTS WITHIN TOLERANCE RANGE OF
!     POINT NUMBER -NCLOSE- IN LIST ARE ADDED.
!
               fmax = 0.0
               x = Dep(1,i)
               y = Dep(2,i)
               icrq = Ni - jz
               IF ( Ni>jz ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO j = 1 , Ni
                  Z(j) = (Xscale*(Indep(1,j)-x))**2 + (Yscale*(Indep(2,j)-y))**2
                  IF ( Z(j)>fmax ) fmax = Z(j)
               ENDDO
               fmax = 2.0*fmax + 1.0
!
!     ALLOCATE FOR LIST OF INDEXES TO THE MINIMUMS.
!
               ilist = Ni + 1
               nlist = Ni
!
!     FIND -NCLOSE- SMALLEST VALUES.
!
               DO j = 1 , nclose
                  fmin = fmax
!
                  DO k = 1 , Ni
                     IF ( fmin>Z(k) ) THEN
                        fmin = Z(k)
                        idx = k
                     ENDIF
                  ENDDO
!
!     ADD INDEX TO THIS MINIMUM TO THE LIST
!
                  icrq = nlist + 1 - jz
                  IF ( icrq>0 ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  Iz(nlist+1) = idx
                  nlist = nlist + 1
!
!     RESET THIS VALUE SO IT CAN NOT BE USED AGAIN
!
                  Z(idx) = fmax
               ENDDO
!
!     ADD ANY ADDITIONAL INDEPENDENT POINTS WITHIN TOLERANCE RANGE OF
!     LAST ONE SELECTED ABOVE.
!
               fmax = tol*fmin
               DO j = 1 , Ni
                  IF ( Z(j)<=fmax ) THEN
                     icrq = nlist + 1 - jz
                     IF ( icrq>0 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     Iz(nlist+1) = j
                     nlist = nlist + 1
                  ENDIF
               ENDDO
!
!     LIST IS COMPLETE THUS MOVE IT TO THE BEGINNING OF THE CORE BLOCK.
!
               j = 0
               DO k = ilist , nlist
                  j = j + 1
                  Iz(j) = Iz(k)
               ENDDO
               ilist = 1
               nlist = j
               ipts = j
!
!     HERE AND IZ(ILIST) TO IZ(NLIST) CONTAINS LIST OF
!     POSITION INDEXES OF INDEPENDENT POINT COORDINATES TO BE USED.
!
!     NOW SET UP LIST OF XY-CCORDINATES OF THESE INDEPENDENT POINTS
!     FOR THE SSPLIN CALL.
!
               ixy = nlist + 1
               nxy = nlist + 2*ipts
               icrq = nxy - jz
               IF ( nxy>jz ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               jxy = nlist
               DO j = ilist , nlist
                  k = Iz(j)
                  Z(jxy+1) = Indep(1,k)
                  Z(jxy+2) = Indep(2,k)
                  jxy = jxy + 2
               ENDDO
!
!     NOW READY FOR SSPLIN ROUTINE CALL.
!
               CALL ssplin(ipts,Z(ixy),1,Dep(1,i),0,0,0,1,0,Z(jxy+1),jz-jxy,ising)
               IF ( ising/=2 ) THEN
!
!     REPLACE INDEPENDENT POINT XY PAIRS WITH SPECIAL FORM DEPENDENT
!     POINT G-MATRIX OUTPUT ROW.
!
                  k1 = ilist
                  k2 = jxy + 1
                  DO j = ixy , nxy , 2
                     Iz(j) = Iz(k1)
                     Z(j+1) = Z(k2)
                     k1 = k1 + 1
                     k2 = k2 + 1
                  ENDDO
               ELSE
!
!     ILL-CONDITION FOR THIS DEPENDENT POINT - NO SOLUTION POSSIBLE.
!
                  CALL page2(4)
                  WRITE (Ioutpt,99001) Uwm , i , Mcsid
                  ipts = 0
               ENDIF
!
               CALL write(Ifile,Iz(ixy),2*ipts,eor)
!
!  GO PROCESS NEXT DEPENDENT POINT.
!
            ENDDO
!
!     ALL G-MATRIX ROWS COMPLETE. (ROWS SINGULAR ARE EMPTY LOGICAL
!     RECORDS IN -IFILE- )
!
 10         CALL close(Ifile,Clsrew)
            RETURN
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
         CALL mesage(-8,icrq,subr)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99001 FORMAT (A25,' 2252. (CURVIT-1) LOCAL INTERPOLATION USING INDE','PENDENT VALUES WITHIN RANGE OF THE',/5X,I7,'-TH SORTED ',     &
             &'ORDER GRID ID INVOLVED WITH RESPECT TO MATERIAL COORDIN','ATE SYSTEM ID',I9,/5X,'CAN NOT BE COMPLETED.  ILL-CONDI',  &
             &'TION MAY HAVE RESULTED FROM ALIGNMENT OF INDEPENDENT ','VALUE COORDINATES.',/5X,                                     &
             &'OUTPUT FOR THE GRID ID IN QUESTION WILL NOT APPEAR.')
END SUBROUTINE curvit
