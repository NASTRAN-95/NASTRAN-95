!*==cf1ort.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cf1ort(Sucess,Maxits,Ten2mt,Nzero,Iortho,Vr,Vl,V1,V1l,V2,V2l,Zb)
   USE c_feeraa
   USE c_feerxc
   USE c_names
   USE c_system
   USE c_unpakx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   LOGICAL :: Sucess
   INTEGER :: Maxits
   REAL :: Ten2mt
   INTEGER :: Nzero
   INTEGER :: Iortho
   REAL , DIMENSION(1) :: Vr
   REAL , DIMENSION(1) :: Vl
   REAL , DIMENSION(1) :: V1
   REAL , DIMENSION(1) :: V1l
   REAL , DIMENSION(1) :: V2
   REAL , DIMENSION(1) :: V2l
   INTEGER , DIMENSION(1) :: Zb
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: a
   REAL :: critf
   INTEGER :: i , j , k , l , ll , mortho
   REAL , DIMENSION(4) :: otest
   LOGICAL :: skip
   EXTERNAL cfnor1 , close , gopen , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!*******
!     CF1ORT IS A SINGLE-PRECISION ROUTINE (CREATED FOR USE BY
!     THE COMPLEX FEER METHOD) WHICH PERFORMS THE
!     REORTHOGONALIZATION ALGORITHM
!*******
!     DEFINITION OF INPUT AND OUTPUT PARAMETERS
!*******
!     SUCESS   = LOGICAL INDICATOR FOR SUCCESSFUL REORTHOGONALIZATION
!                (OUTPUT)
!     MAXITS   = MAXIMUM NUMBER OF ALLOWED ITERATIONS (INPUT)
!     TEN2MT   = CONVERGENCE CRITERION
!     NZERO    = NUMBER OF ORTHOGONAL VECTOR PAIRS IN PRIOR
!                NEIGHBORHOODS INCLUDING RESTART
!     IORTHO   = NUMBER OF EXISTING ORTHOGONAL VECTOR PAIRS
!                IN CURRENT NEIGHBORHOOD
!     VR       = RIGHT-HANDED VECTOR TO BE REORTHOGONALIZED
!     VL       = LEFT -HANDED VECTOR TO BE REORTHOGONALIZED
!     V1,V1L,  = WORKING SPACE FOR FOUR VECTORS (V1L MUST
!     V2,V2L     FOLLOW V1 IN CORE)
!     ZB       = WORKING SPACE FOR ONE GINO BUFFER
!*******
         mortho = Nzero + Iortho
         IF ( mortho<=0 ) THEN
            Sucess = .TRUE.
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( qpr ) WRITE (nout,99001)
99001       FORMAT (1H0,//26H BEGIN REORTHOGONALIZATION,//)
            numort = numort + 1
            k = 0
            Sucess = .FALSE.
            nn = nord2
            critf = 100.*Ten2mt**2
            DO i = 1 , nord2
               V2(i) = Vr(i)
               V2l(i) = Vl(i)
            ENDDO
            CALL gopen(iscr7,Zb(1),rdrew)
         ENDIF
         SPAG_Loop_1_2: DO
            DO i = 1 , 4
               otest(i) = 0.
            ENDDO
            ll = 2
!*******
!     ENTER LOOP
!*******
            DO i = 1 , mortho
               IF ( i==Nzero+1 ) ll = 0
               IF ( qpr ) WRITE (nout,99002) i
99002          FORMAT (1H ,13HUNPACK VECTOR,I4)
!     VALUES ARE UNPACKED INTO BOTH V1 AND V1L
               CALL unpack(*5,iscr7,V1(1))
               IF ( qpr ) THEN
                  WRITE (nout,99006) (V1(j),j=1,nord2)
                  WRITE (nout,99006) (V1l(j),j=1,nord2)
               ENDIF
!*******
!     OBTAIN RIGHT-HAND INNER-PRODUCT TERM
!*******
               CALL cfnor1(Vr(1),V1l(1),nord2,1,a(1))
!*******
!     SUBTRACT OFF RIGHT-HAND INNER-PRODUCT TERM
!*******
               DO j = 1 , nord2 , 2
                  l = j + 1
                  V2(j) = V2(j) - a(1)*V1(j) + a(2)*V1(l)
                  V2(l) = V2(l) - a(1)*V1(l) - a(2)*V1(j)
               ENDDO
!*******
!     COMPUTE MAXIMUM RIGHT-HAND SQUARED-ERROR
!*******
               a(1) = a(1)**2 + a(2)**2
               IF ( otest(ll+1)<a(1) ) otest(ll+1) = a(1)
!*******
!     OBTAIN LEFT-HAND INNER-PRODUCT TERM
!*******
               CALL cfnor1(Vl(1),V1(1),nord2,1,a(1))
!*******
!     SUBTRACT OFF LEFT-HAND INNER-PRODUCT TERM
!*******
               DO j = 1 , nord2 , 2
                  l = j + 1
                  V2l(j) = V2l(j) - a(1)*V1l(j) + a(2)*V1l(l)
                  V2l(l) = V2l(l) - a(1)*V1l(l) - a(2)*V1l(j)
               ENDDO
!*******
!     COMPUTE MAXIMUM LEFT-HAND SQUARED-ERROR
!*******
               a(1) = a(1)**2 + a(2)**2
               IF ( otest(ll+2)<a(1) ) otest(ll+2) = a(1)
               CYCLE
 5             IF ( idiag/=0 ) WRITE (nout,99003) i
99003          FORMAT (18H ORTHOGONAL VECTOR,I4,39H IS NULL IN REORTHOGONALIZATION ROUTINE)
            ENDDO
            DO i = 1 , nord2
               Vr(i) = V2(i)
               Vl(i) = V2l(i)
            ENDDO
            skip = .FALSE.
            IF ( qpr ) THEN
               WRITE (nout,99006) (Vr(i),i=1,nord2)
               WRITE (nout,99006) (Vl(i),i=1,nord2)
            ENDIF
            SPAG_Loop_2_1: DO
!*******
!     TEST FOR CONVERGENCE
!*******
               IF ( idiag/=0 ) WRITE (nout,99004) k , critf , otest
99004          FORMAT (32H   REORTHOGONALIZATION ITERATION,I3,9X,14HTARGET VALUE =,E12.4,4X,8HERRORS =,4E12.4)
               IF ( otest(1)<=critf .AND. otest(2)<=critf .AND. otest(3)<=critf .AND. otest(4)<=critf ) THEN
                  CALL close(iscr7,norew)
                  Sucess = .TRUE.
                  EXIT SPAG_Loop_2_1
               ELSE
                  IF ( .NOT.(skip) ) THEN
                     IF ( k==1 .OR. k==3 .OR. k==5 ) THEN
                        IF ( idiag/=0 ) WRITE (nout,99005)
99005                   FORMAT (52H   REORTHOGONALIZATION TOLERANCE TEMPORARILY RELAXED)
                        critf = 100.*critf
                        skip = .TRUE.
                        CYCLE
                     ENDIF
                  ENDIF
                  k = k + 1
                  IF ( k>Maxits ) THEN
                     CALL close(iscr7,norew)
                     EXIT SPAG_Loop_2_1
                  ELSE
                     CALL close(iscr7,eofnrw)
                     CALL gopen(iscr7,Zb(1),rdrew)
                     CYCLE SPAG_Loop_1_2
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_2_1
            EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 2
      CASE (2)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99006 FORMAT (3H --,32(4H----),/(1H ,4E25.16))
END SUBROUTINE cf1ort
