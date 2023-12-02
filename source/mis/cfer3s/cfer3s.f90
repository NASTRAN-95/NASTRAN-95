!*==cfer3s.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cfer3s(V1,V1l,V2,V2l,V3,V3l,V4,V4l,V5,V5l,Zb,Zc)
USE C_FEERAA
USE C_FEERXC
USE C_NAMES
USE C_PACKX
USE C_SYSTEM
USE C_UNPAKX
USE C_XMSSG
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: V1
   REAL , DIMENSION(1) :: V1l
   REAL , DIMENSION(1) :: V2
   REAL , DIMENSION(1) :: V2l
   REAL , DIMENSION(1) :: V3
   REAL , DIMENSION(1) :: V3l
   REAL , DIMENSION(1) :: V4
   REAL , DIMENSION(1) :: V4l
   REAL , DIMENSION(1) :: V5
   REAL , DIMENSION(1) :: V5l
   REAL , DIMENSION(1) :: Zb
   REAL , DIMENSION(1) :: Zc
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: a , dsave , temp1
   LOGICAL :: again , skip , sucess
   REAL , DIMENSION(4) :: d
   INTEGER :: i , ij , j , j2 , jj , lancos , n3 , nord8 , nout
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(8) :: s
   REAL :: ss
   REAL(REAL64) :: temp2
   REAL , SAVE :: zero
   EXTERNAL cf1ort , cfe1ao , cfnor1 , close , gopen , mesage , open , pack , read , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     CFER3S IS A SINGLE PRECISION ROUTINE (CALLED BY CFEER3) WHICH
!     PERFORMS THE TRIDIAGONAL REDUCTION FOR THE COMPLEX FEER METHOD
!
   !>>>>EQUIVALENCE (a(1),d(3)) , (Ksystm(2),Nout) , (d(1),s(1)) , (temp1(1),temp2)
   DATA zero/0./
   DATA name/4HCFER , 4H3S  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DEFINITION OF INPUT AND OUTPUT PARAMETERS
!
!     V1,V2,V3,V4,V5  = AREAS OF OPEN CORE DESIGNATED BY SUBROUTINE
!                       CFEER3 AND USED INTERNALLY AS WORKING VECTORS,
!                       USUALLY RIGHT-HANDED
!     V1L,.......,V5L = SAME AS V1 THRU V5 BUT USUALLY LEFT-HANDED
!     RESTRICTION ..... LEFT-HANDED VECTOR MUST IMMEDIATELY FOLLOW
!                       CORRESPONDING RIGHT-HANDED VECTOR IN CORE
!                       ALSO, V2 SHOULD FOLLOW V1L FOR READ TO WORK
!     ZB,ZC           = REQUIRED GINO BUFFERS
!
!     DEFINITION OF INTERNAL PARAMETERS
!
!     A        = DIAGONAL ELEMENTS OF REDUCED TRIDIAGONAL MATRIX
!     D        = OFF-DIAG ELEMENTS OF REDUCED TRIDIAGONAL MATRIX
!     AGAIN    = LOGICAL INDICATOR FOR CYCLING THRU LOGIC AGAIN WHEN
!                NULL VECTOR TEST (D-BAR) FAILS
!     SKIP     = LOGICAL INDICATOR FOR AVOIDING REDUNDANT OPERATIONS
!     NORTHO   = TOTAL CURRENT NUMBER OF VECTOR PAIRS ON ORTHOGONAL
!                VECTOR FILE
!     NZERO    = NUMBER OF EIGENVECTOR PAIRS ON EIGENVECTOR FILE
!                (RESTART AND PRIOR NEIGHBORHOODS)
!     LANCOS   = LANCZOS ALGORITHM COUNTER
!     NSTART   = NUMBER OF INITIAL REORTHOGONALIZATION ATTEMPTS
!
         IF ( Qpr ) WRITE (nout,99001)
99001    FORMAT (1H1,50X,6HCFER3S,/1H0)
!
!     SET PACK AND UNPACK CONSTANTS
!
         Iprc = Csp
         Incr = 1
         Itp1 = Iprc
         Itp2 = Itp1
         Incrp = Incr
         Ii = 1
         Iip = 1
!
!     NN AND NNP ARE SET LOCALLY
!
         CALL gopen(Iscr(7),Zb(1),Wrtrew)
         CALL close(Iscr(7),Norew)
         IF ( Northo/=0 ) THEN
!
!     LOAD AND RE-NORMALIZE ALL EXISTING VECTORS ON THE NASTRAN
!     EIGENVECTOR FILE (INCLUDES ANY RESTART VECTORS AND ALL VECTORS
!     OBTAINED IN PRIOR NEIGHBORHOODS). PACK THESE VECTORS ON
!     THE ORTHOGONAL VECTOR SCRATCH FILE.
!
            CALL open(*20,Iphi(1),Zc(1),0)
!
!     LEFT-HAND VECTOR IS STORED IMMEDIATELY AFTER RIGHT-HAND VECTOR
!
            Nnp = Nord2
            nord8 = 2*Nord4
            DO i = 1 , Northo
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     IF ( Qpr ) WRITE (nout,99002) i
99002                FORMAT (1H ,13(10H----------),/,18H ORTHOGONAL VECTOR,I3)
!
!     THIS LOADS VALUES INTO V1, V1L, V2, AND V2L
!
                     CALL read(*40,*2,Iphi(1),V1(1),nord8+10,0,n3)
                     spag_nextblock_1 = 8
                     CYCLE SPAG_DispatchLoop_1
!
!     COMPRESS PHYSICAL EIGENVECTORS TO SINGLE PRECISION
!
 2                   DO j = 1 , Nord4
                        j2 = j*2
                        temp1(1) = V1(j2-1)
                        temp1(2) = V1(j2)
                        V1(j) = temp2
                     ENDDO
                     IF ( Idiag/=0 ) THEN
                        DO j = 1 , Nord4
                           IF ( V1(j)/=zero ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                        ENDDO
                        WRITE (nout,99003) i
99003                   FORMAT (18H ORTHOGONAL VECTOR,I4,8H IS NULL)
                     ENDIF
                     spag_nextblock_2 = 2
                  CASE (2)
                     IF ( Qpr ) WRITE (nout,99010) (V1(j),j=1,Nord2)
                     IF ( Qpr ) WRITE (nout,99010) (V1l(j),j=1,Nord2)
                     CALL cfnor1(V1(1),V1l(1),Nord2,0,d(1))
                     IF ( Idiag/=0 .AND. Nord2<=70 ) WRITE (nout,99004) i , (V1(j),V1(j+1),V1l(j),V1l(j+1),j=1,Nord2,2)
99004                FORMAT (1H0,17HORTHOGONAL VECTOR,I4,/1H0,23X,5HRIGHT,56X,4HLEFT,//,(1H ,2E25.16,10X,2E25.16))
                     CALL gopen(Iscr(7),Zb(1),Wrt)
                     CALL pack(V1(1),Iscr(7),Mcbvec(1))
                     CALL close(Iscr(7),Norew)
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
            CALL close(Iphi(1),Norew)
            IF ( Idiag/=0 ) WRITE (nout,99005) Northo , Mcbvec
99005       FORMAT (1H0,I10,32H ORTHOGONAL VECTOR PAIRS ON FILE,I5,12X,6I8,/)
         ENDIF
!
!     GENERATE INITIAL PSEUDO-RANDOM VECTORS
!
         n3 = 3*Nord
         ij = 0
         ss = 1.
         Nzero = Northo
         Nstart = 0
         lancos = 0
         again = .FALSE.
         d(1) = zero
         d(2) = zero
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            Numran = Numran + 1
            DO i = 1 , Nord4
               ij = ij + 1
               ss = -ss
               IF ( i>Nord2 ) THEN
                  IF ( i>n3 ) THEN
                     jj = 2*(i-n3) + Nord2
                  ELSE
                     jj = 2*i - 1 - Nord2
                  ENDIF
               ELSEIF ( i>Nord ) THEN
                  jj = 2*(i-Nord)
               ELSE
                  jj = 2*i - 1
               ENDIF
!
!     THIS LOADS VALUES INTO V1 AND V1L
!
               V1(jj) = ss*(mod(ij,3)+1)/(3.*(mod(ij,13)+1)*(1+5*float(i)/Nord))
            ENDDO
            IF ( Qpr ) WRITE (nout,99011) (V1(i),i=1,Nord4)
            IF ( Qpr ) WRITE (nout,99012)
!
!     NORMALIZE RIGHT AND LEFT START VECTORS
!
            CALL cfnor1(V1(1),V1l(1),Nord2,0,d(1))
!
!     REORTHOGONALIZE START VECTORS W.R.T. RESTART AND
!     PRIOR-NEIGHBORHOOD VECTORS
!
            CALL cf1ort(sucess,10,Ten2mt,Nzero,lancos,V1(1),V1l(1),V5(1),V5l(1),V3(1),V3l(1),Zb(1))
            IF ( sucess ) THEN
               IF ( again ) THEN
                  CALL cfe1ao(.FALSE.,V1(1),V4(1),V3(1),Zb(1))
                  CALL cfe1ao(.TRUE.,V1l(1),V4l(1),V3l(1),Zb(1))
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSE
!
!     SWEEP START VECTORS CLEAN OF ZERO-ROOT EIGENVECTORS
!
                  CALL cfe1ao(.FALSE.,V1(1),V2(1),V3(1),Zb(1))
                  CALL cfe1ao(.TRUE.,V1l(1),V2l(1),V3l(1),Zb(1))
!
!     NORMALIZE THE PURIFIED VECTOR AND OBTAIN D(1)
!
                  CALL cfnor1(V2(1),V2l(1),Nord2,0,d(1))
                  IF ( Nzero==0 .OR. Northo>Nzero ) EXIT SPAG_Loop_1_1
!
!     IF RESTART OR BEGINNING OF NEXT NEIGHBORHOOD, PERFORM
!     REORTHOGONALIZATION AND RENORMALIZATION
!
                  CALL cf1ort(sucess,10,Ten2mt,Nzero,lancos,V2(1),V2l(1),V5(1),V5l(1),V3(1),V3l(1),Zb(1))
                  IF ( sucess ) THEN
                     CALL cfnor1(V2(1),V2l(1),Nord2,0,d(1))
                     EXIT SPAG_Loop_1_1
                  ENDIF
               ENDIF
            ELSEIF ( again ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Nstart = Nstart + 1
            IF ( Nstart>2 ) THEN
               WRITE (nout,99006) Uwm , Lambda(1) , Lambda(3)
99006          FORMAT (A25,' 3158',//5X,'NO ADDITIONAL MODES CAN BE FOUND BY ','FEER IN THE NEIGHBORHOOD OF ',2E14.6,//)
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     LOAD FIRST VECTORS TO ORTHOGONAL VECTOR FILE
!
         CALL gopen(Iscr(7),Zb(1),Wrt)
         Nnp = Nord2
         CALL pack(V2(1),Iscr(7),Mcbvec(1))
         CALL close(Iscr(7),Norew)
         Northo = Northo + 1
!
!     COMMENCE LANCZOS ALGORITHM
!
!     INITIALIZE BY CREATING NULL VECTOR
!
         DO i = 1 , Nord2
            V1(i) = zero
            V1l(i) = zero
         ENDDO
         skip = .FALSE.
         spag_nextblock_1 = 3
      CASE (3)
!
!     ENTER LANCZOS LOOP
!
         lancos = lancos + 1
!
!     GENERATE DIAGONAL ELEMENT OF REDUCED TRIDIAGONAL MATRIX
!
         IF ( .NOT.skip ) CALL cfe1ao(.FALSE.,V2(1),V3(1),V5(1),Zb(1))
         skip = .FALSE.
         CALL cfnor1(V3(1),V2l(1),Nord2,1,a(1))
!
!     COMPUTE D-BAR
!
         CALL cfe1ao(.TRUE.,V2l(1),V3l(1),V5(1),Zb(1))
         DO i = 1 , Nord2 , 2
            j = i + 1
            V4(i) = V3(i) - a(1)*V2(i) + a(2)*V2(j) - d(1)*V1(i) + d(2)*V1(j)
            V4(j) = V3(j) - a(1)*V2(j) - a(2)*V2(i) - d(1)*V1(j) - d(2)*V1(i)
            V4l(i) = V3l(i) - a(1)*V2l(i) + a(2)*V2l(j) - d(1)*V1l(i) + d(2)*V1l(j)
            V4l(j) = V3l(j) - a(1)*V2l(j) - a(2)*V2l(i) - d(1)*V1l(j) - d(2)*V1l(i)
         ENDDO
         CALL cfnor1(V4(1),V4l(1),Nord2,2,d(1))
         dsave(1) = d(1)
         dsave(2) = d(2)
!
!     TEST IF LANCZOS ALGORITHM FINISHED
!
         IF ( lancos==Mreduc ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Qpr ) THEN
            WRITE (nout,99012)
            WRITE (nout,99007) d
99007       FORMAT (8H D-BAR =,2E16.8,9X,3HA =,2E16.8)
            WRITE (nout,99011) (V4(i),i=1,Nord2)
            WRITE (nout,99011) (V4l(i),i=1,Nord2)
            WRITE (nout,99012)
         ENDIF
!
!     NULL VECTOR TEST
!
         IF ( sqrt(d(1)**2+d(2)**2)<=sqrt(a(1)**2+a(2)**2)*Tenmht ) THEN
            IF ( Idiag/=0 ) WRITE (nout,99008) d
99008       FORMAT (14H D-BAR IS NULL,10X,4E20.12)
            again = .TRUE.
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     PERFORM REORTHOGONALIZATION
!
         CALL cfnor1(V4(1),V4l(1),Nord2,0,d(1))
         CALL cf1ort(sucess,10,Ten2mt,Nzero,lancos,V4(1),V4l(1),V3(1),V3l(1),V5(1),V5l(1),Zb(1))
         IF ( .NOT.sucess ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     NORMALIZE THE REORTHOGONALIZED VECTORS
!
         CALL cfnor1(V4(1),V4l(1),Nord2,0,d(1))
!
!     GENERATE OFF-DIAGONAL ELEMENT OF REDUCED TRIDIAGONAL MATRIX
!
         CALL cfe1ao(.FALSE.,V4(1),V3(1),V5(1),Zb(1))
         skip = .TRUE.
         CALL cfnor1(V3(1),V2l(1),Nord2,1,d(1))
         IF ( again ) THEN
            again = .FALSE.
            d(1) = zero
            d(2) = zero
!
!     NULL VECTOR TEST
!
         ELSEIF ( sqrt(d(1)**2+d(2)**2)<=sqrt(a(1)**2+a(2)**2)*Tenmht ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     TRANSFER TWO ELEMENTS TO REDUCED TRIDIAGONAL MATRIX FILE
!
         CALL write(Iscr(5),s(1),4,1)
         IF ( Idiag/=0 ) WRITE (nout,99013) lancos , d
!
!     LOAD CURRENT VECTORS TO ORTHOGONAL VECTOR FILE
!
         CALL gopen(Iscr(7),Zb(1),Wrt)
         Nnp = Nord2
         CALL pack(V4(1),Iscr(7),Mcbvec(1))
         CALL close(Iscr(7),Norew)
         Northo = Northo + 1
!
!     TRANSFER (I+1)-VECTORS TO (I)-VECTORS AND CONTINUE LANCZOS LOOP
!
         DO i = 1 , Nord2
            V1(i) = V2(i)
            V1l(i) = V2l(i)
            V2(i) = V4(i)
            V2l(i) = V4l(i)
         ENDDO
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
!
!     TRANSFER TWO ELEMENTS TO REDUCED TRIDIAGONAL MATRIX FILE
!
         IF ( d(1)==zero .AND. d(2)==zero ) THEN
            d(1) = dsave(1)
            d(2) = dsave(2)
         ENDIF
         CALL write(Iscr(5),s(1),4,1)
         IF ( Idiag/=0 ) WRITE (nout,99013) lancos , d
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
         Mreduc = lancos
         WRITE (nout,99009) Uwm , Mreduc , Lambda(1) , Lambda(3)
!
99009    FORMAT (A25,' 3157',//5X,'FEER PROCESS MAY HAVE CALCULATED ','FEWER ACCURATE MODES',I5,                                    &
                &' THAN REQUESTED IN THE NEIGHBORHOOD OF ',2E14.6,//)
         IF ( again ) THEN
            d(1) = zero
            d(2) = zero
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
 20      i = -1
         spag_nextblock_1 = 7
      CASE (7)
         CALL mesage(i,Iphi(1),name)
 40      i = -2
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
         i = -8
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
!
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99010 FORMAT (1H ,(1H ,4E25.16))
99011 FORMAT (1H0,13(10H----------),/,(1H ,4E25.16))
99012 FORMAT (1H ,13(10H----------))
99013 FORMAT (36H REDUCED TRIDIAGONAL MATRIX ELEMENTS,5X,3HROW,I4,/10X,14HOFF-DIAGONAL =,2E24.16,/14X,10HDIAGONAL =,2E24.16)
END SUBROUTINE cfer3s
