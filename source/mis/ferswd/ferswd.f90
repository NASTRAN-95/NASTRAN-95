!*==ferswd.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ferswd(V1,V3,Vb)
   USE c_feerim
   USE c_opinv
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: V1
   REAL(REAL64) , DIMENSION(1) :: V3
   REAL(REAL64) , DIMENSION(1) :: Vb
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: dcore , xl
   INTEGER , DIMENSION(20) :: iblk
   INTEGER :: icol , icrow , ii , ik , ilrow , io , j , ji , mem , nrow , ntms , ntmsnx , ntmss
   REAL(REAL64) :: sum , v3j , xljj
   REAL(REAL64) , SAVE :: zero
   EXTERNAL dsspos , endget , endgtb , ferltd , getstb , getstr , rewind , skprec
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!  The original to this subroutine was FRSW2.  It has been modified
!  to read the matrix data from memory and after this data is exhausted
!  then to read the remaining data from the file.
!
   !>>>>EQUIVALENCE (Ksystm(02),Io)
   !>>>>EQUIVALENCE (Dcore(1),Icore(1),Xl(1))
   DATA zero/0.0D0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nrow = mcblt(2)
         CALL ferltd(mcbsma(1),V1(1),V3(1),Vb(1))
!   FORWARD SWEEP DIRECTLY ON V3
         icrow = 1
         IF ( nidlt==0 ) THEN
            CALL rewind(mcblt)
            CALL skprec(mcblt,1)
            spag_nextblock_1 = 3
         ELSE
            ilrow = ltpos(1)
            mem = nidlt
            DO j = 1 , nrow
               icrow = j
               IF ( icrow>ilrow ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               SPAG_Loop_2_1: DO
                  icol = icore(mem)
                  IF ( icol/=j ) THEN
                     V3(j) = V3(j)/xljj
                     EXIT SPAG_Loop_2_1
                  ELSE
                     ji = mem/2 + 2
                     ntms = icore(mem+1)
                     ntmss = ntms
                     ik = icore(mem+2+2*ntms)
                     IF ( ik==j ) THEN
                        ntms = ntms - 1
                        xljj = dcore(ji)
                        ji = ji + 1
                        ik = ik + 1
                     ENDIF
                     IF ( ntms/=0 ) THEN
                        v3j = V3(j)
                        DO ii = 1 , ntms
                           V3(ik) = V3(ik) + dcore(ji)*v3j
                           ik = ik + 1
                           ji = ji + 1
                        ENDDO
                     ENDIF
                     mem = mem + ntmss*2 + 4
                  ENDIF
               ENDDO SPAG_Loop_2_1
            ENDDO
!  CONTINUE BACKWARD SUBSTITUTION USING DATA FROM MEMORY
            mem = mem - ntmss*2 - 4
            spag_nextblock_1 = 5
         ENDIF
      CASE (2)
! POSITION FILE TO APPROPRIATE COLUMN
         CALL dsspos(mcblt,ltpos(2),ltpos(3),ltpos(4))
         spag_nextblock_1 = 3
      CASE (3)
         iblk(1) = mcblt(1)
!
! CONTINUE WITH FORWARD SWEEP
!
         DO j = icrow , nrow
            iblk(8) = -1
            DO
               CALL getstr(*10,iblk)
               ik = iblk(4)
               ji = iblk(5)
               ntms = iblk(6)
               IF ( ik==j ) THEN
                  ntms = ntms - 1
                  xljj = xl(ji)
                  ji = ji + 1
                  ik = ik + 1
               ENDIF
               IF ( ntms/=0 ) THEN
                  v3j = V3(j)
                  IF ( v3j/=zero ) THEN
                     DO ii = 1 , ntms
                        V3(ik) = V3(ik) + xl(ji)*v3j
                        ik = ik + 1
                        ji = ji + 1
                     ENDDO
                  ENDIF
               ENDIF
               CALL endget(iblk)
            ENDDO
 10         V3(j) = V3(j)/xljj
         ENDDO
!
!     BACKWARD SUBSTITUTION OMIT DIAGONAL
!
         icrow = nrow
         IF ( j==1 ) RETURN
         IF ( ilrow==nrow .AND. nidlt/=0 ) THEN
            mem = mem - ntmss*2 - 4
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSE
            j = nrow
            iblk(8) = -1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         DO
            CALL getstb(*20,iblk)
            ik = iblk(4)
            ji = iblk(5)
            ntms = iblk(6)
            IF ( ik-ntms+1==j ) ntms = ntms - 1
            IF ( ntms/=0 ) THEN
               sum = zero
               DO ii = 1 , ntms
                  sum = sum + xl(ji)*V3(ik)
                  ji = ji - 1
                  ik = ik - 1
               ENDDO
               V3(j) = V3(j) + sum
            ENDIF
            CALL endgtb(iblk)
         ENDDO
 20      IF ( j==1 ) RETURN
         j = j - 1
         IF ( j<=ilrow ) THEN
            mem = mem - ntmss*2 - 4
         ELSE
            iblk(8) = -1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         SPAG_Loop_1_2: DO
            icol = icore(mem)
            IF ( icol/=j ) THEN
               IF ( j==1 ) EXIT SPAG_Loop_1_2
               j = j - 1
            ELSE
               ntms = icore(mem+1)
               ntmss = ntms
               ji = mem/2 + 1 + ntms
               ik = icore(mem+2+2*ntms) + ntms - 1
               IF ( ik-ntms+1==j ) ntms = ntms - 1
               IF ( ntms/=0 ) THEN
                  v3j = V3(j)
                  DO ii = 1 , ntms
                     v3j = v3j + dcore(ji)*V3(ik)
                     ji = ji - 1
                     ik = ik - 1
                  ENDDO
                  V3(j) = v3j
               ENDIF
               IF ( mem==nidlt ) EXIT SPAG_Loop_1_2
               ntmsnx = icore(mem-1)
               mem = mem - ntmsnx*2 - 4
            ENDIF
         ENDDO SPAG_Loop_1_2
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ferswd
