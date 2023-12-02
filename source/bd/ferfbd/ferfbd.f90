!*==ferfbd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ferfbd(V1,V2,V3,Vb)
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
   REAL(REAL64) , DIMENSION(1) :: V2
   REAL(REAL64) , DIMENSION(1) :: V3
   REAL(REAL64) , DIMENSION(1) :: Vb
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: dcore , xl
   INTEGER :: i , iblock , icbp , iclr , icol , icrow , ii , ik , ilrow , j , ji , mem , nout , nrow , ntms , ntmsnx , ntmss
   INTEGER , DIMENSION(20) :: iblk
   REAL(REAL64) :: v2j , v3j , xljj
   EXTERNAL dscpos , dsspos , endget , endgtb , ferltd , getstb , getstr , rewind , skprec
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!  FERFBD is a modification of the old FRBK2 subroutine.  It has been
!  modified to read matrix data from memory until that data is exhausted
!  and then to read the remaining data from the file.
!
   !>>>>EQUIVALENCE (Ksystm(02),Nout)
   !>>>>EQUIVALENCE (Dcore(1),Icore(1),Xl)
!
         nrow = mcblt(2)
         DO i = 1 , nrow
            V2(i) = V1(i)
         ENDDO
         ilrow = ltpos(1)
         icrow = nrow
!      PRINT *,' FERFBD,ILROW,NIDLT=',ILROW,NIDLT
!      PRINT *,' LTPOS=',LTPOS
         IF ( ilrow==nrow .AND. nidlt/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     BACKWARD SUBSTITUTION
!
!     POSITION FILE TO LAST COLUMN
!
         IF ( nidlt/=0 ) THEN
            CALL dsspos(mcblt,ltpos(5),ltpos(6),ltpos(7))
         ELSEIF ( ltpos(5)/=-1 ) THEN
            CALL dsspos(mcblt,ltpos(5),ltpos(6),ltpos(7))
         ELSE
            CALL rewind(mcblt)
            CALL skprec(mcblt,nrow+1)
            CALL dscpos(mcblt,iblock,iclr,icbp)
            ltpos(5) = iblock
            ltpos(6) = iclr
            ltpos(7) = icbp
         ENDIF
         iblk(1) = mcblt(1)
         j = nrow
         spag_nextblock_1 = 2
      CASE (2)
         iblk(8) = -1
         icrow = j
         IF ( j<=ilrow ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         CALL getstb(*20,iblk(1))
         ntms = iblk(6)
         ji = iblk(5)
         ik = iblk(4)
         IF ( ik-ntms+1==j ) THEN
            ntms = ntms - 1
            xljj = xl(ji-ntms)
            IF ( ntms==0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         v2j = V2(j)
         DO ii = 1 , ntms
            v2j = v2j + xl(ji)*V2(ik)
            ji = ji - 1
            ik = ik - 1
         ENDDO
         V2(j) = v2j
         spag_nextblock_1 = 4
      CASE (4)
         CALL endgtb(iblk(1))
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 20      V2(j) = V2(j)/xljj
         IF ( j==1 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = j - 1
         spag_nextblock_1 = 2
      CASE (5)
!
!     CONTINUE BACKWARD SUBSTITUTION WITH DATA IN MEMORY
!
         mem = nltli
!      PRINT *,' AT 1000,NLTLI=',NLTLI
         ntms = icore(mem)
!      PRINT *,' ICORE(NLTLI,-1=',ICORE(NLTLI),ICORE(NLTLI-1)
         mem = mem - 2*ntms - 3
         j = icrow
         spag_nextblock_1 = 6
      CASE (6)
         icol = icore(mem)
!      PRINT *,' MEM,ICORE(MEM-1,0,+1=',MEM,ICORE(MEM-1),ICORE(MEM),
!     & ICORE(MEM+1)
!      PRINT *,' ICOL,MEM,NTMS,ICROW,J=',ICOL,MEM,NTMS,ICROW,J
         IF ( icol/=j ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ntms = icore(mem+1)
!      PRINT *,' FERFBD,A1015,J,NTMS,ICOL=',J,NTMS,ICOL
         ntmss = ntms
         ji = mem/2 + 1 + ntms
         ik = icore(mem+2+2*ntms) + ntms - 1
!      PRINT *,' FERFBD,IK=',IK
         IF ( ik-ntms+1==j ) THEN
            ntms = ntms - 1
            xljj = dcore(ji-ntms)
!      PRINT *,' FERFBD,XLJJ=',XLJJ
            IF ( ntms==0 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         v2j = V2(j)
         DO ii = 1 , ntms
            v2j = v2j + dcore(ji)*V2(ik)
            ji = ji - 1
            ik = ik - 1
         ENDDO
         V2(j) = v2j
         spag_nextblock_1 = 7
      CASE (7)
         IF ( mem/=nidlt ) THEN
            ntmsnx = icore(mem-1)
            mem = mem - 2*ntmsnx - 4
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         V2(j) = V2(j)/xljj
         IF ( j/=1 ) THEN
            j = j - 1
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         CALL ferltd(mcbsma(1),V2(1),V3(1),Vb(1))
!
! BEGIN FORWARD SWEEP DIRECTLY ON V3
!
         icrow = 1
         IF ( nidlt==0 ) THEN
            CALL rewind(mcblt)
            CALL skprec(mcblt,1)
            spag_nextblock_1 = 11
         ELSE
            mem = nidlt
            DO j = 1 , nrow
               icrow = j
               IF ( j>ilrow ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               SPAG_Loop_2_1: DO
                  icol = icore(mem)
                  IF ( icol/=j ) EXIT SPAG_Loop_2_1
                  ji = mem/2 + 2
                  ntms = icore(mem+1)
                  ntmss = ntms
                  ik = icore(mem+2+2*ntms)
                  IF ( ik==j ) THEN
                     ntms = ntms - 1
                     V3(j) = V3(j)/dcore(ji)
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
                  mem = mem + 2*ntmss + 4
               ENDDO SPAG_Loop_2_1
            ENDDO
            RETURN
         ENDIF
      CASE (10)
!
!     CONTINUE FORWARD SWEEP DIRECTLY ON V3
!
!     POSITION FILE TO CONTINUE READING COLUMN DATA NOT IN MEMORY
!
         CALL dsspos(mcblt,ltpos(2),ltpos(3),ltpos(4))
         spag_nextblock_1 = 11
      CASE (11)
         DO j = icrow , nrow
            iblk(8) = -1
            DO
               CALL getstr(*40,iblk)
!      PRINT *,' GETSTR,J,IBLK(12=',J,IBLK(12)
               ik = iblk(4)
               ji = iblk(5)
               ntms = iblk(6)
               IF ( ik==j ) THEN
                  ntms = ntms - 1
!      PRINT *,' IK,JI,XL(JI=',IK,JI,XL(JI)
                  V3(j) = V3(j)/xl(ji)
                  ji = ji + 1
                  ik = ik + 1
               ENDIF
               IF ( ntms/=0 ) THEN
                  v3j = V3(j)
                  DO ii = 1 , ntms
                     V3(ik) = V3(ik) + xl(ji)*v3j
                     ik = ik + 1
                     ji = ji + 1
                  ENDDO
               ENDIF
               CALL endget(iblk(1))
            ENDDO
 40      ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ferfbd
