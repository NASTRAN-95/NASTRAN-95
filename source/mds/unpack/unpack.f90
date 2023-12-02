!*==unpack.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE unpack(File,A) !HIDESTARS (*,File,A)
   USE i_pakblk
   USE i_dsiof
   USE i_xnstrn
   USE c_unpakx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   REAL , DIMENSION(4) :: A
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: idiff , ilsrow , index1 , index2 , irow , itype , k , kk , kkk , num , numinc , numlef
   INTEGER , SAVE :: large
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
   DATA large/65536/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         name = File
         num = nwrdel(iabs(itypot))
         CALL dsipk1(iblkd,itypot)
         IF ( iretrn==1 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( irobgn<=0 .OR. lasrow<=0 ) THEN
            irobgn = iblkd(4)
            irow = irobgn
            ilsrow = large
         ELSE
            irow = irobgn
            ilsrow = lasrow
         ENDIF
         index2 = 1
         itype = iblkd(13)
         index1 = (iblkd(5)-1)*iblkd(14) + 1
         numinc = num*incr
         spag_nextblock_1 = 2
      CASE (2)
         IF ( iblkd(4)>ilsrow ) GOTO 20
         IF ( (iblkd(4)+iblkd(6)-1)<irobgn ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         idiff = (iblkd(4)+iblkd(7)) - irow
         iblkd(7) = iblkd(7) + 1
         IF ( idiff/=0 ) THEN
            IF ( idiff<0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO k = 1 , num
               DO kkk = 1 , idiff
                  A(index2+k-1+(kkk-1)*numinc) = 0.
               ENDDO
            ENDDO
            index2 = index2 + idiff*numinc
            irow = irow + idiff
         ENDIF
         IF ( iblkd(2)/=itype ) THEN
            CALL dsupkc(iblkd(2),itype,ibase(index1),A(index2))
         ELSE
!DIR$ NOVECTOR
            DO k = 1 , num
               A(index2+k-1) = ibase(index1+k-1)
!DIR$ VECTOR
            ENDDO
         ENDIF
         IF ( irow>=ilsrow ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         irow = irow + 1
         index2 = index2 + numinc
         spag_nextblock_1 = 4
      CASE (4)
         index1 = index1 + iblkd(11)
         IF ( iblkd(7)/=iblkd(6) ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         CALL endget(iblkd)
         CALL getstr(*20,iblkd)
         index1 = (iblkd(5)-1)*iblkd(14) + 1
         iblkd(7) = 0
         IF ( iblkd(8)<1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      IF ( ilsrow==large ) THEN
            lasrow = irow - 1
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
            numlef = lasrow - irow + 1
            IF ( numlef>0 ) THEN
               DO kk = 1 , num
                  DO k = 1 , numlef
                     A(index2+kk-1+(k-1)*numinc) = 0.
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         IF ( iblkd(8)<1 ) THEN
            CALL dsskrc
            CALL dssdcb
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         RETURN
      CASE (8)
         RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE unpack
