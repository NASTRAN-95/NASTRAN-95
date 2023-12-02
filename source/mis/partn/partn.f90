!*==partn.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE partn(Irp,Icp,Core)
   USE c_parmeg
   USE c_system
   USE c_two
   USE c_zntpkx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Irp
   INTEGER , DIMENSION(1) :: Icp
   INTEGER , DIMENSION(1) :: Core
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(40) :: block1
   REAL , DIMENSION(2) :: head
   INTEGER :: i , ibuf , ibufcp , ibufrp , inorp , iopen , iotp , ipm1 , ipm2 , ipos , itemp , j , k , km , l , l1 , lcore , loop , &
            & m , m1
   INTEGER , DIMENSION(7,4) :: ias
   INTEGER , SAVE :: iln
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: ocpct , orpct , zcpct , zrpct
   EXTERNAL andf , bldpk , bldpki , bldpkn , close , fname , gopen , intpk , mesage , open , rshift , ruler , skprec , write ,      &
          & zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (Ias(1,1),Ia11(1))
   DATA iln/20/ , name/4HPART , 4HN   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     ZERO 6 AND 7 OF OUTPUT BLOCKS
!
         iotp = itypa
         iopen = 0
         DO i = 1 , 4
            DO j = 6 , 7
               ias(j,i) = 0
            ENDDO
            IF ( ias(1,i)/=0 ) THEN
               IF ( ias(5,i)/=itypa ) iotp = 4
               iopen = iopen + 1
               DO j = 2 , 5
                  IF ( ias(j,i)<=0 ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
               ias(2,i) = 0
            ENDIF
         ENDDO
         lcore = lcare
         ibuf = lcore - sysbuf + 1
         ibufcp = ibuf - nrowa
         ibufrp = ibufcp - (ncola+31)/32
         IF ( ibufrp<=0 ) THEN
!
            ipm1 = -8
            CALL mesage(ipm1,ipm2,name)
         ELSE
            lcore = ibufrp - 1
            inorp = 0
            CALL ruler(rule,Icp,zcpct,ocpct,Core(ibufcp),nrowa,Core(ibuf),1)
            IF ( Irp(1)==Icp(1) .AND. Irp(1)/=0 .AND. nrowa==ncola ) THEN
               inorp = 1
               lcore = ibufcp - 1
            ELSE
               CALL ruler(rule,Irp,zrpct,orpct,Core(ibufrp),ncola,Core(ibuf),0)
            ENDIF
!
!     OPEN OUTPUT MATRICES
!
            IF ( iopen*sysbuf>lcore ) THEN
               ipm1 = -8
               CALL mesage(ipm1,ipm2,name)
            ELSE
               DO i = 1 , 4
                  IF ( ias(1,i)/=0 ) THEN
                     lcore = lcore - sysbuf
                     CALL open(*2,ias(1,i),Core(lcore+1),1)
                     CALL fname(ias(1,i),head)
                     CALL write(ias(1,i),head,2,1)
                  ENDIF
                  CYCLE
 2                ias(1,i) = 0
               ENDDO
!
!     OPEN INPUT MATRIX
!
               CALL gopen(namea,Core(ibuf),0)
!
!     LOOP FOR EACH COLUMN
!
               km = 0
               DO loop = 1 , ncola
                  IF ( inorp/=0 ) THEN
                     l = ibufcp + loop - 1
                     IF ( Core(l)<0 ) THEN
                        l1 = 0
                     ELSE
                        l1 = 2
                     ENDIF
                  ELSE
!
!     COLUMN PARTITION A SEQ. OF ZEROS AND ONES
!
                     km = km + 1
                     IF ( km>32 ) km = 1
                     l = ibufrp + (loop-1)/32
                     itemp = andf(Core(l),two1(km))
                     IF ( km==1 ) itemp = rshift(andf(Core(l),two1(km)),1)
                     IF ( itemp/=0 ) THEN
                        l1 = 2
                     ELSE
                        l1 = 0
                     ENDIF
                  ENDIF
!
!     BEGIN BLDPK ON TWO SUBS
!
                  j = 0
                  DO l = 1 , 2
                     k = l1 + l
                     m = iln*(l-1) + 1
                     IF ( ias(1,k)/=0 ) THEN
                        CALL bldpk(iotp,ias(5,k),ias(1,k),block1(m),1)
                        j = j + 1
                     ENDIF
                  ENDDO
                  IF ( j/=0 ) THEN
!
!     SEARCH COLUMN FOR NON-ZERO ELEMENTS
!
                     CALL intpk(*4,namea,0,iotp,0)
!
!     LOOP FOR ROWS WITHIN COLUMN
!
                     DO WHILE ( ieol==0 )
                        CALL zntpki
!
!     COMPUTE ROW POSITION AND OUTPUT MATRIX
!
                        l = ibufcp + ii - 1
                        ipos = iabs(Core(l))
                        IF ( Core(l)<0 ) THEN
                           m1 = l1 + 1
                           m = 1
                        ELSE
                           m1 = l1 + 2
                           m = iln + 1
                        ENDIF
                        IF ( ias(1,m1)/=0 ) CALL bldpki(a11(1),ipos,ias(1,m1),block1(m))
                     ENDDO
                  ELSE
                     CALL skprec(namea,1)
                     CYCLE
                  ENDIF
 4                DO l = 1 , 2
                     k = l + l1
                     m = iln*(l-1) + 1
                     IF ( ias(1,k)/=0 ) CALL bldpkn(ias(1,k),block1(m),ias(1,k))
                  ENDDO
               ENDDO
!
!     ALL DONE - CLOSE OPEN MATRICES
!
               CALL close(namea,1)
               DO i = 1 , 4
                  IF ( ias(1,i)/=0 ) CALL close(ias(1,i),1)
               ENDDO
               RETURN
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         DO
            ipm1 = -7
            CALL mesage(ipm1,ipm2,name)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE partn
