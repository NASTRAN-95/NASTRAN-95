!*==merge.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE merge(Irp,Icp,Core)
!
!     MERGE WILL PUT UP TO 4 MATRICES, IA11,IA21,IA12,IA22, TOGETHER
!     INTO NAMEA -- THIS ROUTINE IS THE INVERSE OF PARTN
!
!     THE ARGUMENTS ARE EXACTLY THE SAME IN MEANING AND OPTION AS FOR
!     PARTITION
!
   USE c_parmeg
   USE c_system
   USE c_two
   USE c_zblpkx
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
   INTEGER , DIMENSION(4) :: a11 , b11
   INTEGER , DIMENSION(40) :: block
   INTEGER :: i , iaz , ibuf , ibufcp , ibufrp , ibz , ieol , io , iotp , ipos , irew , istor , itemp , j , jeol , jpos , k , km ,  &
            & l , l1 , l2 , l3 , lcore , loop , m , mn , nam1 , nam2 , ncola1 , nm , nmat , ntypa , ocpct , orpct , zcpct , zrpct
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL andf , bldpk , bldpkn , close , gopen , intpk , intpki , mesage , open , rshift , ruler , skprec , zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   DATA name/4HMERG , 4HE   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CHECK FILES
!
         lcore = iabs(lcare)
         k = namea
         DO i = 1 , 4
            IF ( k/=0 ) THEN
               DO j = i , 4
                  IF ( ia11(1,j)==k ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ENDIF
            k = ia11(1,i)
         ENDDO
!
!     PICK UP PARAMETERS AND INITIALIZE
!
         irew = 0
         IF ( lcare<0 ) irew = 2
         ncola1 = ncola
         ncola = 0
         ia(1) = 0
         ia(2) = 0
         istor = 0
         iotp = itypa
         nmat = 0
         DO i = 1 , 4
            IF ( ia11(1,i)>0 ) THEN
!WKBD 2/94 SPR93025 IF (IA11(5,I) .NE. ITYPA) IOTP = 4
               nmat = nmat + 1
               DO j = 2 , 5
                  IF ( ia11(j,i)==0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         ntypa = iotp
         IF ( ntypa==3 ) ntypa = 2
         ibuf = lcore - sysbuf + 1
         ibufcp = ibuf - nrowa
         IF ( ibufcp<=0 ) THEN
!
            mn = -8
            spag_nextblock_1 = 4
         ELSE
            lcore = ibufcp - 1
            CALL ruler(rule,Icp,zcpct,ocpct,Core(ibufcp),nrowa,Core(ibuf),1)
            IF ( Irp(1)==Icp(1) .AND. Irp(1)/=0 ) THEN
               istor = 1
            ELSE
               ibufrp = ibufcp - (ncola1+31)/32
               IF ( ibufrp<=0 ) THEN
                  mn = -8
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  CALL ruler(rule,Irp,zrpct,orpct,Core(ibufrp),ncola1,Core(ibuf),0)
                  lcore = ibufrp - 1
               ENDIF
            ENDIF
!
!     OPEN INPUT FILES
!
            IF ( lcore<nmat*sysbuf ) THEN
               mn = -8
               spag_nextblock_1 = 4
            ELSE
               DO i = 1 , 4
                  IF ( ia11(1,i)>=0 ) THEN
                     IF ( ia11(1,i)/=0 ) THEN
                        lcore = lcore - sysbuf
                        CALL open(*2,ia11(1,i),Core(lcore+1),irew)
                        CALL skprec(ia11(1,i),1)
                     ENDIF
                     CYCLE
                  ENDIF
 2                ia11(1,i) = 0
               ENDDO
!
!     OPEN OUTPUT FILE
!
               CALL gopen(namea,Core(ibuf),1)
!
!     FIX POINTERS -- SORT ON ABS VALUE
!
               k = ibufcp - 1
               l = ibufcp
               DO i = 1 , nrowa
                  k = k + 1
                  IF ( Core(k)<0 ) THEN
                     Core(l) = i
                     l = l + 1
                  ENDIF
               ENDDO
               m = l - 1
               k = ibufcp
               SPAG_Loop_1_2: DO i = 1 , nrowa
                  SPAG_Loop_2_1: DO
                     IF ( Core(k)<i ) THEN
                        IF ( k==m ) EXIT SPAG_Loop_2_1
                        k = k + 1
                     ELSEIF ( Core(k)==i ) THEN
                        CYCLE SPAG_Loop_1_2
                     ELSE
                        EXIT SPAG_Loop_2_1
                     ENDIF
                  ENDDO SPAG_Loop_2_1
                  Core(l) = i
                  l = l + 1
               ENDDO SPAG_Loop_1_2
!
!     LOOP ON COLUMNS OF OUTPUT
!
               km = 0
               l2 = ibufcp
               l3 = ibufcp + zcpct
               DO loop = 1 , ncola1
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        CALL bldpk(iotp,itypa,namea,0,0)
                        IF ( istor/=1 ) THEN
                           j = (loop-1)/32 + ibufrp
                           km = km + 1
                           IF ( km>32 ) km = 1
                           itemp = andf(Core(j),two1(km))
                           IF ( km==1 ) itemp = rshift(andf(Core(j),two1(km)),1)
                           IF ( itemp/=0 ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
!
!     USE ROW STORE
!
                        ELSEIF ( Core(l2)/=loop ) THEN
                           IF ( Core(l3)==loop ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
!
!     IA11 AND IA21 BEING USED
!
                        l1 = 0
                        IF ( l2/=l3-1 ) l2 = l2 + 1
                        spag_nextblock_2 = 3
                     CASE (2)
!
!     IA12 AND IA22 BEING USED
!
                        l1 = 2
                        l3 = l3 + 1
                        spag_nextblock_2 = 3
                     CASE (3)
!
!     BEGIN ON SUBMATRICES
!
                        io = 0
                        DO j = 1 , 2
                           k = l1 + j
                           IF ( ia11(1,k)/=0 ) THEN
                              m = 20*j - 19
                              CALL intpk(*4,ia11(1,k),block(m),iotp,1)
                              io = io + j
                           ENDIF
 4                      ENDDO
                        IF ( io==0 ) THEN
                           spag_nextblock_2 = 9
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
!
!     PICK UP NON ZERO
!
                        ieol = 0
                        jeol = 0
                        ipos = 9999999
                        jpos = 9999999
                        iaz = 1
                        ibz = 1
                        nam1 = ia11(1,l1+1)
                        nam2 = ia11(1,l1+2)
                        IF ( io/=2 ) THEN
                           iaz = 0
                        ELSE
                           ibz = 0
                           spag_nextblock_2 = 5
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        spag_nextblock_2 = 4
                     CASE (4)
                        IF ( ieol/=0 ) THEN
                           iaz = 1
                           ipos = 9999999
                           IF ( iaz+ibz/=2 ) THEN
                              spag_nextblock_2 = 7
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                           spag_nextblock_2 = 9
                           CYCLE SPAG_DispatchLoop_2
                        ELSE
                           CALL intpki(a11(1),i,nam1,block(1),ieol)
                           k = ibufcp + i - 1
                           ipos = Core(k)
                           IF ( io==1 ) THEN
                              spag_nextblock_2 = 6
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                           io = 1
                           ibz = 0
                        ENDIF
                        spag_nextblock_2 = 5
                     CASE (5)
                        IF ( jeol/=0 ) THEN
                           jpos = 9999999
                           ibz = 1
                           IF ( iaz+ibz==2 ) THEN
                              spag_nextblock_2 = 9
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                           spag_nextblock_2 = 8
                           CYCLE SPAG_DispatchLoop_2
                        ELSE
                           CALL intpki(b11(1),j,nam2,block(21),jeol)
                           k = ibufcp + zcpct + j - 1
                           jpos = Core(k)
                        ENDIF
                        spag_nextblock_2 = 6
                     CASE (6)
                        IF ( ipos<jpos ) THEN
                           spag_nextblock_2 = 8
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        spag_nextblock_2 = 7
                     CASE (7)
!
!     PUT IN B11
!
                        DO m = 1 , ntypa
                           ic11(m) = b11(m)
                        ENDDO
                        ii = jpos
                        CALL zblpki
                        spag_nextblock_2 = 5
                     CASE (8)
                        DO m = 1 , ntypa
                           ic11(m) = a11(m)
                        ENDDO
                        ii = ipos
                        CALL zblpki
                        spag_nextblock_2 = 4
                     CASE (9)
!
!     OUTPUT COLUMN
!
                        CALL bldpkn(namea,0,namea)
                        EXIT SPAG_DispatchLoop_2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
!
               ENDDO
!
!     DONE -- CLOSE OPEN MATRICES
!
               DO i = 1 , 4
                  IF ( ia11(1,i)>0 ) CALL close(ia11(1,i),1)
               ENDDO
               CALL close(namea,1)
               RETURN
            ENDIF
         ENDIF
      CASE (2)
         WRITE (nout,99001) k
99001    FORMAT ('0*** SYSTEM OR USER ERROR, DUPLICATE GINO FILES AS ','DETECTED BY MERGE ROUTINE - ',I5)
         nm = -37
         spag_nextblock_1 = 4
      CASE (3)
         mn = -7
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(mn,0,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE merge
