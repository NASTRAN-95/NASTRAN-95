!*==order.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE order(Gplst,Id,Rest,Grids,Idtab,Lcor,B1,B2,B3)
   IMPLICIT NONE
   USE C_BLANK
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Gplst
   INTEGER , DIMENSION(1) :: Id
   INTEGER , DIMENSION(2) :: Rest
   INTEGER , DIMENSION(1) :: Grids
   INTEGER , DIMENSION(2) :: Idtab
   INTEGER :: Lcor
   INTEGER :: B1
   INTEGER :: B2
   INTEGER :: B3
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: elid , i , iafter , iall , ibefor , ieor , iflag , igdpt , index , ione , iq , itwo , j , jspill , k , kq4 , kt3 ,    &
            & lastng , lcorx , length , lgrids , lidtab , m , nelmt , newin , newout , ngppe , offset , tp
   INTEGER , DIMENSION(3) :: hold , three
   INTEGER , DIMENSION(2) :: igrd
   INTEGER , DIMENSION(14) , SAVE :: isym , itype
   INTEGER , SAVE :: kbar , ntype
   INTEGER , DIMENSION(34) :: sils
   LOGICAL :: spill
   EXTERNAL bckrec , close , cpyfil , fread , fwdrec , gopen , open , read , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (three(1),iflag) , (three(2),nelmt) , (three(3),igdpt)
   !>>>>EQUIVALENCE (kq4,isym(13)) , (kt3,isym(14))
   DATA isym/2HSH , 2HT1 , 2HTB , 2HTP , 2HTM , 2HQP , 2HQM , 2HT2 , 2HQ2 , 2HQ1 , 2HM1 , 2HM2 , 2HQ4 , 2HT3/ , kbar/2HBR/
   DATA itype/4 , 6 , 7 , 8 , 9 , 15 , 16 , 17 , 18 , 19 , 62 , 63 , 64 , 83/
   DATA ntype/14/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     BUILD A TABLE FOR GPECT POINTERS TO ELID AND ITS ORDERED GRID PTS
!
         spill = .FALSE.
         j = 1
         i = 3
         Idtab(1) = 0
         jspill = 1
         lcorx = Lcor
         newin = Scr4
         newout = Scr2
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*80,*20,Est,tp,1,0,m)
         offset = 0
         IF ( tp==kbar ) offset = 6
         IF ( tp==kt3 .OR. tp==kq4 ) offset = 1
         spag_nextblock_1 = 3
      CASE (3)
         CALL fread(Est,ngppe,1,0)
         Idtab(i-1) = ngppe
!
!     SKIP PAST THE NON-CONTOUR ELEMENTS
!
         DO k = 1 , ntype
            IF ( tp==isym(k) ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         DO
            CALL fread(Est,elid,1,0)
            IF ( elid==0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            j = 1 + ngppe + offset
            CALL fread(Est,0,-j,0)
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
         DO
!
!     CONSTRUCT IDTAB  1. 0, 2.NGPPE, 3.ELID, 4.ELIDPTR, 5.REPEAT.  3,4
!     FOR ALL ELEMENTS OF THIS TYPE, 6.REPEAT 1-5 FOR ALL ELEMENTS IN
!     THE SET. CONSTRUCT GRIDS  1-NGPPE. GRIDS FOR 1ST ELEMENT, NEXT.
!     REPEAT 1ST FOR ALL ELEMENTS IN THE IDTAB
!
            CALL read(*20,*20,Est,Idtab(i),2,0,m)
            i = i + 2
            IF ( Idtab(i-2)/=0 ) THEN
!
               CALL fread(Est,Grids(j),ngppe,0)
               IF ( offset/=0 ) CALL fread(Est,0,-offset,0)
               j = j + ngppe
               IF ( i>=lcorx ) THEN
!
!     SPILL OCCURS - TABLE DID NOT FIT
!
                  spill = .TRUE.
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
!
!     END OF ELEMENTS OF THIS TYPE
!
               tp = Idtab(i-1)
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     TABLE FIT INTO CORE
!
 20      CALL bckrec(Est)
         spag_nextblock_1 = 5
      CASE (5)
!
!     END OF TABLE
!
         lidtab = i - 1
         IF ( lidtab<=2 ) THEN
            IF ( jspill==1 ) GOTO 80
            IF ( jspill==2 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         lgrids = j - 1
         lastng = ngppe
         IF ( jspill==2 ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL open(*80,Ecpt,Gplst(B2),0)
         CALL gopen(Scr2,Gplst(B1),1)
         CALL fwdrec(*60,Ecpt)
         igdpt = 0
         spag_nextblock_1 = 6
      CASE (6)
         SPAG_Loop_1_1: DO
            igdpt = igdpt + 1
            ieor = 0
            IF ( Gplst(igdpt)/=0 ) EXIT SPAG_Loop_1_1
            CALL fwdrec(*60,Ecpt)
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 7
      CASE (7)
         nelmt = 0
         iflag = -1
!
!      ECPT--1. PIVOT POINT, 2. DEG.FREEDOM, 3. -LENGTH, 4. ELID POINTER
!      5. ELTYPE, 6.SILS (THERE ARE (LENGTH-2) OF THEM), 7. REPEAT ITEMS
!      (3-6) FOR ALL ELEMENTS ATTACHED TO PIVOT, 8. EOR, 9. REPEAT ITEMS
!      (1-8) FOR ALL GRIDS IN THE PROBLEM.
!
         CALL read(*60,*60,Ecpt,igrd,2,0,m)
         spag_nextblock_1 = 8
      CASE (8)
         SPAG_Loop_1_2: DO
            CALL read(*60,*40,Ecpt,length,1,0,m)
            CALL fread(Ecpt,sils,-length,0)
            tp = sils(2)
            DO i = 1 , ntype
               IF ( tp==itype(i) ) EXIT SPAG_Loop_1_2
            ENDDO
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 9
      CASE (9)
!
!     MATCH ELIDPTR WITH ITS ELID AND GRID POINTS IF POSSIBLE
!
         j = 1
         DO i = 1 , lidtab , 2
            IF ( Idtab(i)/=0 ) THEN
               IF ( Idtab(i+1)==sils(1) ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               j = j + ngppe
            ELSE
               ngppe = Idtab(i+1)
            ENDIF
         ENDDO
!
!     IF NOT IN THE TABLE, IS THERE SPILL(IE IS TABLE NOT COMPLETE).
!     NO SPILL, SKIP HIM.  YES SPILL, FLAG HIM.
!
         IF ( .NOT.spill ) THEN
            IF ( jspill==1 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( jspill==2 ) THEN
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         elid = -sils(1)
         nelmt = nelmt + 1
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
!
!     FOUND ELEMENT IN THE TABLE
!
         elid = Idtab(i)
         SPAG_Loop_1_3: DO i = 1 , ngppe
            k = j + i - 1
            IF ( igdpt==Grids(k) ) EXIT SPAG_Loop_1_3
         ENDDO SPAG_Loop_1_3
         iafter = i - (i/ngppe)*ngppe + j
         ibefor = j + i - 2
         IF ( i==1 ) ibefor = ibefor + ngppe
         nelmt = nelmt + 1
         Rest(2*nelmt-1) = Grids(iafter)
         Rest(2*nelmt) = Grids(ibefor)
         spag_nextblock_1 = 11
      CASE (11)
         Id(nelmt) = elid
         IF ( nelmt<Lcor/2 ) THEN
            IF ( jspill==1 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( jspill==2 ) THEN
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 40      IF ( nelmt==0 ) THEN
            IF ( jspill==1 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( jspill==2 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         ieor = 1
         spag_nextblock_1 = 12
      CASE (12)
!
!     ORDER ELEMENTS IF WE HAVE REACHED END OF EST FILE
!
         IF ( spill ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nelmt<=2 ) THEN
!
!     BORDER ELEMENTS
!
            iflag = -2
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ELSE
            index = 3
            iall = 2*nelmt
            ione = Rest(1)
            itwo = Rest(2)
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
         IF ( ione==itwo ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = index , iall , 2
            IF ( itwo==Rest(i) ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         iflag = -2
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
      CASE (14)
         IF ( i/=index ) THEN
            j = (index+1)/2
            k = (i+1)/2
            hold(1) = Id(j)
            Id(j) = Id(k)
            Id(k) = hold(1)
            hold(2) = Rest(index)
            hold(3) = Rest(index+1)
            Rest(index) = Rest(i)
            Rest(index+1) = Rest(i+1)
            Rest(i) = hold(2)
            Rest(i+1) = hold(3)
         ENDIF
         index = index + 2
         itwo = Rest(index-1)
         IF ( index<iall ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ione/=itwo ) THEN
            iflag = -2
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 15
      CASE (15)
!
!     INTERIOR ELEMENTS
!
         CALL write(newout,three,3,0)
         CALL write(newout,Id,nelmt,1)
         IF ( igdpt<Ngp ) THEN
            IF ( jspill==1 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( jspill==2 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         GOTO 60
      CASE (16)
         CALL write(newout,three,3,0)
         j = -1
         DO i = 1 , nelmt
            j = j + 2
            CALL write(newout,Id(i),1,0)
            CALL write(newout,Rest(j),2,0)
         ENDDO
         iq = 2*nelmt
         CALL write(newout,0,0,1)
         IF ( jspill==2 ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ieor<=0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( igdpt<Ngp ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 60      CALL close(Ecpt,1)
         spag_nextblock_1 = 17
      CASE (17)
         CALL write(newout,0,1,1)
         CALL close(newout,1)
!
!     IF NO SPILL -  RETURN
!
 80      IF ( .NOT.spill ) THEN
!
!     OUTPUT FILE MUST BE SCRATCH 2
!
            IF ( newout==Scr2 ) RETURN
            CALL gopen(newout,Gplst(B1),0)
            CALL gopen(Scr2,Gplst(B2),1)
            CALL cpyfil(newout,Scr2,Rest,Lcor,m)
            CALL close(Scr2,1)
            CALL close(newout,1)
            RETURN
         ELSE
!
!    COME HERE IF WE HAVE SPILL
!
            i = newout
            newout = newin
            newin = i
            CALL gopen(newin,Gplst(B1),0)
            CALL gopen(newout,Gplst(B2),1)
            jspill = 2
            ngppe = lastng
            Idtab(1) = 0
            Idtab(2) = lastng
            i = 3
            j = 1
            spill = .FALSE.
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (18)
!
!     TABLE CONSTRUCTED SO RETURN HERE
!
         CALL read(*100,*100,newin,three,3,0,m)
         nelmt = 0
         spag_nextblock_1 = 19
      CASE (19)
         CALL read(*100,*40,newin,sils(1),3,0,m)
         IF ( sils(1)<=0 ) THEN
            sils(1) = -sils(1)
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSE
            elid = sils(1)
            nelmt = nelmt + 1
            Rest(2*nelmt-1) = sils(2)
            Rest(2*nelmt) = sils(3)
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     END OF FILE
!
 100     CALL close(newin,1)
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE order
