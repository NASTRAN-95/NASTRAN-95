
SUBROUTINE order(Gplst,Id,Rest,Grids,Idtab,Lcor,B1,B2,B3)
   IMPLICIT NONE
   INTEGER Ecpt , Est , Newoes , Ngp , Scr2 , Scr4 , Sil
   REAL Oes1 , Scr1 , Skip1(3) , Skip2(5) , Skp(11)
   COMMON /blank / Ngp , Skp , Est , Skip1 , Sil , Skip2 , Ecpt , Oes1 , Scr1 , Scr2 , Newoes , Scr4
   INTEGER B1 , B2 , B3 , Lcor
   INTEGER Gplst(1) , Grids(1) , Id(1) , Idtab(2) , Rest(2)
   INTEGER elid , hold(3) , i , iafter , iall , ibefor , ieor , iflag , igdpt , igrd(2) , index , ione , iq , isym(14) , itwo ,     &
         & itype(14) , j , jspill , k , kbar , kq4 , kt3 , lastng , lcorx , length , lgrids , lidtab , m , nelmt , newin , newout , &
         & ngppe , ntype , offset , sils(34) , three(3) , tp
   LOGICAL spill
!
   EQUIVALENCE (three(1),iflag) , (three(2),nelmt) , (three(3),igdpt)
   EQUIVALENCE (kq4,isym(13)) , (kt3,isym(14))
   DATA isym/2HSH , 2HT1 , 2HTB , 2HTP , 2HTM , 2HQP , 2HQM , 2HT2 , 2HQ2 , 2HQ1 , 2HM1 , 2HM2 , 2HQ4 , 2HT3/ , kbar/2HBR/
   DATA itype/4 , 6 , 7 , 8 , 9 , 15 , 16 , 17 , 18 , 19 , 62 , 63 , 64 , 83/
   DATA ntype/14/
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
 100  CALL read(*2000,*400,Est,tp,1,0,m)
   offset = 0
   IF ( tp==kbar ) offset = 6
   IF ( tp==kt3 .OR. tp==kq4 ) offset = 1
 200  CALL fread(Est,ngppe,1,0)
   Idtab(i-1) = ngppe
!
!     SKIP PAST THE NON-CONTOUR ELEMENTS
!
   DO k = 1 , ntype
      IF ( tp==isym(k) ) GOTO 300
   ENDDO
   DO
      CALL fread(Est,elid,1,0)
      IF ( elid==0 ) GOTO 100
      j = 1 + ngppe + offset
      CALL fread(Est,0,-j,0)
   ENDDO
 300  DO
!
!     CONSTRUCT IDTAB  1. 0, 2.NGPPE, 3.ELID, 4.ELIDPTR, 5.REPEAT.  3,4
!     FOR ALL ELEMENTS OF THIS TYPE, 6.REPEAT 1-5 FOR ALL ELEMENTS IN
!     THE SET. CONSTRUCT GRIDS  1-NGPPE. GRIDS FOR 1ST ELEMENT, NEXT.
!     REPEAT 1ST FOR ALL ELEMENTS IN THE IDTAB
!
      CALL read(*400,*400,Est,Idtab(i),2,0,m)
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
            GOTO 500
         ENDIF
      ELSE
!
!     END OF ELEMENTS OF THIS TYPE
!
         tp = Idtab(i-1)
         GOTO 200
      ENDIF
   ENDDO
!
!     TABLE FIT INTO CORE
!
 400  CALL bckrec(Est)
!
!     END OF TABLE
!
 500  lidtab = i - 1
   IF ( lidtab<=2 ) THEN
      IF ( jspill==1 ) GOTO 2000
      IF ( jspill==2 ) GOTO 2100
   ENDIF
   lgrids = j - 1
   lastng = ngppe
   IF ( jspill==2 ) GOTO 2100
   CALL open(*2000,Ecpt,Gplst(B2),0)
   CALL gopen(Scr2,Gplst(B1),1)
   CALL fwdrec(*1800,Ecpt)
   igdpt = 0
 600  DO
      igdpt = igdpt + 1
      ieor = 0
      IF ( Gplst(igdpt)/=0 ) EXIT
      CALL fwdrec(*1800,Ecpt)
   ENDDO
 700  nelmt = 0
   iflag = -1
!
!      ECPT--1. PIVOT POINT, 2. DEG.FREEDOM, 3. -LENGTH, 4. ELID POINTER
!      5. ELTYPE, 6.SILS (THERE ARE (LENGTH-2) OF THEM), 7. REPEAT ITEMS
!      (3-6) FOR ALL ELEMENTS ATTACHED TO PIVOT, 8. EOR, 9. REPEAT ITEMS
!      (1-8) FOR ALL GRIDS IN THE PROBLEM.
!
   CALL read(*1800,*1800,Ecpt,igrd,2,0,m)
 800  DO
      CALL read(*1800,*1200,Ecpt,length,1,0,m)
      CALL fread(Ecpt,sils,-length,0)
      tp = sils(2)
      DO i = 1 , ntype
         IF ( tp==itype(i) ) GOTO 900
      ENDDO
   ENDDO
!
!     MATCH ELIDPTR WITH ITS ELID AND GRID POINTS IF POSSIBLE
!
 900  j = 1
   DO i = 1 , lidtab , 2
      IF ( Idtab(i)/=0 ) THEN
         IF ( Idtab(i+1)==sils(1) ) GOTO 1000
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
      IF ( jspill==1 ) GOTO 800
      IF ( jspill==2 ) GOTO 2200
   ENDIF
   elid = -sils(1)
   nelmt = nelmt + 1
   GOTO 1100
!
!     FOUND ELEMENT IN THE TABLE
!
 1000 elid = Idtab(i)
   DO i = 1 , ngppe
      k = j + i - 1
      IF ( igdpt==Grids(k) ) EXIT
   ENDDO
   iafter = i - (i/ngppe)*ngppe + j
   ibefor = j + i - 2
   IF ( i==1 ) ibefor = ibefor + ngppe
   nelmt = nelmt + 1
   Rest(2*nelmt-1) = Grids(iafter)
   Rest(2*nelmt) = Grids(ibefor)
 1100 Id(nelmt) = elid
   IF ( nelmt<Lcor/2 ) THEN
      IF ( jspill==1 ) GOTO 800
      IF ( jspill==2 ) GOTO 2200
   ENDIF
   GOTO 1300
 1200 IF ( nelmt==0 ) THEN
      IF ( jspill==1 ) GOTO 600
      IF ( jspill==2 ) GOTO 2100
   ENDIF
   ieor = 1
!
!     ORDER ELEMENTS IF WE HAVE REACHED END OF EST FILE
!
 1300 IF ( spill ) GOTO 1700
   IF ( nelmt<=2 ) THEN
!
!     BORDER ELEMENTS
!
      iflag = -2
      GOTO 1700
   ELSE
      index = 3
      iall = 2*nelmt
      ione = Rest(1)
      itwo = Rest(2)
   ENDIF
 1400 IF ( ione==itwo ) GOTO 1600
   DO i = index , iall , 2
      IF ( itwo==Rest(i) ) GOTO 1500
   ENDDO
   iflag = -2
   GOTO 1700
 1500 IF ( i/=index ) THEN
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
   IF ( index<iall ) GOTO 1400
   IF ( ione/=itwo ) THEN
      iflag = -2
      GOTO 1700
   ENDIF
!
!     INTERIOR ELEMENTS
!
 1600 CALL write(newout,three,3,0)
   CALL write(newout,Id,nelmt,1)
   IF ( igdpt<Ngp ) THEN
      IF ( jspill==1 ) GOTO 600
      IF ( jspill==2 ) GOTO 2100
   ENDIF
   GOTO 1800
 1700 CALL write(newout,three,3,0)
   j = -1
   DO i = 1 , nelmt
      j = j + 2
      CALL write(newout,Id(i),1,0)
      CALL write(newout,Rest(j),2,0)
   ENDDO
   iq = 2*nelmt
   CALL write(newout,0,0,1)
   IF ( jspill==2 ) GOTO 2100
   IF ( ieor<=0 ) GOTO 700
   IF ( igdpt<Ngp ) GOTO 600
 1800 CALL close(Ecpt,1)
 1900 CALL write(newout,0,1,1)
   CALL close(newout,1)
!
!     IF NO SPILL -  RETURN
!
 2000 IF ( .NOT.spill ) THEN
!
!     OUTPUT FILE MUST BE SCRATCH 2
!
      IF ( newout==Scr2 ) RETURN
      CALL gopen(newout,Gplst(B1),0)
      CALL gopen(Scr2,Gplst(B2),1)
      CALL cpyfil(newout,Scr2,Rest,Lcor,m)
      CALL close(Scr2,1)
      CALL close(newout,1)
      GOTO 99999
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
      GOTO 300
   ENDIF
!
!     TABLE CONSTRUCTED SO RETURN HERE
!
 2100 CALL read(*2300,*2300,newin,three,3,0,m)
   nelmt = 0
 2200 CALL read(*2300,*1200,newin,sils(1),3,0,m)
   IF ( sils(1)<=0 ) THEN
      sils(1) = -sils(1)
      GOTO 900
   ELSE
      elid = sils(1)
      nelmt = nelmt + 1
      Rest(2*nelmt-1) = sils(2)
      Rest(2*nelmt) = sils(3)
      GOTO 1100
   ENDIF
!
!     END OF FILE
!
 2300 CALL close(newin,1)
   GOTO 1900
99999 RETURN
END SUBROUTINE order
