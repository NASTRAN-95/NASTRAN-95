!*==elelbl.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE elelbl(Gplst,X,U,Deform,Buf1)
   IMPLICIT NONE
   USE C_BLANK
   USE C_CHAR94
   USE C_DRWDAT
   USE C_GPTA1
   USE C_NAMES
   USE C_PLTDAT
   USE C_PLTSCR
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Gplst
   REAL , DIMENSION(3,1) :: X
   REAL , DIMENSION(2,1) :: U
   INTEGER :: Deform
   INTEGER :: Buf1
!
! Local variable declarations rewritten by SPAG
!
   REAL :: absslp , b , ba , bb , cc , cntx , cnty , len , ma , maxlen , mb , minslp , slp , xc , xx , yc , yy , zz
   INTEGER , SAVE :: blank , br , hb , iect , itetra , pid , q4 , t3
   INTEGER :: elid , eltype , i , j , jtj , k , lpid , ngpel , ngpel1 , nl , np , npid , offset , twod
   INTEGER , DIMENSION(2) :: elidp
   REAL , SAVE :: infnty , slpmax
   INTEGER , DIMENSION(10) :: lbl
   INTEGER , DIMENSION(8) :: lblp
   LOGICAL :: solid
   EXTERNAL close , delset , fname , fread , fwdrec , khrfn1 , locate , preloc , read , tipe
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!
   DATA blank , infnty , slpmax/1H  , 1.E3 , 5./ , pid/4/ , itetra/2HTE/ , iect/4HECT / , hb/2HHB/ , br/2HBR/ , q4/2HQ4/ , t3/2HT3/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         np = 0
         CALL tipe(0,0,0,0,0,-1)
         cntx = Cntchr(1)
         cnty = Cntchr(1) + (Cntchr(2)-Cntchr(1))/2.
!
!   . CHECK IF PROPERTY ID IS TO BE TYPED NEXT TO ELEMENT ID
!
         IF ( Plabel/=pid ) GOTO 40
         IF ( Pltflg>=0 ) THEN
            CALL preloc(*20,Gplst(Buf1),Ect)
            CALL fname(Ect,Gpts(1))
            IF ( Gpts(1)==iect ) THEN
               CALL delset
               lpid = 0
!
!   . READ THE ELEMENT TYPE + NUMBER OF GRID POINTS / ELEMENT OF THIS
!     TYPE.
!
               IF ( lpid>0 ) CALL fwdrec(*40,Ect)
               GOTO 40
            ELSE
               CALL close(Ect,Crew)
            ENDIF
         ENDIF
 20      Plabel = pid - 1
 40      SPAG_Loop_1_1: DO
            CALL read(*100,*100,Elsets,eltype,1,0,i)
            CALL fread(Elsets,ngpel,1,0)
            twod = 0
            IF ( ngpel>2 ) twod = 1
            ngpel = iabs(ngpel)
            solid = .FALSE.
            IF ( (eltype==itetra .OR. ngpel>4) .AND. eltype/=hb ) solid = .TRUE.
!-----
!   . REJECT ELEMENTS WITH 0 OR MORE THAN --NCOR-16-- GRID POINTS
!
            IF ( ngpel>1 .AND. ngpel<Ncor-13 ) THEN
!-----
               IF ( Plabel==pid ) THEN
                  j = 16
                  DO i = 1 , Ntyps
                     IF ( Ne(j)==eltype ) EXIT SPAG_Loop_1_1
                     j = j + Incr
                  ENDDO
               ENDIF
               GOTO 60
            ELSE
               SPAG_Loop_2_2: DO
                  CALL fread(Elsets,elid,1,0)
                  IF ( elid<=0 ) EXIT SPAG_Loop_2_2
                  CALL fread(Elsets,0,-1,0)
                  CALL fread(Elsets,0,-ngpel,0)
               ENDDO SPAG_Loop_2_2
            ENDIF
         ENDDO SPAG_Loop_1_1
         lpid = j - 12
         IF ( Ne(lpid+2)>0 ) THEN
            npid = Ne(lpid+2)
            CALL locate(*60,Gplst(Buf1),Ne(lpid),Gpts(1))
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 60      lpid = 0
         spag_nextblock_1 = 2
      CASE (2)
!
         ngpel1 = ngpel + 1
         offset = 0
         IF ( eltype==br ) offset = 6
         IF ( eltype==q4 .OR. eltype==t3 ) offset = 1
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ AN ELEMENT ID + ITS GRID POINTS.
!
         CALL fread(Elsets,elid,1,0)
         IF ( eltype==hb ) ngpel = 8
         IF ( elid<=0 ) THEN
            IF ( lpid>0 ) CALL fwdrec(*40,Ect)
            GOTO 40
         ELSE
            CALL fread(Elsets,0,-1,0)
            CALL fread(Elsets,Gpts(1),ngpel,0)
            IF ( offset>0 ) CALL fread(Elsets,0,-offset,0)
            IF ( eltype==hb ) THEN
               DO i = 2 , 4
                  IF ( Gpts(i)==0 ) GOTO 65
               ENDDO
               i = 5
 65            ngpel = i - 1
            ENDIF
            k = elid
            nl = 0
            DO i = 1 , 8
               j = elid/10**(8-i)
               IF ( j/=0 .OR. nl/=0 ) THEN
                  nl = nl + 1
                  lbl(nl) = Chr(j+1)
                  elid = elid - j*10**(8-i)
               ENDIF
            ENDDO
            lbl(nl+1) = khrfn1(blank,1,eltype,1)
            lbl(nl+2) = khrfn1(blank,1,eltype,2)
            nl = nl + 2
!
!   . DECODE PROPERTY ID
!
            IF ( lpid<=0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO
               CALL read(*80,*80,Ect,elidp,2,0,i)
               CALL fread(Ect,0,-(npid-2),0)
               IF ( elidp(1)==k ) THEN
!
!   . ELEMENT PROPERTY FOUND
!
                  k = 10000000
                  np = 0
                  DO i = 1 , 8
                     j = elidp(2)/k
                     IF ( j/=0 .OR. np/=0 ) THEN
                        np = np + 1
                        lblp(np) = Chr(j+1)
                        elidp(2) = elidp(2) - j*k
                     ENDIF
                     k = k/10
                  ENDDO
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
 80      lpid = -1
         spag_nextblock_1 = 4
      CASE (4)
!
!
!   . SET UP THE COORDINATES OF THE GRID POINTS
!
         DO i = 1 , ngpel
            j = Gpts(i)
            j = iabs(Gplst(j))
            IF ( Deform/=0 ) THEN
               xx = U(1,j)
               yy = U(2,j)
            ELSE
               xx = X(2,j)
               yy = X(3,j)
            ENDIF
            IF ( solid ) THEN
               IF ( i<=2 ) THEN
                  Xy(1,i) = xx
                  Xy(2,i) = yy
                  IF ( i==1 ) THEN
                     Xy(1,3) = 0.0
                     Xy(2,3) = 0.0
                  ENDIF
               ENDIF
               Xy(1,3) = xx + Xy(1,3)
               Xy(2,3) = yy + Xy(2,3)
            ELSE
               Xy(1,i) = xx
               Xy(2,i) = yy
               j = ngpel + i
               Xy(1,j) = xx
               Xy(2,j) = yy
            ENDIF
         ENDDO
!
         IF ( solid ) THEN
!
!   . ELEMENTS WITH MORE THAN FOUR GRIDS
!
            xc = Xy(1,3)/float(ngpel)
            yc = Xy(2,3)/float(ngpel)
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( twod/=0 ) THEN
!
!     FIND THE BASE OF THIS POLYGON = LONGEST SIDE (IF MORE THAN ONE
!     LONGEST SIDE, CHOOSE FROM THEM THE SIDE OF SMALLEST SLOPE).
!
               maxlen = 0.
               DO i = 1 , ngpel
                  xx = Xy(1,i+1) - Xy(1,i)
                  yy = Xy(2,i+1) - Xy(2,i)
                  len = xx**2 + yy**2
                  IF ( xx/=0. ) THEN
                     slp = abs(yy/xx)
                  ELSE
                     slp = infnty
                  ENDIF
                  IF ( maxlen<len ) THEN
                     maxlen = len
                  ELSEIF ( maxlen==len ) THEN
                     IF ( slp>=minslp ) CYCLE
                  ELSE
                     CYCLE
                  ENDIF
                  k = i
                  minslp = slp
               ENDDO
!
               IF ( k==1 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               IF ( ngpel==2 ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               k = 3
            ENDIF
            DO i = 1 , ngpel1
               Xy(1,i) = Xy(1,k)
               Xy(2,i) = Xy(2,k)
               k = k + 1
            ENDDO
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         IF ( ngpel==6 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ngpel<3 ) THEN
         ELSEIF ( ngpel==3 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     QUADRILATERAL ELEMENT.
!
            xx = (Xy(1,3)+Xy(1,4)) - (Xy(1,1)+Xy(1,2))
            IF ( xx/=0. ) THEN
               yy = (Xy(2,3)+Xy(2,4)) - (Xy(2,1)+Xy(2,2))
               ma = yy/xx
               ba = (Xy(2,1)+Xy(2,2))/2. - ma*(Xy(1,1)+Xy(1,2))/2.
            ELSE
               ma = infnty
            ENDIF
            xx = (Xy(1,2)+Xy(1,3)) - (Xy(1,1)+Xy(1,4))
            IF ( xx/=0. ) THEN
               yy = (Xy(2,2)+Xy(2,3)) - (Xy(2,1)+Xy(2,4))
               mb = yy/xx
               bb = (Xy(2,1)+Xy(2,4))/2. - mb*(Xy(1,1)+Xy(1,4))/2.
            ELSE
               mb = infnty
            ENDIF
!
            IF ( abs(ma)>=infnty ) THEN
               xc = (Xy(1,1)+Xy(1,2))/2.
               yc = mb*xc + bb
            ELSEIF ( abs(mb)>=infnty ) THEN
               xc = (Xy(1,1)+Xy(1,4))/2.
               yc = ma*xc + ba
            ELSEIF ( mb==ma ) THEN
               xc = (Xy(1,3)+Xy(1,4)+Xy(1,2)+Xy(1,1))/4.0
               yc = (Xy(2,3)+Xy(2,4)+Xy(2,2)+Xy(2,1))/4.0
            ELSE
               xc = (ba-bb)/(mb-ma)
               yc = ma*xc + ba
            ENDIF
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     LINE ELEMENT.
!
         xx = Xy(1,2) - Xy(1,1)
         IF ( xx==0. ) THEN
            slp = infnty
         ELSE
            yy = Xy(2,2) - Xy(2,1)
            slp = yy/xx
         ENDIF
         xc = (Xy(1,1)+Xy(1,2))/2.
         yc = (Xy(2,1)+Xy(2,2))/2.
!
         IF ( abs(slp)<=1. ) THEN
            yc = yc + cnty
         ELSEIF ( abs(slp)<slpmax ) THEN
            xc = xc - sign(cntx,slp)
         ELSE
            xc = xc + cntx
         ENDIF
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
!
!     TRIANGULAR ELEMENT.  POINTS 1+2 ARE THE BASE - POINT 3 THE APEX.
!
         xc = (Xy(1,1)+Xy(1,2)+Xy(1,3))/3.
         yc = (Xy(2,1)+Xy(2,2)+Xy(2,3))/3.
         spag_nextblock_1 = 8
      CASE (8)
!
!     SETUP THE STRAIGHT LINE EQUATION OF THE LINE ON WHICH THE ELEMENT
!     LABEL IS TO BE TYPED - Y=MX+B.
!
         xx = Xy(1,2) - Xy(1,1)
         IF ( xx==0. ) THEN
            slp = infnty
         ELSE
            yy = Xy(2,2) - Xy(2,1)
            slp = yy/xx
            b = yc - xc*slp
         ENDIF
!
!     TYPE THE ELEMENT LABEL (NL CHARACTERS)
!
         zz = nl/2
         IF ( nl/2==(nl+1)/2 ) zz = zz - .5
         absslp = abs(slp)
         cc = cntx
         IF ( absslp>=slpmax ) cc = cnty
         k = max0(nl,np)
!
         DO i = 1 , k
            xx = cc*(zz-float(i-1))
            IF ( absslp>1. ) THEN
               IF ( absslp>=slpmax ) THEN
                  yy = -1.
               ELSE
                  yy = sign(1.,slp)
               ENDIF
               yy = yc - yy*xx
               IF ( absslp>=infnty ) THEN
                  xx = xc
               ELSE
                  xx = (yy-b)/slp
               ENDIF
            ELSE
               xx = xc - xx
               yy = slp*xx + b
            ENDIF
!
!
!     OFFSET THE HB LABEL AND PROPERTY ID IF ANY WHEN TIPE LABEL
!
            IF ( eltype==hb ) THEN
               jtj = 2
               IF ( absslp<slpmax ) yy = yy - jtj*cc
               IF ( absslp>=slpmax ) xx = xx + jtj*cc
            ENDIF
            IF ( nl>=i ) CALL tipe(xx,yy,1,lbl(i),1,0)
            IF ( lpid>0 ) THEN
               IF ( np>=i ) THEN
                  IF ( absslp<slpmax ) yy = yy - 2.*cc
                  IF ( absslp>=slpmax ) xx = xx + 2.*cc
                  CALL tipe(xx,yy,1,lblp(i),1,0)
               ENDIF
            ENDIF
         ENDDO
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
 100     CALL tipe(0,0,0,0,0,1)
         IF ( Plabel==pid ) CALL close(Ect,Crew)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE elelbl
