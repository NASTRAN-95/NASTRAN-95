!*==shape.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE shape(Gplst,X,U,Pen,Deform,Iopt,Iptl,Liplt,Opcor) !HIDESTARS (*,Gplst,X,U,Pen,Deform,Iopt,Iptl,Liplt,Opcor)
   IMPLICIT NONE
   USE C_BLANK
   USE C_DRWDAT
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Gplst
   REAL , DIMENSION(3,1) :: X
   REAL , DIMENSION(2,1) :: U
   INTEGER :: Pen
   INTEGER :: Deform
   INTEGER :: Iopt
   INTEGER :: Iptl
   INTEGER , DIMENSION(1) :: Liplt
   INTEGER :: Opcor
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: br , q4 , t3 , te
   INTEGER :: elid , etyp , gp , i , j , l , lgpel , lid , ltyp , m , ngpel , offset
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(6) :: off
   REAL :: x1 , x2 , xy , y1 , y2
   EXTERNAL bckrec , fread , line , linel , mesage , read , suplt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     IOPT CONTROLS THIS ROUTINE
!     IOPT .LT. 0
!       THE LINEL ARRAY WAS NOT CREATED.  UNIQUE LINES ARE NOT DRAWN.
!     IOPT .GE. 0
!       THE LIPLT ARRAY HAS CONNECTION DATA TO MAKE UNIQUE LINES. SUPLT
!       WILL CREATE THE LINES.  IPTR IS ONE OF THE PARAMETERS.
!
!     REVISED 10/1990 BY G.CHAN/UNISYS, TO INCLUDE BAR, TRIA3 AND QUAE4
!     OFFSET (PEDGE = 3)
!
   DATA te , br , t3 , q4/2HTE , 2HBR , 2HT3 , 2HQ4/
   DATA name/4HSHAP , 1HE/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL line(0,0,0,0,0,-1)
         IF ( Iopt>=0 ) THEN
!
!
            IF ( Pedge/=3 ) CALL suplt(Liplt(1),Liplt(Iptl+1),X,U,Gplst,Pen,Deform)
            GOTO 20
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*40,*20,Ect,etyp,1,0,i)
         offset = 0
         IF ( etyp==br ) offset = 6
         IF ( etyp==t3 .OR. etyp==q4 ) offset = 1
         CALL fread(Ect,i,1,0)
         ngpel = iabs(i)
         IF ( etyp/=te .AND. ngpel<5 ) THEN
!
            l = ngpel + 1
            IF ( ngpel<=2 .OR. i<0 ) l = ngpel
            ltyp = 10000
            m = 1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     NOT A SIMPLE ELEMENT
!
         lgpel = ngpel
         ltyp = etyp
         CALL linel(Liplt,ltyp,Opcor,lgpel,X,Pen,Deform,Gplst)
         l = iabs(ltyp)
         IF ( l<1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( l/=1 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         CALL fread(Ect,elid,1,0)
         IF ( elid<=0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL fread(Ect,lid,1,0)
         CALL fread(Ect,Liplt,ngpel,0)
         IF ( l/=ngpel ) Liplt(l) = Liplt(1)
!
         IF ( offset/=0 ) CALL fread(Ect,off,offset,0)
         IF ( offset/=0 .AND. Deform==0 ) THEN
!
!     IF THIS IS A BAR, TRIA3 OR QUAD4 ELEMENTS READ IN THE OFFSET
!     NO SCALE FACTOR APPLIES TO OFFSET HERE
!
            IF ( offset/=6 ) THEN
!
!     TRIA3 AND QUAD4 OFFSET
!
               off(1) = 0.707*off(1)
               DO i = 2 , 5
                  off(i) = off(1)
               ENDDO
            ELSE
!
!     BAR OFFSET
!
               off(1) = 0.707*sqrt(off(1)**2+off(2)**2+off(3)**2)
               off(2) = 0.707*sqrt(off(4)**2+off(5)**2+off(6)**2)
               off(3) = off(1)
            ENDIF
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     WRITE THE LINES.  0 FOR SIL MEANS NO LINES DRAWN
!
         j = 0
         DO i = 1 , l
            IF ( j/=0 ) THEN
               x1 = x2
               y1 = y2
            ENDIF
            gp = Liplt(i)
            IF ( gp/=0 ) THEN
               gp = iabs(Gplst(gp))
               IF ( Deform/=0 ) THEN
                  x2 = U(1,gp)
                  y2 = U(2,gp)
               ELSE
                  x2 = X(2,gp)
                  y2 = X(3,gp)
                  IF ( offset/=0 ) THEN
!
!     IF OFFSET IS PRESENT, ADD ARBITRARY AN OFFSET LENGTH TO X2 AND Y2.
!     SINCE THE OFFSET LENGTH IS SO TINY, ITS TRUE DIRECTION IS NOT OF
!     VITAL CONCERNS. THE IDEA HERE IS THAT BIG OFFSET WILL SHOW IN THE
!     PLOT IF ORIGINAL DATA CONTAINS ERRONEOUS AND BIG OFFSET VALUE(S).
!
!     IF OFFSET IS ADDED IN SAME DIRECTION AS THE PLOTTED LINE, ROTATE
!     THE OFFSET LENGTH BY 90 DEGREE
!
                     x2 = x2 + off(i)
                     xy = xy + off(i)
                     IF ( abs((x2-x1)-(y2-y1))<0.01 ) x2 = x2 - 2.*off(i)
                  ENDIF
               ENDIF
               IF ( j/=0 .AND. j/=gp ) CALL line(x1,y1,x2,y2,Pen,0)
            ENDIF
            j = gp
         ENDDO
         spag_nextblock_1 = 6
      CASE (6)
!
         IF ( l<ltyp ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( l/=ltyp ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      CALL line(0,0,0,0,0,1)
         IF ( Iopt<0 ) CALL bckrec(Ect)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
!     ILLEGAL EOF
!
 40      CALL mesage(-2,Ect,name)
         spag_nextblock_1 = 7
      CASE (7)
         IF ( Pedge==3 ) RETURN 1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE shape
