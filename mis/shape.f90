
SUBROUTINE shape(Gplst,X,U,Pen,Deform,Iopt,Iptl,Liplt,Opcor) !HIDESTARS (*,Gplst,X,U,Pen,Deform,Iopt,Iptl,Liplt,Opcor)
   IMPLICIT NONE
   INTEGER Ect , Merr , Ngp , Pedge
   REAL Skp1(9) , Skp15(15) , Skp2(2) , Skp21(7)
   COMMON /blank / Ngp , Skp1 , Skp2 , Ect , Skp21 , Merr
   COMMON /drwdat/ Skp15 , Pedge
   INTEGER Deform , Iopt , Iptl , Opcor , Pen
   INTEGER Gplst(1) , Liplt(1)
   REAL U(2,1) , X(3,1)
   INTEGER br , elid , etyp , gp , i , j , l , lgpel , lid , ltyp , m , name(2) , ngpel , offset , q4 , t3 , te
   REAL off(6) , x1 , x2 , xy , y1 , y2
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
!
   CALL line(0,0,0,0,0,-1)
   IF ( Iopt>=0 ) THEN
!
!
      IF ( Pedge/=3 ) CALL suplt(Liplt(1),Liplt(Iptl+1),X,U,Gplst,Pen,Deform)
      GOTO 600
   ENDIF
 100  CALL read(*700,*600,Ect,etyp,1,0,i)
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
      GOTO 300
   ENDIF
!
!     NOT A SIMPLE ELEMENT
!
 200  lgpel = ngpel
   ltyp = etyp
   CALL linel(Liplt,ltyp,Opcor,lgpel,X,Pen,Deform,Gplst)
   l = iabs(ltyp)
   IF ( l<1 ) GOTO 100
   IF ( l==1 ) GOTO 500
   GOTO 400
 300  CALL fread(Ect,elid,1,0)
   IF ( elid<=0 ) GOTO 100
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
!
!     WRITE THE LINES.  0 FOR SIL MEANS NO LINES DRAWN
!
 400  j = 0
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
!
 500  IF ( l<ltyp ) GOTO 300
   IF ( l==ltyp ) GOTO 100
   GOTO 200
 600  CALL line(0,0,0,0,0,1)
   IF ( Iopt<0 ) CALL bckrec(Ect)
   GOTO 800
!
!     ILLEGAL EOF
!
 700  CALL mesage(-2,Ect,name)
 800  IF ( Pedge==3 ) RETURN 1
END SUBROUTINE shape