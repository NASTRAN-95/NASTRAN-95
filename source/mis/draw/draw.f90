!*==draw.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE draw(Gplst,X,U,S,Disp,Stereo,Opcor,Buf1)
USE C_BLANK
USE C_DRWDAT
USE C_PLTDAT
USE C_RSTXXX
USE C_XXPARM
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Gplst
   REAL , DIMENSION(3,1) :: X
   REAL , DIMENSION(3,1) :: U
   REAL , DIMENSION(2,1) :: S
   LOGICAL :: Disp
   INTEGER :: Stereo
   INTEGER :: Opcor
   INTEGER :: Buf1
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3) :: a , sign
   INTEGER , DIMENSION(3) :: axis , vec
   REAL :: d , gpl , scalex , xorig
   INTEGER :: defm , gp , i , iopt , iptl , ishape , j , k , later , maxsf , nmax , nv , pen , sucor , v
   REAL(REAL64) :: dr , sum
   INTEGER , DIMENSION(2) :: sym
   EXTERNAL andf , bckrec , border , contor , dvectr , elelbl , gopen , gptlbl , gptsym , hdplot , hdsurf , intvec , linel ,        &
          & pcoord , perpec , proces , shape
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     /DRWDAT/ CONTROLS THIS ROUTINE
!     PLABEL - LABELING GRIDS, ELEMENTS...
!            -N = NONE
!             0 = GID             3 = EID          6 = EID + GID
!             1 = GID + SPC       4 = EID + PID
!             2 = UNDEFINED.      5 = UNDEFINED
!     PSHAPE - WHICH SHAPE OR OUTLINE OPTION TO DRAW...
!             1 = UNDEFORMED      2 = DEFORMED     3 = BOTH
!     PSYMBL(2) - DRAW SYMBOLS IF PSYMBL(1).NE.0
!     PSYMM (6) - SYMMETRY FLAGS...
!           (1) = X AXIS SIGN CHANGE   (4) = X DEFORMATION SIGN CHANGE
!           (2) = Y                    (5) = Y
!           (3) = Z                    (6) = Z
!     PVECTR - DEFORMATION VECTORS DRAWN (AS INTERPRETED BY INTVEC)...
!             0 = NONE
!             1 = X     4 = Z     7 = XYZ        10 = RY    13 = RXZ
!             2 = Y     5 = ZX    8 = UNDEFINED  11 = RXY   14 = RYZ
!             3 = XY    6 = ZY    9 = RX         12 = RZ    15 = RXYZ
!             THE NEGATIVE OF ABOVE, DO NOT DRAW SHAPE.
!    PCON   - NONZERO MEANS CONTOUR PLOT...
!    PEDGE  - 0 = SHAPE DRAWN,
!             1 = OUTLINE (BORDER) DRAWN ACCORDING TO PSHAPE-S
!             2 = HIDDEN LINE PLOT
!             3 = OFFSET PLOT
!             4 THRU N = SHRINK PLOT, ELEMENT SHRUNK BY THIS PERCENT
!             200 +  N = HIDDEN LINE AND SHRINK PLOT, N.GT.2
!             100 = FILL ?
!
!     OPCOR = NO. OF OPEN CORE WORDS AVAILABLE IN S
!             IT IS NOT A POINTER TO S, NOR A OPEN CORE ARRAY IN S
!     BUF1  = BUFFER AVAILABLE AT END OF CORE W.R.T. GPLST = BUFSIZ+1
!
!     OPEN CORE /ZZPLOT/
!     SETID NSETS NDOF      NGP 3*NGPSET 3*NGPSET  OPCOR   N
!     -----+-----+----+----+---+--------+--------+-------+--+--+-+-+-+-+
!          !          N1   N2  I1 (X)   I2 (U)   I3  (S)   DEFBUF..BUF..
!          !(DEFLST)       !
!                          !(GPLST)                      N=2*NGPSET
!
!     NGP    = TOTAL NO. OF GRID POINTS IN THE STRUCTURE
!     NGPSET = NO. OF GRID POINTS USED IN CURRENT SET OF PLOTTING
!     GPLST  = TABLE OF NGP IN LENGTH,
!              GPLST(I) = 0 IF THIS I-TH GRID POINT IS NOT USED FOR THE
!              CURRENT PLOT. OTHERWISE GPLST(I) IS NON-ZERO.
!     X      = X,Y,Z COORDINATES OF THE GRID POINTS CORRESPONDING TO THE
!              NON-ZERO GRID POINTS IN THE GPLST TABLE
!              TOTALLY, THERE ARE NGPSET GRID POINTS IN X
!     U      = X,Y,Z DISPLACEMENTS, ARRANGED SIMILARLY TO X
!     S      = SCRATCH AREA
!
         scalex = 1.0
         IF ( Prject==3 ) scalex = Objmod
!
!     SETUP THE PLOTTER REGION.
!
         IF ( Psymm(1)<0 .OR. Psymm(2)<0 .OR. Psymm(3)<0 ) THEN
            Reg(1) = 0.0
            Reg(2) = 0.0
            Reg(3) = Axymax(1)
            Reg(4) = Axymax(2)
         ELSE
            Reg(1) = Edge(Porig,1)*Axymax(1)
            Reg(2) = Edge(Porig,2)*Axymax(2)
            Reg(3) = Edge(Porig,3)*Axymax(1)
            Reg(4) = Edge(Porig,4)*Axymax(2)
         ENDIF
!
!     REDUCE THE GRID POINT CO-ORDINATES TO PLOT SIZE + TRANSLATE TO
!     THE SELECTED ORIGIN.
!
         DO i = 1 , 3
            Min(i) = +1.E+20
            Max(i) = -1.E+20
            IF ( Psymm(i)<0 ) THEN
               DO gp = 1 , Ngpset
                  X(i,gp) = -X(i,gp)
               ENDDO
            ENDIF
         ENDDO
         CALL proces(X)
         CALL perpec(X,Stereo)
         xorig = Xy(Porig,1)
         IF ( Stereo/=0 ) xorig = Xy(Porig,2)
         DO gp = 1 , Ngpset
            X(2,gp) = Scale*X(2,gp) - xorig
            X(3,gp) = Scale*X(3,gp) - Xy(Porig,3)
         ENDDO
!
         IF ( .NOT.(.NOT.Disp .OR. Maxdef==0 .OR. Defmax==0) ) THEN
!
!     PROCESS THE DEFORMATIONS.
!     EXCHANGE AXES, REDUCE THE MAXIMUM DEFORMATION TO -MAXDEF-.
!
            DO i = 1 , 3
               axis(i) = iabs(Daxis(i))
               sign(i) = 1.
               IF ( Daxis(i)<0 ) sign(i) = -1.
            ENDDO
            i = axis(1)
            j = axis(2)
            k = axis(3)
            d = Maxdef/Defmax
            DO gp = 1 , Ngpset
               IF ( Psymm(4)<0 ) U(1,gp) = -U(1,gp)
               IF ( Psymm(5)<0 ) U(2,gp) = -U(2,gp)
               IF ( Psymm(6)<0 ) U(3,gp) = -U(3,gp)
               a(1) = U(i,gp)
               a(2) = U(j,gp)
               a(3) = U(k,gp)
               U(1,gp) = a(1)*sign(1)*d
               U(2,gp) = a(2)*sign(2)*d
               U(3,gp) = a(3)*sign(3)*d
            ENDDO
            CALL intvec(Pvectr)
         ENDIF
!
!     IF PVECTR .LT. 0 NO SHAPE WILL BE DRAWN
!     ATTEMPT TO REMOVE DUPLICATE LINES
!
         iopt = -1
         sucor = 2*Ngpset + 1
         IF ( .NOT.Disp ) sucor = 1
!
!     FIRST DETERMINE OPTIONS - UNIQUE LINES FOR PSHAPE=3 MAY ONLY BE
!     FOR THE UNDERLAY.  ISHAPE = 0 MEANS DRAW THE SHAPE..
!
         ishape = -1
         later = 0
         IF ( .NOT.(Pvectr<0 .OR. (Pedge/=0 .AND. Pedge/=3)) ) THEN
            ishape = 0
            IF ( Opcor>=Ngpset+Ngp+1 ) THEN
               iopt = 0
               defm = 0
               IF ( Pshape>=2 ) defm = 1
               CALL linel(S(sucor,1),iptl,Opcor,iopt,X,Ppen,defm,Gplst)
               IF ( Pedge==3 ) GOTO 20
               IF ( iptl<=0 ) iopt = -1
               CALL bckrec(Elset)
            ENDIF
         ENDIF
         IF ( Pshape==2 .AND. Disp ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     DRAW UNDEFORMED SHAPE (USE PEN1 + SYMBOL 2 IF THE DEFORMED SHAPE
!     OR DEFORMATION VECTORS ARE ALSO TO BE DRAWN).
!
         pen = Ppen
         IF ( Disp .AND. Pshape>2 ) pen = 1
         IF ( ishape==0 ) CALL shape(*20,Gplst,X,0,pen,0,iopt,iptl,S(sucor,1),Opcor)
         IF ( Pedge>=2 ) THEN
            CALL hdsurf(Gplst,X,0,pen,0,nmax,maxsf,S(sucor,1),Buf1,Pedge,Opcor)
            IF ( Pedge==2 .OR. Pedge>=200 ) THEN
               CALL hdplot(Gplst,nmax,maxsf,Opcor,Buf1)
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( Pcon/=0 ) THEN
            IF ( .NOT.Disp .OR. Pshape<3 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            later = Pcon
            Pcon = 0
         ENDIF
         IF ( Pedge==0 .OR. Pedge>=2 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         iopt = -1
         CALL contor(Gplst,X,0,U,S(sucor,1),S(sucor,1),pen,0,Buf1,Opcor)
         IF ( Pedge==1 ) CALL border(Gplst,X,0,S(sucor,1),0,Buf1,Opcor)
         IF ( Pedge/=1 .AND. Color<0 ) THEN
            CALL gopen(Elset,Gplst(Buf1),0)
            CALL shape(*20,Gplst,X,0,1,0,iopt,iptl,S(sucor,1),Opcor)
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         Pcon = max0(Pcon,later)
         IF ( Ppen>31 ) CALL shape(*20,Gplst,X,0,1,0,iopt,iptl,S(sucor,1),Opcor)
         IF ( Pshape==1 ) Pcon = 0
         IF ( Psymbl(1)/=0 ) THEN
            IF ( Disp ) THEN
               sym(1) = 2
               sym(2) = 0
            ELSE
               sym(1) = Psymbl(1)
               sym(2) = Psymbl(2)
            ENDIF
            CALL gptsym(Gplst,X,0,sym,0)
         ENDIF
         IF ( Plabel>=0 ) THEN
            i = Plabel/3
            IF ( i/=1 ) CALL gptlbl(Gplst,X,0,0,Buf1)
            IF ( i>=1 ) THEN
               CALL elelbl(Gplst,X,0,0,Buf1)
               CALL bckrec(Elset)
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         IF ( .NOT.(.NOT.Disp .OR. Maxdef==0.0 .OR. Defmax==0.0) ) THEN
            IF ( Pedge/=3 ) THEN
               IF ( Pshape>=2 .OR. later/=0 ) THEN
!
!     ROTATE THE DEFORMATIONS
!
                  DO gp = 1 , Ngpset
                     DO j = 1 , 3
                        sum = Cstm(j,1)*U(1,gp) + Cstm(j,2)*U(2,gp) + Cstm(j,3)*U(3,gp)
                        IF ( j/=1 ) THEN
                           IF ( Prject/=1 ) sum = scalex*dr*sum
                           S(j-1,gp) = X(j,gp) + Scale*sum
                        ELSE
                           IF ( Prject/=1 ) dr = D0/(R0-scalex*(X(1,gp)+sum))
                        ENDIF
                     ENDDO
                  ENDDO
!
!     DRAW THE DEFORMED SHAPE
!
                  IF ( Pvectr>=0 ) THEN
                     pen = Ppen
                     IF ( Pshape==2 .AND. Pvectr/=0 ) pen = 1
                     IF ( Pedge==0 ) CALL shape(*20,Gplst,X,S,pen,1,iopt,iptl,S(sucor,1),Opcor)
                     IF ( Pedge>=2 ) THEN
                        CALL hdsurf(Gplst,X,S,pen,1,nmax,maxsf,S(sucor,1),Buf1,Pedge,Opcor)
                        IF ( Pedge==2 .OR. Pedge>200 ) CALL hdplot(Gplst,nmax,maxsf,Opcor,Buf1)
                     ENDIF
                  ENDIF
                  IF ( Pcon/=0 .AND. Pedge/=2 .AND. Pedge<=200 ) THEN
                     IF ( Icntvl>9 .OR. Pshape/=1 ) THEN
                        IF ( Icntvl<=13 .OR. Pshape/=1 ) THEN
                           CALL contor(Gplst,X,S,U,S(sucor,1),S(sucor,1),pen,0,Buf1,Opcor)
                           IF ( Pedge/=1 .AND. Color<0 ) THEN
                              CALL gopen(Elset,Gplst(Buf1),0)
                              CALL shape(*20,Gplst,X,0,1,0,iopt,iptl,S(sucor,1),Opcor)
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
                  IF ( Pedge==1 ) CALL border(Gplst,X,S,S(sucor,1),1,Buf1,Opcor)
                  IF ( Ppen>31 ) CALL shape(*20,gpl,X,0,1,0,iopt,iptl,S(sucor,1),Opcor)
                  IF ( Psymbl(1)/=0 ) THEN
                     IF ( Pshape==2 .AND. Pvectr/=0 ) THEN
                        sym(1) = 2
                        sym(2) = 0
                     ELSE
                        sym(1) = Psymbl(1)
                        sym(2) = Psymbl(2)
                     ENDIF
                     CALL gptsym(Gplst,X,S,sym,1)
                  ENDIF
                  IF ( Plabel>=0 .AND. Pshape==2 ) THEN
                     i = Plabel/3
                     IF ( i/=1 ) CALL gptlbl(Gplst,X,S,1,Buf1)
                     IF ( i>=1 ) CALL elelbl(Gplst,X,S,1,Buf1)
                  ENDIF
               ENDIF
               IF ( Pvectr/=0 ) THEN
                  Pvectr = iabs(Pvectr)
!
!     PROCESS THE DEFORMATION VECTORS
!
                  IF ( Pvectr<=7 ) THEN
                     nv = 3
                  ELSE
                     nv = 1
                     vec(1) = 0
                     vec(2) = 0
                     vec(3) = 0
                     DO v = 1 , 3
                        IF ( andf(Pvectr,2**(v-1))/=0 ) THEN
                           IF ( axis(1)==v ) vec(1) = 1
                           IF ( axis(2)==v ) vec(2) = 1
                           IF ( axis(3)==v ) vec(3) = 1
                        ENDIF
                     ENDDO
                  ENDIF
                  DO v = 1 , nv
                     IF ( Pvectr<=7 ) THEN
                        IF ( andf(Pvectr,2**(v-1))==0 ) CYCLE
                        DO i = 1 , 3
                           vec(i) = 0
                           IF ( axis(i)==v ) vec(i) = 1
                        ENDDO
                     ENDIF
!
!     ROTATE THE DEFORMATIONS (VEC = VECTOR DIRECTION TO BE DRAWN)
!
                     DO gp = 1 , Ngpset
                        DO j = 1 , 3
                           sum = 0.D0
                           DO i = 1 , 3
                              IF ( vec(i)/=0 ) sum = sum + Cstm(j,i)*U(i,gp)
                           ENDDO
                           IF ( j/=1 ) THEN
                              IF ( Prject/=1 ) sum = scalex*dr*sum
                              S(j-1,gp) = X(j,gp) + Scale*sum
                           ELSE
                              IF ( Prject/=1 ) dr = D0/(R0-scalex*(X(1,gp)+sum))
                           ENDIF
                        ENDDO
                     ENDDO
!
!     DRAW THE DEFORMATION VECTOR
!
                     CALL dvectr(Gplst,X,S,Ppen)
                     IF ( Psymbl(1)/=0 .AND. Pshape/=3 ) THEN
                        j = 0
                        IF ( Pshape==1 ) j = 1
                        CALL gptsym(Gplst,X,S,Psymbl,j)
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
!
!     END OF PLOT
!
!     IF NOT CONTOUR PLOT, CALL PCOORD TO DRAW A SMALL X-Y-Z COORDINATE
!     TRIAD AT THE LOWER RIGHT CORNER OF PLOT
!
 20      IF ( Pedge/=1 ) CALL pcoord(pen)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE draw
