!*==sdhtf1.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sdhtf1(Type,Reject)
   IMPLICIT NONE
   USE c_condas
   USE c_gpta1
   USE c_matin
   USE c_sdr2x4
   USE c_sdr2x5
   USE c_sdr2x6
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Type
   LOGICAL :: Reject
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3,32) :: bxyz , dshp
   INTEGER , SAVE :: chbdy , ftube , numelt , tube , typold
   REAL :: d , detj , pi , x , y , z
   INTEGER :: estwds , i , ia , iel , ig , iloc , im , ip , is , isil , itemp , ith , itype , j
   REAL , DIMENSION(32) :: gpt , shp
   INTEGER , DIMENSION(2) :: nest
   INTEGER , DIMENSION(100) :: nesto
   INTEGER , DIMENSION(200) :: nestsc
   INTEGER , DIMENSION(8,20) , SAVE :: point1
   INTEGER , DIMENSION(8,3) , SAVE :: point2
   INTEGER , DIMENSION(8,23) :: pointr
   REAL , DIMENSION(3,3) :: xjacob
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE CONVERTS THE EST DATA FOR ALL THERMAL ELEMENTS TO A
!     COMMON FORMAT. SDHT1B IS CALLED TO PRODUCE THE OUTPUT
!
   !>>>>EQUIVALENCE (Consts(1),Pi) , (Nestsc(1),Estscr(1)) , (Nesto(1),Sub) , (Nest(1),Est(1)) , (point1(1,1),pointr(1,1)) ,             &
!>>>>    & (point2(1,1),pointr(1,21))
   DATA typold , numelt , tube , ftube , chbdy/0 , 23 , 3 , 82 , 52/
!     DATA    HEX   / 16    /
!
!     THE POINTERS TO THE EST DATA ARE
!
!        IM    MAT ID
!        ITH   THETA
!        IA    AREA
!        IG    GRID POINT DATA
!        IS    SIL MINUS 1
!        NP    NO. OF POINTS
!        SUB   SUBROUTINE TYPE
!                       NO.  IS   ITH  IM   IA   IG   NP   SUB
!                      ----  --   ---  --   --   --   --   ----
   DATA point1/1 , 0 , 0 , 4 , 5 , 9 , 2 , 1 , 3 , 0 , 0 , 4 , 5 , 8 , 2 , 1 , 6 , 0 , 5 , 6 , 7 , 15 , 3 , 2 , 9 , 0 , 5 , 6 , 7 , &
      & 9 , 3 , 2 , 10 , 0 , 0 , 4 , 5 , 9 , 2 , 1 , 16 , 0 , 6 , 7 , 8 , 10 , 4 , 3 , 17 , 0 , 5 , 6 , 7 , 9 , 3 , 2 , 18 , 0 , 6 ,&
      & 7 , 8 , 10 , 4 , 3 , 19 , 0 , 6 , 7 , 8 , 16 , 4 , 3 , 34 , 0 , 0 , 16 , 17 , 34 , 2 , 1 , 36 , 0 , 5 , 6 , 0 , 7 , 3 , 4 , &
      & 37 , 0 , 6 , 7 , 0 , 8 , 4 , 5 , 39 , 1 , 0 , 2 , 0 , 7 , 4 , 6 , 40 , 1 , 0 , 2 , 0 , 9 , 6 , 7 , 41 , 1 , 0 , 2 , 0 , 11 ,&
      & 8 , 8 , 42 , 1 , 0 , 2 , 0 , 11 , 8 , 9 , 52 , 1 , 0 , 15 , 16 , 21 , 8 , 10 , 62 , 0 , 6 , 7 , 8 , 10 , 4 , 3 , 63 , 0 ,   &
      & 6 , 7 , 8 , 10 , 4 , 3 , 65 , 0 , 0 , 10 , 0 , 16 , 8 , 16/
   DATA point2/66 , 0 , 0 , 22 , 0 , 28 , 20 , 16 , 67 , 0 , 0 , 34 , 0 , 40 , 32 , 16 , 76 , 0 , 11 , 12 , 13 , 14 , 8 , 17/
!
   IF ( Type==ftube ) THEN
!
!     FTUBE CONVECTION ELEMENT
!
      Reject = .FALSE.
      i = 0
      nest(i+101) = nestsc(1)
      nest(i+102) = nestsc(2)
      nest(i+103) = nestsc(3)
      est(i+104) = estscr(4)*estscr(5)
      est(i+105) = 0.0
      GOTO 99999
   ELSE
      IF ( Type==typold ) GOTO 100
      typold = Type
      Reject = .TRUE.
      DO i = 1 , numelt
         iel = i
         IF ( Type<pointr(1,i) ) EXIT
         IF ( Type==pointr(1,i) ) GOTO 50
      ENDDO
      RETURN
!
 50   Reject = .FALSE.
   ENDIF
 100  IF ( (Type>=65 .AND. Type<=67) .AND. strspt==0 ) strspt = strspt + 1
   ip = (Type-1)*incr
   estwds = elem(ip+12)
!
!     THE LOCATIONS OF DATA FOR EACH PARTICULAR ELEMENT ARE ZEROED OUT
!
   nq = 0
   DO i = 1 , 100
      nesto(i) = 0
   ENDDO
   name(1) = elem(ip+1)
   name(2) = elem(ip+2)
   elid = nest(1)
   DO i = 1 , 32
      nest(i+101) = 0
   ENDDO
   DO i = 1 , 201
      nest(i+137) = 0
   ENDDO
   IF ( Type==tube ) est(5) = pi*estscr(6)*(estscr(5)-estscr(6))
   IF ( Type==chbdy .AND. nestsc(2)==7 ) est(16) = pi*(estscr(19)+estscr(20))
   is = pointr(2,iel)
   ith = pointr(3,iel)
   im = pointr(4,iel)
   ia = pointr(5,iel)
   ig = pointr(6,iel)
   sub = pointr(8,iel)
   np = pointr(7,iel)
!
   IF ( sub==10 ) sub = sub + nestsc(2) - 1
   inflag = 1
   IF ( sub>=16 ) inflag = 3
   IF ( sub>=2 .AND. sub<=5 ) THEN
      inflag = 2
   ELSEIF ( sub>=6 .AND. sub<=9 ) THEN
      inflag = 3
   ENDIF
   IF ( sub/=16 ) GOTO 400
!
!     GET SHAPE FUNCTIONS ETC. FOR STRESS POINT(ALSO DETERMINE THE
!     STRESS POINT, WHICH WILL BE THE GRID POINTS PLUS CENTROID IN
!     ELEMENT COORDINATES
!
   itype = Type - 64
   DO i = 1 , np
      gpt(i) = estscr(5*np+7+i)
      DO j = 1 , 3
         bxyz(j,i) = estscr(np+4+4*i+j)
      ENDDO
   ENDDO
!
!     GET STRESS POINT
!
   y = -1.
   z = -1.
   IF ( itype>1 ) THEN
      d = 1.
      x = 0.
   ELSE
      d = 2.
      x = 1.
   ENDIF
   IF ( itype>1 ) THEN
      IF ( strspt==1 .OR. strspt==6 .OR. strspt==7 .OR. strspt==12 .OR. strspt==18 .OR. strspt==19 ) THEN
         x = x - d
      ELSEIF ( strspt==2 .OR. strspt==3 .OR. strspt==10 .OR. strspt==14 .OR. strspt==15 ) THEN
         x = x + d
      ELSEIF ( strspt==4 .OR. strspt==5 .OR. strspt==11 .OR. strspt==16 .OR. strspt==17 ) THEN
         y = y + d
      ELSEIF ( strspt==9 .OR. strspt==13 ) THEN
         z = z + 1.
         y = -1.
         d = 3. - d
      ELSEIF ( strspt==21 ) THEN
         GOTO 200
      ELSE
         y = y - d
      ENDIF
   ELSEIF ( strspt==2 .OR. strspt==6 ) THEN
      x = x + d
   ELSEIF ( strspt==3 .OR. strspt==7 ) THEN
      y = y + d
   ELSEIF ( strspt==5 ) THEN
      z = z + d
      y = -1.
   ELSEIF ( strspt==9 ) THEN
      GOTO 200
   ELSE
      x = x - d
   ENDIF
   GOTO 300
 200  x = 0.
   y = 0.
   z = 0.
 300  CALL ihexss(itype,shp,dshp,xjacob,detj,elid,x,y,z,bxyz)
!
!     GET DERIVATIVES W.R.T.X,Y,Z(REVERSE CALLING SEQUENCE BECAUSE
!     COLUMN-STORED
!
   CALL gmmats(dshp,np,3,0,xjacob,3,3,0,dshpb)
!
!
 400  IF ( ia>0 ) af = estscr(ia)
   matid = nestsc(im)
   IF ( matid<=0 ) RETURN
   sinth = 0.0
   costh = 1.0
   IF ( inflag==2 ) THEN
      theta = estscr(ith)*pi/180.
      IF ( theta/=0.0 ) THEN
         sinth = sin(theta)
         costh = cos(theta)
      ENDIF
   ENDIF
   itemp = ig + 4*np
   eltemp = estscr(itemp)
   IF ( sub==16 ) THEN
      isopl8 = 8
      eltemp = 0.
      DO i = 1 , np
         eltemp = eltemp + gpt(i)*shp(i)
      ENDDO
   ENDIF
   imat = matid
   CALL hmat(elid)
!
   DO i = 1 , np
      ip = 4*(i-1) + ig
      DO j = 1 , 3
         iloc = ip + j
         r(j,i) = estscr(iloc)
      ENDDO
      isil = is + i + 1
      sil(i) = nestsc(isil)
   ENDDO
!
   CALL sdhtff
!
99999 END SUBROUTINE sdhtf1
