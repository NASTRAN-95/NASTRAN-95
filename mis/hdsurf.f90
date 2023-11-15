
SUBROUTINE hdsurf(Gplst,X,U,Pen,Deform,Nmax,Maxsf,Iz,Ib,Pedge,Iopcor)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Elid , Elset , G(3) , Ibuf , Idum(3) , Iout , Lid , Merr , Ngp , Nnn , Nofsur , Npers , Ns , Nscr1 , Nscr2 , Nscr3
   REAL P(3,13) , Skp11(11) , Skp22(7)
   COMMON /blank / Ngp , Skp11 , Elset , Skp22 , Merr , Idum , Nscr1 , Nscr2 , Nscr3
   COMMON /hdrec / Nofsur , Ns , Elid , Lid , Npers , P
   COMMON /pltscr/ Nnn , G
   COMMON /system/ Ibuf , Iout
!
! Dummy argument declarations
!
   INTEGER Deform , Ib , Iopcor , Maxsf , Nmax , Pedge , Pen
   INTEGER Gplst(1) , Iz(14,1)
   REAL U(2,1) , X(3,1)
!
! Local variable declarations
!
   INTEGER etyp , file , gp , i , im1 , ipedge , ipen , is , itype , j , k , kae , kbar , kfhex1 , kfhex2 , kfteta , kfwedg , khx1 ,&
         & khx2 , kis2d8 , kix1 , kix2 , kix3 , kq4 , kt3 , ktet , ktrim6 , ktrplt , ktrshl , kweg , ldx(9) , let(229) , let1(5) ,  &
         & let2(4,4) , let3(5,5) , let4(5,6) , let5(9,6) , let6(13,6) , let7(5) , let8(7) , let9(9) , letsz(3,9) , letsz2 , ls ,    &
         & lsmax , m , m1(16) , min , mm , n , name(2) , nelsrf , ngpel , ngpelx , nm1 , nn , nps1 , nps2 , npts , nwds , offset ,  &
         & solid , temp(27)
   LOGICAL hidden , shrink
   REAL shk , x1 , x2 , xc , y1 , y2 , yc , zc
!
! End of declarations
!
!
!     THIS ROUTINE PREPARES THE ELEMENT SURFACES FOR HIDDEN LINE PLOT
!     IT ALSO GENERATES THE SHRINK PLOT IF SHRINK ALONE IS REQUESTED.
!     IF SHRINK AND HIDDEN ARE REQUESTED, THIS ROUTINE WILL PREPARE THE
!     SHRUNK SURFACES FOR HDPLOT.
!
!     REVISED  10/1990 BY G.CHAN/UNISYS
!     (1) HIDDEN PLOT WITH SOLID ELEMENTS BUGS
!     (2) HIDDEN AND SHRINK TOGETHER
!     (3) SKIP ANY OFFSET DATA IN ELSET FILE IF THEY ARE PRESENT
!
!
!     DIMENSIONS      TEMP, IZ, AND P ARE TEMP(2*N+1), IZ(N+1,1), AND
!                     P(3,N) WHERE N=LETSZ2=MAX OF LETSZ(2,I), I=1,9
!
   EQUIVALENCE (let(1),let1(1)) , (let(6),let2(1,1)) , (let(22),let3(1,1)) , (let(47),let4(1,1)) , (let(77),let5(1,1)) ,            &
    & (let(131),let6(1,1)) , (let(209),let7(1)) , (let(214),let8(1)) , (let(221),let9(1))
!
   DATA name/4HHDSU , 4HRF  / , nm1 , m1/16 , 4H(33X , 4H,13H , 4HELEM , 4HENT  , 4HTYPE , 4H A5, , 4H4HWI , 4HTHI8 , 4H,24H ,      &
       &4H GRI , 4HDS S , 4HKIPP , 4HED I , 4HN LI , 4HNEL. , 4H)   /
!
!     SPECIAL ELEMENT CONNECTION PATTERNS
!
   DATA ldx/2HD1 , 2HD2 , 2HD3 , 2HD4 , 2HD5 , 2HD6 , 2HD7 , 2HD8 , 2HD9/
   DATA ktet/2HTE/ , kweg/2HWG/ , khx1/2HH1/ , khx2/2HH2/ , kix1/2HXL/ , kix2/2HXQ/ , kix3/2HXC/ , kae/2HAE/ , ktrim6/2HT6/ ,       &
      & ktrplt/2HP6/ , ktrshl/2HSL/ , kis2d8/2HD8/ , kfhex1/2HFA/ , kfhex2/2HFB/ , kfteta/2HFT/ , kfwedg/2HFW/ , kbar/2HBR/ ,       &
       &kt3/2HT3/ , kq4/2HQ4/
!    7        KELBOW/ 2HEB /
!
!     1   -   LINE,TRIANGLE,QUAD    5   -   IHEXA2
!     2   -   TETRA                 6   -   IHEXA3
!     3   -   WEDGE                 7   -   AERO
!     4   -   HEXA                  8   -   TRIM6 AND TRPLT1 AND TRSHL
!
   DATA letsz2/13/
   DATA letsz/1 , 5 , 1 , 4 , 4 , 6 , 5 , 5 , 22 , 6 , 5 , 47 , 6 , 9 , 77 , 6 , 13 , 131 , 1 , 5 , 209 , 1 , 7 , 214 , 1 , 9 , 221/
!         NELSRF,   NPTS,    IS
   DATA let1/1 , 2 , 3 , 4 , 5/
   DATA let2/1 , 2 , 3 , 1 , 1 , 2 , 4 , 1 , 2 , 3 , 4 , 2 , 1 , 3 , 4 , 1/
   DATA let3/1 , 2 , 3 , 1 , 0 , 4 , 5 , 6 , 4 , 0 , 1 , 3 , 6 , 4 , 1 , 1 , 2 , 5 , 4 , 1 , 2 , 3 , 6 , 5 , 2/
   DATA let4/1 , 2 , 3 , 4 , 1 , 5 , 6 , 7 , 8 , 5 , 3 , 4 , 8 , 7 , 3 , 1 , 2 , 6 , 5 , 1 , 2 , 3 , 7 , 6 , 2 , 1 , 4 , 8 , 5 , 1/
   DATA let5/1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 1 , 13 , 14 , 15 , 16 , 17 , 18 , 19 , 20 , 13 , 3 , 10 , 15 , 16 , 17 , 11 , 5 , 4 ,  &
      & 3 , 5 , 11 , 17 , 18 , 19 , 12 , 7 , 6 , 5 , 7 , 12 , 19 , 20 , 13 , 9 , 1 , 8 , 7 , 1 , 2 , 3 , 10 , 15 , 14 , 13 , 9 , 1/
   DATA let6/1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 12 , 1 , 21 , 22 , 23 , 24 , 25 , 26 , 27 , 28 , 29 , 30 , 31 , 32 , 21 ,&
      & 4 , 5 , 6 , 7 , 15 , 19 , 27 , 26 , 25 , 24 , 18 , 14 , 4 , 7 , 8 , 9 , 10 , 16 , 20 , 30 , 29 , 28 , 27 , 19 , 15 , 7 ,    &
      & 10 , 11 , 12 , 1 , 13 , 17 , 21 , 32 , 31 , 30 , 20 , 16 , 10 , 1 , 2 , 3 , 9 , 14 , 18 , 24 , 23 , 22 , 21 , 17 , 13 , 1/
   DATA let7/1 , 2 , 3 , 4 , 1/
   DATA let8/1 , 2 , 3 , 4 , 5 , 6 , 1/
   DATA let9/1 , 5 , 2 , 6 , 3 , 7 , 4 , 8 , 1/
!
!     PEDGE FLAG = 2 OR 200    - HIDDEN LINE PLOT
!                = 10 THRU 100 - SHRINK PLOT.
!                = 100         - FILL, NOT USED HERE
!                = .GT.    200 - SHRINK AND HIDDEN LINE PLOT
!     E.G. PEDGE = 270 INDICATES HIDDEN LINE PLOT WITH EACH ELEMENT
!                  SHRUNK TO 70 PERCENT OF FULL SIZE.
!
   ipedge = mod(Pedge,200)
   nwds = 0
   Nmax = 0
   ls = 0
   lsmax = Iopcor/14
   IF ( Pedge>200 ) lsmax = 0
   Nofsur = 0
   shk = 1.0
   shrink = .FALSE.
   IF ( Pedge>=10 ) THEN
      shrink = .TRUE.
      shk = 1. - float(ipedge)/100.
      CALL line(0.,0.,0.,0.,1.,-1)
   ENDIF
   hidden = .FALSE.
   IF ( Pedge==2 .OR. Pedge>=200 ) THEN
      hidden = .TRUE.
      CALL gopen(Nscr2,Gplst(Ib),1)
      nwds = 3*letsz2 + 5
   ENDIF
!
 100  CALL read(*600,*500,Elset,etyp,1,0,i)
   CALL fread(Elset,i,1,0)
   ngpel = iabs(i)
   ngpelx = ngpel
   solid = 0
!
   offset = 0
   IF ( etyp==kbar ) offset = 6
   IF ( etyp==kt3 .OR. etyp==kq4 ) offset = 1
   itype = 1
   IF ( etyp==ktet ) itype = 2
   IF ( etyp==kfteta ) itype = 2
   IF ( etyp==kweg ) itype = 3
   IF ( etyp==kfwedg ) itype = 3
   IF ( etyp==khx1 .OR. etyp==khx2 .OR. etyp==kix1 .OR. etyp==kfhex1 .OR. etyp==kfhex2 ) itype = 4
   IF ( etyp==kix2 ) itype = 5
   IF ( etyp==kis2d8 ) itype = 9
   IF ( etyp==kix3 ) itype = 6
   IF ( etyp==kae ) itype = 7
   IF ( etyp==ktrim6 .OR. etyp==ktrplt .OR. etyp==ktrshl ) itype = 8
!
   IF ( itype/=1 ) THEN
!
!     COMPLEX ELEMENT
!
      IF ( itype>=2 .AND. itype<=6 ) solid = 1
      npts = letsz(2,itype)
      IF ( npts-1>Nmax ) Nmax = npts - 1
   ELSE
!
!     SIMPLE ELEMENT
!
      IF ( ngpel>2 .AND. i>0 ) ngpelx = ngpel + 1
      IF ( ngpel>4 ) THEN
!
!     CHECK FOR PDUM ELEMENTS BEFORE  EJECTING
!
         DO i = 1 , 9
            IF ( etyp==ldx(i) ) GOTO 300
         ENDDO
         GOTO 200
      ELSE
         npts = ngpelx
         IF ( npts-1>Nmax ) Nmax = npts - 1
      ENDIF
   ENDIF
   DO
!
!     READ THE ELEMENT DATA
!
      CALL fread(Elset,Elid,1,0)
      IF ( Elid<=0 ) GOTO 100
      CALL fread(Elset,Lid,1,0)
      CALL fread(Elset,G,ngpel,0)
      IF ( offset/=0 ) CALL fread(Elset,0,-offset,0)
      IF ( ngpel/=ngpelx ) G(ngpelx) = G(1)
      IF ( .NOT.(hidden .AND. .NOT.shrink) ) THEN
         xc = 0.
         yc = 0.
         zc = 0.
         DO i = 1 , ngpel
            gp = G(i)
            gp = iabs(Gplst(gp))
            xc = xc + X(2,gp)
            yc = yc + X(3,gp)
            zc = zc + X(1,gp)
         ENDDO
         xc = xc/ngpel
         yc = yc/ngpel
         zc = zc/ngpel
      ENDIF
!
      nelsrf = letsz(1,itype)
      is = letsz(3,itype)
!
      DO Ns = 1 , nelsrf
         nn = 0
         mm = (Ns-1)*npts + is - 1
         Npers = npts
         DO i = 1 , npts
            m = mm + i
            n = let(m)
            IF ( n/=0 ) THEN
               gp = G(n)
               IF ( gp/=0 ) THEN
                  nn = nn + 1
                  gp = iabs(Gplst(gp))
                  P(3,nn) = X(1,gp)
                  IF ( Deform/=0 ) THEN
                     P(1,nn) = U(1,gp)
                     P(2,nn) = U(2,gp)
                  ELSE
                     P(1,nn) = X(2,gp)
                     P(2,nn) = X(3,gp)
                  ENDIF
                  IF ( shrink ) THEN
                     IF ( hidden ) THEN
                        P(3,nn) = X(1,gp) - (X(1,gp)-zc)*shk
                        P(1,nn) = X(2,gp) - (X(2,gp)-xc)*shk
                        P(2,nn) = X(3,gp) - (X(3,gp)-yc)*shk
                        IF ( Deform/=0 ) THEN
                           P(1,nn) = U(1,gp) - (X(2,gp)-xc)*shk
                           P(2,nn) = U(2,gp) - (X(3,gp)-yc)*shk
                        ENDIF
                     ELSEIF ( nn/=1 ) THEN
                        x1 = P(1,nn-1) - (P(1,nn-1)-xc)*shk
                        y1 = P(2,nn-1) - (P(2,nn-1)-yc)*shk
                        x2 = P(1,nn) - (P(1,nn)-xc)*shk
                        y2 = P(2,nn) - (P(2,nn)-yc)*shk
                        ipen = Pen
                        IF ( ipedge==100 .AND. Pen>31 .AND. i==Npers ) Pen = 0
                        IF ( shrink ) CALL line(x1,y1,x2,y2,Pen,0)
                        IF ( Pen==0 ) Pen = ipen
                     ENDIF
                  ENDIF
                  CYCLE
               ENDIF
            ENDIF
            Npers = Npers - 1
         ENDDO
         IF ( .NOT.(shrink .AND. .NOT.hidden) ) THEN
            CALL write(Nscr2,Nofsur,nwds,0)
            Nofsur = Nofsur + 1
            IF ( .NOT.(solid==0 .OR. .NOT.hidden) ) THEN
!
!     SAVE SOLID SURFACE DATA IN IZ SPACE FOR SECOND PROCESSING, HIDDEN
!     PLOT ONLY. SAVE AS MANY AS OPEN CORE CAN HOLD
!
               IF ( ls<lsmax ) THEN
                  ls = ls + 1
                  nps1 = Npers - 1
                  DO i = 1 , nps1
                     m = mm + i
                     n = let(m)
                     gp = G(n)
                     temp(i) = gp
                     temp(i+nps1) = gp
                  ENDDO
                  m = 1
                  min = temp(1)
                  DO i = 2 , nps1
                     IF ( temp(i)<min ) THEN
                        m = i
                        min = temp(i)
                     ENDIF
                  ENDDO
                  IF ( m==1 ) m = m + nps1
                  n = +1
                  IF ( temp(m-1)<temp(m+1) ) n = -1
                  IF ( n==-1 .AND. m<nps1 ) m = m + nps1
                  k = nps1 + 2
                  DO i = 3 , k
                     Iz(i,ls) = temp(m)
                     m = m + n
                  ENDDO
                  Iz(1,ls) = Nofsur
                  Iz(2,ls) = nps1
               ENDIF
            ENDIF
         ENDIF
!
      ENDDO
   ENDDO
!
!     ILLEGAL ELEMENT, NO CORE FOR 1 ELEMENT
!
 200  G(1) = 2
   G(2) = etyp
   G(3) = ngpel
   CALL wrtprt(Merr,G,m1,nm1)
   DO
!
!     READ TO THE END OF THIS ELEMENT
!
      CALL read(*400,*100,Elset,Elid,1,0,m)
      IF ( Elid<=0 ) GOTO 100
      j = 1 + ngpel + offset
      CALL fread(Elset,0,-j,0)
   ENDDO
 300  WRITE (Iout,99001) i
99001 FORMAT ('0*** MISSING PDUM',I1,' SUBROUTINE/HDSURF')
   GOTO 200
 400  CALL mesage(-8,Elset,name)
!
 500  Maxsf = Nofsur
   CALL bckrec(Elset)
   IF ( shrink ) CALL line(0.,0.,0.,0.,1.,+1)
   IF ( hidden ) THEN
      CALL write(Nscr2,0,0,1)
      IF ( ls>=60 ) THEN
!
!     REPROCESS NSCR2 TO REMOVE DUPLICATE SURFACES (INTERIOR-INTERFACES)
!     AND SAVE REDUCED DATA IN NSCR1.
!     INTERCHANGE NSCR1 AND NSCR2 INDICES
!
         j = (letsz2+1)*ls
         CALL sort2k(0,0,letsz2+1,3,Iz,j)
         m = 0
         nps1 = 0
         DO i = 1 , ls
            nps2 = Iz(2,i) + 2
            IF ( nps2==nps1 ) THEN
               im1 = i - 1
               DO j = 3 , nps1
                  IF ( Iz(j,i)/=Iz(j,im1) ) GOTO 520
               ENDDO
               IF ( m/=0 ) THEN
                  IF ( Iz(m,1)==Iz(1,im1) ) GOTO 510
               ENDIF
               m = m + 1
               Iz(m,1) = Iz(1,im1)
            ELSE
               nps1 = nps2
               CYCLE
            ENDIF
 510        m = m + 1
            Iz(m,1) = Iz(1,i)
 520     ENDDO
!
         IF ( m>=20 ) THEN
            CALL sort(0,0,1,1,Iz,m)
            Iz(m+1,1) = 999999999
            file = Nscr1
            CALL gopen(Nscr1,Gplst(Ib+Ibuf),1)
            file = Nscr2
            CALL close(Nscr2,1)
            CALL gopen(Nscr2,Gplst(Ib),0)
            n = 1
            DO i = 1 , Maxsf
               CALL read(*700,*800,Nscr2,Nofsur,nwds,0,j)
               IF ( i<Iz(n,1) ) THEN
                  CALL write(Nscr1,Nofsur,nwds,0)
               ELSE
                  n = n + 1
               ENDIF
            ENDDO
!
            CALL close(Nscr2,1)
            j = Nscr2
            Nscr2 = Nscr1
            Nscr1 = j
            Maxsf = Maxsf - m
            CALL write(Nscr2,0,0,1)
         ENDIF
      ENDIF
      CALL close(Nscr2,1)
   ENDIF
   RETURN
!
 600  j = -1
   file = Elset
   GOTO 900
 700  j = -2
   GOTO 900
 800  j = -3
 900  CALL mesage(j,file,name)
   GOTO 500
END SUBROUTINE hdsurf
