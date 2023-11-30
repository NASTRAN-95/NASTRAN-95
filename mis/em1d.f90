
SUBROUTINE em1d(Eltype,Istart,Itype,Ncount,Ido,Iwords,Nbdys,All,Nelout)
   IMPLICIT NONE
   REAL Ecpt(200) , Eltemp , Xmat , Z(1)
   INTEGER Incr , Inflag , Ithrml , Iz(1) , Ksystm(64) , Last , Matid , Ne(1) , Necpt(200) , Nelems , Nsil(2) , Outpt , Sysbuf
   CHARACTER*23 Ufm
   COMMON /emecpt/ Ecpt
   COMMON /gpta1 / Nelems , Last , Incr , Ne
   COMMON /hmtout/ Xmat
   COMMON /matin / Matid , Inflag , Eltemp
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER All , Eltype , Ido , Istart , Itype , Iwords , Nbdys , Ncount , Nelout
   REAL ahcx , ahcy , ahcz , arrod , buf(50) , const , dia , dndx(2) , dndy(2) , dndz(2) , gnu , h1 , h2 , h3 , hc1 , hc2 , hc3 ,   &
      & hcx(2) , hcy(2) , hcz(2) , hl , pi , rad , sc(5) , th , vol , w(2) , xi(2) , xl , xlacc(3) , xlen , xload(2) , xlocal ,     &
      & xlx , xlxp , xn(2) , xx , yl , yy , zi(3) , zl , zz
   INTEGER estwds , i , iar , ibuf(50) , idx , ijk , is , isc(5) , isub , itemp , ix1 , ix2 , iy1 , iy2 , iz1 , iz2 , j , jtype ,   &
         & k , mid , nam(2) , name(2) , ng , ngrids , npts , scr6
   LOGICAL onlyc
!
!     COMPUTE LOAD DUE TO MAGNETIC FIELD,  K*A + F = 0
!     SOLVE FOR -F
!
!     USE ELEMENT COORDINATES  FOR  ROD
!
!     SET UP COMMON BLOCKS, TABLES
!
!     KSYSTM(1) = 1ST POS. OF OPEN CORE
!     KSYSTM(2) = OUTPUT FILE NO.
!     KSYSTM(56) NE 0 FOR HEAT TRANSFER OPTION
!
!     Z      = OPEN CORE ARRAY
!     OUTPT  = OUTPUT FILE NO.
!     NELEMS = NO OF ELEMENTS (TYPES) IN THIS TABLE
!     LAST   = LOC OF 1ST WORD OF LAST ENTRY(EL) IN TABLE
!     INCR   = MAX NO WDS ALLOWED IN ANY ENTRY
!
!     BUF1   = BUFFER FOR EST
!     EST    = ELEMENT SUMMARY TABLE(PROG MAN 2.3.56)
!     SLT    = STAIC LOADS TABLE(2.3.51)
!     SYSTEM 2.4.13 PROG MANUAL
!     GPTA1  2.5.6
!     EST    2.3.56
!     SLT    2.3.51
!
!     ISTART GIVES 1ST POSITION OF HC OR REMFLUX VALUES
!     ROD IS IN ELEMENT COORDINATES, AS ARE TUBE,CONROD,BAR
!
!     X1 = 0.   X2 = X
!     AREA OF ROD NEEDED TO COMPUTE VOL
!     VOL  = LENGTH  * A
!     AREA OF TUBE CONPUTED WT OUTS.DIA.
!
!     OPEN FILE EST FOR ELEMENT DATA
!
!     INTEGRAL OVER VOL OF (GRADIENT SHAPE FUNC. TIMES GNU TIMES HC)
!
!     Z(1)  1ST POSITION OF LOAD
!     NELEMS = NO OF ELEMENTS
!     INCR   = MAX NO OF WORDS FOR AN ELEMENT OF THE ES T TABLE
!     NE(1 AND2) = ELEMENT NAME
!
   !>>>>EQUIVALENCE (ibuf(1),buf(1)) , (sc(1),isc(1)) , (Ksystm(1),Sysbuf) , (Ksystm(2),Outpt) , (Ksystm(56),Ithrml) , (Ecpt(1),Necpt(1))&
!>>>>    & , (Z(1),Iz(1)) , (Nsil(1),Necpt(2))
   DATA nam/4H  EM , 4H1D  /
   DATA scr6/306/
!
!     FROM EST GET ALL NECESSARY ELEMENT INFO
!
   onlyc = .FALSE.
   ng = 3
   isc(1) = Necpt(1)
   isc(2) = 1
   xi(1) = -.5773502692
   xi(2) = -xi(1)
   w(1) = 1.
   w(2) = 1.
   idx = (Eltype-1)*Incr
   estwds = Ne(idx+12)
   ngrids = Ne(idx+10)
   name(1) = Ne(idx+1)
   name(2) = Ne(idx+2)
!
!     CHECK TO SEE IF THIS ELEMENT CONTAINS A GRID POINT ON A PERMBDY
!     CARD. IF SO, OR IF NO PERMBDY CARD EXISTS, COMPUTE LOADS FOR THE
!     ELEMENT IF NOT, COMPUTE HC CENTROIDAL VALUE ONLY. (ONLYC=.TRUE.)
!     THE PERMBDY SILS START AT Z(ISTART-NBDYS-1)
!
   IF ( Nbdys/=0 ) THEN
!
      DO i = 1 , ngrids
         DO j = 1 , Nbdys
            IF ( Nsil(i)==Iz(Istart-Nbdys-Nelout+j-1) ) GOTO 100
         ENDDO
      ENDDO
!
!     ELEMENT HAS NO GRIDS ON PERMBDY
!
      onlyc = .TRUE.
      ng = 1
   ENDIF
 100  IF ( onlyc .AND. Itype==24 ) RETURN
!
!     IF ONLYC=TRUE, CHECK TO SEE IF THE ELEMENT HAD AN ELFORCE REQUEST.
!     IF SO, CONTINUE. IF NOT, JUST WRITE ZEROS TO HCCEN,SCR6) AND
!     RETURN.
!
   IF ( onlyc ) THEN
      IF ( All/=1 ) THEN
         IF ( Nelout/=0 ) THEN
!
            DO i = 1 , Nelout
               IF ( Necpt(1)==Iz(Istart-Nelout+i-1) ) GOTO 200
            ENDDO
         ENDIF
         GOTO 300
      ENDIF
   ENDIF
!
!     1ST CHECK FOR ZERO LOAD
!
 200  IF ( Itype/=20 .AND. Itype/=24 ) GOTO 400
   h1 = 0.
   h2 = 0.
   h3 = 0.
   DO i = 1 , 2
      isub = Istart + 3*Nsil(i) - 3
      IF ( Itype==24 ) isub = Istart + 3*Ncount - 3
      h1 = h1 + abs(Z(isub))
      h2 = h2 + abs(Z(isub+1))
      h3 = h3 + abs(Z(isub+2))
      IF ( Itype==24 ) EXIT
   ENDDO
   hl = h1 + h2 + h3
   IF ( hl/=0. ) GOTO 400
   IF ( Itype==24 ) RETURN
!
!     ALL ZEROS-WRITE ON SCR6
!
 300  sc(3) = 0.
   sc(4) = 0.
   sc(5) = 0.
   CALL write(scr6,sc,5,0)
   RETURN
!
!
!     ROD ELTYPE 1
!     TUBE       3
!     CONROD    10
!     BAR       34
!     OTHERWISE  GET OUT
!     ONED  SOLVES LOAD DUE TO MAGNETIC FILED
!
 400  pi = 3.14159
   Inflag = 1
   IF ( Eltype==1 .OR. Eltype==10 ) THEN
      mid = 4
      itemp = 17
      ix1 = 10
      ix2 = 14
      iy1 = 11
      iy2 = 15
      iz1 = 12
      iz2 = 16
      iar = 5
   ELSEIF ( Eltype==3 ) THEN
      mid = 4
      itemp = 16
      ix1 = 9
      ix2 = 13
      iy1 = 10
      iy2 = 14
      iz1 = 11
      iz2 = 15
!
!     COMPUTE AREA
!
      dia = Ecpt(5)
      th = Ecpt(6)
      rad = dia - 2.*th
      arrod = pi*((dia/2)**2-(rad/2.)**2)
   ELSEIF ( Eltype/=34 ) THEN
!
      WRITE (Outpt,99001) Ufm
99001 FORMAT (A23,', ELEMENT TYPE ',2A4,' WAS USED IN AN E AND M ','PROBLEM.')
      CALL mesage(-37,0,nam)
      GOTO 99999
   ELSE
      mid = 16
      itemp = 42
      ix1 = 35
      ix2 = 39
      iy1 = 36
      iy2 = 40
      iz1 = 37
      iz2 = 41
      iar = 17
   ENDIF
   IF ( .NOT.(onlyc) ) THEN
      xl = Ecpt(ix2) - Ecpt(ix1)
      yl = Ecpt(iy2) - Ecpt(iy1)
      zl = Ecpt(iz2) - Ecpt(iz1)
      xlen = sqrt(xl**2+yl**2+zl**2)
      xn(1) = -1./xlen
      xn(2) = 1./xlen
      IF ( Eltype/=3 ) arrod = Ecpt(iar)
      Eltemp = Ecpt(itemp)
      Matid = Necpt(mid)
!
!     ARROD = AREA OF CROSS SECTION OF ROD
!
      IF ( Itype/=24 ) CALL hmat(Necpt(1))
      gnu = Xmat
      IF ( Itype==24 ) gnu = 1.
!
!     HC   FROM Z(ISTART)
!     YIELDS X COORD OF HC FOR GRID PT DEFINED BY NSIL
!
      vol = arrod*xlen
!
!     COMPUTE BASIC TO LOCAL TRANSFORMATION
!
      zi(1) = xl/xlen
      zi(2) = yl/xlen
      zi(3) = zl/xlen
!
!     PARTIALS OF N W.R.T X-GLOBAL,Y-GLOBAL,Z-GLOBAL
!
      dndx(1) = -zi(1)/xlen
      dndy(1) = -zi(2)/xlen
      dndz(1) = -zi(3)/xlen
      dndx(2) = -dndx(1)
      dndy(2) = -dndy(1)
      dndz(2) = -dndz(1)
      const = .5*gnu*vol
      IF ( Itype==24 ) THEN
!
!     REMFLUX
!
         is = Istart + 3*Ncount - 3
         ahcx = Z(is)
         ahcy = Z(is+1)
         ahcz = Z(is+2)
!
         xload(1) = gnu*vol*(dndx(1)*ahcx+dndy(1)*ahcy+dndz(1)*ahcz)
         xload(2) = gnu*vol*(dndx(2)*ahcx+dndy(2)*ahcy+dndz(2)*ahcz)
         GOTO 500
      ENDIF
   ENDIF
   jtype = Itype - 19
   xlacc(1) = 0.
   xlacc(2) = 0.
   xlacc(3) = 0.
!
!     LOOP OVER INTEGRATION POINTS-ASSUME CUBIC VARIATION. SO NEED 2
!     INTEGRATION POINTS + CENTROID
!
   DO npts = 1 , ng
      IF ( npts/=ng ) THEN
!
!     COMPUTE LOCAL COORDINATE OF SAMPLING POINT
!
         xlocal = .5*xlen*(1.+xi(npts))
         xlx = xlocal/xlen
         xlxp = 1. - xlx
!
!     COMPUTE BASIC COORDS FOR XLOCAL
!
         xx = xlxp*Ecpt(ix1) + xlx*Ecpt(ix2)
         yy = xlxp*Ecpt(iy1) + xlx*Ecpt(iy2)
         zz = xlxp*Ecpt(iz1) + xlx*Ecpt(iz2)
      ELSE
         xx = .5*(Ecpt(ix1)+Ecpt(ix2))
         yy = .5*(Ecpt(iy1)+Ecpt(iy2))
         zz = .5*(Ecpt(iz1)+Ecpt(iz2))
!
!     AVERAGE SPCFLD
!
         xlx = .5
         xlxp = .5
      ENDIF
      ahcx = 0.
      ahcy = 0.
      ahcz = 0.
!
!     COMPUTE HC AT THIS POINT DUE TO ALL LOADS OF THIS TYPE
!
      DO ijk = 1 , Ido
         IF ( Itype/=20 ) THEN
            isub = Istart + (ijk-1)*Iwords - 1
            DO i = 1 , Iwords
               buf(i) = Z(isub+i)
            ENDDO
            IF ( jtype==2 ) THEN
               CALL axloop(buf,ibuf,xx,yy,zz,hc1,hc2,hc3)
            ELSEIF ( jtype==3 ) THEN
               CALL geloop(buf,ibuf,xx,yy,zz,hc1,hc2,hc3)
            ELSEIF ( jtype==4 ) THEN
               CALL dipole(buf,ibuf,xx,yy,zz,hc1,hc2,hc3)
            ELSE
               GOTO 420
            ENDIF
            GOTO 440
         ENDIF
!
!     SPCFLD
!
 420     DO i = 1 , 2
            is = Istart + 3*Nsil(i) - 3
            hcx(i) = Z(is)
            hcy(i) = Z(is+1)
            hcz(i) = Z(is+2)
         ENDDO
!
!     INTERPOLATE GRID VALUES TO INTEGRATION POINT
!
         hc1 = xlxp*hcx(1) + xlx*hcx(2)
         hc2 = xlxp*hcy(1) + xlx*hcy(2)
         hc3 = xlxp*hcz(1) + xlx*hcz(2)
 440     ahcx = ahcx + hc1
         ahcy = ahcy + hc2
         ahcz = ahcz + hc3
      ENDDO
      IF ( npts/=ng ) THEN
!
!     WE HAVE HC AT THIS INTEGRATION POINT. MULT. BY WEIGHT AND
!     ACCUMULATE
!
         xlacc(1) = xlacc(1) + ahcx*w(npts)
         xlacc(2) = xlacc(2) + ahcy*w(npts)
         xlacc(3) = xlacc(3) + ahcz*w(npts)
      ELSE
         sc(3) = ahcx
         sc(4) = ahcy
         sc(5) = ahcz
         CALL write(scr6,sc,5,0)
         IF ( onlyc ) RETURN
      ENDIF
   ENDDO
!
!     MULT. BY CONST AND GRAD N TO GET LOADS
!
   xload(1) = const*(dndx(1)*xlacc(1)+dndy(1)*xlacc(2)+dndz(1)*xlacc(3))
   xload(2) = const*(dndx(2)*xlacc(1)+dndy(2)*xlacc(2)+dndz(2)*xlacc(3))
 500  DO i = 1 , 2
      j = Nsil(i)
!
!     IF PERMBDY EXISTS AND IF GRID IS NOT ON IT, IGNORE ITS LOAD
!
      IF ( Nbdys/=0 ) THEN
         DO k = 1 , Nbdys
            IF ( j==Iz(Istart-Nbdys-Nelout+k-1) ) EXIT
         ENDDO
      ENDIF
      Z(j) = Z(j) - xload(i)
   ENDDO
   RETURN
99999 RETURN
END SUBROUTINE em1d