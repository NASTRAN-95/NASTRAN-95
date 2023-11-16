
SUBROUTINE estmag(Hest,Estfld,Mpt,Dit,Geom1,Iany,Kcount)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Costh , Eltemp , Sinth , Stress , Xmat(6) , Z(1)
   INTEGER Iditfl , Iihmat , Incr , Inflag , Iz(1) , Last , Matid , Mptfil , Ne(1) , Nelems , Nnhmat , Otpe , Sysbuf
   CHARACTER*23 Ufm
   COMMON /gpta1 / Nelems , Last , Incr , Ne
   COMMON /hmatdd/ Iihmat , Nnhmat , Mptfil , Iditfl
   COMMON /hmtout/ Xmat
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /system/ Sysbuf , Otpe
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Dit , Estfld , Geom1 , Hest , Iany , Kcount , Mpt
!
! Local variable declarations
!
   REAL angle , c , con1 , con2 , coord(3) , cs , csq , dn(8) , e(9) , ecpt(200) , g(9) , s , ssq , temp , v12(3) , v13(3) , x2 ,   &
      & xi(3) , xj(3) , xk(3) , xlen , xm(32)
   INTEGER bfield(2) , buf1 , buf2 , buf3 , eltype , estwds , file , frstgd , i , iadd , iall , icount , idefid , idex , idim ,     &
         & idx , iecpt(200) , ifield , iflag , isys1 , isys2 , isys3 , itemp , ith , iwords , j , jco , jcount , jel , jsub , kadd ,&
         & kount(2) , lcore , mcb(7) , mid , n , nam(2) , name(2) , nextz , nfield , ngrids , num , oldeid , pointr(6,20)
   INTEGER korsz
!
! End of declarations
!
!
!     CREATE SCRATCH FILE ESTFLD WHICH WILL BE USED TO COMPUTE TOTAL
!     MAGNETIC FIELD. READ EST AND CREATE SIMILAR RECORDS CONTAINING
!     ELTYPE,EID,NUMBER OF SILS,SILS,3 X 3 MATERIALS MATRIX, AND 3 X 3
!     TRANSFORMATION MATRIX TO BRING HM BACK TO BASIC COORD. SYSTEM
!     FROM ELEMENT SYSTEM,OUTPUT COORD. SYSTEM ID, AND BASIC COORDS.
!     OF STRESS POINT(USUALLY AVERAGE OF GRID COORDS.)
!
   EQUIVALENCE (Z(1),Iz(1)) , (ecpt(1),iecpt(1))
   DATA nam/4HESTM , 4HAG  /
   DATA bfield/3101 , 31/
   DATA dn/4* - .25 , 4*.5/
!
!     ITYPE ITH MID ISYS1 ITEMP DIM
!
   DATA pointr/1 , 0 , 4 , 9 , 17 , 1 , 3 , 0 , 4 , 8 , 16 , 1 , 6 , 5 , 6 , 15 , 27 , 2 , 9 , 5 , 6 , 9 , 21 , 2 , 10 , 0 , 4 , 9 ,&
      & 17 , 1 , 16 , 6 , 7 , 10 , 26 , 2 , 17 , 5 , 6 , 9 , 21 , 2 , 18 , 6 , 7 , 10 , 26 , 2 , 19 , 6 , 7 , 16 , 32 , 2 , 34 , 0 ,&
      & 16 , 34 , 42 , 1 , 36 , 5 , 6 , 7 , 19 , 2 , 37 , 6 , 7 , 8 , 24 , 2 , 39 , 0 , 2 , 7 , 23 , 3 , 40 , 0 , 2 , 9 , 33 , 3 ,  &
      & 41 , 0 , 2 , 11 , 43 , 3 , 42 , 0 , 2 , 11 , 43 , 3 , 65 , 0 , 10 , 16 , 48 , 3 , 66 , 0 , 22 , 28 , 108 , 3 , 67 , 0 , 34 ,&
      & 40 , 168 , 3 , 80 , 11 , 12 , 14 , 46 , 2/
!
   lcore = korsz(Z)
   buf1 = lcore - Sysbuf + 1
   buf2 = buf1 - Sysbuf
   buf3 = buf2 - Sysbuf - 1
   lcore = buf3 - 1
   IF ( lcore>0 ) THEN
!
!     COUNT NUMBER OF TERMS IN A COLUMN OF HCCEN
!
      Kcount = 0
!
!     SET UP MATERIALS
!
      Iihmat = 0
      Nnhmat = lcore
      Mptfil = Mpt
      Iditfl = Dit
      CALL prehma(Z)
      nextz = Nnhmat + 1
!
      CALL gopen(Hest,Z(buf1),0)
      CALL gopen(Estfld,Z(buf2),1)
!
!     READ IN ANY BFIELD CARDS
!
      Iany = 0
      iall = 1
      ifield = 0
      idefid = 0
      nfield = 0
      file = Geom1
      CALL preloc(*1500,Z(buf3),Geom1)
      CALL locate(*200,Z(buf3),bfield,idex)
      Iany = 1
      CALL read(*1600,*100,Geom1,Z(nextz+1),lcore-nextz,0,iwords)
   ENDIF
   n = -8
   file = 0
   CALL mesage(n,file,nam)
   GOTO 99999
 100  nfield = iwords/2
   IF ( nfield/=1 .OR. Iz(nextz+2)/=-1 ) THEN
!
!     BFIELD ARE NOT THE SAME FOR EVERY ELEMENT
!
      iall = 0
   ELSE
      ifield = Iz(nextz+1)
      idefid = ifield
   ENDIF
 200  CALL close(Geom1,1)
!
!     CHECK FOR ALL SO THAT CSTM WONT BE OPENED
!
   IF ( nfield/=0 ) THEN
      DO i = 1 , iwords , 2
         IF ( Iz(nextz+i)/=0 ) GOTO 250
      ENDDO
      Iany = 0
      iall = 1
      ifield = 0
!
!     CHECK FOR A DEFAULT ID
!
 250  DO i = 2 , iwords , 2
         IF ( Iz(nextz+i)==-1 ) GOTO 300
      ENDDO
   ENDIF
   GOTO 400
 300  idefid = Iz(nextz+i-1)
 400  file = Hest
!
 500  CALL read(*1400,*1700,Hest,eltype,1,0,iflag)
   CALL write(Estfld,eltype,1,0)
   oldeid = 0
   icount = 0
   idx = (eltype-1)*Incr
   estwds = Ne(idx+12)
   ngrids = Ne(idx+10)
   frstgd = 2
   IF ( eltype>=39 .AND. eltype<=42 ) frstgd = 3
   name(1) = Ne(idx+1)
   name(2) = Ne(idx+2)
!
!     PICK UP MATERIAL ID, START OF BGPDT DATA, AND DIMENSIONALITY OF
!     ELEMENT
!
   DO i = 1 , 20
      jel = i
      IF ( eltype<pointr(1,i) ) EXIT
      IF ( eltype==pointr(1,i) ) GOTO 600
   ENDDO
!
!     FATAL ERRORS
!
   WRITE (Otpe,99001) Ufm , name
99001 FORMAT (A23,', ELEMENT TYPE ',2A4,' NOT ALLOWED IN ESTMAG')
   CALL mesage(-61,0,0)
   GOTO 1500
!
 600  ith = pointr(2,jel)
   mid = pointr(3,jel)
   isys1 = pointr(4,jel)
   isys2 = isys1 + 4
   isys3 = isys2 + 4
!
!     FOR IS2D8, USE 4TH POINT FOR GEOMETRY SINCE THAT IS WHAT WE USE
!     FOR IS2D8 ELSEWHERE
!
   IF ( eltype==80 ) isys3 = isys3 + 4
   itemp = pointr(5,jel)
   idim = pointr(6,jel)
!
 700  CALL read(*1600,*1300,Hest,ecpt,estwds,0,iflag)
   IF ( eltype<65 ) Kcount = Kcount + 3
   IF ( eltype==65 ) Kcount = Kcount + 27
   IF ( eltype==66 .OR. eltype==67 ) Kcount = Kcount + 63
   IF ( eltype==80 ) Kcount = Kcount + 27
!
!     FIND BFIELD FOR THIS ELEMENT
!
   IF ( iall/=1 ) THEN
      DO i = 2 , iwords , 2
         IF ( iecpt(1)==Iz(nextz+i) ) GOTO 800
      ENDDO
      ifield = idefid
   ENDIF
   GOTO 900
 800  ifield = Iz(nextz+i-1)
!
!     WRITE EID, SILS
!
 900  CALL write(Estfld,iecpt(1),1,0)
   CALL write(Estfld,ngrids,1,0)
   CALL write(Estfld,iecpt(frstgd),ngrids,0)
!
!     FETCH MATERIALS
!
   Matid = iecpt(mid)
   Sinth = 0.
   Costh = 0.
!***
!    ASSUME HERE THAT FOR ISOPARAMETRICS WE HAVE TEMPERATURE-INDEPENDENT
!    MATERIALS IN THIS MAGNETICS PROBLEM
!***
   Eltemp = ecpt(itemp)
   Inflag = 3
   CALL hmat(iecpt(1))
   g(1) = Xmat(1)
   g(2) = Xmat(2)
   g(3) = Xmat(3)
   g(4) = Xmat(2)
   g(5) = Xmat(4)
   g(6) = Xmat(5)
   g(7) = Xmat(3)
   g(8) = Xmat(5)
   g(9) = Xmat(6)
!
!     NOW CREATE TRANSFORMATION MATRIX FROM LOACL COORDS TO BASIC
!     DETERMINE DIMENSIONALITY OF ELEMENT
!
   IF ( idim==2 ) THEN
!
!     TWO-DIMENSIONAL  WE WILL USE ONLY GRIDS 1,2,3 ASSUMING A PLANAR
!     OR NEARLY PLANAR ELEMENT FOR QUADS
!
      DO i = 1 , 3
         v12(i) = ecpt(isys2+i) - ecpt(isys1+i)
         v13(i) = ecpt(isys3+i) - ecpt(isys1+i)
      ENDDO
      xlen = sqrt(v12(1)**2+v12(2)**2+v12(3)**2)
      DO i = 1 , 3
         xi(i) = v12(i)/xlen
      ENDDO
!
      xk(1) = xi(2)*v13(3) - xi(3)*v13(2)
      xk(2) = xi(3)*v13(1) - xi(1)*v13(3)
      xk(3) = xi(1)*v13(2) - xi(2)*v13(1)
      xlen = sqrt(xk(1)**2+xk(2)**2+xk(3)**2)
      DO i = 1 , 3
         xk(i) = xk(i)/xlen
      ENDDO
!
      xj(1) = xk(2)*xi(3) - xk(3)*xi(2)
      xj(2) = xk(3)*xi(1) - xk(1)*xi(3)
      xj(3) = xk(1)*xi(2) - xk(2)*xi(1)
      xlen = sqrt(xj(1)**2+xj(2)**2+xj(3)**2)
      DO i = 1 , 3
         xj(i) = xj(i)/xlen
      ENDDO
!
      DO i = 1 , 3
         e(3*i-2) = xi(i)
         e(3*i-1) = xj(i)
         e(3*i) = xk(i)
      ENDDO
!
!     CHECK ON MATERIALS AS IN EMRING
!
      angle = ecpt(ith)*0.017453293
      IF ( Xmat(3)==0. .AND. Xmat(5)==0. ) THEN
         IF ( abs(angle)>.0001 ) THEN
            DO i = 1 , 9
               g(i) = 0.
            ENDDO
            s = sin(angle)
            c = cos(angle)
            csq = c*c
            ssq = s*s
            cs = c*s
            x2 = 2.*cs*Xmat(2)
            g(1) = csq*Xmat(1) - x2 + ssq*Xmat(4)
            g(2) = cs*(Xmat(1)-Xmat(4)) + (csq-ssq)*Xmat(2)
            g(5) = ssq*Xmat(1) + x2 + csq*Xmat(4)
            g(4) = g(2)
            g(9) = Xmat(6)
!
            IF ( eltype==36 .OR. eltype==37 ) THEN
!
!     SINCE MAT5 INFO FOR TRAPRG,TRIARG MUST BE GIVEN IN X-Y TERMS,
!     RE-ORDER THE 3 X 3 , INTERCHANGING Y AND Z
!
               temp = g(5)
               g(5) = g(9)
               g(9) = temp
               temp = g(2)
               g(2) = g(3)
               g(3) = temp
               g(4) = g(2)
               g(7) = g(3)
            ENDIF
         ENDIF
      ENDIF
   ELSEIF ( idim==3 ) THEN
!
!     THREE-DIMENSIONAL-NO ELEMENT COORDINATE SYSTEM-EVERYTHING IS
!     OUTPUT IN BASIC- SO E IS IDENTITY
!
      DO i = 1 , 9
         e(i) = 0.
      ENDDO
      e(1) = 1.
      e(5) = 1.
      e(9) = 1.
   ELSE
!
!     ONE-DIMENSIONAL-DETERMINE THE LOCAL X-AXIS(IN BASIC COORDS)
!
      DO i = 1 , 3
         v12(i) = ecpt(isys2+i) - ecpt(isys1+i)
      ENDDO
      xlen = sqrt(v12(1)**2+v12(2)**2+v12(3)**2)
      DO i = 1 , 3
         v12(i) = v12(i)/xlen
      ENDDO
!
      DO i = 1 , 9
         e(i) = 0.
      ENDDO
      e(1) = v12(1)
      e(4) = v12(2)
      e(7) = v12(3)
   ENDIF
!
   CALL write(Estfld,g,9,0)
   CALL write(Estfld,e,9,0)
   IF ( eltype<65 .OR. eltype>67 ) THEN
!
!     COMPUTE THE AVERAGE COORDINATES OF THE GRID POINTS OF THE ELEMENT
!     FOR USE IN NON-RECTANGULAR COORDIANTE SYSTEMS. THIS POINT IS NOT
!     NECESSARILY THE CENTROID,BUT ANY POINT WILL DO FOR CONSTANT STRAIN
!     ELEMENTS AND THIS IS CONVENIENT
!
      IF ( eltype/=80 ) THEN
         DO i = 1 , 3
            coord(i) = 0.
            DO j = 1 , ngrids
               jsub = isys1 + 4*(j-1)
               coord(i) = coord(i) + ecpt(jsub+i)
            ENDDO
         ENDDO
         DO i = 1 , 3
            coord(i) = coord(i)/float(ngrids)
         ENDDO
      ELSE
!
!     FOR IS2D8 USE SHAPE FUNCTION
!
         DO i = 1 , 3
            coord(i) = 0.
            DO j = 1 , 8
               jsub = isys1 + 4*(j-1)
               coord(i) = coord(i) + dn(j)*ecpt(jsub+i)
            ENDDO
         ENDDO
      ENDIF
!
!     WRITE OUT CID AND COORDINATES
!
      CALL write(Estfld,ifield,1,0)
      GOTO 1200
!
!     ISOPARAMETRICS-PICK UP COORDS. OF APPLICABLE POINT. FOR THE LAST
!     POINT, GO TO THE PREVIOUS METHOD
!
   ELSEIF ( iecpt(1)/=oldeid ) THEN
      oldeid = iecpt(1)
   ENDIF
 1000 icount = icount + 1
   IF ( eltype/=65 .OR. icount>=9 ) THEN
      IF ( eltype<=65 .OR. icount>=21 ) THEN
!
!     CENTROIDAL POINT-COMPUTE COORDS BASED ON XI=ETA=ZETA=0
!
         icount = 0
         oldeid = 0
         IF ( eltype==65 ) THEN
            DO i = 1 , 8
               xm(i) = .125
            ENDDO
         ELSEIF ( eltype/=66 ) THEN
            con1 = 9./64.
            con2 = -19./64.
            DO i = 1 , 32
               xm(i) = con1
            ENDDO
            DO i = 1 , 10 , 3
               xm(i) = con2
               xm(i+20) = con2
            ENDDO
         ELSE
            DO i = 1 , 20
               xm(i) = .25
            ENDDO
            DO i = 1 , 7 , 2
               xm(i) = -.25
               xm(i+12) = -.25
            ENDDO
         ENDIF
!
         DO i = 1 , 3
            coord(i) = 0.
            DO j = 1 , ngrids
               jsub = isys1 + 4*(j-1)
               coord(i) = coord(i) + ecpt(jsub+i)*xm(j)
            ENDDO
         ENDDO
         GOTO 1200
      ENDIF
   ENDIF
   IF ( eltype==67 .AND. icount<21 ) THEN
!
!     FOR IHEX3, MUST GET PROPER COORDINATES IF NOT THE LAST POINT
!
      IF ( icount>=9 .AND. icount<=12 ) THEN
!
!     MIDSIDES
!
         kadd = 4
         jco = 3
      ELSEIF ( (icount/2)*2==icount ) THEN
         kadd = 1
         IF ( icount==2 .OR. icount==14 ) jco = -1
      ELSE
!
!     CORNERS
!
         IF ( icount==1 .OR. icount==13 ) jcount = -1
         iadd = 0
         IF ( icount>=13 ) iadd = 8
         jcount = jcount + 1
         num = 1
         kount(1) = icount + jcount + iadd
         GOTO 1100
      ENDIF
      iadd = 0
      IF ( icount>=14 ) iadd = 8
      jco = jco + 1
      num = 2
      kount(1) = icount + jco + iadd
      kount(2) = kount(1) + kadd
   ELSE
      jsub = isys1 + 4*(icount-1)
      DO i = 1 , 3
         coord(i) = ecpt(jsub+i)
      ENDDO
      IF ( icount<=1 ) CALL write(Estfld,ifield,1,0)
      GOTO 1200
   ENDIF
 1100 DO i = 1 , 3
      coord(i) = 0.
      DO j = 1 , num
         jsub = isys1 + 4*(kount(j)-1)
         coord(i) = coord(i) + ecpt(jsub+i)
      ENDDO
   ENDDO
   DO i = 1 , 3
      coord(i) = coord(i)/float(num)
   ENDDO
   IF ( icount<=1 ) CALL write(Estfld,ifield,1,0)
 1200 CALL write(Estfld,coord,3,0)
!
!     FOR ISOPARAMETRICS, GET COORDS OF NEXT POINT, OTHERWISE,
!     GO BACK FOR ANOTHER ELEMENT OF THIS TYPE
!
   IF ( oldeid/=0 ) GOTO 1000
   GOTO 700
!
!
!     GET ANOTHER ELEMENT TYPE
!
 1300 CALL write(Estfld,0,0,1)
   GOTO 500
!
!     DONE
!
 1400 CALL close(Estfld,1)
   CALL close(Hest,1)
   mcb(1) = Hest
   CALL rdtrl(mcb)
   mcb(1) = Estfld
   CALL wrttrl(mcb)
   RETURN
!
 1500 n = -1
   CALL mesage(n,file,nam)
   GOTO 99999
 1600 n = -2
   CALL mesage(n,file,nam)
   GOTO 99999
 1700 n = -3
   CALL mesage(n,file,nam)
99999 RETURN
END SUBROUTINE estmag
