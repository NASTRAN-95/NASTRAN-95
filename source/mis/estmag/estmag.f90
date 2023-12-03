!*==estmag.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE estmag(Hest,Estfld,Mpt,Dit,Geom1,Iany,Kcount)
   USE c_gpta1
   USE c_hmatdd
   USE c_hmtout
   USE c_matin
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Hest
   INTEGER :: Estfld
   INTEGER :: Mpt
   INTEGER :: Dit
   INTEGER :: Geom1
   INTEGER :: Iany
   INTEGER :: Kcount
!
! Local variable declarations rewritten by SPAG
!
   REAL :: angle , c , con1 , con2 , cs , csq , s , ssq , temp , x2 , xlen
   INTEGER , DIMENSION(2) , SAVE :: bfield , nam
   INTEGER :: buf1 , buf2 , buf3 , eltype , estwds , file , frstgd , i , iadd , iall , icount , idefid , idex , idim , idx ,        &
            & ifield , iflag , isys1 , isys2 , isys3 , itemp , ith , iwords , j , jco , jcount , jel , jsub , kadd , lcore , mid ,  &
            & n , nextz , nfield , ngrids , num , oldeid
   REAL , DIMENSION(3) :: coord , v12 , v13 , xi , xj , xk
   REAL , DIMENSION(8) , SAVE :: dn
   REAL , DIMENSION(9) :: e , g
   REAL , DIMENSION(200) :: ecpt
   INTEGER , DIMENSION(200) :: iecpt
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) :: kount , name
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(6,20) , SAVE :: pointr
   REAL , DIMENSION(32) :: xm
   EXTERNAL close , gopen , hmat , korsz , locate , mesage , prehma , preloc , rdtrl , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     CREATE SCRATCH FILE ESTFLD WHICH WILL BE USED TO COMPUTE TOTAL
!     MAGNETIC FIELD. READ EST AND CREATE SIMILAR RECORDS CONTAINING
!     ELTYPE,EID,NUMBER OF SILS,SILS,3 X 3 MATERIALS MATRIX, AND 3 X 3
!     TRANSFORMATION MATRIX TO BRING HM BACK TO BASIC COORD. SYSTEM
!     FROM ELEMENT SYSTEM,OUTPUT COORD. SYSTEM ID, AND BASIC COORDS.
!     OF STRESS POINT(USUALLY AVERAGE OF GRID COORDS.)
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (ecpt(1),iecpt(1))
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         lcore = korsz(z)
         buf1 = lcore - sysbuf + 1
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf - 1
         lcore = buf3 - 1
         IF ( lcore>0 ) THEN
!
!     COUNT NUMBER OF TERMS IN A COLUMN OF HCCEN
!
            Kcount = 0
!
!     SET UP MATERIALS
!
            iihmat = 0
            nnhmat = lcore
            mptfil = Mpt
            iditfl = Dit
            CALL prehma(z)
            nextz = nnhmat + 1
!
            CALL gopen(Hest,z(buf1),0)
            CALL gopen(Estfld,z(buf2),1)
!
!     READ IN ANY BFIELD CARDS
!
            Iany = 0
            iall = 1
            ifield = 0
            idefid = 0
            nfield = 0
            file = Geom1
            CALL preloc(*100,z(buf3),Geom1)
            CALL locate(*40,z(buf3),bfield,idex)
            Iany = 1
            CALL read(*120,*20,Geom1,z(nextz+1),lcore-nextz,0,iwords)
         ENDIF
         n = -8
         file = 0
         CALL mesage(n,file,nam)
         RETURN
 20      nfield = iwords/2
         IF ( nfield/=1 .OR. iz(nextz+2)/=-1 ) THEN
!
!     BFIELD ARE NOT THE SAME FOR EVERY ELEMENT
!
            iall = 0
         ELSE
            ifield = iz(nextz+1)
            idefid = ifield
         ENDIF
 40      CALL close(Geom1,1)
!
!     CHECK FOR ALL SO THAT CSTM WONT BE OPENED
!
         IF ( nfield/=0 ) THEN
            DO i = 1 , iwords , 2
               IF ( iz(nextz+i)/=0 ) GOTO 50
            ENDDO
            Iany = 0
            iall = 1
            ifield = 0
!
!     CHECK FOR A DEFAULT ID
!
 50         DO i = 2 , iwords , 2
               IF ( iz(nextz+i)==-1 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 3
      CASE (2)
         idefid = iz(nextz+i-1)
         spag_nextblock_1 = 3
      CASE (3)
         file = Hest
         spag_nextblock_1 = 4
      CASE (4)
!
         CALL read(*80,*140,Hest,eltype,1,0,iflag)
         CALL write(Estfld,eltype,1,0)
         oldeid = 0
         icount = 0
         idx = (eltype-1)*incr
         estwds = ne(idx+12)
         ngrids = ne(idx+10)
         frstgd = 2
         IF ( eltype>=39 .AND. eltype<=42 ) frstgd = 3
         name(1) = ne(idx+1)
         name(2) = ne(idx+2)
!
!     PICK UP MATERIAL ID, START OF BGPDT DATA, AND DIMENSIONALITY OF
!     ELEMENT
!
         SPAG_Loop_1_1: DO i = 1 , 20
            jel = i
            IF ( eltype<pointr(1,i) ) EXIT SPAG_Loop_1_1
            IF ( eltype==pointr(1,i) ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     FATAL ERRORS
!
         WRITE (otpe,99001) ufm , name
99001    FORMAT (A23,', ELEMENT TYPE ',2A4,' NOT ALLOWED IN ESTMAG')
         CALL mesage(-61,0,0)
         GOTO 100
      CASE (5)
!
         ith = pointr(2,jel)
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
         spag_nextblock_1 = 6
      CASE (6)
!
         CALL read(*120,*60,Hest,ecpt,estwds,0,iflag)
         IF ( eltype<65 ) Kcount = Kcount + 3
         IF ( eltype==65 ) Kcount = Kcount + 27
         IF ( eltype==66 .OR. eltype==67 ) Kcount = Kcount + 63
         IF ( eltype==80 ) Kcount = Kcount + 27
!
!     FIND BFIELD FOR THIS ELEMENT
!
         IF ( iall/=1 ) THEN
            DO i = 2 , iwords , 2
               IF ( iecpt(1)==iz(nextz+i) ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            ifield = idefid
         ENDIF
         spag_nextblock_1 = 8
      CASE (7)
         ifield = iz(nextz+i-1)
         spag_nextblock_1 = 8
      CASE (8)
!
!     WRITE EID, SILS
!
         CALL write(Estfld,iecpt(1),1,0)
         CALL write(Estfld,ngrids,1,0)
         CALL write(Estfld,iecpt(frstgd),ngrids,0)
!
!     FETCH MATERIALS
!
         matid = iecpt(mid)
         sinth = 0.
         costh = 0.
!***
!    ASSUME HERE THAT FOR ISOPARAMETRICS WE HAVE TEMPERATURE-INDEPENDENT
!    MATERIALS IN THIS MAGNETICS PROBLEM
!***
         eltemp = ecpt(itemp)
         inflag = 3
         CALL hmat(iecpt(1))
         g(1) = xmat(1)
         g(2) = xmat(2)
         g(3) = xmat(3)
         g(4) = xmat(2)
         g(5) = xmat(4)
         g(6) = xmat(5)
         g(7) = xmat(3)
         g(8) = xmat(5)
         g(9) = xmat(6)
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
            IF ( xmat(3)==0. .AND. xmat(5)==0. ) THEN
               IF ( abs(angle)>.0001 ) THEN
                  DO i = 1 , 9
                     g(i) = 0.
                  ENDDO
                  s = sin(angle)
                  c = cos(angle)
                  csq = c*c
                  ssq = s*s
                  cs = c*s
                  x2 = 2.*cs*xmat(2)
                  g(1) = csq*xmat(1) - x2 + ssq*xmat(4)
                  g(2) = cs*(xmat(1)-xmat(4)) + (csq-ssq)*xmat(2)
                  g(5) = ssq*xmat(1) + x2 + csq*xmat(4)
                  g(4) = g(2)
                  g(9) = xmat(6)
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
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
!
!     ISOPARAMETRICS-PICK UP COORDS. OF APPLICABLE POINT. FOR THE LAST
!     POINT, GO TO THE PREVIOUS METHOD
!
         ELSEIF ( iecpt(1)/=oldeid ) THEN
            oldeid = iecpt(1)
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         icount = icount + 1
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
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
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
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
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
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
         DO i = 1 , 3
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
         spag_nextblock_1 = 11
      CASE (11)
         CALL write(Estfld,coord,3,0)
!
!     FOR ISOPARAMETRICS, GET COORDS OF NEXT POINT, OTHERWISE,
!     GO BACK FOR ANOTHER ELEMENT OF THIS TYPE
!
         IF ( oldeid==0 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
!
!     GET ANOTHER ELEMENT TYPE
!
 60      CALL write(Estfld,0,0,1)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!     DONE
!
 80      CALL close(Estfld,1)
         CALL close(Hest,1)
         mcb(1) = Hest
         CALL rdtrl(mcb)
         mcb(1) = Estfld
         CALL wrttrl(mcb)
         RETURN
!
 100     n = -1
         CALL mesage(n,file,nam)
         RETURN
 120     n = -2
         CALL mesage(n,file,nam)
         RETURN
 140     n = -3
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE estmag