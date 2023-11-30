
SUBROUTINE ihex(Temps,Pg,Type)
   IMPLICIT NONE
   REAL Bufm6(46) , Cdamp , Est(200) , G , H(4) , Heat , Rho , Se , Snu , Space(18) , Sys1(7) , Sys2(45) , Sysbuf , Talpha , Tref
   DOUBLE PRECISION Dshp(3,32) , Eltemp , Jacob(3,3) , S(4) , Shp(32)
   INTEGER Eid , Ib(46) , Iest(1) , Inflag , Jz(32) , Mid , Mtemp , Otpt
   LOGICAL Mtdep
   COMMON /matin / Mid , Inflag , Eltemp
   COMMON /matiso/ Bufm6
   COMMON /matout/ Se , G , Snu , Rho , Talpha , Tref , Cdamp , Space , Mtdep
   COMMON /ssgwrk/ Shp , Dshp , Jacob , S , H
   COMMON /system/ Sysbuf , Otpt , Sys1 , Mtemp , Sys2 , Heat
   COMMON /trimex/ Est
   INTEGER Type
   REAL Pg(1) , Temps(1)
   DOUBLE PRECISION a(6) , alpvec , cn(3,32) , dalpha(6) , detj , e1 , e2 , e3 , gauss(8) , gmat(36) , parg(96) , sfact , temp
   LOGICAL anis , rect , tdep
   INTEGER bcord , bgpdt , cid , gpt , i , ibgp , ii , ijk , j , jj , k , l , ngp , nip , nogo , ntlp , sil , ufm(6)
   REAL psgl(96)
!
!     ELEMENT THERMAL LOAD GENERATOR FOR ISOPARAMETRIC SOLID ELEMENTS
!
!     TYPE = 1     CIHEX1
!     TYPE = 2     CIHEX2
!     TYPE = 3     CIHEX3
!
!***********************************************************************
!           THE EST ENTRIES ARE
!
!     NAME  ---------INDEX---------   DESCRIPTION
!            IHEX1   IHEX2   IHEX3
!
!     EID        1       1       1    ELEMENT ID NO.
!     SIL      2-9    2-21    2-33    SCALAR INDEX LIST
!     MID       10      22      34    MATERIAL ID NO.
!     CID       11      23      35    MATERIAL COORD. SYSTEM ID NO.
!     NIP       12      24      36    NO. INTEGRATION POINTS PER EDGE
!     MAXAR     13      25      37    MAX ASPECT RATIO
!     ALFA      14      26      38    MAX ANGLE FOR NORMALS
!     BETA      15      27      39    MAX ANGLE FOR MIDSIDE POINTS
!     BGPDT  16-47  28-107  40-167    BASIC GRID POINT DATA
!     GPT    48-55 108-127 168-199    GRID POINT TEMPERATURES
!***********************************************************************
!
!
!
!
!
!
!
   !>>>>EQUIVALENCE (Eid,Est(1),Iest(1)) , (Jz(1),Shp(1))
   !>>>>EQUIVALENCE (psgl(1),parg(1))
   !>>>>EQUIVALENCE (Ib(1),Bufm6(1))
!
   DATA gauss/0.577350269189626D0 , 0.555555555555556D0 , 0.774596669241483D0 , 0.888888888888889D0 , 0.347854845137454D0 ,         &
      & 0.861136311594053D0 , 0.652145154862546D0 , 0.339981043584856D0/
   DATA ufm/4H0*** , 4H USE , 4HR FA , 4HTAL  , 4HMESS , 4HAGE /
!
!*****
!     COMPUTE EST POINTERS
!*****
   ngp = 12*Type - 4
   Mid = 10 + 12*(Type-1)
   cid = Iest(Mid+1)
   nip = Iest(Mid+2)
   IF ( nip<2 .OR. nip>4 ) nip = Type/2 + 2
   bgpdt = Mid + 6
   gpt = bgpdt + 4*ngp
   DO i = 1 , ngp
      Jz(i) = Iest(bgpdt+4*i-4)
   ENDDO
   bcord = gpt - 3
   DO i = 2 , ngp
      DO j = 1 , 3
         k = bgpdt + 4*(ngp-i) + 4 - j
         bcord = bcord - 1
         Est(bcord) = Est(k)
      ENDDO
   ENDDO
   DO i = 2 , ngp
      Iest(bgpdt+i-1) = Jz(i)
   ENDDO
   Mid = Iest(ngp+2)
!
!     ABSCISSAE AND WEIGHT COEFFICIENTS FOR GAUSSIAN QUADRATURE
!
   i = nip - 1
   IF ( i==2 ) THEN
      H(1) = gauss(2)
      S(1) = gauss(3)
      H(2) = gauss(4)
      S(2) = 0.0
      H(3) = H(1)
      S(3) = -S(1)
   ELSEIF ( i==3 ) THEN
      H(1) = gauss(5)
      S(1) = gauss(6)
      H(2) = gauss(7)
      S(2) = gauss(8)
      H(3) = H(2)
      S(3) = -S(2)
      H(4) = H(1)
      S(4) = -S(1)
   ELSE
      H(1) = 1.0
      S(1) = gauss(1)
      H(2) = H(1)
      S(2) = -S(1)
   ENDIF
!
!=======================================================================
!     THIS SECTION OF CODE MUST BE UPDATED WHEN GENERAL ANISOTROPIC
!     MATERIAL IS ADDED
!
!     TEST FOR ANISOTROPIC MATERIAL
!
   anis = .FALSE.
   Inflag = 10
!
!     TEST FOR RECTANGULAR COORDINATE SYSTEM IN WHICH THE ANISOTROPIC
!     MATERIAL IS DEFINED
!
   rect = .TRUE.
!=======================================================================
!
!     FETCH MATERIAL AND SET TEMPERATURE DEPENDENCE FLAG
!
   tdep = .TRUE.
   DO i = 2 , ngp
      IF ( Est(gpt)/=Est(gpt+i-1) ) GOTO 100
   ENDDO
   tdep = .FALSE.
 100  Eltemp = Est(gpt)
   CALL mat(Eid)
   IF ( .NOT.Mtdep ) tdep = .FALSE.
   IF ( Ib(46)==6 ) anis = .TRUE.
   Tref = Bufm6(44)
!*****
!     IF ISOTROPIC TEMPERATURE INDEPENDENT MATERIAL, COMPUTE CONSTANTS
!*****
   IF ( .NOT.(tdep) ) THEN
      IF ( anis ) THEN
!
!=======================================================================
!     CODE TO TRANSFORM GENERAL ANISOTROPIC MATERIAL PROPERTIES TO
!     BASIC COORDINATE SYSTEM MUST BE ADDED HERE
!=======================================================================
!
         DO ijk = 1 , 36
            gmat(ijk) = Bufm6(ijk)
         ENDDO
      ELSEIF ( Ib(46)/=0 ) THEN
         e1 = Bufm6(1)
         e2 = Bufm6(2)
         e3 = Bufm6(22)
         Talpha = Bufm6(38)
      ELSE
         CALL page2(2)
         WRITE (Otpt,99001) ufm , Mid , Eid
         nogo = 1
         RETURN
      ENDIF
   ENDIF
   ntlp = 3*ngp
   DO i = 1 , ntlp
      parg(i) = 0.0
   ENDDO
!*****
!     BEGIN INTEGRATION LOOP NOW
!*****
   DO i = 1 , nip
      DO j = 1 , nip
         DO k = 1 , nip
!*****
!     GENERATE SHAPE FUNCTIONS AND JACOBIAN MATRIX INVERSE
!*****
            CALL ihexsd(Type,Shp,Dshp,Jacob,detj,Eid,S(i),S(j),S(k),Est(bcord))
!
!     JACOBIAN MATRIX WAS SINGULAR
!
            IF ( detj==0.0D0 ) CALL mesage(-61,0,0)
!*****
!     COMPUTE PARTIAL DERIVATIVE OF SHAPE FUNCTIONS WITH RESPECT
!     TO BASIC COORDINATES
!*****
            CALL gmmatd(Dshp,ngp,3,0,Jacob,3,3,0,cn)
!*****
!     COMPUTE LOADING TEMPERATURE AT THIS INTEGRATION POINT
!*****
            temp = 0.0D0
            DO l = 1 , ngp
               temp = temp + Shp(l)*dble(Temps(l))
            ENDDO
            temp = temp - dble(Tref)
!*****
!     IF MATERIAL IS TEMPERATURE DEPENDENT, COMPUTE TEMPERATURE AT THIS
!     INTEGRATION POINT AND FETCH MATERIAL PROPERTIES
!*****
            IF ( .NOT.tdep ) THEN
!*****
!     IF MATERIAL IS ANISOTROPIC AND NOT DEFINED IN RECTANGULAR COOR-
!     DINATE SYSTEM, MUST TRANSFORM TO BASIC COORDINATE SYSTEM AT THIS
!     INTEGRATION POINT
!*****
               IF ( .NOT.anis ) GOTO 110
               IF ( rect ) GOTO 120
            ELSE
               Eltemp = 0.0D0
               DO l = 1 , ngp
                  Eltemp = Eltemp + Shp(l)*dble(Est(gpt+l-1))
               ENDDO
               CALL mat(Eid)
               IF ( .NOT.(anis) ) THEN
                  IF ( Ib(46)/=0 ) THEN
                     e1 = Bufm6(1)
                     e2 = Bufm6(2)
                     e3 = Bufm6(22)
                     Talpha = Bufm6(38)
                     GOTO 110
                  ELSE
                     CALL page2(2)
                     WRITE (Otpt,99001) ufm , Mid , Eid
                     nogo = 1
                     RETURN
                  ENDIF
               ENDIF
            ENDIF
            DO ijk = 1 , 36
               gmat(ijk) = Bufm6(ijk)
            ENDDO
!
!=======================================================================
!     INSERT GLOBAL TO BASIC TRANSFORMATION OPERATIONS HERE FOR
!     ANISOTROPIC MATERIAL MATRIX
            GOTO 120
!=======================================================================
!*****
!     COMPUTE CONTRIBUTION TO THERMAL LOAD VECTOR FOR ISOTROPIC MATERIAL
!*****
 110        alpvec = dble(Talpha)*(e1+2.0*e2)
            sfact = H(i)*H(j)*H(k)*detj*alpvec*temp
            l = 0
            DO ii = 1 , ngp
               DO jj = 1 , 3
                  l = l + 1
                  parg(l) = sfact*cn(jj,ii) + parg(l)
               ENDDO
            ENDDO
            CYCLE
!=======================================================================
!     ADD LOAD COMPUTATIONS FOR ANISOTROPIC MATERIAL HERE
!=======================================================================
!
 120        sfact = H(i)*H(j)*H(k)*detj*temp
            DO ijk = 1 , 6
               dalpha(ijk) = Bufm6(ijk+37)
            ENDDO
!
            CALL gmmatd(gmat,6,6,0,dalpha,6,1,0,a(1))
            l = 0
            DO ii = 1 , ngp
               l = l + 1
               parg(l) = parg(l) + sfact*(cn(1,ii)*a(1)+cn(2,ii)*a(4)+cn(3,ii)*a(6))
               l = l + 1
               parg(l) = parg(l) + sfact*(cn(2,ii)*a(2)+cn(1,ii)*a(4)+cn(3,ii)*a(5))
               l = l + 1
               parg(l) = parg(l) + sfact*(cn(3,ii)*a(3)+cn(2,ii)*a(5)+cn(1,ii)*a(6))
            ENDDO
         ENDDO
      ENDDO
   ENDDO
   DO i = 1 , ntlp
      psgl(i) = parg(i)
   ENDDO
!*****
!     INSERT THERMAL LOAD INTO GLOBAL LOAD VECTOR  (PG ARRAY)
!*****
!
   DO i = 1 , ngp
      sil = Iest(i+1)
      ibgp = bgpdt + i - 1
      IF ( Iest(ibgp)/=0 ) CALL basglb(psgl(3*i-2),psgl(3*i-2),Est(bcord+3*i-3),Iest(ibgp))
      DO j = 1 , 3
         Pg(sil+j-1) = Pg(sil+j-1) + psgl(3*i-3+j)
      ENDDO
   ENDDO
!
!
99001 FORMAT (6A4,69H4005. AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED UNDER MATERIAL ID =,I10,17H FOR ELEMENT ID =,I10)
END SUBROUTINE ihex