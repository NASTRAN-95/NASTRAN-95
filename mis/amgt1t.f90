
SUBROUTINE amgt1t(Nline,Nl,Acpt,Nstns,C3t,C4t)
   IMPLICIT NONE
   INTEGER Acpt , Nl , Nline , Nstns
   REAL C3t , C4t
   REAL a1 , a2 , a3 , a4 , a5 , a6 , a7 , a8 , a9 , b1 , b2 , b3 , b4 , b5 , b6 , b7 , b8 , b9 , c1 , c2 , c3 , c4 , c5 , c6 , c7 ,&
      & c8 , c9 , data(3) , f1(3) , fn(3) , l1 , l2 , l3 , p1(3) , pn(3) , s1(3) , sn(3) , w1 , w2
   INTEGER file , i , ip1 , mm , name(2) , nskip
!
!     GENERATE CONSTANTS C3T AND C4T FOR
!     STREAMLINE NL OF SWEPT TURBOPROP BLADE.
!
   DATA file/102/ , name/4HAMGT , 4H1T  /
!
!     INPUT VARIABLES -
!     NLINE      TOTAL NO. OF STREAMLINES
!     NL         PRESENT STEAMLINE
!     ACPT       SCRATCH UNIT WITH BASIC COORDINATES OF NODES
!     NSTNS      TOTAL NO. OF STATIONS
!
!     OUTPUT VARIABLES -
!     C3T        CONSTANTS USED BY SUB. AND SUP. AERODYNAMIC ROUTINES
!     C4T        USED IN DEFINING DATA BLOCK AJJ
!
!     LOCAL VARIABLES -
!     PN         COORDINATES TRAILING EDGE PREVIOUS STREAMLINE
!     P1         COORDINATES LEADING EDGE PREVIOUS STREAMLINE
!     FN         COORDINATES TRAILING EDGE NEXT STREAMLINE
!     F1         COORDINATES LEADING EDGE NEXT STREAMLINE
!     S1         COORDINATES OF LEADING EDGE OF CURRENT STREAMLINE
!     SN         COORDINATES OF TRAILING EDGE OF CURRENT STREAMLINE
!
!     EXTRACT LEADING COORDINATES OF CURRENT STREAMLINE
!
   CALL fread(Acpt,data,3,0)
   DO i = 1 , 3
      s1(i) = data(i)
   ENDDO
!
!     SKIP TO TRAILING EDGE COORDINATES OF CURRENT STREAMLINE
!
   nskip = (2-Nstns)*3
   CALL read(*200,*100,Acpt,data,nskip,0,mm)
   CALL fread(Acpt,data,3,0)
   DO i = 1 , 3
      sn(i) = data(i)
   ENDDO
!
!     EXTRACT COORDINATES FOR PREVIOUS--P-FOR FIRST STREAMLINE
!
   IF ( Nl==1 ) THEN
      DO i = 1 , 3
         pn(i) = sn(i)
         p1(i) = s1(i)
      ENDDO
   ENDIF
!
!     NOW COORDINATES FOR NEXT -F- FOR LAST STREAMLINE
!
   IF ( Nl/=Nline ) THEN
!
!     NOW COORDINATES FOR NEXT -F- FOR ALL OTHER STREAMLINES
!
!     SKIP FIRST 10 WORDS OF NEXT STREAMLINE
!
      CALL read(*200,*100,Acpt,data,-10,0,mm)
      CALL fread(Acpt,data,3,0)
      f1(1) = data(1)
      f1(2) = data(2)
      f1(3) = data(3)
!
!     COMPUTE SKIP TO TRAILING EDGE COORDINATES
!
      nskip = (2-Nstns)*3
      CALL read(*200,*100,Acpt,data,nskip,0,mm)
      CALL fread(Acpt,data,3,0)
      fn(1) = data(1)
      fn(2) = data(2)
      fn(3) = data(3)
   ELSE
      DO i = 1 , 3
         fn(i) = sn(i)
         f1(i) = s1(i)
      ENDDO
   ENDIF
   a1 = sn(1) - s1(1)
   b1 = sn(2) - s1(2)
   c1 = sn(3) - s1(3)
!
   a2 = fn(1) - p1(1)
   b2 = fn(2) - p1(2)
   c2 = fn(3) - p1(3)
!
   a3 = pn(1) - f1(1)
   b3 = pn(2) - f1(2)
   c3 = pn(3) - f1(3)
!
   a4 = b2*c1 - b1*c2
   b4 = c2*a1 - c1*a2
   c4 = a2*b1 - a1*b2
!
   a5 = b1*c3 - b3*c1
   b5 = c1*a3 - c3*a1
   c5 = a1*b3 - a3*b1
!
   l1 = sqrt(a1**2+b1**2+c1**2)
   l2 = sqrt(a4**2+b4**2+c4**2)
   l3 = sqrt(a5**2+b5**2+c5**2)
!
   a6 = 0.5*(a4/l2+a5/l3)
   b6 = 0.5*(b4/l2+b5/l3)
   c6 = 0.5*(c4/l2+c5/l3)
!
   a7 = (b1*c6-b6*c1)/l1
   b7 = (c1*a6-c6*a1)/l1
   c7 = (a1*b6-a6*b1)/l1
!
   a8 = f1(1) - p1(1)
   b8 = f1(2) - p1(2)
   c8 = f1(3) - p1(3)
!
   a9 = fn(1) - pn(1)
   b9 = fn(2) - pn(2)
   c9 = fn(3) - pn(3)
!
   w1 = a7*a8 + b7*b8 + c7*c8
   w2 = a7*a9 + b7*b9 + c7*c9
!
   C3t = (w2-w1)/(2.0*l1)
   C4t = w1/2.0
!
   IF ( Nl==Nline ) RETURN
!
!     RETURN TO START OF RECORD
!
   CALL bckrec(Acpt)
!
!     COMPUTE SKIP TO NEXT STREAMLINE AT EXIT FROM THIS ROUTINE
!
   nskip = -6 - (10+3*Nstns)*Nl
   CALL read(*200,*100,Acpt,data,nskip,0,mm)
!
!     SET PREVIOUS COORDINATES -P- TO PRESENT STREAMLINE COORDINATES
!
   DO i = 1 , 3
      pn(i) = sn(i)
      p1(i) = s1(i)
   ENDDO
   RETURN
!
!     E-O-R    ENCOUNTERED
 100  ip1 = -3
   GOTO 300
!
!     E-O-F    ENCOUNTERED
!
 200  ip1 = -2
 300  CALL mesage(ip1,file,name)
END SUBROUTINE amgt1t