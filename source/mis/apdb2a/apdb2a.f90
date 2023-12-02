!*==apdb2a.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE apdb2a(Nline,Nl,Scr1,Nstns,M1,S1,Sn,Tblt,Tblr)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nline
   INTEGER :: Nl
   INTEGER :: Scr1
   INTEGER :: Nstns
   REAL :: M1
   REAL , DIMENSION(3) :: S1
   REAL , DIMENSION(3) :: Sn
   REAL , DIMENSION(3) :: Tblt
   REAL , DIMENSION(3) :: Tblr
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a1 , a2 , a3 , a4 , a5 , a6 , b1 , b2 , b3 , b4 , b5 , b6 , c1 , c2 , c3 , c4 , c5 , c6 , l1 , l2 , l3
   REAL , DIMENSION(7) :: data
   REAL , DIMENSION(3) :: f1 , fn , p1 , pn
   INTEGER , SAVE :: file
   INTEGER :: i , ip1 , mm , nskip
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL bckrec , fread , mesage , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     GENERATE BASIC TO LOCAL TRANSFORMATION MATRIX FOR
!     STREAMLINE NL OF SWEPT TURBOPROP BLADE.
!
!
!
!
   DATA file/301/ , name/4HAPDB , 4H2A  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!---------------------------------------------------------------------
!     INPUT VARIABLES--
!     NLINE      TOTAL NO. OF STREAMLINES
!     NL         PRESENT STEAMLINE
!     SCR1       SCRATCH UNIT WITH BASIC COORDINATES OF NODES
!     NSTNS      TOTAL NO. OF STATIONS
!     M1         SIGN BASED ON ROTATION OF BLADE
!     S1         COORDINATES OF LEADING EDGE OF CURRENT STREAMLINE
!     SN         COORDINATES OF TRAILING EDGE OF CURRENT STREAMLINE
!
!     OUTPUT VARIABLES--
!     TBLT      BASIC TO LOCAL TRANSFORMATION FOR TRANSLATION
!     TBLR      BASIC TO LOCAL TRANSFORMATION FOR ROTATION
!
!     LOCAL VARIABLES--
!     PN        COORDINATES TRAILING EDGE PREVIOUS STREAMLINE
!     P1        COORDINATES LEADING EDGE PREVIOUS STREAMLINE
!     FN        COORDINATES TRAILING EDGE NEXT STREAMLINE
!     F1        COORDINATES LEADING EDGE NEXT STREAMLINE
!---------------------------------------------------------------------
!     EXTRACT COORDINATES FOR PREVIOUS--P-FOR FIRST STREAMLINE
!---------------------------------------------------------------------
         IF ( Nl==1 ) THEN
            DO i = 1 , 3
               pn(i) = Sn(i)
               p1(i) = S1(i)
            ENDDO
         ENDIF
!----------------------------------------------------------------------
!     NOW COORDINATES FOR NEXT--F-FOR LAST STREAMLINE
!----------------------------------------------------------------------
         IF ( Nl/=Nline ) THEN
!----------------------------------------------------------------------
!     NOW COORDINATES FOR NEXT--F-FOR ALL OTHER STREAMLINES
!---------------------------------------------------------------------
            CALL fread(Scr1,data,7,0)
            f1(1) = data(5)
            f1(2) = data(6)
            f1(3) = data(7)
!----------------------------------------------------------------------
!    COMPUTE SKIP TO TRAILING EDGE COORDINATES
!-----------------------------------------------------------------------
            nskip = (2-Nstns)*7
            CALL read(*40,*20,Scr1,data,nskip,0,mm)
            CALL fread(Scr1,data,7,0)
            fn(1) = data(5)
            fn(2) = data(6)
            fn(3) = data(7)
!----------------------------------------------------------------------
!     RETURN TO START OF RECORD
!----------------------------------------------------------------------
            CALL bckrec(Scr1)
!---------------------------------------------------------------------
!     COMPUTE SKIP TO ORIGINAL LOCATION AT ENTRY TO THIS ROUTINE
!---------------------------------------------------------------------
            nskip = -Nstns*Nl*7
            CALL read(*40,*20,Scr1,data,nskip,0,mm)
         ELSE
            DO i = 1 , 3
               fn(i) = Sn(i)
               f1(i) = S1(i)
            ENDDO
         ENDIF
         a1 = Sn(1) - S1(1)
         b1 = Sn(2) - S1(2)
         c1 = Sn(3) - S1(3)
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
!---------------------------------------------------------------------
!     BASIC TO LOCAL TRANSFORMATION FOR TRANSLATION
!---------------------------------------------------------------------
         Tblt(1) = a6*M1
         Tblt(2) = b6*M1
         Tblt(3) = c6*M1
!----------------------------------------------------------------------
!     BASIC TO LOCAL TRANSFORMATION FOR ROTATION
!---------------------------------------------------------------------
         Tblr(1) = -M1*a1/l1
         Tblr(2) = -M1*b1/l1
         Tblr(3) = -M1*c1/l1
         IF ( Nl==Nline ) RETURN
!---------------------------------------------------------------------
!     SET PREVIOUS COORDINATES--P- TO PRESENT STREAMLINE
!---------------------------------------------------------------------
         DO i = 1 , 3
            pn(i) = Sn(i)
            p1(i) = S1(i)
         ENDDO
         RETURN
!     E-O-R    ENCOUNTERED
 20      ip1 = -3
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!     E-O-F    ENCOUNTERED
 40      ip1 = -2
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(ip1,file,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE apdb2a
