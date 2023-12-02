!*==head.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE head(Dtyp,Pltp,Mtyp,Idat)
   USE c_output
   USE c_pltdat
   USE c_xxparm
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Dtyp
   INTEGER :: Pltp
   INTEGER :: Mtyp
   INTEGER , DIMENSION(17) :: Idat
!
! Local variable declarations rewritten by SPAG
!
   REAL , SAVE :: delx
   INTEGER :: j , n
   INTEGER , DIMENSION(3) , SAVE :: maxdef , phas
   INTEGER , DIMENSION(2,3) , SAVE :: mtypf
   REAL , DIMENSION(5) , SAVE :: nt1
   REAL , DIMENSION(4) , SAVE :: nt2
   REAL , DIMENSION(3) , SAVE :: nt3
   INTEGER , DIMENSION(2,5) , SAVE :: ptyp
   INTEGER , DIMENSION(2) , SAVE :: subc
   INTEGER , DIMENSION(4) , SAVE :: undef
   REAL :: x , x0
   EXTERNAL print , typflt , typint
!
! End of declarations rewritten by SPAG
!
!
!
! ... NUMBER CHAR+2 FOR STATIC - CMODAL ... NOTE, 1 BLANK AT START...
! ... NUMBER CHAR+1 FOR DEFO - ACCEL ...
! ... NUMBER CHAR+1 FOR FREQ, EIGENV., TIME  ... IDENTIFY BY MTYP ...
! ... NUMBER OF SPACES BETWEEN IDENTIFIERS ...
   DATA undef/4HUNDE , 4HFORM , 4HED S , 4HHAPE/ , nt1/8. , 7. , 8. , 7. , 8./ , nt2 , ptyp/7. , 9. , 7. , 7. , 4HDEFO , 2HR. ,     &
       &4HVELO , 4HCITY , 4HACCE , 2HL. , 4HSTRE , 2HSS , 4HSTRA , 2HIN/ , subc/4HSUBC , 4HASE / , nt3/6. , 8. , 5./ ,              &
      & mtypf/4HFREQ , 4H.    , 4HEIGE , 4HNV.  , 4HTIME , 1H / , delx/3.0/ , maxdef/4HMAX- , 4HDEF. , 2H =/ , phas/4H PHA ,        &
      & 4HSE   , 1H /
!
   xymin(1) = 0.0
   xymin(2) = 0.0
   xymax(1) = axymax(1)
   xymax(2) = axymax(2)
   CALL print(0,0,0,0,0,-1)
   IF ( Mtyp<0 ) THEN
!
!     PRINT THE MAXIMUM DEFORMATION AT THE TOP
!
      CALL print(20.*cntx,xymax(2),1,maxdef,3,0)
      CALL typflt(31.*cntx,xymax(2),1,Idat(1),-10,0)
   ELSE
!
!     LEFT-MOST CHARACTER MAY NOT BE COMPETELY DRAWN IF FRACTION OF
!     CSCALE IS IS LESS THAN 0.5. SO MOVE OVER A SMALL SPACE OF X0
!
      j = ifix(cscale)
      x0 = cscale - float(j)
      IF ( x0>0.5 ) x0 = 0.0
!
!     PRINT THE TITLE, SUBTITLE AND LABEL
!
      CALL print(x0,3.0*cnty,1,title(1,1),17,0)
      CALL print(x0,2.0*cnty,1,title(1,2),16,0)
      CALL print(x0,cnty,1,title(1,3),17,0)
!
      x = 25. - 5.*(cscale-1.)
      IF ( Dtyp/=0 ) THEN
         x = 40.
         IF ( Idat(1)>8 ) THEN
            x = 45.
            IF ( Idat(1)>=12 ) x = 52.
            IF ( Idat(1)>=15 ) x = 59.
         ENDIF
      ENDIF
      IF ( fpltit/=0 ) CALL print(x*cntx,0.,1,pltitl,17,0)
!
!     BOTTOM LINE IDENTIFIES PLOT
!
      IF ( Dtyp/=0 ) THEN
!
!     DEFORMED SHAPE
!
         CALL print(cntx+x0,0.,1,Idat(3),2,0)
         x = nt1(Dtyp)
         CALL print(x*cntx+x0,0.,1,ptyp(1,Pltp),2,0)
         x = x + nt2(Pltp)
         CALL print(x*cntx+x0,0.,1,subc,2,0)
         x = x + 8.
         n = -1
         CALL typint(x*cntx+x0,0.,1,Idat(7),n,0)
         x = x + float(n) + delx
!
!     LOAD I  OR  MODE I
!
         CALL print(x*cntx+x0,0.,1,Idat(9),1,0)
         x = x + 5.
         n = -1
         CALL typint(x*cntx+x0,0.,1,Idat(8),n,0)
!
!     FREQUENCY, EIGENVALUE, OR TIME
!
         IF ( Idat(1)>8 ) THEN
            x = float(ifix(x+delx+0.1)+n)
            CALL print(x*cntx+x0,0.,1,mtypf(1,Mtyp),2,0)
            x = x + nt3(Mtyp)
            CALL typflt(x*cntx+x0,0.,1,Idat(10),-8,0)
!
!     MAGNITUDE  OR  PHASE LAG
!
            IF ( Idat(1)>12 ) THEN
               x = x + 7.0 + delx
               IF ( Idat(14)==phas(1) ) THEN
                  Idat(15) = phas(2)
                  Idat(16) = phas(3)
               ENDIF
               CALL print(x*cntx+x0,0.,1,Idat(14),3,0)
!
               IF ( Idat(1)>15 ) THEN
                  x = x + 7.0
                  CALL typflt(x*cntx+x0,0.,1,Idat(17),-6,0)
               ENDIF
            ENDIF
         ENDIF
      ELSE
!
!     UNDEFORMED SHAPE
!
         CALL print(cntx+x0,0.,1,undef,4,0)
      ENDIF
   ENDIF
!
!
   CALL print(0,0,0,0,0,1)
END SUBROUTINE head
