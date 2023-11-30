
SUBROUTINE head(Dtyp,Pltp,Mtyp,Idat)
   IMPLICIT NONE
   REAL Axymax(13) , Cntx , Cnty , Cscale , Skpa(3) , Skpplt(2) , Title(32,3) , Xymax(2) , Xymin(2)
   INTEGER Fpltit , Iskp(215) , Pltitl(17)
   COMMON /output/ Title
   COMMON /pltdat/ Skpplt , Xymin , Xymax , Axymax , Cscale , Skpa , Cntx , Cnty
   COMMON /xxparm/ Iskp , Fpltit , Pltitl
   INTEGER Dtyp , Mtyp , Pltp
   INTEGER Idat(17)
   REAL delx , nt1(5) , nt2(4) , nt3(3) , x , x0
   INTEGER j , maxdef(3) , mtypf(2,3) , n , phas(3) , ptyp(2,5) , subc(2) , undef(4)
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
   Xymin(1) = 0.0
   Xymin(2) = 0.0
   Xymax(1) = Axymax(1)
   Xymax(2) = Axymax(2)
   CALL print(0,0,0,0,0,-1)
   IF ( Mtyp<0 ) THEN
!
!     PRINT THE MAXIMUM DEFORMATION AT THE TOP
!
      CALL print(20.*Cntx,Xymax(2),1,maxdef,3,0)
      CALL typflt(31.*Cntx,Xymax(2),1,Idat(1),-10,0)
   ELSE
!
!     LEFT-MOST CHARACTER MAY NOT BE COMPETELY DRAWN IF FRACTION OF
!     CSCALE IS IS LESS THAN 0.5. SO MOVE OVER A SMALL SPACE OF X0
!
      j = ifix(Cscale)
      x0 = Cscale - float(j)
      IF ( x0>0.5 ) x0 = 0.0
!
!     PRINT THE TITLE, SUBTITLE AND LABEL
!
      CALL print(x0,3.0*Cnty,1,Title(1,1),17,0)
      CALL print(x0,2.0*Cnty,1,Title(1,2),16,0)
      CALL print(x0,Cnty,1,Title(1,3),17,0)
!
      x = 25. - 5.*(Cscale-1.)
      IF ( Dtyp/=0 ) THEN
         x = 40.
         IF ( Idat(1)>8 ) THEN
            x = 45.
            IF ( Idat(1)>=12 ) x = 52.
            IF ( Idat(1)>=15 ) x = 59.
         ENDIF
      ENDIF
      IF ( Fpltit/=0 ) CALL print(x*Cntx,0.,1,Pltitl,17,0)
!
!     BOTTOM LINE IDENTIFIES PLOT
!
      IF ( Dtyp/=0 ) THEN
!
!     DEFORMED SHAPE
!
         CALL print(Cntx+x0,0.,1,Idat(3),2,0)
         x = nt1(Dtyp)
         CALL print(x*Cntx+x0,0.,1,ptyp(1,Pltp),2,0)
         x = x + nt2(Pltp)
         CALL print(x*Cntx+x0,0.,1,subc,2,0)
         x = x + 8.
         n = -1
         CALL typint(x*Cntx+x0,0.,1,Idat(7),n,0)
         x = x + float(n) + delx
!
!     LOAD I  OR  MODE I
!
         CALL print(x*Cntx+x0,0.,1,Idat(9),1,0)
         x = x + 5.
         n = -1
         CALL typint(x*Cntx+x0,0.,1,Idat(8),n,0)
!
!     FREQUENCY, EIGENVALUE, OR TIME
!
         IF ( Idat(1)>8 ) THEN
            x = float(ifix(x+delx+0.1)+n)
            CALL print(x*Cntx+x0,0.,1,mtypf(1,Mtyp),2,0)
            x = x + nt3(Mtyp)
            CALL typflt(x*Cntx+x0,0.,1,Idat(10),-8,0)
!
!     MAGNITUDE  OR  PHASE LAG
!
            IF ( Idat(1)>12 ) THEN
               x = x + 7.0 + delx
               IF ( Idat(14)==phas(1) ) THEN
                  Idat(15) = phas(2)
                  Idat(16) = phas(3)
               ENDIF
               CALL print(x*Cntx+x0,0.,1,Idat(14),3,0)
!
               IF ( Idat(1)>15 ) THEN
                  x = x + 7.0
                  CALL typflt(x*Cntx+x0,0.,1,Idat(17),-6,0)
               ENDIF
            ENDIF
         ENDIF
      ELSE
!
!     UNDEFORMED SHAPE
!
         CALL print(Cntx+x0,0.,1,undef,4,0)
      ENDIF
   ENDIF
!
!
   CALL print(0,0,0,0,0,1)
END SUBROUTINE head