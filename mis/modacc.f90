
SUBROUTINE modacc
   IMPLICIT NONE
   INTEGER Id , Iop(2) , Iz(1) , Nfn , Nfo , Nz
   COMMON /blank / Iop
   COMMON /modac3/ Nfo , Nfn , Nz , Id
   COMMON /zzzzzz/ Iz
   INTEGER casecc , iceign , id1 , ireig , istat , itran , j , pdt , pdt3 , pp3 , ppt , pst , pst3 , tol , tol1 , udv1t , udv3t
   INTEGER korsz
!
!     THIS IS THE MODULE MODACC
!
!     DMAP CALL
!
!     MODACC  CASECC,TOL,UDV1T,PPT,PDT,PST/TOL1,UDV3T,PP3,PDT3,PST3/
!             C,N,TRAN $
!
!     THE PURPOSE OF THIS MODULE IS TO REDUCE THE COLUMN LENCTHS OF
!     UDV1T,PPT,PDT,PST  TO THE  LENGTH SPECIFIED BY OFREQ IN CASECC.
!     THE CURRENT LIST OF TIMES IS ON  TOL
!
   DATA casecc , tol , udv1t , ppt , pdt , pst , tol1 , udv3t , pp3 , pdt3 , pst3/101 , 102 , 103 , 104 , 105 , 106 , 201 , 202 ,   &
      & 203 , 204 , 205/
   DATA itran/4HTRAN/ , iceign/4HCEIG/
   DATA ireig/4HREIG/
   DATA istat/4HSTAT/
!
   Id = 1
   IF ( Iop(1)==iceign ) Id = 2
   IF ( Iop(1)==itran ) Id = 3
   IF ( Iop(1)==ireig ) Id = 4
   IF ( Iop(1)==istat ) Id = 5
!
!     FOR EIGENVALUES STOP LIST AT NUMBER OF VECTORS
!
   Nfo = 0
   Iz(1) = udv1t
   CALL rdtrl(Iz)
   j = 2
   Nfo = 2*Iz(j)
   Nz = korsz(Iz(1))
!
!     BUILD LIST OF NEW TIMES, KEEP/REMOVE LIST
!
   CALL modac1(casecc,tol,tol1,pp3,ppt)
!
!     COPY DISPLACEMENTS
!
   id1 = 1
   IF ( Id==3 ) id1 = 3
   CALL modac2(id1,udv1t,udv3t)
   IF ( Id==2 .OR. Id==4 ) RETURN
!
!     COPY P LOAD S  (+ HEAD STUFF FOR NOW)
!
   CALL modac2(-1,ppt,pp3)
!
!     COPY D LOADS
!
   CALL modac2(1,pdt,pdt3)
!
!     COPY S LOADS
!
   CALL modac2(1,pst,pst3)
END SUBROUTINE modacc