
SUBROUTINE trht
   IMPLICIT NONE
   REAL Beta , Sigma , Tabs , Tim , Z(1)
   INTEGER Dit1 , Ib(7) , Icore , Icount , Icr1 , Icr2 , Icr3 , Icr4 , Icr5 , Icr6 , Icr7 , Ik(7) , Iloop1 , Ip4 , Ipnl(7) , Iprec ,&
         & Ist , Isym , Iu2 , Iz(1) , Ksystm(65) , Moda1 , More(6) , Nlft1 , Nlftp1 , Nmodes , Norad , Nout , Nstep , Nz , Pnl1 ,   &
         & Radlin , Sysbuf
   COMMON /blank / Beta , Tabs , Norad , Radlin , Sigma
   COMMON /system/ Ksystm
   COMMON /trdd1 / Nlft1 , Dit1 , Nlftp1 , Nout , Icount , Iloop1 , Moda1 , Nz , Icore , Iu2 , Ip4 , Ipnl , Nmodes , Nstep , Pnl1 , &
                 & Ist , More
   COMMON /trhtx / Ik , Ib , Icr1 , Icr2 , Icr3 , Icr4 , Icr5 , Isym , Icr6 , Icr7 , Tim
   COMMON /zzzzzz/ Z
   INTEGER bdd , casexx , dit , gptt , i , igroup , iscr1 , iscr2 , iscr3 , iscr4 , iscr5 , iscr6 , iscr7 , itim1 , itim2 , itim3 , &
         & itleft , kdd , name(2) , nb , ngroup , nlft , nv , pd , pnld , rdd , trl , udvt , usetd
   REAL delta
   INTEGER korsz
!
!     TRANSIENT INTEGRATION HEAT TRANSFER MODULE
!
!     INPUTS  CASEXX,USETD,NLFT,DIT,GPTT,KDD,BDD,RDD,PD,TRL (10)
!
!     OUTPUTS  UDVT,PNLD (2)
!
!     SCRATCHES (7)
!     PARAMETERS BETA(R),TABS(R),NORAD(L),RADLIN(L)
!
!     ICR1 IS LLL
!     ICR2 IS ULL
!     ICR5 IS INITIAL CONDITIONS
!     ICR6 IS A MATRIX
!     ICR3,ICR4,ICR7 ARE DECOMP SCRATCH FILES
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(55),Iprec) , (Z(1),Iz(1))
   DATA casexx , usetd , nlft , dit , gptt , kdd , bdd , rdd , pd , trl/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108 , 109 , 110/ ,&
      & udvt , pnld , iscr1 , iscr2 , iscr3 , iscr4 , iscr5 , iscr6 , iscr7/201 , 202 , 301 , 302 , 303 , 304 , 305 , 306 , 307/
   DATA name/4HTRD  , 4H    / , nb/8/
!
!     SET UP FILES
!
   Ik(1) = kdd
   CALL rdtrl(Ik)
   Ib(1) = bdd
   CALL rdtrl(Ib)
   Icr1 = iscr1
   Icr2 = iscr2
   Icr3 = iscr3
   Icr4 = iscr4
   Icr5 = iscr5
   Icr6 = iscr6
   Icr7 = iscr7
!
!     SET UP NONLINEAR FILES
!
   Nlft1 = nlft
   Dit1 = dit
   Pnl1 = pnld
   IF ( Ik(1)<=0 ) Ik(1) = 0
   IF ( Ib(1)<=0 ) Ib(1) = 0
   Moda1 = -1
   IF ( Ib(1)/=0 ) Ik(2) = Ib(2)
!
!     OBTAIN PARAMETERS, INITIAL CONDITIONS
!
   CALL trht1a(casexx,usetd,gptt,trl,ngroup)
!
!     ALLOCATE CORE
!
   Nz = korsz(Z)
   igroup = Nz - 3*ngroup + 1
   nv = 4
   IF ( Nlftp1/=0 .OR. Norad/=-1 ) nv = nv + 3
   IF ( Nz<nv*Ik(2)*Iprec-nb*Sysbuf-3*ngroup ) CALL mesage(-8,0,name)
   Tim = 0.
   DO i = 1 , ngroup
      CALL klock(itim1)
      Nstep = Iz(igroup)
      delta = Z(igroup+1)
      igroup = igroup + 3
!
!     FORM  A  MATRIX AND DECOMPOSE
!
      CALL trht1b(3*ngroup,delta)
      CALL klock(itim3)
      CALL trht1c(ngroup,udvt,pd,rdd,i)
      CALL klock(itim2)
      CALL tmtogo(itleft)
      IF ( i/=ngroup ) THEN
         IF ( (itim3-itim1+((itim2-itim3)/Nstep)*Iz(igroup))>=itleft ) GOTO 200
      ENDIF
   ENDDO
 100  RETURN
!
 200  CALL mesage(45,ngroup-i,name)
   GOTO 100
END SUBROUTINE trht
