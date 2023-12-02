!*==trht.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trht
   USE c_blank
   USE c_system
   USE c_trdd1
   USE c_trhtx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bdd , casexx , dit , gptt , iscr1 , iscr2 , iscr3 , iscr4 , iscr5 , iscr6 , iscr7 , kdd , nb , nlft , pd ,     &
                   & pnld , rdd , trl , udvt , usetd
   REAL :: delta
   INTEGER :: i , igroup , iprec , itim1 , itim2 , itim3 , itleft , ngroup , nv , sysbuf
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL klock , korsz , mesage , rdtrl , tmtogo , trht1a , trht1b , trht1c
!
! End of declarations rewritten by SPAG
!
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
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(55),Iprec) , (Z(1),Iz(1))
   DATA casexx , usetd , nlft , dit , gptt , kdd , bdd , rdd , pd , trl/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108 , 109 , 110/ ,&
      & udvt , pnld , iscr1 , iscr2 , iscr3 , iscr4 , iscr5 , iscr6 , iscr7/201 , 202 , 301 , 302 , 303 , 304 , 305 , 306 , 307/
   DATA name/4HTRD  , 4H    / , nb/8/
!
!     SET UP FILES
!
   ik(1) = kdd
   CALL rdtrl(ik)
   ib(1) = bdd
   CALL rdtrl(ib)
   icr1 = iscr1
   icr2 = iscr2
   icr3 = iscr3
   icr4 = iscr4
   icr5 = iscr5
   icr6 = iscr6
   icr7 = iscr7
!
!     SET UP NONLINEAR FILES
!
   nlft1 = nlft
   dit1 = dit
   pnl1 = pnld
   IF ( ik(1)<=0 ) ik(1) = 0
   IF ( ib(1)<=0 ) ib(1) = 0
   moda1 = -1
   IF ( ib(1)/=0 ) ik(2) = ib(2)
!
!     OBTAIN PARAMETERS, INITIAL CONDITIONS
!
   CALL trht1a(casexx,usetd,gptt,trl,ngroup)
!
!     ALLOCATE CORE
!
   nz = korsz(z)
   igroup = nz - 3*ngroup + 1
   nv = 4
   IF ( nlftp1/=0 .OR. norad/=-1 ) nv = nv + 3
   IF ( nz<nv*ik(2)*iprec-nb*sysbuf-3*ngroup ) CALL mesage(-8,0,name)
   tim = 0.
   DO i = 1 , ngroup
      CALL klock(itim1)
      nstep = iz(igroup)
      delta = z(igroup+1)
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
         IF ( (itim3-itim1+((itim2-itim3)/nstep)*iz(igroup))>=itleft ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
      ENDIF
   ENDDO
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
      CALL mesage(45,Ngroup-I,Name)
      CALL spag_block_1
   END SUBROUTINE spag_block_2
END SUBROUTINE trht
