!*==trd.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE trd
   IMPLICIT NONE
   USE c_blank
   USE c_system
   USE c_trdxx
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bdd , casexx , dit , kdd , mdd , moda , nlft , pd , pnld , scr1 , scr2 , scr3 , scr4 , scr5 , scr6 , scr7 ,    &
                   & scr8 , trl , udvt
   REAL :: delta
   INTEGER :: i , icrq , igroup , iskip , itime1 , itime2 , itime3 , itleft , jgroup , moda1 , ngroup , nlftp , nrow , nskip ,      &
            & nstep , nz
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     TRANSIENT RESPONSE MODULE DRIVER
!
!     INPUTS   CASEXX,     TRL,NLFT,DIT,KHH,BHH,MHH,PH
!              CASEXX,     TRL,NLFT,DIT,KDD,BDD,MDD,PD
!
!     OUTPUTS  UDVT,PNLD
!              UHVT,PNLH
!
!     PARAMETERS -- MODAL --BCD--INPUT--MODAL=MODAL IMPLIES MODAL
!                   NOUE  --INT--INPUT--NUMBER OF EXTRA POINTS
!                   NONCUP--INT--INPUT--NONCUP=-1 IMPLIES NONCOUPLED
!                   NCOL  --INT--IN/OUT--APPEND FLAG 0  NO APPEND
!                                                    +  COL NUMBER OF
!                                                        LAST TIME STEP
!
!     SCRATCHES   --
!
!
!
!
!
!
!
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
!
   DATA casexx , trl , nlft , dit/101 , 102 , 103 , 104/ , kdd , bdd , mdd , pd/105 , 106 , 107 , 108/ , udvt , pnld , scr1 ,       &
      & scr2/201 , 202 , 301 , 302/ , scr3 , scr4 , scr5 , scr6/303 , 304 , 305 , 306/ , scr7 , scr8 , moda/307 , 308 , 4HMODA/ ,   &
      & name/4HTRD  , 4H    /
!
! ----------------------------------------------------------------------
!
!     INITIALIZE
!
   moda1 = -1
   IF ( moda==modal(1) ) moda1 = 1
!
!     BUILD INITIAL CONDITIONS
!
   IF ( iprec==1 ) CALL trd1a(casexx,trl,scr1,nlftp,ngroup,moda1)
   IF ( iprec==2 ) CALL trd1a2(casexx,trl,scr1,nlftp,ngroup,moda1)
!
!     TEST FOR ZERO APPLIED LOAD
!
   ik(1) = scr1
   CALL rdtrl(ik(1))
   IF ( ik(6)==0 ) THEN
      IF ( nlftp==0 ) THEN
         ik(1) = pd
         ik(6) = 0
         CALL rdtrl(ik)
         IF ( ik(6)==0 ) THEN
            IF ( ncol<=0 ) CALL mesage(-46,0,0)
         ENDIF
      ENDIF
   ENDIF
!
!     ESTIMATE CORE
!
   IF ( noncup<0 .AND. modal(1)==moda .AND. nlftp==0 ) THEN
!
!     UNCOUPLED MODAL
!
      CALL trd1e(mdd,bdd,kdd,pd,udvt,ngroup)
   ELSE
      nz = korsz(z)
      igroup = nz - 3*ngroup + 1
      ik(1) = kdd
      CALL rdtrl(ik)
      IF ( ik(1)<0 ) THEN
         ik(1) = 0
      ELSE
         nrow = ik(3)
      ENDIF
      ib(1) = bdd
      CALL rdtrl(ib)
      IF ( ib(1)<0 ) THEN
         ib(1) = 0
      ELSE
         nrow = ib(3)
      ENDIF
      im(1) = mdd
      CALL rdtrl(im)
      IF ( im(1)<0 ) THEN
         im(1) = 0
      ELSE
         nrow = im(3)
      ENDIF
      icrq = 8*ibuf + 7*iprec*nrow - igroup
      IF ( icrq>0 ) CALL mesage(-8,icrq,name)
!
!     SET UP COMMON
!
      sr1 = scr2
      sr2 = scr3
      sr3 = scr4
      sr4 = scr5
      sr5 = scr6
      sr6 = scr7
      iskip = 1
      jgroup = igroup
      DO i = 1 , ngroup
         nskip = iz(jgroup+2)
         IF ( nskip==1 ) THEN
            jgroup = jgroup + 3
         ELSE
            iskip = 0
            EXIT
         ENDIF
      ENDDO
      DO i = 1 , ngroup
         CALL klock(itime1)
         nstep = iz(igroup)
         delta = z(igroup+1)
         igroup = igroup + 3
         IF ( iprec==1 ) CALL initl(3*ngroup,delta)
         IF ( iprec==2 ) CALL initl2(3*ngroup,delta)
         CALL klock(itime3)
         IF ( iprec==1 ) CALL trd1c(scr1,pd,ngroup,nlftp,udvt,i,scr8,dit,nlft,noue,moda1,pnld,iskip)
         IF ( iprec==2 ) CALL trd1c2(scr1,pd,ngroup,nlftp,udvt,i,scr8,dit,nlft,noue,moda1,pnld,iskip)
         CALL klock(itime2)
         CALL tmtogo(itleft)
         IF ( itleft<=0 ) GOTO 100
         IF ( i/=ngroup ) THEN
!
!     COMPUTE TIME TO DO NEXT ITERATION
!
            IF ( 2*(itime3-itime1+((itime2-itime3)/nstep)*iz(igroup))>=itleft ) GOTO 100
         ENDIF
      ENDDO
   ENDIF
   ik(1) = udvt
   CALL rdtrl(ik(1))
   ncol = ik(2)/3
   RETURN
!
!     INSUFFICIENT TIME LEFT TO FINISH
!
 100  ik(1) = udvt
   CALL rdtrl(ik(1))
   ncol = ik(2)/3
   ik(1) = pd
   CALL rdtrl(ik)
   CALL mesage(45,ik(2)-ncol,name)
   IF ( ncol==0 ) CALL mesage(-37,0,name)
END SUBROUTINE trd
