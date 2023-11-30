
SUBROUTINE trd
   IMPLICIT NONE
   INTEGER Ib(7) , Ibuf , Idummy(53) , Ik(7) , Im(7) , Iopen , Iprec , Ispnl , Isym , Iz(1) , Modal(2) , Ncol , Noncup , Nopd ,     &
         & Noue , Sr1 , Sr2 , Sr3 , Sr4 , Sr5 , Sr6
   REAL To , Z(1)
   COMMON /blank / Modal , Noue , Noncup , Ncol
   COMMON /system/ Ibuf , Idummy , Iprec
   COMMON /trdxx / Ik , Im , Ib , Sr1 , Sr2 , Sr3 , Sr4 , Sr5 , Sr6 , Iopen , Isym , To , Nopd , Ispnl
   COMMON /zzzzzz/ Z
   INTEGER bdd , casexx , dit , i , icrq , igroup , iskip , itime1 , itime2 , itime3 , itleft , jgroup , kdd , mdd , moda , moda1 , &
         & name(2) , ngroup , nlft , nlftp , nrow , nskip , nstep , nz , pd , pnld , scr1 , scr2 , scr3 , scr4 , scr5 , scr6 ,      &
         & scr7 , scr8 , trl , udvt
   REAL delta
   INTEGER korsz
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
   EQUIVALENCE (Z(1),Iz(1))
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
   IF ( moda==Modal(1) ) moda1 = 1
!
!     BUILD INITIAL CONDITIONS
!
   IF ( Iprec==1 ) CALL trd1a(casexx,trl,scr1,nlftp,ngroup,moda1)
   IF ( Iprec==2 ) CALL trd1a2(casexx,trl,scr1,nlftp,ngroup,moda1)
!
!     TEST FOR ZERO APPLIED LOAD
!
   Ik(1) = scr1
   CALL rdtrl(Ik(1))
   IF ( Ik(6)==0 ) THEN
      IF ( nlftp==0 ) THEN
         Ik(1) = pd
         Ik(6) = 0
         CALL rdtrl(Ik)
         IF ( Ik(6)==0 ) THEN
            IF ( Ncol<=0 ) CALL mesage(-46,0,0)
         ENDIF
      ENDIF
   ENDIF
!
!     ESTIMATE CORE
!
   IF ( Noncup<0 .AND. Modal(1)==moda .AND. nlftp==0 ) THEN
!
!     UNCOUPLED MODAL
!
      CALL trd1e(mdd,bdd,kdd,pd,udvt,ngroup)
   ELSE
      nz = korsz(Z)
      igroup = nz - 3*ngroup + 1
      Ik(1) = kdd
      CALL rdtrl(Ik)
      IF ( Ik(1)<0 ) THEN
         Ik(1) = 0
      ELSE
         nrow = Ik(3)
      ENDIF
      Ib(1) = bdd
      CALL rdtrl(Ib)
      IF ( Ib(1)<0 ) THEN
         Ib(1) = 0
      ELSE
         nrow = Ib(3)
      ENDIF
      Im(1) = mdd
      CALL rdtrl(Im)
      IF ( Im(1)<0 ) THEN
         Im(1) = 0
      ELSE
         nrow = Im(3)
      ENDIF
      icrq = 8*Ibuf + 7*Iprec*nrow - igroup
      IF ( icrq>0 ) CALL mesage(-8,icrq,name)
!
!     SET UP COMMON
!
      Sr1 = scr2
      Sr2 = scr3
      Sr3 = scr4
      Sr4 = scr5
      Sr5 = scr6
      Sr6 = scr7
      iskip = 1
      jgroup = igroup
      DO i = 1 , ngroup
         nskip = Iz(jgroup+2)
         IF ( nskip==1 ) THEN
            jgroup = jgroup + 3
         ELSE
            iskip = 0
            EXIT
         ENDIF
      ENDDO
      DO i = 1 , ngroup
         CALL klock(itime1)
         nstep = Iz(igroup)
         delta = Z(igroup+1)
         igroup = igroup + 3
         IF ( Iprec==1 ) CALL initl(3*ngroup,delta)
         IF ( Iprec==2 ) CALL initl2(3*ngroup,delta)
         CALL klock(itime3)
         IF ( Iprec==1 ) CALL trd1c(scr1,pd,ngroup,nlftp,udvt,i,scr8,dit,nlft,Noue,moda1,pnld,iskip)
         IF ( Iprec==2 ) CALL trd1c2(scr1,pd,ngroup,nlftp,udvt,i,scr8,dit,nlft,Noue,moda1,pnld,iskip)
         CALL klock(itime2)
         CALL tmtogo(itleft)
         IF ( itleft<=0 ) GOTO 100
         IF ( i/=ngroup ) THEN
!
!     COMPUTE TIME TO DO NEXT ITERATION
!
            IF ( 2*(itime3-itime1+((itime2-itime3)/nstep)*Iz(igroup))>=itleft ) GOTO 100
         ENDIF
      ENDDO
   ENDIF
   Ik(1) = udvt
   CALL rdtrl(Ik(1))
   Ncol = Ik(2)/3
   RETURN
!
!     INSUFFICIENT TIME LEFT TO FINISH
!
 100  Ik(1) = udvt
   CALL rdtrl(Ik(1))
   Ncol = Ik(2)/3
   Ik(1) = pd
   CALL rdtrl(Ik)
   CALL mesage(45,Ik(2)-Ncol,name)
   IF ( Ncol==0 ) CALL mesage(-37,0,name)
END SUBROUTINE trd
