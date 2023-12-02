!*==ddr2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddr2
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_PATX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: b2dd , dm , frl , k2dd , lll , mdd , pad , paf , pd , scr2 , scr3 , scr4 , scr5 , scr6 , scr7 , tran , uav ,   &
                   & uev , usetd , vud
   INTEGER :: isol , ivec , pl
   EXTERNAL calcv , ddr1a , ddr1b , korsz , sdr1b , ssg2a , ssg2b , ssg3a
!
! End of declarations rewritten by SPAG
!
!
!     DYNAMIC DATA RECOVERY--PART 2 --MODE ACCELERATION
!
!     DMAP SEQUENCE
!
!     INPUTS = 9
!
!     USETD,VUD,PD,K2DD,B2DD,MDD,FRL,LLL,DM
!
!     OUTPUTS = 3
!
!     UAV,UEV,PAF
!
!     SCRATCHES = 6
!
!     PARAMETERS 1 BCD, 3INTEGERS
!
   DATA usetd , vud , pd , k2dd , b2dd , mdd , frl , lll , dm/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108 , 109/
   DATA uav , uev , paf , tran/201 , 202 , 203 , 4HTRAN/
   DATA scr2 , scr3 , scr4 , scr5 , scr6 , scr7 , pad/302 , 303 , 304 , 305 , 306 , 301 , 302/
!
!
   Lc = korsz(Core)
   vud = 102
   scr7 = 301
   Uset = usetd
   pl = scr6
   isol = scr7
   IF ( Noue<0 ) pad = paf
   IF ( Type(1)/=tran ) scr7 = uav
   IF ( Type(1)/=tran .AND. React<0 .AND. Noue>=0 ) scr7 = vud
!
!     MODE ACCELERATION
!
!     FORM PAD
!
!
   CALL ddr1a(pd,k2dd,b2dd,mdd,vud,pad,frl,Frqset,scr3,scr4,scr5,scr6,Type(1),scr7)
!
!     DISP ON SCR7 IN TRANSIENT
!
   IF ( Noue>=0 ) THEN
      CALL calcv(scr3,Ud,Ua,Ue,Core(1))
      CALL ssg2a(vud,scr4,uev,scr3)
!
!     UA IS ON SCR4
!
      vud = scr4
!
!     BREAK UP PAD
!
      CALL ssg2a(pad,paf,scr5,scr3)
   ENDIF
   IF ( React>=0 ) THEN
!
!     FREE BODY PROBLEM
!
      CALL calcv(scr3,Ua,Ul,Ur,Core(1))
!
!     PARTITION PAF AND UA
!
      CALL ssg2a(paf,pl,scr5,scr3)
      ivec = vud
      IF ( Type(1)==tran ) ivec = scr7
      CALL ssg2a(ivec,scr2,scr5,scr3)
!
!     UR IS  ON SCR5
!
      CALL ssg3a(0,lll,pl,scr3,scr2,scr6,-1,0)
      CALL ssg2b(dm,scr5,scr3,scr4,0,2,1,scr6)
      CALL sdr1b(scr3,scr4,scr5,scr7,Ua,Ul,Ur,usetd,0,0)
   ELSE
!
!     UR NULL
!
      IF ( Type(1)/=tran ) scr7 = isol
      IF ( Type(1)/=tran .AND. Noue<0 ) scr7 = uav
      CALL ssg3a(0,lll,paf,scr7,scr3,scr6,-1,0)
   ENDIF
   IF ( Type(1)==tran ) THEN
!
!     MERGE RECALCULATED SOLUTIONS AND ACCEL AND VELOCITY
!
      isol = uav
      IF ( Noue>=0 ) isol = scr5
      CALL ddr1b(vud,scr7,isol)
   ENDIF
!
!     BUILD UP TO DSIZE  ADDING IN UEV
!
   IF ( Noue>=0 ) CALL sdr1b(scr4,isol,uev,uav,Ud,Ua,Ue,usetd,0,0)
END SUBROUTINE ddr2
