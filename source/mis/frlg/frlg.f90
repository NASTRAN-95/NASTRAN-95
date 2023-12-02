!*==frlg.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frlg
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_TWO
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: casexx , dit , dlt , fol , frl , gmd , god , moda , pdf , phf , phidh , ppf , psf , scr1 , scr2 , scr3 , scr4 ,&
                   & usetd
   REAL :: frqset
   INTEGER , DIMENSION(2) , SAVE :: ifreq , itran
   INTEGER :: lusetd , multi , nfreq , nload , omit , single
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL andf , frlga , frlgb , rdtrl
!
! End of declarations rewritten by SPAG
!
!
!     FREQUENCE RESPONSE LOAD GENERATOR
!
!     INPUTS  - CASEXX,USETD,DLT,FRL,GMD,GOD,DIT,PHIDH
!
!     OUTPUTS - PPF,PSF,PDF,FOL,PHF
!
!     4 SCRATHCES
!
!
   DATA casexx , usetd , dlt , frl , gmd , god , dit , phidh/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108/
   DATA ppf , psf , pdf , fol , phf , scr1 , scr2 , scr3 , scr4/201 , 202 , 203 , 204 , 205 , 301 , 302 , 303 , 304/
   DATA moda/4HMODA/
   DATA ifreq , itran/4HFREQ , 1H  , 4HTRAN , 1H /
!
!     DETERMINE USET DATA
!
   mcb(1) = usetd
   CALL rdtrl(mcb)
   lusetd = mcb(2)
   multi = -1
   IF ( andf(mcb(5),Itwo(Ium))/=0 ) multi = 1
   single = -1
   IF ( andf(mcb(5),Itwo(Ius))/=0 ) single = 1
   omit = -1
   IF ( andf(mcb(5),Itwo(Iuo))/=0 ) omit = 1
!
   Iapp(1) = ifreq(1)
   Iapp(2) = ifreq(2)
!
!     BUILD LOADS ON P SET
!
!     ORDER IS ALL FREQUENCIES FOR GIVEN LOAD TOGETHER
!
   CALL frlga(dlt,frl,casexx,dit,ppf,lusetd,nfreq,nload,frqset,fol,Notrd)
   IF ( Notrd/=-1 ) THEN
      Iapp(1) = itran(1)
      Iapp(2) = itran(2)
   ENDIF
!
!     REDUCE LOADS TO D OR H SET
!
   IF ( multi<0 .AND. single<0 .AND. omit<0 .AND. Modal(1)/=moda ) RETURN
   CALL frlgb(ppf,usetd,gmd,god,multi,single,omit,Modal,phidh,pdf,psf,phf,scr1,scr2,scr3,scr4)
END SUBROUTINE frlg
