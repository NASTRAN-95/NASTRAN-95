!*==frrd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frrd
   USE c_blank
   USE c_cdcmpx
   USE c_frrdst
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bdd , casecc , dit , dlt , fol , frl , gmd , god , kdd , mdd , moda , pd , pdd , phidh , pp , ps , scr1 ,      &
                   & scr2 , scr3 , scr4 , scr5 , scr6 , scr7 , scr8 , udv , usetd
   INTEGER :: i , igood , itime1 , itime2 , itleft , ndone , nfreq , nload , notrd
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL frrd1a , frrd1b , frrd1c , frrd1d , frrd1e , frrd1f , klock , mesage , rdtrl , tmtogo , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     FREQUENCY AND RANDOM RESPONSE MODULE
!
!     INPUTS CASECC,USETD,DLT,FRL,GMD,GOD,KDD,BDD,MDD,PHIDH,DIT
!
!     OUTPUTS UDV,PS,PD,PP
!
!     8 SCRATCHES
!
   DATA casecc , usetd , dlt , frl , gmd , god , kdd , bdd , mdd , phidh , dit/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108 , 109 ,&
      & 110 , 111/
   DATA udv , ps , pd , pp , pdd , fol/201 , 202 , 203 , 204 , 203 , 205/
   DATA scr1 , scr2 , scr3 , scr4 , scr5 , scr6 , scr7 , scr8/301 , 302 , 303 , 304 , 305 , 306 , 307 , 308/
   DATA moda/4HMODA/ , name/4HFRRD , 4H    /
!
   pdd = 203
   scr6 = 306
   ib = 0
!
!     BUILD LOADS ON P SET ORDER IS ALL FREQ. FOR LOAD TOGETHER
!     FRRD1A IS AN ENTRY POINT IN FRLGA
!
   CALL frrd1a(dlt,frl,casecc,dit,pp,lusetd,nfreq,nload,frqset,fol,notrd)
   IF ( multi<0 .AND. single<0 .AND. omit<0 .AND. modal(1)/=moda ) THEN
      pdd = pp
   ELSE
!
!     REDUCE LOADS TO D OR H SET
!     FRRD1B IS AN ENTRY POINT IN FRLGB
!
      CALL frrd1b(pp,usetd,gmd,god,multi,single,omit,modal(1),phidh,pd,ps,scr5,scr1,scr2,scr3,scr4)
!
!     SCR5 HAS PH IF MODAL FORMULATION
!
      IF ( modal(1)==moda ) pdd = scr5
!
!     SOLVE PROBLEM FOR EACH FREQUENCY
!
      IF ( noncup<0 .AND. modal(1)==moda ) THEN
!
!     UNCOUPLED MODAL
!
         CALL frrd1f(mdd,bdd,kdd,frl,frqset,nload,nfreq,pdd,udv)
         CALL spag_block_2
         RETURN
      ENDIF
   ENDIF
   IF ( nfreq==1 .OR. nload==1 ) scr6 = udv
   DO i = 1 , nfreq
      CALL klock(itime1)
!
!     FORM AND DECOMPOSE MATRICES
!     IF MATRIX IS SINGULAR, IGOOD IS SET TO 1 IN FRRD1C. ZERO OTHERWISE
!
      CALL frrd1c(frl,frqset,mdd,bdd,kdd,i,scr1,scr2,scr3,scr4,scr8,scr7,igood)
!
!     ULL IS ON SCR1 -- LLL IS IN SCR2
!
!     SOLVE FOR PD LOADS STACK ON SCR6
!
      CALL frrd1d(pdd,scr1,scr2,scr3,scr4,scr6,i,nload,igood,nfreq)
      CALL klock(itime2)
      CALL tmtogo(itleft)
      IF ( 2*(itime2-itime1)>itleft .AND. i/=nfreq ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
   ENDDO
!
   i = nfreq
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     RESORT SOLUTION VECTORS INTO SAME ORDER AS LOADS
!     FRRD1E IS AN ENTRY POINT IN FRRD1D
!
      IF ( Nfreq/=1 .AND. Nload/=1 ) CALL frrd1e(Scr6,Udv,Nload,I)
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
!
!     INSUFFICIENT TIME TO COMPLETE ANOTHER LOOP
!
      CALL mesage(45,Nfreq-I,Name)
      Mcb(1) = Scr6
      CALL rdtrl(Mcb(1))
      Ndone = Mcb(2)
      Mcb(1) = Pp
      CALL rdtrl(Mcb(1))
      Mcb(2) = Ndone
      CALL wrttrl(Mcb(1))
      IF ( Single>=0 ) THEN
         Mcb(1) = Ps
         CALL rdtrl(Mcb(1))
         Mcb(2) = Ndone
         CALL wrttrl(Mcb(1))
      ENDIF
      Mcb(1) = Pd
      CALL rdtrl(Mcb(1))
      Mcb(2) = Ndone
      CALL wrttrl(Mcb(1))
      CALL spag_block_1
   END SUBROUTINE spag_block_3
END SUBROUTINE frrd
