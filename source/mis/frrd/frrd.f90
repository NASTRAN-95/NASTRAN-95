!*==frrd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frrd
   IMPLICIT NONE
   USE C_BLANK
   USE C_CDCMPX
   USE C_FRRDST
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
   Ib = 0
!
!     BUILD LOADS ON P SET ORDER IS ALL FREQ. FOR LOAD TOGETHER
!     FRRD1A IS AN ENTRY POINT IN FRLGA
!
   CALL frrd1a(dlt,frl,casecc,dit,pp,Lusetd,nfreq,nload,Frqset,fol,notrd)
   IF ( Multi<0 .AND. Single<0 .AND. Omit<0 .AND. Modal(1)/=moda ) THEN
      pdd = pp
   ELSE
!
!     REDUCE LOADS TO D OR H SET
!     FRRD1B IS AN ENTRY POINT IN FRLGB
!
      CALL frrd1b(pp,usetd,gmd,god,Multi,Single,Omit,Modal(1),phidh,pd,ps,scr5,scr1,scr2,scr3,scr4)
!
!     SCR5 HAS PH IF MODAL FORMULATION
!
      IF ( Modal(1)==moda ) pdd = scr5
!
!     SOLVE PROBLEM FOR EACH FREQUENCY
!
      IF ( Noncup<0 .AND. Modal(1)==moda ) THEN
!
!     UNCOUPLED MODAL
!
         CALL frrd1f(mdd,bdd,kdd,frl,Frqset,nload,nfreq,pdd,udv)
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
      CALL frrd1c(frl,Frqset,mdd,bdd,kdd,i,scr1,scr2,scr3,scr4,scr8,scr7,igood)
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
      IF ( nfreq/=1 .AND. nload/=1 ) CALL frrd1e(scr6,udv,nload,i)
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      RETURN
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
!
!     INSUFFICIENT TIME TO COMPLETE ANOTHER LOOP
!
      CALL mesage(45,nfreq-i,name)
      mcb(1) = scr6
      CALL rdtrl(mcb(1))
      ndone = mcb(2)
      mcb(1) = pp
      CALL rdtrl(mcb(1))
      mcb(2) = ndone
      CALL wrttrl(mcb(1))
      IF ( Single>=0 ) THEN
         mcb(1) = ps
         CALL rdtrl(mcb(1))
         mcb(2) = ndone
         CALL wrttrl(mcb(1))
      ENDIF
      mcb(1) = pd
      CALL rdtrl(mcb(1))
      mcb(2) = ndone
      CALL wrttrl(mcb(1))
      CALL spag_block_1
      RETURN
   END SUBROUTINE spag_block_3
END SUBROUTINE frrd
