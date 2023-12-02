!*==random.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE random
   USE c_blank
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: auto , casecc , dit , nfile , psdf , psdl , xycb
   INTEGER , DIMENSION(5) , SAVE :: ifile
   INTEGER :: ltab , nfreq , npsdl , ntau
   EXTERNAL rand5 , rand7 , rand8
!
! End of declarations rewritten by SPAG
!
!
!     RANDOM ANALYSIS MODULE
!
!     INPUTS   CASECC,XYCB,DIT,DISP,SPCF,LOAD,STRESS,FORCE,PSDL  (9)
!
!     OUTPUTS  PSDF,AUTO  (2)
!
!     SCRATCHES (0)
!
!     PARAMETERS 1 INTEGER
   DATA xycb , dit , psdl , ifile , casecc/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108 , 109/
   DATA psdf , auto/201 , 202/
   DATA nfile/5/
!
!     INITIALIZE + SET UP
!
   CALL rand7(ifile,nfile,psdl,dit,icoup,nfreq,npsdl,ntau,ltab,casecc,xycb)
   IF ( icoup<0 ) THEN
   ELSEIF ( icoup==0 ) THEN
!
!     UNCOUPLED
!
      CALL rand5(nfreq,npsdl,ntau,xycb,ltab,ifile,psdf,auto,nfile)
   ELSE
!
!     COUPLED
!
      CALL rand8(nfreq,npsdl,ntau,xycb,ltab,ifile,psdf,auto,nfile)
   ENDIF
END SUBROUTINE random
