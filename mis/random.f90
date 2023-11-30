
SUBROUTINE random
   IMPLICIT NONE
   INTEGER Icoup
   COMMON /blank / Icoup
   INTEGER auto , casecc , dit , ifile(5) , ltab , nfile , nfreq , npsdl , ntau , psdf , psdl , xycb
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
   CALL rand7(ifile,nfile,psdl,dit,Icoup,nfreq,npsdl,ntau,ltab,casecc,xycb)
   IF ( Icoup<0 ) THEN
   ELSEIF ( Icoup==0 ) THEN
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
   RETURN
END SUBROUTINE random
