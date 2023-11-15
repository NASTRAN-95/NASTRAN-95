
SUBROUTINE dsefwr
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! End of declarations
!
   IF ( Iprvop==0 ) CALL dsmsg(7)
   IF ( Indclr/=Indcbp ) THEN
      Ibase(Indcbp+1) = Idsrt + Idsc + (Indclr-Indbas+1)
      Indclr = Indcbp + 2
      Indcbp = Indclr
   ENDIF
   IF ( (Indclr-Indbas-2)>=Nbuff ) THEN
      Ibase(Indclr) = Idseb
      CALL dswrnb
   ENDIF
   Ibase(Indclr) = Idsef
   Ibase(Indclr+1) = Idseb
   Indclr = Indclr + 1
   Indcbp = Indclr
   IF ( (Indclr-Indbas)>Nbuff ) CALL dswrnb
END SUBROUTINE dsefwr
