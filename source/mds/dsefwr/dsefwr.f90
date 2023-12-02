!*==dsefwr.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsefwr
   USE i_dsiof
   USE i_xnstrn
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   IF ( iprvop==0 ) CALL dsmsg(7)
   IF ( indclr/=indcbp ) THEN
      ibase(indcbp+1) = idsrt + idsc + (indclr-indbas+1)
      indclr = indcbp + 2
      indcbp = indclr
   ENDIF
   IF ( (indclr-indbas-2)>=nbuff ) THEN
      ibase(indclr) = idseb
      CALL dswrnb
   ENDIF
   ibase(indclr) = idsef
   ibase(indclr+1) = idseb
   indclr = indclr + 1
   indcbp = indclr
   IF ( (indclr-indbas)>nbuff ) CALL dswrnb
END SUBROUTINE dsefwr
