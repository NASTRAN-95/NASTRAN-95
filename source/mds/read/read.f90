!*==read.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE read(*,*,File,Idata,N,Ieorfl,M)
   USE i_dsiof
   USE i_ginox
   USE i_xnstrn
   USE I_DSIOF
   USE I_GINOX
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'GINOX.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File , Ieorfl , M , N
   INTEGER Idata(2)
   INTEGER id , idiff , ilim , ireq , iwords , k , l , lasblk
   name = File
   nwords = N
   ieor = Ieorfl
   iretrn = 0
   CALL dsgefl
   IF ( iprvop/=0 ) CALL dsmsg(4)
   id = iand(ibase(indclr),maskq1)
   IF ( id==idseb ) THEN
      CALL dbmlbk(lasblk)
      IF ( lasblk>nblock ) THEN
         CALL dsrdnb
         id = iand(ibase(indclr),maskq1)
      ELSE
         iretrn = 1
         CALL spag_block_1
         RETURN
      ENDIF
   ENDIF
   IF ( id==idsrh ) THEN
      iwords = iand(ibase(indclr),maskh2)
      idiff = indcbp - indclr
      iwords = iwords - idiff
      ireq = iabs(nwords)
      IF ( ireq>iwords ) THEN
         CALL dsrdmb(Idata,M)
      ELSE
         IF ( nwords>0 ) THEN
            l = 1
            ilim = indcbp + nwords - 1
            DO k = indcbp , ilim
               Idata(l) = ibase(k+1)
               l = l + 1
            ENDDO
         ENDIF
         indcbp = indcbp + ireq
      ENDIF
      IF ( ieor/=0 ) CALL dsskrc
   ELSE
      IF ( id/=idsef ) CALL dsmsg(105)
      indclr = indclr + 1
      indcbp = indclr
      iretrn = 1
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      EXTERNAL dssdcb
!
! End of declarations rewritten by SPAG
!
      CALL dssdcb
      IF ( Iretrn==2 ) RETURN 2
      IF ( Iretrn==1 ) RETURN 1
   END SUBROUTINE spag_block_1
END SUBROUTINE read
