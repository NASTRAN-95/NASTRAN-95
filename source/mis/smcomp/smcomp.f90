!*==smcomp.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE smcomp(Zi,Zr,Zd) !HIDESTARS (*,Zi,Zr,Zd)
   USE i_smcomx
   USE I_SMCOMX
   IMPLICIT NONE
   INCLUDE 'SMCOMX.COM'
   DOUBLE PRECISION Zd(4)
   INTEGER Zi(4)
   REAL Zr(4)
   INTEGER begn , end , jj , module(5)
   REAL rs
!
! DRIVER PROGRAM FOR SYMMETRIC DECOMPOSITION.  SUBROUTINE SMCPH1 READS
! THE INPUT MATRIX AND STORES THE DATA EITHER IN MEMORY OR ON THE
! SPILL FILE.  SUBROUTINE SMCPH2 IS THEN CALLED TO PERFORM THE
! MATRIX DECOMPOSITION.
!
!  mcb   - matrix control block for input matrix
!  lll   - matrix control block for lower triangular matrix
!  dbc   - dbc(1) = available scratch file, dbc(2-7) are not used
!  scr1, scr2, scr3 - three available scratch files
!  lcore - amount of open core available for use
!  ddr   - d.p. values of (real, imaginary) for scaled value of determinant
!  power - scale factor to apply to determinant, determinant=det * 10**power
!  mindd - d.p. value for minimum value of diagonal elements
!  chlsky - cholesky option when =1, i.e., form c matrix
!
   DATA module/4HSMCO , 4HMP   , 3*4H    /
   DATA begn/4HBEGN/
   DATA end/4HEND /
   ierror = 0
   ncol = mcb(2)
   module(3) = begn
   sturm = 0
   CALL conmsg(module,5,0)
   CALL smcph1(Zi,Zr,Zd)
   IF ( ierror/=1 ) THEN
      IF ( ierror==0 ) THEN
         CALL smcph2(Zi,Zr,Zd)
         IF ( ierror==1 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
!
! print roots information if this is an eigenvalue problem, and keep
! two largest shift point data if several shift point movings are involved.
!
         IF ( shftpt>0. ) WRITE (nout,99001) sturm , shftpt
99001    FORMAT (20X,I5,' ROOTS BELOW ',1P,E14.6)
         IF ( sturm/=0 ) THEN
            IF ( keep<=sturm ) THEN
               jj = keep
               rs = ptshft
               keep = sturm
               ptshft = jj
               shftpt = rs
            ENDIF
         ELSEIF ( keep>0 ) THEN
            sturm = keep
            shftpt = ptshft
         ENDIF
      ENDIF
      module(3) = end
      CALL conmsg(module,5,0)
      IF ( ierror/=0 ) RETURN 1
      RETURN
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      USE ISO_FORTRAN_ENV                 
      Module(3) = End
      CALL conmsg(Module,5,0)
   END SUBROUTINE spag_block_1
END SUBROUTINE smcomp
