
SUBROUTINE smcomp(*,Zi,Zr,Zd)
   IMPLICIT NONE
   INCLUDE 'SMCOMX.COM'
!
! Dummy argument declarations
!
   DOUBLE PRECISION Zd(4)
   INTEGER Zi(4)
   REAL Zr(4)
!
! Local variable declarations
!
   INTEGER begn , end , jj , module(5)
   REAL rs
!
! End of declarations
!
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
   Ierror = 0
   Ncol = Mcb(2)
   module(3) = begn
   Sturm = 0
   CALL conmsg(module,5,0)
   CALL smcph1(Zi,Zr,Zd)
   IF ( Ierror/=1 ) THEN
      IF ( Ierror==0 ) THEN
         CALL smcph2(Zi,Zr,Zd)
         IF ( Ierror==1 ) GOTO 100
!
! print roots information if this is an eigenvalue problem, and keep
! two largest shift point data if several shift point movings are involved.
!
         IF ( Shftpt>0. ) WRITE (Nout,99001) Sturm , Shftpt
99001    FORMAT (20X,I5,' ROOTS BELOW ',1P,E14.6)
         IF ( Sturm/=0 ) THEN
            IF ( Keep<=Sturm ) THEN
               jj = Keep
               rs = Ptshft
               Keep = Sturm
               Ptshft = jj
               Shftpt = rs
            ENDIF
         ELSEIF ( Keep>0 ) THEN
            Sturm = Keep
            Shftpt = Ptshft
         ENDIF
      ENDIF
      module(3) = end
      CALL conmsg(module,5,0)
      IF ( Ierror/=0 ) RETURN 1
      GOTO 99999
   ENDIF
 100  module(3) = end
   CALL conmsg(module,5,0)
99999 RETURN
END SUBROUTINE smcomp
