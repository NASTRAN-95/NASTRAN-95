!*==alg1.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg1(Lnct)
   USE c_algino
   USE c_gas
   USE c_system
   USE c_ud3prt
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Lnct
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: log1 , log2
   REAL , DIMENSION(4) :: rdata
   EXTERNAL fread
!
! End of declarations rewritten by SPAG
!
!
!
   log1 = nalgdb
   log2 = nout
   CALL fread(log1,rdata,4,1)
   cp = rdata(1)
   r = rdata(2)
   g = rdata(3)
   ej = rdata(4)
   IF ( cp==0.0 ) cp = 0.24
   IF ( r==0.0 ) r = 53.32
   IF ( g==0.0 ) g = 32.174
   IF ( ej==0.0 ) ej = 778.16
   IF ( iprtc==1 ) WRITE (log2,99001) cp , r , g , ej
99001 FORMAT (/10X,'SPECIFIC HEAT AT CONSTANT PRESSURE',5X,1H=,F8.5,/10X,'GAS CONSTANT',27X,1H=,F8.4,/10X,'GRAVITATIONAL CONSTANT', &
            & 17X,1H=,F8.4,/10X,'JOULES EQUIVALENT',22X,1H=,F8.3)
   Lnct = Lnct + 5
   rojcp = r/(ej*cp)
   gamma = 1.0/(1.0-rojcp)
END SUBROUTINE alg1
