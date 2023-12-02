!*==alg1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg1(Lnct)
   IMPLICIT NONE
   USE C_ALGINO
   USE C_GAS
   USE C_SYSTEM
   USE C_UD3PRT
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
   log1 = Nalgdb
   log2 = Nout
   CALL fread(log1,rdata,4,1)
   Cp = rdata(1)
   R = rdata(2)
   G = rdata(3)
   Ej = rdata(4)
   IF ( Cp==0.0 ) Cp = 0.24
   IF ( R==0.0 ) R = 53.32
   IF ( G==0.0 ) G = 32.174
   IF ( Ej==0.0 ) Ej = 778.16
   IF ( Iprtc==1 ) WRITE (log2,99001) Cp , R , G , Ej
99001 FORMAT (/10X,'SPECIFIC HEAT AT CONSTANT PRESSURE',5X,1H=,F8.5,/10X,'GAS CONSTANT',27X,1H=,F8.4,/10X,'GRAVITATIONAL CONSTANT', &
            & 17X,1H=,F8.4,/10X,'JOULES EQUIVALENT',22X,1H=,F8.3)
   Lnct = Lnct + 5
   Rojcp = R/(Ej*Cp)
   Gamma = 1.0/(1.0-Rojcp)
END SUBROUTINE alg1
