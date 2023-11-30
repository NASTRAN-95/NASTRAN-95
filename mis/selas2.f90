
SUBROUTINE selas2
   IMPLICIT NONE
   REAL Dummy(33) , Eldefm , Force , Scoeff , Stiff , Stress , Templd , Xxxxxx(95) , Yyyyyy(98) , Zz(1) , Zzzzzz(23)
   INTEGER Icoeff , Icstm , Isilno(2) , Ivec , Ivecn , Jelid , Jfelid , Jselid , Ncstm
   COMMON /sdr2x4/ Dummy , Icstm , Ncstm , Ivec , Ivecn , Templd , Eldefm
   COMMON /sdr2x7/ Jelid , Isilno , Stiff , Scoeff , Xxxxxx , Jselid , Stress , Yyyyyy , Jfelid , Force , Zzzzzz
   COMMON /zzzzzz/ Zz
   REAL disp1 , disp2
   INTEGER idisp , iu
!*****
! THIS ROUTINE IS PHASE II OF STRESS DATA RECOVERY FOR THE SCALAR SPRING
! ELEMENTS ELAS1, ELAS2, ELAS3 AND ELAS4.
!*****
!
!
!
!
! SDR2 VARIABLE CORE
!
!
! BLOCK FOR POINTERS, LOADING TEMPERATURE AND ELEMENT DEFORMATION.
!
!
! SDR2 INPUT AND OUTPUT BLOCK
!
   !>>>>EQUIVALENCE (Scoeff,Icoeff)
!
!
!
   idisp = Ivec - 1
   disp1 = 0.0
   disp2 = 0.0
   IF ( Isilno(1)>0 ) THEN
      iu = idisp + Isilno(1)
      disp1 = Zz(iu)
   ENDIF
   IF ( Isilno(2)>0 ) THEN
      iu = idisp + Isilno(2)
      disp2 = Zz(iu)
   ENDIF
   Jfelid = Jelid
   Force = Stiff*(disp1-disp2)
   IF ( Icoeff==(-1) ) RETURN
   Stress = Scoeff*Force
   Jselid = Jelid
END SUBROUTINE selas2