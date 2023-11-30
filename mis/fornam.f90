
SUBROUTINE fornam(Ielt,Iscan,Name)
   IMPLICIT NONE
   INTEGER Isysbf , Nout
   COMMON /system/ Isysbf , Nout
   INTEGER Ielt , Iscan
   CHARACTER*12 Name
   INTEGER kscan
   Name = ' '
   IF ( Ielt==1 .OR. Ielt==3 .OR. Ielt==10 ) THEN
! ROD, TUBE, CONROD
      IF ( Iscan==2 ) Name = 'AXIAL'
      IF ( Iscan==4 ) Name = 'TORQUE'
   ELSEIF ( Ielt==4 .OR. Ielt==5 ) THEN
! SHEAR, TWIST
      IF ( Iscan==2 ) Name = 'FORCE-1'
      IF ( Iscan==3 ) Name = 'FORCE-2'
   ELSEIF ( Ielt==6 .OR. Ielt==17 .OR. Ielt==19 .OR. Ielt==18 .OR. Ielt==7 .OR. Ielt==8 .OR. Ielt==15 ) THEN
! TRIA1, TRIA2, QUAD1, QUAD2, TRBSC, TRPLT, QDPLT
      IF ( Iscan==2 ) Name = 'MOMENT-X'
      IF ( Iscan==3 ) Name = 'MOMENT-Y'
      IF ( Iscan==5 ) Name = 'SHEAR-X'
      IF ( Iscan==6 ) Name = 'SHEAR-Y'
      IF ( Iscan==4 ) Name = 'TWIST'
   ELSEIF ( Ielt==9 .OR. Ielt==16 .OR. Ielt==62 .OR. Ielt==63 ) THEN
! TRMEM, QDMEM, QDMEM1, QDMEM2
      IF ( Iscan==3 .OR. Iscan==4 ) Name = 'FORCE-12'
      IF ( Iscan==5 .OR. Iscan==6 ) Name = 'FORCE-23'
      IF ( Iscan==7 .OR. Iscan==8 ) Name = 'FORCE-34'
      IF ( Iscan==2 .OR. Iscan==9 ) Name = 'FORCE-41'
      IF ( Iscan==10 ) Name = 'KICK ON1'
      IF ( Iscan==12 ) Name = 'KICK ON2'
      IF ( Iscan==14 ) Name = 'KICK ON3'
      IF ( Iscan==16 ) Name = 'KICK ON4'
      IF ( Iscan==11 ) Name = 'SHEAR-XY'
      IF ( Iscan==13 ) Name = 'SHEAR-YZ'
      IF ( Iscan==15 ) Name = 'SHEAR-ZX'
      IF ( Iscan==17 ) Name = 'SHEAR'
   ELSEIF ( Ielt==11 .OR. Ielt==12 .OR. Ielt==13 .OR. Ielt==80 ) THEN
! ELAS1, ELAS2, ELAS3, IS2D8
      IF ( Iscan==2 ) Name = 'CIRCUM'
      IF ( Iscan==4 .AND. Iscan==9 ) Name = 'FORCE-1'
      IF ( Iscan==3 .AND. Iscan==6 ) Name = 'FORCE-2'
      IF ( Iscan==5 .AND. Iscan==8 ) Name = 'FORCE-3'
      IF ( Iscan==2 .AND. Iscan==7 ) Name = 'FORCE-4'
   ELSEIF ( Ielt==34 .OR. Ielt==81 ) THEN
! BAR, ELBOW
      IF ( Iscan==5 .OR. Iscan==6 ) Name = 'SHEAR'
      IF ( Iscan==2 .OR. Iscan==3 ) Name = 'MOMENT-A'
      IF ( Iscan==4 .OR. Iscan==5 ) Name = 'MOMENT-B'
      IF ( Iscan==8 ) Name = 'AXIAL'
      IF ( Iscan==9 ) Name = 'TORQUE'
   ELSEIF ( Ielt==35 ) THEN
! CONEAX
      IF ( Iscan==3 ) Name = 'MOMENT-U'
      IF ( Iscan==4 ) Name = 'MOMENT-V'
      IF ( Iscan==6 ) Name = 'SHEAR-XY'
      IF ( Iscan==7 ) Name = 'SHEAR-YZ'
   ELSEIF ( Ielt==36 ) THEN
! TRIARG
      kscan = mod(Iscan,3)
      IF ( kscan==2 ) Name = 'RADIAL'
      IF ( kscan==3 ) Name = 'CIRCUM'
      IF ( kscan==1 ) Name = 'AXIAL'
   ELSEIF ( Ielt==37 ) THEN
! TRAPRG
      kscan = mod(Iscan,3)
      IF ( kscan==2 ) Name = 'RADIAL'
      IF ( kscan==3 ) Name = 'CIRCUM'
      IF ( kscan==1 ) Name = 'AXIAL'
   ELSEIF ( Ielt==38 ) THEN
! TORDRG
      kscan = mod(Iscan,6)
      IF ( kscan==2 ) Name = 'RADIAL'
      IF ( kscan==3 ) Name = 'CIRCUM'
      IF ( kscan==4 ) Name = 'AXIAL'
      IF ( kscan==5 ) Name = 'MOMENT'
      IF ( kscan==1 ) Name = 'CURV'
   ELSEIF ( Ielt==70 .OR. Ielt==71 ) THEN
! TRIAAX, TRAPAX
      kscan = mod(Iscan,4)
      IF ( kscan==3 ) Name = 'RADIAL'
      IF ( kscan==0 ) Name = 'CIRCUM'
      IF ( kscan==1 ) Name = 'AXIAL'
   ELSEIF ( Ielt/=64 .AND. Ielt/=83 ) THEN
      WRITE (Nout,99001) Ielt
99001 FORMAT (//' SCAN MODULE PROCESSING UNKNOWN ELEMENT NUMBER ',I8,//)
      CALL mesage(-61,0,0)
   ELSE
! QUAD4, TRIA3
      IF ( Iscan==2 .OR. Iscan==3 ) Name = 'FX+FY'
      IF ( Iscan==4 ) Name = 'FXY'
      IF ( Iscan==5 .OR. Iscan==6 ) Name = 'MX+MY'
      IF ( Iscan==7 ) Name = 'MXY'
      IF ( Iscan==8 .OR. Iscan==9 ) Name = 'VX+VY'
   ENDIF
END SUBROUTINE fornam