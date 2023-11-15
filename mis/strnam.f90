
SUBROUTINE strnam(Ielt,Iscan,Name)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dum(21)
   INTEGER Isysbf , Nout
   LOGICAL Layerd
   COMMON /system/ Isysbf , Nout
   COMMON /xscanx/ Dum , Layerd
!
! Dummy argument declarations
!
   INTEGER Ielt , Iscan
   CHARACTER*12 Name
!
! Local variable declarations
!
   INTEGER kscan
!
! End of declarations
!
!      PRINT *,' ENTERRING STRNAM,IELT,ISCAN=',IELT,ISCAN
   Name = ' '
   IF ( Ielt==1 .OR. Ielt==3 .OR. Ielt==10 ) THEN
! ROD, TUBE, CONROD
      IF ( Iscan==2 ) Name = 'AXIAL'
      IF ( Iscan==4 ) Name = 'TORSIONAL'
      IF ( Iscan==3 ) Name = 'MARGIN'
      IF ( Iscan==5 ) Name = 'MARGIN'
   ELSEIF ( Ielt==4 .OR. Ielt==5 ) THEN
! SHEAR, TWIST
      IF ( Iscan==2 ) Name = 'MAX-SHR'
      IF ( Iscan==4 ) Name = 'MARGIN'
      IF ( Iscan==3 ) Name = 'AVG'
   ELSEIF ( Ielt==6 .OR. Ielt==17 .OR. Ielt==19 .OR. Ielt==18 .OR. Ielt==7 .OR. Ielt==8 .OR. Ielt==15 ) THEN
! TRIA1, TRIA2, QUAD1, QUAD2, TRBSC, TRPLT, QDPLT
      IF ( Iscan==3 .OR. Iscan==11 ) Name = 'NORM-X'
      IF ( Iscan==4 .OR. Iscan==12 ) Name = 'NORM-Y'
      IF ( Iscan==5 .OR. Iscan==13 ) Name = 'SHEAR-XY'
      IF ( Iscan==7 .OR. Iscan==15 ) Name = 'MAJOR'
      IF ( Iscan==8 .OR. Iscan==16 ) Name = 'MINOR'
      IF ( Iscan==9 .OR. Iscan==17 ) Name = 'MAX-SHR'
   ELSEIF ( Ielt==9 .OR. Ielt==16 .OR. Ielt==62 .OR. Ielt==63 ) THEN
! TRMEM, QDMEM, QDMEM1, QDMEM2
      IF ( Iscan==2 ) Name = 'NORM-X'
      IF ( Iscan==3 ) Name = 'NORM-Y'
      IF ( Iscan==4 ) Name = 'SHEAR-XY'
      IF ( Iscan==6 ) Name = 'MAJOR'
      IF ( Iscan==7 ) Name = 'MINOR'
      IF ( Iscan==8 ) Name = 'MAX-SHR'
   ELSEIF ( Ielt==11 .OR. Ielt==12 .OR. Ielt==13 .OR. Ielt==80 ) THEN
! ELAS1, ELAS2, ELAS3, IS2D8
      IF ( Iscan==2 ) Name = 'OCT-SHR'
   ELSEIF ( Ielt==34 .OR. Ielt==81 ) THEN
! BAR, ELBOW
      IF ( Iscan==7 .OR. Iscan==8 ) Name = 'SB-MAX'
      IF ( Iscan==14 .OR. Iscan==15 ) Name = 'SB-MAX'
      IF ( Iscan==9 .OR. Iscan==16 ) Name = 'MARGIN'
      IF ( Iscan==6 ) Name = 'AXIAL'
   ELSEIF ( Ielt==35 ) THEN
! CONEAX
      IF ( Iscan==4 .OR. Iscan==22 ) Name = 'NORM-U'
      IF ( Iscan==5 .OR. Iscan==23 ) Name = 'NORM-V'
      IF ( Iscan==6 .OR. Iscan==24 ) Name = 'SHEAR-UV'
      IF ( Iscan==8 .OR. Iscan==26 ) Name = 'MAJOR'
      IF ( Iscan==9 .OR. Iscan==27 ) Name = 'MINOR'
      IF ( Iscan==10 .OR. Iscan==28 ) Name = 'MAX-SHR'
   ELSEIF ( Ielt==36 ) THEN
! TRIARG
      IF ( Iscan==2 ) Name = 'RADIAL'
      IF ( Iscan==3 ) Name = 'CIRCUM'
      IF ( Iscan==4 ) Name = 'AXIAL'
      IF ( Iscan==5 ) Name = 'SHEAR'
   ELSEIF ( Ielt==37 ) THEN
! TRAPRG
      kscan = mod(Iscan,4)
      IF ( kscan==2 .AND. Iscan/=18 ) Name = 'RADIAL'
      IF ( kscan==3 ) Name = 'CIRCUM'
      IF ( kscan==0 ) Name = 'AXIAL'
      IF ( kscan==1 ) Name = 'SHEAR'
      IF ( kscan==2 .AND. Iscan/=2 ) Name = 'SHR-FORC'
   ELSEIF ( Ielt==38 ) THEN
! TORDRG
      kscan = mod(Iscan,5)
      IF ( kscan==2 ) Name = 'MEM-T'
      IF ( kscan==3 ) Name = 'MEM-C'
      IF ( kscan==4 ) Name = 'FLEX-T'
      IF ( kscan==0 ) Name = 'FLEX-C'
      IF ( kscan==1 ) Name = 'SHR-FORC'
   ELSEIF ( Ielt==65 .OR. Ielt==66 ) THEN
! IHEX1, IHEX2
      kscan = mod(Iscan,22)
      IF ( kscan==3 ) Name = 'NORM-X'
      IF ( kscan==4 ) Name = 'SHEAR-XY'
      IF ( kscan==5 ) Name = 'PRINC-A'
      IF ( kscan==9 ) Name = 'MEAN'
      IF ( kscan==11 ) Name = 'NORM-Y'
      IF ( kscan==12 ) Name = 'SHEAR-YZ'
      IF ( kscan==13 ) Name = 'PRINC-B'
      IF ( kscan==17 ) Name = 'NORM-Z'
      IF ( kscan==18 ) Name = 'SHEAR-ZX'
      IF ( kscan==19 ) Name = 'PRINC-C'
      IF ( kscan==10 ) Name = 'MAX-SHR'
   ELSEIF ( Ielt==67 ) THEN
! IHEX3
      kscan = mod(Iscan,23)
      IF ( kscan==3 ) Name = 'NORM-X'
      IF ( kscan==4 ) Name = 'SHEAR-XY'
      IF ( kscan==5 ) Name = 'PRINC-A'
      IF ( kscan==9 ) Name = 'MEAN'
      IF ( kscan==12 ) Name = 'NORM-Y'
      IF ( kscan==13 ) Name = 'SHEAR-YZ'
      IF ( kscan==14 ) Name = 'PRINC-B'
      IF ( kscan==18 ) Name = 'NORM-Z'
      IF ( kscan==19 ) Name = 'SHEAR-ZX'
      IF ( kscan==20 ) Name = 'PRINC-C'
      IF ( kscan==10 ) Name = 'MAX-SHR'
   ELSEIF ( Ielt==70 .OR. Ielt==71 ) THEN
! TRIAAX, TRAPAX
      kscan = mod(Iscan,8)
      IF ( kscan==3 ) Name = 'RADIAL'
      IF ( kscan==4 ) Name = 'AXIAL'
      IF ( kscan==5 ) Name = 'CIRCUM'
      IF ( kscan==6 ) Name = 'MEM-C'
      IF ( kscan==7 ) Name = 'FLEX-T'
      IF ( kscan==0 ) Name = 'FLEX-C'
   ELSEIF ( Ielt/=64 .AND. Ielt/=83 ) THEN
      WRITE (Nout,99001) Ielt
99001 FORMAT (//,' SCAN MODULE PROCESSING UNKNOWN ELEMENT NUMBER ',I8,//)
      CALL mesage(-61,0,0)
! QUAD4, TRIA3 WITHOUT LAMINATION
   ELSEIF ( Layerd ) THEN
!   QUAD4 AND TRIA3 WITH LAMINATION
      kscan = mod(Iscan,10)
      IF ( Iscan==5 ) Name = 'NORMAL-1'
      IF ( Iscan==6 ) Name = 'NORMAL-2'
      IF ( Iscan==7 ) Name = 'SHEAR-12'
      IF ( Iscan==0 ) Name = 'SHEAR-1Z'
      IF ( Iscan==1 ) Name = 'SHEAR-2Z'
   ELSE
      IF ( Iscan==3 .OR. Iscan==11 ) Name = 'NORMAL-X'
      IF ( Iscan==4 .OR. Iscan==12 ) Name = 'NORMAL-Y'
      IF ( Iscan==5 .OR. Iscan==13 ) Name = 'SHEAR-XY'
      IF ( Iscan==7 .OR. Iscan==15 ) Name = 'MAJOR'
      IF ( Iscan==18 .OR. Iscan==16 ) Name = 'MINOR'
      IF ( Iscan==9 .OR. Iscan==17 ) Name = 'MAX-SHR'
   ENDIF
!      PRINT *,' RETURNING FROM STRNAM,FIELD=',NAME
END SUBROUTINE strnam
