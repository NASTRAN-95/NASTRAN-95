
SUBROUTINE mbprit(Aw,Ac,At)
   IMPLICIT NONE
   REAL Ang(10) , Beta , Boxa , Boxl , Boxw , Cotang(10) , Cr , Ek , Ekbar , Ekm , Gc , Sys , Tang(10) , X(12) , Y(12)
   LOGICAL Asym , Cntrl1 , Cntrl2 , Crank1 , Crank2
   INTEGER Kc , Kc1 , Kc1t , Kc2 , Kc2t , Kct , Mach , N6 , Nbox , Ncb , Njj , Npts0 , Npts1 , Npts2 , Nsb , Nsbd , Ntote
   COMMON /mboxa / X , Y , Tang , Ang , Cotang
   COMMON /mboxc / Njj , Crank1 , Crank2 , Cntrl1 , Cntrl2 , Nbox , Npts0 , Npts1 , Npts2 , Asym , Gc , Cr , Mach , Beta , Ek ,     &
                 & Ekbar , Ekm , Boxl , Boxw , Boxa , Ncb , Nsb , Nsbd , Ntote , Kc , Kc1 , Kc2 , Kct , Kc1t , Kc2t
   COMMON /system/ Sys , N6
   REAL Ac , At , Aw
   INTEGER i
!
!     SUBROUTINE TO PRINT GEOMETRY DATA
!
!
   WRITE (N6,99001) Cntrl2 , Cntrl1 , Crank1 , Crank2 , Asym
99001 FORMAT (1H1,35X,27HSUPERSONIC MACH BOX PROGRAM/1H0,43X,12HCONTROL DATA/L20,9X,6HCNTRL2/L20,9X,6HCNTRL1/L20,9X,                &
             &21HCRANK  (LEADING EDGE)/L20,9X,22HCRANK  (TRAILING EDGE)/L20,9X,14HANTI-SYMMETRIC/L20)
!
   WRITE (N6,99002) (i,X(i),Y(i),Tang(i),Ang(i),i=1,7)
99002 FORMAT (1H-,42X,13HGEOMETRY DATA/1H0,8X,1HN,11X,1HX,17X,1HY,16X,4HTANG,14X,3HANG/(I10,4E18.6))
!
   WRITE (N6,99003) (i,X(i),Y(i),Tang(i),i=8,10) , (i,X(i),Y(i),i=11,12)
99003 FORMAT (I10,3E18.6/I10,3E18.6/I10,3E18.6/(I10,2E18.6))
!
   WRITE (N6,99004) Aw , Ac , At
99004 FORMAT (1H0,5X,23HAREA OF MAIN (SEMISPAN),11X,15HAREA OF CNTRL1 ,18X,14HAREA OF CNTRL2/E22.6,E34.6,E29.6)
END SUBROUTINE mbprit