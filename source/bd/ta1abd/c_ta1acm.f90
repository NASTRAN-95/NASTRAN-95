!*==/home/marcusmae/nasa/nastran/SPAGged/C_TA1ACM.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_TA1ACM
   IMPLICIT NONE
   INTEGER, DIMENSION(90) :: Ig
!TA1ABD
!
!     /TA1ACM/ SPECIFIES THE OPEN CORE LABELED COMMONS /ZZEMXX/
!              TO BE USED BY EACH ELEMENT TYPE IN LINK8 OVERLAY
!              TREE.
!              THE LABELED COMMONS /ZZEMXX/ ARE USED ONLY IN CDC
!              AND UNIVAC TO COMPUTE (BY EMGSOC) THE OFFSET OF THE
!              OPEN CORE BETWEEN /ZZEMXX/ AND /ZZEMGX/.
!
!              E.G. /ZZEM24/ IS ASSIGNED TO QUAD4 ELEMENT, TYPE 64
!
!
!
   DATA ig/1 , 0 , 1 , 3 , 4 , 14 , 14 , 14 , 14 , 1 , 2 , 2 , 2 , 2 , 14 , 14 , 14 , 14 , 14 , 2 , 2 , 2 , 2 , 11 , 2 , 2 , 2 , 2 ,&
      & 11 , 11 , 0 , 0 , 0 , 12 , 28 , 18 , 19 , 20 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 13 , 21 ,   &
      & 21 , 21 , 21 , 21 , 21 , 21 , 21 , 21 , 15 , 16 , 24 , 22 , 22 , 22 , 0 , 0 , 25 , 26 , 0 , 48 , 48 , 48 , 48 , 48 , 48 ,   &
      & 48 , 8 , 23 , 13 , 27 , 29 , 29 , 29 , 0 , 0 , 0 , 0/
!
END MODULE c_ta1acm