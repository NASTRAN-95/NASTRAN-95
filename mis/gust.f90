
SUBROUTINE gust
   IMPLICIT NONE
   REAL Bov , Q , Rmach , Xm(2)
   INTEGER Iz(1) , Nogust , Sysbuf
   COMMON /blank / Nogust , Bov , Rmach , Q
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Iz
   INTEGER acpt , casecc , cstma , dit , dlt , frl , iblock(11) , ibuf1 , name(2) , ncolw , nfreq , nload , nogo , nrowj , nz ,     &
         & phf , phf1 , qhjl , scr1 , scr2 , scr3 , scr4 , scr5 , scr6 , scr7
   INTEGER korsz
   REAL rblock(11) , v , xo
!
!     THE PURPOSE OF THIS MODULE IS TO COMPUTE STATIONARY VERTICAL GUST
!         LOADS FOR USE IN AEROLASTIC ANALYSIS
!
!     DMAP CALLING SEQUENCE
!
!         GUST   CASECC,DLT,FRL,QHJL,,,ACPT,CSTMA,PHF1/PHF/V,N,NOGUST/
!                V,N,BOV/C,Y,MACH/C,Y,Q  $
!
!     GUST USES SEVEN SCRATCH FILES
   !>>>>EQUIVALENCE (Xm(1),Nogust) , (iblock(1),rblock(1))
   DATA casecc , dlt , frl , qhjl , acpt , cstma , phf1 , phf , scr1 , scr2 , scr3 , scr4/101 , 102 , 103 , 105 , 108 , 109 , 110 , &
      & 201 , 301 , 302 , 303 , 304/
   DATA dit , scr5 , scr6 , scr7/104 , 305 , 306 , 307/ , name/4HGUST , 1H / , rblock/11*0.0/
!
!     GUST1  GENERATES A FREQUENCY FUNCTION TABLE(SCR1)
!                        FOL DATA BLOCK          (SCR2)
!                        A IMAGE OF GUST CARDS  SID,DLOAD,WG,X0,V(SCR4)
!                      AND SUPPLIES NFREQ,NLOAD,XO,V,NOGUST
!
   CALL gust1(casecc,dit,dlt,frl,scr1,scr2,scr4,nfreq,nload,xo,v,Nogust,scr3)
   IF ( Nogust<0 ) RETURN
!
!     GUST2 COMPUTES WJ MATRIX(SCR3)
!
   CALL gust2(scr2,scr3,acpt,xo,v,cstma,qhjl)
!
!     SET UP FOR ADRI
!
   nz = korsz(Iz)
   ibuf1 = nz - Sysbuf + 1
   Xm(1) = Bov
   Xm(2) = Rmach
   CALL gopen(scr2,Iz(ibuf1),0)
   CALL bckrec(scr2)
   CALL fread(scr2,Iz,-2,0)
   CALL fread(scr2,Iz,nfreq,1)
   CALL close(scr2,1)
   nz = nz - nfreq
!
!     ADRI INTERPOLATES ON QHJL PUTTING OUTPUT ON SCR2 (QHJK)
!
   CALL adri(Iz,nfreq,nz,qhjl,scr2,scr5,scr6,scr7,nrowj,ncolw,nogo)
   IF ( nogo==1 ) CALL mesage(-61,0,name)
!
!     GUST3  MULTIPLIES QHJK BY WJ ONTO SCR5
!             SCR5 IS MULTIPLIED BY LOAD FUNCTION,WG,AND Q ONTO
!             SCR6
!
   CALL gust3(scr2,scr3,scr1,scr4,scr5,scr6,Q,nfreq,nload,nrowj,ncolw)
!                QHJK  WJ  P    GUST POEL
!
!
!     SET UP TO ADD LOADS
!
   Nogust = 1
   iblock(1) = 1
   rblock(2) = 1.0
   iblock(7) = 1
   rblock(8) = 1.0
   CALL ssg2c(scr6,phf1,phf,1,iblock)
END SUBROUTINE gust