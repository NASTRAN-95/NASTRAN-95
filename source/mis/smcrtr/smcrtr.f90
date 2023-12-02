!*==smcrtr.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE smcrtr(Zr,Zd)
   IMPLICIT NONE
   USE I_SMCOMX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(10) :: Zr
   REAL*8 , DIMENSION(10) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: istr , k , mrow , mstr , mterms , mtype , num
   REAL*8 , DIMENSION(10) :: xnd
!
! End of declarations rewritten by SPAG
!
!
!  THIS SUBROUTINE MOVES DATA FROM STRINGS TO OPEN CORE AND PERFORMS
!  ANY TYPE CONVERSIONS REQUIRED.  KTYPE IS THE TYPE THAT THE
!  DECOMPOSITION IS TO BE DONE.  MTYPE IS THE TYPE OF INPUT DATA ON
!  THE MATRIX TO BE DECOMPOSED.  ISKIP IS THE NUMBER OF TERMS AT THE
!  BEGINNING OF THE STRING TO SKIP OVER.
!
   !>>>>EQUIVALENCE (Xns,Xnd)
   !>>>>EQUIVALENCE (Mblk(6),Mterms) , (Mblk(5),Mstr)
   !>>>>EQUIVALENCE (Mblk(4),Mrow) , (Mblk(2),Mtype)
   IF ( ktype==2 ) THEN
      IF ( mtype==2 ) THEN
!
! INPUT IS RD AND DECOMPOSITION TO BE DONE IN RD
!
         istr = mstr + iskip
         num = mterms - iskip
         DO k = 1 , num
            Zd(indexvd+k-1) = xnd(istr+k-1)
         ENDDO
         indexvd = indexvd + num
         indexv = indexv + 2*num
      ELSEIF ( mtype==3 ) THEN
!
! INPUT IS CS AND DECOMPOSITION TO BE DONE IN RD
!
         istr = mstr + iskip*2
         num = mterms - iskip
         DO k = 1 , num
            Zd(indexvd+k-1) = Xns(istr+(k-1)*2)
         ENDDO
         indexv = indexv + 2*num
         indexvd = indexvd + num
      ELSEIF ( mtype==4 ) THEN
!
! INPUT IS CD AND DECOMPOSITION TO BE DONE IN RD
!
         istr = mstr + iskip*2
         num = mterms - iskip
         DO k = 1 , num
            Zd(indexvd+k-1) = xnd(istr+(k-1)*2)
         ENDDO
         indexvd = indexvd + num
         indexv = indexv + 2*num
      ELSE
!
! INPUT IS RS AND DECOMPOSITION TO BE DONE IN RD
!
         istr = mstr + iskip
         num = mterms - iskip
         DO k = 1 , num
            Zd(indexvd+k-1) = Xns(istr+k-1)
         ENDDO
         indexv = indexv + 2*num
         indexvd = indexvd + num
      ENDIF
   ELSEIF ( ktype==3 ) THEN
      IF ( mtype==2 ) THEN
!
! INPUT IS RD AND DECOMPOSITION TO BE DONE IN CS
!
         istr = mstr + iskip
         num = mterms - iskip
         DO k = 1 , num
            Zr(indexv+(k-1)*2) = xnd(istr+k-1)
            Zr(indexv+(k-1)*2+1) = 0.0D0
         ENDDO
         indexv = indexv + 2*num
      ELSEIF ( mtype==3 ) THEN
!
! INPUT IS CS AND DECOMPOSITION TO BE DONE IN CS
!
         istr = mstr + iskip*2
         num = (mterms-iskip)*2
         DO k = 1 , num
            Zr(indexv+k-1) = Xns(istr+k-1)
         ENDDO
         indexv = indexv + num
      ELSEIF ( mtype==4 ) THEN
!
! INPUT IS CD AND DECOMPOSITION TO BE DONE IN CS
!
         istr = mstr + iskip*2
         num = (mterms-iskip)*2
         DO k = 1 , num
            Zr(indexv+k-1) = xnd(istr+k-1)
         ENDDO
         indexv = indexv + num
      ELSE
!
! INPUT IS RS AND DECOMPOSITION TO BE DONE IN CS
!
         istr = mstr + iskip
         num = mterms - iskip
         DO k = 1 , num
            Zr(indexv+(k-1)*2) = Xns(istr+k-1)
            Zr(indexv+(k-1)*2+1) = 0.0
         ENDDO
         indexv = indexv + 2*num
      ENDIF
   ELSEIF ( ktype==4 ) THEN
      IF ( mtype==2 ) THEN
!
! INPUT IS RD AND DECOMPOSITION TO BE DONE IN CD
!
         istr = mstr + iskip
         num = mterms - iskip
         DO k = 1 , num
            Zd(indexvd+(k-1)*2) = xnd(istr+k-1)
            Zd(indexvd+(k-1)*2+1) = 0.0D0
         ENDDO
         indexv = indexv + 4*num
         indexvd = indexvd + 2*num
      ELSEIF ( mtype==3 ) THEN
!
! INPUT IS CS AND DECOMPOSITION TO BE DONE IN CD
!
         istr = mstr + iskip*2
         num = (mterms-iskip)*2
         DO k = 1 , num
            Zd(indexvd+k-1) = Xns(istr+k-1)
         ENDDO
         indexv = indexv + 2*num
         indexvd = indexvd + num
      ELSEIF ( mtype==4 ) THEN
!
! INPUT IS CD AND DECOMPOSITION TO BE DONE IN CD
!
         istr = mstr + iskip*2
         num = (mterms-iskip)*2
         DO k = 1 , num
            Zd(indexvd+k-1) = xnd(istr+k-1)
         ENDDO
         indexv = indexv + 2*num
         indexvd = indexvd + num
      ELSE
!
! INPUT IS RS AND DECOMPOSITION TO BE DONE IN CD
!
         istr = mstr + iskip
         num = mterms - iskip
         DO k = 1 , num
            Zd(indexvd+(k-1)*2) = Xns(istr+k-1)
            Zd(indexvd+(k-1)*2+1) = 0.0
         ENDDO
         indexv = indexv + 4*num
         indexvd = indexvd + 2*num
      ENDIF
   ELSEIF ( mtype==2 ) THEN
!
! INPUT IS RD AND DECOMPOSITION TO BE DONE IN RS
!
      istr = mstr + iskip
      num = mterms - iskip
      DO k = 1 , num
         Zr(indexv+k-1) = xnd(istr+k-1)
      ENDDO
      indexv = indexv + num
   ELSEIF ( mtype==3 ) THEN
!
! INPUT IS CS AND DECOMPOSITION TO BE DONE IN RS
!
      istr = mstr + iskip*2
      num = mterms - iskip
      DO k = 1 , num
         Zr(indexv+k-1) = Xns(istr+(k-1)*2)
      ENDDO
      indexv = indexv + num
   ELSEIF ( mtype==4 ) THEN
!
! INPUT IS CD AND DECOMPOSITION TO BE DONE IN RS
!
      istr = mstr + iskip*2
      num = mterms - iskip
      DO k = 1 , num
         Zr(indexv+k-1) = xnd(istr+(k-1)*2)
      ENDDO
      indexv = indexv + num
   ELSE
!
! INPUT IS RS AND DECOMPOSITION TO BE DONE IN RS
!
      istr = mstr + iskip
      num = mterms - iskip
      DO k = 1 , num
         Zr(indexv+k-1) = Xns(istr+k-1)
      ENDDO
      indexv = indexv + num
   ENDIF
END SUBROUTINE smcrtr
