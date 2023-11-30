
SUBROUTINE smcrtr(Zr,Zd)
   IMPLICIT NONE
   INCLUDE 'SMCOMX.COM'
   DOUBLE PRECISION Xnd(10)
   REAL Xns(10)
   COMMON /zzzzzz/ Xns
   DOUBLE PRECISION Zd(10)
   REAL Zr(10)
   INTEGER istr , k , num , Mrow , Mtype , Mstr , Mterms
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
   IF ( Ktype==2 ) THEN
      IF ( Mtype==2 ) THEN
!
! INPUT IS RD AND DECOMPOSITION TO BE DONE IN RD
!
         istr = Mstr + Iskip
         num = Mterms - Iskip
         DO k = 1 , num
            Zd(Indexvd+k-1) = Xnd(istr+k-1)
         ENDDO
         Indexvd = Indexvd + num
         Indexv = Indexv + 2*num
      ELSEIF ( Mtype==3 ) THEN
!
! INPUT IS CS AND DECOMPOSITION TO BE DONE IN RD
!
         istr = Mstr + Iskip*2
         num = Mterms - Iskip
         DO k = 1 , num
            Zd(Indexvd+k-1) = Xns(istr+(k-1)*2)
         ENDDO
         Indexv = Indexv + 2*num
         Indexvd = Indexvd + num
      ELSEIF ( Mtype==4 ) THEN
!
! INPUT IS CD AND DECOMPOSITION TO BE DONE IN RD
!
         istr = Mstr + Iskip*2
         num = Mterms - Iskip
         DO k = 1 , num
            Zd(Indexvd+k-1) = Xnd(istr+(k-1)*2)
         ENDDO
         Indexvd = Indexvd + num
         Indexv = Indexv + 2*num
      ELSE
!
! INPUT IS RS AND DECOMPOSITION TO BE DONE IN RD
!
         istr = Mstr + Iskip
         num = Mterms - Iskip
         DO k = 1 , num
            Zd(Indexvd+k-1) = Xns(istr+k-1)
         ENDDO
         Indexv = Indexv + 2*num
         Indexvd = Indexvd + num
      ENDIF
   ELSEIF ( Ktype==3 ) THEN
      IF ( Mtype==2 ) THEN
!
! INPUT IS RD AND DECOMPOSITION TO BE DONE IN CS
!
         istr = Mstr + Iskip
         num = Mterms - Iskip
         DO k = 1 , num
            Zr(Indexv+(k-1)*2) = Xnd(istr+k-1)
            Zr(Indexv+(k-1)*2+1) = 0.0D0
         ENDDO
         Indexv = Indexv + 2*num
      ELSEIF ( Mtype==3 ) THEN
!
! INPUT IS CS AND DECOMPOSITION TO BE DONE IN CS
!
         istr = Mstr + Iskip*2
         num = (Mterms-Iskip)*2
         DO k = 1 , num
            Zr(Indexv+k-1) = Xns(istr+k-1)
         ENDDO
         Indexv = Indexv + num
      ELSEIF ( Mtype==4 ) THEN
!
! INPUT IS CD AND DECOMPOSITION TO BE DONE IN CS
!
         istr = Mstr + Iskip*2
         num = (Mterms-Iskip)*2
         DO k = 1 , num
            Zr(Indexv+k-1) = Xnd(istr+k-1)
         ENDDO
         Indexv = Indexv + num
      ELSE
!
! INPUT IS RS AND DECOMPOSITION TO BE DONE IN CS
!
         istr = Mstr + Iskip
         num = Mterms - Iskip
         DO k = 1 , num
            Zr(Indexv+(k-1)*2) = Xns(istr+k-1)
            Zr(Indexv+(k-1)*2+1) = 0.0
         ENDDO
         Indexv = Indexv + 2*num
      ENDIF
   ELSEIF ( Ktype==4 ) THEN
      IF ( Mtype==2 ) THEN
!
! INPUT IS RD AND DECOMPOSITION TO BE DONE IN CD
!
         istr = Mstr + Iskip
         num = Mterms - Iskip
         DO k = 1 , num
            Zd(Indexvd+(k-1)*2) = Xnd(istr+k-1)
            Zd(Indexvd+(k-1)*2+1) = 0.0D0
         ENDDO
         Indexv = Indexv + 4*num
         Indexvd = Indexvd + 2*num
      ELSEIF ( Mtype==3 ) THEN
!
! INPUT IS CS AND DECOMPOSITION TO BE DONE IN CD
!
         istr = Mstr + Iskip*2
         num = (Mterms-Iskip)*2
         DO k = 1 , num
            Zd(Indexvd+k-1) = Xns(istr+k-1)
         ENDDO
         Indexv = Indexv + 2*num
         Indexvd = Indexvd + num
      ELSEIF ( Mtype==4 ) THEN
!
! INPUT IS CD AND DECOMPOSITION TO BE DONE IN CD
!
         istr = Mstr + Iskip*2
         num = (Mterms-Iskip)*2
         DO k = 1 , num
            Zd(Indexvd+k-1) = Xnd(istr+k-1)
         ENDDO
         Indexv = Indexv + 2*num
         Indexvd = Indexvd + num
      ELSE
!
! INPUT IS RS AND DECOMPOSITION TO BE DONE IN CD
!
         istr = Mstr + Iskip
         num = Mterms - Iskip
         DO k = 1 , num
            Zd(Indexvd+(k-1)*2) = Xns(istr+k-1)
            Zd(Indexvd+(k-1)*2+1) = 0.0
         ENDDO
         Indexv = Indexv + 4*num
         Indexvd = Indexvd + 2*num
      ENDIF
   ELSEIF ( Mtype==2 ) THEN
!
! INPUT IS RD AND DECOMPOSITION TO BE DONE IN RS
!
      istr = Mstr + Iskip
      num = Mterms - Iskip
      DO k = 1 , num
         Zr(Indexv+k-1) = Xnd(istr+k-1)
      ENDDO
      Indexv = Indexv + num
   ELSEIF ( Mtype==3 ) THEN
!
! INPUT IS CS AND DECOMPOSITION TO BE DONE IN RS
!
      istr = Mstr + Iskip*2
      num = Mterms - Iskip
      DO k = 1 , num
         Zr(Indexv+k-1) = Xns(istr+(k-1)*2)
      ENDDO
      Indexv = Indexv + num
   ELSEIF ( Mtype==4 ) THEN
!
! INPUT IS CD AND DECOMPOSITION TO BE DONE IN RS
!
      istr = Mstr + Iskip*2
      num = Mterms - Iskip
      DO k = 1 , num
         Zr(Indexv+k-1) = Xnd(istr+(k-1)*2)
      ENDDO
      Indexv = Indexv + num
   ELSE
!
! INPUT IS RS AND DECOMPOSITION TO BE DONE IN RS
!
      istr = Mstr + Iskip
      num = Mterms - Iskip
      DO k = 1 , num
         Zr(Indexv+k-1) = Xns(istr+k-1)
      ENDDO
      Indexv = Indexv + num
   ENDIF
END SUBROUTINE smcrtr