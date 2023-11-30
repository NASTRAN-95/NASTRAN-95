
SUBROUTINE gust1(Casecc,Dit,Dlt,Frl,Pp,Fol,Gustl,Nfreq,Nload,Xo,V,Nogust,Casnew)
   IMPLICIT NONE
   INTEGER Iz(1) , Sysbuf
   REAL Z(1)
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Iz
   INTEGER Casecc , Casnew , Dit , Dlt , Fol , Frl , Gustl , Nfreq , Nload , Nogust , Pp
   REAL V , Xo
   INTEGER file , i , ibuf1 , ibuf2 , ibuf3 , icc , idx , igsid , igst , igust(2) , ip1 , lcc , lgust(5) , name(2) , nlgust , nogo ,&
         & notrd , nz
   REAL frqset , rgust(5)
   INTEGER korsz
!
!     THE PURPOSE OF THI ROUTINE IS TO GERATE PP,GUSTL,FOL.
!
!     THE ROUTINE PROCEEDS AS FOLLOWS
!
!         FIND  GUST CARD(NO-CARDS--SET NOGUST=1 AND RETURN)
!         PUT GUST CARDS IN CORE
!         READ CASECC -- BUILD GUSTL
!           SUPPLU DLOAD =   FROM GUST =
!
!         CALL GUST1A WITH NEW CASECC
!
   !>>>>EQUIVALENCE (Iz(1),Z(1)) , (rgust(1),lgust(1))
   DATA name/4HGUST , 1H1/ , igust/1005 , 10/
   DATA igst/178/
!
!     INITIALIZE
!
   nz = korsz(Iz)
   ibuf1 = nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   nz = ibuf3 - 1
   Nogust = -1
   nogo = 0
   CALL preloc(*700,Iz(ibuf1),Dit)
   CALL locate(*700,Iz(ibuf1),igust,idx)
!
!     PUT  GUST CARDS IN CORE
!
   file = Dit
   CALL read(*800,*100,Dit,Iz,nz,0,nlgust)
   CALL mesage(-8,0,name)
 100  CALL close(Dit,1)
   icc = nlgust + 1
   CALL gopen(Casecc,Iz(ibuf1),0)
   CALL gopen(Casnew,Iz(ibuf2),1)
   CALL gopen(Gustl,Iz(ibuf3),1)
   nz = nz - nlgust
!
!     BLAST READ A CASE CONTROL RECORD INTO CORE
!
 200  file = Casecc
   CALL read(*600,*300,Casecc,Iz(icc),nz,0,lcc)
   CALL mesage(-8,0,name)
 300  igsid = Iz(icc+igst)
   Iz(icc+12) = igsid
   CALL zeroc(rgust,5)
   IF ( igsid/=0 ) THEN
!
!     FIND GUST ID AMONG GUST CARDS
!
      DO i = 1 , nlgust , 5
         IF ( Iz(i)==igsid ) GOTO 400
      ENDDO
      CALL mesage(31,igsid,name)
      nogo = 1
   ENDIF
   GOTO 500
!
!     FOUND GUST CARD
!
 400  Iz(icc+12) = Iz(i+1)
   igust(1) = igsid
   lgust(2) = Iz(i+1)
   rgust(3) = Z(i+2)
   rgust(4) = Z(i+3)
   rgust(5) = Z(i+4)
   Xo = rgust(4)
   V = rgust(5)
   Nogust = 1
!
!     PUT OUT GUSTL /CASNEW
!
 500  CALL write(Casnew,Iz(icc),lcc,1)
   CALL write(Gustl,lgust,5,1)
   GOTO 200
!
!     END OF FILE ON CASECC
 600  IF ( nogo==1 ) CALL mesage(-61,0,name)
   CALL close(Casecc,1)
   CALL close(Gustl,1)
   CALL close(Casnew,1)
!
!     CALL GUST1A FOR LOADS(W)
!
   CALL gust1a(Dlt,Frl,-Casnew,Dit,Pp,1,Nfreq,Nload,frqset,Fol,notrd)
   CALL dmpfil(-Pp,Iz,nz)
 700  CALL close(Dit,1)
   RETURN
!
!     FILE  ERRORS
!
 800  ip1 = -2
   CALL mesage(ip1,file,name)
END SUBROUTINE gust1