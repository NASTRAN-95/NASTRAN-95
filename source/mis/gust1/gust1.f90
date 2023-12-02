!*==gust1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gust1(Casecc,Dit,Dlt,Frl,Pp,Fol,Gustl,Nfreq,Nload,Xo,V,Nogust,Casnew)
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Casecc
   INTEGER :: Dit
   INTEGER :: Dlt
   INTEGER :: Frl
   INTEGER :: Pp
   INTEGER :: Fol
   INTEGER :: Gustl
   INTEGER :: Nfreq
   INTEGER :: Nload
   REAL :: Xo
   REAL :: V
   INTEGER :: Nogust
   INTEGER :: Casnew
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , ibuf1 , ibuf2 , ibuf3 , icc , idx , igsid , ip1 , lcc , nlgust , nogo , notrd , nz
   REAL :: frqset
   INTEGER , SAVE :: igst
   INTEGER , DIMENSION(2) , SAVE :: igust , name
   INTEGER , DIMENSION(5) :: lgust
   REAL , DIMENSION(5) :: rgust
   REAL , DIMENSION(1) :: z
   EXTERNAL close , dmpfil , gopen , gust1a , korsz , locate , mesage , preloc , read , write , zeroc
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         CALL preloc(*80,Iz(ibuf1),Dit)
         CALL locate(*80,Iz(ibuf1),igust,idx)
!
!     PUT  GUST CARDS IN CORE
!
         file = Dit
         CALL read(*100,*20,Dit,Iz,nz,0,nlgust)
         CALL mesage(-8,0,name)
 20      CALL close(Dit,1)
         icc = nlgust + 1
         CALL gopen(Casecc,Iz(ibuf1),0)
         CALL gopen(Casnew,Iz(ibuf2),1)
         CALL gopen(Gustl,Iz(ibuf3),1)
         nz = nz - nlgust
         spag_nextblock_1 = 2
      CASE (2)
!
!     BLAST READ A CASE CONTROL RECORD INTO CORE
!
         file = Casecc
         CALL read(*60,*40,Casecc,Iz(icc),nz,0,lcc)
         CALL mesage(-8,0,name)
 40      igsid = Iz(icc+igst)
         Iz(icc+12) = igsid
         CALL zeroc(rgust,5)
         IF ( igsid/=0 ) THEN
!
!     FIND GUST ID AMONG GUST CARDS
!
            DO i = 1 , nlgust , 5
               IF ( Iz(i)==igsid ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            CALL mesage(31,igsid,name)
            nogo = 1
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
!     FOUND GUST CARD
!
         Iz(icc+12) = Iz(i+1)
         igust(1) = igsid
         lgust(2) = Iz(i+1)
         rgust(3) = z(i+2)
         rgust(4) = z(i+3)
         rgust(5) = z(i+4)
         Xo = rgust(4)
         V = rgust(5)
         Nogust = 1
         spag_nextblock_1 = 4
      CASE (4)
!
!     PUT OUT GUSTL /CASNEW
!
         CALL write(Casnew,Iz(icc),lcc,1)
         CALL write(Gustl,lgust,5,1)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     END OF FILE ON CASECC
 60      IF ( nogo==1 ) CALL mesage(-61,0,name)
         CALL close(Casecc,1)
         CALL close(Gustl,1)
         CALL close(Casnew,1)
!
!     CALL GUST1A FOR LOADS(W)
!
         CALL gust1a(Dlt,Frl,-Casnew,Dit,Pp,1,Nfreq,Nload,frqset,Fol,notrd)
         CALL dmpfil(-Pp,Iz,nz)
 80      CALL close(Dit,1)
         RETURN
!
!     FILE  ERRORS
!
 100     ip1 = -2
         CALL mesage(ip1,file,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gust1
