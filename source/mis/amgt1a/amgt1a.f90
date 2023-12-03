!*==amgt1a.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgt1a(Input,Matout,Ajj,Tsonx,Tamach,Tredf,Nstns2)
   USE c_amgbug
   USE c_amgmn
   USE c_condas
   USE c_packx
   USE c_tamg1l
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nstns2
   INTEGER :: Input
   INTEGER :: Matout
   COMPLEX , DIMENSION(Nstns2,1) :: Ajj
   INTEGER , DIMENSION(1) :: Tsonx
   REAL , DIMENSION(1) :: Tamach
   REAL , DIMENSION(1) :: Tredf
!
! Local variable declarations rewritten by SPAG
!
   REAL :: c3 , c4
   INTEGER :: i , iajjc , line , nline , nstns3 , nwar
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL amgt1b , amgt1c , amgt1d , amgt1t , bug1 , mesage , pack , read
!
! End of declarations rewritten by SPAG
!
!
!     COMPUTE AJJ MATRIX FOR SWEPT TURBOPROP BLADES.
!
   DATA name/4HAMGT , 4H1A  /
!
!     LOOP ON STREAMLINES, COMPUTE AJJ FOR EACH STREAMLINE AND THEN
!     PACK AJJ INTO AJJL MATRIX AT CORRECT POSITION
!
   ii = 0
   nn = 0
   nstns3 = 3*nstns
   DO line = 1 , nlines
!
!     READ STREAMLINE DATA (SKIP COORDINATE DATA)
!
      CALL read(*100,*100,Input,sln,10,0,nwar)
!
!     COMPUTE PARAMETERS
!
      amach = mach
      redf = rfreq*(chord/refcrd)*(refvel/vel)
      blspc = bspace/chord
!
!     COMPUTE C3 AND C4 FOR THIS STREAMLINE.
!
!     INPUT IS POSITIONED AT THE FIRST 10 WORDS OF THE NEXT
!     STREAMLINE WHEN IT RETURNS FROM AMGT1T
!
      CALL amgt1t(nlines,line,Input,nstns,c3,c4)
!
      IF ( debug ) CALL bug1('TAMG1L    ',5,iref,26)
!
!     COMPUTE POINTER FOR LOCATION INTO AJJ MATRIX
!
      iajjc = 1
      IF ( tsonic ) iajjc = Nstns2*(line-1) + 1
!
!     BRANCH TO SUBSONIC, SUPERSONIC OR TRANSONIC CODE
!
      Tamach(line) = amach
      Tredf(line) = redf
      IF ( amach<=maxmac ) THEN
!
!     SUBSONIC STREAMLINE
!
         CALL amgt1b(Ajj(1,iajjc),Nstns2,c3,c4)
      ELSEIF ( amach>=minmac ) THEN
!
!     SUPERSONIC STREAMLINE
!
         CALL amgt1c(Ajj(1,iajjc),Nstns2,c3,c4)
      ELSE
!
!     TRANSONIC STREAMLINE. STORE DATA FOR TRANSONIC INTERPOLATION
!
         Tsonx(line) = iajjc
         CYCLE
      ENDIF
!
!     IF THERE ARE NO TRANSONIC STREAMLINES OUTPUT THIS AJJ SUBMATRIX
!
      IF ( tsonic ) THEN
         Tsonx(line) = 0
      ELSE
         ii = nn + 1
         nn = nn + Nstns2
!
!     OUTPUT AJJ MATRIX
!
         DO i = 1 , Nstns2
            IF ( debug ) CALL bug1('SS-AJJL   ',40,Ajj(1,i),Nstns2*2)
            CALL pack(Ajj(1,i),Matout,mcb)
         ENDDO
      ENDIF
   ENDDO
!
!     PERFORM TRANSONIC INTERPOLATION, IF NECESSARY
!
   IF ( tsonic ) THEN
      IF ( debug ) CALL bug1('TSONX     ',80,Tsonx,nlines)
      IF ( debug ) CALL bug1('TAMACH    ',90,Tamach,nlines)
      IF ( debug ) CALL bug1('TREDF     ',100,Tredf,nlines)
      CALL amgt1d(Ajj,Tsonx,Tamach,Tredf,Nstns2)
!
!     OUTPUT AJJ FOR EACH STREAMLINE
!
      DO nline = 1 , nlines
         ii = nn + 1
         nn = nn + Nstns2
         DO i = ii , nn
            IF ( debug ) CALL bug1('STS-AJJL  ',110,Ajj(1,i),Nstns2*2)
            CALL pack(Ajj(1,i),Matout,mcb)
         ENDDO
      ENDDO
   ENDIF
   RETURN
!
!     ERROR MESSAGES
!
!     INPUT NOT POSITIONED PROPERLY OR INCORRECTLY WRITTEN
!
 100  CALL mesage(-7,0,name)
END SUBROUTINE amgt1a