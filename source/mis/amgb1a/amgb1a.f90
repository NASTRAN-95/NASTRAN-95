!*==amgb1a.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgb1a(Input,Matout,Ajj,Ajjt,Tsonx,Tamach,Tredf)
   USE c_amgbug
   USE c_amgmn
   USE c_bamg1l
   USE c_condas
   USE c_packx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   INTEGER :: Matout
   COMPLEX , DIMENSION(nstns,1) :: Ajj
   COMPLEX , DIMENSION(nstns) :: Ajjt
   INTEGER , DIMENSION(1) :: Tsonx
   REAL , DIMENSION(1) :: Tamach
   REAL , DIMENSION(1) :: Tredf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iajjc , line , nline , nstns3 , nwar
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL amgb1b , amgb1c , amgb1d , bug1 , mesage , pack , read
!
! End of declarations rewritten by SPAG
!
!
!     COMPUTE AJJ MATRIX FOR COMPRESSOR BLADES
!
   DATA name/4HAMGB , 4H1A  /
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
      CALL read(*100,*100,Input,0,-nstns3,0,nwar)
!
!     COMPUTE PARAMETERS
!
      amach = mach*cos(degra*(flowa-stager))
      redf = rfreq*(chord/refcrd)*(refvel/vel)*(mach/amach)
      blspc = bspace/chord
      IF ( debug ) CALL bug1('BAMG1L    ',5,iref,26)
!
!     COMPUTE POINTER FOR LOCATION INTO AJJ MATRIX
!
      iajjc = 1
      IF ( tsonic ) iajjc = nstns*(line-1) + 1
!
!     BRANCH TO SUBSONIC, SUPERSONIC OR TRANSONIC CODE
!
      Tamach(line) = amach
      Tredf(line) = redf
      IF ( amach<=maxmac ) THEN
!
!     SUBSONIC STREAMLINE
!
         CALL amgb1b(Ajj(1,iajjc))
      ELSEIF ( amach>=minmac ) THEN
!
!     SUPERSONIC STREAMLINE
!
         CALL amgb1c(Ajj(1,iajjc))
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
         nn = nn + nstns
!
!     OUTPUT AJJ MATRIX
!
         DO i = 1 , nstns
            IF ( debug ) CALL bug1('SS-AJJL   ',40,Ajj(1,i),nstns*2)
            CALL pack(Ajj(1,i),Matout,mcb)
         ENDDO
      ENDIF
   ENDDO
!
!     PERFORM TRANSONIC INTERPOLATION, IF NECESSARY
!
   IF ( tsonic ) THEN
      IF ( debug ) CALL bug1('TSONX     ',102,Tsonx,nlines)
      IF ( debug ) CALL bug1('TAMACH    ',103,Tamach,nlines)
      IF ( debug ) CALL bug1('TREDF     ',104,Tredf,nlines)
      CALL amgb1d(Ajj,Tsonx,Tamach,Tredf)
!
!     OUTPUT AJJ FOR EACH STREAMLINE
!
      DO nline = 1 , nlines
         ii = nn + 1
         nn = nn + nstns
         DO i = ii , nn
            IF ( debug ) CALL bug1('STS-AJJL  ',110,Ajj(1,i),nstns*2)
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
END SUBROUTINE amgb1a
