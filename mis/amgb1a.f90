
SUBROUTINE amgb1a(Input,Matout,Ajj,Ajjt,Tsonx,Tamach,Tredf)
   IMPLICIT NONE
   REAL Amach , Amachr , Blspc , Bspace , Chord , Degra , Den , Dum(2) , Flowa , Mach , Maxmac , Minmac , Pi , Radeg , Radius ,     &
      & Redf , Refc , Refcrd , Refden , Refflo , Refmac , Refstg , Refvel , Rfreq , S4pisq , Sigma , Stager , Twopi , Vel
   LOGICAL Debug , Tsonic
   INTEGER Ii , Incr , Iref , Iti , Ito , Mcb(7) , Nlines , Nn , Nrow , Nstns , Nstnsx , Sln
   COMMON /amgbug/ Debug
   COMMON /amgmn / Mcb , Nrow , Dum , Refc , Sigma , Rfreq
   COMMON /bamg1l/ Iref , Minmac , Maxmac , Nlines , Nstns , Refstg , Refcrd , Refmac , Refden , Refvel , Refflo , Sln , Nstnsx ,   &
                 & Stager , Chord , Radius , Bspace , Mach , Den , Vel , Flowa , Amach , Redf , Blspc , Amachr , Tsonic
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   INTEGER Input , Matout
   COMPLEX Ajj(Nstns,1) , Ajjt(Nstns)
   REAL Tamach(1) , Tredf(1)
   INTEGER Tsonx(1)
   INTEGER i , iajjc , line , name(2) , nline , nstns3 , nwar
!
!     COMPUTE AJJ MATRIX FOR COMPRESSOR BLADES
!
   DATA name/4HAMGB , 4H1A  /
!
!     LOOP ON STREAMLINES, COMPUTE AJJ FOR EACH STREAMLINE AND THEN
!     PACK AJJ INTO AJJL MATRIX AT CORRECT POSITION
!
   Ii = 0
   Nn = 0
   nstns3 = 3*Nstns
   DO line = 1 , Nlines
!
!     READ STREAMLINE DATA (SKIP COORDINATE DATA)
!
      CALL read(*100,*100,Input,Sln,10,0,nwar)
      CALL read(*100,*100,Input,0,-nstns3,0,nwar)
!
!     COMPUTE PARAMETERS
!
      Amach = Mach*cos(Degra*(Flowa-Stager))
      Redf = Rfreq*(Chord/Refcrd)*(Refvel/Vel)*(Mach/Amach)
      Blspc = Bspace/Chord
      IF ( Debug ) CALL bug1('BAMG1L    ',5,Iref,26)
!
!     COMPUTE POINTER FOR LOCATION INTO AJJ MATRIX
!
      iajjc = 1
      IF ( Tsonic ) iajjc = Nstns*(line-1) + 1
!
!     BRANCH TO SUBSONIC, SUPERSONIC OR TRANSONIC CODE
!
      Tamach(line) = Amach
      Tredf(line) = Redf
      IF ( Amach<=Maxmac ) THEN
!
!     SUBSONIC STREAMLINE
!
         CALL amgb1b(Ajj(1,iajjc))
      ELSEIF ( Amach>=Minmac ) THEN
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
      IF ( Tsonic ) THEN
         Tsonx(line) = 0
      ELSE
         Ii = Nn + 1
         Nn = Nn + Nstns
!
!     OUTPUT AJJ MATRIX
!
         DO i = 1 , Nstns
            IF ( Debug ) CALL bug1('SS-AJJL   ',40,Ajj(1,i),Nstns*2)
            CALL pack(Ajj(1,i),Matout,Mcb)
         ENDDO
      ENDIF
   ENDDO
!
!     PERFORM TRANSONIC INTERPOLATION, IF NECESSARY
!
   IF ( Tsonic ) THEN
      IF ( Debug ) CALL bug1('TSONX     ',102,Tsonx,Nlines)
      IF ( Debug ) CALL bug1('TAMACH    ',103,Tamach,Nlines)
      IF ( Debug ) CALL bug1('TREDF     ',104,Tredf,Nlines)
      CALL amgb1d(Ajj,Tsonx,Tamach,Tredf)
!
!     OUTPUT AJJ FOR EACH STREAMLINE
!
      DO nline = 1 , Nlines
         Ii = Nn + 1
         Nn = Nn + Nstns
         DO i = Ii , Nn
            IF ( Debug ) CALL bug1('STS-AJJL  ',110,Ajj(1,i),Nstns*2)
            CALL pack(Ajj(1,i),Matout,Mcb)
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