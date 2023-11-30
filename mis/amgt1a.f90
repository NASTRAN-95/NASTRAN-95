
SUBROUTINE amgt1a(Input,Matout,Ajj,Tsonx,Tamach,Tredf,Nstns2)
   IMPLICIT NONE
   REAL Amach , Amachr , Blspc , Bspace , Chord , Dcbdzb , Degra , Den , Dum(2) , Mach , Maxmac , Minmac , Pi , Radeg , Redf ,      &
      & Refc , Refcrd , Refden , Refmac , Refstg , Refswp , Refvel , Rfreq , S4pisq , Sigma , Stager , Sweep , Twopi , Vel
   LOGICAL Debug , Tsonic
   INTEGER Ii , Incr , Iref , Iti , Ito , Mcb(7) , Nlines , Nn , Nrow , Nstns , Nstnsx , Sln
   COMMON /amgbug/ Debug
   COMMON /amgmn / Mcb , Nrow , Dum , Refc , Sigma , Rfreq
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /tamg1l/ Iref , Minmac , Maxmac , Nlines , Nstns , Refstg , Refcrd , Refmac , Refden , Refvel , Refswp , Sln , Nstnsx ,   &
                 & Stager , Chord , Dcbdzb , Bspace , Mach , Den , Vel , Sweep , Amach , Redf , Blspc , Amachr , Tsonic
   INTEGER Input , Matout , Nstns2
   COMPLEX Ajj(Nstns2,1)
   REAL Tamach(1) , Tredf(1)
   INTEGER Tsonx(1)
   REAL c3 , c4
   INTEGER i , iajjc , line , name(2) , nline , nstns3 , nwar
!
!     COMPUTE AJJ MATRIX FOR SWEPT TURBOPROP BLADES.
!
   DATA name/4HAMGT , 4H1A  /
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
!
!     COMPUTE PARAMETERS
!
      Amach = Mach
      Redf = Rfreq*(Chord/Refcrd)*(Refvel/Vel)
      Blspc = Bspace/Chord
!
!     COMPUTE C3 AND C4 FOR THIS STREAMLINE.
!
!     INPUT IS POSITIONED AT THE FIRST 10 WORDS OF THE NEXT
!     STREAMLINE WHEN IT RETURNS FROM AMGT1T
!
      CALL amgt1t(Nlines,line,Input,Nstns,c3,c4)
!
      IF ( Debug ) CALL bug1('TAMG1L    ',5,Iref,26)
!
!     COMPUTE POINTER FOR LOCATION INTO AJJ MATRIX
!
      iajjc = 1
      IF ( Tsonic ) iajjc = Nstns2*(line-1) + 1
!
!     BRANCH TO SUBSONIC, SUPERSONIC OR TRANSONIC CODE
!
      Tamach(line) = Amach
      Tredf(line) = Redf
      IF ( Amach<=Maxmac ) THEN
!
!     SUBSONIC STREAMLINE
!
         CALL amgt1b(Ajj(1,iajjc),Nstns2,c3,c4)
      ELSEIF ( Amach>=Minmac ) THEN
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
      IF ( Tsonic ) THEN
         Tsonx(line) = 0
      ELSE
         Ii = Nn + 1
         Nn = Nn + Nstns2
!
!     OUTPUT AJJ MATRIX
!
         DO i = 1 , Nstns2
            IF ( Debug ) CALL bug1('SS-AJJL   ',40,Ajj(1,i),Nstns2*2)
            CALL pack(Ajj(1,i),Matout,Mcb)
         ENDDO
      ENDIF
   ENDDO
!
!     PERFORM TRANSONIC INTERPOLATION, IF NECESSARY
!
   IF ( Tsonic ) THEN
      IF ( Debug ) CALL bug1('TSONX     ',80,Tsonx,Nlines)
      IF ( Debug ) CALL bug1('TAMACH    ',90,Tamach,Nlines)
      IF ( Debug ) CALL bug1('TREDF     ',100,Tredf,Nlines)
      CALL amgt1d(Ajj,Tsonx,Tamach,Tredf,Nstns2)
!
!     OUTPUT AJJ FOR EACH STREAMLINE
!
      DO nline = 1 , Nlines
         Ii = Nn + 1
         Nn = Nn + Nstns2
         DO i = Ii , Nn
            IF ( Debug ) CALL bug1('STS-AJJL  ',110,Ajj(1,i),Nstns2*2)
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
END SUBROUTINE amgt1a
