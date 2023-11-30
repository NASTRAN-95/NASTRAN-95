
SUBROUTINE amgt1d(Ajj,Tsonx,Tamach,Tredf,Nstns2)
   IMPLICIT NONE
   REAL Amach , Amachr , Blspc , Bspace , Chord , Dcbdzb , Den , Dum(2) , Redf , Refc , Refcrd , Refden , Refmac , Refstg , Refswp ,&
      & Refvel , Rfreq , Sigma , Stag , Sweep , Tsonic , Vel
   INTEGER Iref , Mach , Maxmac , Mcb(7) , Minmac , Nlines , Nrow , Nstns , Nstnsx , Sln
   COMMON /amgmn / Mcb , Nrow , Dum , Refc , Sigma , Rfreq
   COMMON /tamg1l/ Iref , Minmac , Maxmac , Nlines , Nstns , Refstg , Refcrd , Refmac , Refden , Refvel , Refswp , Sln , Nstnsx ,   &
                 & Stag , Chord , Dcbdzb , Bspace , Mach , Den , Vel , Sweep , Amach , Redf , Blspc , Amachr , Tsonic
   INTEGER Nstns2
   COMPLEX Ajj(Nstns2,1)
   REAL Tamach(1) , Tredf(1)
   INTEGER Tsonx(1)
   INTEGER i , nline , nline1 , nline2 , nnline , ns , numm
!
!     TRANSONIC INTERPOLATION CODE FOR SWEPT TURBOPROPS.
!
!
!
!
!
   numm = 2*Nstns2*Nstns2
   DO nline = 1 , Nlines
      IF ( Tsonx(nline)==0 ) CYCLE
      ns = 0
      IF ( nline==1 ) THEN
!       SEARCH FOR 1ST--2--KNOWN STREAMLINES
         nline1 = 0
         GOTO 100
      ELSEIF ( Tamach(nline)>=1.0 ) THEN
!        SUPERSONIC
         IF ( nline/=Nlines ) THEN
            ns = 1
            nline1 = 0
            GOTO 100
         ENDIF
      ELSE
!       SUBSONIC
         IF ( nline==2 ) nline1 = 1
         IF ( nline==2 ) GOTO 100
      ENDIF
 50   nline1 = nline - 2
      nline2 = nline - 1
      CALL intert(nline,nline1,nline2,numm,Ajj,Tamach)
      CYCLE
 100  nline2 = 0
      nnline = nline + 1
      DO i = nnline , Nlines
         IF ( nline2/=0 ) EXIT
         IF ( Tsonx(i)==0 ) THEN
            IF ( nline1==0 ) nline1 = i
            IF ( nline1/=i ) nline2 = i
         ENDIF
      ENDDO
      IF ( ns==0 ) THEN
         CALL intert(nline,nline1,nline2,numm,Ajj,Tamach)
      ELSE
         IF ( nline1==0 ) GOTO 50
         IF ( nline2==0 ) THEN
            nline2 = nline1
            nline1 = nline - 1
         ENDIF
         CALL intert(nline,nline1,nline2,numm,Ajj,Tamach)
      ENDIF
   ENDDO
END SUBROUTINE amgt1d
