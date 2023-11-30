
SUBROUTINE amgb1d(Ajj,Tsonx,Tamach,Tredf)
   IMPLICIT NONE
   REAL Amach , Amachr , Blspc , Bspace , Chord , Den , Dum(2) , Flowa , Radius , Redf , Refc , Refcrd , Refden , Refflo , Refmac , &
      & Refstg , Refvel , Rfreq , Sigma , Sln , Stager , Tsonic , Vel
   INTEGER Iref , Mach , Maxmac , Mcb(7) , Minmac , Nlines , Nrow , Nstns , Nstnsx
   COMMON /amgmn / Mcb , Nrow , Dum , Refc , Sigma , Rfreq
   COMMON /bamg1l/ Iref , Minmac , Maxmac , Nlines , Nstns , Refstg , Refcrd , Refmac , Refden , Refvel , Refflo , Sln , Nstnsx ,   &
                 & Stager , Chord , Radius , Bspace , Mach , Den , Vel , Flowa , Amach , Redf , Blspc , Amachr , Tsonic
   COMPLEX Ajj(Nstns,1)
   REAL Tamach(1) , Tredf(1)
   INTEGER Tsonx(1)
   INTEGER i , nline , nline1 , nline2 , nnline , ns , numm
!
!     THIS ROUTINE INTERPOLATES TRANSONIC AJJ MATRICES
!
!
!
!
!
   numm = 2*Nstns*Nstns
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
END SUBROUTINE amgb1d