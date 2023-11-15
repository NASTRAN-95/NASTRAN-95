
SUBROUTINE sma3c(Iflag,K)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION D11(2)
   REAL Dum(4) , Dum1(62) , Dummy(53) , Z(1)
   INTEGER Gei , Id , Iprec , Iz(1) , Luset , M , N , Stz(1) , Stzs(1) , Sysbuf , Zinvs(1) , Zs(1)
   COMMON /blank / Luset
   COMMON /genely/ Gei , Dum , Stzs , Zinvs , Zs , Stz , Dum1 , M , N
   COMMON /system/ Sysbuf , Dummy , Iprec
   COMMON /zblpkx/ D11 , Id
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Iflag
   INTEGER K(7)
!
! Local variable declarations
!
   DOUBLE PRECISION a11 , b11
   REAL block1(20) , block2(20)
   INTEGER i , ia , iad , iaeol , ib , ibd , ibeol , idp , ihop , ii , iip , iud , iui , jj , l , llen , nam1 , nam2 , name(2) , nz
   INTEGER korsz
!
! End of declarations
!
!
!     THIS ROUTINE WILL MERGE ZINVS,ZS,STZ,AND STZS INTO KE AND
!       BUILD KE UP TO G SIZE.  IF INFLAG .LT. 0 THERE ARE NO
!       UD-S
!
!
!
   EQUIVALENCE (Z(1),Iz(1))
   DATA name/4HSMA3 , 4HC   /
!
!     IUI IS POINTER TO UI SET, IUD IS POINTER TO UD SET
!
   iui = 1
   iud = M + 1
   nz = korsz(Z)
!
!     OPEN GEI(WITHOUT REWIND)
!
   nz = nz - Sysbuf
   CALL gopen(Gei,Z(nz+1),2)
!
!     READ IN UI SET
!
   CALL fread(Gei,Z,-3,0)
   CALL fread(Gei,Z,M,0)
!
!     READ IN UD
!
   IF ( Iflag>=0 ) CALL fread(Gei,Z(iud),N,1)
!
!     OPEN BUFFERS FOR MATRICES
!
   llen = M + N + 2*Sysbuf
   IF ( Iflag>=0 ) llen = llen + 3*Sysbuf
   IF ( llen>nz ) THEN
!
!     ERROR MESAGES
!
      CALL mesage(-8,Gei,name)
   ELSE
      nz = nz - Sysbuf
      CALL gopen(K,Z(nz+1),1)
      nz = nz - Sysbuf
      CALL gopen(Zinvs,Z(nz+1),0)
      IF ( Iflag>=0 ) THEN
         nz = nz - Sysbuf
         CALL gopen(Zs,Z(nz+1),0)
         nz = nz - Sysbuf
         CALL gopen(Stz,Z(nz+1),0)
         nz = nz - Sysbuf
         CALL gopen(Stzs,Z(nz+1),0)
      ENDIF
!
!     LOOP ON LUSET MAKING COLUMNS OF KGG
!
      K(2) = 0
      K(3) = Luset
      K(4) = 6
      K(5) = 2
      K(6) = 0
      K(7) = 0
      iip = 0
      idp = 0
      DO i = 1 , Luset
         CALL bldpk(2,Iprec,K(1),0,0)
         IF ( iip<M ) THEN
            l = iui + iip
            IF ( i==Iz(l) ) THEN
!
!     USING UI -- ZINVS AND STZ
!
               iip = iip + 1
               nam1 = Zinvs(1)
               nam2 = Stz(1)
               GOTO 20
            ENDIF
         ENDIF
         IF ( Iflag<0 ) GOTO 200
         IF ( idp>=N ) GOTO 200
         l = iud + idp
         IF ( i/=Iz(l) ) GOTO 200
!
!     USING UD ZS AND STZS
!
         idp = idp + 1
         nam1 = Zs(1)
         nam2 = Stzs(1)
!
!     MERGE ROUTINE FOR COLUMN
!
 20      iad = 0
         ibd = 0
         ihop = 0
         CALL intpk(*160,nam1,block1(1),2,1)
 40      IF ( Iflag<0 ) GOTO 180
         CALL intpk(*180,nam2,block2(1),2,1)
 60      CALL intpki(a11,ia,nam1,block1(1),iaeol)
         l = iui + ia - 1
         ii = Iz(l)
         IF ( ihop==1 ) GOTO 100
         ihop = 1
 80      CALL intpki(b11,ib,nam2,block2(1),ibeol)
         l = iud + ib - 1
         jj = Iz(l)
 100     IF ( ii<jj ) THEN
         ELSEIF ( ii==jj ) THEN
            GOTO 300
         ELSE
            GOTO 140
         ENDIF
!
!     PUT IN A11
!
 120     D11(1) = a11
         Id = ii
         CALL zblpki
         IF ( iaeol==0 ) GOTO 60
         iad = 1
         ii = 99999
         IF ( ibd/=0 ) GOTO 200
!
!     PUT IN BUU
!
 140     D11(1) = b11
         Id = jj
         CALL zblpki
         IF ( ibeol==0 ) GOTO 80
         ibd = 1
         jj = 99999
         IF ( iad==0 ) GOTO 120
         GOTO 200
!
!     NULL NAM1
!
 160     iad = 1
         ii = 99999
         GOTO 40
!
!     NO NAM2
!
 180     ibd = 1
         jj = 99999
         ihop = 1
         GOTO 60
!
!     END OF COLUMN
!
 200     CALL bldpkn(K(1),0,K)
!
!     END LOOP
!
      ENDDO
      CALL wrttrl(K)
      CALL close(K(1),1)
      CALL close(Zinvs(1),1)
      IF ( Iflag>=0 ) THEN
         CALL close(Stz(1),1)
         CALL close(Stzs(1),1)
         CALL close(Zs(1),1)
      ENDIF
      RETURN
   ENDIF
 300  CALL mesage(-7,0,name)
END SUBROUTINE sma3c
