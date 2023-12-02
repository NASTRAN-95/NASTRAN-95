!*==sma3c.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sma3c(Iflag,K)
USE C_BLANK
USE C_GENELY
USE C_SYSTEM
USE C_ZBLPKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iflag
   INTEGER , DIMENSION(7) :: K
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: a11 , b11
   REAL , DIMENSION(20) :: block1 , block2
   INTEGER :: i , ia , iad , iaeol , ib , ibd , ibeol , idp , ihop , ii , iip , iud , iui , jj , l , llen , nam1 , nam2 , nz
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL bldpk , bldpkn , close , fread , gopen , intpk , intpki , korsz , mesage , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS ROUTINE WILL MERGE ZINVS,ZS,STZ,AND STZS INTO KE AND
!       BUILD KE UP TO G SIZE.  IF INFLAG .LT. 0 THERE ARE NO
!       UD-S
!
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA name/4HSMA3 , 4HC   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     CALL bldpk(2,Iprec,K(1),0,0)
                     IF ( iip<M ) THEN
                        l = iui + iip
                        IF ( i==iz(l) ) THEN
!
!     USING UI -- ZINVS AND STZ
!
                           iip = iip + 1
                           nam1 = Zinvs(1)
                           nam2 = Stz(1)
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDIF
                     IF ( Iflag<0 ) THEN
                        spag_nextblock_2 = 9
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     IF ( idp>=N ) THEN
                        spag_nextblock_2 = 9
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     l = iud + idp
                     IF ( i/=iz(l) ) THEN
                        spag_nextblock_2 = 9
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
!
!     USING UD ZS AND STZS
!
                     idp = idp + 1
                     nam1 = Zs(1)
                     nam2 = Stzs(1)
                     spag_nextblock_2 = 2
                  CASE (2)
!
!     MERGE ROUTINE FOR COLUMN
!
                     iad = 0
                     ibd = 0
                     ihop = 0
                     CALL intpk(*2,nam1,block1(1),2,1)
                     spag_nextblock_2 = 3
                  CASE (3)
                     IF ( Iflag<0 ) GOTO 4
                     CALL intpk(*4,nam2,block2(1),2,1)
                     spag_nextblock_2 = 4
                  CASE (4)
                     CALL intpki(a11,ia,nam1,block1(1),iaeol)
                     l = iui + ia - 1
                     ii = iz(l)
                     IF ( ihop==1 ) THEN
                        spag_nextblock_2 = 6
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     ihop = 1
                     spag_nextblock_2 = 5
                  CASE (5)
                     CALL intpki(b11,ib,nam2,block2(1),ibeol)
                     l = iud + ib - 1
                     jj = iz(l)
                     spag_nextblock_2 = 6
                  CASE (6)
                     IF ( ii<jj ) THEN
                     ELSEIF ( ii==jj ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        spag_nextblock_2 = 8
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     spag_nextblock_2 = 7
                  CASE (7)
!
!     PUT IN A11
!
                     D11(1) = a11
                     Id = ii
                     CALL zblpki
                     IF ( iaeol==0 ) THEN
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     iad = 1
                     ii = 99999
                     IF ( ibd/=0 ) THEN
                        spag_nextblock_2 = 9
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     spag_nextblock_2 = 8
                  CASE (8)
!
!     PUT IN BUU
!
                     D11(1) = b11
                     Id = jj
                     CALL zblpki
                     IF ( ibeol==0 ) THEN
                        spag_nextblock_2 = 5
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     ibd = 1
                     jj = 99999
                     IF ( iad/=0 ) THEN
                        spag_nextblock_2 = 9
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     spag_nextblock_2 = 7
                     CYCLE SPAG_DispatchLoop_2
!
!     NULL NAM1
!
 2                   iad = 1
                     ii = 99999
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
!
!     NO NAM2
!
 4                   ibd = 1
                     jj = 99999
                     ihop = 1
                     spag_nextblock_2 = 4
                     CYCLE SPAG_DispatchLoop_2
                  CASE (9)
!
!     END OF COLUMN
!
                     CALL bldpkn(K(1),0,K)
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
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
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(-7,0,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sma3c
