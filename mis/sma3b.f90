
SUBROUTINE sma3b(Iflag,Izk)
   IMPLICIT NONE
   DOUBLE PRECISION D11(2)
   REAL Dum1(2) , Dum2(22) , Dum4(35) , Z(1)
   INTEGER Gei , Id(7) , Id1 , Idx , M , N , Se1 , Sysbuf , Ze1 , Zinvs
   COMMON /genely/ Gei , Dum1 , Ze1 , Se1 , Id1 , Zinvs , Dum2 , Id , Dum4 , M , N
   COMMON /system/ Sysbuf
   COMMON /zblpkx/ D11 , Idx
   COMMON /zzzzzz/ Z
   INTEGER Iflag , Izk
   INTEGER i , j , nz , se(7) , ze(7)
   INTEGER korsz
!
!     THIS ROUTINE PROCESSES A GENERAL ELEMENT FROM GEI
!
!     IT  PRODUCES A ZE MATRIX OR A ZINVS MATRIX AND A SE MATRIX
!
!     ASSUMES GEI SITS AT BEGINNING OF UI SET AND IS OPEN TO READ
!
!
!
!     COMPUTE LENGTH OF VARIABLE CORE
!
   nz = korsz(Z) - Sysbuf
   Iflag = -1
   nz = nz - Sysbuf
!
! SKIP M+N WORDS ON GEI FILE
!
   CALL fread(Gei,Z,M+N,0)
!
! READ FLAG VARIABLE FOR Z OR K MATRIX
!
   CALL fread(Gei,Izk,1,0)
!
! IF Z MATRIX INPUT,WRITE ON ZE1 FILE
! IF K MATRIX INPUT,WRITE ON ZINVS FILE
!
   CALL makmcb(ze,Ze1,M,6,2)
   IF ( Izk==2 ) ze(1) = Zinvs
   CALL makmcb(se,Se1,N,2,2)
!
! READY FOR PACKING MATRICES
!
!
! OPEN ZE MATRIX
!
   CALL gopen(ze,Z(nz+1),1)
!
!     LOOP ON M COLUMNS OF ZE
!
   DO i = 1 , M
      CALL bldpk(2,2,ze(1),0,0)
      DO j = 1 , M
         CALL fread(Gei,Z,1,0)
         D11(1) = Z(1)
         Idx = j
         CALL zblpki
      ENDDO
      CALL bldpkn(ze(1),0,ze)
   ENDDO
   CALL close(ze(1),1)
   CALL wrttrl(ze)
   IF ( N/=0 ) THEN
      Iflag = 1
!
!     NOW BUILD SE TRANSPOSE
!
!
!     OPEN AND WRITE HEADER
!
      CALL gopen(se,Z(nz+1),1)
!
!     LOOP ON N COLUMNS OF SE
!     LOOP ON M COLUMNS OF SE  TRANSPOSE
!
      DO i = 1 , M
         CALL bldpk(2,2,se(1),0,0)
         DO j = 1 , N
            CALL fread(Gei,Z,1,0)
            D11(1) = -Z(1)
            Idx = j
            CALL zblpki
         ENDDO
         CALL bldpkn(se(1),0,se)
      ENDDO
      CALL close(se(1),1)
      CALL wrttrl(se)
   ENDIF
!
!     BACKSPACE GEI SO UD AND UI AVAILABLE LATER
!
   CALL bckrec(Gei)
   CALL close(Gei,2)
   Id(1) = Id1
   Id(2) = M
   Id(3) = M
   Id(4) = 8
   Id(5) = 2
   Id(6) = 1
   Id(7) = 0
END SUBROUTINE sma3b