!*==frd2f.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frd2f(Mhh,Bhh,Khh,Frl,Frqset,Nload,Nfreq,Ph,Uhv)
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_ZBLPKX
   USE C_ZNTPKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Mhh
   INTEGER :: Bhh
   INTEGER :: Khh
   INTEGER :: Frl
   INTEGER :: Frqset
   INTEGER :: Nload
   INTEGER :: Nfreq
   INTEGER :: Ph
   INTEGER :: Uhv
!
! Local variable declarations rewritten by SPAG
!
   REAL :: cdem , dem , rdem , w , w2
   INTEGER :: file , i , ib , ibhh , ibuf1 , ibuf2 , ik , ikhh , im , imhh , ip1 , ipnt , iret , j , lhset , matnam
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL bldpk , bldpkn , close , fread , gopen , intpk , korsz , makmcb , mesage , open , rdtrl , skprec , wrttrl , zblpki ,    &
          & zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     ROUTINE  SOLVES DIRECTLY FOR UNCOUPLED MODAL FORMULATION
!
!
!
   DATA name/4HFRD2 , 4HF   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! ----------------------------------------------------------------------
!
         ibuf1 = korsz(Core) - Sysbuf + 1
!
!     PICK UP FREQUENCY LIST
!
         CALL gopen(Frl,Core(ibuf1),0)
         CALL skprec(Frl,Frqset-1)
         IF ( ibuf1-1<Nfreq ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL fread(Frl,Core,Nfreq,1)
         CALL close(Frl,1)
!
!     BRING IN  MODAL MATRICES
!
         imhh = Nfreq
         mcb(1) = Mhh
         CALL rdtrl(mcb)
         lhset = mcb(2)
         IF ( ibuf1-1<Nfreq+3*lhset ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ibhh = imhh + lhset
         ikhh = ibhh + lhset
!
!     BRING IN MHH
!
         matnam = Mhh
         ASSIGN 20 TO iret
         ipnt = imhh
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     BRING  IN  BHH
!
 20      matnam = Bhh
         ASSIGN 40 TO iret
         ipnt = ibhh
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     BRING IN KHH
!
 40      matnam = Khh
         ASSIGN 60 TO iret
         ipnt = ikhh
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     READY LOADS
!
 60      CALL gopen(Ph,Core(ibuf1),0)
!
!     READY SOLUTIONS
!
         ibuf2 = ibuf1 - Sysbuf
         CALL gopen(Uhv,Core(ibuf2),1)
         CALL makmcb(mcb,Uhv,lhset,2,3)
!
!     COMPUTE  SOLUTIONS
!
         DO i = 1 , Nload
            DO j = 1 , Nfreq
!
!     PICK  UP  FREQ
!
               w = Core(j)
               w2 = -w*w
               CALL bldpk(3,3,Uhv,0,0)
               CALL intpk(*65,Ph,0,3,0)
               DO WHILE ( Ieol==0 )
                  CALL zntpki
!
!     COMPUTE  REAL AND COMPLEX PARTS OF DENOMINATOR
!
                  ik = ikhh + Ii
                  ib = ibhh + Ii
                  im = imhh + Ii
                  rdem = w2*Core(im) + Core(ik)
                  cdem = Core(ib)*w
                  dem = rdem*rdem + cdem*cdem
                  IF ( dem/=0.0 ) THEN
!
!     COMPUTE REAL AND COMPLEX PHI-S
!
                     B(1) = (A(1)*rdem+A(2)*cdem)/dem
                     B(2) = (A(2)*rdem-A(1)*cdem)/dem
                  ELSE
                     CALL mesage(5,j,name)
                     B(1) = 0.0
                     B(2) = 0.0
                  ENDIF
                  Jj = Ii
                  CALL zblpki
               ENDDO
!
!     END  COLUMN
!
 65            CALL bldpkn(Uhv,0,mcb)
            ENDDO
         ENDDO
         CALL close(Uhv,1)
         CALL close(Ph,1)
         CALL wrttrl(mcb)
         RETURN
      CASE (2)
!
!     INTERNAL SUBROUTINE TO BRING IN  H MATRICES
!
         file = matnam
         CALL open(*80,matnam,Core(ibuf1),0)
         CALL skprec(matnam,1)
         DO i = 1 , lhset
            ipnt = ipnt + 1
            CALL intpk(*70,matnam,0,1,0)
            CALL zntpki
            IF ( Ii/=i .OR. Ieol/=1 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Core(ipnt) = A(1)
            CYCLE
!
!     NULL COLUMN
!
 70         Core(ipnt) = 0.0
         ENDDO
         CALL close(matnam,1)
         spag_nextblock_1 = 3
      CASE (3)
         GOTO iret
!
!      ZERO CORE FOR PURGED MATRIX
!
 80      DO i = 1 , lhset
            ipnt = ipnt + 1
            Core(ipnt) = 0.0
         ENDDO
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         DO
            ip1 = -8
!
!     ERROR MESAGES
!
            CALL mesage(ip1,file,name)
         ENDDO
         spag_nextblock_1 = 5
      CASE (5)
         ip1 = -7
         CALL mesage(ip1,file,name)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE frd2f
