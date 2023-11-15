
SUBROUTINE frrd1f(Mhh,Bhh,Khh,Frl,Frqset,Nload,Nfreq,Ph,Uhv)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(4) , B(4) , Core(1)
   INTEGER Ieol , Ieor , Ii , Jj , Sysbuf
   COMMON /system/ Sysbuf
   COMMON /zblpkx/ B , Jj
   COMMON /zntpkx/ A , Ii , Ieol , Ieor
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   INTEGER Bhh , Frl , Frqset , Khh , Mhh , Nfreq , Nload , Ph , Uhv
!
! Local variable declarations
!
   REAL cdem , factr , ratio , rdem , w , w2
   INTEGER file , i , ib , ibhh , ibuf1 , ibuf2 , ik , ikhh , im , imhh , ip1 , ipnt , iret , j , lhset , matnam , mcb(7) , name(2)
   INTEGER korsz
!
! End of declarations
!
!
!     ROUTINE  SOLVES DIRECTLY FOR UNCOUPLED MODAL FORMULATION
!
   DATA name/4HFRRD , 4H1F  /
!
!
   ibuf1 = korsz(Core) - Sysbuf + 1
!
!     PICK UP FREQUENCY LIST
!
   CALL gopen(Frl,Core(ibuf1),0)
   CALL skprec(Frl,Frqset-1)
   IF ( ibuf1-1<Nfreq ) GOTO 700
   CALL fread(Frl,Core,Nfreq,1)
   CALL close(Frl,1)
!
!     BRING IN  MODAL MATRICES
!
   imhh = Nfreq
   mcb(1) = Mhh
   CALL rdtrl(mcb)
   lhset = mcb(2)
   IF ( ibuf1-1<Nfreq+3*lhset ) GOTO 700
   ibhh = imhh + lhset
   ikhh = ibhh + lhset
!
!     BRING IN MHH
!
   matnam = Mhh
   ASSIGN 100 TO iret
   ipnt = imhh
   GOTO 400
!
!     BRING  IN  BHH
!
 100  matnam = Bhh
   ASSIGN 200 TO iret
   ipnt = ibhh
   GOTO 400
!
!     BRING IN KHH
!
 200  matnam = Khh
   ASSIGN 300 TO iret
   ipnt = ikhh
   GOTO 400
!
!     READY LOADS
!
 300  CALL gopen(Ph,Core(ibuf1),0)
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
         CALL intpk(*320,Ph,0,3,0)
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
!IBMD DEM  = RDEM*RDEM + CDEM*CDEM
!IBMR IF (DEM .NE. 0.0) GO TO 71
            IF ( rdem==0.0 .AND. cdem==0.0 ) THEN
               CALL mesage(5,j,name)
               B(1) = 0.0
               B(2) = 0.0
!
!     COMPUTE REAL AND COMPLEX PHI-S
!
!IBMD B(1) = (A(1)*RDEM + A(2)*CDEM)/DEM
!IBMD B(2) = (A(2)*RDEM - A(1)*CDEM)/DEM
!IBMNB
            ELSEIF ( rdem==0.0 ) THEN
               ratio = rdem/cdem
               factr = 1.0/(ratio*rdem+cdem)
               B(1) = (A(1)*ratio+A(2))*factr
               B(2) = (A(2)*ratio-A(1))*factr
            ELSE
               ratio = cdem/rdem
               factr = 1.0/(rdem+ratio*cdem)
               B(1) = (A(1)+A(2)*ratio)*factr
               B(2) = (A(2)-A(1)*ratio)*factr
            ENDIF
!IBMNE
            Jj = Ii
            CALL zblpki
         ENDDO
!
!     END  COLUMN
!
 320     CALL bldpkn(Uhv,0,mcb)
      ENDDO
   ENDDO
   CALL close(Uhv,1)
   CALL close(Ph,1)
   CALL wrttrl(mcb)
   RETURN
!
!     INTERNAL SUBROUTINE TO BRING IN  H MATRICES
!
 400  file = matnam
   CALL open(*600,matnam,Core(ibuf1),0)
   CALL skprec(matnam,1)
   DO i = 1 , lhset
      ipnt = ipnt + 1
      CALL intpk(*450,matnam,0,1,0)
      CALL zntpki
      IF ( Ii/=i .OR. Ieol/=1 ) GOTO 800
      Core(ipnt) = A(1)
      CYCLE
!
!     NULL COLUMN
!
 450  Core(ipnt) = 0.0
   ENDDO
   CALL close(matnam,1)
 500  GOTO iret
!
!     ZERO CORE FOR PURGED MATRIX
!
 600  DO i = 1 , lhset
      ipnt = ipnt + 1
      Core(ipnt) = 0.0
   ENDDO
   GOTO 500
 700  DO
      ip1 = -8
!
!     ERROR MESAGES
!
      CALL mesage(ip1,file,name)
   ENDDO
 800  ip1 = -7
   CALL mesage(ip1,file,name)
   GOTO 700
END SUBROUTINE frrd1f
