
SUBROUTINE trd1a(Casexx,Trl,Ic,Nlftp,Ngroup,Moda1)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ii , Incr , It1 , It2 , Iz(160) , Jj , Sysbuf
   REAL Z(1)
   COMMON /packx / It1 , It2 , Ii , Jj , Incr
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Casexx , Ic , Moda1 , Ngroup , Nlftp , Trl
!
! Local variable declarations
!
   INTEGER file , i , i2 , ibuf1 , ibuf2 , icp , icrq , idisp , iflag , igroup , intrl(2) , ip1 , itrl , itstep , ivel , k , l ,    &
         & lud , mcb(7) , name(2) , nx , nz
   INTEGER korsz
!
! End of declarations
!
!
!     THIS ROUTINE BUILDS THE INITIAL CONDITIONS TABLE, PUTS TSTEP STUFF
!     IN CORE AND EXTRACTS THE NLFTP POINTER
!
!     THIS ROUTINE IS SUITABLE FOR SINGLE PRECISION OPERATION
!
!
!
   EQUIVALENCE (Z(1),Iz(1))
!
   DATA name , intrl/4HTRD1 , 4HA    , 4HTRL  , 4HTRD /
!
!     IDENTIFICATION VARIABLES
!
!     NGROUP        NUMBER OF CHANGES OF TIME STEP
!
!     ITSTEP        SELECTED TSTEP ID
!
!     NLFTP         SELECTED NON-LINEAR LOAD ID
!
!     ICP           SELECTED INITIAL CONDITION ID
!
!     LUD           LENGTH OF INITIAL CONDITION--D SET
!
!     IGROUP        POINTER TO TSTEP STUFF
!
!
!
!     INITIALIZE
!
   It1 = 1
   It2 = 1
   Ii = 1
   Incr = 1
   nz = korsz(Z)
   nx = nz
!
!     PICK UP AND STORE CASECC POINTERS
!
   ibuf1 = nz - Sysbuf + 1
   CALL gopen(Casexx,Iz(ibuf1),0)
   CALL fread(Casexx,Iz,166,1)
   CALL close(Casexx,1)
   itstep = Iz(38)
   icp = Iz(9)
   Nlftp = Iz(160)
   IF ( icp/=0 .AND. Moda1==1 ) THEN
      ip1 = -51
      file = icp
      GOTO 700
   ELSE
!
!     BUILD INITIAL CONDITION FILE
!
      CALL gopen(Ic,Iz(ibuf1),1)
      ibuf2 = ibuf1 - Sysbuf
      nz = nz - 2*Sysbuf
      icrq = -nz
      IF ( icrq<=0 ) THEN
         file = Trl
         CALL open(*600,Trl,Iz(ibuf2),0)
         CALL read(*800,*100,Trl,Iz(1),nz,0,iflag)
         icrq = nz
      ENDIF
      GOTO 1000
   ENDIF
 100  lud = Iz(iflag)
   Jj = lud
   icrq = 2*lud - nz
   IF ( icrq>0 ) GOTO 1000
   l = Iz(3)
   itrl = l
!
!     ZERO I. C.
!
   ivel = ibuf2 - lud - 1
   idisp = ivel - lud
   DO i = 1 , lud
      k = ivel + i
      Z(k) = 0.0
      k = idisp + i
      Z(k) = 0.0
   ENDDO
   CALL makmcb(mcb,Ic,lud,2,1)
   IF ( icp==0 ) GOTO 300
   IF ( Iz(3)/=0 ) THEN
      iflag = iflag - 1
      DO i = 4 , iflag
         IF ( Iz(i)==icp ) GOTO 200
      ENDDO
   ENDIF
   itstep = icp
   GOTO 900
 200  k = i - 4
   l = iflag - i
   CALL skprec(Trl,k)
   DO
      CALL read(*800,*300,Trl,Iz(1),3,0,iflag)
      k = Iz(1) + idisp
      i2 = 2
      Z(k) = Z(k) + Z(i2)
      k = Iz(1) + ivel
      Z(k) = Z(k) + Z(i2+1)
   ENDDO
 300  CALL pack(Z(idisp+1),Ic,mcb)
   CALL pack(Z(ivel+1),Ic,mcb)
   CALL close(Ic,1)
   CALL wrttrl(mcb)
   CALL skprec(Trl,l)
!
!     BRING TSTEP STUFF INTO CORE
!
 400  itrl = itrl + 1
   CALL read(*900,*500,Trl,Iz(1),nz,0,iflag)
   icrq = nz
   GOTO 1000
 500  IF ( Iz(1)/=itstep ) GOTO 400
!
!     TSTEP CARD FOUND
!
   CALL close(Trl,1)
   Ngroup = (iflag-1)/3
!
!     MOVE TSTEP STUFF TO BOTTOM OF CORE
!
   nz = nx - iflag + 1
   igroup = nz + 1
   DO i = 2 , iflag
      k = igroup + i - 2
      Iz(k) = Iz(i)
   ENDDO
   RETURN
!
!     ERROR MESSAGES
!
 600  ip1 = -1
 700  CALL mesage(ip1,file,name)
   RETURN
 800  ip1 = -2
   GOTO 700
 900  CALL mesage(-31,itstep,intrl)
   RETURN
 1000 ip1 = -8
   file = icrq
   GOTO 700
END SUBROUTINE trd1a
