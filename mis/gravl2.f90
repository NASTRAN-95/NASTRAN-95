
SUBROUTINE gravl2(Nvect,Fild,Pg)
   IMPLICIT NONE
   REAL A(4) , B(4) , Bgpdt , Core(1) , Cstm , Old
   INTEGER Ieol , Ii , Istl , Ll , Mass , N(2) , Nn(8) , Nrowsp , Sil , Sysbuf
   COMMON /blank / Nrowsp
   COMMON /loadx / N , Bgpdt , Old , Cstm , Sil , Istl , Nn , Mass
   COMMON /system/ Sysbuf
   COMMON /zblpkx/ B , Ii
   COMMON /zntpkx/ A , Ll , Ieol
   COMMON /zzzzzz/ Core
   INTEGER Fild , Nvect
   INTEGER Pg(7)
   REAL flag
   INTEGER ibuf , iloop , iout , ipm , isil , isil1 , isil2 , lcore , name(2) , nz
   INTEGER korsz
!
!
!
   DATA name/4HGRAV , 4HL2  /
!
! ----------------------------------------------------------------------
!
   lcore = korsz(Core)
   nz = lcore
   lcore = lcore - Sysbuf
   CALL open(*300,Pg(1),Core(lcore+1),0)
   CALL skpfil(Pg,1)
   CALL skpfil(Pg,-1)
   CALL close(Pg,2)
   CALL open(*300,Pg(1),Core(lcore+1),3)
   lcore = lcore - Sysbuf
   CALL gopen(Fild,Core(lcore+1),0)
   lcore = lcore - Sysbuf
   CALL gopen(Sil,Core(lcore+1),0)
   ibuf = lcore
   isil = 0
   DO iloop = 1 , Nvect
      DO
         CALL read(*400,*200,Sil,isil1,1,0,flag)
         IF ( isil1>=0 ) THEN
            ASSIGN 150 TO iout
            CALL bldpk(1,1,Pg(1),0,0)
            CALL intpk(*250,Fild,0,1,0)
            EXIT
         ENDIF
      ENDDO
 50   DO
         CALL read(*400,*200,Sil,isil2,1,0,flag)
         IF ( isil2>=0 ) THEN
            IF ( isil2-isil1/=1 ) isil1 = 999999
            EXIT
         ENDIF
      ENDDO
 100  GOTO iout
 150  IF ( Ieol/=0 ) GOTO 250
      CALL zntpki
      IF ( Ll<isil1 ) THEN
         B(1) = A(1)
         Ii = Ll
         CALL zblpki
      ELSEIF ( Ll/=isil1 ) THEN
         GOTO 50
      ENDIF
      GOTO 100
 200  ASSIGN 250 TO iout
      IF ( Nrowsp/=isil1 ) THEN
         isil1 = 999999
         GOTO 100
      ENDIF
 250  CALL rewind(Sil)
      CALL bldpkn(Pg(1),0,Pg)
      CALL skprec(Sil,1)
      isil = 0
   ENDDO
   CALL close(Sil,1)
   CALL close(Fild,1)
   CALL wrttrl(Pg)
   CALL close(Pg,1)
   RETURN
!
 300  ipm = Pg(1)
   CALL mesage(-1,ipm,name)
!
 400  CALL mesage(-3,Sil,name)
!
END SUBROUTINE gravl2
