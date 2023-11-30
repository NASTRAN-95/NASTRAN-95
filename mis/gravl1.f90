
SUBROUTINE gravl1(Nvect,Gvect,Sr1,Iharm)
   IMPLICIT NONE
   REAL B(4) , Core(1)
   INTEGER Bgpdt , Cstm , Ii , Istl , Mass , N(2) , Nn(8) , Nrowsp , Old , Sil , Sysbuf
   COMMON /blank / Nrowsp
   COMMON /loadx / N , Bgpdt , Old , Cstm , Sil , Istl , Nn , Mass
   COMMON /system/ Sysbuf
   COMMON /zblpkx/ B , Ii
   COMMON /zzzzzz/ Core
   INTEGER Iharm , Nvect , Sr1
   REAL Gvect(1)
   REAL flag , vect(3)
   INTEGER gravt(7) , i , icm , igpco(4) , il , iloop , in , iout , ipm , ipont , isil , isil1 , isil2 , lcore , name(2) , nz
   INTEGER korsz
!
!
!
!
   DATA name/4HGRAV , 4HL1  /
!
! ----------------------------------------------------------------------
!
   IF ( Iharm==0 ) THEN
      lcore = korsz(Core)
      icm = 1
      nz = lcore
      lcore = lcore - Sysbuf
      CALL gopen(Sr1,Core(lcore+1),1)
      lcore = lcore - Sysbuf
      CALL gopen(Bgpdt,Core(lcore+1),0)
      Old = 0
      lcore = lcore - Sysbuf
      CALL open(*100,Cstm,Core(lcore+1),0)
      icm = 0
      CALL skprec(Cstm,1)
      lcore = lcore - Sysbuf
   ELSE
      CALL gravl3(Nvect,Gvect,Sr1,Iharm)
      RETURN
   ENDIF
 100  CALL gopen(Sil,Core(lcore+1),0)
   isil = 0
   CALL makmcb(gravt,Sr1,Nrowsp,2,1)
   DO iloop = 1 , Nvect
      DO
         CALL read(*400,*300,Sil,isil1,1,0,flag)
         IF ( isil1>=0 ) THEN
            il = (iloop-1)*3
            ASSIGN 200 TO iout
            ipont = 1
            CALL bldpk(1,1,gravt(1),0,0)
            EXIT
         ENDIF
      ENDDO
 150  DO
         CALL read(*400,*300,Sil,isil2,1,0,flag)
         IF ( isil2>=0 ) THEN
            IF ( isil2-isil1==1 ) EXIT
            GOTO 250
         ENDIF
      ENDDO
 200  isil1 = isil2
      ipont = ipont + 1
      GOTO 150
 250  CALL fndpnt(igpco(1),ipont)
      DO i = 1 , 3
         in = i + il
         vect(i) = Gvect(in)
      ENDDO
      IF ( igpco(1)/=0 ) CALL basglb(vect(1),vect(1),igpco(2),igpco(1))
      DO i = 1 , 3
         B(1) = vect(i)
         Ii = isil1 - 1 + i
         CALL zblpki
      ENDDO
      GOTO iout
!
!     END SIL
!
 300  ASSIGN 350 TO iout
      IF ( Nrowsp/=isil1 ) GOTO 250
 350  CALL rewind(Bgpdt)
      CALL rewind(Sil)
      CALL bldpkn(gravt(1),0,gravt)
      CALL skprec(Sil,1)
      isil = 0
      CALL skprec(Bgpdt,1)
      Old = 0
   ENDDO
   CALL close(Bgpdt,1)
   IF ( icm==0 ) CALL close(Cstm,1)
   CALL close(Sil,1)
   CALL close(gravt(1),1)
   CALL wrttrl(gravt)
   RETURN
!
 400  CALL mesage(-3,ipm,name)
!
END SUBROUTINE gravl1