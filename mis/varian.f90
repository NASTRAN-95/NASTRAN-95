
SUBROUTINE varian
   IMPLICIT NONE
   REAL Delta , Rz(1) , Skip(91) , Z(200)
   INTEGER Iop(2) , Iz(260) , Jrun , Sysbuf
   COMMON /blank / Iop , Delta
   COMMON /system/ Sysbuf , Skip , Jrun
   COMMON /zzzzzz/ Rz
   INTEGER der , derl(3) , file , i , iblnk , ibuf1 , ibuf2 , ibuf3 , id1 , id2 , ieor , iflag , ifound , in1 , inpt , io1 , ip1 ,  &
         & iread , iret , irtn , irtn1 , irtn2 , irtn3 , it1 , it2 , it3 , it4 , it5 , itf(5) , ito(5) , itype , j , k , m , mcb(7) &
         & , name(2) , njrun , nrec , nw , nz , ot1 , ot2 , ot3 , ot4 , ot5 , scr1 , scr2 , var , varan(2) , varl(3)
   INTEGER korsz , numtyp
   LOGICAL tapbit
!
!     VARIANCE ANALYSIS POST PROCESSOR MODULE
!
!     INPUTS--O1,O2,O3,O4,O5 OR EDT
!
!     OUTPUTS--O1O,O2O,O3O,O4O,O5O
!
!     PARAMETERS--OP--BCD'DER' OR 'VAR'
!                 DELTA--REAL--DEFAULT=1.0
!
   !>>>>EQUIVALENCE (it1,itf(1)) , (itf(2),it2) , (itf(3),it3) , (itf(4),it4) , (itf(5),it5) , (ito(1),ot1) , (ito(2),ot2) , (ito(3),ot3)&
!>>>>    & , (ito(4),ot4) , (ito(5),ot5) , (Z(1),Iz(1),Rz(1))
   DATA it1 , it2 , it3 , it4 , it5 , ot1 , ot2 , ot3 , ot4 , scr1 , ot5 , inpt , scr2/101 , 102 , 103 , 104 , 105 , 201 , 202 ,    &
      & 203 , 204 , 301 , 205 , 4HINPT , 302/
   DATA der , var , derl , name/4HDER  , 4HVAR  , 4HDERI , 4HVATI , 4HVE   , 4HVARI , 4HAN  /
   DATA varl , iblnk , varan , mcb/4HVARI , 4HANCE , 4H     , 4H     , 4202 , 42 , 7*0/
!
   ibuf1 = korsz(Z(1)) - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   nz = ibuf3 - 1
   IF ( nz<=0 ) CALL mesage(-8,0,name)
   IF ( .NOT.tapbit(inpt) ) CALL mesage(-7,0,name)
   CALL int2a8(*100,Jrun,Iz)
 100  njrun = Iz(1)
   nw = 1
   IF ( Iop(1)==var ) THEN
!
!     VARIANCE SECTION
!
      IF ( Jrun==0 ) RETURN
!
!     SEE IF VARIANCE IS TO BE COMPUTED
!
      CALL preloc(*600,Iz(ibuf1),it1)
      CALL locate(*900,Iz(ibuf1),varan,iflag)
!
!     READ IN VARIANCES
!
      CALL read(*1400,*700,it1,Iz(1),nz,0,iflag)
      CALL mesage(-8,0,name)
      GOTO 700
   ELSE
      IF ( Iop(1)/=der ) RETURN
!
!     DERIVATIVES SECTION
!
      IF ( Jrun/=0 ) THEN
!
!     COMPUTE DERIVATIVES  DJ = (OJ - O0)/DELTA
!
         CALL open(*1300,inpt,Iz(ibuf1),0)
!
         DO i = 1 , 5
            ifound = 0
            CALL fwdrec(*220,inpt)
            CALL open(*210,itf(i),Iz(ibuf2),0)
            CALL fwdrec(*1400,itf(i))
            CALL gopen(ito(i),Iz(ibuf3),1)
            file = itf(i)
 110        ASSIGN 130 TO irtn
 120        CALL read(*220,*1500,inpt,Iz(1),146,1,iflag)
            GOTO irtn
 130        CALL read(*210,*1500,file,Iz(147),146,1,iflag)
!
!     CHECK FOR MATCH ON SUBCASE
!
 140        IF ( Iz(4)<Iz(150) ) THEN
            ELSEIF ( Iz(4)==Iz(150) ) THEN
!
!     CHECK FOR MATCH ON TIME, FREQ ETC
!
               IF ( Z(5)<Z(151) ) THEN
               ELSEIF ( Z(5)==Z(151) ) THEN
!
!     CHECK FOR MATCH ON ELTYPE
!
                  IF ( Iz(3)<Iz(149) ) THEN
                  ELSEIF ( Iz(3)==Iz(149) ) THEN
!
!     WE GOT ONE
!
                     Iz(257) = derl(1)
                     Iz(258) = derl(2)
                     Iz(259) = derl(3)
                     Iz(260) = njrun
                     ifound = 1 + ifound
                     CALL write(ito(i),Iz(147),146,1)
                     nrec = Iz(10)
                     ASSIGN 160 TO irtn1
                     GOTO 150
                  ELSE
!
!     NEED NEW FILE RECORD
!
                     CALL fwdrec(*1400,file)
                     GOTO 130
                  ENDIF
               ELSE
                  CALL fwdrec(*1400,file)
                  GOTO 130
               ENDIF
            ELSE
               CALL fwdrec(*1400,file)
               GOTO 130
            ENDIF
!
!     NEED NEW INPT RECORD
!
            CALL fwdrec(*1400,inpt)
            ASSIGN 140 TO irtn
            GOTO 120
 150        CALL read(*1400,*180,inpt,Iz(1),nrec,0,iflag)
            id1 = Iz(1)/10
            GOTO irtn1
 160        CALL read(*1400,*190,file,Iz(nrec+1),nrec,0,iflag)
            id2 = Iz(nrec+1)/10
            ASSIGN 170 TO irtn1
 170        IF ( id1<id2 ) THEN
            ELSEIF ( id1==id2 ) THEN
!
!     POINT CHECKS
!
               DO j = 2 , nrec
                  itype = numtyp(Iz(j))
                  IF ( itype==2 .OR. itype==0 ) Z(nrec+j) = (Z(nrec+j)-Z(j))/Delta
               ENDDO
               CALL write(ito(i),Iz(nrec+1),nrec,0)
               ASSIGN 160 TO irtn1
            ELSE
               GOTO 160
            ENDIF
            GOTO 150
!
!     END OF DATA RECORD
!
 180        CALL fwdrec(*1400,file)
            GOTO 200
 190        CALL fwdrec(*1400,inpt)
 200        CALL write(ito(i),0,0,1)
!
!     EOF ON INPT
!
            GOTO 110
!
!     EOF ON FILE
!
 210        CALL skpfil(inpt,1)
 220        CALL close(file,1)
            CALL close(ito(i),1)
            mcb(1) = ito(i)
            mcb(2) = ifound
            IF ( ifound/=0 ) CALL wrttrl(mcb)
         ENDDO
!
!     SKIP OVER OLD DERIVATIVES
!
         i = 5*Jrun - 5
         CALL skpfil(inpt,i)
         CALL close(inpt,2)
         CALL gopen(inpt,Iz(ibuf1),3)
         i = 1
         ASSIGN 500 TO iret
         GOTO 400
      ELSE
!
!     COPY INPUT FILES TO INPT TAPE
!
         CALL open(*1300,inpt,Iz(ibuf1),1)
         i = 1
         ASSIGN 300 TO iret
         file = itf(i)
!
!     INTERNAL ROUTINE TO COPY FILES
!
         CALL open(*1200,file,Iz(ibuf2),0)
         GOTO 1000
      ENDIF
   ENDIF
 300  i = i + 1
   IF ( i<=5 ) THEN
      file = itf(i)
      CALL open(*1200,file,Iz(ibuf2),0)
      GOTO 1000
   ELSE
      CALL close(inpt,2)
      RETURN
   ENDIF
 400  file = ito(i)
   mcb(1) = file
   CALL rdtrl(mcb)
   IF ( mcb(2)/=0 ) THEN
      CALL open(*1200,file,Iz(ibuf2),0)
      GOTO 1000
   ELSE
      CALL eof(inpt)
   ENDIF
 500  i = i + 1
   IF ( i<=5 ) GOTO 400
   CALL close(inpt,2)
 600  RETURN
 700  IF ( iflag-1==Jrun ) THEN
!
!     SET UP FOR VARIANCES
!
      CALL close(it1,1)
      CALL open(*1300,inpt,Iz(ibuf1),0)
      CALL skpfil(inpt,5)
      in1 = inpt
      io1 = scr1
      DO i = 1 , Jrun
         IF ( i/=Jrun ) CALL open(*1300,io1,Iz(ibuf2),1)
         IF ( i/=1 ) CALL open(*1300,in1,Iz(ibuf3),0)
         DO j = 1 , 5
            IF ( i==Jrun ) THEN
!
!     FIX UP FOR WRITING ON OUTPUT FILES
!
               CALL open(*860,ito(j),Iz(ibuf2),1)
               CALL fname(ito(j),mcb)
               CALL write(ito(j),mcb,2,1)
               ifound = 0
               io1 = ito(j)
            ENDIF
            CALL fwdrec(*840,inpt)
            ASSIGN 720 TO irtn2
 710        CALL read(*850,*1500,in1,Iz(Jrun+1),146,1,iflag)
            IF ( i/=1 ) THEN
!
!     CHECK FOR MATCH
!
               GOTO irtn2
            ELSE
               Iz(Jrun+111) = varl(1)
               Iz(Jrun+112) = varl(2)
               Iz(Jrun+113) = varl(3)
               Iz(Jrun+114) = iblnk
               GOTO 740
            ENDIF
 720        CALL read(*830,*1500,inpt,Iz(Jrun+147),146,1,iflag)
 730        IF ( Iz(Jrun+4)<Iz(Jrun+150) ) THEN
            ELSEIF ( Iz(Jrun+4)==Iz(Jrun+150) ) THEN
               IF ( Z(Jrun+5)<Z(Jrun+151) ) THEN
               ELSEIF ( Z(Jrun+5)==Z(Jrun+151) ) THEN
                  IF ( Iz(Jrun+3)<Iz(Jrun+149) ) THEN
                  ELSEIF ( Iz(Jrun+3)==Iz(Jrun+149) ) THEN
                     GOTO 740
                  ELSE
                     CALL fwdrec(*1400,inpt)
                     GOTO 720
                  ENDIF
               ELSE
                  CALL fwdrec(*1400,inpt)
                  GOTO 720
               ENDIF
            ELSE
               CALL fwdrec(*1400,inpt)
               GOTO 720
            ENDIF
            CALL fwdrec(*1400,in1)
            ASSIGN 730 TO irtn2
            GOTO 710
!
!     MATCH
!
 740        CALL write(io1,Iz(Jrun+1),146,1)
            nrec = Iz(Jrun+10)
            m = Jrun + nrec
 750        ASSIGN 770 TO irtn3
 760        CALL read(*1400,*800,in1,Iz(Jrun+1),nrec,0,iflag)
            IF ( i==1 ) GOTO 790
            id1 = Iz(Jrun+1)/10
            GOTO irtn3
 770        CALL read(*1400,*810,inpt,Iz(m+1),nrec,0,iflag)
            id2 = Iz(m+1)/10
            ASSIGN 780 TO irtn3
 780        IF ( id1<id2 ) GOTO 760
            IF ( id1/=id2 ) GOTO 770
!
!     POINT MATCH
!
 790        IF ( i==Jrun ) ifound = ifound + 1
            DO k = 2 , nrec
               itype = numtyp(Iz(Jrun+k))
               IF ( itype==2 .OR. itype==0 ) THEN
                  IF ( i/=1 ) THEN
                     Z(Jrun+k) = Z(Jrun+k) + (Z(m+k)*Z(i))**2
                  ELSE
                     Z(Jrun+k) = (Z(Jrun+k)*Z(1))**2
                  ENDIF
                  IF ( i==Jrun ) Z(Jrun+k) = sqrt(Z(Jrun+k))
               ENDIF
            ENDDO
            CALL write(io1,Iz(Jrun+1),nrec,0)
            GOTO 750
!
!     END OF DATA ON IN1
!
 800        IF ( i/=1 ) CALL fwdrec(*1400,inpt)
            GOTO 820
 810        CALL fwdrec(*1400,in1)
 820        CALL write(io1,0,0,1)
            ASSIGN 720 TO irtn2
            GOTO 710
!
!     EOF ON INPT
!
 830        CALL skpfil(in1,1)
 840        CALL eof(io1)
            IF ( i==Jrun ) THEN
               CALL close(io1,1)
               mcb(1) = io1
               mcb(2) = ifound
               CALL wrttrl(mcb)
            ENDIF
            CYCLE
 850        IF ( i/=1 ) CALL skpfil(inpt,1)
            GOTO 840
 860     ENDDO
!
!     SWITCH FILES
!
         IF ( i/=Jrun ) CALL close(io1,1)
         IF ( i/=1 ) CALL close(in1,1)
         j = in1
         in1 = io1
         io1 = j
         IF ( i==1 ) io1 = scr2
      ENDDO
      CALL close(inpt,1)
      Jrun = 9999999
      RETURN
   ENDIF
 900  CALL close(it1,1)
   RETURN
 1000 ieor = 1
   CALL read(*1200,*1100,file,Iz(1),nz,0,iread)
   ieor = 0
 1100 CALL write(inpt,Iz(1),iread,ieor)
   GOTO 1000
 1200 CALL eof(inpt)
   CALL close(file,1)
   GOTO iret
!
!     ERROR MESSAGES
!
 1300 ip1 = -1
   GOTO 1600
 1400 ip1 = -2
   GOTO 1600
 1500 ip1 = -3
 1600 CALL mesage(ip1,file,name)
   STOP
END SUBROUTINE varian