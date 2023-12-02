!*==varian.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE varian
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: der , iblnk , inpt , it1 , it2 , it3 , it4 , it5 , ot1 , ot2 , ot3 , ot4 , ot5 , scr1 , scr2 , var
   INTEGER , DIMENSION(3) , SAVE :: derl , varl
   INTEGER :: file , i , ibuf1 , ibuf2 , ibuf3 , id1 , id2 , ieor , iflag , ifound , in1 , io1 , ip1 , iread , iret , irtn , irtn1 ,&
            & irtn2 , irtn3 , itype , j , k , m , njrun , nrec , nw , nz
   INTEGER , DIMENSION(5) :: itf , ito
   INTEGER , DIMENSION(260) :: iz
   INTEGER , DIMENSION(7) , SAVE :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name , varan
   REAL , DIMENSION(200) :: z
   EXTERNAL close , eof , fname , fwdrec , gopen , int2a8 , korsz , locate , mesage , numtyp , open , preloc , rdtrl , read ,       &
          & skpfil , tapbit , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ibuf1 = korsz(z(1)) - Sysbuf
         ibuf2 = ibuf1 - Sysbuf
         ibuf3 = ibuf2 - Sysbuf
         nz = ibuf3 - 1
         IF ( nz<=0 ) CALL mesage(-8,0,name)
         IF ( .NOT.tapbit(inpt) ) CALL mesage(-7,0,name)
         CALL int2a8(*20,Jrun,iz)
 20      njrun = iz(1)
         nw = 1
         IF ( Iop(1)==var ) THEN
!
!     VARIANCE SECTION
!
            IF ( Jrun==0 ) RETURN
!
!     SEE IF VARIANCE IS TO BE COMPUTED
!
            CALL preloc(*80,iz(ibuf1),it1)
            CALL locate(*140,iz(ibuf1),varan,iflag)
!
!     READ IN VARIANCES
!
            CALL read(*220,*100,it1,iz(1),nz,0,iflag)
            CALL mesage(-8,0,name)
            GOTO 100
         ELSE
            IF ( Iop(1)/=der ) RETURN
!
!     DERIVATIVES SECTION
!
            IF ( Jrun/=0 ) THEN
!
!     COMPUTE DERIVATIVES  DJ = (OJ - O0)/DELTA
!
               CALL open(*200,inpt,iz(ibuf1),0)
!
               DO i = 1 , 5
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        ifound = 0
                        CALL fwdrec(*36,inpt)
                        CALL open(*34,itf(i),iz(ibuf2),0)
                        CALL fwdrec(*220,itf(i))
                        CALL gopen(ito(i),iz(ibuf3),1)
                        file = itf(i)
                        spag_nextblock_2 = 2
                     CASE (2)
                        ASSIGN 22 TO irtn
                        spag_nextblock_2 = 3
                     CASE (3)
                        CALL read(*36,*240,inpt,iz(1),146,1,iflag)
                        GOTO irtn
 22                     CALL read(*34,*240,file,iz(147),146,1,iflag)
!
!     CHECK FOR MATCH ON SUBCASE
!
 24                     IF ( iz(4)<iz(150) ) THEN
                        ELSEIF ( iz(4)==iz(150) ) THEN
!
!     CHECK FOR MATCH ON TIME, FREQ ETC
!
                           IF ( z(5)<z(151) ) THEN
                           ELSEIF ( z(5)==z(151) ) THEN
!
!     CHECK FOR MATCH ON ELTYPE
!
                              IF ( iz(3)<iz(149) ) THEN
                              ELSEIF ( iz(3)==iz(149) ) THEN
!
!     WE GOT ONE
!
                                 iz(257) = derl(1)
                                 iz(258) = derl(2)
                                 iz(259) = derl(3)
                                 iz(260) = njrun
                                 ifound = 1 + ifound
                                 CALL write(ito(i),iz(147),146,1)
                                 nrec = iz(10)
                                 ASSIGN 26 TO irtn1
                                 spag_nextblock_2 = 4
                                 CYCLE SPAG_DispatchLoop_2
                              ELSE
!
!     NEED NEW FILE RECORD
!
                                 CALL fwdrec(*220,file)
                                 GOTO 22
                              ENDIF
                           ELSE
                              CALL fwdrec(*220,file)
                              GOTO 22
                           ENDIF
                        ELSE
                           CALL fwdrec(*220,file)
                           GOTO 22
                        ENDIF
!
!     NEED NEW INPT RECORD
!
                        CALL fwdrec(*220,inpt)
                        ASSIGN 24 TO irtn
                        spag_nextblock_2 = 3
                        CYCLE SPAG_DispatchLoop_2
                     CASE (4)
                        CALL read(*220,*30,inpt,iz(1),nrec,0,iflag)
                        id1 = iz(1)/10
                        GOTO irtn1
 26                     CALL read(*220,*32,file,iz(nrec+1),nrec,0,iflag)
                        id2 = iz(nrec+1)/10
                        ASSIGN 28 TO irtn1
 28                     IF ( id1<id2 ) THEN
                        ELSEIF ( id1==id2 ) THEN
!
!     POINT CHECKS
!
                           DO j = 2 , nrec
                              itype = numtyp(iz(j))
                              IF ( itype==2 .OR. itype==0 ) z(nrec+j) = (z(nrec+j)-z(j))/Delta
                           ENDDO
                           CALL write(ito(i),iz(nrec+1),nrec,0)
                           ASSIGN 26 TO irtn1
                        ELSE
                           GOTO 26
                        ENDIF
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
!
!     END OF DATA RECORD
!
 30                     CALL fwdrec(*220,file)
                        spag_nextblock_2 = 5
                        CYCLE SPAG_DispatchLoop_2
 32                     CALL fwdrec(*220,inpt)
                        spag_nextblock_2 = 5
                     CASE (5)
!
!     EOF ON INPT
!
                        CALL write(ito(i),0,0,1)
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
!
!     EOF ON FILE
!
 34                     CALL skpfil(inpt,1)
 36                     CALL close(file,1)
                        CALL close(ito(i),1)
                        mcb(1) = ito(i)
                        mcb(2) = ifound
                        IF ( ifound/=0 ) CALL wrttrl(mcb)
                        EXIT SPAG_DispatchLoop_2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
               ENDDO
!
!     SKIP OVER OLD DERIVATIVES
!
               i = 5*Jrun - 5
               CALL skpfil(inpt,i)
               CALL close(inpt,2)
               CALL gopen(inpt,iz(ibuf1),3)
               i = 1
               ASSIGN 60 TO iret
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     COPY INPUT FILES TO INPT TAPE
!
               CALL open(*200,inpt,iz(ibuf1),1)
               i = 1
               ASSIGN 40 TO iret
               file = itf(i)
!
!     INTERNAL ROUTINE TO COPY FILES
!
               CALL open(*180,file,iz(ibuf2),0)
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
 40      i = i + 1
         IF ( i<=5 ) THEN
            file = itf(i)
            CALL open(*180,file,iz(ibuf2),0)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL close(inpt,2)
            RETURN
         ENDIF
      CASE (2)
         file = ito(i)
         mcb(1) = file
         CALL rdtrl(mcb)
         IF ( mcb(2)/=0 ) THEN
            CALL open(*180,file,iz(ibuf2),0)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL eof(inpt)
         ENDIF
 60      i = i + 1
         IF ( i<=5 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL close(inpt,2)
 80      RETURN
 100     IF ( iflag-1==Jrun ) THEN
!
!     SET UP FOR VARIANCES
!
            CALL close(it1,1)
            CALL open(*200,inpt,iz(ibuf1),0)
            CALL skpfil(inpt,5)
            in1 = inpt
            io1 = scr1
            DO i = 1 , Jrun
               IF ( i/=Jrun ) CALL open(*200,io1,iz(ibuf2),1)
               IF ( i/=1 ) CALL open(*200,in1,iz(ibuf3),0)
               DO j = 1 , 5
                  spag_nextblock_3 = 1
                  SPAG_DispatchLoop_3: DO
                     SELECT CASE (spag_nextblock_3)
                     CASE (1)
                        IF ( i==Jrun ) THEN
!
!     FIX UP FOR WRITING ON OUTPUT FILES
!
                           CALL open(*120,ito(j),iz(ibuf2),1)
                           CALL fname(ito(j),mcb)
                           CALL write(ito(j),mcb,2,1)
                           ifound = 0
                           io1 = ito(j)
                        ENDIF
                        CALL fwdrec(*116,inpt)
                        ASSIGN 102 TO irtn2
                        spag_nextblock_3 = 2
                     CASE (2)
                        CALL read(*118,*240,in1,iz(Jrun+1),146,1,iflag)
                        IF ( i/=1 ) THEN
!
!     CHECK FOR MATCH
!
                           GOTO irtn2
                        ELSE
                           iz(Jrun+111) = varl(1)
                           iz(Jrun+112) = varl(2)
                           iz(Jrun+113) = varl(3)
                           iz(Jrun+114) = iblnk
                           spag_nextblock_3 = 3
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
 102                    CALL read(*114,*240,inpt,iz(Jrun+147),146,1,iflag)
 104                    IF ( iz(Jrun+4)<iz(Jrun+150) ) THEN
                        ELSEIF ( iz(Jrun+4)==iz(Jrun+150) ) THEN
                           IF ( z(Jrun+5)<z(Jrun+151) ) THEN
                           ELSEIF ( z(Jrun+5)==z(Jrun+151) ) THEN
                              IF ( iz(Jrun+3)<iz(Jrun+149) ) THEN
                              ELSEIF ( iz(Jrun+3)==iz(Jrun+149) ) THEN
                                 spag_nextblock_3 = 3
                                 CYCLE SPAG_DispatchLoop_3
                              ELSE
                                 CALL fwdrec(*220,inpt)
                                 GOTO 102
                              ENDIF
                           ELSE
                              CALL fwdrec(*220,inpt)
                              GOTO 102
                           ENDIF
                        ELSE
                           CALL fwdrec(*220,inpt)
                           GOTO 102
                        ENDIF
                        CALL fwdrec(*220,in1)
                        ASSIGN 104 TO irtn2
                        spag_nextblock_3 = 2
                        CYCLE SPAG_DispatchLoop_3
                     CASE (3)
!
!     MATCH
!
                        CALL write(io1,iz(Jrun+1),146,1)
                        nrec = iz(Jrun+10)
                        m = Jrun + nrec
                        spag_nextblock_3 = 4
                     CASE (4)
                        ASSIGN 106 TO irtn3
                        spag_nextblock_3 = 5
                     CASE (5)
                        CALL read(*220,*110,in1,iz(Jrun+1),nrec,0,iflag)
                        IF ( i==1 ) THEN
                           spag_nextblock_3 = 6
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
                        id1 = iz(Jrun+1)/10
                        GOTO irtn3
 106                    CALL read(*220,*112,inpt,iz(m+1),nrec,0,iflag)
                        id2 = iz(m+1)/10
                        ASSIGN 108 TO irtn3
 108                    IF ( id1<id2 ) THEN
                           spag_nextblock_3 = 5
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
                        IF ( id1/=id2 ) GOTO 106
                        spag_nextblock_3 = 6
                     CASE (6)
!
!     POINT MATCH
!
                        IF ( i==Jrun ) ifound = ifound + 1
                        DO k = 2 , nrec
                           itype = numtyp(iz(Jrun+k))
                           IF ( itype==2 .OR. itype==0 ) THEN
                              IF ( i/=1 ) THEN
                                 z(Jrun+k) = z(Jrun+k) + (z(m+k)*z(i))**2
                              ELSE
                                 z(Jrun+k) = (z(Jrun+k)*z(1))**2
                              ENDIF
                              IF ( i==Jrun ) z(Jrun+k) = sqrt(z(Jrun+k))
                           ENDIF
                        ENDDO
                        CALL write(io1,iz(Jrun+1),nrec,0)
                        spag_nextblock_3 = 4
                        CYCLE SPAG_DispatchLoop_3
!
!     END OF DATA ON IN1
!
 110                    IF ( i/=1 ) CALL fwdrec(*220,inpt)
                        spag_nextblock_3 = 7
                        CYCLE SPAG_DispatchLoop_3
 112                    CALL fwdrec(*220,in1)
                        spag_nextblock_3 = 7
                     CASE (7)
                        CALL write(io1,0,0,1)
                        ASSIGN 102 TO irtn2
                        spag_nextblock_3 = 2
                        CYCLE SPAG_DispatchLoop_3
!
!     EOF ON INPT
!
 114                    CALL skpfil(in1,1)
 116                    CALL eof(io1)
                        IF ( i==Jrun ) THEN
                           CALL close(io1,1)
                           mcb(1) = io1
                           mcb(2) = ifound
                           CALL wrttrl(mcb)
                        ENDIF
                        CYCLE
 118                    IF ( i/=1 ) CALL skpfil(inpt,1)
                        GOTO 116
                     END SELECT
                  ENDDO SPAG_DispatchLoop_3
 120           ENDDO
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
 140     CALL close(it1,1)
         RETURN
      CASE (3)
         ieor = 1
         CALL read(*180,*160,file,iz(1),nz,0,iread)
         ieor = 0
 160     CALL write(inpt,iz(1),iread,ieor)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 180     CALL eof(inpt)
         CALL close(file,1)
         GOTO iret
!
!     ERROR MESSAGES
!
 200     ip1 = -1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 220     ip1 = -2
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 240     ip1 = -3
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(ip1,file,name)
         STOP
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE varian
