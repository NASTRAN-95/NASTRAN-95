
!RLBR SPR94003 9/94
!RLBR1                   NOUE,MODAL,PNL)
SUBROUTINE trd1c2(Ic,Pd,Ngroup,Nlftp,Udv,Iloop,Scr1,Dit,Nlft,Noue,Modal,Pnl,Iskip)
   IMPLICIT NONE
   REAL Deltat , Dummy(4) , Rz(1) , To
   INTEGER Dit1 , Icore , Icount , Icpflg , Idum(14) , Ii , Iii , Ik(7) , Iloop1 , Incr , Incr1 , Iopen , Ip4 , Ipnl(7) , Iscr1 ,   &
         & Iscr2 , Iscr3 , Iscr4 , Iscr5 , Iscr6 , Ispnl , Ist , Isym , Isystm(79) , It1 , It2 , It3 , Iu1 , Iu2 , Iz(1) , Jj ,     &
         & Jjj , Modal1 , Ncol , Nlft1 , Nlftp1 , Nmodes , Nnout , Nout , Nstep , Nz , Pnl1 , Sysbuf
   LOGICAL Nopd
   DOUBLE PRECISION Z(1)
   COMMON /blank / Dummy , Ncol
   COMMON /packx / It1 , It2 , Ii , Jj , Incr
   COMMON /system/ Sysbuf , Nnout , Isystm , Icpflg
   COMMON /trdd1 / Nlft1 , Dit1 , Nlftp1 , Nout , Icount , Iloop1 , Modal1 , Nz , Icore , Iu2 , Ip4 , Ipnl , Nmodes , Nstep , Pnl1 ,&
                 & Ist , Iu1 , Deltat
   COMMON /trdxx / Ik , Idum , Iscr1 , Iscr2 , Iscr3 , Iscr4 , Iscr5 , Iscr6 , Iopen , Isym , To , Nopd , Ispnl
   COMMON /unpakx/ It3 , Iii , Jjj , Incr1
   COMMON /zzzzzz/ Z
   INTEGER Dit , Ic , Iloop , Iskip , Modal , Ngroup , Nlft , Nlftp , Noue , Pd , Pnl , Scr1 , Udv
   REAL delta1
   INTEGER file , i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , ibuf5 , ibuf6 , ibuf7 , ibuf8 , ibuf9 , ibufa , icrq , iflag , igroup , inl , &
         & ioutpu , ip1 , ip2 , ip3 , ipnt , iret1 , iretn , iscr9 , itest , itleft , itype , iu3 , j , jskip , k , kk , kkk , kkp ,&
         & l , m , mcb(7) , mcol , moutpu(7) , nnrow , nrow , nwds , subnam(2)
   DOUBLE PRECISION h
   INTEGER korsz
!
!     THIS ROUTINE STEPS INTEGRATION PROCEDURE
!
!     THIS ROUTINE IS SUITABLE FOR DOUBLE PRECISION OPERATION
!
!RLBR SPR94003 9/94
!RLBR1                MCB(7),IPNL(7),SUBNAM(2)
!RLBR SPR94003 9/94
!RLBR COMMON /SYSTEM/ SYSBUF
   !>>>>EQUIVALENCE (Z(1),Rz(1),Iz(1))
   DATA subnam/4HTRD1 , 2HC2/
!RLBNB SPR94003 9/94
   DATA ioutpu , iscr9/203 , 309/
!RLBNE
!
!     INITIALIZE
!
   nrow = Ik(3)
   nnrow = 2*nrow
   It1 = 2
   It2 = 2
   Ii = 1
   Jj = nrow
   Incr = 1
   It3 = 2
   Iii = 1
   Jjj = nrow
   Incr1 = 1
   Nz = korsz(Z)
   igroup = Nz - 3*Ngroup + 1
   ibuf1 = igroup - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   ibuf4 = ibuf3 - Sysbuf
   ibuf5 = ibuf4 - Sysbuf
   ibuf6 = ibuf5 - Sysbuf
   ibuf7 = ibuf6 - Sysbuf
   ibuf8 = ibuf7 - Sysbuf
!RLBNB SPR94003 9/94
   IF ( Nlftp==0 ) ibuf8 = ibuf7
   ibuf9 = ibuf8 - Sysbuf
   ibufa = ibuf9 - Sysbuf
   IF ( Icpflg==0 ) ibufa = ibuf8
   IF ( Icpflg/=0 .AND. Iskip==1 ) ibufa = ibuf9
   Nz = ibufa - 1
!RLBNE
!RLBD SPR94003 9/94 NZ = IBUF7-1
!RLBD SPR94003 9/94 IF(NLFTP .NE. 0) NZ = IBUF8-1
   Iopen = 0
!RLBR SPR94003 9/94 ICRQ = 14*NROW + 1 - NZ
   icrq = 14*(nrow+1) + 1 - Nz
   IF ( icrq>0 ) THEN
!RLBNE
!
      ip1 = -8
      file = icrq
      GOTO 3000
   ELSE
!RLBR SPR94003 9/94 IU1=0
      Iu1 = 1
!RLBR SPR94003 9/94 IU2= IU1+NROW
      Iu2 = Iu1 + nrow + 1
!RLBR SPR94003 9/94 IU3= IU2+ NROW
      iu3 = Iu2 + nrow + 1
!RLBR SPR94003 9/94 IP1= IU3+ NROW
      ip1 = iu3 + nrow + 1
!RLBR SPR94003 9/94 IP2= IP1+ NROW
      ip2 = ip1 + nrow + 1
      ip3 = ip2 + nrow
      Ip4 = ip3 + nrow
      Nlft1 = Nlft
      Dit1 = Dit
      Nlftp1 = Nlftp
      Iloop1 = Iloop
      Modal1 = Modal
      Ist = 0
!RLBR SPR94003 9/94 NZ    = NZ - 14*NROW - 1
      Nz = Nz - 14*(nrow+1) - 1
      Icore = 2*(Ip4+nrow)
      Nmodes = nrow - Noue
      Pnl1 = Pnl
      ASSIGN 600 TO iret1
      Nstep = Iz(igroup) + 1
      Deltat = Rz(igroup+1)
      Nout = Iz(igroup+2)
      IF ( Iloop/=1 ) THEN
!
!     CHANGE OF TIME STEP--RESTORE POINTERS ETC
!
         igroup = igroup + (Iloop-1)*3
         delta1 = Rz(igroup-2)
         Nstep = Iz(igroup)
         Deltat = Rz(igroup+1)
         Nout = Iz(igroup+2)
         IF ( .NOT.Nopd ) CALL gopen(Pd,Iz(ibuf2),2)
         CALL gopen(Udv,Iz(ibuf3),3)
         mcb(1) = Udv
         CALL rdtrl(mcb)
!RLBNB SPR94003 9/94
         IF ( Icpflg/=0 ) THEN
            CALL gopen(ioutpu,Iz(ibuf9),3)
            moutpu(1) = ioutpu
            CALL rdtrl(moutpu)
         ENDIF
!RLBNE
         IF ( Nlftp/=0 ) THEN
            IF ( Ispnl>0 ) CALL gopen(Pnl1,Iz(ibuf8),3)
         ENDIF
!
!     RESTORE STUFF SAVED
!
         file = Scr1
         CALL open(*2900,Scr1,Iz(ibuf1),0)
         CALL fread(Scr1,Z(Iu1+1),nnrow,1)
         CALL fread(Scr1,Z(iu3+1),nnrow,1)
         CALL fread(Scr1,Z(Iu2+1),nnrow,1)
         CALL fread(Scr1,Z(ip2+1),nnrow,1)
         CALL close(Scr1,1)
         GOTO 1600
      ELSE
!
!     FIRST ENTRY INITIALIZE STUFF
!
         Ist = -1
         file = Pd
!
!     PUT P0 IN IP2
!
         ipnt = ip2
         Nopd = .TRUE.
         ASSIGN 100 TO iretn
         CALL open(*1800,Pd,Iz(ibuf2),0)
         CALL skprec(Pd,1)
         Nopd = .FALSE.
!
!     INTERNAL ROUTINE TO UNPACK VECTORS
!
         CALL unpack(*1800,file,Z(ipnt+1))
         GOTO 1700
      ENDIF
   ENDIF
!RLBD SPR94003 9/94     5 FILE = UDV
!RLBD SPR94003 9/94       IAPEND = 0
!RLBR SPR94003 9/94       IF (NCOL .LE. 0) GO TO 8
 100  IF ( Ncol>2 ) THEN
!RLBNB SPR94003 9/94
!     THE FOLLOWING LINES (UNTIL CRPKNE) REPRESENT
!     REPLACEMENTS FOR THE OLD CODE WHICH HAS BEEN
!     DELETED BELOW
!
!     RETRIEVE REQUIRED INFORMATION FROM
!     THE CHECKPOINT RUN
!
      mcol = Ncol
      CALL gopen(ioutpu,Iz(ibuf4),0)
      moutpu(1) = ioutpu
      CALL rdtrl(moutpu)
      jskip = 1
      IF ( moutpu(4)==1 ) GOTO 2100
      jskip = 2
      CALL skprec(ioutpu,moutpu(2))
      file = ioutpu
      nwds = Ncol - 1
      CALL read(*3100,*1900,ioutpu,mcol,-nwds,0,iflag)
      GOTO 2000
   ELSE
!RLBD SPR94003 9/94      MCB(1) = UDV
!RLBD SPR94003 9/94      CALL RDTRL (MCB)
!RLBD SPR94003 9/94      IF (MCB(2) .NE. 0) GO TO 330
!RLBR SPR94003 9/94    8 CALL GOPEN (UDV,IZ(IBUF3),1)
      CALL gopen(Udv,Iz(ibuf3),1)
      CALL makmcb(mcb,Udv,nrow,2,2)
   ENDIF
!RLBNB SPR94003 9/94
 200  IF ( Icpflg/=0 ) THEN
      CALL makmcb(moutpu,ioutpu,nrow+1,Iskip,2)
      CALL gopen(ioutpu,Iz(ibuf9),1)
      IF ( Iskip==0 ) CALL gopen(iscr9,Iz(ibufa),1)
   ENDIF
!RLBNE
   IF ( Nlftp/=0 ) THEN
!
!     CHECK TO SEE IF PNL HAS BEEN PRE-PURGED.
!
      Ipnl(1) = Pnl1
      CALL rdtrl(Ipnl)
      Ispnl = 0
      IF ( Ipnl(1)>0 ) THEN
         Ispnl = 1
         CALL gopen(Pnl1,Iz(ibuf8),1)
         CALL makmcb(Ipnl,Pnl1,nrow,2,2)
      ENDIF
   ENDIF
!RLBR SPR94003 9/94      IF (IAPEND .EQ. 1) GO TO 50
   IF ( Ncol>2 ) GOTO 500
   file = Ic
   CALL gopen(Ic,Iz(ibuf1),0)
   ASSIGN 300 TO iretn
   ipnt = Iu2
   CALL unpack(*1800,file,Z(ipnt+1))
   GOTO 1700
 300  ASSIGN 400 TO iretn
   ipnt = iu3
   CALL unpack(*1800,file,Z(ipnt+1))
   GOTO 1700
 400  CALL close(Ic,1)
   Nstep = Iz(igroup) + 1
   Deltat = Rz(igroup+1)
   Nout = Iz(igroup+2)
!
!     FORM  U=1, PO, P-1
!
   CALL form12(Z(Iu2+1),Z(iu3+1),Z(Iu1+1),Z(ip2+1),Z(ip1+1),Deltat,Rz(ibuf1))
!
!     START TIME STEP COUNT
!
 500  Icount = 1
!RLBNB SPR94003 9/94
   mcol = 1
!RLBNE
!
!     OPEN FBS FILES
!
 600  file = Iscr1
   CALL open(*2900,Iscr1,Iz(ibuf4),0)
   file = Iscr2
   CALL open(*2900,Iscr2,Iz(ibuf5),0)
   file = Iscr3
!IBMR 5/95
!     CALL OPEN (*390,ISCR3,IZ(IBUF6),0)
   IF ( Isym==1 ) CALL open(*2900,Iscr3,Iz(ibuf6),0)
   file = Iscr4
   CALL open(*2900,Iscr4,Iz(ibuf7),0)
!
!     ZERO P*
!
 700  CALL tmtogo(itleft)
   IF ( itleft<=0 ) THEN
      j = 1
      GOTO 1200
   ELSE
      DO i = 1 , nrow
         k = Ip4 + i
         Z(k) = 0.0D0
      ENDDO
      IF ( Nlftp/=0 ) THEN
!
!     FORM NON-LINEAR LOADS
!
         CALL trd1d2
         IF ( Icount==1 .OR. Icount==Nstep .OR. mod(Icount+Ist,Nout)==0 ) THEN
            IF ( Ispnl>0 ) CALL pack(Z(Ip4+1),Pnl,Ipnl)
         ENDIF
      ENDIF
!
!     BRING IN NEXT P
!
      ipnt = ip3
      file = Pd
      ASSIGN 800 TO iretn
      IF ( Nopd ) GOTO 1800
      CALL unpack(*1800,file,Z(ipnt+1))
      GOTO 1700
   ENDIF
!
!     ADD P-S TO FORM P*
!
 800  DO i = 1 , nrow
      k = Ip4 + i
      l = ip1 + i
      m = ip2 + i
      j = ip3 + i
      Z(k) = Z(k) + (Z(l)+Z(m)+Z(j))/3.0D0
   ENDDO
   IF ( Iloop==1 .AND. Icount==1 ) THEN
!RLBR SPR94003 9/94      IF (IAPEND .EQ. 1) GO TO 115
      IF ( Ncol<=2 ) THEN
!
!     OUTPUT INITIAL DISPLACEMENT
!
         CALL pack(Z(Iu2+1),Udv,mcb(1))
!
!     OUTPUT INITIAL VELOCITY
!
         CALL pack(Z(iu3+1),Udv,mcb(1))
      ENDIF
!RLBNB SPR94003 9/94
      IF ( Icpflg/=0 ) THEN
         IF ( Iskip==0 ) CALL write(iscr9,mcol,1,0)
      ENDIF
   ENDIF
!RLBNE
!
!     SOLVE FOR NEXT SOLUTION
!
   CALL step2(Z(iu3+1),Z(Iu2+1),Z(Iu1+1),Z(Ip4+1),Iz(ibuf1))
!RLBNB SPR94003 9/94
   IF ( Icpflg/=0 ) THEN
      Jj = nrow + 1
      Z(ip2) = Deltat
      IF ( Iloop/=1 .AND. Icount==0 ) Z(ip2) = delta1
      CALL pack(Z(ip2),ioutpu,moutpu)
      IF ( Iskip/=1 ) THEN
         Z(Iu2) = mcol + 0.1
         CALL pack(Z(Iu2),ioutpu,moutpu)
      ENDIF
      Jj = nrow
   ENDIF
!RLBNE
   IF ( Iloop==1 .AND. Icount==1 ) GOTO 1100
   IF ( Icount==Nstep .OR. mod(Icount+Ist,Nout)==0 ) GOTO 1000
   IF ( Icount==1 ) GOTO 1000
!
!     ROTATE P POINTERS
!
 900  j = ip1
   ip1 = ip2
   ip2 = ip3
   ip3 = j
!
!     ROTATE U POINTERS
!
   j = Iu1
   Iu1 = Iu2
   Iu2 = iu3
   iu3 = j
   Icount = Icount + 1
!RLBNB SPR94003 9/94
   mcol = mcol + 1
!RLBNE
   IF ( Icount<Nstep ) GOTO 700
   IF ( Icount==Nstep ) THEN
!
!     END OF 1 GROUP
!
      IF ( Iloop==Ngroup ) GOTO 700
!
!     MORE GROUPS TO COME SAVE STUFF
!
      j = 2
      file = Scr1
      CALL open(*2900,Scr1,Iz(ibuf1),1)
      CALL write(Scr1,Z(iu3+1),nnrow,1)
      CALL write(Scr1,Z(Iu1+1),nnrow,1)
      CALL write(Scr1,Z(Iu2+1),nnrow,1)
!RLBR SPR94003 9/94
!RLBR CALL WRITE (SCR1,Z(IP1+1),NNROW,1)
      CALL write(Scr1,Z(ip2+1),nnrow,1)
      CALL close(Scr1,1)
      GOTO 1200
   ELSE
      j = 1
      GOTO 1200
   ENDIF
!
!     IT-S OUTPUT TIME -- LUCKY FELLOW
!
 1000 CALL pack(Z(Iu2+1),Udv,mcb(1))
!
!     COMPUTE U DOT
!
   h = 1.0D0/(2.0D0*Deltat)
   DO i = 1 , nrow
      k = Ip4 + i
      l = iu3 + i
      m = Iu1 + i
      Z(k) = (Z(l)-Z(m))*h
   ENDDO
   CALL pack(Z(Ip4+1),Udv,mcb(1))
!RLBNB SPR94003 9/94
   IF ( Icpflg/=0 ) THEN
      IF ( Iskip==0 ) CALL write(iscr9,mcol,1,0)
   ENDIF
!RLBNE
!
!     COMPUTE U DOT DOT
!
 1100 h = 1.0D0/(Deltat*Deltat)
   DO i = 1 , nrow
      k = Ip4 + i
      l = iu3 + i
      m = Iu1 + i
      j = Iu2 + i
      Z(k) = (Z(l)+Z(m)-2.0D0*Z(j))*h
   ENDDO
   CALL pack(Z(Ip4+1),Udv,mcb(1))
   GOTO 900
 1200 CALL close(Udv,j)
   CALL close(Pd,j)
!RLBNB SPR94003 9/94
   IF ( Icpflg==0 ) GOTO 1500
   IF ( j/=1 .OR. Iskip==1 ) GOTO 1400
   CALL close(iscr9,1)
!
!     COPY THE SINGLE RECORD IN FILE ISCR9 AS THE
!     LAST RECORD IN FILE IOUTPU
!
   CALL gopen(iscr9,Iz(ibufa),0)
   file = iscr9
   DO
      CALL read(*3100,*1300,iscr9,Z(Iu2+1),nrow,0,iflag)
      CALL write(ioutpu,Z(Iu2+1),nrow,0)
   ENDDO
 1300 CALL write(ioutpu,Z(Iu2+1),iflag,1)
   CALL close(iscr9,1)
 1400 CALL close(ioutpu,j)
   CALL wrttrl(moutpu)
!RLBNE
 1500 CALL close(Iscr1,1)
   CALL close(Iscr2,1)
!IBMR 5/95
!     CALL CLOSE (ISCR3,1)
   IF ( Isym==1 ) CALL close(Iscr3,1)
   CALL close(Iscr4,1)
   CALL wrttrl(mcb)
   IF ( Nlftp/=0 .AND. Ispnl/=0 ) THEN
      CALL close(Pnl,j)
      CALL wrttrl(Ipnl)
   ENDIF
   RETURN
!
!     COMPUTE U DOT
!
!RLBR SPR94003 9/94      H = 1.0D0/DELTA1
 1600 h = 1.0D0/delta1
   DO i = 1 , nrow
      k = ip1 + i
      l = Iu2 + i
      m = iu3 + i
      Z(k) = (Z(l)-Z(m))*h
   ENDDO
!
!     COMPUTE U DOT DOT
!
   h = 1.0D0/(delta1*delta1)
   DO i = 1 , nrow
      k = Ip4 + i
      l = Iu2 + i
      m = iu3 + i
      j = Iu1 + i
      Z(k) = (Z(l)-2.0D0*Z(m)+Z(j))*h
   ENDDO
!RLBD SPR94003 9/94   250 CONTINUE
!
!     COMPUTE UI PRIME
!
   h = Deltat*Deltat/2.0D0
   DO i = 1 , nrow
      k = Iu1 + i
      l = Iu2 + i
      m = ip1 + i
      j = Ip4 + i
      Z(k) = Z(l) - Deltat*Z(m) + h*Z(j)
   ENDDO
!
!     COMPUTE U DOT PRIME
!
   DO i = 1 , nrow
      k = iu3 + i
      l = ip1 + i
      m = Ip4 + i
      Z(k) = Z(l) - Deltat*Z(m)
   ENDDO
!
!     COMPUTE PI PRIME
!
   DO i = 1 , nrow
      k = ip1 + i
      Z(k) = 0.0D0
   ENDDO
   CALL form22(Z(Ip4+1),Z(iu3+1),Z(Iu1+1),Z(ip1+1),Rz(ibuf1))
   Icount = 0
!RLBR SPR94003 9/94      GO TO IRET1, (60,10)
   GOTO iret1
!RLBR SPR94003 9/94  300 GO TO IRETN, (5,30,40,100,350,360,370)
 1700 GOTO iretn
!RLBR SPR94003 9/94  310 DO 320 INL = 1,NROW
 1800 DO inl = Iii , Jjj
      k = ipnt + inl
      Z(k) = 0.0D0
   ENDDO
   GOTO 1700
 1900 nwds = nwds - iflag
   CALL read(*3100,*1900,ioutpu,mcol,-nwds,0,iflag)
 2000 CALL read(*3100,*2000,ioutpu,mcol,1,0,iflag)
   CALL rewind(ioutpu)
   CALL skprec(ioutpu,1)
!
 2100 CALL skprec(ioutpu,jskip*(mcol-1))
   file = ioutpu
   Jjj = nrow + 1
!
!     GET P SUB I+1
!
   ipnt = ip2 - 1
   ASSIGN 2200 TO iretn
   CALL unpack(*1800,file,Z(ipnt+1))
   GOTO 1700
 2200 itype = 1
   delta1 = Z(ip2)
   IF ( delta1==Deltat ) THEN
      CALL skprec(ioutpu,-(jskip+1))
!
!     GET P SUB I
!
      ipnt = ip1 - 1
      ASSIGN 2300 TO iretn
      CALL unpack(*1800,file,Z(ipnt+1))
      GOTO 1700
   ELSE
      itype = 2
   ENDIF
 2300 CALL close(ioutpu,1)
!
   file = Udv
   CALL gopen(Udv,Iz(ibuf3),0)
   k = 3*(Ncol-1)
   kk = 5
   kkk = 4
   kkp = 0
   Jjj = nrow
   CALL skprec(Udv,k)
!
!     GET U SUB I+1
!
   ipnt = Iu2
   ASSIGN 2400 TO iretn
   CALL unpack(*1800,file,Z(ipnt+1))
   GOTO 1700
!
!     GET U DOT SUB I+1
!
 2400 ipnt = ip3
   ASSIGN 2500 TO iretn
   CALL unpack(*1800,file,Z(ipnt+1))
   GOTO 1700
!
 2500 IF ( mcol/=Ncol ) THEN
      CALL close(Udv,1)
      file = ioutpu
      CALL gopen(ioutpu,Iz(ibuf4),0)
      k = 2*mcol - 3
      kk = 0
      kkk = 3
      kkp = 1
      Jjj = nrow + 1
      CALL skprec(ioutpu,k)
   ENDIF
!
!     GET U SUB I
!
   ipnt = Iu1 - kkp
   IF ( itype==2 ) ipnt = iu3 - kkp
   CALL skprec(file,-kk)
   ASSIGN 2600 TO iretn
   CALL unpack(*1800,file,Z(ipnt+1))
   GOTO 1700
 2600 IF ( itype==1 ) GOTO 2800
   IF ( mcol/=Ncol ) THEN
      itest = Z(ipnt+1)
      IF ( mcol/=itest+1 ) THEN
         WRITE (Nnout,99001)
!RLBNB SPR94003 9/94
99001    FORMAT ('0*** SYSTEM FATAL MESSAGE, LOGIC ERROR 1 IN ','SUBROUTINE TRD1C2 WHILE PROCESSING THE RESTART ','INFORMATION')
         CALL mesage(-61,0,0)
      ENDIF
   ENDIF
   CALL skprec(file,-kkk)
!
!     GET U SUB I-1
!
   ipnt = Iu1 - kkp
   ASSIGN 2700 TO iretn
   CALL unpack(*1800,file,Z(ipnt+1))
   GOTO 1700
 2700 IF ( mcol/=Ncol ) THEN
      itest = Z(ipnt+1)
      IF ( mcol/=itest+2 ) THEN
         WRITE (Nnout,99002)
99002    FORMAT ('0*** SYSTEM FATAL MESSAGE, LOGIC ERROR 2 IN ','SUBROUTINE TRD1C2 WHILE PROCESSING THE RESTART ','INFORMATION')
         CALL mesage(-61,0,0)
      ENDIF
   ENDIF
 2800 CALL close(file,1)
   Jjj = nrow
   CALL gopen(Udv,Iz(ibuf3),1)
   CALL makmcb(mcb,Udv,nrow,2,1)
!
!     OUTPUT INITIAL DISPLACEMENT
!
   CALL pack(Z(Iu2+1),Udv,mcb(1))
!
!     OUTPUT INITIAL VELOCITY
!
   CALL pack(Z(ip3+1),Udv,mcb(1))
   IF ( itype==1 ) GOTO 200
   ASSIGN 200 TO iret1
   GOTO 1600
!RLBNE
!RLBDB SPR94003 9/94
!RLBD C
!RLBD C     RETRIEVE LAST VECTOR
!RLBD C
!RLBD   330 CALL GOPEN (UDV,IZ(IBUF3),0)
!RLBD       K = 3*(NCOL-1)
!RLBD       IAPEND = 1
!RLBD       CALL SKPREC (UDV,K)
!RLBD C
!RLBD C     GET U SUB I+1
!RLBD C
!RLBD       IPNT = IU2
!RLBD       ASSIGN 350 TO IRETN
!RLBD       GO TO 290
!RLBD CP
!RLBD C     GET U SUB I+1 DOT
!RLBD C
!RLBD   350 IPNT = IP1
!RLBD       ASSIGN 360 TO IRETN
!RLBD       GO TO 290
!RLBD C
!RLBD C     GET U SUB I+1 DOT DOT
!RLBD C
!RLBD   360 IPNT = IP4
!RLBD       ASSIGN 370 TO IRETN
!RLBD       GO TO 290
!RLBD   370 CONTINUE
!RLBD       CALL CLOSE (UDV,1)
!RLBD       CALL GOPEN (UDV,IZ(IBUF3),1)
!RLBD       CALL MAKMCB (MCB,UDV,NROW,2,2)
!RLBD C
!RLBD C     OUTPUT INITIAL DISPLACEMENT
!RLBD C
!RLBD       CALL PACK (Z(IU2+1),UDV,MCB(1))
!RLBD C
!RLBD C     OUTPUT INITIAL VELOCITY
!RLBD C
!RLBD       CALL PACK (Z(IP1+1),UDV,MCB(1))
!RLBD C
!RLBD C     FORM P SUB I+1
!RLBD C
!RLBD       DO 380 I = 1,NROW
!RLBD       K = IP2 + I
!RLBD       Z(K) = 0.0D0
!RLBD   380 CONTINUE
!RLBD       CALL FORM22 (Z(IP4+1),Z(IP1+1),Z(IU2+1),Z(IP2+1),RZ(IBUF1))
!RLBD       ASSIGN 10 TO IRET1
!RLBD       GO TO 250
!RLBDE
!
!     ERROR MESAGES
!
 2900 ip1 = -1
 3000 CALL mesage(ip1,file,subnam)
   RETURN
!RLBNB SPR94003 9/94
 3100 ip1 = -2
   GOTO 3000
!RLBNE
END SUBROUTINE trd1c2