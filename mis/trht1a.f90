
SUBROUTINE trht1a(Casexx,Usetd,Gptt,Trl,Ngroup)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(4) , Dit1 , X , Z(1)
   INTEGER Ia(1) , Ib(7) , Icr1 , Icr2 , Icr3 , Icr4 , Ii , Ii1 , Ik(7) , Incr , Iscr5 , Isk(11) , Isk1(3) , It1 , It2 , Iud , Iue ,&
         & Iz(160) , Jj1 , Nlft1 , Nlftp1 , Sysbuf , Two1(32)
   COMMON /bitpos/ Isk , Iue , Isk1 , Iud
   COMMON /blank / X
   COMMON /packx / It1 , It2 , Ii1 , Jj1 , Incr
   COMMON /system/ Sysbuf
   COMMON /trdd1 / Nlft1 , Dit1 , Nlftp1
   COMMON /trhtx / Ik , Ib , Icr1 , Icr2 , Icr3 , Icr4 , Iscr5
   COMMON /two   / Two1
   COMMON /zblpkx/ A , Ii
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Casexx , Gptt , Ngroup , Trl , Usetd
!
! Local variable declarations
!
   INTEGER andf , korsz
   INTEGER file , i , ibuf1 , ibuf2 , iflag , iflg , igroup , inext , inltmp , intmp , ip1 , ipos , its , itstep , ival , j , k ,   &
         & l , list , lusetd , m , mcb(7) , mskud , mskue , name(2) , ns , nsk , nx , nz
   REAL tdflt
   EXTERNAL andf
!
! End of declarations
!
!
!     TRHT1A INITIALIZES FOR TRHT MODULE
!
!     ITS TASK IS TO EXTRACT INITIAL CONDITION POINTS FROM CASEXX
!     AND TO PUT INITIAL STUFF ON ICR5
!
   EQUIVALENCE (Z(1),Iz(1)) , (A(1),Ia(1))
   DATA name/4HTRHT , 4H1A  /
!
!
   nz = korsz(Z)
   nx = nz
   ibuf1 = nz - Sysbuf + 1
   nz = nz - Sysbuf
   CALL gopen(Casexx,Iz(ibuf1),0)
   CALL fread(Casexx,Iz(1),166,1)
   CALL close(Casexx,1)
   itstep = Iz(38)
   Nlftp1 = Iz(160)
   intmp = Iz(9)
   inltmp = Iz(8)
!
!     FIND STUFF ON TRL
!
   file = Trl
   CALL open(*1000,Trl,Iz(ibuf1),0)
   CALL read(*1200,*100,Trl,Iz(1),nz,0,iflag)
   ip1 = -8
   GOTO 1100
 100  ns = Iz(3)
   CALL skprec(Trl,ns)
 200  CALL read(*1300,*300,Trl,Iz(1),nz,0,iflag)
   ip1 = -8
   GOTO 1100
 300  IF ( Iz(1)/=itstep ) GOTO 200
!
!     TSTEP STUFF FOUND
!
   CALL close(Trl,1)
   Ngroup = (iflag-1)/3
!
!     MOVE TSETP STUFF TO BOTTOM OF CURE
!
   nz = nx - iflag + 1
   igroup = nz + 1
   DO i = 2 , iflag
      k = igroup + i - 2
      Iz(k) = Iz(i)
   ENDDO
   ibuf1 = nz - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   nz = ibuf2
   CALL gopen(Iscr5,Iz(ibuf1),1)
   CALL write(Iscr5,Iz(igroup),iflag-1,1)
   file = Usetd
!
!     BRING IN USETD
!
   CALL gopen(Usetd,Iz(ibuf2),0)
   CALL read(*1200,*400,Usetd,Iz(1),nz,1,lusetd)
   ip1 = -8
   GOTO 1100
 400  CALL close(Usetd,1)
!
!     BUILD SIL TO SILD CONVERTER TABLE
!
   mskue = Two1(Iue)
   mskud = Two1(Iud)
   m = 1
   l = 0
   DO i = 1 , lusetd
      IF ( andf(Iz(i),mskue)==0 ) THEN
         l = l + 1
         IF ( andf(Iz(i),mskud)==0 ) THEN
            Iz(l) = 0
            CYCLE
         ELSE
            Iz(l) = m
         ENDIF
      ENDIF
      m = m + 1
   ENDDO
!
!     FIND STUFF IN GPTT
!
   its = intmp
   CALL makmcb(mcb,Iscr5,m-1,2,1)
   ns = 0
   file = Gptt
   CALL open(*1000,Gptt,Iz(ibuf2),0)
!
!     POSITION TO HEADER RECORD
!
   ival = nz - 2*l
   CALL read(*1200,*500,Gptt,Iz(l+1),ival,0,iflag)
   ip1 = -8
   GOTO 1100
!
!     PUT OUT TEMPS
!
!
!     DETERMINE NUMBER OF ELEMENT TEMP RECORDS TO SKIP.
!
 500  list = l + 3
   k = l + iflag
   DO
      nsk = Iz(k)
      IF ( nsk>0 ) EXIT
      k = k - 3
      IF ( k<=list ) EXIT
   ENDDO
!
!     SET IPOS TO SKIP ELEMENT TEMP RECORDS AND DUPLICATE HEADER.
!
   ipos = -nsk
   mcb(2) = 0
 600  IF ( its==0 ) GOTO 900
   k = list
   DO WHILE ( Iz(k)/=its )
      k = k + 3
      IF ( k>l+iflag ) CALL mesage(-31,its,name)
   ENDDO
!
!     FOUND TEMP SET
!
   tdflt = 0.0
   IF ( Iz(k+1)/=-1 ) tdflt = Z(k+1)
   m = l + iflag
   DO i = 1 , l
      j = m + i
      Z(j) = tdflt
   ENDDO
!
!     RECORD NUMBER OF TEMP SET FOUND
!
   ns = Iz(k+2)
   IF ( ns==0 ) GOTO 800
   DO
!
!     SKIP TO DESIRED RECORD
!
      IF ( ns<ipos ) THEN
         CALL bckrec(Gptt)
         ipos = ipos - 1
      ELSEIF ( ns==ipos ) THEN
         DO
            CALL read(*1200,*700,Gptt,A,2,0,iflg)
            IF ( Ia(1)>0 ) THEN
               j = Ia(1) + m
               Z(j) = A(2)
            ENDIF
         ENDDO
      ELSE
         CALL fwdrec(*1200,Gptt)
         ipos = ipos + 1
      ENDIF
   ENDDO
 700  ipos = ipos + 1
!
!     ALL SET UP OUTPUT
!
 800  inext = m + 1
   DO i = 1 , l
      j = m + i
      Ii = Iz(i) + m
      IF ( Ii/=m ) THEN
         IF ( Ii/=inext ) THEN
            DO k = inext , Ii
               Z(k) = 0.0
            ENDDO
         ENDIF
         Z(Ii) = Z(j)
         inext = Ii + 1
      ENDIF
   ENDDO
   j = inext - (m+1)
   CALL write(Iscr5,Z(m+1),j,0)
 900  CALL write(Iscr5,Z(1),0,1)
   mcb(2) = mcb(2) + 1
   IF ( mcb(2)==2 ) THEN
!
!     ALL DONE
!
      CALL close(Iscr5,1)
      CALL close(Gptt,1)
      CALL wrttrl(mcb)
      RETURN
   ELSE
      its = inltmp
      GOTO 600
   ENDIF
!
!     ERROR MESAGES
!
 1000 ip1 = -1
 1100 CALL mesage(ip1,file,name)
   RETURN
 1200 ip1 = -2
   GOTO 1100
 1300 CALL mesage(-31,itstep,name)
END SUBROUTINE trht1a
