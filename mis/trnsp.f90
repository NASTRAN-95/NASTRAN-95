
SUBROUTINE trnsp(Core)
   IMPLICIT NONE
   INTEGER Ia(2) , Iat(2) , Iforat , Iforma , Ii , Incr , Incr1 , Iotyp , Iotyp1 , Iotypa , Is1 , Itypa , Itypat , Jj , Ksys94 ,    &
         & Lcare , Mach , Namea , Nameat , Ncola , Ncolat , Nrow1 , Nrowa , Nrowat , Nscrh , Otpe , Scrth(8) , Sysbuf
   CHARACTER*25 Sfm , Uwm
   REAL Skip(91)
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /machin/ Mach
   COMMON /packx / Iotyp , Iotypa , Ii , Jj , Incr
   COMMON /system/ Sysbuf , Otpe , Skip , Ksys94
   COMMON /trnspx/ Namea , Ncola , Nrowa , Iforma , Itypa , Ia , Nameat , Ncolat , Nrowat , Iforat , Itypat , Iat , Lcare , Nscrh , &
                 & Scrth
   COMMON /unpakx/ Iotyp1 , Is1 , Nrow1 , Incr1
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   REAL Core(1)
   REAL a(2) , zero(4)
   INTEGER i , ibuf , ibuf1 , icol , iii , ij1 , ik , iloop , im1 , ioloop , iover , iparm(2) , iparm1 , j , k , km , l , last ,    &
         & lcore , m , m2 , n1 , name(2) , nbrut , ncalat , nl , nrem , nrow , nrow2 , nrowo , nscrth , ntype , trb1(7,8)
!
!     OUT-OF-CORE MATRIX TRANSPOSE USING 1 TO 8 SCRATCH FILES - NASTRAN
!     ORIGINAL ROUTINE.
!
!     (SEE TRANSP FOR IN-CORE MATRIX TRANSPOSE FOR UPPER TRIAG. MATRIX,
!      AND TRNSPS FOR OUT-OF-CORE MATRIX TRANSPOSE WITH 1 SCRATCH FILE,
!      A NASTRAN NEW ROUTINE)
!
!     REVERT TO NASTRAN ORIGINAL TRNSP IF DIAG 41 IS ON, OR 94TH WORD OF
!     /SYSTEM/ IS 1000. OTHERWISE SEND THE TRANSPOSE JOB TO THE NEW
!     TRNSPS ROUTINE, EXECPT LOWER AND UPPER TRIANGULAR MATRICES
!
   DATA iparm/4HTRAN , 4HPOSE/ , zero/4*0.0/
   DATA name/4HTRNS , 4HP   /
!
   IF ( Nscrh/=8 ) CALL conmsg(iparm,2,0)
   Iat(1) = 0
   Iat(2) = 0
   Incr1 = 1
   Ii = 1
   IF ( Itypat==0 ) Itypat = Itypa
   Iotyp = min0(Itypat,Itypa)
   Iotypa = Iotyp
   Iotyp1 = Iotyp
   IF ( Iforma/=4 .AND. Iforma/=5 ) THEN
!               LOWER            UPPER TRIANG. MATRICES
!
      j = mod(Ksys94,10000)/1000
      IF ( j/=1 ) THEN
         CALL sswtch(41,j)
         IF ( j/=1 ) THEN
!
!     NASTRAN MAINTENANCE WORK IS DONE ON VAX
!
            IF ( Mach==5 .AND. Iforma>=3 .AND. Iforma/=6 ) THEN
!               VAX      NOT SQUARE, RECTANG., AND SYMM.
               CALL fname(Namea,a)
               WRITE (4,99001) a , Iforma
99001          FORMAT (40X,'MATRIX ',2A4,', FORM =',I2,' ===>TRNSPS')
            ENDIF
            Ncolat = 0
            nscrth = 1
            CALL trnsps(Core,Core)
            GOTO 400
         ENDIF
      ENDIF
   ENDIF
!
   iparm1 = Namea
   nscrth = Nscrh
   im1 = 1
   ncalat = Ncolat
   Ncolat = 0
   ij1 = 0
   last = 1
   ntype = Iotypa
   IF ( ntype==3 ) ntype = 2
   lcore = Lcare
   ibuf1 = lcore - Sysbuf
   ibuf = ibuf1 - Sysbuf
   lcore = ibuf - 1
   IF ( lcore<=0 ) THEN
      n1 = -8
      CALL mesage(n1,iparm1,name)
      GOTO 400
   ELSE
!
!     COMMENT FROM G.CHAN/UNISYS    1/91
!     ABOUT THE SQUARE OR RECTANGULAR MATRIX TRANSPOSE BY THE VAX -
!     DATA, 1.0**-10 OR SMALLER, ON THE TRANSPOSED MATRIX MAY DIFFER
!     FROM THE ORIGINAL VALUES. CAN NOT EXPLAIN WHY.
!     THE NORMAL DATA, 1.0**+5 OR LARGER, ARE ALL OK.
!     (NO CHECK ON THE OTHER MACHINES)
!
      nrowo = min0(Nrowat,Ncola)
      nbrut = lcore/(nrowo*ntype)
      IF ( nbrut==0 ) THEN
         n1 = -8
         CALL mesage(n1,iparm1,name)
         GOTO 400
      ELSE
         nrem = nbrut
         IF ( nbrut>ncalat ) THEN
!
!     ONE PASS ONLY
!
            nscrth = 1
            Scrth(1) = Namea
            nbrut = ncalat
            k = 1
            ij1 = 1
            Iotyp = Itypa
         ELSE
            k = amax1(float(Nrowat)*sqrt(float(ntype)/float(lcore)),1.0)
         ENDIF
         nrow2 = nbrut*k
         nrow = min0(nscrth*nrow2,ncalat)
         km = (ncalat+nrow-1)/nrow
         icol = nbrut*ntype
         IF ( lcore<nrow*ntype+(nscrth-1)*Sysbuf ) THEN
            n1 = -8
            CALL mesage(n1,iparm1,name)
            GOTO 400
         ELSE
!
!     THERE ARE NROW2 ROWS IN EACH SUBMATRIX
!     WE GENERATE NROW ROWS PER PASS OF FULL MATRIX
!     THERE WILL BE KM SUCH PASSES
!
            ioloop = 1
         ENDIF
      ENDIF
   ENDIF
 100  IF ( ij1==0 ) THEN
      IF ( ioloop==km ) THEN
         iover = ncalat - (km-1)*nrow
         nbrut = min0(nbrut,iover)
         icol = nbrut*ntype
         nrow = iover
         nrow2 = min0(nbrut*k,nrow)
         k = (nrow2+nbrut-1)/nbrut
         nscrth = min0((iover+k*nbrut-1)/(k*nbrut),nscrth)
         IF ( nscrth==0 ) nscrth = 1
         Nrow1 = ncalat
      ELSE
         Nrow1 = nrow*ioloop
      ENDIF
      Is1 = Nrow1 - nrow + 1
      IF ( ioloop==1 ) THEN
         iparm1 = Namea
         CALL open(*200,Namea,Core(ibuf1),0)
      ENDIF
      CALL fwdrec(*300,Namea)
      nl = nrow*ntype
!
!     OPEN SCRATCHES
!
      j = ibuf
      DO i = 1 , nscrth
         iparm1 = Scrth(i)
         CALL open(*200,Scrth(i),Core(j),1)
         j = j - Sysbuf
         DO iii = 1 , 7
            trb1(iii,i) = 0
         ENDDO
      ENDDO
      DO iloop = 1 , nrowo
         CALL unpack(*140,Namea,Core)
 120     ik = 1
         Jj = nrow2
         Incr = 1
         DO i = 1 , nscrth
            CALL pack(Core(ik),Scrth(i),trb1(1,i))
            ik = ik + nrow2*ntype
         ENDDO
!
!     END LOOP ON BUILDING 1 COL OF SUB MATRICES
!
         CYCLE
!
 140     DO i = 1 , nl
            Core(i) = 0.0
         ENDDO
         GOTO 120
      ENDDO
      CALL rewind(Namea)
!
!     END LOOP ON BUILDING NSCRATH SUB MATRICES
!
      DO i = 1 , nscrth
         CALL close(Scrth(i),1)
      ENDDO
   ENDIF
   DO j = 1 , nscrth
      IF ( ij1==0 ) THEN
         IF ( ioloop==km .AND. j==nscrth ) last = 0
      ENDIF
      DO m = 1 , k
         iparm1 = Scrth(j)
         CALL open(*200,Scrth(j),Core(ibuf),0)
         IF ( last==1 .OR. ncalat-Ncolat>=nrem ) THEN
            IF ( ij1/=0 ) CALL fwdrec(*300,Scrth(j))
            Is1 = (m-1)*nbrut + 1
            Nrow1 = nbrut*m
         ELSE
            nbrut = ncalat - Ncolat
            icol = nbrut*ntype
            Is1 = (m-1)*nrem + 1
            Nrow1 = Is1 + nbrut
         ENDIF
         l = 1
         DO i = 1 , nrowo
            CALL unpack(*150,Scrth(j),Core(l))
            GOTO 160
 150        DO nl = 1 , icol
               m2 = nl + l - 1
               Core(m2) = 0.0
            ENDDO
 160        l = l + icol
         ENDDO
         CALL close(Scrth(j),1)
         iparm1 = Nameat
         CALL open(*200,Nameat,Core(ibuf),im1)
         IF ( im1/=3 ) THEN
            CALL fname(Nameat,a(1))
            CALL write(Nameat,a(1),2,1)
            im1 = 3
         ENDIF
         Incr = nbrut
         Jj = nrowo
         DO l = 1 , nbrut
            m2 = ntype*(l-1) + 1
            CALL pack(Core(m2),Nameat,Nameat)
         ENDDO
         CALL close(Nameat,2)
!
!     END LOOP ON SUBMATRIX
!
         IF ( Ncolat>=ncalat ) EXIT
      ENDDO
!
!     END LOOP ON EACH SCRATCH
!
   ENDDO
!
!     END LOOP ON EACH PASS THROUGH LARGE MATRIX
!
   ioloop = ioloop + 1
   IF ( ioloop<=km ) GOTO 100
   iparm1 = Nameat
   CALL open(*200,Nameat,Core(ibuf),3)
   CALL close(Nameat,1)
   CALL close(Namea,1)
   GOTO 400
!
!     ERROR MESSAGES
!
 200  n1 = -1
   CALL mesage(n1,iparm1,name)
   GOTO 400
 300  n1 = -2
   CALL mesage(n1,iparm1,name)
!
!     ONE FINAL CHECK BEFORE RETURN
!
 400  IF ( Iforma/=3 .AND. Iforma/=7 ) THEN
      IF ( Ncolat/=Nrowa .OR. Nrowat/=Ncola ) THEN
         CALL fname(Namea,a)
         WRITE (Otpe,99002) Swm , a , Iforma , Ncola , Nrowa , Iforat , Ncolat , Nrowat
99002    FORMAT (A27,' FORM TRNSP. TRANSPOSED MATRIX APPEARS IN ERROR',/5X,'ORIGINAL ',2A4,' - FORM =',I3,',  (',I6,' X',I6,')',/5X,&
                &'TRNASPOSED MATRIX - FORM =',I3,',  (',I6,' X',I6,')')
      ENDIF
   ENDIF
   IF ( Nscrh/=8 ) CALL conmsg(iparm,2,0)
END SUBROUTINE trnsp