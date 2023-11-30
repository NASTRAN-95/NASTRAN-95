
SUBROUTINE com12(*,Ix,X,Dx,Itermm)
   IMPLICIT NONE
   INTEGER Cdp , Dum(3) , Ifila(7) , Ifill(7) , Ifilu(7) , Incrx , Incry , Itype1 , Itype2 , Itypex , Ixy , Iy , Jj , Jxy , Jy ,    &
         & Ncol , Norew , Nx , Rdp , Sr2fil , Sysbuf , Typel
   REAL Csp , Eofnrw , Power , Rd , Rdrew , Rew , Rsp , Wrt , Wrtrew
   DOUBLE PRECISION Det(2) , Dz(2) , Mindia
   COMMON /cdcmpx/ Ifila , Ifill , Ifilu , Dum , Det , Power , Nx , Mindia
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp
   COMMON /packx / Itype1 , Itype2 , Iy , Jy , Incry
   COMMON /system/ Sysbuf
   COMMON /unpakx/ Itypex , Ixy , Jxy , Incrx
   COMMON /zblpkx/ Dz , Jj
   INTEGER Bbar , Bbbar , Bbbar1 , Cbcnt , I1 , I1sp , I4 , I4sp , I6sp , Ipak , Iterm , Itermm , Jposl , Lcol , R , Scrflg , Sr2fl
   DOUBLE PRECISION Dx(12)
   INTEGER Ix(1)
   REAL X(1)
   REAL a , sub(2)
   DOUBLE PRECISION da
   INTEGER i , ibbar4 , ibuf1 , ibuf2 , ibuf3 , iend , ifile , in1 , in2 , j , k , kk , l , ll , name , no
!
!*******
!      PROGRAM TO SOLVE A MATRIX OF ORDER ONE OR TWO FOR CDCOMP
!*******
   EQUIVALENCE (Ifila(2),Ncol) , (Ifill(5),Typel) , (Sr2fil,Dum(2))
   DATA sub(1) , sub(2)/4HCOM1 , 4H2   /
!
   ibuf1 = Nx - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   CALL close(Sr2fil,Rew)
   ibuf3 = ibuf2 - Sysbuf
   ifile = Ifilu(1)
   IF ( Itermm==1 ) ifile = Sr2fil
   CALL gopen(ifile,Ix(ibuf3),Wrtrew)
   CALL gopen(Ifila(1),Ix(ibuf1),Rdrew)
   Itypex = Cdp
   Itype1 = Cdp
   Itype2 = Typel
   Incrx = 1
   Incry = 1
   IF ( Ncol==2 ) THEN
      Ixy = 1
!*******
!     SOLVE A (2X2)
!*******
      Jxy = 2
      CALL unpack(*600,Ifila(1),Dx)
      CALL unpack(*600,Ifila(1),Dx(5))
      a = 1.
      IF ( Dx(1)**2+Dx(2)**2<Dx(3)**2+Dx(4)**2 ) THEN
!*******
!     PERFORM INTERCHANGE
!*******
         Det(1) = Dx(1)
         Dx(1) = Dx(3)
         Dx(3) = Det(1)
         Det(1) = Dx(2)
         Dx(2) = Dx(4)
         Dx(4) = Det(1)
         Det(1) = Dx(5)
         Dx(5) = Dx(7)
         Dx(7) = Det(1)
         Det(1) = Dx(6)
         Dx(6) = Dx(8)
         Dx(8) = Det(1)
         a = -1.
      ENDIF
      Det(1) = (Dx(3)*Dx(1)+Dx(4)*Dx(2))/(Dx(1)**2+Dx(2)**2)
      Dx(4) = (Dx(4)*Dx(1)-Dx(3)*Dx(2))/(Dx(1)**2+Dx(2)**2)
      Dx(3) = Det(1)
      Dx(7) = Dx(7) - Dx(3)*Dx(5) + Dx(4)*Dx(6)
      Dx(8) = Dx(8) - Dx(3)*Dx(6) - Dx(4)*Dx(5)
      Det(1) = Dx(1)*Dx(7) - Dx(2)*Dx(8)
      Det(2) = Dx(2)*Dx(7) + Dx(1)*Dx(8)
      IF ( (Dx(1)==0.D0 .AND. Dx(2)==0.D0) .OR. (Dx(7)==0.D0 .AND. Dx(8)==0.D0) ) GOTO 600
      Mindia = dmin1(dsqrt(Dx(1)**2+Dx(2)**2),dsqrt(Dx(7)**2+Dx(8)**2))
      Iy = 1
      Jy = 2
      Dx(9) = 0.0D0
      Dx(10) = 0.0D0
      IF ( a<0. ) Dx(9) = 1.D0
      Dx(11) = Dx(3)
      Dx(12) = Dx(4)
      CALL pack(Dx(9),Ifill(1),Ifill)
      Dx(9) = 0.D0
      Dx(10) = 0.D0
      Jy = 1
      CALL pack(Dx(9),Ifill(1),Ifill)
      IF ( Itermm==1 ) THEN
         CALL pack(Dx,ifile,Ifilu)
         Jy = 2
         CALL pack(Dx(5),ifile,Ifilu)
         CALL close(ifile,Eofnrw)
      ELSE
         Dx(3) = Dx(5)
         Dx(5) = Dx(7)
         Dx(7) = Dx(3)
         Dx(3) = Dx(6)
         Dx(6) = Dx(8)
         Dx(8) = Dx(3)
         Jy = 2
         CALL pack(Dx(5),Ifilu(1),Ifilu)
         Iy = 2
         CALL pack(Dx,Ifilu(1),Ifilu)
         CALL close(ifile,Rew)
      ENDIF
   ELSEIF ( Ncol/=1 ) THEN
      no = -8
      CALL mesage(no,name,sub(1))
      GOTO 99999
   ELSE
!*******
!     SOLVE A (1X1)
!*******
      Ixy = 1
      Jxy = 1
      CALL unpack(*600,Ifila(1),Dx)
      Det(1) = Dx(1)
      Det(2) = Dx(2)
      Mindia = dsqrt(Dx(1)**2+Dx(2)**2)
      Iy = 1
      Jy = 1
      CALL pack(Dx,ifile,Ifilu)
      Dx(1) = 0.D0
      Dx(2) = 0.D0
      CALL pack(Dx,Ifill(1),Ifill)
      CALL close(ifile,Rew)
   ENDIF
   CALL close(Ifila(1),Rew)
   CALL close(Ifill(1),Rew)
   RETURN
!
!
   ENTRY comfin(Iterm,Scrflg,Sr2fl,Jposl,I1sp,Bbar,I1,Cbcnt,Ipak,R,Bbbar1,Bbbar,I6sp,I4,I4sp,Ix,Dx,X,Lcol)
!
   ibuf1 = Nx - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   CALL close(Ifila(1),Rew)
   CALL open(*300,Sr2fil,Ix(ibuf1),Wrt)
   CALL close(Sr2fil,Eofnrw)
   k = 0
   name = Ifill(1)
   CALL open(*300,Ifill(1),Ix(ibuf2),Wrt)
   IF ( Scrflg/=0 ) THEN
      name = Sr2fl
      CALL open(*300,Sr2fl,Ix(ibuf3),Rd)
   ENDIF
   ll = 0
 100  Jposl = Jposl + 1
   CALL bldpk(Cdp,Typel,Ifill(1),0,0)
   in1 = I1sp + k
   Jj = Jposl
   Dz(1) = Ix(in1)
   Dz(2) = 0.D0
   CALL zblpki
   kk = 0
   iend = min0(Bbar,Ncol-Jj)
   IF ( iend/=0 ) THEN
      in1 = I1 + ll*Bbar*2
      DO
         Jj = Jj + 1
         in2 = in1 + kk + kk
         Dz(1) = Dx(in2)
         Dz(2) = Dx(in2+1)
         CALL zblpki
         kk = kk + 1
         IF ( kk<iend ) THEN
         ELSEIF ( kk==iend ) THEN
            EXIT
         ELSE
            no = -25
            CALL mesage(no,name,sub(1))
            GOTO 99999
         ENDIF
      ENDDO
   ENDIF
   IF ( Cbcnt==0 ) GOTO 200
!*******
!     PACK ACTIVE ROW ELEMENTS ALSO
!*******
   kk = 0
   DO
      in1 = I6sp + kk
      in2 = I4 + (Ix(in1)*Bbbar+k)*2
      Dz(1) = Dx(in2)
      Dz(2) = Dx(in2+1)
      IF ( Dz(1)/=0.D0 .OR. Dz(2)/=0.D0 ) THEN
         in1 = I4sp + Ix(in1)
         Jj = Ix(in1)
         CALL zblpki
      ENDIF
      kk = kk + 1
      IF ( kk>=Cbcnt ) EXIT
   ENDDO
 200  CALL bldpkn(Ifill(1),0,Ifill)
   ll = ll + 1
   k = k + 1
   IF ( k==Lcol ) THEN
      CALL close(Ifill(1),Rew)
      IF ( Scrflg>0 ) CALL close(Sr2fl,Rew)
      IF ( Iterm/=0 ) RETURN
!*******
!     RE-WRITE THE UPPER TRIANGLE WITH THE RECORDS IN THE REVERSE ORDER
!*******
      Incrx = 1
      Incry = 1
      Itype1 = Typel
      Itype2 = Typel
      Itypex = Typel
      Ifilu(2) = 0
      Ifilu(6) = 0
      Ifilu(7) = 0
      name = Sr2fil
      CALL open(*300,Sr2fil,Ix(ibuf1),Rd)
      CALL gopen(Ifilu(1),Ix(ibuf2),Wrtrew)
      DO i = 1 , Ncol
         Ixy = 0
         CALL bckrec(Sr2fil)
         CALL unpack(*600,Sr2fil,Ix)
         CALL bckrec(Sr2fil)
         kk = Jxy - Ixy + 1
         k = kk/2
         kk = kk + 1
         IF ( Typel==1 ) THEN
            DO j = 1 , k
               l = kk - j
               a = X(j)
               X(j) = X(l)
               X(l) = a
            ENDDO
         ELSEIF ( Typel==4 ) THEN
            kk = kk + kk - 1
            k = k + k
            DO j = 1 , k , 2
               l = kk - j - 1
               da = Dx(l)
               Dx(l) = Dx(j)
               Dx(j) = da
               da = Dx(l+1)
               Dx(l+1) = Dx(j+1)
               Dx(j+1) = da
            ENDDO
         ELSE
            DO j = 1 , k
               l = kk - j
               da = Dx(j)
               Dx(j) = Dx(l)
               Dx(l) = da
            ENDDO
         ENDIF
         Iy = Ncol - Jxy + 1
         Jy = Ncol - Ixy + 1
         CALL pack(Ix,Ifilu(1),Ifilu)
      ENDDO
      CALL close(Ifilu(1),Rew)
      CALL close(Sr2fil,Rew)
      RETURN
   ELSE
      IF ( k-R+1<0 ) GOTO 100
      IF ( k-R+1==0 ) THEN
         IF ( R<Bbbar1 ) THEN
         ELSEIF ( R==Bbbar1 ) THEN
            GOTO 100
         ELSE
            no = -25
            CALL mesage(no,name,sub(1))
            GOTO 99999
         ENDIF
      ENDIF
      ll = ll - 1
      in1 = I1 + ll*Bbar*2
      ibbar4 = 4*Bbar
      CALL read(*400,*500,Sr2fl,Dx(in1),ibbar4,0,no)
      GOTO 100
   ENDIF
 300  no = -1
   CALL mesage(no,name,sub(1))
   GOTO 99999
 400  no = -2
   CALL mesage(no,name,sub(1))
   GOTO 99999
 500  no = -3
   CALL mesage(no,name,sub(1))
   GOTO 99999
 600  RETURN 1
99999 RETURN
END SUBROUTINE com12
