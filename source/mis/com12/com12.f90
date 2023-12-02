!*==com12.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE com12(*,Ix,X,Dx,Itermm)
   USE c_cdcmpx
   USE c_names
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_zblpkx
   USE C_CDCMPX
   USE C_NAMES
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZBLPKX
   IMPLICIT NONE
   INTEGER Cdp , Dum(3) , Ifila(7) , Ifill(7) , Ifilu(7) , Incrx , Incry , Itype1 , Itype2 , Itypex , Ixy , Iy , Jj , Jxy , Jy ,    &
         & ncol , Norew , Nx , Rdp , sr2fil , Sysbuf , typel
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
   INTEGER :: spag_nextblock_1
!
!*******
!      PROGRAM TO SOLVE A MATRIX OF ORDER ONE OR TWO FOR CDCOMP
!*******
   !>>>>EQUIVALENCE (Ifila(2),Ncol) , (Ifill(5),Typel) , (Sr2fil,Dum(2))
   DATA sub(1) , sub(2)/4HCOM1 , 4H2   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ibuf1 = Nx - Sysbuf
         ibuf2 = ibuf1 - Sysbuf
         CALL close(sr2fil,Rew)
         ibuf3 = ibuf2 - Sysbuf
         ifile = Ifilu(1)
         IF ( Itermm==1 ) ifile = sr2fil
         CALL gopen(ifile,Ix(ibuf3),Wrtrew)
         CALL gopen(Ifila(1),Ix(ibuf1),Rdrew)
         Itypex = Cdp
         Itype1 = Cdp
         Itype2 = typel
         Incrx = 1
         Incry = 1
         IF ( ncol==2 ) THEN
            Ixy = 1
!*******
!     SOLVE A (2X2)
!*******
            Jxy = 2
            CALL unpack(*80,Ifila(1),Dx)
            CALL unpack(*80,Ifila(1),Dx(5))
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
            IF ( (Dx(1)==0.D0 .AND. Dx(2)==0.D0) .OR. (Dx(7)==0.D0 .AND. Dx(8)==0.D0) ) GOTO 80
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
         ELSEIF ( ncol/=1 ) THEN
            no = -8
            CALL mesage(no,name,sub(1))
            RETURN
         ELSE
!*******
!     SOLVE A (1X1)
!*******
            Ixy = 1
            Jxy = 1
            CALL unpack(*80,Ifila(1),Dx)
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
         CALL open(*20,sr2fil,Ix(ibuf1),Wrt)
         CALL close(sr2fil,Eofnrw)
         k = 0
         name = Ifill(1)
         CALL open(*20,Ifill(1),Ix(ibuf2),Wrt)
         IF ( Scrflg/=0 ) THEN
            name = Sr2fl
            CALL open(*20,Sr2fl,Ix(ibuf3),Rd)
         ENDIF
         ll = 0
         spag_nextblock_1 = 2
      CASE (2)
         Jposl = Jposl + 1
         CALL bldpk(Cdp,typel,Ifill(1),0,0)
         in1 = I1sp + k
         Jj = Jposl
         Dz(1) = Ix(in1)
         Dz(2) = 0.D0
         CALL zblpki
         kk = 0
         iend = min0(Bbar,ncol-Jj)
         IF ( iend/=0 ) THEN
            in1 = I1 + ll*Bbar*2
            SPAG_Loop_1_1: DO
               Jj = Jj + 1
               in2 = in1 + kk + kk
               Dz(1) = Dx(in2)
               Dz(2) = Dx(in2+1)
               CALL zblpki
               kk = kk + 1
               IF ( kk<iend ) THEN
               ELSEIF ( kk==iend ) THEN
                  EXIT SPAG_Loop_1_1
               ELSE
                  no = -25
                  CALL mesage(no,name,sub(1))
                  RETURN
               ENDIF
            ENDDO SPAG_Loop_1_1
         ENDIF
         IF ( Cbcnt/=0 ) THEN
!*******
!     PACK ACTIVE ROW ELEMENTS ALSO
!*******
            kk = 0
            SPAG_Loop_1_2: DO
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
               IF ( kk>=Cbcnt ) EXIT SPAG_Loop_1_2
            ENDDO SPAG_Loop_1_2
         ENDIF
         CALL bldpkn(Ifill(1),0,Ifill)
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
            Itype1 = typel
            Itype2 = typel
            Itypex = typel
            Ifilu(2) = 0
            Ifilu(6) = 0
            Ifilu(7) = 0
            name = sr2fil
            CALL open(*20,sr2fil,Ix(ibuf1),Rd)
            CALL gopen(Ifilu(1),Ix(ibuf2),Wrtrew)
            DO i = 1 , ncol
               Ixy = 0
               CALL bckrec(sr2fil)
               CALL unpack(*80,sr2fil,Ix)
               CALL bckrec(sr2fil)
               kk = Jxy - Ixy + 1
               k = kk/2
               kk = kk + 1
               IF ( typel==1 ) THEN
                  DO j = 1 , k
                     l = kk - j
                     a = X(j)
                     X(j) = X(l)
                     X(l) = a
                  ENDDO
               ELSEIF ( typel==4 ) THEN
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
               Iy = ncol - Jxy + 1
               Jy = ncol - Ixy + 1
               CALL pack(Ix,Ifilu(1),Ifilu)
            ENDDO
            CALL close(Ifilu(1),Rew)
            CALL close(sr2fil,Rew)
            RETURN
         ELSE
            IF ( k-R+1>=0 ) THEN
               IF ( k-R+1==0 ) THEN
                  IF ( R<Bbbar1 ) THEN
                  ELSEIF ( R==Bbbar1 ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     no = -25
                     CALL mesage(no,name,sub(1))
                     RETURN
                  ENDIF
               ENDIF
               ll = ll - 1
               in1 = I1 + ll*Bbar*2
               ibbar4 = 4*Bbar
               CALL read(*40,*60,Sr2fl,Dx(in1),ibbar4,0,no)
            ENDIF
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      no = -1
         CALL mesage(no,name,sub(1))
         RETURN
 40      no = -2
         CALL mesage(no,name,sub(1))
         RETURN
 60      no = -3
         CALL mesage(no,name,sub(1))
         RETURN
 80      RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE com12
