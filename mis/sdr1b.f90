
SUBROUTINE sdr1b(Ipvect,Im1,Im2,Iout,Major,Sub1,Sub2,Iuset,Iopt,Iout1)
   IMPLICIT NONE
   INTEGER Core(7) , Ia(7) , Ia11(7) , Ia12(7) , Ib11(7) , Ib12(7) , Iip1 , Iiu1 , Incr , Incr1 , Irule , Itp1 , Itp2 , Itu1 ,      &
         & Iuset1 , Jjp1 , Jju1 , Kore(1) , Nsub1 , Nsub2 , Nsub3 , Nz , Nz1 , Sysbuf
   COMMON /packx / Itp1 , Itp2 , Iip1 , Jjp1 , Incr
   COMMON /parmeg/ Ia , Ia11 , Ia12 , Ib11 , Ib12 , Nz1 , Irule
   COMMON /patx  / Nz , Nsub1 , Nsub2 , Nsub3 , Iuset1
   COMMON /system/ Sysbuf
   COMMON /unpakx/ Itu1 , Iiu1 , Jju1 , Incr1
   COMMON /zzzzzz/ Kore
   INTEGER Im1 , Im2 , Iopt , Iout , Iout1 , Ipvect , Iuset , Major
   REAL Sub1 , Sub2
   INTEGER i , ip1 , ip2 , iprec , ipv1(7) , itype , name(2) , nload , noys
   INTEGER korsz
!
   !>>>>EQUIVALENCE (Core(1),Kore(1))
   DATA name/4HSDR1 , 4HB   /
!
!
   Nz = korsz(Core)
   Nz1 = Nz
   Iuset1 = Iuset
   DO i = 2 , 7
      Ia11(i) = 0
      Ia12(i) = 0
      Ia(i) = 0
   ENDDO
   Ia11(1) = Im1
   IF ( Im1/=0 ) CALL rdtrl(Ia11)
   Ia12(1) = Im2
   CALL rdtrl(Ia12)
   IF ( Ia11(1)<0 .AND. Ia12(1)<0 ) RETURN
   CALL calcv(Ipvect,Major,Sub1,Sub2,Core)
   IF ( Iopt/=0 ) THEN
!
!     EXPAND YS
!
      Nz = Nz - Sysbuf
      CALL open(*200,Im2,Core(Nz+1),0)
      Nz = Nz - Sysbuf
      CALL open(*400,Iout1,Core(Nz+1),1)
      CALL fname(Im2,Core)
      CALL write(Iout1,Core,2,1)
      Ia(1) = Im2
      CALL rdtrl(Ia)
      noys = Ia(2)
      Ia(2) = 0
      Ia(1) = Iout1
      Ia(6) = 0
      Ia(7) = 0
      CALL fwdrec(*200,Im2)
      nload = Ia11(2)
      Itu1 = Ia(5)
      Incr = 1
      Itp1 = Itu1
      Itp2 = Itp1
      Incr1 = 1
      DO i = 1 , nload
         IF ( i<=noys ) THEN
            Iiu1 = 0
            CALL unpack(*20,Im2,Core)
            Iip1 = Iiu1
            Jjp1 = Jju1
         ENDIF
         CALL pack(Core,Iout1,Ia)
         CYCLE
 20      Core(1) = 0
         Core(2) = 0
         Core(3) = 0
         Core(4) = 0
         Iip1 = 1
         Jjp1 = 1
         CALL pack(Core,Iout1,Ia)
      ENDDO
      CALL close(Iout1,1)
      CALL close(Im2,1)
      CALL wrttrl(Ia)
      Ia12(1) = Iout1
      CALL rdtrl(Ia12)
   ELSE
      IF ( Ia12(1)<=0 ) Ia12(1) = 0
   ENDIF
   Ib11(1) = 0
   Ib12(1) = 0
   Ia(3) = Nsub1 + Nsub2 + Nsub3
   Ia(2) = max0(Ia11(2),Ia12(2))
   Ia(4) = 2
   IF ( Im2==0 ) Ia12(5) = Ia11(5)
   iprec = min0(1-mod(Ia11(5),2),1-mod(Ia12(5),2))
   itype = 1
   IF ( Ia11(5)>2 .OR. Ia12(5)>2 ) itype = 3
   Ia(5) = iprec + itype
 100  Irule = 0
   Ia(1) = Iout
   ipv1(1) = Ipvect
   CALL rdtrl(ipv1)
   Core(1) = 0
   Core(2) = 1
   Core(3) = Ia(2)
   Core(4) = 2
   Core(5) = 1
   Core(6) = 0
   Core(7) = 0
   CALL merge(Core,ipv1,Core)
   CALL wrttrl(Ia)
   RETURN
!
!
   ENTRY sdr1c(Ipvect,Im1,Iout)
!     =============================
!
!     EXPAND ROWS OF IM1 TO D SET SIZE
!
   DO i = 1 , 7
      Ia12(i) = 0
      Ib11(i) = 0
      Ib12(i) = 0
   ENDDO
   Ia11(1) = Im1
   CALL rdtrl(Ia11)
   Ia(1) = Im1
   CALL rdtrl(Ia)
   Ia(3) = Nsub1 + Nsub2 + Nsub3
   GOTO 100
!
!     ERROR MESAGES
!
 200  ip1 = -1
   ip2 = Im2
 300  CALL mesage(ip1,ip2,name)
 400  ip1 = -1
   ip2 = Iout1
   GOTO 300
END SUBROUTINE sdr1b