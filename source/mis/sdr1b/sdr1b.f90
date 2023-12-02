!*==sdr1b.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr1b(Ipvect,Im1,Im2,Iout,Major,Sub1,Sub2,Iuset,Iopt,Iout1)
   USE c_packx
   USE c_parmeg
   USE c_patx
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ipvect
   INTEGER :: Im1
   INTEGER :: Im2
   INTEGER :: Iout
   INTEGER :: Major
   REAL :: Sub1
   REAL :: Sub2
   INTEGER :: Iuset
   INTEGER :: Iopt
   INTEGER :: Iout1
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: core , ipv1
   INTEGER :: i , ip1 , ip2 , iprec , itype , nload , noys
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Core(1),Kore(1))
   DATA name/4HSDR1 , 4HB   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         nz = korsz(core)
         nz1 = nz
         iuset1 = Iuset
         DO i = 2 , 7
            ia11(i) = 0
            ia12(i) = 0
            ia(i) = 0
         ENDDO
         ia11(1) = Im1
         IF ( Im1/=0 ) CALL rdtrl(ia11)
         ia12(1) = Im2
         CALL rdtrl(ia12)
         IF ( ia11(1)<0 .AND. ia12(1)<0 ) RETURN
         CALL calcv(Ipvect,Major,Sub1,Sub2,core)
         IF ( Iopt/=0 ) THEN
!
!     EXPAND YS
!
            nz = nz - sysbuf
            CALL open(*20,Im2,core(nz+1),0)
            nz = nz - sysbuf
            CALL open(*40,Iout1,core(nz+1),1)
            CALL fname(Im2,core)
            CALL write(Iout1,core,2,1)
            ia(1) = Im2
            CALL rdtrl(ia)
            noys = ia(2)
            ia(2) = 0
            ia(1) = Iout1
            ia(6) = 0
            ia(7) = 0
            CALL fwdrec(*20,Im2)
            nload = ia11(2)
            itu1 = ia(5)
            incr = 1
            itp1 = itu1
            itp2 = itp1
            incr1 = 1
            DO i = 1 , nload
               IF ( i<=noys ) THEN
                  iiu1 = 0
                  CALL unpack(*5,Im2,core)
                  iip1 = iiu1
                  jjp1 = jju1
               ENDIF
               CALL pack(core,Iout1,ia)
               CYCLE
 5             core(1) = 0
               core(2) = 0
               core(3) = 0
               core(4) = 0
               iip1 = 1
               jjp1 = 1
               CALL pack(core,Iout1,ia)
            ENDDO
            CALL close(Iout1,1)
            CALL close(Im2,1)
            CALL wrttrl(ia)
            ia12(1) = Iout1
            CALL rdtrl(ia12)
         ELSE
            IF ( ia12(1)<=0 ) ia12(1) = 0
         ENDIF
         ib11(1) = 0
         ib12(1) = 0
         ia(3) = nsub1 + nsub2 + nsub3
         ia(2) = max0(ia11(2),ia12(2))
         ia(4) = 2
         IF ( Im2==0 ) ia12(5) = ia11(5)
         iprec = min0(1-mod(ia11(5),2),1-mod(ia12(5),2))
         itype = 1
         IF ( ia11(5)>2 .OR. ia12(5)>2 ) itype = 3
         ia(5) = iprec + itype
         spag_nextblock_1 = 2
      CASE (2)
         irule = 0
         ia(1) = Iout
         ipv1(1) = Ipvect
         CALL rdtrl(ipv1)
         core(1) = 0
         core(2) = 1
         core(3) = ia(2)
         core(4) = 2
         core(5) = 1
         core(6) = 0
         core(7) = 0
         CALL merge(core,ipv1,core)
         CALL wrttrl(ia)
         RETURN
!
!
         ENTRY sdr1c(Ipvect,Im1,Iout)
!     =============================
!
!     EXPAND ROWS OF IM1 TO D SET SIZE
!
         DO i = 1 , 7
            ia12(i) = 0
            ib11(i) = 0
            ib12(i) = 0
         ENDDO
         ia11(1) = Im1
         CALL rdtrl(ia11)
         ia(1) = Im1
         CALL rdtrl(ia)
         ia(3) = nsub1 + nsub2 + nsub3
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     ERROR MESAGES
!
 20      ip1 = -1
         ip2 = Im2
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(ip1,ip2,name)
 40      ip1 = -1
         ip2 = Iout1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sdr1b
