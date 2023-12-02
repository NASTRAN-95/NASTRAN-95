!*==ssg3a.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssg3a(A,Lll,B,X,Sr1,Sr2,Itr1,Res)
   USE c_blank
   USE c_fbsx
   USE c_system
   USE c_unpakx
   USE c_zntpkx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: A
   INTEGER :: Lll
   INTEGER :: B
   INTEGER :: X
   INTEGER :: Sr1
   INTEGER :: Sr2
   INTEGER :: Itr1
   INTEGER :: Res
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: dcore
   REAL(REAL64) :: dnom , dnum
   REAL :: epsi
   INTEGER :: ipm , iprec , l , nlen , nload , sysbuf
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , fbs , fwdrec , gopen , intpk , korsz , mesage , rdtrl , ssg2b , unpack , wrttrl , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     SSG3A SOLVES AX = B USING A = L*LT
!
!     ON OPTION COMPUTES RESIDUAL VECTOR RES = A*X - B
!     AND EPSI= X(T)*RES/B(T)*X
!
   !>>>>EQUIVALENCE (Core(1),Dcore(1)) , (Ksystm(1),Sysbuf) , (Ksystm(55),Iprec)
   DATA name/4HSSG3 , 4HA   /
!
   fill(1) = Lll
   CALL rdtrl(fill)
   IF ( fill(1)<=0 ) CALL mesage(-1,Lll,name)
   filb(1) = B
   CALL rdtrl(filb)
   nload = filb(2)
   nlen = filb(3)
   isign = 1
   prec = 2
   nz = korsz(core)
   DO i = 2 , 7
      filx(i) = filb(i)
   ENDDO
   filx(1) = X
!
!     SAVE DISPLACEMENT VECTOR IN DOUBLE PRECISION
!
   filx(5) = 1
   IF ( filb(5)>2 ) filx(5) = 3
   filx(5) = filx(5) + iprec - 1
   CALL fbs(core,core)
   CALL wrttrl(filx)
   IF ( Itr1>=0 ) THEN
      fill(1) = Res
      CALL rdtrl(fill)
      IF ( fill(1)>0 ) THEN
!
!     COMPUTE RESIDUAL VECTOR
!
         CALL ssg2b(A,X,B,Res,0,2,-2,Sr1)
!
!     COMPUTE EPSI
!
         nz = nz - sysbuf
         CALL gopen(X,core(nz+1),0)
         nz = nz - sysbuf
         CALL gopen(Res,core(nz+1),0)
         nz = nz - sysbuf
         CALL gopen(B,core(nz+1),0)
         IF ( nz<2*nlen ) THEN
            CALL mesage(-8,0,name)
            RETURN
         ELSE
            itb = 2
            incur = 1
            i = 1
            j = nlen
            DO l = 1 , nload
               spag_nextblock_1 = 1
               SPAG_DispatchLoop_1: DO
                  SELECT CASE (spag_nextblock_1)
                  CASE (1)
                     CALL unpack(*2,X,core)
                     dnum = 0.0D0
                     dnom = 0.0D0
                     CALL intpk(*4,Res,0,2,0)
                     DO WHILE ( ieol==0 )
                        CALL zntpki
                        dnum = dnum + dx(1)*dcore(ik)
                     ENDDO
                     CALL intpk(*6,B,0,2,0)
                     DO WHILE ( ieol==0 )
                        CALL zntpki
                        dnom = dnom + dx(1)*dcore(ik)
                     ENDDO
                     epsi = dnum/dnom
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
 2                   CALL fwdrec(*100,Res)
 4                   CALL fwdrec(*200,B)
 6                   epsi = 0.0
                     spag_nextblock_1 = 2
                  CASE (2)
                     CALL mesage(35,nskip+l-1,epsi)
                     IF ( abs(epsi)>=1.0E-3 ) THEN
                        iepsi = -1
                        CALL mesage(58,1.0E-3,nskip+l-1)
                     ENDIF
                     EXIT SPAG_DispatchLoop_1
                  END SELECT
               ENDDO SPAG_DispatchLoop_1
            ENDDO
            CALL close(X,1)
            CALL close(Res,1)
            CALL close(B,1)
         ENDIF
      ENDIF
   ENDIF
   RETURN
 100  DO
      ipm = Res
!
      CALL mesage(-1,ipm,name)
   ENDDO
 200  ipm = B
   CALL mesage(-1,ipm,name)
   GOTO 100
!
END SUBROUTINE ssg3a
