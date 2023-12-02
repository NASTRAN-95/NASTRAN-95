!*==sma3.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sma3
USE C_BLANK
USE C_GENELY
USE C_SYSTEM
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(11) :: block
   REAL(REAL64) , DIMENSION(1) :: dq
   LOGICAL :: even , onlyge
   INTEGER :: i , idummy , if201 , if301 , if302 , if303 , if304 , if305 , if306 , ifg , iga , iggei , ii , ipass , iprec , iqmax , &
            & ise , isys , itemp1 , izk , m , n , needed , nz
   INTEGER , DIMENSION(11) :: iblock
   INTEGER , DIMENSION(1) :: iq
   INTEGER , DIMENSION(7) :: mcbid
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , factor , gopen , korsz , makmcb , mesage , rdtrl , read , sma3a , sma3b , sma3c , ssg2b , ssg2c , ssg3a ,       &
          & tranp1 , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE, FOR EACH GENERAL ELEMENT, READS THE GENERAL ELEMENT
!     INPUT FILE, GEI, CALLS SMA3A OR SMA3B, DEPENDING UPON WHETHER OR
!     NOT THE ORDERS OF THE K OR Z AND S MATRICES WILL ALLOW THE IN CORE
!     MATRIX ROUTINES (CALLED BY SMA3A) TO BE USED, AND THEN CALLS THE
!     MATRIX ADD ROUTINE TO ADD THE KGGX MATRIX TO THE GENERAL ELEMENT
!     MATRIX.
!
   !>>>>EQUIVALENCE (Ksystm(1),Isys) , (Ksystm(55),Iprec) , (Iq(1),Dq(1),Q(1)) , (Ibuff3(2),M) , (Ibuff3(3),N) , (Mcbid(1),Mcbc(1)) ,    &
!>>>>    & (block(1),iblock(1))
   DATA name/4HSMA3 , 4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     GENERAL INITIALIZATION
!
         Ifgei = 101
         Ifkggx = 102
         if201 = 201
         if301 = 301
         if302 = 302
         if303 = 303
         if304 = 304
         if305 = 305
         if306 = 306
         Ifout = if201
         Ifa = if301
         Ifb = if302
         Ifc = if303
         Ifd = if304
         Ife = if305
         Iff = if306
         ifg = 307
         Inrw = 0
         Outrw = 1
         Clsrw = 1
         Clsnrw = 2
         Eor = 1
         Neor = 0
!
!     DETERMINE THE SIZE OF VARIABLE CORE AVAILABLE AND SET IUI TO THE
!     ZEROTH LOCATION OF VARIABLE CORE.
!
         iqmax = korsz(Q)
         Iui = 0
!
!     OPEN THE GENERAL ELEMENT INPUT FILE AND SKIP OVER THE HEADER
!     RECORD.
!
         iggei = iqmax - isys + 1
         CALL gopen(Ifgei,Q(iggei),0)
         iga = iggei - isys
!
!     DETERMINE IF THE NUMBER OF GENERAL ELEMENTS IS EVEN OR ODD.
!
         even = .TRUE.
         IF ( (Ngenel/2)*2/=Ngenel ) even = .FALSE.
         ipass = 0
!
!     COMPUTE LENGTH OF OPEN CORE
!
         Left = iga - 1
         nz = Left
!
!     READ THE TRAILER FOR KGGX TO SEE IF IT EXISTS.
!
         onlyge = .FALSE.
         Mcbkgg(1) = Ifkggx
         CALL rdtrl(Mcbkgg(1))
         IF ( Mcbkgg(1)<0 ) THEN
            onlyge = .TRUE.
         ELSE
            Ifb = Mcbkgg(1)
            DO i = 1 , 7
               Mcbb(i) = Mcbkgg(i)
               Mcbkgg(i) = 0
            ENDDO
         ENDIF
!
!     INITIALIZATION PRIOR TO LOOP
!
         IF ( onlyge ) THEN
            Ifa = Ifout
            IF ( even ) Ifa = if302
         ELSE
            Ifout = if201
            IF ( even ) Ifout = if302
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     BEGIN MAIN LOOP OF THE PROGRAM
!
         ipass = ipass + 1
!
!     READ THE ELEMENT ID, THE LENGTH OF THE UI SET, M, AND THE LENGTH
!     OF THE UD SET, N
!
         CALL read(*20,*40,Ifgei,Ibuff3(1),3,Neor,idummy)
         needed = 2*(m+n+m**2+n**2+2*m*n)
         itemp1 = 2*(m+n+m**2) + 3*m
         IF ( itemp1>needed ) needed = itemp1
!
!     DETERMINE IF THERE IS ENOUGH CORE STORAGE AVAILABLE TO USE THE IN
!     CORE MATRIX ROUTINES.
!
         IF ( needed>Left ) THEN
!
!     ***********  OUT OF CORE VERSION  *************
!
!     IFOUT MUST CONTAIN THE RESULTS OF THE LAST GENEL PROCESSED
!     SWITCH FILES IFB AND IFOUT FOR OUT OF CORE VERSION
!
            IF ( .NOT.(ipass==1 .AND. onlyge .AND. .NOT.even) ) THEN
               DO i = 1 , 7
                  ii = Mcbkgg(i)
                  Mcbkgg(i) = Mcbb(i)
                  Mcbb(i) = ii
               ENDDO
               ii = Ifout
               Ifout = Ifb
               Ifb = ii
            ENDIF
!
!     THE IN CORE MATRIX ROUTINES CANNOT BE USED.SUBROUTINE SMA3B BUILDS
!     THE ZE IF Z IS INPUT OR THE ZINYS IF K IS INPUT AND IF PRESENT THE
!     SE MATRICES. IF THE SE MATRIX IS PRESENT ISE IS POSITIVE.
!     NOTE - SE(T) IS ON THE SE FILE.
!
            CALL sma3b(ise,izk)
            IF ( izk/=2 ) THEN
!
!     FACTOR DECOMPOSES THE ZE MATRIX INTO ITS UPPER AND LOWER
!     TRIANGULAR FACTORS.  TWO SCRATCH FILES ARE NEEDED.
!
               CALL factor(Ifa,Ife,Iff,Ifd,Ifc,ifg)
!
!     CONVERT IFB INTO THE IDENTITY MATRIX.  (MCBID HAS BEEN SET UP BY
!     SMA3B)
!
               CALL wrttrl(mcbid)
!
!     COMPUTE Z INVERSE
!
               CALL ssg3a(Ifa,Ife,Ifc,Ifd,0,0,-1,0)
            ENDIF
!
!     GO TO 150 IF NO SE MATRIX IS PRESENT.
!
            IF ( ise>=0 ) THEN
!
!               T        T  -1
!     COMPUTE -S XK OR -S XZ  AND STORE ON IFF
!               E  E     E  E
!
               CALL ssg2b(Ifb,Ifd,0,Iff,0,iprec,1,Ifc)
!
!     TRANSPOSE THE SE FILE ONTO IFA.  HENCE IFA CONTAINS THE -SE MATRIX
!
               CALL tranp1(Ifb,Ifa,1,Ifc,0,0,0,0,0,0,0)
!
!                       -1
!     COMPUTE K X-S OR Z  X-S AND STORE ON IFE
!              E   E    E    E
!
               CALL ssg2b(Ifd,Ifa,0,Ife,0,iprec,1,Ifc)
!
!              T          T  -1
!     COMPUTE S XK XS OR S XZ  XS AND STORE ON IFC
!              E  E  E    E  E   E
!
               CALL ssg2b(Ifb,Ife,0,Ifc,0,iprec,1,Ifa)
!
!     SMA3C BUILDS THE FINAL MATRIX OF G (LUSET) SIZE.
!
               Mcba(1) = Ifa
            ENDIF
            CALL sma3c(ise,Mcba)
!
!     RETURN FILES IFB AND IFOUT TO ORIGIONAL FILES AFTER OUT OF CORE
!
            IF ( .NOT.(ipass==1 .AND. onlyge .AND. .NOT.even) ) THEN
               DO i = 1 , 7
                  ii = Mcbkgg(i)
                  Mcbkgg(i) = Mcbb(i)
                  Mcbb(i) = ii
               ENDDO
               ii = Ifout
               Ifout = Ifb
!
!     RETURN TO SUMATION
!
               Ifb = ii
            ENDIF
         ELSE
!
!
!     **********  IN CORE VERSION  ****************
!
!     USE THE IN CORE MATRIX ROUTINES.  CALL SMA3A.
!
            CALL makmcb(Mcba,Ifa,0,6,iprec)
!
!     OPEN THE FILE ON WHICH THE CURRENT GENERAL ELEMENT WILL BE OUTPUT.
!
            CALL gopen(Ifa,Q(iga),1)
            CALL sma3a(Mcba)
!
!     STORE THE CORRECT NUMBER OF ROWS IN THE 3RD WORD OF THE MATRIX
!     CONTROL BLOCK AND CLOSE THE FILE WITH REWIND.
!
            Mcba(3) = Mcba(2)
            CALL wrttrl(Mcba)
            CALL close(Ifa,Clsrw)
         ENDIF
!
!     SUMATION
!
!     JUMP TO 100 ONLY IF THIS IS THE FIRST PASS AND KGGX DOES NOT EXIST
!
         IF ( .NOT.(ipass==1 .AND. onlyge) ) THEN
            CALL makmcb(Mcbkgg,Ifout,0,6,iprec)
            iblock(1) = 1
            block(2) = 1.0
            block(3) = 0.0
            block(4) = 0.0
            block(5) = 0.0
            block(6) = 0.0
            iblock(7) = 1
            block(8) = 1.0
            block(9) = 0.0
            block(10) = 0.0
            block(11) = 0.0
!
!     CLOSE GEI WITH NO REWIND SO SUBROUTINE ADD CAN HAVE THE BUFFER
!
            CALL close(Ifgei,2)
!
!     CALL SSG2C TO PERFORM SUMMATION - OUTPUT ON IFOUT
!
            CALL ssg2c(Ifa,Ifb,Ifout,0,block)
            IF ( ipass==Ngenel ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL rdtrl(Mcbkgg)
!
!     RESTORE GEI AFTER SUMATION
!
            CALL gopen(Ifgei,Q(iggei),2)
            IF ( ipass>1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( Ngenel==1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Ifa = if301
         Ifb = if302
         Ifout = if201
         IF ( even ) THEN
            Ifb = if201
            Ifout = if302
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     SWITCH FILES IFB AND IFOUT FOR NEXT GENEL PROCESSING
!
         DO i = 1 , 7
            ii = Mcbkgg(i)
            Mcbkgg(i) = Mcbb(i)
            Mcbb(i) = ii
         ENDDO
         ii = Ifout
         Ifout = Ifb
!
!     RETURN TO BEGIN LOOP
!
         Ifb = ii
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!     WRAP-UP
!
         CALL close(Ifgei,Clsrw)
         IF ( Ifout/=if201 ) CALL mesage(-30,28,5)
         RETURN
!
!     FATAL ERROR MESSAGES
!
 20      CALL mesage(-2,Ifgei,name)
 40      CALL mesage(-3,Ifgei,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sma3
