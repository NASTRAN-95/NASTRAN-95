
SUBROUTINE piklvl(*,Lvls1,Lvls2,Ccstor,Idflt,Isdir,Xc,Nhigh,Nlow,Nacum,Size,Stpt)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Idpth , Idum
   COMMON /bandg / Idum , Idpth
!
! Dummy argument declarations
!
   INTEGER Idflt , Isdir , Xc
   INTEGER Ccstor(1) , Lvls1(1) , Lvls2(1) , Nacum(1) , Nhigh(1) , Nlow(1) , Size(1) , Stpt(1)
!
! Local variable declarations
!
   INTEGER end , i , inode , it , j , k , lvlnh , lvlnl , m , max1 , max2 , n , temp
!
! End of declarations
!
!
!
!     THIS ROUTINE IS USED ONLY BY GIBSTK OF BANDIT MODULE
!
!     PIKLVL CHOOSES THE LEVEL STRUCTURE  USED IN NUMBERING GRAPH
!
!     LVLS1-    ON INPUT CONTAINS FORWARD LEVELING INFO
!     LVLS2-    ON INPUT CONTAINS REVERSE LEVELING INFO
!               ON OUTPUT THE FINAL LEVEL STRUCTURE CHOSEN
!     CCSTOR-   ON INPUT CONTAINS CONNECTED COMPONENT INFO
!     IDFLT-    ON INPUT =1 IF WDTH LVLS1'WDTH LVLS2, =2 OTHERWISE
!     NHIGH     KEEPS TRACK OF LEVEL WIDTHS FOR HIGH NUMBERING
!               DIMENSION OF NHIGH IS MAXIMUM ALLOWABLE NUMBER OF LEVELS
!     NLOW-     KEEPS TRACK OF LEVEL WIDTHS FOR LOW NUMBERING
!     NACUM-    KEEPS TRACK OF LEVEL WIDTHS FOR CHOSEN LEVEL STRUCTURE
!     XC-       NUMBER OF MAXIMUM ALLOWABLE CONNECTED COMPONENTS
!               (IS THE DIMENSION FOR SIZE AND STPT)
!     SIZE(I)-  SIZE OF ITH CONNECTED COMPONENT
!     STPT(I)-  INDEX INTO CCSTORE OF 1ST NODE IN ITH CON COMPT
!     ISDIR-    FLAG WHICH INDICATES WHICH WAY THE LARGEST CONNECTED
!               COMPONENT FELL.  =+1 IF LOW AND -1 IF HIGH
!
!
!     PART 1 -
!     ========
!     SORTS SIZE AND STPT HERE, IN DECENDING ORDER
!     (PREVIOUS SORT2 ROUTINE IS NOW MOVED INTO HERE.
!     THE ORIGINAL BUBBLE SORT HAS BEEN REPLACED BY THE MODIFIED SHELL
!     SORT WHICH IS MUCH FASTER   /G.CHAN,  MAY 1988)
!
   IF ( Xc==0 ) RETURN 1
   m = Xc
 100  m = m/2
   IF ( m==0 ) THEN
!
!
!     PART 2 -
!     ========
!     CHOOSES THE LEVEL STRUCTURE USED IN NUMBERING GRAPH
!
!
!     FOR EACH CONNECTED COMPONENT DO
!
      DO i = 1 , Xc
         j = Stpt(i)
         end = Size(i) + j - 1
!
!     SET NHIGH AND NLOW EQUAL TO NACUM
!
         DO k = 1 , Idpth
            Nhigh(k) = Nacum(k)
            Nlow(k) = Nacum(k)
         ENDDO
!
!     UPDATE NHIGH AND NLOW FOR EACH NODE IN CONNECTED COMPONENT
!
         DO k = j , end
            inode = Ccstor(k)
            lvlnh = Lvls1(inode)
            Nhigh(lvlnh) = Nhigh(lvlnh) + 1
            lvlnl = Lvls2(inode)
            Nlow(lvlnl) = Nlow(lvlnl) + 1
         ENDDO
         max1 = 0
         max2 = 0
!
!     SET MAX1=LARGEST NEW NUMBER IN NHIGH
!     SET MAX2=LARGEST NEW NUMBER IN NLOW
!
         DO k = 1 , Idpth
            IF ( 2*Nacum(k)/=Nlow(k)+Nhigh(k) ) THEN
               IF ( Nhigh(k)>max1 ) max1 = Nhigh(k)
               IF ( Nlow(k)>max2 ) max2 = Nlow(k)
            ENDIF
         ENDDO
!
!     SET IT= NUMBER OF LEVEL STRUCTURE TO BE USED
!
         it = 1
         IF ( max1>max2 ) it = 2
         IF ( max1==max2 ) it = Idflt
         IF ( it==2 ) THEN
!
!     UPDATE NACUM TO BE THE SAME AS NLOW
!
            DO k = 1 , Idpth
               Nacum(k) = Nlow(k)
            ENDDO
         ELSE
            IF ( i==1 ) Isdir = -1
!
!     COPY LVLS1 INTO LVLS2 FOR EACH NODE IN CONNECTED COMPONENT
!
            DO k = j , end
               inode = Ccstor(k)
               Lvls2(inode) = Lvls1(inode)
            ENDDO
!
!     UPDATE NACUM TO BE THE SAME AS NHIGH
!
            DO k = 1 , Idpth
               Nacum(k) = Nhigh(k)
            ENDDO
         ENDIF
      ENDDO
      GOTO 99999
   ELSE
      j = 1
      k = Xc - m
      i = j
   ENDIF
 200  DO
      n = i + m
      IF ( Size(n)<=Size(i) ) EXIT
      temp = Size(i)
      Size(i) = Size(n)
      Size(n) = temp
      temp = Stpt(i)
      Stpt(i) = Stpt(n)
      Stpt(n) = temp
      i = i - m
      IF ( i<1 ) EXIT
   ENDDO
   j = j + 1
   IF ( j>k ) GOTO 100
   i = j
   GOTO 200
99999 RETURN
END SUBROUTINE piklvl
