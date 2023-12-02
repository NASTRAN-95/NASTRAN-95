!*==dpltst.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dpltst
!
   USE c_blank
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ept , i1 , i2 , i3
   INTEGER , DIMENSION(32) , SAVE :: errttl
   INTEGER , SAVE :: outrew , rew
   EXTERNAL clstab , cnstrc , comect , gopen , korsz , rdtrl , setinp , totape , write
!
! End of declarations rewritten by SPAG
!
   DATA outrew , rew/1 , 1/
   DATA errttl/8*2H   , 4HERRO , 4HR ME , 4HSSAG , 4HES F , 4HROM  , 4HTHE  , 4HPLOT , 4H SET , 4H DEF , 4HINIT , 4HION  , 4HMODU , &
       &4HLE ( , 4HPLTS , 4HET)  , 9*1H /
!
   nsets = 0
   pcdb = 101
   eqexin = 102
   ect = 103
   ept = 104
   merr = 201
   parm = 202
   gpset = 203
   elset = 204
   mset = 301
   pect = 302
   CALL totape(1,x(1))
!
   x(1) = eqexin
   CALL rdtrl(x)
   i2 = 2
   i3 = 3
   ngp = x(i2) - x(i3)
   i1 = korsz(x) - bufsiz + 1
   CALL gopen(merr,x(i1),outrew)
   CALL write(merr,-4,1,0)
   CALL write(merr,errttl,32,0)
   CALL setinp
   IF ( nsets/=0 ) THEN
      i1 = nsets + 1
      i2 = i1 + ngp
      i3 = korsz(x) - 4*bufsiz + 1
      CALL comect(x(i2),i3-i2)
      CALL cnstrc(x(i1),x(i2),x(i3),i3-i2)
   ELSE
      nsets = -1
   ENDIF
!
   CALL clstab(merr,rew)
END SUBROUTINE dpltst
