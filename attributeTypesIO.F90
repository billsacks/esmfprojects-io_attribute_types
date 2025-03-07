program attribute_types_io
   use ESMF
   implicit none

   type(ESMF_Grid) :: grid
   integer :: rc
   integer :: i, j, u1, u2
   integer :: lde, ldeCount
   integer :: lbndX(2), ubndX(2), lbndY(2), ubndY(2)
   real(ESMF_KIND_R8), pointer :: coordX(:), coordY(:)
   type(ESMF_ArraySpec) :: arraySpec
   type(ESMF_Field) :: field
   type(ESMF_Info) :: info
   integer, pointer :: dataPtr2d(:,:)
   integer :: val

   character(len=*), parameter :: fname = "output.nc"

   call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, defaultCalkind=ESMF_CALKIND_GREGORIAN, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   ! ------------------------------------------------------------------------
   ! Create grid
   ! ------------------------------------------------------------------------

   ! The grid creation here follows
   ! http://earthsystemmodeling.org/docs/nightly/develop/ESMF_refdoc/node5.html#SECTION05083100000000000000


   grid = ESMF_GridCreateNoPeriDim( &
         maxIndex = [10, 20], &
         regDecomp = [5, 2], &
         coordSys = ESMF_COORDSYS_CART, &
         coordDep1 = [1], &
         coordDep2 = [2], &
         indexflag = ESMF_INDEX_GLOBAL, &
         rc = rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_GridGet(grid, localDECount=ldeCount, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   do lde = 0, ldeCount-1
      call ESMF_GridGetCoord(grid, coordDim=1, localDE=lde, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            computationalLBound=lbndX, computationalUBound=ubndX, &
            farrayPtr=coordX, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      do i = lbndX(1), ubndX(1)
         coordX(i) = i*10.0
      end do

      call ESMF_GridGetCoord(grid, coordDim=2, localDE=lde, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            computationalLBound=lbndY, computationalUBound=ubndY, &
            farrayPtr=coordY, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      do j = lbndY(1), ubndY(1)
         coordY(j) = j*10.0
      end do
   end do

   call ESMF_AttributeAdd(grid, convention="NetCDF", purpose="NOAHMP", attrList=(/"ESMF:gridded_dim_labels"/), rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(grid, convention="NetCDF", purpose="NOAHMP", name="ESMF:gridded_dim_labels", valueList=(/"x", "y"/), rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   ! ------------------------------------------------------------------------
   ! Create field
   ! ------------------------------------------------------------------------

   ! Set type and rank for ESMF arrayspec
   call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   ! Create field
   field = ESMF_FieldCreate(grid, arraySpec, staggerloc=ESMF_STAGGERLOC_CENTER, &
         indexflag=ESMF_INDEX_GLOBAL, name='dummy', rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   ! Fill field
   val = 3
   do lde = 0, ldeCount-1
      call ESMF_FieldGet(field, localDE=lde, farrayPtr=dataPtr2d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      dataPtr2d(:,:) = val
   end do

   ! ------------------------------------------------------------------------
   ! Add attribute
   ! ------------------------------------------------------------------------

   ! call ESMF_InfoGetFromHost(field, info, rc=rc)
   ! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
   !       line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   ! call ESMF_InfoSet(info, "missing_value", 3)

   call ESMF_AttributeAdd(field, convention="NetCDF", purpose="NOAHMP", attrList=(/"long_name"/), rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(field, convention="NetCDF", purpose="NOAHMP", name='long_name', value='dummy', rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_AttributeAdd(field, convention="NetCDF", purpose="NOAHMP", attrList=(/"missing_value"/), rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(field, convention="NetCDF", purpose="NOAHMP", name='missing_value', value=val, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   ! ------------------------------------------------------------------------
   ! Write field
   ! ------------------------------------------------------------------------
   call ESMF_FieldWrite(field, fileName=fname, variableName='dummy', overwrite=.true., convention="NetCDF", purpose="NOAHMP", rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_Finalize()

end program attribute_types_io
