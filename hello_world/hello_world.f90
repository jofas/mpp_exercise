program hello_world
  use mpi

  implicit none

  integer ierror, rank, world_size, namelen
  character*(mpi_max_processor_name) :: procname

  call mpi_init(ierror)

  call mpi_comm_rank(mpi_comm_world, rank, ierror)

  call mpi_comm_size(mpi_comm_world, world_size, ierror)

  call mpi_get_processor_name(procname, namelen, ierror)

  print *, "rank: ", rank, "proc: ", procname(:namelen)

  !if (rank == 0) then
  !  print *, "This MPI world runs ", world_size, " processes"
  !end if

  call mpi_finalize(ierror)
end
