program collective
  use mpi_f08

  implicit none

  integer :: rank, send_, sum_

  real :: start_t, end_t

  type(MPI_Comm), save :: comm
  comm = mpi_comm_world

  call mpi_init()

  call mpi_comm_rank(comm, rank)

  send_ = (rank + 1) ** 2
  sum_ = send_

  start_t = mpi_wtime()
  call mpi_reduce( &
    send_, sum_, 1, mpi_integer, mpi_sum, 0, comm &
  )
  end_t = mpi_wtime()

  if (rank == 0) print *, "time ", end_t - start_t, "sum ", sum_

  call mpi_finalize()
end
