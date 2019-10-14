program ring
  use mpi_f08

  implicit none

  integer :: rank, w_size, sum_, recv_, recv_from, send_to
  integer :: i

  real :: start_t, end_t

  type(MPI_Request) :: req_send
  type(MPI_Status)  :: stat_recv

  type(MPI_Comm), save :: comm
  comm = mpi_comm_world

  call mpi_init()

  call mpi_comm_rank(comm, rank)
  call mpi_comm_size(comm, w_size)

  call set_neighbors()

  recv_ = rank !(rank + 1) ** 2
  sum_  = recv_

  start_t = mpi_wtime()
  do i = 1, w_size - 1
    call mpi_sendrecv_replace( &
      recv_, 1, mpi_integer, send_to, 0, recv_from, 0, &
      comm, stat_recv &
    )
    sum_ = sum_ + recv_
  end do
  end_t = mpi_wtime()

  print *, rank, sum_
  if (rank == 0) print *, "time ", end_t - start_t

  call mpi_finalize()

contains

  subroutine set_neighbors()
    if (rank == 0) then
      recv_from = w_size - 1
    else
      recv_from = rank - 1
    end if

    if (rank == w_size - 1) then
      send_to = 0
    else
      send_to = rank + 1
    end if
  end
end
