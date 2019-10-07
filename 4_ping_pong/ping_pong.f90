program ping_pong
  use mpi

  implicit none

  integer :: err, rank, i
  integer :: MAX_ITER = 1000000, N = 1000000

  real :: t_start, t_end

  integer, dimension(mpi_status_size) :: stat
  integer(kind=1), dimension(:), allocatable :: msg

  allocate(msg(N))

  call mpi_init(err)

  call mpi_comm_rank(mpi_comm_world, rank, err)

  msg(:) = 0

  t_start = mpi_wtime()

  do i = 1, MAX_ITER
    select case(rank)
      case(0)
        call mpi_ssend(msg, N, mpi_integer1, 1, 0, &
          mpi_comm_world, err)
        call mpi_recv(msg, N, mpi_integer1, 1, 0, &
          mpi_comm_world, stat, err)
        !print *, "rank 0 received data: ", msg
      case(1)
        call mpi_recv(msg, N, mpi_integer1, 0, 0, &
          mpi_comm_world, stat, err)
        !print *, "rank 1 received data: ", msg
        call mpi_ssend(msg, N, mpi_integer1, 0, 0, &
          mpi_comm_world, err)
      case default
        exit
    end select
  end do

  t_end = mpi_wtime()

  if (rank == 0) then
    print "(I10, A, I10, A, F6.4, A, F6.4, A, F21.20)", &
      N, ",", MAX_ITER, ",", t_end - t_start, ",", &
      (t_end - t_start) / float(MAX_ITER), ",", &
      float(N) / (t_end - t_start) * (1024.0 ** 2)
  end if

  call mpi_finalize(err)
end
