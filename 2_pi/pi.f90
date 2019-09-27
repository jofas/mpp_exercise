program pi
  use mpi

  implicit none

  integer :: N = 84000, RUN_N_TIMES = 10000
  integer :: ierror, rank, world_size
  integer :: i, j, start_iter, end_iter
  real    :: pi_approx, pi_buff
  real    :: t_start, t_end

  character(len=20) :: FMT_STR
  integer, dimension(mpi_status_size) :: stat

  FMT_STR = "(F22.20)"

  call mpi_init(ierror)

  t_start = mpi_wtime()

  call mpi_comm_rank(mpi_comm_world, rank, ierror)

  call mpi_comm_size(mpi_comm_world, world_size, ierror)

  do j = 1, RUN_N_TIMES

    start_iter = rank * (N / world_size) + 1
    end_iter   = start_iter + (N / world_size) - 1
    if (rank == world_size - 1) end_iter = N

    pi_approx = 4.0 / float(N) * sum( (/ (1.0 / (1.0 + ( &
      ( float(i) - 0.5 ) / float(N) ) ** 2), &
      i = start_iter, end_iter) /) )

    if (rank > 0) then
      call mpi_ssend(pi_approx, 1, mpi_real, 0, 0, &
        mpi_comm_world, ierror)
    else
      do i = 1, world_size - 1
        call mpi_recv(pi_buff, 1, mpi_real, i, 0, &
          mpi_comm_world, stat, ierror)
        pi_approx = pi_approx + pi_buff
      end do
    end if
  end do

  t_end = mpi_wtime()
  if (rank == 0) then
    write (*, FMT_STR) pi_approx
    print *, "time ", t_end - t_start
  end if
  call mpi_finalize(ierror)
end
