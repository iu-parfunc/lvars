set terminal postscript enhanced color
set output "BFS_LVar.eps"
set title "Benchmark: BFS LVar, speedup relative to serial time 1.0e-2 seconds "
set xlabel "Number of Threads"
set ylabel "Parallel Speedup"
set xrange [1:]
set key left top
plot \
   "BFS_LVar1.dat" using 1:2:3:4 with errorbars lt 1 title "", \
   "BFS_LVar2.dat" using 1:2:3:4 with errorbars lt 2 title "", \
   "BFS_LVar3.dat" using 1:2:3:4 with errorbars lt 3 title "", \
   "BFS_LVar4.dat" using 1:2:3:4 with errorbars lt 4 title "", \
   "BFS_LVar5.dat" using 1:2:3:4 with errorbars lt 5 title "", \
   "BFS_LVar6.dat" using 1:2:3:4 with errorbars lt 7 title "", \
   "BFS_LVar7.dat" using 1:2:3:4 with errorbars lt 8 title "", \
   "BFS_LVar8.dat" using 1:2:3:4 with errorbars lt 9 title "", \
   "BFS_LVar9.dat" using 1:2:3:4 with errorbars lt 10 title "", \
   "BFS_LVar10.dat" using 1:2:3:4 with errorbars lt 11 title "", \
   "BFS_LVar11.dat" using 1:2:3:4 with errorbars lt 12 title "", \
   "BFS_LVar12.dat" using 1:2:3:4 with errorbars lt 13 title "", \
   "BFS_LVar13.dat" using 1:2:3:4 with errorbars lt 14 title "", \
   "BFS_LVar14.dat" using 1:2:3:4 with errorbars lt 15 title "", \
   "BFS_LVar1.dat" using 1:2 with lines linewidth 5.0 lt 1 title "/tmp/rand_160000_10000_5_1000/None" ,\
   "BFS_LVar2.dat" using 1:2 with lines linewidth 5.0 lt 2 title "/tmp/rand_160000_20000_5_1000/None" ,\
   "BFS_LVar3.dat" using 1:2 with lines linewidth 5.0 lt 3 title "/tmp/rand_160000_40000_5_1000/None" ,\
   "BFS_LVar4.dat" using 1:2 with lines linewidth 5.0 lt 4 title "/tmp/rand_40000_10000_5_1000/None" ,\
   "BFS_LVar5.dat" using 1:2 with lines linewidth 5.0 lt 5 title "/tmp/rand_40000_10000_5_10000/None" ,\
   "BFS_LVar6.dat" using 1:2 with lines linewidth 5.0 lt 7 title "/tmp/rand_40000_20000_5_1000/None" ,\
   "BFS_LVar7.dat" using 1:2 with lines linewidth 5.0 lt 8 title "/tmp/rand_40000_20000_5_10000/None" ,\
   "BFS_LVar8.dat" using 1:2 with lines linewidth 5.0 lt 9 title "/tmp/rand_40000_40000_5_1000/None" ,\
   "BFS_LVar9.dat" using 1:2 with lines linewidth 5.0 lt 10 title "/tmp/rand_40000_40000_5_10000/None" ,\
   "BFS_LVar10.dat" using 1:2 with lines linewidth 5.0 lt 11 title "/tmp/rand_80000_10000_5_1000/None" ,\
   "BFS_LVar11.dat" using 1:2 with lines linewidth 5.0 lt 12 title "/tmp/rand_80000_10000_5_10000/None" ,\
   "BFS_LVar12.dat" using 1:2 with lines linewidth 5.0 lt 13 title "/tmp/rand_80000_20000_5_1000/None" ,\
   "BFS_LVar13.dat" using 1:2 with lines linewidth 5.0 lt 14 title "/tmp/rand_80000_20000_5_10000/None" ,\
   "BFS_LVar14.dat" using 1:2 with lines linewidth 5.0 lt 15 title "/tmp/rand_80000_40000_5_1000/None" 
