set terminal postscript eps enhanced color font 'Helvetica,20'
set output "pureset.eps"
set format y "%g"
set grid xtics
set grid ytics
set grid ztics
set xlabel "Threads"
set xtics 1
set ylabel "Time in seconds"
set datafile separator ","
plot "reports/pureset.csv" using 1:2 \
     with errorlines linewidth 1.5 pointtype 4 pointsize 1.0 \
     title "PureSet", \
     "reports/vpureset.csv" using 1:2 \
     with errorlines linewidth 1.5 pointtype 7 pointsize 1.0 \
     title "Verified PureSet", \
     "reports/slset.csv" using 1:2 \
     with errorlines linewidth 1.5 pointtype 8 pointsize 1.0 \
     title "SLSet", \
     "reports/vslset.csv" using 1:2 \
     with errorlines linewidth 1.5 pointtype 5 pointsize 1.0 \
     title "Verified SLSet"
