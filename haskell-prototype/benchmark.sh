make clean
make BFS

# Generate test data using PBBS.
~/repos/pbbs/testData/graphData/gridGraph -d 3 100000 /tmp/grid

# Run with various numbers of cores.
num_cores="1 2 4";
for i in $num_cores; do ./BFS -o BFS$i.html +RTS -N$i; done