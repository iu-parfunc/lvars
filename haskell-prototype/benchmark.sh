# Generate test data using PBBS.
~/repos/pbbs/testData/graphData/gridGraph -d 3 100000 /tmp/grid

num_cores="1 2 4";

# Impure version
make clean
make BFS
for i in $num_cores; do ./BFS -o BFS_impure_$i.html +RTS -N$i; done

# Pure version
make clean
make pure
for i in $num_cores; do ./BFS -o BFS_pure_$i.html +RTS -N$i; done