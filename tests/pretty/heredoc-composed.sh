cat <<EOF1 | cat <<EOF2
Here doc 1
EOF1
Here doc 2
EOF2

cat <<EOF1 && cat <<EOF2
Here doc 1
EOF1
Here doc 2
EOF2

cat <<EOF1 || cat <<EOF2
Here doc 1
EOF1
Here doc 2
EOF2

cat <<EOF1 | for x in true; do true; done
Here doc 1
EOF1

cat <<EOF1 | while true; do true; done
Here doc 1
EOF1
