#!/usr/bin/zsh

cat > /tmp/dzen2_proto_values <<EOF
put sample/test data here
EOF

rm -f /tmp/dzen2_proto_pipe && mkfifo /tmp/dzen2_proto_pipe
dzen2 -xs 1 -e '' -l 22 -fn "-*-inconsolata-*-*-*-*-*-*-*-*-*-*-*-*" -m < /tmp/dzen2_proto_pipe &
exec 3> /tmp/dzen2_proto_pipe
q=""
echo "^uncollapse()" >&3
#set -x
./grabkey 2>&1 | while read i ; do
	if [ $i = "C-s" ] ; then
		selected=$(expr $selected + 1)
	elif [ $i = "C-r" ] ; then
		selected=$(expr $selected - 1)
		if [ $selected = 0 ] ; then
			selected=1
		fi
	else
		if [ $i = "BackSpace" ] ; then
			q=${q%?}
		else
			q="$q$i"
		fi
		selected=1
		echo "^tw()^fg(#cb4b16)$q" >&3
		rows=()
		grep -i "$q" /tmp/dzen2_proto_values | head -n 20 | sed "s/\($q\)/^fg(#859900)\1^fg()/ig" | while read j ; do
			#rows+=($j)
			rows[$(($#rows+1))]=$j
		done
	fi
	echo "^cs()" >&3
	OUTPUT=""
	for n in $(seq 1 ${#rows[@]}) ; do
		x=""
		if [ $n = $selected ] ; then
			x="^bg(#586e75)"
		fi
		OUTPUT+="$x${rows[$n]}\n"
	done
	echo "$OUTPUT" >&3
done
