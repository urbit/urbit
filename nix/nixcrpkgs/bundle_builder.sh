source $setup
names=($names)
dirs=($dirs)
mkdir $out
cd $out
for ((i=0;i<${#names[@]};i++)); do
  ln -s "${dirs[i]}" "${names[i]}"
done
