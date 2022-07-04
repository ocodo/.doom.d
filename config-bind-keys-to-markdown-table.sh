echo "| Command | Binding |"
echo "|---------|---------|"
grep "bind-key" config.el | sed 's/(bind-key //' | tr -d '"#' | tr -d "'" | sed 's/)$//' | perl -pe "s/(.*?) {1,}(.*)$/\| \2 \| <kbd>\1<\/kbd> \|/"
