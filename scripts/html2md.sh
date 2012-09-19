# Finds only files that have the html extension, and strip the ./ from find
for file in `find . -type f -name \*.html | sed "s|^\./||"`
do
	# Trim the file extension
	name=${file%\.*}
	# Output to the new wiki
	html2text.py $file > ~/newwiki/$name.md
done

