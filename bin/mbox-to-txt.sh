#! /bin/bash

# strip .mbox file with mixed content of all unnecessary content

# strip html content from mbox
echo "Erstelle Backup:"
backup=$1'.BACKUP'
cp $1 $backup

echo "Arbeite auf der Datei: " $1
sed -i '/^Content-Type: text\/html/,/^--_/d' $1 #strip html content
sed -i '/^Content-Type: text\/calendar/,/^--_/d' $1 #strip calendar invitations
sed -i '/^Content-Type: image/,/^--_/d'  $1 #strip images
sed -i '/^Content-Type: multipart/,/^--_/d' $1 #strip the line that informs about concated content
sed -i '/^Content-Type: application/,/^--_/d' $1 #strip pdfs

#remove techincal informations that are not needed:
sed -i '/^Received:/d' $1
sed -i '/X-Proofpoint/d' $1
sed -i '/X-MS-/d' $1
sed -i '/Content-Transfer-Encoding/d' $1

sed -i '/^--_/d' $1 #strip unnecessary lines
#sed -i 's/<[^>]\+>//g'  $1
sed -i 's/\r//g' $1 #strip \r (^M)

#convert encoding
filename=$1'_utf-8';
echo "Konvertiere die Encodierung zu UTF-8..."
iconv -c -f ISO-8859-1 -t UTF-8 -o $filename $1
mv $filename $1'.txt'
