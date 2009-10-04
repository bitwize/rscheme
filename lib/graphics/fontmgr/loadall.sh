#! /bin/sh

echo "Initial setup..."

rsf -q +graphics.fontmgr ~/lib/fonts.scm -exit > /tmp/fontbase.log

while rsf -q +graphics.fontmgr +graphics.charpath \
       -e '(let* ((db (open-font-database-for-update))
                  (rc (load-some-font-outlines db 1)))
             (commit db)
             (process-exit (if rc 1 0)))' -exit
do echo "more..."
done
