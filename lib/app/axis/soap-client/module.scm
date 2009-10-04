(define-module app.axis.soap-client ()
  (&module
   (import usual-inlines
           util.xpath
           util.xml
           rs.net.soap
           unixm
           syscalls
           rs.net.md5
           )
   ;;
   (load "axis.scm")
   ;;
   (export axis:connect
           axis:whoami
           axis:GetHistory
           axis:RepositoryTimestamp
           axis:SnapshotDifference
           axis:CreateTagSnapshot
           axis:ConfigureKeywordBySnapshot
           axis:ChangeRequestLookup

           axis:ListDirectory
           axis:Retrieve

           axis:CreateDirectory
           axis:CreateFile
           axis:Update

           <axis-FileRef>
           <axis-Path>
           steps
           fileset
           snapshot
           path
           )
   ;;
   ))
    
