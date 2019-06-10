-module(mime).
-compile(export_all).

guess(File) when is_binary(File) ->
  guess(text:str(File));
guess(File) ->
  Extension = filename:extension(File),
  mime_type(Extension).

mime_type(".html")     -> "text/html";
mime_type(".htm")      -> "text/html";
mime_type(".shtml")    -> "text/html";
mime_type(".css")      -> "text/css";
mime_type(".xml")      -> "text/xml";
mime_type(".gif")      -> "image/gif";
mime_type(".jpg")      -> "image/jpeg";
mime_type(".jpeg")     -> "image/jpeg";
mime_type(".js")       -> "application/x-javascript";
mime_type(".atom")     -> "application/atom+xml";
mime_type(".rss")      -> "application/rss+xml";

mime_type(".mml")      -> "text/mathml";
mime_type(".txt")      -> "text/plain";
mime_type(".jad")      -> "text/vnd.sun.j2me.app-descriptor";
mime_type(".wml")      -> "text/vnd.wap.wml";
mime_type(".htc")      -> "text/x-component";

mime_type(".png")      -> "image/png";
mime_type(".tif")      -> "image/tiff";
mime_type(".tiff")     -> "image/tiff";
mime_type(".wbmp")     -> "image/vnd.wap.wbmp";
mime_type(".ico")      -> "image/x-icon";
mime_type(".jng")      -> "image/x-jng";
mime_type(".bmp")      -> "image/x-ms-bmp";
mime_type(".svg")      -> "image/svg+xml";
mime_type(".svgz")     -> "image/svg+xml";
mime_type(".webp")     -> "image/webp";

mime_type(".jar")      -> "application/java-archive";
mime_type(".war")      -> "application/java-archive";
mime_type(".ear")      -> "application/java-archive";
mime_type(".hqx")      -> "application/mac-binhex40";
mime_type(".doc")      -> "application/msword";
mime_type(".pdf")      -> "application/pdf";
mime_type(".ps")       -> "application/postscript";
mime_type(".eps")      -> "application/postscript";
mime_type(".ai")       -> "application/postscript";
mime_type(".rtf")      -> "application/rtf";
mime_type(".xls")      -> "application/vnd.ms-excel";
mime_type(".ppt")      -> "application/vnd.ms-powerpoint";
mime_type(".wmlc")     -> "application/vnd.wap.wmlc";
mime_type(".kml")      -> "application/vnd.google-earth.kml+xml";
mime_type(".kmz")      -> "application/vnd.google-earth.kmz";
mime_type(".7z")       -> "application/x-7z-compressed";
mime_type(".cco")      -> "application/x-cocoa";
mime_type(".jardiff")  -> "application/x-java-archive-diff";
mime_type(".jnlp")     -> "application/x-java-jnlp-file";
mime_type(".run")      -> "application/x-makeself";
mime_type(".pl")       -> "application/x-perl";
mime_type(".pm")       -> "application/x-perl";
mime_type(".prc")      -> "application/x-pilot";
mime_type(".pdb")      -> "application/x-pilot";
mime_type(".rar")      -> "application/x-rar-compressed";
mime_type(".rpm")      -> "application/x-redhat-package-manager";
mime_type(".sea")      -> "application/x-sea";
mime_type(".swf")      -> "application/x-shockwave-flash";
mime_type(".sit")      -> "application/x-stuffit";
mime_type(".tcl")      -> "application/x-tcl";
mime_type(".tk")       -> "application/x-tcl";
mime_type(".der")      -> "application/x-x509-ca-cert";
mime_type(".pem")      -> "application/x-x509-ca-cert";
mime_type(".crt")      -> "application/x-x509-ca-cert";
mime_type(".xpi")      -> "application/x-xpinstall";
mime_type(".xhtml")    -> "application/xhtml+xml";
mime_type(".zip")      -> "application/zip";

mime_type(".bin")      -> "application/octet-stream";
mime_type(".exe")      -> "application/octet-stream";
mime_type(".dll")      -> "application/octet-stream";
mime_type(".deb")      -> "application/octet-stream";
mime_type(".dmg")      -> "application/octet-stream";
mime_type(".eot")      -> "application/octet-stream";
mime_type(".iso")      -> "application/octet-stream";
mime_type(".img")      -> "application/octet-stream";
mime_type(".msi")      -> "application/octet-stream";
mime_type(".msp")      -> "application/octet-stream";
mime_type(".msm")      -> "application/octet-stream";

mime_type(".mid")      -> "audio/midi";
mime_type(".midi")     -> "audio/midi";
mime_type(".kar")      -> "audio/midi";
mime_type(".mp3")      -> "audio/mpeg";
mime_type(".ogg")      -> "audio/ogg";
mime_type(".m4a")      -> "audio/x-m4a";
mime_type(".ra")       -> "audio/x-realaudio";

mime_type(".3gpp")     -> "video/3gpp";
mime_type(".3gp")      -> "video/3gpp";
mime_type(".mp4")      -> "video/mp4";
mime_type(".mpeg")     -> "video/mpeg";
mime_type(".mpg")      -> "video/mpeg";
mime_type(".mov")      -> "video/quicktime";
mime_type(".webm")     -> "video/webm";
mime_type(".flv")      -> "video/x-flv";
mime_type(".m4v")      -> "video/x-m4v";
mime_type(".mng")      -> "video/x-mng";
mime_type(".asx")      -> "video/x-ms-asf";
mime_type(".asf")      -> "video/x-ms-asf";
mime_type(".wmv")      -> "video/x-ms-wmv";
mime_type(".avi")      -> "video/x-msvideo";

mime_type(_)          -> "application/octet-stream".


is_mime_type(Type) when is_atom(Type) ->
  is_mime_type(atom_to_list(Type));
is_mime_type(Type) when is_binary(Type) ->
  is_mime_type(text:str(Type));
is_mime_type(Type_) ->
  Type = string:to_lower(Type_),
  MimeTypeList = [
    "text/html",
    "text/css",
    "text/xml",
    "image/gif",
    "image/jpeg",
    "application/x-javascript",
    "application/atom+xml",
    "application/rss+xml",
    
    "text/mathml",
    "text/plain",
    "text/vnd.sun.j2me.app-descriptor",
    "text/vnd.wap.wml",
    "text/x-component",
    
    "image/png",
    "image/tiff",
    "image/vnd.wap.wbmp",
    "image/x-icon",
    "image/x-jng",
    "image/x-ms-bmp",
    "image/svg+xml",
    "image/webp",
    
    "application/java-archive",
    "application/mac-binhex40",
    "application/msword",
    "application/pdf",
    "application/postscript",
    "application/rtf",
    "application/vnd.ms-excel",
    "application/vnd.ms-powerpoint",
    "application/vnd.wap.wmlc",
    "application/vnd.google-earth.kml+xml",
    "application/vnd.google-earth.kmz",
    "application/x-7z-compressed",
    "application/x-cocoa",
    "application/x-java-archive-diff ",
    "application/x-java-jnlp-file",
    "application/x-makeself",
    "application/x-perl",
    "application/x-pilot",
    "application/x-rar-compressed",
    "application/x-redhat-package-manager",
    "application/x-sea",
    "application/x-shockwave-flash",
    "application/x-stuffit",
    "application/x-tcl",
    "application/x-x509-ca-cert",
    "application/x-xpinstall",
    "application/xhtml+xml",
    "application/zip",
    
    "application/octet-stream",
    
    "audio/midi",
    "audio/mpeg",
    "audio/ogg",
    "audio/x-m4a",
    "audio/x-realaudio",
    
    "video/3gpp",
    "video/mp4",
    "video/mpeg",
    "video/quicktime",
    "video/webm",
    "video/x-flv",
    "video/x-m4v",
    "video/x-mng",
    "video/x-ms-asf",
    "video/x-ms-wmv",
    "video/x-msvideo"
  ],
  lists:member(Type, MimeTypeList).


  
