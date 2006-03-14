
function fixImage(img, width, height) {
   var isIE = navigator.userAgent.toLowerCase().indexOf("msie") >= 0;
   if (!isIE)
      return;

   var currentSrc = img.src;

   var imgStyle = "progid:DXImageTransform.Microsoft.AlphaImageLoader(src='" + currentSrc + "', sizingMethod='scale')";
   img.src = 'images/clearpixel.gif';
   img.style.width  = width + "px";
   img.style.height = height + "px";
   img.style.filter =  imgStyle;
}

var navigationPages = [ "home.page", "eatures.page", "demos.page", "docs.page", "downloads.page", "about.page" ];
var navigationLinks = [ "homeLink", "featuresLink", "demosLink", "docsLink", "downloadsLink", "aboutLink" ];

function showMenuContext() {
   var currentLocation = document.location.href;
   for ( var i = 0 ; i < navigationPages.length ; i++ )
      if ( currentLocation.indexOf(navigationPages[i]) != -1 ) { setLinkStyle($(navigationLinks[i])); break; }
}

function setLinkStyle(link) {
   link.style.fontWeight = 'bold';
   var currentFontSize = parseInt(RicoUtil.getElementsComputedStyle(link, "fontSize", "font-size" ));
   link.style.fontSize = (currentFontSize+2) + "px";
   link.style.color    = 'white';
}
