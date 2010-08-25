// This code was written by Tyler Akins and has been placed in the
// public domain.  It would be nice if you left this header intact.
// Base64 code from Tyler Akins -- http://rumkin.com
//
// Heavily modified.  We need to deal with an array of bytes, instead
// of a string, as input; a string as input doesn't make much sense
// because the whole point of BASE64 is to send *binary* data (which
// is pretty hard to encode in a proper string).
(function(){var keyStr="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";EXTEND_CLASS(Array,function(D,P){P.bytesToBase64=function(){var i=0,n=this.length;var output="";while(i<n){var chr1=this[i++],chr2=this[i++],chr3=this[i++],enc1=chr1>>>2,enc2=((chr1&3)<<4)|(chr2>>>4),enc3=((chr2&15)<<2)|(chr3>>>6),enc4=chr3&63;if(i-n==2){enc3=enc4=64;}else if(i-n==1){enc4=64;}output+=keyStr.charAt(enc1)+keyStr.charAt(enc2)+keyStr.charAt(enc3)+keyStr.charAt(enc4);}return output;};});EXTEND_CLASS(String,function(D,P){P.base64ToBytes=function(){var input=this.replace(/[^A-Za-z0-9\+\/\=]/g,""),i=0,n=input.length,output=[];while(i<n){var enc1=keyStr.indexOf(input.charAt(i++)),enc2=keyStr.indexOf(input.charAt(i++)),enc3=keyStr.indexOf(input.charAt(i++)),enc4=keyStr.indexOf(input.charAt(i++)),chr1=(enc1<<2)|(enc2>>>4),chr2=((enc2&15)<<4)|(enc3>>>2),chr3=((enc3&3)<<6)|enc4;output.push(chr1);if(enc3!=64)output.push(chr2);if(enc4!=64)output.push(chr3);}return output;};});})();