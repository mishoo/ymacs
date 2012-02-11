// This code was written by Tyler Akins and has been placed in the
// public domain.  It would be nice if you left this header intact.
// Base64 code from Tyler Akins -- http://rumkin.com
//
// Heavily modified.  We need to deal with an array of bytes, instead
// of a string, as input; a string as input doesn't make much sense
// because the whole point of BASE64 is to send *binary* data (which
// is pretty hard to encode in a proper string).
(function(){var a="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";EXTEND_CLASS(Array,function(b,c){c.bytesToBase64=function(){var b,c,d,e,f,g,h,i=0,j=this.length,k="";while(i<j)b=this[i++],c=this[i++],d=this[i++],e=b>>>2,f=(b&3)<<4|c>>>4,g=(c&15)<<2|d>>>6,h=d&63,i-j==2?g=h=64:i-j==1&&(h=64),k+=a.charAt(e)+a.charAt(f)+a.charAt(g)+a.charAt(h);return k}}),EXTEND_CLASS(String,function(b,c){c.base64ToBytes=function(){var b,c,d,e,f,g,h,i=this.replace(/[^A-Za-z0-9\+\/\=]/g,""),j=0,k=i.length,l=[];while(j<k)b=a.indexOf(i.charAt(j++)),c=a.indexOf(i.charAt(j++)),d=a.indexOf(i.charAt(j++)),e=a.indexOf(i.charAt(j++)),f=b<<2|c>>>4,g=(c&15)<<4|d>>>2,h=(d&3)<<6|e,l.push(f),d!=64&&l.push(g),e!=64&&l.push(h);return l}})})();