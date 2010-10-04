// This code was written by Tyler Akins and has been placed in the
// public domain.  It would be nice if you left this header intact.
// Base64 code from Tyler Akins -- http://rumkin.com
//
// Heavily modified.  We need to deal with an array of bytes, instead
// of a string, as input; a string as input doesn't make much sense
// because the whole point of BASE64 is to send *binary* data (which
// is pretty hard to encode in a proper string).
(function(){var a="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";EXTEND_CLASS(Array,function(b,c){c.bytesToBase64=function(){var b=0,c=this.length,d="";while(b<c){var e=this[b++],f=this[b++],g=this[b++],h=e>>>2,i=(e&3)<<4|f>>>4,j=(f&15)<<2|g>>>6,k=g&63;b-c==2?j=k=64:b-c==1&&(k=64),d+=a.charAt(h)+a.charAt(i)+a.charAt(j)+a.charAt(k)}return d}}),EXTEND_CLASS(String,function(b,c){c.base64ToBytes=function(){var b=this.replace(/[^A-Za-z0-9\+\/\=]/g,""),c=0,d=b.length,e=[];while(c<d){var f=a.indexOf(b.charAt(c++)),g=a.indexOf(b.charAt(c++)),h=a.indexOf(b.charAt(c++)),i=a.indexOf(b.charAt(c++)),j=f<<2|g>>>4,k=(g&15)<<4|h>>>2,l=(h&3)<<6|i;e.push(j),h!=64&&e.push(k),i!=64&&e.push(l)}return e}})})()