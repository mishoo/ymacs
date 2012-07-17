// This code was written by Tyler Akins and has been placed in the
// public domain.  It would be nice if you left this header intact.
// Base64 code from Tyler Akins -- http://rumkin.com
//
// Heavily modified.  We need to deal with an array of bytes, instead
// of a string, as input; a string as input doesn't make much sense
// because the whole point of BASE64 is to send *binary* data (which
// is pretty hard to encode in a proper string).
(function(){var e="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";EXTEND_CLASS(Array,function(t,n){n.bytesToBase64=function(){var t,n,r,i,s,o,u,a=0,f=this.length,l="";while(a<f)t=this[a++],n=this[a++],r=this[a++],i=t>>>2,s=(t&3)<<4|n>>>4,o=(n&15)<<2|r>>>6,u=r&63,a-f==2?o=u=64:a-f==1&&(u=64),l+=e.charAt(i)+e.charAt(s)+e.charAt(o)+e.charAt(u);return l}}),EXTEND_CLASS(String,function(t,n){n.base64ToBytes=function(){var t,n,r,i,s,o,u,a=this.replace(/[^A-Za-z0-9\+\/\=]/g,""),f=0,l=a.length,c=[];while(f<l)t=e.indexOf(a.charAt(f++)),n=e.indexOf(a.charAt(f++)),r=e.indexOf(a.charAt(f++)),i=e.indexOf(a.charAt(f++)),s=t<<2|n>>>4,o=(n&15)<<4|r>>>2,u=(r&3)<<6|i,c.push(s),r!=64&&c.push(o),i!=64&&c.push(u);return c}})})();