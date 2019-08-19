export function stringToSymbol(str) {
  let result = '';
  for (var i=0; i<str.length; i++){
    var n = str.charCodeAt(i);
    if (( (n >= 97) && (n <= 122) ) ||
        ( (n >= 48) && (n <= 57) ))
    {
      result += str[i];
    } else if ( (n >= 65) &&  (n <= 90) ) 
    {
      result += String.fromCharCode(n + 32);
    } else {
      result += '-';
    }
  }
  result = result.replace(/^[\-\d]+|\-+/g, '-');
  result = result.replace(/^\-+|\-+$/g, '');
  if (result === ''){
    return dateToDa(new Date());
  }
  return result;
}


export function dateToDa(d, mil) {
  var fil = function(n) {
    return n >= 10 ? n : "0" + n;
  };
  return (
    `~${d.getUTCFullYear()}.` +
    `${(d.getUTCMonth() + 1)}.` +
    `${fil(d.getUTCDate())}..` +
    `${fil(d.getUTCHours())}.` +
    `${fil(d.getUTCMinutes())}.` +
    `${fil(d.getUTCSeconds())}` +
    `${mil ? "..0000" : ""}`
  );
}
