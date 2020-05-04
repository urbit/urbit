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

export function uxToHex(ux) {
  if (ux.length > 2 && ux.substr(0, 2) === '0x') {
    let value = ux.substr(2).replace('.', '').padStart(6, '0');
    return value;
  }

  let value = ux.replace('.', '').padStart(6, '0');
  return value;
}

export function writeText(str) {
  return new Promise(function (resolve, reject) {

    var range = document.createRange();
    range.selectNodeContents(document.body);
    document.getSelection().addRange(range);

    var success = false;
    function listener(e) {
      e.clipboardData.setData("text/plain", str);
      e.preventDefault();
      success = true;
    }
    document.addEventListener("copy", listener);
    document.execCommand("copy");
    document.removeEventListener("copy", listener);

    document.getSelection().removeAllRanges();

    success ? resolve() : reject();
  }).catch(function (error) {
    console.error(error);
  });;
};


// trim patps to match dojo, chat-cli
export function cite(ship) {
  let patp = ship, shortened = "";
  if (patp.startsWith("~")) {
    patp = patp.substr(1);
  }
  // comet
  if (patp.length === 56) {
    shortened = "~" + patp.slice(0, 6) + "_" + patp.slice(50, 56);
    return shortened;
  }
  // moon
  if (patp.length === 27) {
    shortened = "~" + patp.slice(14, 20) + "^" + patp.slice(21, 27);
    return shortened;
  }
  return `~${patp}`;
}

export function alphabetiseAssociations(associations) {
  let result = {};
  Object.keys(associations).sort((a, b) => {
    let aName = a.substr(1);
    let bName = b.substr(1);
    if (associations[a].metadata && associations[a].metadata.title) {
      aName = associations[a].metadata.title !== ""
        ? associations[a].metadata.title
        : a.substr(1);
    }
    if (associations[b].metadata && associations[b].metadata.title) {
      bName = associations[b].metadata.title !== ""
        ? associations[b].metadata.title
        : b.substr(1);
    }
    return aName.toLowerCase().localeCompare(bName.toLowerCase());
  }).map((each) => {
    result[each] = associations[each];
  })
  return result;
}