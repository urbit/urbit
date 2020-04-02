export function daToDate(st) {
  var dub = function(n) {
    return parseInt(n) < 10 ? "0" + parseInt(n) : n.toString();
  };
  var da = st.split('..');
  var bigEnd = da[0].split('.');
  var lilEnd = da[1].split('.');
  var ds = `${bigEnd[0].slice(1)}-${dub(bigEnd[1])}-${dub(bigEnd[2])}T${dub(lilEnd[0])}:${dub(lilEnd[1])}:${dub(lilEnd[2])}Z`;
  return new Date(ds);
}

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