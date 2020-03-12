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