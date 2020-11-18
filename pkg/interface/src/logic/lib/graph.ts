import { Content } from "~/types";
import urbitOb from "urbit-ob";

export function scanForMentions(text: string) {
  const regex = /~([a-z]|-)+/g;
  let result: Content[] = [];
  let match: RegExpExecArray | null;
  let lastPos = 0;
  while ((match = regex.exec(text)) !== null) {
    const newPos = match.index + match[0].length;
    if (urbitOb.isValidPatp(match[0])) {
      if (match.index !== lastPos) {
        result.push({ text: text.slice(lastPos, match.index) });
      }
      result.push({ mention: match[0] });
    }
    lastPos = newPos;
  }
  const remainder = text.slice(lastPos, text.length);
  if (remainder) {
    result.push({ text: remainder });
  }
  return result;
}
