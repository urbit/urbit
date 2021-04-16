import urbitOb from 'urbit-ob';
import { parsePermalink, permalinkToReference } from "~/logic/lib/permalinks";

const URL_REGEX = new RegExp(String(/^(([\w\-\+]+:\/\/)[-a-zA-Z0-9:@;?&=\/%\+\.\*!'\(\),\$_\{\}\^~\[\]`#|]+\w)/.source));

const isUrl = (string) => {
  try {
    return URL_REGEX.test(string);
  } catch (e) {
    return false;
  }
}

const isRef = (str) => {
  return isUrl(str) && str.startsWith("web+urbitgraph://");
}

const tokenizeMessage = (text) => {
  let messages = [];
  let message = [];
  let isInCodeBlock = false;
  let endOfCodeBlock = false;
  text.split(/\r?\n/).forEach((line, index) => {
    if (index !== 0) {
      message.push('\n');
    }
    // A line of backticks enters and exits a codeblock
    if (line.startsWith('```')) {
      // But we need to check if we've ended a codeblock
      endOfCodeBlock = isInCodeBlock;
      isInCodeBlock = (!isInCodeBlock);
    } else {
      endOfCodeBlock = false;
    }

    if (isInCodeBlock || endOfCodeBlock) {
      message.push(line);
    } else {
      line.split(/\s/).forEach((str) => {
        const punctTest = str.match(/([^\w\d]+)?([0-9a-z\-\s]+)?([^\w\d]+)?/);
        if (
          (str !== '`' && punctTest[1] && punctTest[1] === '`')
          || (str === '`' && !isInCodeBlock)
        ) {
          isInCodeBlock = true;
        } else if (
          (str !== '`' && punctTest[3] && punctTest[3].startsWith('`'))
          || (str === '`' && isInCodeBlock)
        ) {
          isInCodeBlock = false;
        }

        if(isRef(str) && !isInCodeBlock) {
          if (message.length > 0) {
            // If we're in the middle of a message, add it to the stack and reset
            messages.push({ text: message.join(' ') });
          }
          const link = parsePermalink(str);
          if(!link) {
            messages.push({ url: str });
          } else {
            const reference = permalinkToReference(link);
            messages.push(reference);
          }
          message = [];
        } else if (isUrl(str) && !isInCodeBlock) {
          if (message.length > 0) {
            // If we're in the middle of a message, add it to the stack and reset
            messages.push({ text: message.join(' ') });
            message = [];
          }
          messages.push({ url: str });
          message = [];
        } else if (urbitOb.isValidPatp(`~${punctTest[2]}`) && !isInCodeBlock) {
          if (message.length > 0) {
            // If we're in the middle of a message, add it to the stack and reset
            messages.push({ text: message.join(' ') });
            message = [];
          }
          if (punctTest[1] && punctTest[1] !== '~') {
            messages.push({ text: punctTest[1].replace('~', '')})
          }
          messages.push({ mention: punctTest[2] });
          if (punctTest[3]) {
            messages.push({ text: `${punctTest[3]} ` })
          }
          message = [];

        } else {
          message.push(str);
        }
      });
    }
  });

  if (message.length) {
    // Add any remaining message
    messages.push({ text: message.join(' ') });
  }
  return messages;
};

export { tokenizeMessage as default, isUrl, URL_REGEX };
